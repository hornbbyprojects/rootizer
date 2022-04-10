(defpackage :rootizer
  (:use #:cl)
  (:local-nicknames (:i :iterate))
  (:export #:start-server))

(in-package :rootizer)

(alexandria:define-constant +lock-key+ "rootizer-lock" :test 'equal)
(alexandria:define-constant +current-game-key+ "current-game" :test 'equal)

(defparameter *militant-factions*
  '("Moles" "Cats" "Eyrie"))

(defparameter *insurgent-factions*
  '("Lizards" "Crows" "Woodland Alliance" "Riverfolk"))

(defun concat-reverse-first (first second)
  (i:iter
    (i:for item in first)
    (setf second (cons item second))
    (i:finally (return second))))

(defun remove-at-worker (list index before-item)
  (trivia:let-match (((list* first rest) list))
    (if (= 0 index)
        (values first (concat-reverse-first before-item rest))
        (remove-at-worker rest (1- index) (cons first before-item)))))

(defun remove-at (list index)
  (remove-at-worker list index nil))

(defmacro random-pop (list)
  (alexandria:with-gensyms (index remaining item)
    `(let ((,index (random (length ,list))))
      (multiple-value-bind (,item ,remaining) (remove-at ,list ,index)
        (setf ,list ,remaining)
        ,item))))

(defun select-factions (player-count)
  (let ((to-select (copy-list *militant-factions*))
        (selected nil))
    (push (random-pop to-select) selected)
    (i:iterate
      (i:for faction in *insurgent-factions*)
      (push faction to-select))
    (dotimes (i player-count)
      (push (random-pop to-select) selected))
    (reverse selected)))



(defun players-circle-key (game)
  (format nil "~a-players-circle" game))

(defun players-static-key (game)
  (format nil "~a-players-static" game))

(defun all-turns-key (game)
  (format nil "~a-all-turns" game))

(defun game-last-updated-key (game)
  (format nil "~a-last-updated" game))

(defun player-total-key (game player)
  (format nil "~a-~a-total" game player))

(defun new-game (players)
  (with-lock
    (let ((new-game (red:incr +current-game-key+)))
      (red:set (game-last-updated-key new-game) (get-epoch-time))
      (apply #'red:lpush (players-circle-key new-game) (reverse players))
      (apply #'red:lpush (players-static-key new-game) (reverse players))
      new-game)))

(defun get-players (game)
  (red:lrange (players-static-key game) 0 -1))

(defun get-lock-id ()
  (format nil "~a-~a" (get-universal-time) (random 10000)))

(define-condition already-locked-error (error) ())

(defun create-lock (lock-id)
  (if (not (equal
            "OK"
            (red:set +lock-key+ lock-id "nx" "px" 10000)))
      (error 'already-locked-error)))

(alexandria:define-constant +delete-lock-script+ "
local current_lock = redis.call('GET', KEYS[1])
if current_lock == ARGV[1] then
    return redis.call('del', KEYS[1])
else
    return 0
end
" :test 'equal)
(defun delete-lock (lock-id)
  (red:eval +delete-lock-script+ 1 +lock-key+ lock-id))

(defmacro with-lock (&body body)
  (alexandria:with-gensyms (lock-id ret)
    `(let ((,lock-id (get-lock-id)))
       (unwind-protect 
            (progn
              (create-lock ,lock-id)
              ,@body)
           (delete-lock ,lock-id)))))

(defun turn-times-key (game)
  (format nil "~a-turn-times" game))
(defun player-turns-key (game player)
  (format nil "~a-~a-turns" game player))

(defun get-current-player (game)
  (car (red:lrange (players-circle-key game) 0 0)))

(defun switch-to-next-player (game)
  (let ((key (players-circle-key game)))
    (red:lmove key key "LEFT" "RIGHT")))

(defun current-game ()
  (red:get +current-game-key+))

(defconstant +epoch-start+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun convert-universal-time-to-epoch-time (universal-time)
  (- universal-time +epoch-start+))

(defun get-epoch-time ()
  (convert-universal-time-to-epoch-time (get-universal-time)))

(defun get-last-time (game)
  (parse-integer (red:get (game-last-updated-key game))))


(defun list-to-array (list)
  (map 'vector #'identity list))

(defun get-all-turns (game)
  (list-to-array
   (mapcar
    (lambda (x) (str:split "-" x))
    (red:lrange (all-turns-key game) -10 -1))))

(defun get-player-total (game player)
  (let ((as-string (red:get (player-total-key game player))))
    (if (null as-string)
        nil
        (parse-integer as-string))))

(defun get-number-turns (game player)
  (red:llen (player-turns-key game player)))

(defun get-player-averages (game)
  (i:iterate
    (i:with players = (get-players game))
    (i:for player in players)
    (i:for total = (get-player-total game player))
    (when (null total)
      (i:collect `(,player . 0))
      (i:next-iteration))
    (i:for num-turns = (get-number-turns game player))
    (i:collect `(,player . ,(/ total num-turns)))))

(defun get-game-data (game)
  (with-lock
    (let ((current-player (get-current-player game))
          (last-time (get-last-time game))
          (turns (get-all-turns game))
          (averages (get-player-averages game)))
      `(("current_player" . ,current-player)
        ("last_time" . ,last-time)
        ("turns" . ,turns)
        ("averages" . ,averages)))))

(defun submit-time (game)
  (with-lock
    (let* ((player (switch-to-next-player game))
           (last-time (get-last-time game))
           (new-time (get-epoch-time))
           (time-diff (- new-time last-time)))
      (red:set (game-last-updated-key game) new-time)
      (red:rpush (player-turns-key game player) time-diff)
      (red:rpush (all-turns-key game) (format nil "~a-~a" player time-diff))
      (red:incrby (player-total-key game player) time-diff)
      (send-update game))))


(defun require-post ()
  (if (not (equal :post (hunchentoot:request-method*)))
      (error "Allowed methods: post")))

(defun set-json-headers ()
  (setf (hunchentoot:header-out "Content-Type") "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*"))

(defun prin1-symbol-lower (symbol)
  (let ((*print-case* :downcase)) (prin1-to-string symbol)))

(defmacro with-parameters ((&rest parameters) &body body)
  (let ((let-bindings
          (mapcar
           (lambda (parameter)
             `(,parameter (hunchentoot:get-parameter
                           ,(prin1-symbol-lower parameter))))
           parameters)))
    `(let (,@let-bindings)
       ,@body)))

(defmacro redis-json-endpoint ((&rest parameters) &body body)
  `(redis:with-connection ()
     (with-parameters (,@parameters)
       (set-json-headers)
       ,@body)))

(defun game-data-endpoint ()
  (redis-json-endpoint (game)
    (json:encode-json-alist-to-string (get-game-data game))))

(defun submit-endpoint ()
  (redis-json-endpoint (game)
    (submit-time game)
    (require-post)
    "{\"message\": \"successfully submitted turn\"}"))

(defun new-game-endpoint ()
  (redis-json-endpoint (players)
    (let* ((players-list (str:split "," players))
           (game-number (new-game players-list)))
      (require-post)
      (json:encode-json-alist-to-string `(("game_number" . ,game-number))))))

(defun select-factions-endpoint ()
  (with-parameters (player_count)
    (set-json-headers)
    (let* ((factions (if (null player_count)
                         (error "no player count provided")
                         (select-factions (parse-integer player_count)))))
      (json:encode-json-alist-to-string
       `(("factions" . ,factions))))))

(defun latest-game-endpoint ()
  (redis-json-endpoint () 
    (let* ((latest-game (red:get +current-game-key+)))
      (json:encode-json-alist-to-string
       `(("game_number" . ,latest-game))))))

(defun players-endpoint ()
  (redis-json-endpoint (game)
    (set-json-headers)
    (json:encode-json-to-string (get-players game))))

(defmacro add-route (path func)
  `(push
     (hunchentoot:create-prefix-dispatcher ,path ,func)
     hunchentoot:*dispatch-table*))

(defclass client (hunchensocket:websocket-client)
  ((game :initarg :game :reader client-game)))

(defmethod initialize-instance :after ((client client) &key &allow-other-keys)
  (with-slots (game hunchensocket::request) client
    (setf game (hunchentoot:get-parameter "game" hunchensocket::request))))

(defclass notifier (hunchensocket:websocket-resource) ()
  (:default-initargs :client-class 'client))


(defvar *notifier*)
(defvar *notifier-server*)
(defvar *server*)

(defun send-update (game)
  (format t "sending update for ~a~%" game)
  (dolist (client (hunchensocket:clients *notifier*))
    (if (equal (client-game client) game)
        (progn
          (format t "Sending update to ~a~%" client)
          (hunchensocket:send-text-message client ""))
        (format t "skipping client with game ~a~%" (client-game client)))))

(defun start-notifier ()
  (setf *notifier* (make-instance 'notifier))
  (pushnew (lambda (request) (declare (ignore request)) *notifier*) hunchensocket:*websocket-dispatch-table*)
  (setf *notifier-server* (make-instance 'hunchensocket:websocket-acceptor :port 9090))
  (hunchentoot:start *notifier-server*))

(defun start-server ()
  (if (and (boundp '*server*) (hunchentoot::acceptor-listen-socket *server*))
      (hunchentoot:stop *server*))
  (if (and (boundp '*notifier-server*) (hunchentoot::acceptor-listen-socket *notifier-server*))
      (hunchentoot:stop *notifier-server*))
  (setf hunchentoot:*dispatch-table* nil)
  (setf hunchensocket:*websocket-dispatch-table* nil)
  (start-notifier)
  (add-route "/submit" #'submit-endpoint)
  (add-route "/new-game" #'new-game-endpoint)
  (add-route "/select-factions" #'select-factions-endpoint)
  (add-route "/latest-game" #'latest-game-endpoint)
  (add-route "/game-data" #'game-data-endpoint)
  (add-route "/players" #'players-endpoint)
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (hunchentoot:start *server*))

