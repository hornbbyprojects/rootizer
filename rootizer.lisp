(defpackage :rootizer
  (:use #:cl)
  (:local-nicknames (:i :iterate))
  (:export #:start-server
           ))

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

(defun all-turns-key (game)
  (format nil "~a-all-turns" game))

(defun game-last-updated-key (game)
  (format nil "~a-last-updated" game))

(defun new-game (players)
  (with-lock
    (let ((new-game (red:incr +current-game-key+)))
      (red:set (game-last-updated-key new-game) (get-universal-time))
      (apply #'red:lpush (players-circle-key new-game) (reverse players))
      new-game)))


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
       (create-lock ,lock-id)
       (let ((,ret (progn ,@body)))
         (delete-lock ,lock-id)
         ,ret))))

(defun turn-times-key (game)
  (format nil "~a-turn-times" game))
(defun player-turns-key (game player)
  (format nil "~a-~a-turns" game player))

(defun current-player (game)
  (car (red:lrange (players-circle-key game) 0 0)))

(defun switch-to-next-player (game)
  (let ((key (players-circle-key game)))
    (red:lmove key key "LEFT" "RIGHT")))

(defun current-game ()
  (red:get +current-game-key+))

(defun submit-time ()
  (with-lock
    (let* ((game (current-game))
           (player (switch-to-next-player game))
           (next-player (current-player game))
           (last-time (parse-integer (red:get (game-last-updated-key game))))
           (new-time (get-universal-time))
           (time-diff (- new-time last-time)))
      (red:set (game-last-updated-key game) new-time)
      (red:rpush (player-turns-key game player) time-diff)
      (red:rpush (all-turns-key game) (format nil "~a-~a" player time-diff))
      (list game player next-player time-diff))))


(defun require-post ()
  (if (not (equal :post (hunchentoot:request-method*)))
      (error "Allowed methods: post")))

(defun set-json-headers ()
  (setf (hunchentoot:header-out "Content-Type") "application/json")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*"))



(defun get-all-turns (game)
  (with-lock
    (mapcar (lambda (x) (str:split "-" x)) (red:lrange (all-turns-key game) 0 -1))))

(defun all-turns-endpoint ()
  (redis:with-connection ()
    (set-json-headers)
    (let ((game (hunchentoot:get-parameter "game")))
      (json:encode-json-to-string (get-all-turns game)))))

(defun current-player-endpoint ()
  (redis:with-connection ()
    (set-json-headers)
    (let ((game (hunchentoot:get-parameter "game")))
      (json:encode-json-to-string (current-player game)))))


(defun submit-endpoint ()
  (redis:with-connection ()
    (trivia:let-match (((list game player next-player time) (submit-time)))
      (require-post)
      (set-json-headers)
      (json:encode-json-alist-to-string
       `(("player" . ,player)
         ("next-player" . ,next-player)
         ("game_number" . ,game)
         ("time" . ,time))))))

(defun new-game-endpoint ()
  (redis:with-connection ()
    (let* ((players (str:split "," (hunchentoot:get-parameter "players")))
           (game-number (new-game players)))
      (set-json-headers)
      (require-post)
      (json:encode-json-alist-to-string `(("game_number" . ,game-number))))))

(defun select-factions-endpoint ()
  (let* ((player-count (hunchentoot:get-parameter "player_count"))
         (factions (if (null player-count)
                       (error "no player count provided")
                       (select-factions (parse-integer player-count)))))
    (set-json-headers)
    (json:encode-json-alist-to-string
     `(("factions" . ,factions)))))

(defun latest-game-endpoint ()
  (redis:with-connection ()
    (let* ((latest-game (red:get +current-game-key+))
           (latest-players (red:lrange (players-circle-key latest-game) 0 -1)))
      (set-json-headers)
      (json:encode-json-alist-to-string
       `(("game_number" . ,latest-game)
         ("players" . ,latest-players))))))


(defmacro add-route (path func)
  `(push
     (hunchentoot:create-prefix-dispatcher ,path ,func)
     hunchentoot:*dispatch-table*))
(defun start-server ()
  (if (boundp '*server*)
      (hunchentoot:stop *server*))
  (setf hunchentoot:*dispatch-table* nil)
  (add-route "/submit" #'submit-endpoint)
  (add-route "/new-game" #'new-game-endpoint)
  (add-route "/select-factions" #'select-factions-endpoint)
  (add-route "/latest-game" #'latest-game-endpoint)
  (add-route "/all-turns" #'all-turns-endpoint)
  (add-route "/current-player" #'current-player-endpoint)
  (defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (hunchentoot:start *server*))

