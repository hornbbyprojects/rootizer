(defsystem "rootizer"
  :depends-on ("cl-redis" "trivia" "alexandria" "hunchentoot" "hunchensocket" "cl-json" "str")
  :components ((:file "rootizer")))
