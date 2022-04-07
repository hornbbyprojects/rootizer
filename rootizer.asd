(defsystem "rootizer"
  :depends-on ("cl-redis" "trivia" "alexandria" "hunchentoot" "cl-json" "str")
  :components ((:file "rootizer")))
