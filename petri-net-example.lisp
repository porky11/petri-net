;;;;this is a small tutorial for using petri nets to model stories

(defpackage #:petri-net-example
  (:use #:cl #:petri-net #:alexandria)
  (:export #:call-story))
(in-package #:petri-net-example)


;;;  we define how the desicions for the transitions,
;;;  that will apper in our petrinet should be called.

(defparameter *story-decision-names*
  (plist-hash-table
   '(male-go-school "Bob shall go to school"
     female-go-school "Alice shall go to school"
     both-go-school "Both shall go to school"
     male-skip-school "Bob shall skip school"
     female-skip-school "Alice shall skip school"
     both-skip-school "Both shall skip school"
     male-go-home "Bob shall go home"
     female-go-home "Alice shall go home"
     both-go-home "Alice and Bob shall go home together"
     both-sleep-alone "Both shall sleep now"
     both-sleep "Both shall sleep now"
     exit "Stop game"
     )))


;;;  we define a function, where the user can decide, what shall happen next.
;;;  this will be the petri nets execution policy

(defun user-select-from-list (list)
  (assert list)
  (map nil (lambda (num key)
             (format t "~a: ~a~%"
                     num (gethash key *story-decision-names*)))
       (iota (length list) :start 1)
       list)
  (loop for number = (parse-integer (read-line) :junk-allowed t)
     if (and number (<= 1 number (length list)))
     return (nth (1- number) list)))


;;; we define the petri net for our story

(defparameter *story-petri-net*
  (make-petri-net
   :place-tokens
   ;;there will be a male and a female person in the morning
   '(male-morning
     female-morning)
   :transitions
   ;;you can decide for every person, what to do next in different situations
   ;;we named the decisions before
   '((male-go-school (male-morning) (male-school))
     (female-go-school (female-morning) (female-school))
     (both-go-school (both-morning) (male-school female-school))
     (male-skip-school (male-morning) (male-evening))
     (female-skip-school (female-morning) (female-evening))
     (male-go-home (male-school) (male-evening))
     (female-go-home (female-school) (female-evening))
     (both-go-home (female-school male-school) (both-evening))
     (both-sleep-alone (male-evening female-evening) (male-morning female-morning))
     (both-sleep (both-evening) (both-morning))
     (exit (both-morning) ()))
     
   
   :execution-policy #'user-select-from-list))

;;;  we define the texts of the petri-net
;;;  the text will be displayed after selecting a decision
     
(defparameter *story-texts*
  (plist-hash-table
   '(male-go-school "Bob goes to school."
     female-go-school "Alice goes to school."
     both-go-school "Both go to school."
     make-skip-school "Bob skips school and has a good day."
     female-skip-school "Alice skips school and has a good day."
     both-skip-school "Both skip school and have a good day together."
     male-go-home "Bob goes home after school."
     female-go-home "Alice goes home after school."
     both-go-home "Both go to a hotel after school."
     both-sleep-alone "Alice and Bob sleep the whole night and are lonely."
     both-sleep "Both cuddle the whole night and don't sleep much."
     exit "The end")
   :test #'eq))

(defun call-story ()
  (loop while (enabled-transitions *story-petri-net*)
        do (write-line
            (gethash 
             (petri-net-step *story-petri-net*)
             *story-texts*))))




