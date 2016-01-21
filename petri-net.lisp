(defpackage #:petri-net
  (:use #:cl #:alexandria)
  (:export
   ;;transition
   #:transition
   #:make-transition
   #:copy-transition
   #:transition-identifier
   #:transition-input-places
   #:transition-output-places
   ;;petri-net
   #:petri-net
   #:make-petri-net
   #:copy-petri-net
   #:petri-net-transitions
   #:petri-net-place-tokens
   #:petri-net-execution-policy
   ;;execute
   #:transition-enabled-p
   #:enabled-transitions
   #:transition-fire
   #:petri-net-step
   ;;dynamic
   #:petri-net-create-place-token
   #:petri-net-consume-place-token
   ))
(in-package #:petri-net)



(defstruct (transition (:type list))
  identifier
  input-places
  output-places)


(defun random-element (list)
  (nth (random (length list)) list))
    
(defstruct (petri-net (:type list))
  place-tokens
  transitions
  (execution-policy #'random-element))

(defun members (elements list &rest member-args)
  (or (not elements)
      (and list
           (apply #'members (cdr elements)
                  (if (apply #'member (car elements) list member-args)
                      (apply #'remove (car elements) list :count 1 member-args)
                      (return-from members nil))
                  member-args))))

(defun transition-enabled-p (transition petri-net)
  "test if <transitoin> is enabled in <petri-net>"
  (members (transition-input-places transition) (petri-net-place-tokens petri-net) :test #'eq))

(defun petri-net-create-token (petri-net place)
  "add a token to the <place> of <petri-net>"
  (push place (petri-net-place-tokens petri-net)))

(defun petri-net-consume-token (petri-net token)
  "delete a token to the <place> of <petri-net>"
  (deletef (petri-net-place-tokens petri-net) token :test #'eq :count 1))

(defun transition-fire (transition petri-net)
  "let the <transition> fire in petri-net
doesn't test if it can fire"
  (dolist (place (transition-input-places transition))
    (petri-net-consume-token petri-net place))
  (dolist (place (transition-output-places transition))
    (petri-net-create-token petri-net place))
  (transition-identifier transition))

(defun enabled-transitions (petri-net)
  "list enabled transitions of <petri-net>"
  (remove-if-not (lambda (transition)
                   (transition-enabled-p transition petri-net))
                 (petri-net-transitions petri-net)))

(defun petri-net-step (petri-net &optional (errorp t) error-value)
  "lets fire one transition of <petri-net> as defined in its execution-policy
optional you can set <errorp> to nil and define an <error-value>"
  (with-accessors ((tokens petri-net-place-tokens)
                   (function petri-net-execution-policy))
      petri-net
    (let ((transition-list (enabled-transitions petri-net)))
      (transition-fire (or (assoc (funcall function (mapcar #'transition-identifier transition-list))
                                  transition-list :test #'eq)
                           (if errorp
                               (error (or error-value "Chosen transition cannot fire"))
                               (return-from petri-net-step error-value)))
                       petri-net))))





