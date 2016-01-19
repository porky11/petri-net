(defpackage #:petri-net-dot
  (:use #:cl #:alexandria #:petri-net #:inferior-shell)
  (:export #:petri-net->file
           #:relevant-petri-net->file
           #:enabled-petri-net->file
           ))
(in-package #:petri-net-dot)

(defun transition-relevant-p (transition petri-net)
  (dolist (place (transition-input-places transition))
    (when (member place (petri-net-place-tokens petri-net) :test #'eq)
      (return-from transition-relevant-p t))))


(defun petri-net->dot (name net)
  (with-open-file (stream (format nil "~a.dot" name) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "
digraph ~S {
  rankdir=BT
  ~@[
  ~{\"T_~a\"~^,~%  ~}
  [shape=box]
  ~]
  ~{\"P_~a\"~^,~}[fillcolor=red style=filled]
  ~{
   {~{\"P_~a\"~^,~}}->\"T_~a\"->{~{\"P_~a\"~^,~}}
  ~}
}"
            name
            (mapcar #'transition-identifier (petri-net-transitions net))
            (petri-net-place-tokens net)
            (apply #'append (mapcar (lambda (transition)
                                      (with-accessors ((id transition-identifier)
                                                       (in transition-input-places)
                                                       (out transition-output-places))
                                          transition
                                        (list in id out)))
                                    (petri-net-transitions net))))))


(defun relevant-transition-list (net)
  (remove-if-not (lambda (transition)
                   (transition-relevant-p transition net))
                 (petri-net-transitions net)))

(defun compute-relevant-petri-net (net)
  (make-petri-net :place-tokens (petri-net-place-tokens net)
                  :transitions (relevant-transition-list net)))

(defun compute-enabled-petri-net (net)
  (make-petri-net :place-tokens (petri-net-place-tokens net)
                  :transitions (enabled-transition-list net)))


(defun relevant-petri-net->dot (name net)
  (petri-net->dot name (compute-relevant-petri-net net)))

(defun enabled-petri-net->dot (name net)
  (petri-net->dot name (compute-enabled-petri-net net)))

(defun dot->file (name type)
  (run (format nil "dot ~a.dot -T~a -o~a.~a" name type name type)))

(defun petri-net->file (name type net)
  (petri-net->dot name net)
  (dot->file name type))

(defun relevant-petri-net->file (name type net)
  (relevant-petri-net->dot name net)
  (dot->file name type))

(defun enabled-petri-net->file (name type net)
  (enabled-petri-net->dot name net)
  (dot->file name type))

