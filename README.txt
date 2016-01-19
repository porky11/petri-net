Make and simulate petri-nets in a simple way

make petri net
(make-petri-net &key transitions place-tokens execution-policy)

transitions is a list of transitions
transitoins can be written as list of the form:
(identifier list-of-input-places list-of-output-places)

place-tokens is a list of identifiers for places, where initially tokens are

execution-policy is a function-designator
the function takes the list of identifiers of enabled transitions in the petri-net and should return one of them
when the transition doesn't exist an error will occur by default when executing the petri-net
by default a random transiton will fire in the petri-net


simulate petri net
(petri-net-step petri-net &optional errorp error-value)

petri-net is made by make-petri-net
if errorp is nil, error-value will be returned, instead of signalling an error, when transition-identifier doesn't exist
normally it returns the identifier of the transition that fires


see petri-net-example.lisp for a small tutorial
it shows how to model a simple story based on petri nets


Use:

download the git into your quicklisp/local-projects/

;;use the program
(ql:quickload :petri-net)

;;test the example
(ql:quickload :petri-net-test)
(petri-net-test:call-story)
