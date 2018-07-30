Make and simulate petri-nets in a simple way

_This is not actively maintained anymore_

=> use [simple-petri-net](https://gitlab.com/porky11/simple-petri-net) instead

If features are missing, I will rather add them there


## Functions

* `make-petri-net &key transitions place-tokens execution-policy`

Creates the petri net

`transitions` is a list of transitions

transitions can be written as list of the form:

`(identifier list-of-input-places list-of-output-places)`

`place-tokens` is a list of identifiers for places, where initially tokens are

`execution-policy` is a function-designator

the function takes the list of identifiers of enabled transitions in the petri-net and should return one of them
when the transition doesn't exist an error will occur by default when executing the petri-net
by default a random transiton will fire in the petri-net


* `petri-net-step petri-net &optional errorp error-value`

Executes a single step for petri net simulation

`petri-net` is made by make-petri-net

if `errorp` is nil, `error-value` will be returned, instead of signalling an error, when transition-identifier doesn't exist

by default it returns the identifier of the transition that actually fires


See [petri-net-example.lisp](petri-net-example.lisp) for a small tutorial
it shows how to model a simple story based on petri nets


## Visualization

See [petri-net-dot.lisp](petri-net-dot.lisp) to generate `.dot` files.

You can use them to visualize the petri net using graphviz.


## Example usage

Download the git into your `quicklisp/local-projects/` directory

;;use the program
(ql:quickload :petri-net)

;;test the example
(ql:quickload :petri-net-test)
(petri-net-test:call-story)
