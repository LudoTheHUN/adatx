# adatx

## Given `x` and `y`, find `?` such that `(fn [x] ?)  => y`

A Clojure library for Automatic Design of Algorithms Through X.

Initially, X will be a brute force search through the space of all programs.


### Usage

add a dependency in lein

```
[adatx "0.1.0-SNAPSHOT"]
```

Then in you code:

```
(ns your-ns
  (:require [adatx.prob-solve :as adatx]))
```

A 2 arity example:

```clojure
(def workings
 (adatx/prob-solve
  {
  :symvec        ['+ '- 'x1 'x2]
  :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)
  :in-out-pairs  [{:in [1 2] :out 4}
                  {:in [1 3] :out 5}
                  {:in [2 3] :out 7}
                  {:in [4 3] :out 11}]
  :sandbox :none}))   ;;In general, this may take some time...

(adatx/get-solution workings)    ; => (fn [x1 x2] (+ x1 x1 x2))

((adatx/solution_fn workings) 4 3)     ; => 11

```

It is highly recommended that none of the functions (their symbols) you place into :symvec have any side effects.

`:adatx.prog-hold/prog`  is the `?` in `(fn [x] ?)  => y`  , it represents where in the s-expression we want to generate code

eg:

`:prog-holder   '(fn [x1 x2] (* x1 3 (- x2 :adatx.prog-hold/prog)))` is valid...

### Inspired by

  * http://www-ia.hiof.no/~rolando/
  * http://www.wolframscience.com/
  * http://www.idsia.ch/~juergen/goedelmachine.html
  * http://en.wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach

### Motivation

Test driven development... literally.

### Methodology.

The shortest program is probably the correct one.

In a lisp, programs are just ASTs, which can be sequentially enumerated given a dictionary of symbols.

Adatx covers the AST space from the shortest to longest, `eval`'ing each one aginst the :in-out-pairs.

Adatx never attempting to test the same program twice and aims to make the tests run quickly.

Adatx attempts to fail eval attempts gracefully and fast via the optional use of clojail.

### Know issues

To secure your execution environment, you will need to have a '''~/.java.policy''', an example can be found here: https://github.com/flatland/clojail

If you are under windows, you many find this issue resolution helpful in setting up the .java.policy  https://github.com/Raynes/clojail/issues/4

One of the challenges is surviving execution of potentially crippling non terminating functions. Each attempt is given a time-out, after which the thread it runs on is killed. It may be necessary to increase this time-out on slower computers so that we minimise the chance of killing a correct program. Java's thread.stop is deprecated, but used heavily by this project via the clojail library.

Too slow for non toy problems in current version.

List as inputs or outputs seems to be an issue, vectors work ok.


### TODO's

Only works on specific arities.

java interop is not tested at all.

Non optimal search, some low hanging speedups possible, eg: knowing allowed arities of functions, by constraning via types, by remembering previous programs that were partially correct (just for starters)

The search is completely sequential.

### Full spec

```clojure
(def prog-holder {
  :symvec        ['+ '- 'x1 'x2]
  :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)    ;this is the simplest two arity prog-holder. :adatx.prog-hold/prog represents where in the s-expression we want to generate code.
  :testfun       (fn [returned out] (= returned out))   ;optional, default is = , comparator function for output and :out.
  :in-out-pairs  [{:in [1 2] :out 4}
                  {:in [1 3] :out 5}
                  {:in [2 3] :out 7}
                  {:in [4 3] :out 11}]
  ;;:sandbox (prog-eval/make-sb 'adatx.sandboxns)         ; a clojail sandbox without a timeout  ;;;(prog-eval/make-sb (symbol (str *ns*)))
  ;:sandbox (prog-eval/make-sb_tout 'adatx.sandboxns 500) ; a clojail sandbox with a sandbox timeout
     ;;Note you can use your own, maybe more secure clojail sandbox.
  ;;:sandbox :none     ;; This will run with no sandbox, this will not survive some infinite currucsions
  :sandbox :none
  :timeout 200    ;optinal default 200 ms, making this too low can mean skipping a valid (or all) solutions. Is used to timeout the future that execures the work as well as the sandbox timeout.
  :loglevel 0     ;optional, default 0
  :maxprogs 5000  ;optional, default 1000000 , how many program we'll generate before giving up
  :nprogs   2       ;optional, default 1 , number of correct programs to find
 }
)

(adatx/prob-solve  prog-holder)
```

`:symvec` is a vector of the (quoted) symbols you wish to search the program space with. You should include the :prog-holder fn arity symbols (eg: `x1`, `x2` in the examples). Try limiting these to functions likely relavant to your problem domain, or be ready for a wait a very long time.

`:prog-holder` If you know part of your function, but need to figure out a sub expression, just use :adatx.prog-hold/prog to replace that subexpression. You will need to be mindfull of the arity of the fn.

`:testfun` This is the function that will tests if the fn output matches to what you specified in `in-out-pairs`

`:in-out-pairs` This is where you provide your test cases for input that outputs (or the x's and y's in `(fn [x] ?)  => y`

`:sandbox` you can specify a clojail jail to run your code in, if you really do want to have side effects, this is probably a must.

`:timeout` ms before we give up on evaluating the expression being tested. If you make it too low, you will never find any solutions.

`:loglevel` set this higher up if you want to see what's happening under the hood...

`:maxprogs` number of expressions we will generate before giving up on finding a solution.

`:nprogs` we can keep searching untill we find :nprogs solutions.

### Some more examples

... all of them quite contrived.

```clojure
(def prog-holder
  ;;finding a reduce function
  ;;solution: (fn [lst] (reduce (fn [xs x] (+ xs (* xs x))) lst))
  ;;runtime ~300000 seconds
  {
  :symvec        ['* '+ 'xs 'x]
  :prog-holder   '(fn [lst]
                    (reduce (fn [xs x]
                              :adatx.prog-hold/prog)
                            lst))
  :testfun       (fn [returned out] (= returned out))
  :in-out-pairs  [{:in [ [1 4 5] ] :out 30}
                  {:in [ [2 4 5] ] :out 60}
                  {:in [ [1 4 7] ] :out 40}
                  ]
  :sandbox :none
  :timeout 500
  :loglevel 10
  :maxprogs 50000
  :nprogs   1
 }
)
```

```clojure
(def prog-holder
  ;;First ten numbers tautology.
  ;;solution   (fn [] (vec (take 10 (iterate inc 1))))
  ;;runtime    ~130ms
  {
  :symvec        ['iterate 'inc '1]
  :prog-holder   '(fn [] (vec (take 10
                           :adatx.prog-hold/prog)
                          ))
  :testfun       (fn [returned out] (= returned out))
  :in-out-pairs  [{:in [] :out [1 2 3 4 5 6 7 8 9 10]}
                  ]
  :sandbox :none;;(prog-eval/make-sb_tout 'adatx.sandboxns 500)
  :timeout 200
  :loglevel 10
  :maxprogs 5000
  :nprogs   1
 }
)
```

```clojure
(def prog-holder
  ;;Fibonacci sequence
  ;;solution  (fn [] (take 10 ((fn fib [] (lazy-cat [0 1] (map + (rest (fib)) (fib)))))))
  ;;runtime ~1000years (based on ~7^16 tries at 1000 tests per second. Note, this would be far shorter if we take Moore's law into account, aprox 9 hours in 2044  )
  {
  :symvec        ['lazy-cat 'map '+ 'rest '0 '1 'fib]
  :prog-holder   '(fn [] (vec (take 10
                           ((fn fib [] :adatx.prog-hold/prog))
                          )))
  :testfun       (fn [returned out] (= returned out))
  :in-out-pairs  [{:in [] :out [0 1 1 2 3 5 8 13 21 34]}
                  ]
  :sandbox :none
  :timeout 200
  :loglevel 0
  :maxprogs 100000000000000000000
  :nprogs   1
 }
)
```


### Discussion

https://groups.google.com/forum/#!topic/clojure/F-OmM9tLwbc

https://github.com/webyrd/mad-at-x

https://groups.google.com/forum/#!topic/london-clojurians/mUNWibQNKLk

### Notice

You must take care with side-effect functions. They will be called with all values/functions reachable by the search.

I take Zero responsibility the any damage caused by the use of this software.

## License

Copyright © 2013 Ludwik Grodzki

Distributed under the Eclipse Public License, the same as Clojure.

