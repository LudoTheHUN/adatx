(ns adatx.prob-solve-test
  (:use [clojure.test])
  (:require [adatx.prob-solve :as solve]
            [adatx.prog-hold :as prog-hold]))




(def prog-holder {
  :symvec        ['+ '- 'x1 'x2]
  ;:symlookup     {1 '+ 2 '- 5 'x1 6 'x2 :l :listgen :ld :depthdown}   ;
  ;:symlookup     (symlookup/make_symlookup_map_l  ['+ '- 'x1 'x2])
  :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)    ;this is the simplest two arity prog-holder. :adatx.prog-hold/prog represents where in the s-expression we want to generate code.
  :testfun       (fn [returned out] (= returned out))   ;optional, default is = .  Returned is result of the :in being passed as parameters to the generated fn, out is the specified :out.
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
  :loglevel 0     ;optional, default 1
  :maxprogs 5000  ;optional, default 1000000 , how many program we'll generate before giving up
  :nprogs   2       ;optional, default 1 , number of correct programs to find
 }
)


(def solution_workings (solve/prob-solve prog-holder))

(deftest test-prob-solve

(is (= (solve/get-solution solution_workings) '(fn [x1 x2] (+ x1 x1 x2))))


(is (= ((solve/solution_fn solution_workings) 4 3) 11))

  )



#_(solve/prob-solve
 {
  :symvec        ['+ '- '* '/ 'x1 'x2]
  :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)    ;this is the simplest two arity prog-holder. :adatx.prog-hold/prog represents where in the s-expression we want to generate code.
  :in-out-pairs  [{:in [1 3] :out 4}
                  {:in [1 4] :out 5}
                  {:in [2 4] :out 7}
                  {:in [4 4] :out 11}]
  :sandbox :none
  :loglevel 1     ;optional, default 1
  ;:maxprogs 5000  ;optional, default 1000000 , how many program we'll generate before giving up
 }
 )
