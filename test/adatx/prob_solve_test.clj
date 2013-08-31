(ns adatx.prob-solve-test
  (:use [clojure.test])
  (:require [adatx.prob-solve :as solve]
            [adatx.prog-hold :as prog-hold]))




(def prog-holder {:symlookup  {:l :listgen
                               :ld :depthdown
                               1 '+
                               2 '-
                               3 '*
                               4 '/
                               5 'x1
                               6 'x2
                               7 'x3}
                  ;:prog-holder '(fn [x1 x2 x3] (+ x1 :adatx.prog-hold/prog))
                  :prog-holder '(fn [x1 x2 x3] :adatx.prog-hold/prog)
                  ;:testfun (fn [out ans] (= out ans))
                  :testfun (fn [out ans] (try (= out ans) (catch Exception e nil)))

                  :in-out-pairs  [{:in [1 2 3] :out 5}
                                  {:in [1 3 3] :out 6}
                                  {:in [2 3 3] :out 6}
                                  {:in [4 3 3] :out 6}
                                  ]
                  :maxprogs 1000
                  :loglevel 10
                  }
)



(quote
(def answer (solve/prob-solve prog-holder))
(:ready-prog (first (:evaled-maps (first answer))))

;(is (= (solve/prob-solve prog-holder) '(fn [x1 x2 x3] (+ x1 (+ x2 x3 (- x1))))))

(solve/prob-solve 
 {:symlookup {1 '+ 2 '- 5 'x1 6 'x2 :l :listgen :ld :depthdown}
  :prog-holder '(fn [x1 x2] :adatx.prog-hold/prog)
  :testfun (fn [in out] (= in out))
  :in-out-pairs  [{:in [1 2] :out 4}
                  {:in [1 3] :out 5}
                  {:in [2 3] :out 7}
                  {:in [4 3] :out 11}]
  :sandbox (prog-eval/make-sb 'adatx.sandboxns)  ;;;(prog-eval/make-sb (symbol (str *ns*)))   
  :timeout 200
 })

)



