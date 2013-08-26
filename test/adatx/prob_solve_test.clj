(ns adatx.prob-solve-test
  (:use [clojure.test])
  (:require [adatx.prob-solve :as solve]))




(def prog-holder {:symlookup  {:l :listgen
                               :ld :depthdown
                               1 '+
                               2 '-
                               3 '*
                               4 '/
                               5 'x1
                               6 'x2
                               7 'x3}
                  :prog-holder '(fn [x1 x2 x3] (+ x1 :adatx.prog-hold/prog))
                  :testfun (fn [in out] (= in out))
                  :in-out-pairs  [{:in [1 2 3] :out 5}
                                  {:in [1 3 3] :out 6}
                                  {:in [2 3 3] :out 6}
                                  {:in [4 3 3] :out 6}
                                  ]
                  
                  }
)


;(is (= (solve/prob-solve prog-holder) '(fn [x1 x2 x3] (+ x1 (+ x2 x3 (- x1))))))






