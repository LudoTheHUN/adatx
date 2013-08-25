(ns adatx.prog-hold-test
  (:use [clojure.test])
  (:require [adatx.prog-hold :as phold]))


(def prog-holder
   '(fn [x] (+ x :adatx.prog-hold/prog)))


(def prog '(+ x 1)) 


(deftest prog_wrap-test
  (testing "Tests to see if different prog holders work"
    (is (= 
          (let [prog-holder '(fn :adatx.prog-hold/prog)                    prog '(+ x 1)] (phold/prog_wrap prog-holder prog))
          '(fn (+ x 1))))
    (is (= 
          (let [prog-holder '(fn [x] :adatx.prog-hold/prog)                prog '(+ x 1)] (phold/prog_wrap prog-holder prog))
          '(fn [x] (+ x 1))))
    (is (= 
          (let [prog-holder '(fn [x] {:a 1 :prog :adatx.prog-hold/prog})   prog '(+ x 1)] (phold/prog_wrap prog-holder prog))
          '(fn [x] {:a 1, :prog (+ x 1)})))
    ))
 

;(prog_wrap-test)