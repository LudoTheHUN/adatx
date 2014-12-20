(ns adatx.demo
    (:use clojure.test
        adatx.core
        adatx.progseq
        adatx.symlookup)
   (:require [adatx.prob-solve :as adatx]))

;;put this together as prep for https://skillsmatter.com/skillscasts/5337-adatx-test-driven-development-literally

1
2
3
'...
:l
:v
:ld

(def numbering '(1 2 3 4 5 :l :v :ld))
(def numbering2 '(+ * / map))

(make_symlookup_map_v numbering)
(make_symlookup_map_v numbering2)


;;TODO generate this with a helper...
(def symlookup-demo  {1 '+
                      2 '*
                      3 'x
                      4 'count
                      5 10
                      })

(def symlookup-demo2 (make_symlookup_map_v numbering2))

(spec_iterate '()  numbering)
(spec_iterate '(1)  numbering)
(spec_iterate '(2)  numbering)
(spec_iterate '(3)  numbering)
(spec_iterate '(4)  numbering)
(spec_iterate '(5)  numbering)
(spec_iterate '(:l)  numbering)
(spec_iterate '(:v)  numbering)
(spec_iterate '(1 1)  numbering)


(spec_iterate ' (2 :l :v :v)  numbering)
(spec_iterate ' (2  2 :l :v)  numbering)
(spec_iterate ' (2  2 :l :v)  numbering)

(spec_iterate '(map)  numbering2)


(def specs100
  (take 100 (iterate
    (fn [spec] (spec_iterate spec  numbering)) '()
      )))

(genprog nil '(1) symlookup-demo)
(genprog nil '(2) symlookup-demo)
(genprog nil '(3) symlookup-demo)
(genprog nil '(:ld) symlookup-demo)
(genprog nil '(:l) symlookup-demo)
(genprog nil '(:v) symlookup-demo)
(genprog nil '(5 :v 5) symlookup-demo)
(genprog nil '(1 5 :l 4 :v 1 2 5 4 5 :ld :ld 5) symlookup-demo)

(eval (genprog nil '(1 5 :l 4 :v 1 2 5 4 5 :ld :ld 5) symlookup-demo) )


(map (fn [z] (genprog nil z symlookup-demo) ) specs100)


;;Solving (simplified) countdown

#_(def workings
 (adatx/prob-solve
  {
  :symvec        ['+ '*  75 50 23 8 7]
  :prog-holder   '(fn [] :adatx.prog-hold/prog)
  :in-out-pairs  [{:in [] :out 812}
                  ]
  :sandbox :none
   :loglevel 1
  :maxprogs 5000000
   :timeout 1000}))

#_(adatx/get-solution workings)

#_((adatx/solution_fn workings))



{ :symvec        ['+ '- 'x1 'x2]
  :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)
  :testfun       (fn [returned out] (= returned out))
  :in-out-pairs  [{:in [1 2] :out 4}
                  {:in [1 3] :out 5}
                  {:in [2 3] :out 7}
                  {:in [4 3] :out 11}]}




#_(def spek
  { :symvec        ['+ '* 'x1 'x2 2]
    :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)
    :testfun       (fn [returned out] (= returned out))
    :in-out-pairs  [{:in [1 2] :out 4}  {:in [1 3] :out 5} {:in [2 3] :out 7} {:in [4 3] :out 11}]})

#_(let [workings (adatx/prob-solve spek)]
  (adatx/get-solution workings))
