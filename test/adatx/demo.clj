(ns adatx.demo
    (:use clojure.test
        adatx.core
        adatx.progseq))


1
2
3
'...
:l
:v
:ld

(def numbering '(1 2 3 4 5 :l :v :ld))

;;TODO generate this with a helper...
(def symlookup-demo {1 '+
                2 '*
                3 'x
                4 'count
                5 10
                })

(spec_iterate '()  numbering)
(spec_iterate '(1)  numbering)
(spec_iterate '(2)  numbering)
(spec_iterate '(:l)  numbering)
(spec_iterate '(:v)  numbering)


(spec_iterate ' (2 :l :v :v)  numbering)
(spec_iterate ' (2  2 :l :v)  numbering)


(take 100 (iterate
  (fn [spec] (spec_iterate spec  numbering)) '()
    ))


(genprog nil '(1) symlookup-demo)
(genprog nil '(2) symlookup-demo)
(genprog nil '(3) symlookup-demo)
(genprog nil '(:ld) symlookup-demo)
(genprog nil '(:l) symlookup-demo)
(genprog nil '(:v) symlookup-demo)
(genprog nil '(5 :v 5) symlookup-demo)
(genprog nil '(1 5 :l 4 :v 1 2 5 4 5 :ld :ld 5) symlookup-demo)

(eval (genprog nil '(1 5 :l 4 :v 1 2 5 4 5 :ld :ld 5) symlookup-demo) )










