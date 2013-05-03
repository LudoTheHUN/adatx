(ns adatx.core-test
  (:use clojure.test
        adatx.core))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))


(def symlookup-nums
  {1 1
   2 2
   3 3
   4 4
   5 5
   6 6
   7 7
   8 8
   9 9
   10 10
   11 11 })

(deftest genprog-test
  (testing "genprog for correct behaviour"
    (is (= 
          (genprog nil '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 ) symlookup-nums)   
          '(10 (9 8 (7) 6 5) 4 3 2 1)
          ))
    (is (=
          (genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) symlookup-nums)
          '(9 8 (7 6 5) 4 3 2 1)
          ))
    (is (= 
          (genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) symlookup-nums)
          '(9 8 (7 6 5) 4 3 2 1)
          ))
    (is (= 
          (genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 ) symlookup-nums)  
          '(((9 8 (7 6 5))) 4 3 2 1)
          ))
    (is (=
          (genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 :ld 10 11) symlookup-nums) 
          '((11 10 (9 8 (7 6 5))) 4 3 2 1)
          ))
  )
)
