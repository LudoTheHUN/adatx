(ns adatx.core-test
  (:use clojure.test
        adatx.core))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))



(deftest genprog-test
  (testing "genprog for correct behaviour"
    (is (= 
          (genprog nil '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 ) nil nil)   
          '(10 (9 8 (7) 6 5) 4 3 2 1)
          ))
 (quote   (is (=
          (genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil nil)
          '(9 8 (7 6 5) 4 3 2 1)
          ))
    (is (= 
          (genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil nil)
          '(9 8 (7 6 5) 4 3 2 1)
          ))
    (is (= 
          (genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 ) nil nil)  
          '(((9 8 (7 6 5))) 4 3 2 1)
          ))
    (is (=
          (genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 :ld 10 11) nil nil) 
          '(( 10 11 (9 8 (7 6 5))) 4 3 2 1)
          ))
  )
))
