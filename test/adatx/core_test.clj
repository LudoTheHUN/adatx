(ns adatx.core-test
  (:use clojure.test
        adatx.core))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))


(def symlookup-nums
  {:l :listgen
   :v :vectorgen
   :ld :depthdown
   1 1
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



(deftest r-pairoff-test
  (testing "r-pairoff-pre, lists upto the next time the depth is the same. "
    (is (=     (r-pairoff-pre '() #{:l} :ld)       '()  ))     ;nil cases
    (is (=     (r-pairoff-pre nil #{:l} :ld)       '()  ))     ;nil cases
    (is (=     (r-pairoff-pre '(:ld) #{:l} :ld)                 '(:ld)  ))     ;bad cases
    (is (=     (r-pairoff-pre '(1 2 3 :l 4 5 6 7 8)  #{:l} :ld)     '()))   ;first element not a depth increaser, no result 
    (is (=     (r-pairoff-pre '(:l 4 5 :ld 6 7 8)    #{:l} :ld)     '(:l 4 5 :ld)  ))  ;;
    (is (=     (r-pairoff-pre '(:l 4 :l 5 :ld 6 :ld 7 8) #{:l} :ld) '(:l 4 :l 5 :ld 6 :ld) ))
    (is (=     (r-pairoff-pre '(:l 4 5 :ld 6 :ld 7 8) #{:l} :ld)    '(:l 4 5 :ld) ))
    (is (=     (r-pairoff-pre '(:l 4 5 :l 6 :ld 7 8) #{:l} :ld)     '(:l 4 5 :l 6 :ld 7 8)  ))
    (is (=     (r-pairoff-pre '(:l 4 5 :l :l 6 :ld :ld :ld 7 8) #{:l} :ld)     '(:l 4 5 :l :l 6 :ld :ld :ld) ))
    (is (=     (r-pairoff-pre '(:l :l :l :l 6 :ld :ld :ld :ld 7 8) #{:l} :ld)  '(:l :l :l :l 6 :ld :ld :ld :ld) ))
    (is (=     (r-pairoff-pre '(:l :l :l 6 :ld :ld :ld :l 7 8) #{:l} :ld)      '(:l :l :l 6 :ld :ld :ld) ))
    (is (=     (r-pairoff-pre '(:l :ld 2 :l 6 :ld :ld :ld :l 7 8) #{:l} :ld)   '(:l :ld)  ))
    (is (=     (r-pairoff-pre '(:l :ld :ld :ld :ld :l 7 8) #{:l} :ld)          '(:l :ld)  ))
    (is (=     (r-pairoff-pre '(:l 1 2 :l 3 :ld 4 5 :l 6 :ld 7 :ld 8 9) #{:l} :ld)   '(:l 1 2 :l 3 :ld 4 5 :l 6 :ld 7 :ld)  ))
    (is (=     (r-pairoff-pre '(:l 1 2 :l 3 :ld :l 6 :ld 7 :ld 8 9) #{:l} :ld)       '(:l 1 2 :l 3 :ld :l 6 :ld 7 :ld)  ))
    (is (=     (r-pairoff-pre '(:l 1 2 :l 3 :ld :l 6 :ld :ld 8 9) #{:l} :ld)         '(:l 1 2 :l 3 :ld :l 6 :ld :ld)  ))
    (is (=     (r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld 8 9) #{:l} :ld)  '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld)  ))
    (is (=     (r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) #{:l} :ld)  '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld)  ))
    )
  (testing "r-pairoff-post, lists past the next time the depth is the same. "

    (is (=     (r-pairoff-post '() #{:l} :ld)   '() ))
    (is (=     (r-pairoff-post nil #{:l} :ld)   '() ))
    (is (=     (r-pairoff-post nil #{:l} :ld)   '() ))
    (is (=     (r-pairoff-post '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) #{:l} :ld)  '(:ld 8 9) ))
    (is (=     (r-pairoff-post '(:l 4 5 :ld 6 7 8)        #{:l} :ld)   '(6 7 8)  ))
    (is (=     (r-pairoff-post '(:l :ld 2 :l 6 :ld :ld :ld :l 7 8) #{:l} :ld)   '(2 :l 6 :ld :ld :ld :l 7 8)  ))
  )
)
        



(deftest genprog-test
  (testing "genprog for correct behaviour with :l"
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
  
  (testing "genprog for correct behaviour with :v"
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
