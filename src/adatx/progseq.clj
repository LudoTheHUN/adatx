(ns adatx.progseq
  ;(:use [clojure.pprint] [clojure.walk])
  )


(def symlookup
  "example lookup map"
  {1 'hello
   2 2
   3 "are"
   4 '(sdf asasd)
   5 '+
   6 \a
   7 ['a 'b]
   8 #{:1 :a}
   9 {:a 1 :b "foo"}
   10 ''(sdf sdfwe wer)     })

(def symlookup
  "example symbol map"
  {:l :listgen     ;reserved for iterator
   :v :vectorgen   ;reserved for iterator
   :ld :depthdown  ;reserved for iterator
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
   11 11})


(def keylist "keys of the example symol map" (keys symlookup))

(def spec "exampel program spec that can be itterated over or be turned into a prog" '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 ))

(def pgpartial "prog partial, when envoking, should be" nil)   ;could close over this

(def n "exampel n "4)


(defn- pairoff-pre [list accum match matchd depth]
  "finds the list upto the point the match and matched are paired off
   match is a set of keys matchd eg: 
   :l for list 
   :v for vector
   :m for map  - not implemented
   :s for set  - not implemented
   :r for regex  - not implemented
   and maybe other literals
   matchd is just the one closing off key, typically :ld , which is where list, vectors etc are closed off"
  (cond 
     (and (empty? depth) (not (empty? accum)))
       accum
     (contains? match (first list))
       (do ;(println "depth up")
         (pairoff-pre (rest list) (cons (first list) accum) match matchd (cons 0 depth)))
     ;(and (= (first list) matchd) (not (empty? depth)))
     (= (first list) matchd)
       (do ;(println "depth down")
         (pairoff-pre (rest list) (cons (first list) accum) match matchd (rest depth)))
     (and (not (empty? depth)) (not (empty? list)))
       (do ;(println "non nil depth")
         (pairoff-pre (rest list) (cons (first list) accum) match matchd depth))
     (empty? depth)
        accum
  :else
    accum
  ))

(defn- r-pairoff-pre [list  match matchd]
 "looks ahead and returns list until match and matchd are all paired off."
  (reverse (pairoff-pre list nil match matchd nil)))

(defn- r-pairoff-post [list  match matchd]
 "looks ahead and returns list until match and matchd are all paired off."
    (let [len (count (pairoff-pre list nil match matchd nil))]
      (drop len list)))
     
(defn genprog [pgpartial spec symlookup]
  "v1.2 correct for lists generation with :l, :v and :ld
   consider adding :m for literal map support, will need to deal with bad pairs
   consider adding :s for literal set support, will need to deal with duplicate keys
   It drops data if we :ld past lowest level
  "
  ;(println "pgpartial:" pgpartial " spec:" spec " depth:"depth)
  (let [speclookup   (symlookup (first spec))
        specf        (first spec)
        spec_pre_ld  (if (contains? #{:l :v} specf)
                         (r-pairoff-pre spec #{:l :v} :ld)
                          '())
        spec_past_ld (let [len (count spec_pre_ld)] (drop len spec))
        ]
   (cond 
    (= specf :l)     
        (genprog
             (cons 
               (genprog (list) (rest spec_pre_ld) symlookup)          ;;list version
               pgpartial
               )
         spec_past_ld symlookup)
    (= specf :v)     
        (genprog
             (cons 
               (vec (genprog (list) (rest spec_pre_ld) symlookup))   ;; vector version
               pgpartial
               )
         spec_past_ld symlookup)
    (and (not (= specf nil)) 
         (not (= specf :ld))
         ;(not (= specf :l))
         )   ;; the sale level add a symbol
           (genprog (cons speclookup pgpartial)  (rest spec) symlookup)
     :else
        (reverse pgpartial))   ;NOTE , this will be interesting when we add maps and sets...
  ))

(genprog pgpartial spec symlookup)   
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) symlookup)   
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) {})


(defn- spec_iterate_dumb [spec keylist]
  "DO NOT USE v1.0 returns next spec   where spec is a list, each element is in the ordered list given by symlookup
   iterating up on the left first
   very basic, will generate specs for silly lisp forms that go lower then base level with too many :ld"
 (let [specf (first spec)
       next  (second (drop-while #(not(= % specf))  keylist))  ]
   (cond 
     next    ;first number is going up by one
       (cons next (rest spec))
     (not specf)  ;;we need to grow the spec, 
       (cons (first keylist) spec)
     :else   ;first number it ticking round, rest of the numbers get to tick up by one if need be.. 
       (cons (first keylist) (spec_iterate_dumb (rest spec) keylist))     
       )))

(spec_iterate_dumb spec keylist)



(defn- spec-depth-pair [spec]
  "returns pairs of the specs and the depth at that spec item
   supports :l :v :ld only "
  (loop [pgpartial '() spec spec depth 0]
    (if (empty? spec)
      (reverse pgpartial)
      (let [specf (first spec)]
        (recur (cons (list specf depth)  pgpartial)
               (rest spec)
               (+ depth (cond 
                          (contains? #{:l :v} specf) 1
                          (= specf :ld) -1
                           :else 0))
       )))))
  
(spec-depth-pair spec)
(spec-depth-pair '(:l :l 1 :ld :ld  1))
(spec-depth-pair '(:l 1 :ld :ld  1))
(spec-depth-pair '(:l 1 :v :ld  1))


(defn spec_iterate [spec keylist]
  "v2.0 Iterate on the right, grows on the left, just like normal numbers, but with special carrying for :ld symbol.
   Depth aware based on spec-depth-pair function.
   carrying aware, does not produce silly specs iterations. 
   Should cover the space of all program specs with each correct program being generated once and only once.
   keylist has to have :l :v at the end, and :ld at the very end, althugh they are optional if we do not want to
   generate programs with list, vectors (:l :v), and or if never want to end these (:ld)"
 (let [spec-depth   (reverse (spec-depth-pair spec))]
    (loop [pgpartial '() spec-depth spec-depth carry-r? true]
      (if (empty? spec-depth)
        (if carry-r? 
          (cons (first keylist) pgpartial)   ;we are on the last number and we need to carry, hence grow the spec
          pgpartial)  
        (let [specf  (ffirst spec-depth)
              depth  (second (first spec-depth))
              nex    (second (drop-while #(not(= % specf))  keylist))
              carry? (or (and (= depth 0) (= nex :ld) carry-r?)
                         (and (nil? nex) carry-r?)
                         (and (= pgpartial '()) (= nex :ld))
                         )
              next   (if carry-r?
                            (if carry? (first keylist) nex) 
                            specf)]
          (recur          
                     ;;grow on the left to carry over
                  (cons next pgpartial)
                  (rest spec-depth)
                  carry?
                  )
)))))




(spec_iterate spec keylist)
(= (spec_iterate '(2 2 1 2)      '(1 2))     '(1 2 2 :l))
(spec_iterate '(:v :v :v :v)  '(1 2 :l :v :ld))
(spec_iterate '(1 :v 2 :ld)  '(1 2 :l :v :ld))
(spec_iterate '(1)  '(1 2 :ld :l :v) )






;;;;



(defn- spec_iterate_f [keylist]
  "iterator maker function"
  (fn [spec] (spec_iterate spec  keylist )))


 ;;Examples
;(take 100 (iterate (spec_iterate_f '(1 2 :l :v :ld))  '()  ))
;(take 100 (iterate (spec_iterate_f '(1 2 :l :v :ld))  '(1 1 2 :l)  ))
;(def -spec_iter_defed 
; "example final itterator with a fully qualified keylist" 
;  (spec_iterate_f '(1 2 :l :v :ld)))





(defn genprogs_n [n spec keylist symlookup]   ;;keylist could be determined from symlookup  
  "WIP generator of many progs , returns the list of progs.
   TODO, should return the list of progs but also the next spec to itterate from on a map
   NOTE do not evaluate the iterate "
  (map (fn [x] (genprog nil x symlookup))
    (take n (iterate (spec_iterate_f keylist)  spec ))))

;;(time (last (genprogs_n 50 '( 1) '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 :l :ld) symlookup)   ))

(defn genprogs-lazy [startspec keylist symlookup]   ;;keylist could be determined from symlookup  
  "WIP generator of many progs , returns the list of progs.
   TODO, should return the list of progs but also the next spec to itterate from on a map"
  (map (fn [x] (genprog nil x symlookup))
    (iterate (spec_iterate_f keylist)  startspec )))


;;(take 2 (genprogs-lazy spec keylist symlookup))

;;(def allprogs (genprogs-lazy '( 1) '(1 2 3 :l :ld) symlookup))
;;  (time (nth allprogs 10000))
;  ;;;;(-- next allprogs)
; (time (first (filter (fn[x] (= x '(3 3 3 2 2 2 2))) allprogs)))


