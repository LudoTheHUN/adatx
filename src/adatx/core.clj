(ns adatx.core
   (:use [clojail.core :only [sandbox]]
        [clojail.testers :only [blacklist-symbols blacklist-objects]]
        [clojure.pprint]
        [clojure.walk])
 )

;;TODO
;inspect correct arrities
;DONE create known function store - symlookup
;create teacher in-out pairs
;DONE create valid s-expression builder
  ;DONE numbering system to generate expressions?
;DONE create safe, execution time aware execute function
;DONE create brute force strategy
   ;DONE create itterator for spec-builder

;Consider a spec comparator function...

;reseach core.logic to see if logic programs count be found this way
;;DONE Setting up sandbox in clojail


;;(time (sb '(+ 1 2)))
;;(time (+ 1 2))
;;(time (eval '(+ 1 2)))
;;(time(deref(future (+ 1 2))))
;;(time(deref(future (eval '(+ 1 2)))))
;;(time(deref(future (sb   '(+ 1 2)))))
;; (* 6000 60 24)
;   (ns-publics 'adatx.core)
(def tester [(blacklist-symbols #{'alter-var-root})
             (blacklist-objects [java.lang.Thread])]) ; Create a blacklist.
(def sb (sandbox tester :timeout 50 :namespace 'adatx.core))



(defmacro my_eval [& body]
  `(try (deref (future
           {:eval (eval ~@body)
            :expr    ~@body 
            :quoted '~@body})
         10 {:timeout 10})
     (catch Exception e# {:error 1 :errormsg e#     :expr    ~@body :quoted '~@body})
     ))

(defmacro my_eval2 [& body]
  `(let [fut# (future
               {:eval   (eval ~@body)
                :expr    ~@body 
                :quoted '~@body})
         ans# (try (deref fut#
                     10 {:timeout 10})
                  (catch Exception e# {:error 1 :errormsg e#     :expr    ~@body :quoted '~@body}))
         ]
     (future-cancel fut#)
     ans#
   ))

(defmacro my_eval3 [& body]    ;; trying to cancel before we deref to prevent heapspace hits
  `(let [fut# (future
               {:eval   (eval ~@body)
                :expr    ~@body 
                :quoted '~@body})
         cancel# (do (Thread/sleep 10) (future-cancel fut#))
         ]

     (try (conj (deref fut# 10 {:timeout 10}) {:fcanel cancel#})
                  (catch Exception e# {:error 1 :errormsg e#     :expr    ~@body :quoted '~@body  :fcanel cancel#}))
   ))


(defmacro my_eval21 [& body]        ;;uses clojail sandbox, is safe to all known hangs,  Just need to feed a collectin as program, probably a list
  `(let [fut# (future
               {:eval-sb   (sb ~@body)
                :expr    ~@body 
                :quoted '~@body})
         ans# (try (deref fut#
                     50 {:timeout 50 :expr ~@body :quoted '~@body})
                  (catch Exception e# {:error 1 :errormsg e#  :expr ~@body :quoted '~@body}))
         ]
     ;(future-cancel fut#)
     ans#
   ))


(defn my_eval22 [body]     ;;function version also works, but breaks the :quoted key, which is not important
  (let [fut (future
               {:eval-sb   (sb body)  ;sandboxed evaluation
                :expr    body 
                :quoted 'body})
        ans (try (deref fut
                     50 {:timeout 50 :expr    body :quoted 'body :error 1})
                  (catch Exception e {:error 1 :errormsg e     :expr    body :quoted 'body}))
         ]
     ;(future-cancel fut)
     ans
   ))



(defmacro add_timing [& body]    ;Do I really need a macro here??? Yes, nano checkpoints would not see the execution time under normal evaluation 
  `(let [start_nanotime#  (System/nanoTime)
         answer# ~@body
         end_nanotime# (System/nanoTime)]
   (conj answer# { ;:start_nanotime start_nanotime# 
                   ;:end_nanotime end_nanotime# 
                   :eval_nanotime (- end_nanotime# start_nanotime#) })))


;; (dotimes [n 1000000] (add_timing {:foo :booo}))  ~3000ms on the thinkpad...



(def codesinpet '(take 5 (iterate inc 5)))

(defn stackm [x]
  "designe to stack overflow"
  (if (< x 0)
    x
    (stackm (dec x))))
  
;;  (stackm 10000)

;complexity estimates
(count codesinpet)
(count (flatten codesinpet))
(count (str codesinpet))
;better estiate from the spec list




(defn pairoff-pre [list accum match matchd depth]
  "finds the list upto the point the match and matched are paired off
   match is a set of keys matchd eg: 
   :l for list 
   :v for vector
   :m for map
   :s for set
   :r for regex
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

(defn r-pairoff-pre [list  match matchd]
 "looks ahead and returns list until match and matchd are all paired off."
  (reverse (pairoff-pre list nil match matchd nil)))

(defn r-pairoff-post [list  match matchd]
 "looks ahead and returns list until match and matchd are all paired off."
    (let [len (count (pairoff-pre list nil match matchd nil))]
      (drop len list)))
     

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


(defn genprog [partial spec symlookup]
  "v1.2 correct for lists generation with :l, :v and :ld
   consider adding :m for literal map support, will need to deal with bad pairs
   consider adding :s for literal set support, will need to deal with duplicate keys
   It drops data if we :ld past lowest level
  "
  ;(println "partial:" partial " spec:" spec " depth:"depth)
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
               partial
               )
         spec_past_ld symlookup)
    (= specf :v)     
        (genprog
             (cons 
               (vec (genprog (list) (rest spec_pre_ld) symlookup))   ;; vector version
               partial
               )
         spec_past_ld symlookup)
    (and (not (= specf nil)) 
         (not (= specf :ld))
         ;(not (= specf :l))
         )   ;; the sale level add a symbol
           (genprog (cons speclookup partial)  (rest spec) symlookup)
     :else
        (reverse partial))   ;NOTE , this will be interesting when we add maps and sets...
  ))



(defn spec_iterate_dumb [spec keylist]
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
       )
  )
)




(defn spec-depth-pair [spec]
  "returns pairs of the specs and the depth at that spec item
   supports :l :v :ld only "
  (loop [partial '() spec spec depth 0]
    (if (empty? spec)
      (reverse partial)
      (let [specf (first spec)]
        (recur (cons (list specf depth)  partial)
               (rest spec)
               (+ depth (cond 
                          (contains? #{:l :v} specf) 1
                          (= specf :ld) -1
                           :else 0))
       )))))
  
;(spec-depth-pair '(:l :l 1 :ld :ld  1))
;(spec-depth-pair '(:l 1 :ld :ld  1))
;(spec-depth-pair '(:l 1 :v :ld  1))


(defn spec_iterate [spec keylist]
  "v2.0 Iterate on the right, grows on the left, just like normal numbers, but with special carrying for :ld symbol.
   Depth aware based on spec-depth-pair function.
   carrying aware, does not produce silly specs iterations. 
   Should cover the space of all program specs with each correct program being generated once and only once.
   keylist has to have :l :v at the end, and :ld at the very end, althugh they are optional if we do not want to
   generate programs with list, vectors (:l :v), and or if never want to end these (:ld)"
 (let [spec-depth   (reverse (spec-depth-pair spec))]
    (loop [partial '() spec-depth spec-depth carry-r? true]
      (if (empty? spec-depth)
        (if carry-r? 
          (cons (first keylist) partial)   ;we are on the last number and we need to carry, hence grow the spec
          partial)  
        (let [specf  (ffirst spec-depth)
              depth  (second (first spec-depth))
              nex    (second (drop-while #(not(= % specf))  keylist))
              carry? (or (and (= depth 0) (= nex :ld) carry-r?)
                         (and (nil? nex) carry-r?)
                         (and (= partial '()) (= nex :ld))
                         )
              next   (if carry-r?
                            (if carry? (first keylist) nex) 
                            specf)]
          (recur          
                     ;;grow on the left to carry over
                  (cons next partial)
                  (rest spec-depth)
                  carry?
                  )
)))))


;;(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) symlookup)   
;;(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) {}) 

(defn spec_iterate_f [keylist]
  "iterator maker function"
  (fn [x] (spec_iterate x  keylist )))

;
(take 100 (iterate (spec_iterate_f '(1 2 :l :v :ld))  '()  ))


(def spec_iter_defed 
 "example final itterator with a fully qualified keylist" 
  (spec_iterate_f '(1 2 :l :v :ld)))


(list   '(2))
(identity 2)
(vec '(:a 1 :b 2))
(apply hash-map '(:a 1 :b 2))
(hash-map :a 1 :b 2)

(= (spec_iterate '(2 2 1 2)      '(1 2))     '(1 2 2 :l))

(spec_iterate '(:v :v :v :v)  '(1 2 :l :v :ld))
(spec_iterate '(1 :v 2 :ld)  '(1 2 :l :v :ld))
(spec_iterate '(1)  '(1 2 :ld :l :v) )


(defn genprogs1 [howmany startspec keylist symlookup]   ;;keylist could be determined from symlookup  
  (map (fn [x] (genprog nil x symlookup))
    (take howmany (iterate (spec_iterate_f keylist)  startspec ))))



(def symlookup_basicmath
  {1 '+
   2 '-
   3 '*
   4 '/
   5 'x})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prog_wrap [holder prog ]
 (postwalk-replace {::prog prog} holder) )

(defn exemplar-test-list [prog-holder prog x-in]
    (list (prog_wrap prog-holder prog) x-in))

(defn exemplar-in [prog-holder prog x-in y-out] 
  (let [y-ans (time (my_eval22 (exemplar-test-list 
                    prog-holder
                    prog
                    x-in)))]
    y-ans))

(def prog-holder
   '(fn [x] ::prog))

(def sb (sandbox tester :timeout 100 :namespace 'adatx.core))


(exemplar-in prog-holder '(+ x x) 5 :goo)


;  (time (my_eval22 (exemplar-test-list 
;                   '(fn [x] (+ 4 ::prog))
;                   '(+ x x)
;                    5)))
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def prog '(identity x))

(prog_wrap prog-holder prog)

(prog_wrap '(fn [x] (+ 5 ::prog))  '(+ 1 x) )
(let [fun (eval (prog_wrap '(fn [x] (+ 5 ::prog))  '(+ 1 x) ))]
  (fun 1))





(def sb (sandbox tester :timeout 100 :namespace 'adatx.core))






(def x 5)
(pprint (genprogs1 10 '()  '(1 2 3 4 5 :l :v :ld) symlookup_basicmath))

(genprogs1 10 '(1 1 1 1 1 1 1 1 1)  '(1 2 3 4 5 :l) symlookup_basicmath)

(def prog_wrap
  "probel specific program wrap to a specific arity"
  (fn [x] prog))

(defn prep_for_eval [prog]
  (concat '(fn [x]) (list prog))
  )

(prep_for_eval '(+ 1 x)) ;; ->   '(fn [x] (+ 1 x))





(defmacro prep_for_eval2 [prog]
  `(fn [~'z] (+ ~'z ~prog)))
(macroexpand-1 '(prep_for_eval2  1))
(macroexpand-1 '(prep_for_eval2  (+ z 3)))
(prep_for_eval2  1)
((prep_for_eval2  1) 5)
((prep_for_eval2  (+ z 3)) 5)

((prep_for_eval2 (+ x 3)) 5)

(def prog1 (+ x 4))
((prep_for_eval2 prog1) 5)
(macroexpand-1 '(prep_for_eval2 prog2))

(defn foooo [prog xin]    ;; the problem holding
;;;`((~'fn [~'x] (+ ~'x ~prog))  ~xin))
`((~'fn [~'x] ~prog)  ~xin))
(foooo  '(+ x 1)  45)
 (eval (foooo  '(+ x 1) 45))  ;-> y-out to be compared vs examples  ;;this is where safe eval would happen
(let [prog '(* x 6)
      in  56]
  (foooo  prog in))
 





(genprog nil 
         (last 
           (take 100 (iterate spec_iter_defed  '()  ))
               )
         symlookup)


(take 5 (iterate spec_iter_defed  '(1 :v 2 2 :ld :l 2 2 :ld)  ))


(time
 (pprint 
   (map (fn [x] (genprog nil x symlookup))  (take 4 (iterate spec_iter_defed  '(1 :v :v 1)  ))  )
 )
)

(pprint (map (fn [x] (genprog nil x symlookup))  (take 5 (iterate spec_iter_defed  '(2 :v 2 2 :ld :l 1 1 1)   ))))


;(frequencies (map count (take 100000 (iterate spec_iter_defed  '(1 1 1 1 1 1 1)   ))))
;{7 78125, 8 21875}     ;;what would be the saving if we skipped the itterator when depth went below base level?
;{7 37472, 8 62528}     ;;with non silly specs


;(time (frequencies (pmap count (take 10000000 (iterate spec_iter_defed  '(1)   )))))
;;"Elapsed time: 245897.282269 msecs"  (on the thinkpad)
;;{1 4, 2 16, 3 72, 4 336, 5 1600, 6 7712, 7 37472, 8 183104, 9 898432, 10 4422144, 11 4449108} 
;(time (frequencies (map count (take 10000000 (iterate spec_iter_defed  '(1)   )))))
;;"Elapsed time: 157034.765806 msecs"   (on the thinkpad)
;;;{1 4, 2 16, 3 72, 4 336, 5 1600, 6 7712, 7 37472, 8 183104, 9 898432, 10 4422144, 11 4449108}

(genprog nil
   (last (take 170000 (iterate spec_iter_defed  '(1)   )))
  symlookup)

(/ (* 10000 0.05) 60 60)

(def symlookup
  {:l :listgen
   :v :vectorgen
   :ld :depthdown
   1 1
   2 2}
  )


  
(nth (iterate inc 5) 10)
;;(sort (keys symlookup))


;; calls have to be after sb redefinition , else sandbox resets the namespace
;;TODO move sandboxing to dedicated namespace
(def sb (sandbox tester :timeout 100 :namespace 'adatx.core))
(ns-publics 'adatx.core)


(add_timing (my_eval22 (genprog nil '(2 2 5) symlookup)))
(add_timing (my_eval22 '(+ 2 a)))
(class
  (class (:errormsg  (add_timing (my_eval22 '(+ 2 a))))))




