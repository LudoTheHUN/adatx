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

;;TODO Identify all the critical errors that only sb can survive
  ;;Work out the sandboxing strategy for batching evaluations, everything always in a sandbox, but with local try catches for speed
  ;;Work out how to fall back to indivual sandboxing of each candidate if a batch fails.
  ;;Know what a critical failour of a batch looks like so that a fall back can be started.
  ;;look to parallelise the batching +fallback execution... should be always fully loaded.

;reseach core.logic to see if logic programs count be found this way
;;DONE Setting up sandbox in clojail



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


(defn my_eval22 [body]     ;;function version here also works, but breaks the :quoted key, which is not important
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

(defn my_eval22e [body]     ;;function version here also works, but breaks the :quoted key, which is not important
  (let [fut (future
               {:eval-sb   (eval body)  ;sandboxed evaluation
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


(defn genprogs1 [n startspec keylist symlookup]   ;;keylist could be determined from symlookup  
  "WIP generator of many progs , returns the list of progs.
   TODO, should return the list of progs but also the next spec to itterate from on a map"
  (map (fn [x] (genprog nil x symlookup))
    (take n (iterate (spec_iterate_f keylist)  startspec ))))


;;(time (last (genprogs1 50000 '( 1) '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 :l :ld) symlookup)   ))

(def symlookup_basicmath
  {1 '+
   2 '-
   3 '*
   4 '/
   5 'x})

;;Some benchmarks
;;(time (sb '(+ 1 2)))                   ;;~20ms  on Thinkpad
;;(time (+ 1 2))                         ;;~0.086ms  on Thinkpad
;;(time (eval '(+ 1 2)))                 ;;~1.9ms  on Thinkpad
;;(time(deref(future (+ 1 2))))          ;;~0.67s
;;(time(deref(future (eval '(+ 1 2)))))  ;;~2.98ms
;;(time(deref(future (sb   '(+ 1 2)))))  ;;~20ms


;; (* 6000 60 24)
;   (ns-publics 'adatx.core)

;;;;;;;;;;;Prog holder;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; candidate for program holding here.
;;TODO needs to have arbit arrity?

(defn prog_wrap [holder prog ]
 (postwalk-replace {::prog prog} holder) )

(defn exemplar-test-list [prog-holder prog x-in]
    (cons (prog_wrap prog-holder prog) x-in))


;;  (cons (prog_wrap prog-holder '(+ x 10) ) '(10))
;;  (eval (cons (prog_wrap prog-holder '(+ x 10) ) '(10)))
;;  (eval ((fn ([x] (+ x 10)) ([x y] (+ x y 10))) 10 5))
;;  (eval ((fn foo ([x] (+ x 10 (foo 0))) ([x y] (+ x y 10))) 10 5))


(defn exemplar-in [prog-holder prog x-in y-out]
  "WIP evaluates the prog against the x-in data
   TODO need to test against inputs
   TODO need to put think about tests all examplars
   TODO need to think about performance, my_eval22 and sandbox are very slow, need to batch these up within the sandbox
   TODO but fall back to indiviudal tests if something blows up past try catch within the sb."
  (let [y-ans (time (my_eval22 (exemplar-test-list 
                    prog-holder
                    prog
                    x-in)))]
    y-ans))

(def prog-holder
  "a prog holder that will take longer then needed to find a specific arity function, do not use.
   NOTE finding veriadic functions is not supported, but would require small alterations to prog_wrap, or a rethink of the prog-holder idea."
   '(fn ::prog))

(def prog-holder
  "a prog holder that will eventually find a potentailly recursive function if myfname symbal is in the keylist and symlookup"
   '(fn myfname ::prog))

(def prog-holder
 "prog-holder that embeds the search within an exisitng AST, here adds x to whatever the function spec the ::prog is"
   '(fn [x] (+ x ::prog)))

(def prog-holder
  "example of the simples two arity prog holder, x and y should be in keylist and symlookup "
   '(fn [x y] ::prog))

(def prog-holder
  "example of the simples single arity prog holder"
   '(fn [x] ::prog))

(def sb (sandbox tester :timeout 100 :namespace 'adatx.core))


(exemplar-in prog-holder '(+ x x) '(5) :goo)

;;;;;;;;;;;Prog holder end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(quote
  ;;Review of stability under different levels of eval safety.
  
  (time (my_eval22 (exemplar-test-list 
                  '(fn [x] (+ 4 ::prog))
                   '(+ x x)
                    '(5))))
  (time (my_eval2 (exemplar-test-list 
                  '(fn [x] (+ 4 ::prog))
                   '(+ x x)
                    '(5))))
  (time (my_eval2 (exemplar-test-list 
                  '(fn forever[x] (+ 4 (forever 4) ::prog))
                   '(+ x x)
                    '(5))))
  ;; this does not blow up my_eval2 due to a type check...
  (time (my_eval2 (exemplar-test-list 
                  '(fn forever [x] (+ 4 (iterate inc 1) ::prog))
                   '(+ x x)
                    '(5))))
  ;; But this gives us a GC overhead limit with my_eval2 but is ok with my_eval22 by hitting the timeout.
  (time (my_eval22 (exemplar-test-list 
                  '(fn forever [x] (iterate inc 1))
                   '(+ x x)
                    '(5))))
  
  ;;Plan is to pack 1000 my_eval2 calls into a single my_eval22 call....
  (time (my_eval22   '(macroexpand-1 '(my_eval2 (exemplar-test-list   ;;"DO NOT RUN, this will blow up even my_eval22"
                  '(fn forever [x] (iterate inc 1))
                  ;;  '(fn forever [x] ::prog)
                   '(+ x x)
                    '(5))))))
      ;;This cripples the sandbox  and then returns: #<ExecutionException java.util.concurrent.ExecutionException: java.security.AccessControlException: access denied (java.lang.RuntimePermission createSecurityManager
      ;; even on other, valid calls.  Kills the repls and even eclipse.
      
      
(defn makemanytests []
  
      (map (fn [x] (exemplar-test-list 
                  '(fn [x] (+ 4 ::prog))
                   x
                    '(5)))
      
      (genprogs1 100 '(1) '(1 2 :l :ld) {1 '+ 2 'x :l :ld})
      ))


  (defn answers []   (map my_eval22e
   (makemanytests)))
 
  (def sb2 (sandbox tester :timeout 1000 :namespace 'adatx.core))

  
  (defn answers-safe [] (sb2  '(map my_eval22e
   (makemanytests))))
  
  (answers-safe)
        
  
(time 
(pprint (filter #(not (nil? %)) (map :eval-sb (answers-safe))))
)

 (realized? answer)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;(time (frequencies (map count (take 100000 (iterate spec_iter_defed  '(1 1 1 1 1 1 1)   )))))   ;;1284ms on ThinkPad. (v2.0)
;{7 78125, 8 21875}     ;;what would be the saving if we skipped the itterator when depth went below base level?
;{7 37472, 8 62528}     ;;with non silly specs

;;for these key list was tiny, at (1 2 :l :v :ld)
;(time (frequencies (pmap count (take 10000000 (iterate spec_iter_defed  '(1)   )))))
;;"Elapsed time: 245897.282269 msecs"  (on the thinkpad)
;;{1 4, 2 16, 3 72, 4 336, 5 1600, 6 7712, 7 37472, 8 183104, 9 898432, 10 4422144, 11 4449108} 
;(time (frequencies (map count (take 10000000 (iterate spec_iter_defed  '(1)   )))))
;;"Elapsed time: 157034.765806 msecs"   (on the thinkpad)
;;;{1 4, 2 16, 3 72, 4 336, 5 1600, 6 7712, 7 37472, 8 183104, 9 898432, 10 4422144, 11 4449108}

;;(time (frequencies (map count (take 10000000 (iterate (spec_iterate_f '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 :l :v :ld))  '(1)   )))))



(def sb (sandbox tester :timeout 100 :namespace 'adatx.core))


