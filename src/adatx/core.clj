(ns adatx.core
   (:use [clojail.core :only [sandbox]]
        [clojail.testers :only [blacklist-symbols blacklist-objects]])
   )

;;TODO
;inspect correct arrities
;create known function store
;create teacher in-out pairs
;DONE create valid s-expression builder
  ;DONE numbering system to generate expressions?
;DONE create safe, execution time aware execute function
;create brute force strategy
   ;create itterator for s-builder

;;DONE Setting up sandbox in clojail


;;(sb '(+ 1 2))
;   (ns-publics 'adatx.core)
(def willthissimbolsurvie? :foo)
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
               {:eval-sb   (sb body)
                :expr    body 
                :quoted 'body})
        ans (try (deref fut
                     50 {:timeout 50 :expr    body :quoted 'body :error 1})
                  (catch Exception e {:error 1 :errormsg e     :expr    body :quoted 'body}))
         ]
     ;(future-cancel fut#)
     ans
   ))



(defmacro add_timing [& body]    ;Do I really need a macro here??? Yes, nano checkpoints would not see the execution time under normal evaluation 
  `(let [start_nanotime#  (System/nanoTime)
         answer# ~@body
         end_nanotime# (System/nanoTime)]
   (conj answer# { ;:start_nanotime start_nanotime# 
                   ;:end_nanotime end_nanotime# 
                   :eval_nanotime (- end_nanotime# start_nanotime#) })))



(def codesinpet '(take 5 (iterate inc 5)))

(defn stackm [x]
  (if (< x 0)
    x
    (stackm (dec x))))
  
;;  (stackm 10000)

;complexity estimates
(count codesinpet)
(count (flatten codesinpet))
(count (str codesinpet))



(def spec '(:l 1 2))


(defn pairoff-pre [list accum match matchd depth]
  "finds the list upto the point the match and matched are paired off
   match is a set of keys matchd like 
   :l for list 
   :v for vector
   :m for map
   :s for set
   :r for regex
   and maybe other literals
   is just the one closing off key, typically :ld"
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
     
(r-pairoff-pre '() #{:l} :ld)
(r-pairoff-pre nil #{:l} :ld)
(r-pairoff-pre '(1 2 3 :l 4 5 6 7 8)  #{:l} :ld)      ;-> '()
(r-pairoff-pre '(1 2 3 :l 4 5 :ld 6 7 8) #{:l}:ld)  ;-> '()
(r-pairoff-pre '(:l 4 5 :ld 6 7 8)       #{:l}:ld)  ;-> '(:l 4 5 :ld)
(r-pairoff-pre '(:l 4 :l 5 :ld 6 :ld 7 8) #{:l} :ld)  ;-> '(:l 4 :l 5 :ld 6 :ld)
(r-pairoff-pre '(:l 4 5 :ld 6 :ld 7 8) #{:l} :ld)     ;-> '(:l 4 5 :ld)
(r-pairoff-pre '(:l 4 5 :l 6 :ld 7 8) #{:l} :ld)      ; -> (:l 4 5 :l 6 :ld 7 8)
(r-pairoff-pre '(:l 4 5 :l :l 6 :ld :ld :ld 7 8) #{:l} :ld)     ; (:l 4 5 :l :l 6 :ld :ld :ld)
(r-pairoff-pre '(:l :l :l :l 6 :ld :ld :ld :ld 7 8) #{:l} :ld)  ; (:l :l :l :l 6 :ld :ld :ld :ld)
(r-pairoff-pre '(:l :l :l 6 :ld :ld :ld :l 7 8) #{:l} :ld)      ;-> (:l :l :l 6 :ld :ld :ld)
(r-pairoff-pre '(:l :ld 2 :l 6 :ld :ld :ld :l 7 8) #{:l} :ld)   ;->  (:l :ld)
(r-pairoff-pre '(:l :ld :ld :ld :ld :l 7 8) #{:l} :ld)          ;->  (:l :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :ld 4 5 :l 6 :ld 7 :ld 8 9) #{:l} :ld)   ;-> (:l 1 2 :l 3 :ld 4 5 :l 6 :ld 7 :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :ld :l 6 :ld 7 :ld 8 9) #{:l} :ld)       ;-> (:l 1 2 :l 3 :ld :l 6 :ld 7 :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :ld :l 6 :ld :ld 8 9) #{:l} :ld)         ;-> (:l 1 2 :l 3 :ld :l 6 :ld :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld 8 9) #{:l} :ld)  ;-> (:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) #{:l} :ld)  ;-> (:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld)

(time (r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) #{:l} :ld))


(r-pairoff-post '() #{:l} :ld)
(r-pairoff-post nil #{:l} :ld)
(r-pairoff-post nil #{:l} :ld)
(r-pairoff-post '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) #{:l} :ld)
(r-pairoff-post '(:l 4 5 :ld 6 7 8)        #{:l} :ld)
(r-pairoff-post '(:l :ld 2 :l 6 :ld :ld :ld :l 7 8) #{:l} :ld)


(def symlookup
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
   11 11})


(defn genprog [partial spec symlookup]
  "v1.2 correct for lists generation with :l, :v and :ld"
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
        partial)
  ))



(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) symlookup)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 11 ) symlookup)   ; '(10 (9 8 (7) 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 :ld ) symlookup)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l :l 6 :l  7 :ld 8 9 :l :ld 10 :ld :ld 11 :ld :ld) symlookup)
(genprog nil '(1 2 3 4 :l :l 6 :l  7 :ld 8 9 :ld :ld 10 :l :l :l :ld :ld 11 :ld 8 :ld) symlookup)

(genprog nil '(1 2 3 4 :l :l 6 :l  7 :ld 8 9 :ld :ld 10 :l :l :l :ld :ld 11 :ld 8 :ld) symlookup)
(genprog nil '(1 2 3 4 :v :l 6 :l  7 :ld 8 9 :ld :ld 10 :v :l :l :ld :ld 11 :ld 8 :v) symlookup)


(genprog nil '(:l :ld :ld 1) symlookup)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 ) symlookup)
(genprog nil '(1 2) symlookup)

(genprog nil '(2 2 5) symlookup)
(genprog nil '(2) symlookup)

(list   '(2))
(identity 2)

(vec '(:a 1 :b 2))
(apply hash-map '(:a 1 :b 2))
(hash-map :a 1 :b 2)




(def sb (sandbox tester :timeout 50 :namespace 'adatx.core))

;; calls have to be after sb redefinition 
(time (add_timing (my_eval22 (genprog nil '(2 2 5) symlookup))))
(time (add_timing (my_eval22 '(+ 2 a))))

(class
  (class (:errormsg (time (add_timing (my_eval22 '(+ 2 a)))))))

 (ns-publics 'adatx.core)

