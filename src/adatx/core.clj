(ns adatx.core
   (:use [clojail.core :only [sandbox]]
        [clojail.testers :only [blacklist-symbols blacklist-objects]])
   )

;;TODO
;inspect correct arrities
;create known function store
;create teacher in-out pairs
;create valid s-expression builder
  ;numbering system to generate expressions?
;create safe, execution time aware execute function
;create brute force search strategy

;;Setting up sandbox in clojail


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
  (cond 
     (and (empty? depth) (not (empty? accum)))
       accum
     (= (first list) match)
       (do ;(println "depth up")
         (pairoff-pre (rest list) (cons (first list) accum) match matchd (cons match depth)))
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
     
(r-pairoff-pre '() :l :ld)
(r-pairoff-pre nil :l :ld)
(r-pairoff-pre '(1 2 3 :l 4 5 6 7 8)  :l :ld)      ;-> '()
(r-pairoff-pre '(1 2 3 :l 4 5 :ld 6 7 8)  :l :ld)  ;-> '()
(r-pairoff-pre '(:l 4 5 :ld 6 7 8)        :l :ld)  ;-> '(:l 4 5 :ld)
(r-pairoff-pre '(:l 4 :l 5 :ld 6 :ld 7 8) :l :ld)  ;-> '(:l 4 :l 5 :ld 6 :ld)
(r-pairoff-pre '(:l 4 5 :ld 6 :ld 7 8) :l :ld)     ;-> '(:l 4 5 :ld)
(r-pairoff-pre '(:l 4 5 :l 6 :ld 7 8) :l :ld)      ; -> (:l 4 5 :l 6 :ld 7 8)
(r-pairoff-pre '(:l 4 5 :l :l 6 :ld :ld :ld 7 8) :l :ld)     ; (:l 4 5 :l :l 6 :ld :ld :ld)
(r-pairoff-pre '(:l :l :l :l 6 :ld :ld :ld :ld 7 8) :l :ld)  ; (:l :l :l :l 6 :ld :ld :ld :ld)
(r-pairoff-pre '(:l :l :l 6 :ld :ld :ld :l 7 8) :l :ld)      ;-> (:l :l :l 6 :ld :ld :ld)
(r-pairoff-pre '(:l :ld 2 :l 6 :ld :ld :ld :l 7 8) :l :ld)   ;->  (:l :ld)
(r-pairoff-pre '(:l :ld :ld :ld :ld :l 7 8) :l :ld)          ;->  (:l :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :ld 4 5 :l 6 :ld 7 :ld 8 9) :l :ld)   ;-> (:l 1 2 :l 3 :ld 4 5 :l 6 :ld 7 :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :ld :l 6 :ld 7 :ld 8 9) :l :ld)       ;-> (:l 1 2 :l 3 :ld :l 6 :ld 7 :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :ld :l 6 :ld :ld 8 9) :l :ld)         ;-> (:l 1 2 :l 3 :ld :l 6 :ld :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld 8 9) :l :ld)  ;-> (:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld)
(r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) :l :ld)  ;-> (:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld)

(time (r-pairoff-pre '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) :l :ld))


(r-pairoff-post '() :l :ld)
(r-pairoff-post nil :l :ld)
(r-pairoff-post nil :l :ld)
(r-pairoff-post '(:l 1 2 :l 3 :l :ld :l :ld 6 :ld :ld :ld 8 9) :l :ld)
(r-pairoff-post '(:l 4 5 :ld 6 7 8)        :l :ld)
(r-pairoff-post '(:l :ld 2 :l 6 :ld :ld :ld :l 7 8) :l :ld)


(defn genprog [partial spec depth]
  "WIP works, needs to do much to make it array literal aware"
  (println "partial:" partial " spec:" spec " depth:"depth)
  (let [specf        (first spec)
        depthf       (first depth)
        spec_pre_ld  (r-pairoff-pre spec :l :ld)
        spec_past_ld (let [len (count spec_pre_ld)] (drop len spec))
        second_spec  (second spec)]
    (cond 
      (= depthf :l)
           (cons (genprog  (first partial) spec (rest depth)) (rest partial))
      (= specf :l)     ;;starting a new list
           (genprog    ;;work past return to the same depth level
             (genprog (cons (list) partial) (rest spec_pre_ld) (cons :l depth))   ;;instructions with which to fill the inner list
             spec_past_ld
             depth)
      (and (not (= specf nil)) (not (= specf :ld)))    ;; the sale level add a symbol
           (genprog (cons specf partial)  (rest spec) depth)
      :else
        partial)
  ))

(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 11 ) nil)   ; '(10 (9 8 (7) 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 :ld ) nil)   ; '(9 8 (7 6 5) 4 3 2 1)

(genprog nil '(1 2 3 4 :l :l 6 :l  7 :ld 8 9 :l :ld 10 :ld :ld 11 :ld :ld) nil)

(genprog nil '(1 2 3 4 :l :l 6 :l  7 :ld 8 9 :ld :ld 10 :l :l :l :ld :ld 11 :ld :ld) nil)



(def sb (sandbox tester :timeout 50 :namespace 'adatx.core))   ;;; NOTE sb will snapshot the 'adatx.core namespace as it is at the time we come here, it will be reset to this state with each call to sb

