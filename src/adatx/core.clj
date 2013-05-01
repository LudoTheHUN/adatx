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

(defn genprog [partial spec depth]
  (println "partial:" partial " spec:" spec " depth:"depth)
  (let [specf        (first spec)
        depthf       (first depth)
        spec_past_ld (drop-while #(not (= % :ld)) spec)
        spec_pre_ld  (take-while #(not (= % :ld)) spec)]
    (cond 
    ;  (= specf :ld)
    ;       nil
      (= depthf :l)
           (cons (genprog  (first partial) spec (rest depth)) (rest partial))
      (= specf :l)
           (genprog
             (genprog (cons (list) partial) (rest spec) (cons :l depth))   ;;ok from the right
             (rest spec_past_ld)
             depth)      
      (not (= specf nil))
           (genprog (cons specf partial)  (rest spec) depth)
      :else
        partial)
  ))

;;;   :l make a list   :ld  list down  

;;Test worthy
(quote 
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 ) nil)   ; '(10 (9 8 (7) 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 7 8 9 ) nil)       ; '((9 8 7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 ) nil)  ;; '(((9 8 (7 6 5))) 4 3 2 1)
(genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 :ld 10 11) nil)  ;; '(( 10 11 (9 8 (7 6 5))) 4 3 2 1)

)

;;Defining sb again at the end so we do not lose all out defined functions above with each call to sb
(def sb (sandbox tester :timeout 50 :namespace 'adatx.core))   ;;; NOTE sb will snapshot the 'adatx.core namespace as it is at the time we come here, it will be reset to this state with each call to sb


