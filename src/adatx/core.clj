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


(defn foo
  "I don't do a whole lot."
  [x & foos]
  (println x "Hello, World!"))

;;Setting up sandbox in clojail
(def tester [(blacklist-symbols #{'alter-var-root})
             (blacklist-objects [java.lang.Thread])]) ; Create a blacklist.
(def sb (sandbox tester :timeout 50 :namespace 'adatx.core))

(:arglists (meta #'foo))  

(let [myfoo (fn [x] (+ 1 x))]
  (meta myfoo))

(foo "d")

(ns-publics 'adatx.core)
  
;(print (ns-refers 'adatx.core))

(defn takeyourtime [x] (future 
             (do (Thread/sleep x) (+ 1 2) 
               )))

(deref (takeyourtime 1) 2 :timedout)


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


(defmacro my_eval21 [& body]        ;;uses clojail sandbox, is safe to all known hangs. Just need to feed a collectin as program, probably a list
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


(defn my_eval22 [body]     ;;function version also works... but breaks the :quoted key, which is not important...
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

  
(macroexpand-1 '(add_timing {:d (+ 1 2)}))


;;--------------------------------------------------------------

;;scratch code starts here


(add_timing {:d (+ 1 2)})

;;(sb '(+ 3 3))


(def codesinpet '(+ 1 2))


(def codesinpet '(let [a 1 
                       b 2]
                   (+ a b)))


  (def codesinpet '(let (vector a 1 
                       b 2)
                   (+ a b)))
       
(quote 
  ;;fails, needs to be literal :-(
(let (vec '(a 1 
                       b 2))
                   (+ a b))  
)

(def codesinpet '(let [a 1
                       b 2
                       f1 (fn [x] (* x x))]
                   (+ a (f1 b))))

(def codesinpet '(let [a {:v 1}
                       b {:w 2}
                       f1 (fn [x] (* x x))]
                   (conj a b)))

(def codesinpet '(take 5 (iterate inc 5)))
;(def codesinpet '(iterate inc 5))  ;; this does blow us up :-( with OutOfMemoryError GC overhead limit exceeded  [trace missing]
;trying to introduce an explicity future cancel

;(class (iterate inc 5))

;(sb '(iterate inc 5))    ;;;Thank you clojail... this is now safe too


(defn stackm [x]
  (if (< x 0)
    x
    (stackm (dec x))))
  
;;  (stackm 10000)

(def codesinpet  '(stackm 10000))  ; shows we are safe to stack overflow
;NOTE we are already safe to timeout, so we should never blow up.

(quote
  (def ans1 (time (add_timing (my_eval22 codesinpet))))    ;;this blows up clojure??!?!?
ans1
  (def ans2 (time (add_timing (my_eval22 '(stackm 1000000)))))
ans2
(def ans3 (time (add_timing (my_eval22 '(iterate inc 5)))))
ans3
)
;(sb '(stackm 1000000))

;(time (add_timing (my_eval21 '(:a 1))))
;(time (add_timing (my_eval21  '(stackm 1000000))))



;complexity estimates
(count codesinpet)
(count (flatten codesinpet))
(count (str codesinpet))

(def a1 '())
(def a2 (cons 'let a1))
(def a3 (cons (vector) a2))
(def a4 (cons (vec (cons 'a (first a3))) (rest a3)))


(quote
;;TODO need to track depth... number , pairs for maps and lets (an optimisation)
;;take a flat list  with special keywords for starting, ending list,vecs,maps...??
(hash-map 'a 'b)
(add_timing (my_eval21 '(hash-map :3 2 5)))
(add_timing (my_eval21 '(+ 1 2)))
(conj {:d (+ 1 2)} {:foo 2 :boo 3})
(macroexpand-1 '(my_eval21 (+ 1 2)))
(let [dothis '(+ 12 3)]
  (my_eval21 dothis))
(let [dothis '(- 12 3)]
  (my_eval dothis))
(let [dothis {:ex '(- 12 3)}]
  (my_eval (:ex dothis)))
(let [dothis {:ex '(/ 12 0)}]
  (my_eval (:ex dothis)))
(let [a1     3
      dothis '(+ 12 a1)]
 (my_eval dothis))
(my_eval
(let [a1     3
      dothis '(+ 12 4)]
  dothis
  ))
(my_eval 3)
(my_eval (+ 12 3))
(eval '(+ 12 3))
(defn safederef [future]
(try
   {:answer (deref future 
                 2 :timeout)
    :fut future
    }
   (catch Exception e  
         {:error 1 :errormsg e}   )))
(safederef (my_eval (do (Thread/sleep 10)  (+ 1 2))))
(macroexpand-1 (my_eval (do (+ 1 2))))


)  ;end quote


;;-----------------------  genlist

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
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 :l  7 :ld 8 9 :ld 10 ) nil)   ; '(10 (9 8 (7) 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 7 :ld 8 9 ) nil)   ; '(9 8 (7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l 5 6 7 8 9 ) nil)       ; '((9 8 7 6 5) 4 3 2 1)
(genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 ) nil)  ;; '(((9 8 (7 6 5))) 4 3 2 1)
(genprog nil '(1 2 3 4 :l :l :l 5 6 7 :ld 8 9 :ld 10 11) nil)  ;; '(( 10 11 (9 8 (7 6 5))) 4 3 2 1)


(genprog '(:a) '(:l 1 :l 2) nil)

(second '(1))

(split-at 4 [1 1 1 1 1 1 1 1])
(drop-while #(not (= % :ld)) '(1 2 3 :ld  4 5 6 :ld 7 8 ))
(drop-while #(not (= % :ld)) '(1 2 3  4 5 6 7 8 ))
(take-while #(not (= % :ld)) '(1 2 3 :ld 4 5 6 7 8 ))

(genprog '((:b ):a) '(:l 1 :l 2) '(:l))

(genprog nil '(1 :l 2) nil)
(genprog nil '(1 3 :l 2 4 :l #{5 4 3} '2 "foo" :goo {:key 'val} :l [3 3 5] ) nil)
(genprog nil '(1 2 3 4 :l 4 5 6 ) nil)



(= #{5 4 3} #{3 4 5})

'((({:key (quote val)} :goo "foo" (quote 2) 5) 4 2) 3 1)
(cons 'nil '(a b 1))
(list)
