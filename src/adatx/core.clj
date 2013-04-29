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
  `(future ~@body))

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


(defmacro my_eval21 [& body]
  `(let [fut# (future
               {:eval-sb   (sb ~@body)
                :expr    ~@body 
                :quoted '~@body})
         ans# (try (deref fut#
                     50 {:timeout 50 :expr ~@body :quoted '~@body})
                  (catch Exception e# {:error 1 :errormsg e#     :expr    ~@body :quoted '~@body}))
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
                     50 {:timeout 50 :expr    body :quoted 'body})
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





(add_timing {:d (+ 1 2)})

;;(sb '(+ 3 3))


(add_timing (my_eval (+ 1 2)))
(add_timing (my_eval '(do (+ 1 2) )))


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

(def ans (time (add_timing (my_eval22 codesinpet))))
ans

(sb '(stackm 10000))




(add_timing (my_eval codesinpet))

;complexity estimates
(count codesinpet)
(count (flatten codesinpet))
(count (str codesinpet))



(defn generate_list [& symbols]
  (let [len_symbols (count symbols)]
    len_symbols))

(generate_list 'w)
              

'(a b [23 h])
'()
(cons 'a '(b))
(cons (vector 'a) '[b [8 9]])
(vec (cons (vector 'a) '[b [8 9]]))

(def a1 '())
(def a2 (cons 'let a1))
(def a3 (cons (vector) a2))
(def a4 (cons (vec (cons 'a (first a3))) (rest a3)))


;;TODO need to track depth... number , pairs for maps and lets (an optimisation)
;;take a flat list  with special keywords for starting, ending list,vecs,maps...??
(hash-map 'a 'b)



(add_timing (my_eval '(hash-map :3 2 5)))



(add_timing (my_eval '(+ 1 2)))
  
(conj {:d (+ 1 2)} {:foo 2 :boo 3})


(macroexpand-1 '(my_eval (+ 1 2)))


(let [dothis '(+ 12 3)]
  (my_eval dothis))


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







