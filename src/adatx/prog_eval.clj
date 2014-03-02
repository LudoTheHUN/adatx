(ns adatx.prog-eval
     (:use [clojail.core :only [sandbox]]
        [clojail.testers :only [blacklist-symbols blacklist-objects]])
     )



(def tester [(blacklist-symbols #{'alter-var-root})
             (blacklist-objects [java.lang.Thread])]) ; Create a blacklist.

(def timeout 500)

(def sb (sandbox tester :timeout timeout :namespace 'adatx.prog-eval))

(defn make-sb [host-namespace]
  (sandbox tester :timeout timeout :namespace host-namespace))

(defn make-sb_tout [host-namespace timeout]
  (sandbox tester :timeout timeout :namespace host-namespace))

(defn stackm!! [x]
  "designed to throw StackOverflowError"
  (if (< x 0)
    x
    (stackm!! (dec x))))


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


(defn my_eval22e_tout [body timeout]     ;;function version here also works, but breaks the :quoted key, which is not important
  (let [fut (future
               {:eval-sb   (eval body)  ;non sandboxed evaluation
                :expr    body
                :error 0})
        ans (try (deref fut
                     timeout {:timeout timeout :expr    body :quoted 'body :error 1})
                  (catch Exception e {:error 1 :errormsg e     :expr    body :quoted 'body})
                  (finally (future-cancel fut)))
         ]
     ;(future-cancel fut)
     ans
   ))


(defn my_eval22-wtimeout [body timeout]     ;;function version here also works, but breaks the :quoted key, which is not important
  (let [fut (future
               {:eval-sb   (sb body)  ;sandboxed evaluation
                :expr    body
                :error 0})
        ans (try (deref fut
                     timeout         {:timeout timeout :expr body :error 1})
                  (catch Exception e {:error 1 :errormsg e     :expr    body }))
         ]
     ;(future-cancel fut)
     ans
   ))


(defn my_eval22-wtimeout-wsb [body sandbox timeout]     ;;function version here also works, but breaks the :quoted key, which is not important
  (let [fut (future
               {:eval-sb   (sandbox body)  ;sandboxed evaluation
                :expr    body
                :error 0})
        ans (try (deref fut
                     timeout         {:timeout timeout :expr body :error 1})
                  (catch Exception e {:error 1 :errormsg e     :expr    body }))
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





(def evalready-prog '((fn [x1 x2 x3] (+ x1 (identity x2))) 1 2 3))


(defn prog-eval [evalready-prog sandbox timeout]
  "result_of_the_evaluation"
 (if (= :none sandbox)
  (my_eval22e_tout evalready-prog  timeout)
  (my_eval22-wtimeout-wsb evalready-prog sandbox timeout)))





;;(prog-eval evalready-prog sb 100)


;;(prog-eval '(stackm!! 4000) sb 100)




;(prog-eval '(stackm!! 4) 100)

;;(def sb (sandbox tester :timeout timeout :namespace 'adatx.prog-eval))

;(let [sb-l (sandbox tester :timeout timeout :namespace 'adatx.prog-eval)]
;       (sb-l  '(stackm!! 40)))




















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


(defn makemanytests [numtests]

      (map (fn [x] (exemplar-test-list
                  '(fn [x] (+ 4 ::prog))
                   x
                    '(5)))
      (genprogs_n numtests '(1) '(1 2 :l :ld) {1 '+ 2 'x :l :ld})
      ))

  (def answers (map my_eval22e
   (makemanytests 10)))

  (def sb2 (sandbox tester :timeout 1000 :namespace 'adatx.core))

  (defn answers-safe [] (sb2  '(map my_eval22e
   (makemanytests 200))))
   (count (answers-safe))

(time
(pprint (filter #(not (nil? %)) (map :eval-sb (answers-safe))))
)
 (realized? answers)

)


