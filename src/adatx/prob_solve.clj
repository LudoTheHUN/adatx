(ns adatx.prob-solve
  (:require [adatx.progseq :as progseq]
            [adatx.prog-hold :as prog-hold]
            [adatx.prog-eval :as prog-eval]))


;;Bringing it all together....


;(def sandbox "example quick sandbox"
;  (prog-eval/make-sb (symbol (str *ns*))))


(def symlookup {
               0 'identity
               1 '+
               2 '-
               3 '*
               4 '/
               5 'x1
               6 'x2
               7 'x3
               :l :listgen
               :ld :depthdown
               })


(def in-out-pairs [{:in [1 2 3] :out 5}
                  {:in [1 3 3] :out 6}
                  {:in [2 3 3] :out 6}
                  {:in [4 3 3] :out 6}])



(def timeout 500)



(def prob-holder 
 {:symlookup  symlookup
  :prog-holder '(fn [x1 x2 x3] (+ x1 :adatx.prog-hold/prog))
  :testfun (fn [in out] (= in out))
  :in-out-pairs  in-out-pairs
  :sandbox (prog-eval/make-sb 'adatx.sandboxns)  ;;;(prog-eval/make-sb (symbol (str *ns*)))   
  :timeout 200
 }
)

(def ready-prog  '(fn [x1 x2 x3] (+ x1 (identity x2))))   ;;example generated program ready for running against in out pairs


(defn preped-prob-holder [prob-holder]
  (conj prob-holder {:keylist (concat (filter (fn [x] (not (= x :ld))) (keys (:symlookup prob-holder))) '(:ld))
                     :init-spec nil}
  ))




;(preped-prob-holder prob-holder)
(quote
(progseq/genprogs-lazy )
(prog-hold/put-prog-in-prog-holder  prog-holder prog x-ins)
)



(def ready-prog '(fn [x1 x2 x3] (+ x1 (identity x2)))) 

;;Neep prog-eval ns to put this into
(defn prepforeval-in-out-pairs [ready-prog in-out-pairs]
  ;;---- need the executors namespace to homd my_eval stuff
  (let [ins_into_prog (fn [in-out-pair] (conj in-out-pair {:evalready-prog  (cons ready-prog (:in in-out-pair)) :ready-prog ready-prog }))]
     (map ins_into_prog in-out-pairs))
  )


(prepforeval-in-out-pairs ready-prog in-out-pairs)
(:evalready-prog (first 
                   (prepforeval-in-out-pairs  ready-prog in-out-pairs)
))


(def eval-map (first (prepforeval-in-out-pairs ready-prog in-out-pairs)))


(defn prog-eval-in-out-pairs [ready-prog in-out-pairs sandbox timeout]
      (map  (fn [eval-map]  (conj eval-map (prog-eval/prog-eval  (:evalready-prog eval-map)
                                             sandbox timeout) ))
            (prepforeval-in-out-pairs ready-prog in-out-pairs)))



;;(prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout)
;;(first (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout))
;;
;(Thread/sleep 1000) 
  
;(def evaled-maps (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout))
;(def evaled-map (first evaled-maps))

;(def sandbox "example quick sandbox, note that this needs to be created again just before use since use of sandbox reverts the state of the ns"
;  (prog-eval/make-sb (symbol (str *ns*))))

;(def evaled-maps (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout))

(def testfun (fn [out eval-sb] (> out eval-sb)))

(defn assess-prog-evaled-in-out-pairs [evaled-maps testfun]
      (let [
            errors           (reduce + (map :error evaled-maps))
            testfun-result   (map (fn [evaled-map] (testfun (:out evaled-map) (:eval-sb evaled-map))) evaled-maps)
            first_n_correct  (reduce + (map (fn [x] (if (true? x) 1 0)) (take-while true? testfun-result)))
            total_correct    (reduce + (map (fn [x] (if (true? x) 1 0)) (filter true? testfun-result)))
            ]
        {:testfun-result testfun-result :errors errors :first_n_correct first_n_correct :total_correct total_correct }
        ))

;(assess-prog-evaled-in-out-pairs evaled-maps (:testfun prob-holder))
(assess-prog-evaled-in-out-pairs [{:error 1 :out 10 :eval-sb 3} {:error 1 :out 1 :eval-sb 3}] testfun)


(def assessed-evaled-map "example of result created by prob-solve"
{:assessment {:testfun-result '(true true true true), :errors 0, :first_n_correct 4, :total_correct 4}, 
 :evaled-maps '({:in [1 2 3], :ready-prog (fn [x1 x2 x3] (+ x2 x3)), :evalready-prog ((fn [x1 x2 x3] (+ x2 x3)) 1 2 3), :error 0, :eval-sb 5, :out 5, :expr ((fn [x1 x2 x3] (+ x2 x3)) 1 2 3)} 
                {:in [1 3 3], :ready-prog (fn [x1 x2 x3] (+ x2 x3)), :evalready-prog ((fn [x1 x2 x3] (+ x2 x3)) 1 3 3), :error 0, :eval-sb 6, :out 6, :expr ((fn [x1 x2 x3] (+ x2 x3)) 1 3 3)} 
                {:in [2 3 3], :ready-prog (fn [x1 x2 x3] (+ x2 x3)), :evalready-prog ((fn [x1 x2 x3] (+ x2 x3)) 2 3 3), :error 0, :eval-sb 6, :out 6, :expr ((fn [x1 x2 x3] (+ x2 x3)) 2 3 3)} 
                {:in [4 3 3], :ready-prog (fn [x1 x2 x3] (+ x2 x3)), :evalready-prog ((fn [x1 x2 x3] (+ x2 x3)) 4 3 3), :error 0, :eval-sb 6, :out 6, :expr ((fn [x1 x2 x3] (+ x2 x3)) 4 3 3)})
 :counter 119})


(defn log-assessed-evaled-map [assessed-evaled-map loglevel]
    (if (and (= (:errors (:assessment assessed-evaled-map)) 0)   (> loglevel 0))   ;;TODO make a debug level function that can control what stuff is shown
         (println  (if (> loglevel 1)  (str " counter: "       (:counter  assessed-evaled-map) ""))
                   (if (> loglevel 4)  (str " errors: "        (:errors        (:assessment assessed-evaled-map))) "")
                   (if (> loglevel 3)  (str " total_correct: " (:total_correct (:assessment assessed-evaled-map))) "")
                   (if (> loglevel 2)  (str " eval-sb: "       (:eval-sb       (first (:evaled-maps assessed-evaled-map))))"")
                   (if (> loglevel 1)  (str " ready-prog: "    (:ready-prog    (first (:evaled-maps assessed-evaled-map))))"")
                   ))
)

;(log-assessed-evaled-map assessed-evaled-map)
 ;(try (subs (str (:errormsg (first (:evaled-maps assessed-evaled-map)))) 0 20) (catch Exception e ""))
                                 ;(:expr (first (:evaled-maps assessed-evaled-map))) 
                                 ;assessed-evaled-map

(defn prob-solve [prob-holder] 
  (let [prob-holder (preped-prob-holder prob-holder)    

         {:keys [init-spec keylist symlookup prog-holder 
                 in-out-pairs sandbox timeout testfun maxprogs loglevel]
          :or {sandbox (prog-eval/make-sb 'adatx.sandboxns) 
               timeout 200 
               maxprogs 1000000
               loglevel 1}}   prob-holder
         count-in-out-pairs        (count in-out-pairs)
         progs-seq                 (take maxprogs (progseq/genprogs-lazy init-spec keylist symlookup)) ; carefull, this is infinate
         ready-prog-seq            (map (fn [prog] (prog-hold/prog_wrap prog-holder prog )) progs-seq)
         ;;prepforeval-seq         (map (fn [ready-prog] (prepforeval-in-out-pairs  ready-prog in-out-pairs)) ready-prog-seq)
         evaled-maps-seq           (map (fn [ready-prog] (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout)) ready-prog-seq)
         assessed-evaled-maps-seq  (map (fn [evaled-maps counter] {:assessment (assess-prog-evaled-in-out-pairs evaled-maps testfun) :evaled-maps evaled-maps :counter counter })  evaled-maps-seq (range))
         
                         ;; (map (fn       prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout
        ]
    ;;;;TODO need a function that will go and apply :in-out-pairs and collect number of correct answers (thus EVAL)
    
    (doall
      (take 1 
                  (filter (fn [assessed-evaled-map]   
                          (log-assessed-evaled-map assessed-evaled-map loglevel)
                          (= count-in-out-pairs (:total_correct (:assessment assessed-evaled-map))))
                      assessed-evaled-maps-seq)))))
    
;(prob-solve prob-holder)



(quote 
(prob-solve 
 {:symlookup {1 '+ 2 '- 5 'x1 6 'x2 :l :listgen :ld :depthdown}
  :prog-holder '(fn [x1 x2] :adatx.prog-hold/prog)
  :testfun (fn [in out] (= in out))
  :in-out-pairs  [{:in [1 2] :out 4}
                  {:in [1 3] :out 5}
                  {:in [2 3] :out 7}
                  {:in [4 3] :out 11}]
  :sandbox (prog-eval/make-sb 'adatx.sandboxns)  ;;;(prog-eval/make-sb (symbol (str *ns*)))   
  :timeout 200
 })

)

