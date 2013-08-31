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


(defn prob-solve [prob-holder] 
  (let [prob-holder (preped-prob-holder prob-holder)    
      ;  {init-spec    :init-spec  
      ;   keylist      :keylist
      ;   symlookup    :symlookup
      ;   prog-holder  :prog-holder
      ;   in-out-pairs :in-out-pairs
      ;   sandbox      :sandbox
      ;   timeout      :timeout
      ;   testfun      :testfun
      ;   }     prob-holder    ;;TODO add defaults here
         {:keys [init-spec keylist symlookup prog-holder 
               in-out-pairs sandbox timeout testfun]
          :or {sandbox (prog-eval/make-sb 'adatx.sandboxns) 
               timeout 200}}   prob-holder
         count-in-out-pairs        (count in-out-pairs)
         progs-seq                 (take 1000000 (progseq/genprogs-lazy init-spec keylist symlookup)) ; carefull, this is infinate
         ready-prog-seq            (map (fn [prog] (prog-hold/prog_wrap prog-holder prog )) progs-seq)
         ;;prepforeval-seq         (map (fn [ready-prog] (prepforeval-in-out-pairs  ready-prog in-out-pairs)) ready-prog-seq)
         evaled-maps-seq           (map (fn [ready-prog] (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout)) ready-prog-seq)
         assessed-evaled-maps-seq  (map (fn [evaled-maps] {:assessment (assess-prog-evaled-in-out-pairs evaled-maps testfun) :evaled-maps evaled-maps })  evaled-maps-seq)
         
                         ;; (map (fn       prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout
        ]
    ;;;;TODO need a function that will go and apply :in-out-pairs and collect number of correct answers (thus EVAL)
    
    (doall
      (take 1 
                  (filter (fn [assessed-evaled-map]   
                          (if (= (:errors (:assessment assessed-evaled-map)) 0)    ;;TODO make a debug level function that can control what stuff is shown
                              (println ;(:assessment assessed-evaled-map) 
                                 "errors:"        (:errors        (:assessment assessed-evaled-map))
                                 "total_correct:" (:total_correct (:assessment assessed-evaled-map))
                                 "ready-prog "    (:ready-prog    (first (:evaled-maps assessed-evaled-map)))
                                 ":eval-sb"       (:eval-sb       (first (:evaled-maps assessed-evaled-map)))
                                 ;(try (subs (str (:errormsg (first (:evaled-maps assessed-evaled-map)))) 0 20) (catch Exception e ""))
                                 ;(:expr (first (:evaled-maps assessed-evaled-map))) 
                                 ;assessed-evaled-map
                                 ))
                        (= count-in-out-pairs (:total_correct (:assessment assessed-evaled-map))))
                      assessed-evaled-maps-seq))
      )
    
    
      
    ;;  (realized? assessed-evaled-maps-seq)
))
    
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

