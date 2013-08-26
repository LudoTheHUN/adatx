(ns adatx.prob-solve
  (:require [adatx.progseq :as progseq]
            [adatx.prog-hold :as prog-hold]
            [adatx.prog-eval :as prog-eval]))


;;Bringing it all together....


(def sandbox "example quick sandbox"
  (prog-eval/make-sb (symbol (str *ns*))))


(def symlookup {:l :listgen
               :ld :depthdown
               0 'identity
               1 '+
               2 '-
               3 '*
               4 '/
               5 'x1
               6 'x2
               7 'x3})


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
  :sandbox sandbox
 }
)

(def ready-prog  '(fn [x1 x2 x3] (+ x1 (identity x2))))   ;;example generated program ready for running against in out pairs


(defn preped-prob-holder [prob-holder]
  (conj prob-holder {:keylist (keys (:symlookup prob-holder))
                     :init-spec nil}
  ))




;(preped-prog-holder prog-holder)
(quote
(progseq/genprogs-lazy )
(prog-hold/put-prog-in-prog-holder  prog-holder prog x-ins)
)



(def ready-prog '(fn [x1 x2 x3] (+ x1 (identity x2)))) 

;;Neep prog-eval ns to put this into
(defn prepforeval-in-out-pairs [ready-prog in-out-pairs]
  ;;---- need the executors namespace to homd my_eval stuff
  (let [ins_into_prog (fn [in-out-pair] (conj in-out-pair {:evalready-prog  (cons ready-prog (:in in-out-pair))}))]
     (map ins_into_prog in-out-pairs))
  )

(prepforeval-in-out-pairs ready-prog in-out-pairs)
(:evalready-prog (first 
                   (prepforeval-in-out-pairs  ready-prog in-out-pairs)
))

(def eval-map (first (prepforeval-in-out-pairs ready-prog in-out-pairs)))


(defn prog-eval-in-out-pairs [ready-prog in-out-pairs sandbox timeout]
      (map  (fn [eval-map]  (conj eval-map (prog-eval/prog-eval (:evalready-prog eval-map) sandbox timeout) ))
            (prepforeval-in-out-pairs ready-prog in-out-pairs)))


;;(prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout)
;;(first (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout))
;;
;(Thread/sleep 1000) 
  
;(def evaled-maps (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout))
;(def evaled-map (first evaled-maps))

(def sandbox "example quick sandbox, not that this needs to be created again just before use since use of sandbox reverts the state of the ns"
  (prog-eval/make-sb (symbol (str *ns*))))

(def evaled-maps (prog-eval-in-out-pairs ready-prog in-out-pairs sandbox timeout))

(defn assess-prog-evaled-in-out-pairs [evaled-maps testfun]
      (let [
            errors           (reduce + (map :error evaled-maps))
            testfun-result   (map (fn [evaled-map] (testfun (:in evaled-map) (:out evaled-map))) evaled-maps)
            first_n_correct  (reduce + (map (fn [x] (if (true? x) 1 0)) (take-while true? testfun-result)))
            total_correct    (reduce + (map (fn [x] (if (true? x) 1 0)) (filter true? testfun-result)))
            ]
        {:testfun-result testfun-result :errors errors :first_n_correct first_n_correct :total_correct total_correct }
        ))

(assess-prog-evaled-in-out-pairs evaled-maps (:testfun prob-holder))






(defn prob-solve [prob-holder] 
  (let [prob-holder (preped-prob-holder prob-holder)
        {spec         :init-spec  
         keylist      :keylist
         symlookup    :symlookup
         prog-holder  :prog-holder
         in-out-pairs :in-out-pairs
         sandbox      :sandbox
         timeout      :timeout}     prob-holder    ;;TODO add defaults here
        ]
    ;;;;TODO need a function that will go and apply :in-out-pairs and collect number of correct answers (thus EVAL)
   (take 1   (map (fn [prog] 
                   ;;TODO need to apply ins, test outputs, find correct solutions
                   ;dont do this (prog-hold/put-prog-in-prog-holder prog-holder prog '(1 2 3)))
                   (cons (prog-hold/prog_wrap prog-holder prog ) (:in (second in-out-pairs )) ))  ;;WIP ;;first get the ready to run_prog, then add the in
                  (progseq/genprogs-lazy spec keylist symlookup)))
             )
             )



 ; "unimplemented"))





