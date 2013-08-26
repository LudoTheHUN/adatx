(ns adatx.prob-solve
  (:require [adatx.progseq :as progseq]
            [adatx.prog-hold :as prog-hold]))

;;Bringing it all together....

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


(def prob-holder 
 {:symlookup  symlookup
  :prog-holder '(fn [x1 x2 x3] (+ x1 :adatx.prog-hold/prog))
  :testfun (fn [in out] (= in out))
  :in-out-pairs  in-out-pairs
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



;;Neep prog-eval ns to put this into
(defn prepforeval-in-out-pairs [ready-prog in-out-pairs]
  ;;---- need the executors namespace to homd my_eval stuff
     (map (fn [in-out-pair] (conj in-out-pair {:evalready-prog  (cons ready-prog (:in in-out-pair))})) in-out-pairs)
  )
(prepforeval-in-out-pairs  ready-prog in-out-pairs)
 
  

(defn prob-solve [prob-holder] 
  (let [prob-holder (preped-prob-holder prob-holder)
        {spec :init-spec  
         keylist :keylist
         symlookup :symlookup
         prog-holder :prog-holder
         in-out-pairs :in-out-pairs}     prob-holder
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





