(ns adatx.prob-solve
  (:require [adatx.progseq :as progseq]
            [adatx.prog-hold :as prog-hold]))


(def symlookup {:l :listgen
               :ld :depthdown
               1 '+
               2 '-
               3 '*
               4 '/
               5 'x1
               6 'x2
               7 'x3})




(def prob-holder 
 {:symlookup  symlookup
  :prog-holder '(fn [x1 x2 x3] (+ x1 :adatx.prog-hold/prog))
  :testfun (fn [in out] (= in out))
  :in-out-pairs  [{:in [1 2 3] :out 5}
                  {:in [1 3 3] :out 6}
                  {:in [2 3 3] :out 6}
                  {:in [4 3 3] :out 6}]
 }
)


(defn preped-prob-holder [prob-holder]
  (conj prob-holder {:keylist (keys (:symlookup prob-holder))
                     :init-spec nil}
  ))


;(preped-prog-holder prog-holder)
(quote
(progseq/genprogs-lazy )

(prog-hold/put-prog-in-prog-holder  prog-holder prog x-ins)
)



(defn prob-solve [prob-holder] 
  (let [prob-holder (preped-prob-holder prob-holder)
        {spec :init-spec  
         keylist :keylist
         symlookup :symlookup
         prog-holder :prog-holder}     prob-holder
        ]
    
    ;;;;TODO need a function that will go and apply :in-out-pairs and collect number of correct answers (thus EVAL)
     
   (take 3   (map (fn [prog] (prog-hold/put-prog-in-prog-holder prog-holder prog '(1 2 3)))
                  (progseq/genprogs-lazy spec keylist symlookup)))
             )
             )


(prob-solve prob-holder)
 ; "unimplemented"))





