(ns adatx.prog-hold
  (:use [clojure.walk]))


(def prog-holder
  "a prog holder that will take longer then needed to find a specific arity function, do not use.
   NOTE finding veriadic functions is not supported, but would require small alterations to prog_wrap, or a rethink of the prog-holder idea."
   '(fn ::prog))

(def prog-holder
  "a prog holder that will eventually find a potentailly recursive function if myfname symbal is in the keylist and symlookup"
   '(fn myfname ::prog))

(def prog-holder
 "prog-holder that embeds the search within an exisitng AST, here adds x to whatever the function spec the ::prog is"
   '(fn [x] (+ x ::prog)))

(def prog-holder
  "example of the simples two arity prog holder, x and y should be in keylist and symlookup "
   '(fn [x y] ::prog))

(def prog-holder
  "example of the simples single arity prog holder"
   '(fn [x] ::prog))

(defn prog_wrap [prog-holder prog ]
 (postwalk-replace {::prog prog} prog-holder) )

(defn put-prog-in-prog-holder [prog-holder prog x-ins]
    (cons (prog_wrap prog-holder prog) x-ins))




(quote
;;;review , probably should not be here
(defn- exemplar-in [prog-holder prog x-in y-out]
  "WIP evaluates the prog against the x-in data
   TODO need to test against inputs
   TODO need to put think about tests all examplars
   TODO need to think about performance, my_eval22 and sandbox are very slow, need to batch these up within the sandbox
   TODO but fall back to indiviudal tests if something blows up past try catch within the sb."
  (let [y-ans (time (my_eval22 (put-prog-in-prog-holder
                    prog-holder
                    prog
                    x-in)))]
    y-ans))

)