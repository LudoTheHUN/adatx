(ns adatx.prog-eval-test
   (:use [clojure.test])
   (:require [adatx.prog-eval :as prog-eval]))





(def sandbox (prog-eval/make-sb (symbol (str *ns*))))

(defn myadd [x1 x2] (+ x1 x2))

(deftest test-prog-eval

(is (=
      (prog-eval/prog-eval '(prog-eval/stackm!! 40) sandbox 1000)  {:eval-sb -1, :expr '(prog-eval/stackm!! 40), :error 0}
      ))

(is (=
      (prog-eval/prog-eval '(myadd 40 50) sandbox 1000)            {:eval-sb 90, :expr '(myadd 40 50), :error 0}
      ))

)
