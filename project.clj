(defproject adatx "0.1.0-SNAPSHOT"
  :description "A Clojure library designed for Automatic Design of Algorithms Through X."
  :url "https://github.com/LudoTheHUN/adatx"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojail "1.0.6"]
                ; [dire "0.5.2"]       ;;to see if we can optimise program search by jacking into the evaluation flow.
                 ]
  :plugins [
            [quickie "0.2.5"]
            [lein-cloverage "1.0.2"]
            ]
  :main adatx.core)
