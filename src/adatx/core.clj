(ns adatx.core
  ; (:use [clojail.core :only [sandbox]]
  ;      [clojail.testers :only [blacklist-symbols blacklist-objects]]
  ;      [clojure.pprint]
  ;      [clojure.walk]
  ;      [adatx.progseq :as progseq])
 )
;;TODOs 2013-0-24
;TODO DONE move genprog and spec_iterate to their own namespace (and anything supporting them)
;TODO


;;TODO
;inspect correct arrities
;DONE create known function store - symlookup
;create teacher in-out pairs
;DONE create valid s-expression builder
  ;DONE numbering system to generate expressions?
;DONE create safe, execution time aware execute function
;DONE create brute force strategy
   ;DONE create itterator for spec-builder

;Consider a spec comparator function...

;;TODO Identify all the critical errors that only sb can survive
  ;;Work out the sandboxing strategy for batching evaluations, everything always in a sandbox, but with local try catches for speed
  ;;Work out how to fall back to indivual sandboxing of each candidate if a batch fails.
  ;;Know what a critical failour of a batch looks like so that a fall back can be started.
  ;;look to parallelise the batching +fallback execution... should be always fully loaded.

;reseach core.logic to see if logic programs count be found this way
;;DONE Setting up sandbox in clojail



