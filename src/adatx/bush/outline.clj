(ns adatx.bush.outline)

;;This is a an exploration of an optimisation of the search


;;imagine you had this problem specification
{ :symvec        ['+ 1]
    :prog-holder   '(fn [] :adatx.prog-hold/prog)
    :testfun       (fn [returned out] (= returned out))
    :in-out-pairs  [{:in [] :out 99}]}

;;solution would be something like (+ 1 1 1 1 ... 1 1)

;;now to get there quickly, you'd need to skip past large parts of the adatx0.1.0 list space. How could you do so? ... by not getting fooled.

;:reachable is something to remember about as a set of things that we have got to in the search space
{:in []  :ready-prog (fn [] +)   :error 0 :eval-sb +      :reachables {+ [+]}}   ;;note that + is not being added here, since it's already in symvec...but this is tested for...
{:in []  :ready-prog (fn [] 1)   :error 0 :eval-sb 1      :reachables {+ [+] 1 [1]}}
{:in []  :ready-prog (fn [] (1)) :error 1 :eval-sb 'e     :reachables {+ [+] 1 [1]}}  ;reachable unchanged
{:in []  :ready-prog (fn [] (+)) :error 0 :eval-sb 0      :reachables {+ [+] 1 [1] 0 [(+)]}}  ;we've reached 0 for the first time, via (+)
;;soon we'll have a diagonal issues....
;;we do (fn [] 0) as it's a new reachable thing, so we try it out....
{:in []  :ready-prog (fn [] 0)   :error 0 :eval-sb 0        :reachables {+ [+] 1 [1] 0 [(+) 0]}} ;not adding 0 since it is already in reachables... adding where it's reachable from, may need to keep a reverse look up?
{:in []  :ready-prog (fn [] (0)) :error 1 :eval-sb 'e        :reachables {+ [+] 1 [1] 0 [(+) 0]}}  ;trying the 1 item posibility, and erring...moving on
;we've exhaused all size 1 and 2 items (eg 1 and (+)), moving to size 3 eg (+ 1)
{:in []  :ready-prog (fn [] (0)) :error 1 :eval-sb 'e        :reachables {+ [+] 1 [1] 0 [(+) 0]}}
{:in []  :ready-prog (fn [] (+ +)) :error 1 :eval-sb 'e        :reachables {+ [+] 1 [1] 0 [(+) 0]}}
{:in []  :ready-prog (fn [] (+ 1)) :error 0 :eval-sb 1         :reachables {+ [+] 1 [1 (+ 1)] 0 [(+) 0]}}
{:in []  :ready-prog (fn [] (+ 0)) :error 0 :eval-sb 0         :reachables {+ [+] 1 [1 (+ 1)] 0 [(+) 0 (+ 0)]}}
;....


;notes
(= + ((fn [] +)))  ;true
(= ((fn [] +)) ((fn [] +)))  ; true ...
(= (fn [] (+ 1)) (fn [] (+ 1)))  ;;false, each fn creates a new function


