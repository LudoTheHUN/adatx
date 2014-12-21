(ns adatx.bush.outline)

;;This is a an exploration of an optimisation of the search


;;imagine you had this problem specification
{ :symvec        ['+ 1]
    :prog-holder   '(fn [x1 x2] :adatx.prog-hold/prog)
    :testfun       (fn [returned out] (= returned out))
    :in-out-pairs  [{:in [] :out 99}]}

;;solution would be something like (+ 1 1 1 1 ... 1 1)

;;now to get there quickly, you'd need to skip past large parts of the adatx0.1.0 list space.


