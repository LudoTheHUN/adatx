(ns adatx.symlookup)


;;TODO  create the sumlookup map based on an array of symbols.

(comment
spec
convert

['+ '- 'x1 'x2]

into

{1 '+ 2 '- 3 '+ 5 'x1 6 'x2 :l :listgen :ld :depthdown :v :vectorgen}

  )


(defn make_symlookup_map_l [symbols_vec]
 (let [number-symbol_pairs (map (fn [x y] [x y]) (range) symbols_vec)
       generated_symlookup_map (reduce (fn [xs x] (conj xs {(first x) (second x)}))  {} number-symbol_pairs)]

   (conj {:l (gensym) :ld (gensym)} generated_symlookup_map )
   ;(conj  {:l :listgen :ld :depthdown} generated_symlookup_map )

))

(defn make_symlookup_map_v [symbols_vec]
 (let [number-symbol_pairs (map (fn [x y] [x y]) (range) symbols_vec)
       generated_symlookup_map (reduce (fn [xs x] (conj xs {(first x) (second x)}))  {} number-symbol_pairs)]

   (conj {:l (gensym) :ld (gensym) :v (gensym)} generated_symlookup_map )
  ; (conj  {:l :listgen :v :vectorgen :ld :depthdown }  generated_symlookup_map)

))

;;eg: (make_symlookup_map ['+ '- 'x1 'x2 'fn])    ;; {:v G__239626, :ld G__239625, :l G__239624, 4 fn, 3 x2, 2 x1, 1 -, 0 +}

