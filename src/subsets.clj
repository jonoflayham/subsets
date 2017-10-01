(ns subsets)

(defn special-inc [values max-values]
  (cond
    (empty? values) nil
    (not= (first values) (first max-values)) (concat [(inc (first values))] (rest values))
    :otherwise (let [special-inc-of-remaining (special-inc (rest values) (rest max-values))]
                 (when (not (empty? special-inc-of-remaining)) (concat [0] special-inc-of-remaining)))))

(defn all-values-up-to [max-values]
  (let [number-of-values (count max-values)
        all-values-at-zero (take number-of-values (repeat 0))]
    (take-while #(not (empty? %)) (iterate #(special-inc % max-values) all-values-at-zero))))

(defn subsets [letters-and-frequency-pairs]
  (let [letters (map first letters-and-frequency-pairs)
        letter-frequencies (map second letters-and-frequency-pairs)
        all-count-permutations (all-values-up-to letter-frequencies)]
    (for [count-permutation all-count-permutations]
      (filter #(not= 0 (second %)) (map vector letters count-permutation)))))

; Evaluate test scenario:
(dorun (map println (subsets [["a" 2] ["b" 2]])))