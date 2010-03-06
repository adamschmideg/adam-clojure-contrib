(ns acc.arrayprog
  (:use 
    [clojure.test]))

(with-test
  (defn trans-lom
    "Transpose a list of maps to a map of lists.
     The inner maps are assumed to have the same keys as the first
     map, or a key-set has to be provided.  For example:
      [{:a 1 :b 2}
       {:a 7 :b 8}]

      becomes

      {:a [1 7]
       :b [2 8]}
    "
    ([coll ks]
     (let [init (zipmap ks (repeat []))
           default (zipmap ks (repeat nil))]
        (reduce 
          #(merge-with conj %1 (merge default %2))
          init
          coll)))
    ([coll]
      (trans-lom coll (keys (first coll)))))
  (is (= {:a [1 7] :b [2 8]}
         (trans-lom [{:a 1 :b 2} {:a 7 :b 8}])))
  (is (= {:a [nil 7] :b [2 nil]}
         (trans-lom [{:b 2} {:a 7}] #{:a :b}))))

(with-test
  (defn trans-lol
    [coll]
    (partition
      (count coll)
      (apply interleave coll)))
  (is (= [[1 3 5] [2 4 6]]
         (trans-lol [[1 2] [3 4] [5 6]]))))

(with-test
  (defn trans-mol
    "Transpose a map of lists to a list of maps with the same keys.  The
    inner lists are assumed to have the same length.
    (trans-lom (trans-mol coll)) is identity.
    "
    [coll]
     (let [ks (keys coll)]
       (map #(zipmap ks %)
         (trans-lol (vals coll)))))
  (is (= [{:a 1 :b 2} {:a 7 :b 8}]
         (trans-mol {:a [1 7] :b [2 8]}))))
