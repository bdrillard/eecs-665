(ns project-1.core
  (:require [clojure.data.int-map :as i]
            [clojure.set :refer [union]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def test-nfa
  {:alpha #{:E :b :a}
   :total 11
   :final #{11}
   :init #{1}
   :states (into (i/int-map) {1 {:E #{2 5} :b #{} :a #{}}
                              2 {:E #{} :b #{} :a #{3}}
                              3 {:E #{} :b #{4} :a #{}}
                              4 {:E #{8} :b #{} :a #{}}
                              5 {:E #{} :b #{6} :a #{}}
                              6 {:E #{} :b #{} :a #{7}}
                              7 {:E #{8} :b #{} :a #{}}
                              8 {:E #{11 9} :b #{} :a #{}}
                              9 {:E #{} :b #{} :a #{10}}
                              10 {:E #{11 9} :b #{} :a #{}}
                              11 {:E #{} :b #{} :a #{}}})})

(defn move
  "Returns a set of states reachable from given states on a symbol in an nfa"
  [nfa states sym]
  (reduce (fn [c s] (union c (get-in nfa [s sym]))) #{} states))

(defn e-closure
  "Returns the epsilon closure on a set of states"
  [nfa states]
  (let [neighbors (move nfa states :E)]
    (if (empty? neighbors)
      states
      (union states (e-closure nfa neighbors)))))

(defn mark-states
  [nfa alpha states]
  [states (reduce (fn [t m]
                    (let [sym (first (keys m))
                          move (first (vals m))]
                      (if (contains? t sym)
                        (assoc t sym (union (sym t) move))
                        (into t m))))
                  {}
                  (for [state states
                        sym (disj alpha :E)
                        :let [move (move nfa #{state} sym)]
                        :when (seq move)]
                    {sym (e-closure nfa move)}))])

(defn minimize
  [nfa]
  (let [mark-states (partial mark-states (:states nfa) (:alpha nfa))
        e-closure (partial e-closure (:states nfa))
        d0 (conj [] (e-closure (:init nfa)))]
    (loop [trans-table [] d-states d0 nodes #{}]
      (if-let [d (peek d-states)]
        (let [next-move (mark-states d)
              new-states (filter #(not (contains? nodes %)) 
                                 (vals (second next-move)))]
          (if (seq new-states)
            (recur (conj trans-table next-move)
                   (apply conj (pop d-states) new-states)
                   (apply conj nodes new-states))
            (recur (conj trans-table next-move)
                   (pop d-states)
                   nodes)))
        trans-table))))

(defn parse
  "Returns a dictionary representation of the parsed nfa file"
  [lines]
  (let [make-set #(read-string (str \# %))]
    (reduce (fn [d l]
              (case (first l)
                "Initial" (assoc d :init (make-set (last l)))
                "Final"   (assoc d :final (make-set (last l)))
                "Total"   (assoc d :total (read-string (last l)))
                "State"   (assoc d :alpha (into #{} (map keyword (rest l))))
                (assoc d :states (assoc (:states d) 
                                        (read-string (first l)) 
                                        (zipmap (:alpha d) (map make-set (rest l)))))))
            {:states (i/int-map)}
            lines)))

(defn -main [& args]
  (let [raw (line-seq (java.io.BufferedReader. *in*))
        nfa (parse (map #(clojure.string/split % #"\s+") raw))]
    (pprint (minimize nfa))))