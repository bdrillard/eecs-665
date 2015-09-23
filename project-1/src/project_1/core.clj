(ns project-1.core
  (:require [clojure.data.int-map :as i]
            [clojure.set :refer [union]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn move
  "Returns a set of states reachable from given states on a symbol in an nfa"
  [nfa states sym]
  (reduce (fn [c s] (union c (get-in nfa [s sym]))) #{} states))

(defn e-closure
  "Returns the epsilon closure on a set of states"
  [nfa states]
  (let [neighborhood (union states (move nfa states :E))]
    (if (= neighborhood states)
      states
      (e-closure nfa neighborhood))))

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
  (let [mark-states (partial mark-states (:states nfa) (into #{} (:alpha nfa)))
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
                "State"   (assoc d :alpha (map keyword (rest l)))
                (assoc d :states (assoc (:states d) 
                                        (read-string (first l)) 
                                        (zipmap (:alpha d) (map make-set (rest l)))))))
            {:states (i/int-map)}
            lines)))

(defn -main [& args]
  (let [raw (line-seq (java.io.BufferedReader. *in*))
        nfa (parse (map #(clojure.string/split % #"\s+") raw))]
    (pprint (minimize nfa))))
