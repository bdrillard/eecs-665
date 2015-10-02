;;;; Aleksander Eskilson
;;;; EECS 665 - Compiler Construction - Fall 2015
;;;; aeskilson@ku.edu
;;;; NFA to DFA Converter
(ns project-1.core
  (:require [clojure.data.int-map :as i]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.set :refer [union intersection]])
  (:gen-class))

(defn e-apply
  [f xs ys]
  (if (seq ys) (apply f xs ys) xs))

(defn move
  "Returns a set of states reachable from a given set of states on a symbol in the nfa"
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
  "Returns a length-2 vector where the first element is a state to mark, and the
  second element is a map where the keys are non-epsilon transition symbols, and
  the values are sets of states reachable on those symbols"
  [nfa alpha states]
  [states (reduce (fn [t m]
                    (let [sym (first (keys m))
                          move (first (vals m))]
                      (if (contains? t sym)
                        (assoc t sym (union (sym t) move))
                        (into t m))))
                  {}
                  (for [sym (disj alpha :E)
                        :let [move (move nfa states sym)]
                        :when (seq move)]
                    {sym [move (e-closure nfa move)]}))])

(defn convert
  "Returns a transition table representing the dfa converted from a given nfa"
  [nfa]
  (let [mark-states (partial mark-states (:states nfa) (into #{} (:alpha nfa)))
        e-closure (partial e-closure (:states nfa))
        d0 (conj '() (e-closure (:init nfa)))]
    (loop [trans-table [] d-states d0 nodes #{}]
      (if-let [d (peek d-states)]
        (let [next-move (mark-states d)
              new-states (filter #(not (contains? nodes %)) (map second (vals (second next-move))))]
          (recur (conj trans-table next-move)
                 (e-apply conj (pop d-states) (reverse new-states))
                 (e-apply conj nodes new-states)))
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

(defn pretty-print
  "Human readable output"
  [nfa dfa]
  (let [nfa-final (:final nfa)
        sets (map first dfa)
        sets->letters (zipmap sets (map char (range 65 99)))
        dfa-final (into [] (map #(get sets->letters %) 
                                (filter #(seq (intersection % nfa-final)) sets)))
        rdfa (for [move dfa
                   :let [[s m] move]]
               (apply merge {:state (get sets->letters s)} 
                      (map (fn [v] {(first v) (get sets->letters (second (second v)))}) m)))
        header (concat [:state] (filter #(not= :E %) (:alpha nfa)))]
    (println (str "E-closure({I0}) = " (ffirst dfa) " = " (get sets->letters (ffirst dfa))))
    (doseq [move dfa]
      (println (str "\nMark " (get sets->letters (first move))))
      (doseq [trans (second move)]
        (let [k (first trans)
              [m e] (second trans)]
          (println (str (first move) " --" (name k) "--> " m))
          (println (str "E-closure(" m ") = " e " = " (get sets->letters e))))))
    (println (str "\nInitial State: " (get sets->letters (ffirst dfa))))
    (println (str "Final States: " dfa-final))
    (print-table header rdfa)))

(defn -main [& args]
  "Main function - executed by Java jar, takes in piped input nfa file, produces printout dfa"
  (let [raw (line-seq (java.io.BufferedReader. *in*))
        nfa (parse (map #(clojure.string/split % #"\s+") raw))
        dfa (convert nfa)]
    (pretty-print nfa dfa)))
