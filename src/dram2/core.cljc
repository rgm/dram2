(ns dram2.core
  (:require
   [malli.core :as m]))

;; When we don't care about speed, the simplest representation is a 2-item
;; vector of a number representing magnitude, and a string representing the
;; units. The string DSL allows arithmetic safety via dimensional analysis.

(def schema:qty [:tuple number? string?])

(defn valid? [x] (m/validate schema:qty x))

(defn default-registry [] {})

(defn make-quantity [_reg mag unit] ^::qty [mag unit])

(def Q_ (partial make-quantity (default-registry)))

(defn mag [q] (first q))

(defn unit [q] (second q))

(defn split [q] [(mag q) (unit q)])
