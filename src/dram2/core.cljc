(ns dram2.core
  (:require
   [malli.core :as m]
   [taoensso.truss :as tr]))

;; When we don't care about speed, the simplest representation is a 2-item
;; vector of a number representing magnitude, and a string representing the
;; units. The string DSL allows arithmetic safety via dimensional analysis.

(def schema:qty [:tuple number? string?])

(defn default-registry [] {})

(defn make-quantity [reg mag unit] ^{::registry reg} [(double mag) unit])

(def Q_ (partial make-quantity (default-registry)))

(defn mag [q] (first q))

(defn unit [q] (second q))

(defn reg [q] (::registry (meta q)))

(defn split [q] [(mag q) (unit q)])

(defn valid? [x] (m/validate schema:qty x))

(defn quantity? [x] (and (reg x) (valid? x)))

;;
;; arithmetic
;;

(defn commensurable? [a b]
  (and (= (reg a) (reg b))
       (= (unit a) (unit b))))

(defn check-commensurable! [a b]
  (when-not (commensurable? a b)
    (throw (ex-info "quantities not commensurable" {::quantities [a b]}))))

(defn q+ [a b]
  (check-commensurable! a b)
  (let [[m1 u] (split a)
        m2 (mag b)]
    (make-quantity (reg a) (+ m1 m2) u)))

(defn qdiv [a b]
  (cond
    ;; only doing simple same-unit til registries work
    (quantity? b)
    (do (check-commensurable! a b)
        (/ (mag a) (mag b)))
    (number? b)
    (make-quantity (reg a) (/ (mag a) (tr/have (complement zero?) b)) (unit a))

    :else (throw (ex-info "not-implemented" {::quantities [a b]}))))
