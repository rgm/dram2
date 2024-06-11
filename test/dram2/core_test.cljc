(ns dram2.core-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [dram2.core :as q :refer [Q_]]))

(deftest base-test
  (testing "specs, registry meta"
    (let [q (Q_ 100 "GJ/year")]
      (is (q/valid? q))
      (is (q/quantity? q))
      (is (not (nil? (q/reg q)))))

    (testing "two quantities defined via Q_ have the same registry"
      (is (= (q/reg (Q_ 10 "ft"))
             (q/reg (Q_ 200 "m**2"))))))

  (testing "accessors"
    (let [q (Q_ 12 "m**2")
          [m u] (q/split q)]
      (is (= 12.0 m))
      (is (= "m**2" u))
      (is (= 12.0 (q/mag q)))
      (is (= "m**2" (q/unit q))))))

(deftest simple-addition-test
  (testing "addition on identical units"
    (let [a (Q_ 12 "ft")
          b (Q_ 44 "ft")
          expected (Q_ (+ 12 44) "ft")
          actual (q/q+ a b)]
      (is (= expected actual))
      (is (= (q/reg actual) (q/reg a))))))

(deftest simple-division-test
  (testing "division on identical units gives a scalar"
    (let [a (Q_ 4 "m")
          b (Q_ 2 "m")
          expected 2.0
          actual (q/qdiv a b)]
      (is (= expected actual))))

  (testing "division with scalar gives a qty"
    (let [a (Q_ 4 "m")
          b 2
          expected (Q_ 2 "m")
          actual (q/qdiv a b)]
      (is (= expected actual))
      (is (= (q/reg actual) (q/reg a)))))

  (testing "division by zero scalar throws"
    (let [a (Q_ 4 "m")]
      (is (thrown? #? (:clj Exception :cljs js/Error) (q/qdiv a 0)))))

  (testing "div by non-identical units throws (for now)"
    (let [a (Q_ (/ 1 3.28) "m/ft")
          b (Q_ 3 "ft")]
      (is (thrown? #? (:clj Exception :cljs js/Error) (q/qdiv a b))))))
