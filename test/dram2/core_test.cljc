(ns dram2.core-test
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [dram2.core :as qty :refer [Q_]]))

(deftest base-test
  (testing "specs, registry meta"
    (let [q (Q_ 100 "GJ/year")]
      (is (qty/valid? q))
      (is (qty/quantity? q))
      (is (not (nil? (qty/reg q))))))

  (testing "accessors"
    (let [q (Q_ 12 "m**2")
          [m u] (qty/split q)]
      (is (= 12 m))
      (is (= "m**2" u))
      (is (= 12 (qty/mag q)))
      (is (= "m**2" (qty/unit q))))))

(deftest addition-test
  (testing "addition on identical units"
    (let [a (Q_ 12 "ft")
          b (Q_ 44 "ft")
          expected (Q_ (+ 12 44) "ft")
          actual (qty/q+ a b)]
      (is (= expected actual))
      (is (= (qty/reg actual) (qty/reg a))))))
