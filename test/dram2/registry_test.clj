(ns dram2.registry-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [dram2.registry :as sut]))

(deftest test-parse-comment-line
  (let [line "# comment"
        actual (sut/parse-registry line)
        expected [:reg]]
    (is (= expected actual))))

(deftest test-parse-prefix-defn
  (testing "no symbol"
    (let [line "micro- = 1e-6"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "micro-"] [:rhs "1e-6"]]]]
      (is (= expected actual))))

  (testing "with unicode symbol"
    (let [line "micro- = 1e-6     = µ-"
          actual (sut/parse-registry line)
          expected [:reg [:defn
                          [:lhs "micro-"]
                          [:rhs "1e-6"]
                          [:rhs "\u00b5-"]]]]
      (is (= expected actual))))

  (testing "with unicode and non-unicode aliases"
    (let [line "micro- = 1e-6  = µ- = μ- = u-"
          actual (sut/parse-registry line)
          expected [:reg
                    [:defn
                     [:lhs "micro-"]
                     [:rhs "1e-6"]
                     [:rhs "\u00b5-"]
                     [:rhs "\u03bc-"]
                     [:rhs "u-"]]]]
      (is (= expected actual))))

  (testing "with double amount, placeholder symbol and alias"
    (let [line "semi- = 0.5 = _ = demi-"
          actual (sut/parse-registry line)
          expected [:reg
                    [:defn
                     [:lhs "semi-"]
                     [:rhs "0.5"]
                     [:rhs "_"]
                     [:rhs "demi-"]]]]
      (is (= expected actual))))

  (testing "with exponent"
    (let [line "kibi- = 2**10"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "kibi-"] [:rhs "2**10"]]]]
      (is (= expected actual))))

  (testing "with trailing comment"
    (let [line "kibi- = 2**10 = Ki- # a trailing comment"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "kibi-"] [:rhs "2**10"] [:rhs "Ki-"]]]]
      (is (= expected actual))))

  (testing "with symbol, aliases, comments"
    (let [line "micro- = 1e-6  = µ- = μ- = u-  # has u00b5 and u03bc"
          actual (sut/parse-registry line)
          expected [:reg [:defn
                          [:lhs "micro-"]
                          [:rhs "1e-6"]
                          [:rhs "\u00b5-"]
                          [:rhs "\u03bc-"]
                          [:rhs "u-"]]]]
      (is (= expected actual)))))

(deftest test-parse-unit-definition
  (testing "symbol and alias"
    (let [line "meter = [length] = m = metre"
          actual (sut/parse-registry line)
          expected [:reg
                    [:defn
                     [:lhs "meter"]
                     [:rhs "[length]"]
                     [:rhs "m"]
                     [:rhs "metre"]]]]
      (is (= expected actual))))

  (testing "relating root unit to root unit"
    (let [line "[area] = [length] ** 2"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "[area]"] [:rhs "[length]" "**" "2"]]]]
      (is (= expected actual))))

  (testing "empty root"
    (let [line "bit = [ ]"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "bit"] [:rhs "[ ]"]]]]
      (is (= expected actual)))

    (let [line "radian = [] = rad"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "radian"] [:rhs "[]"] [:rhs "rad"]]]]
      (is (= expected actual))))

  (testing "offset units"
    (let [line "kelvin = [temperature]; offset: 0 = K = degK = °K = degree_Kelvin = degreeK  # older names supported for compatibility"
          actual (sut/parse-registry line)
          expected [:reg
                    [:defn
                     [:lhs "kelvin"]
                     [:rhs "[temperature];" "offset:" "0"]
                     [:rhs "K"]
                     [:rhs "degK"]
                     [:rhs "°K"]
                     [:rhs "degree_Kelvin"]
                     [:rhs "degreeK"]]]]
      (is (= expected actual)))

    (let [line "degree_Fahrenheit = 5 / 9 * kelvin; offset: 233.15 + 200 / 9 = °F = fahrenheit = degF = degreeF"
          actual (sut/parse-registry line)
          expected [:reg
                    [:defn
                     [:lhs "degree_Fahrenheit"]
                     [:rhs "5" "/" "9" "*" "kelvin;" "offset:" "233.15"]]]]
      (is (= expected actual))))

  (testing "formula relation w unicode"
    (let [line "turn = 2 * pi"
          actual (sut/parse-registry line)
          expected [:reg [:defn [:lhs "turn"] [:rhs "2" "*" "pi"]]]]
      (is (= expected actual)))

    (let [line "turn = 2 * π * radian"
          actual (sut/parse-registry line)
          expected [:reg
                    [:defn
                     [:lhs "turn"]
                     [:rhs "2" "*" "π" "*" "radian"]]]]
      (is (= expected actual)))))

(deftest test-parse-multiple-messy-lines
  (let [lines "
  bit = [ ]
               @group
     # this is a comment
kibi- = 2**10
               @end
               "
        actual (sut/parse-registry lines)
        expected [:reg
                  [:defn [:lhs "bit"] [:rhs "[ ]"]]
                  [:defn [:lhs "kibi-"] [:rhs "2**10"]]]]
    (is (= expected actual))))

(deftest test-parse-default-registry
  (let [default-reg-text sut/short-registry-text
        parse-tree (sut/parse-registry default-reg-text)]
    ;; will be a string if it fails to parse
    (is (vector? parse-tree))))

(deftest test-xf-unit-definition
  (is (= [{:unit "m" :relation "[length]"}]
         (sut/transform-registry [:reg [:defn [:lhs "m"] [:rhs "[length]"]]])))

  (is (= [{:unit "meter" :relation "[length]" :symbol "m" :aliases #{"metre"}}]
         (sut/transform-registry [:reg [:defn
                                        [:lhs "meter"]
                                        [:rhs "[length]"]
                                        [:rhs "m"]
                                        [:rhs "metre"]]]))))

(deftest test-xf-prefix-definition
  (is (= [{:prefix "micro-" :relation "1e-6" :symbol "µ-" :aliases #{"μ-" "u-"}}]
         (sut/transform-registry [:reg [:defn
                                        [:lhs "micro-"]
                                        [:rhs "1e-6"]
                                        [:rhs "\u00b5-"]
                                        [:rhs "\u03bc-"]
                                        [:rhs "u-"]]]))))
