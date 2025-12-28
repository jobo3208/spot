(ns spot.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [spot.core :as core]
            [spot.ratio :refer [rdiv]]))

(def gen-amount (gen/fmap inc gen/nat))

(defn n->participants [n]
  (mapv inc (range n)))

(def gen-participants (gen/fmap (comp n->participants inc) gen/nat))

(def gen-shares (gen/such-that not-empty (gen/vector (gen/fmap inc gen/nat))))

(defn shares->percentages [shares]
  (let [total-shares (reduce + shares)
        ; apportion is used here to ensure percentages sum to 100
        percentages (core/apportion 100 (map #(rdiv (* % 100) total-shares) shares))]
    percentages))

(def gen-percentages (gen/fmap shares->percentages gen-shares))

(def gen-adjustments
  (->> gen-amount
       (gen/fmap #(* % (rand-nth [-1 1])))
       (gen/vector)
       (gen/such-that not-empty)
       (gen/fmap #(conj % 0))))

(def gen-adjustments-and-amount
  (gen/bind gen-adjustments
            (fn [adjs]
              ; not terribly scientific, but guaranteed to be big enough to fit the adjustments
              (let [amount (* (count adjs) (reduce + (map abs adjs)))]
                (gen/tuple (gen/return adjs) (gen/return amount))))))

(defspec test-split-equally
  200
  (prop/for-all [total gen-amount
                 participants gen-participants]
    (let [expense {:amount total
                   :split-method :equally
                   :participants participants}
          amounts (core/split-expense expense)]
      ; amounts sum to total
      (and (= (reduce + amounts) total)
           ; either one distinct amount...
           (or (= (count (distinct amounts)) 1)
               ; ...or two, and they're within a cent of each other
               (and (= (count (distinct amounts)) 2)
                    (<= (abs (apply - (distinct amounts))) 1)))))))

(defspec test-split-by-amount
  200
  (prop/for-all [amounts (gen/such-that not-empty (gen/vector gen-amount))]
    (let [participants (n->participants (count amounts))
          expense {:amount (reduce + amounts)
                   :participants participants
                   :split-method :by-amount
                   :split-params (zipmap participants amounts)}
          amounts' (core/split-expense expense)]
      ; amounts don't change
      (= amounts amounts'))))

(defspec test-split-by-percentage
  200
  (prop/for-all [total gen-amount
                 percentages gen-percentages]
    (let [participants (n->participants (count percentages))
          expense {:amount total
                   :participants participants
                   :split-method :by-percentage
                   :split-params (zipmap participants percentages)}
          amounts (core/split-expense expense)
          expected-amounts (map #(quot (* total %) 100) percentages)]
      (and (= (reduce + amounts) total)
           (every? #(<= (abs %) 1) (map - amounts expected-amounts))))))

(defspec test-split-by-shares
  200
  (prop/for-all [total gen-amount
                 shares gen-shares]
    (let [participants (n->participants (count shares))
          expense {:amount total
                   :participants participants
                   :split-method :by-shares
                   :split-params (zipmap participants shares)}
          amounts (core/split-expense expense)
          expected-amounts (map #(quot (* total %) (reduce + shares)) shares)]
      (and (= (reduce + amounts) total)
           (every? #(<= (abs %) 1) (map - amounts expected-amounts))))))

(defspec test-split-by-adjustment
  200
  (prop/for-all [[adjustments total] gen-adjustments-and-amount]
    (let [participants (n->participants (count adjustments))
          expense {:amount total
                   :split-method :by-adjustment
                   :participants participants
                   :split-params (zipmap participants adjustments)}
          amounts (core/split-expense expense)
          unadjusted-amounts (map - amounts adjustments)]
      (and (= (reduce + amounts) total)
           ; either one distinct amount...
           (or (= (count (distinct unadjusted-amounts)) 1)
               ; ...or two, and they're within a cent of each other
               (and (= (count (distinct unadjusted-amounts)) 2)
                    (<= (abs (apply - (distinct unadjusted-amounts))) 1)))))))

(deftest test-all-split-methods-degenerate-into-equally
  (doseq [method [:equally :by-amount :by-percentage :by-shares :by-adjustment]]
    (is (= (core/split-expense {:split-method method :amount 100 :participants [1 2 3 4]}) [25 25 25 25]))))

(deftest test-partial-params
  (testing "by amount"
    (is (= (core/split-expense
             {:split-method :by-amount
              :amount 100
              :participants [1 2 3]
              :split-params {1 72}})
           [72 14 14])))
  (testing "by percentage"
    (is (= (core/split-expense
             {:split-method :by-percentage
              :amount 100
              :participants [1 2 3]
              :split-params {2 60}})
           [20 60 20])))
  (testing "by shares"
    (is (= (core/split-expense
             {:split-method :by-shares
              :amount 100
              :participants [1 2 3]
              :split-params {3 2}})
           [25 25 50])))
  (testing "by adjustment"
    (is (= (core/split-expense
             {:split-method :by-adjustment
              :amount 100
              :participants [1 2 3 4]
              :split-params {1 10, 2 -10}})
           [35 15 25 25]))))
