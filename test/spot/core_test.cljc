(ns spot.core-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
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

(defspec test-split-equal
  200
  (prop/for-all [total gen-amount
                 participants gen-participants]
    (let [expense {:amount total
                   :split-method :equal
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
    (let [expense {:amount (reduce + amounts)
                   :split-method :by-amount
                   :split-amounts amounts}
          amounts' (core/split-expense expense)]
      ; amounts don't change
      (= amounts amounts'))))

(defspec test-split-by-percentage
  200
  (prop/for-all [total gen-amount
                 percentages gen-percentages]
    (let [expense {:amount total
                   :split-method :by-percentage
                   :split-percentages percentages}
          amounts (core/split-expense expense)
          expected-amounts (map #(quot (* total %) 100) percentages)]
      (and (= (reduce + amounts) total)
           (every? #(<= (abs %) 1) (map - amounts expected-amounts))))))

(defspec test-split-by-shares
  200
  (prop/for-all [total gen-amount
                 shares gen-shares]
    (let [expense {:amount total
                   :split-method :by-shares
                   :split-shares shares}
          amounts (core/split-expense expense)
          expected-amounts (map #(quot (* total %) (reduce + shares)) shares)]
      (and (= (reduce + amounts) total)
           (every? #(<= (abs %) 1) (map - amounts expected-amounts))))))

(defspec test-split-by-adjustment
  200
  (prop/for-all [[adjustments total] gen-adjustments-and-amount]
    (let [expense {:amount total
                   :split-method :by-adjustment
                   :participants (n->participants (count adjustments))
                   :split-adjustments adjustments}
          amounts (core/split-expense expense)
          unadjusted-amounts (map - amounts adjustments)]
      (and (= (reduce + amounts) total)
           ; either one distinct amount...
           (or (= (count (distinct unadjusted-amounts)) 1)
               ; ...or two, and they're within a cent of each other
               (and (= (count (distinct unadjusted-amounts)) 2)
                    (<= (abs (apply - (distinct unadjusted-amounts))) 1)))))))
