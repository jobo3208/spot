(ns spot.core
  (:require [spot.ratio :refer [radd rcomp rdiv rmult rquot rrem]]))

(defn apportion
  "Distribute integer total as evenly as possible according to rational quotas."
  [total quotas]
  (let [lower-quotas (mapv rquot quotas)
        remainders (mapv rrem quotas)
        remaining (- total (reduce + lower-quotas))
        ; indexes of proportions with highest remainders
        give-remaining-to (->> (range (count quotas))
                               (sort-by (partial nth remainders) rcomp)
                               (reverse)
                               (take remaining))]
    (reduce
      #(update %1 %2 inc)
      lower-quotas
      give-remaining-to)))

(defmulti split-expense
  "Return split amounts based on :split-method."
  :split-method)

(defmethod split-expense :equally
  [{:keys [amount participants]}]
  (let [n (count participants)
        quotas (repeat n [amount n])]
    (apportion amount quotas)))

(defmethod split-expense :by-amount
  [{:keys [amount participants split-params]}]
  ; Use split-params where populated, split the rest evenly.
  (let [split-amounts (mapv (partial get split-params) participants)
        num-unpopulated (count (filter nil? split-amounts))
        remainder (- amount (reduce + (filter some? split-amounts)))
        unpopulated-quota [remainder num-unpopulated]
        quotas (mapv #(if (some? %) [% 1] unpopulated-quota) split-amounts)]
    (apportion amount quotas)))

(defmethod split-expense :by-percentage
  [{:keys [amount participants split-params]}]
  (let [split-percentages (mapv (partial get split-params) participants)
        num-unpopulated (count (filter nil? split-percentages))
        remainder (- 100 (reduce + (filter some? split-percentages)))
        unpopulated-percentage [remainder num-unpopulated]
        split-percentages (mapv #(if (some? %) [% 1] unpopulated-percentage) split-percentages)
        proportions (map #(rdiv % 100) split-percentages)
        quotas (map (partial rmult amount) proportions)]
    (apportion amount quotas)))

(defmethod split-expense :by-shares
  [{:keys [amount participants split-params]}]
  (let [split-shares (mapv #(get split-params % 1) participants)
        total-shares (reduce + split-shares)
        proportions (map #(rdiv % total-shares) split-shares)
        quotas (map (partial rmult amount) proportions)]
    (apportion amount quotas)))

(defmethod split-expense :by-adjustment
  [{:keys [amount participants split-params] :as expense}]
  (let [split-adjustments (mapv #(get split-params % 0) participants)
        amount-to-divide (- amount (reduce + split-adjustments))
        amounts (split-expense (assoc expense :amount amount-to-divide :split-method :equally))
        quotas (map radd amounts split-adjustments)]
    (apportion amount quotas)))

(defn expense->loans
  [{:keys [payer id participants] :as expense}]
  (let [amounts (split-expense expense)]
    (map
      (fn [participant amount]
        {:lender payer
         :lendee participant
         :expense id
         :amount amount})
      participants
      amounts)))

(defn loan->debt [{:keys [lender lendee amount]}]
  [[lendee lender] amount])

(defn normalize-debt [[[ower owed] amount]]
  (if (> ower owed)
    [[owed ower] (- amount)]
    [[ower owed] amount]))

(defn denormalize-debt [[[ower owed] amount]]
  (if (neg? amount)
    [[owed ower] (- amount)]
    [[ower owed] amount]))

(defn self-debt? [[[ower owed] _]]
  (= ower owed))

(defn loans->debts [loans]
  (->> loans
       (map loan->debt)
       (map normalize-debt)
       (remove self-debt?)
       (group-by first)
       (map (fn [[[ower owed] debts]]
              [[ower owed] (reduce + (map second debts))]))
       (map denormalize-debt)))

(defn expenses->debts [expenses]
  (->> expenses
       (mapcat expense->loans)
       (loans->debts)))
