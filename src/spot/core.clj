(ns spot.core)

(set! *math-context* (java.math.MathContext. 10))

(defn- distribute-total
  "Distribute total (dollars and cents) as evenly as possible cent-wise
  according to quotas, which may be of arbitrary precision."
  [total quotas]
  (let [total-cents (.longValue (* total 100M))
        cent-quotas (map #(* % 100M) quotas)
        lower-quotas (mapv #(.longValue (quot % 1)) cent-quotas)
        remainders (mapv #(mod % 1) cent-quotas)
        remaining-cents (- total-cents (reduce + lower-quotas))
        ; indexes of proportions with highest remainders
        give-remaining-to (->> (range (count quotas))
                               (sort-by (partial nth remainders))
                               (reverse)
                               (take remaining-cents))
        final-cents (reduce
                      #(update %1 %2 inc)
                      lower-quotas
                      give-remaining-to)]
    (mapv #(/ (bigdec %) 100M) final-cents)))

(defmulti split-expense
  "Return split amounts based on :split-method."
  :split-method)

(defmethod split-expense :equal
  [{:keys [amount participants]}]
  (let [n (count participants)
        quotas (repeat n (/ (bigdec amount) n))]
    (distribute-total amount quotas)))

(defmethod split-expense :by-amount
  [{:keys [split-amounts]}]
  split-amounts)

(defmethod split-expense :by-percentage
  [{:keys [amount split-percentages]}]
  (let [proportions (map #(/ % 100M) split-percentages)
        quotas (map (partial * amount) proportions)]
    (distribute-total amount quotas)))

(defmethod split-expense :by-shares
  [{:keys [amount split-shares]}]
  (let [total-shares (bigdec (reduce + split-shares))
        proportions (map #(/ % total-shares) split-shares)
        quotas (map (partial * amount) proportions)]
    (distribute-total amount quotas)))

(defmethod split-expense :by-adjustment
  [{:keys [amount split-adjustments] :as expense}]
  (let [amount-to-divide (- amount (reduce + split-adjustments))
        amounts (split-expense (assoc expense :amount amount-to-divide :split-method :equal))
        quotas (map + amounts split-adjustments)]
    (distribute-total amount quotas)))

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
