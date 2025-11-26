(ns repl
  (:use spot.core))

; :deps {metosin/malli {:mvn/version "0.19.2"}}

(def Expense
  [:map
   [:id :int]
   [:payer :int]
   [:description :string]
   [:amount decimal?]
   [:participants [:vector :int]]])

(def Loan
  [:map
   [:lender :int]
   [:lendee :int]
   [:expense :int]
   [:amount decimal?]])

(def Debt
  [:cat
   [:schema [:cat :int :int]]
   decimal?])

(comment
  (def expenses [{:id 1
                  :amount 112.44M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]}])

  (def loans [{:lender 1 :lendee 1 :expense 1 :amount 37.48M}
              {:lender 1 :lendee 2 :expense 1 :amount 37.48M}
              {:lender 1 :lendee 3 :expense 1 :amount 37.48M}])

  (= loans (expense->loans (first expenses)))

  (def expenses [{:id 1
                  :amount 112.44M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]}
                 {:id 2
                  :amount 30.12M
                  :payer 2
                  :description "Rook"
                  :participants [1 2 3 4 5]}])

  (def loans (mapcat expense->loans expenses))

  (loans->debts loans)

  (def debts (expenses->debts expenses))

  (distribute-amount 10M 3)

  (m/validate Expense (first expenses))

  (m/validate Loan (first loans))

  (m/validate Debt (first debts))

  ; perhaps this distinction is useful:
  ;   - a *loan* is tied to an expense
  ;   - a *debt* is an amount of money owed to someone else that isn't necessarily directly tied to one expense
  ;   - we basically resolve expenses into loans, and loans into debts

  ; debt [[x y] a] is always read "x owes y amount a"

  (mi/collect!)
  (mi/instrument!))

(comment
  (split-expense {:id 1
                  :amount 112.45M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]
                  :split-method :equal})

  (split-expense {:id 1
                  :amount 112.44M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]
                  :split-method :by-amount
                  :split-amounts [100M 6.22M 6.22M]})

  (split-expense {:id 1
                  :amount 112.44M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]
                  :split-method :by-percentage
                  :split-percentages [70M 20M 10M]})

  (split-expense {:id 1
                  :amount 112.44M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]
                  :split-method :by-shares
                  :split-shares [3 1 1]})

  (split-expense {:id 1
                  :amount 112.44M
                  :payer 1
                  :description "Pour House"
                  :participants [1 2 3]
                  :split-method :by-adjustment
                  :split-adjustments [5M 0M 0M]}))
