(ns spot.db-test
  (:require [clojure.test :refer [deftest is]]
            [spot.db :as sdb]))

(def test-db
  {:next-ids {:people 3 :expenses 2}
   :people {1 {:name "Alice"}
            2 {:name "Bob"}}
   :expenses {1 {:id 1
                 :description "scrub daddy"
                 :amount 329
                 :date "2026-01-02"
                 :payer 1
                 :participants [1 2]
                 :split-method :equally}}})

(deftest can-save-new-expense
  (let [expense {:description "oven cleaner"
                 :amount 899
                 :date "2026-01-02"
                 :payer 1
                 :participants [1 2]
                 :split-method :equally}
        expected (assoc expense :id 2)
        db' (sdb/save-expense test-db expense)]
    (is (= (get-in db' [:expenses 2]) expected))))

(deftest can-edit-existing-expense
  (let [expense (get-in test-db [:expenses 1])
        expense' (assoc expense :amount 429)
        db' (sdb/save-expense test-db expense')]
    (is (= (get-in db' [:expenses 1]) expense'))))
