(ns spot.db
  (:require [malli.core :as m]
            [malli.transform :as mt]))

(def init-db {:next-ids {:people 1 :expenses 1}})

(def Expense
  [:map
   [:id {:optional true} :int]
   [:description [:string {:min 1}]]
   [:amount [:int {:min 1}]]
   [:payer [:int {:min 1}]]
   [:date [:re #"\d{4}-\d{2}-\d{2}"]]
   [:participants [:vector {:min 1} [:int {:min 1}]]]
   [:split-method :keyword]
   [:split-params {:optional true} [:map-of :int :int]]])

(defn add-person [db {:keys [name]}]
  (cond
    (empty? name)
    (throw (ex-info "Could not add person." {:name "Name cannot be blank."}))

    (some #{name} (map :name (vals (:people db))))
    (throw (ex-info "Could not add person." {:name "Name already in use."}))

    :else
    (let [id (get-in db [:next-ids :people])]
      (-> db
          (assoc-in [:people id] {:id id :name name})
          (update-in [:next-ids :people] inc)))))

(defn delete-person [db id]
  (cond
    (not (contains? (:people db) id))
    (throw (ex-info "Could not delete person." {:id "Person does not exist."}))

    (contains? (into #{} (mapcat :participants (-> db :expenses vals))) id)
    (throw (ex-info "Could not delete person." {:id "Person is involved in one or more expenses."}))

    :else
    (update db :people dissoc id)))

(defn save-expense [db expense]
  (let [expense (m/coerce Expense expense mt/string-transformer)
        new?    (nil? (:id expense))
        next-id (get-in db [:next-ids :expenses])
        id      (if new? next-id (:id expense))
        next-id (if new? (inc next-id) next-id)
        expense (if new? (assoc expense :id id) expense)]
    (-> db
        (assoc-in [:expenses id] expense)
        (assoc-in [:next-ids :expenses] next-id))))

(defn delete-expense [db id]
  (if (get-in db [:expenses id])
    (update db :expenses dissoc id)
    (throw (ex-info "Could not delete expense." {:id "Expense does not exist."}))))
