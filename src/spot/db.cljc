(ns spot.db
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.util :as mu]
            [spot.core :as core]))

(def init-db {:next-ids {:people 1 :expenses 1}})

(def Expense
  [:map
   [:id {:optional true} :int]
   [:description [:string {:min 1}]]
   [:amount [:int {:type :money :min 1}]]
   [:payer [:int {:min 1}]]
   [:date [:re #"\d{4}-\d{2}-\d{2}"]]
   [:participants [:vector {:min 1} [:int {:min 1}]]]
   [:split-method :keyword]])

(defmulti get-split-params-schema identity)

(defmethod get-split-params-schema :equally
  [_]
  nil)

(defmethod get-split-params-schema :by-amount
  [_]
  [:map-of :int [:maybe [:int {:type :money :min 0}]]])

(defmethod get-split-params-schema :by-percentage
  [_]
  [:map-of :int [:maybe [:int {:min 0 :max 100}]]])

(defmethod get-split-params-schema :by-shares
  [_]
  [:map-of :int [:maybe [:int {:min 0}]]])

(defmethod get-split-params-schema :by-adjustment
  [_]
  [:map-of :int [:maybe [:int {:type :money}]]])

(defn get-expense-schema
  "Return a schema that can be used to validate this expense. This
  feels a bit hacky but allows full polymorphism on split-method."
  [{:keys [split-method]}]
  (let [split-method (if (string? split-method) (name split-method) split-method)]
    (if-let [schema (get-split-params-schema split-method)]
      (mu/assoc Expense :split-params schema)
      Expense)))

(get-expense-schema {:split-method :equally})

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

(defn save-expense
  "Save a new or existing expense. If expense is invalid, raise
  ExceptionInfo, where data is a (possibly nested) map where the keys
  are problematic fields and the values are vectors of error messages."
  [db expense]
  (let [schema (get-expense-schema expense)
        expense (try
                  (m/coerce schema expense)
                  (catch ExceptionInfo e
                    (throw (ex-info "Expense violates schema." (-> e ex-data :data :explain me/humanize)))))
        _       (try
                  (core/split-expense expense)
                  (catch ExceptionInfo _
                    (throw (ex-info "Invalid split params." {:split-params {:* ["Invalid split params."]}}))))
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
