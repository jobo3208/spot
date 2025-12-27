(ns spot.cljs
  (:require [clojure.string :as string]
            [malli.error :as me]
            [reagent.core :as r]
            [reagent.dom.client :as rdomc]
            [spot.core :as core]
            [spot.db :as db]))

(defn zpad [n len]
  (let [s (str n)]
    (if (< (count s) len)
      (str (apply str (repeat (- len (count s)) "0")) s)
      s)))

(defn format-amount [amount]
  (str "$" (zpad (quot amount 100) 1) "." (zpad (mod (abs amount) 100) 2)))

(defn format-split-method [split-method]
  (string/replace (name split-method) #"-" " "))

(defn vals-from-multi-select [el]
  (->> (.-selectedOptions el)
       (map #(.-value %))
       (into [])))

(defn to-int [s]
  (let [n (js/parseInt s)]
    (if (js/isNaN n) nil n)))

(defn add-person [state]
  (let [person (get-in state [:ui :person :data])]
    (try
      (-> state
          (update :db db/add-person person)
          (update :ui dissoc :person))
      (catch ExceptionInfo e
        (assoc-in state [:ui :person :errors] (ex-data e))))))

(defn remove-person [state id]
  (try
    (update state :db db/delete-person id)
    (catch ExceptionInfo e
      (assoc-in state [:ui :alert] (-> e ex-data :id)))))

(defn new-expense []
  {:split-method :equally
   :date (.toLocaleDateString (new js/Date) "en-CA")})

(defn add-new-expense [state]
  (if (nil? (get-in state [:ui :expense]))
    (assoc-in state [:ui :expense :data] (new-expense))
    (throw (ex-info "Cannot add expense right now." {}))))

(defn edit-expense [state id]
  (if (nil? (get-in state [:ui :expense]))
    (if-let [expense (get-in state [:db :expenses id])]
      (assoc-in state [:ui :expense :data] expense)
      (throw (ex-info "Expense does not exist." {})))
    (throw (ex-info "Cannot edit expense right now." {}))))

(defn save-expense [state expense]
  (try
    (-> state
        (update :db db/save-expense expense)
        (update :ui dissoc :expense))
    (catch ExceptionInfo e
      (assoc-in state [:ui :expense :errors] (-> e ex-data :data :explain me/humanize)))))

(defn cancel-editing-expense [state]
  (update state :ui dissoc :expense))

(defn delete-expense [state id]
  (update state :db db/delete-expense id))

(defn update-split-method [state new-split-method]
  ; TODO - don't hardcode
  (let [param-keys {:by-amount :split-amounts
                    :by-percentage :split-percentages
                    :by-shares :split-shares
                    :by-adjustment :split-adjustments}
        data (get-in state [:ui :expense :data])
        data (reduce dissoc data (vals param-keys))
        data (assoc data :split-method new-split-method)
        data (if-let [param-key (param-keys new-split-method)]
               (assoc data param-key (vec (repeat (count (:participants data)) nil)))
               data)]
    (assoc-in state [:ui :expense :data] data)))

(defn update-participants [state new-participants]
  (-> state
      (assoc-in [:ui :expense :data :participants] new-participants)
      (update-split-method (get-in state [:ui :expense :data :split-method]))))

(defn summary-card [*state]
  (let [{:keys [db]} @*state
        expenses (vals (:expenses db))
        debts (core/expenses->debts expenses)]
    [:div.card.mt-3.mb-3
     [:div.card-header
      [:h5.card-title "Summary"]]
     [:div.card-body
      [:ul
       (for [[[ower owed] amount] debts]
         [:li
          [:strong (get-in db [:people ower :name])]
          " owes "
          [:strong (get-in db [:people owed :name])]
          " "
          [:strong (format-amount amount)]])]]]))

(defn people-card [*state]
  (let [{:keys [db ui]} @*state]
    [:div.card.mt-3.mb-3
     [:div.card-header
      [:h5.card-title "People"]]
     [:div.card-body
      [:ul.list-inline
       (for [[id person] (sort (:people db))]
         [:li.list-inline-item
          {:data-id id}
          (:name person)
          " "
          [:button.btn.btn-sm.p-0
           {:title "Remove"
            :on-click #(swap! *state remove-person id)}
           [:span.text-danger "\u00d7"]]])]
      (let [{:keys [data errors]} (:person ui)]
       [:div.input-group
        [:input.form-control
         {:type :text
          :class (cond-> [] (:name errors) (conj :is-invalid))
          :value (:name data)
          :on-change #(swap! *state assoc-in [:ui :person :data :name] (-> % .-target .-value))}]
        [:button.btn.btn-outline-primary
         {:on-click #(swap! *state add-person)}
         "Add person"]
        (when (:name errors)
         [:div.invalid-feedback (:name errors)])])]]))

(defn expense-card [*state expense]
  (let [{:keys [db ui]} @*state
        editing-expense (:expense ui)
        debts (core/expenses->debts [expense])]
    [:div.card
     [:div.card-header (:description expense)]
     [:div.card-body
      [:dl.row
       [:dt.col-sm-3 "Description"]
       [:dd.col-sm-9 (:description expense)]
       [:dt.col-sm-3 "Amount"]
       [:dd.col-sm-9 (format-amount (:amount expense))]
       [:dt.col-sm-3 "Date"]
       [:dd.col-sm-9 (:date expense)]
       [:dt.col-sm-3 "Paid by"]
       [:dd.col-sm-9 (get-in db [:people (:payer expense) :name])]
       [:dt.col-sm-3 "Split amongst"]
       [:dd.col-sm-9
        (->> (:participants expense)
             (map #(get-in db [:people % :name]))
             (string/join ", "))]
       [:dt.col-sm-3 "Split method"]
       [:dd.col-sm-9 (format-split-method (:split-method expense))]]
      [:ul
       (for [[[ower owed] amount] debts]
         [:li
          [:strong (get-in db [:people ower :name])]
          " owes "
          [:strong (get-in db [:people owed :name])]
          " "
          [:strong (format-amount amount)]])]
      (when-not editing-expense
        [:div.text-end
         [:button.btn.btn-sm.btn-outline-danger
          {:on-click (fn [_]
                       (when (js/confirm "Are you sure you want to delete this expense?")
                         (swap! *state delete-expense (:id expense))))}
          "Delete"]
         " "
         [:button.btn.btn-sm.btn-outline-primary
          {:on-click #(swap! *state edit-expense (:id expense))}
          "Edit"]])]]))

(defmulti edit-expense-split-method-params
  (fn [*state {:keys [data errors]}]
    (:split-method data)))

(defmethod edit-expense-split-method-params :equally
  [*state expense]
  nil)

(defmethod edit-expense-split-method-params :by-amount
  [*state {:keys [data errors]}]
  (let [{:keys [db ui]} @*state]
    [:div.mb-2
     [:label.form-label "Split amounts"]
     (for [[i id amount] (map vector (range) (:participants data) (:split-amounts data))]
       [:div.row.mb-1
        [:label.col-sm-2.col-form-label (get-in db [:people id :name])]
        [:div.col-sm-10
         [:input.form-control
          {:type :text
           :class (cond-> [] (get-in errors [:split-amounts i]) (conj :is-invalid))
           :on-change #(swap! *state assoc-in [:ui :expense :data :split-amounts i] (-> % .-target .-value))
           :value amount}]
         (when (get-in errors [:split-amounts i])
           [:div.invalid-feedback (first (get-in errors [:split-amounts i]))])]])]))

(defmethod edit-expense-split-method-params :by-percentage
  [*state {:keys [data errors]}]
  (let [{:keys [db ui]} @*state]
    [:div.mb-2
     [:label.form-label "Split percentages"]
     (for [[i id percentage] (map vector (range) (:participants data) (:split-percentages data))]
       [:div.row.mb-1
        [:label.col-sm-2.col-form-label (get-in db [:people id :name])]
        [:div.col-sm-10
         [:input.form-control
          {:type :text
           :on-change #(swap! *state assoc-in [:ui :expense :data :split-percentages i] (-> % .-target .-value))
           :value percentage}]]])]))

(defmethod edit-expense-split-method-params :by-shares
  [*state {:keys [data errors]}]
  (let [{:keys [db ui]} @*state]
    [:div.mb-2
     [:label.form-label "Split shares"]
     (for [[i id shares] (map vector (range) (:participants data) (:split-shares data))]
       [:div.row.mb-1
        [:label.col-sm-2.col-form-label (get-in db [:people id :name])]
        [:div.col-sm-10
         [:input.form-control
          {:type :text
           :on-change #(swap! *state assoc-in [:ui :expense :data :split-shares i] (-> % .-target .-value))
           :value shares}]]])]))

(defmethod edit-expense-split-method-params :by-adjustment
  [*state {:keys [data errors]}]
  (let [{:keys [db ui]} @*state]
    [:div.mb-2
     [:label.form-label "Split adjustments"]
     (for [[i id adjustment] (map vector (range) (:participants data) (:split-adjustments data))]
       [:div.row.mb-1
        [:label.col-sm-2.col-form-label (get-in db [:people id :name])]
        [:div.col-sm-10
         [:input.form-control
          {:type :text
           :on-change #(swap! *state assoc-in [:ui :expense :data :split-adjustments i] (-> % .-target .-value))
           :value adjustment}]]])]))

(defn edit-expense-card [*state]
  (let [{:keys [db ui] :as state} @*state
        {:keys [data errors]} (:expense ui)
        existing-expense (get-in db [:expenses (:id data)])]
    [:div.card.mb-2.border.border-2.border-primary
     [:div.card-header (if existing-expense (:description existing-expense) "New expense")]
     [:div.card-body
      [:div.mb-2
       [:label.form-label "Description"]
       [:input.form-control
        {:type :text
         :class (cond-> [] (:description errors) (conj :is-invalid))
         :value (:description data)
         :on-change #(swap! *state assoc-in [:ui :expense :data :description] (-> % .-target .-value))}]
       (when (:description errors)
        [:div.invalid-feedback (first (:description errors))])]
      [:div.mb-2
       [:label.form-label "Amount"]
       [:input.form-control
        {:type :text
         :class (cond-> [] (:amount errors) (conj :is-invalid))
         :value (:amount data)
         :on-change #(swap! *state assoc-in [:ui :expense :data :amount] (-> % .-target .-value))}]
       (when (:amount errors)
        [:div.invalid-feedback (first (:amount errors))])]
      [:div.mb-2
       [:label.form-label "Date"]
       [:input.form-control
        {:type :date
         :class (cond-> [] (:date errors) (conj :is-invalid))
         :value (:date data)
         :on-change #(swap! *state assoc-in [:ui :expense :data :date] (-> % .-target .-value))}]
       (when (:date errors)
        [:div.invalid-feedback (first (:date errors))])]
      [:div.mb-2
       [:label.form-label "Paid by"]
       [:select.form-control
        {:class (cond-> [] (:payer errors) (conj :is-invalid))
         :on-change #(swap! *state assoc-in [:ui :expense :data :payer] (-> % .-target .-value to-int))}
        [:option "-"]
        (for [[id person] (sort (:people db))]
          [:option
           {:value id
            :selected (= (:payer data) id)}
           (:name person)])]
       (when (:payer errors)
        [:div.invalid-feedback (first (:payer errors))])]
      [:div.mb-2
       [:label.form-label "Split amongst"]
       [:select.form-control
        {:multiple true
         :class (cond-> [] (:participants errors) (conj :is-invalid))
         :on-change #(swap! *state update-participants (mapv to-int (vals-from-multi-select (.-target %))))}
        (for [[id person] (sort (:people db))]
          [:option
           {:value id
            :selected (contains? (set (:participants data)) id)}
           (:name person)])]
       (when (:participants errors)
        [:div.invalid-feedback (first (:participants errors))])]
      [:div.mb-2
       [:label.form-label "Split method"]
       [:select.form-control
        {:on-change #(swap! *state update-split-method (-> % .-target .-value keyword))}
        (for [method (sort (keys (methods core/split-expense)))]
          [:option
           {:value method
            :selected (= method (:split-method data))}
           (format-split-method method)])]]
      (edit-expense-split-method-params *state (:expense ui))
      [:div.card-footer.text-end
       [:button.btn.btn-outline-secondary
        {:on-click #(swap! *state cancel-editing-expense)}
        "Cancel"]
       " "
       [:button.btn.btn-outline-primary
        {:on-click #(swap! *state save-expense data)}
        "Save"]]]]))

(defn expenses-card [*state]
  (let [{:keys [db ui]} @*state
        editing-expense (get-in ui [:expense :data])]
    [:div.card.mb-3
     [:div.card-header
      [:h5.card-title "Expenses"]]
     [:div.card-body
      (when (not editing-expense)
        [:button.btn.btn-outline-primary.mb-3
         {:on-click #(swap! *state add-new-expense)}
         "Add new expense"])
      (when (and editing-expense (nil? (:id editing-expense)))
        (edit-expense-card *state))
      (interpose [:div.mb-3]
       (for [[id expense] (reverse (sort-by first (:expenses db)))]
         (if (= id (:id editing-expense))
           (edit-expense-card *state)
           (expense-card *state expense))))]]))

(defn alert [*state]
  [:<>
   (when-let [msg (get-in @*state [:ui :alert])]
     (js/alert msg)
     (swap! *state update :ui dissoc :alert))])

(defn container [*state]
  [:div.container
   [alert *state]
   (summary-card *state)
   (people-card *state)
   (expenses-card *state)])

(defonce root (rdomc/create-root (.getElementById js/document "root")))

(defonce *state (r/atom {:db db/init-db :ui {}}))

(defn init []
  (rdomc/render root [container *state]))
