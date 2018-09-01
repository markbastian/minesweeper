(ns ^:figwheel-hooks minesweeper.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(defn neighbors [[i j]]
  (let [x ((juxt inc identity dec identity) i)
        y ((juxt identity inc identity dec) j)]
    (map vector x y)))

(defn grid-coords [w]
  (set (for [i (range w) j (range w)] [i j])))

(defn mines [n grid-coords]
  (set (take n (shuffle grid-coords))))

(defn warnings [grid mines]
  (->> mines
       (mapcat neighbors)
       frequencies
       (remove (fn [[c]] (mines c)))
       (filter (fn [[c]] (grid c)))
       (into {})))

(defn initial-state []
  (let [grid (grid-coords 20)
        mines (mines 50 grid)
        warnings (warnings grid mines)]
    {:app-state  :main
     :click-mode :select
     :grid       (zipmap
                   grid
                   (map (fn [c] (cond-> {}
                                        (mines c) (assoc :mine :live)
                                        (warnings c) (assoc :warnings (warnings c)))) grid))}))

(defn cell-clear? [[_ {:keys [mine visible?]}]]
  (or visible? (= :detonated mine)))

(defn won? [grid]
  (every? cell-clear? grid))

(defn check-victory-condition [{:keys [grid] :as state}]
  (cond-> state (won? grid) (assoc :app-state :win)))

(defmulti click-cell (fn [{:keys [click-mode]} _] click-mode))

(defmethod click-cell :select [{:keys [grid] :as state} cell]
  (loop [s grid [c & r] [cell] v #{}]
    (if-some [{:keys [mine warnings]} (s c)]
      (let [v (conj v c) s (assoc-in s [c :visible?] true)]
        (cond
          (= :live mine) (-> state
                             (assoc :grid s)
                             (assoc :app-state :loss))
          warnings (recur s r v)
          :default (let [n (->> c neighbors (remove v) (filter s))]
                     (recur s (into r n) v))))
      (check-victory-condition
        (assoc state :grid s)))))

(defmethod click-cell :detonate [{:keys [grid] :as state} cell]
  (let [{:keys [mine visible?]} (grid cell)]
    (cond
      mine (check-victory-condition
             (assoc-in state [:grid cell :mine] :detonated))
      (not visible?) (-> state
                         (assoc :app-state :loss)
                         (assoc-in [:grid cell :error] true))
      :default state)))

(defonce app-state (atom (initial-state)))

(defmulti input-type (comp :click-mode deref))

(defmethod input-type :select [state]
  [:h3 {:onClick #(swap! state assoc :click-mode :detonate)}
   "Select mode (click to enable detonate mode)"])

(defmethod input-type :detonate [state]
  [:h3 {:onClick #(swap! state assoc :click-mode :select)}
   "Detonate mode (click to enable select mode)"])

(defn render-cell [{:keys [mine visible? error]}]
  (cond
    error :black
    (and visible? (= :live mine)) :red
    visible? :green
    (= :detonated mine) :orange
    :default :gray))

(defn render-grid [state]
  (let [{:keys [grid app-state]} @state cell-dim 20]
    [:svg {:width (* cell-dim 20) :height (* cell-dim 22)}
     (doall (for [[[i j :as c] cell] grid]
              [:rect {:key     (str i ":" j)
                      :x       (* i cell-dim)
                      :y       (* j cell-dim)
                      :width   cell-dim
                      :height  cell-dim
                      :stroke  :black
                      :fill    (render-cell cell)
                      :onClick #(when (= :main app-state)
                                  (swap! state click-cell c))}]))
     (doall (for [[[i j] {:keys [visible? warnings]}] grid
                  :when (and warnings visible?)]
              [:text {:key (str i ":" j)
                      :x   (* (+ i 0.25) cell-dim)
                      :y   (* (+ j 0.75) cell-dim)}
               warnings]))]))

(defmulti render (comp :app-state deref))

(defmethod render :loss [state]
  [:div
   [:h3 "You lose!"]
   [:h3 {:onClick #(swap! state (constantly (initial-state)))}
    "Play again?"]
   [render-grid state]])

(defmethod render :win [state]
  [:div
   [:h3 "You win!"]
   [:h3 {:onClick #(swap! state (constantly (initial-state)))}
    "Play again?"]
   [render-grid state]])

(defmethod render :main [state]
  [:div
   [:h3 "Minesweeper"]
   [input-type state]
   [render-grid state]])

(reagent/render-component [render app-state]
                          (. js/document (getElementById "app")))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
