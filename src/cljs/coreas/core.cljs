(ns coreas.core
    (:require [reagent.core :as reagent :refer [atom cursor]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [cljsjs.react :as react]
              [cljs.core.async :as ca :refer [chan put! timeout]])
    (:require-macros
     [cljs.core.async.macros :refer [go alt!]])
    (:import goog.History))

;; -------------------------
;; Views
(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
      (fn [e] (put! out e)))
    out))

(defn img-wrapper [img]
  (reagent/create-class
   {:component-did-mount #(-> % (.getDOMNode) (.appendChild img))
    :reagent-render (fn [img] [:span])}))


(defn canvas-comp [attrs & props]
  (reagent/create-class

   {:paint (or (:paint attrs) (fn [this] (print "default paint")))
    :component-did-mount (fn [this]
                           (let [c (.getDOMNode this)]
                             (aset c "width" (:width attrs))
                             (aset c "height" (:height attrs))
                             (let [ctx (.getContext c "2d")]
                               (aset c "d" ctx)
                               (.paint this ctx props))
                             ))
    :component-will-receive-props
    (fn [this [_ _ & props]]
      (.paint this (-> this (.getDOMNode) (.-d)) props))
    ;;    :component-will-update (fn [this] (pr "will-update"))
    :reagent-render (fn [attrs & props]
                      [:canvas (dissoc attrs :width :height :paint)])}))

(defn canvas->img [c]
  (let [img (js/Image.)]
    (set! (.-src img) (.toDataURL c))
    img))

(defn img-data->rgba [imd]
  (let [width (.-width imd)
        height (.-height imd)
        data (.-data imd)]
     (for [x (range width)]
       (for [y (range height)]
         (let [base (* 4 (+ x (* width y)))]
           {:r (aget data base) :g (aget data (inc base))
            :b (aget data (+ 2 base)) :a (aget data (+ 3 base))})))))

(defn aget-in [x ats]
  (apply aget `(~x ~@ats)))

(.log js/console (aget-in (clj->js {:d {:e {:f 3}}}) ["d" "e" "f"]))

(defn get-pix [imdat x y]
         (let [w (.-width imdat)
               base (* 4 (+ x (* w y)))
               data (.-data imdat)]
           {:r (aget data base) :g (aget data (inc base))
            :b (aget data (+ 2 base)) :a (aget data (+ 3 base))}))

(defn img->bundle
  "Returns a channel on which a record is written containing {:img
  img :data data} where img is the image itself and data is the image
  data object."
  [url]
  (let [im (js/Image.)]
    (aset im "src" url)
    (let [ch (chan)]
      (aset im "onload"
            (fn [e]
              (let [c (.createElement js/document "canvas")
                    d (.getContext c "2d")
                    w (.-width im)
                    h (.-height im)]
                (set! (.-width c) w)
                (set! (.-height c) h)
                (-> d (.drawImage im 0 0))
                (go (>! ch {:img im
                            :data (.getImageData d 0 0 w h)}))
                )))
      ch)))


(go
  (let [bundle (<! (img->bundle "map.png"))]
    (session/put! :bundle bundle)))

(defn msg-widget [] [:span
;;                     (pr-str (session/get :labels))

                     [:input {:type "text"}]   (pr-str (session/get :msg))])

(defn relpos [e]
  (let [x (- (.-pageX e) (-> e (.-target) (.-offsetLeft)))
        y (- (.-pageY e) (-> e (.-target) (.-offsetTop)))]
    {:x x :y y}))

(defn map-jig [img labels]
  (let [paint-fn
        (fn [this d [labels]]
          (-> d (.drawImage img 0 0 ))
          (doseq [[x y] labels]
            (set! (.-strokeStyle d) "white")
            (set! (.-lineWidth d) 2)
            (doto d
              (.beginPath)
              (.moveTo (- x 10) y)
              (.lineTo (+ x 10) y)
              (.stroke)
              (.beginPath)
              (.moveTo x (- y 10))
              (.lineTo x (+ y 10))
              (.stroke))))]
    [canvas-comp {:width (.-width img) :height (.-height img)
                  :paint paint-fn
                  :on-mouse-down
                  (fn [e] (let [{:keys [x y]} (relpos e)]
                            (session/swap! update :labels #(conj (or % []) [x y]))))
                  :on-mouse-move
                  (fn [e]
                    (let [{:keys [x y]} (relpos e)]
                      (session/put!
                       :msg {:x x
                             :y y
                             :c (if-let [bundle (session/get :bundle)]
                                  (dissoc (get-pix (:data bundle) x y) :a)
                                  nil)})))}
     labels]))

(defn home-page []
  (session/put! :imgs {})
  [:div
   (if-let [bundle (session/get :bundle)]
     [map-jig (:img bundle []) (session/get :labels)]
     [:span])
   [:br]
   [msg-widget]])


;; Initialize app
(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (session/put! :imgs {})
  (mount-root))
