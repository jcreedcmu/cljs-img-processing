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


(defn debug [] (pr "what" (session/get :msg)) (session/put! :msg (inc (session/get :msg))) nil)


(defn img-canvas [name other]
  (session/get :msg)
  (reagent/create-class
   {:paint (fn [this] (pr "oh god" (session/get :msg)))
    :component-did-mount (fn [this]
                           (session/get :msg)
                           (let [c (.getDOMNode this)
                                 img (get-in @session/state [:imgs name])]
                             (aset c "width" (.-width img))
                             (aset c "height" (.-height img))
                             (aset c "d" (.getContext c "2d"))
                             (.paint this)
                             ))
    ;; :component-will-receive-props (fn [this] (pr "will-receive-props") (.paint this))
    :component-will-update (fn [this] (pr "will-update") (.paint this))
    :reagent-render (fn [name other]
                      (session/get :msg)
                      [:canvas])}))

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

(defn msg-widget [] [:span [:input {:type "text"}]   (pr-str (session/get :msg))])
(defn home-page []
  (session/put! :imgs {})
  [:div
   [msg-widget]
   [:img { :on-mouse-move
          (fn [e]
            (let [x (- (.-clientX e) (-> e (.-target) (.-offsetLeft)))
                  y (- (.-clientY e) (-> e (.-target) (.-offsetTop)))]
             (session/put!
              :msg {:x x
                    :y y
                    :c (if-let [bundle (session/get :bundle)]
                         (:g (get-pix (:data bundle) x y))
                         nil)})))
          :src "map.png"}]

   ])


;; Initialize app
(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (session/put! :imgs {})
  (mount-root))
