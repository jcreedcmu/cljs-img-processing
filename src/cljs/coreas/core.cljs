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

(defn loading-img [name f]

  (if-let [img (get-in @session/state [:imgs name])]
    (do (print img) [img-wrapper img])
    (let [im (js/Image.)]
      (aset im "src" name)
      (aset im "onload"
            (fn [e]
              (let [c (.createElement js/document "canvas")
                    d (.getContext c "2d")
                    w (.-width im)
                    h (.-height im)]
                (set! (.-width c) w)
                (set! (.-height c) h)
                (-> d (.drawImage im 0 0))
                (let [data (.getImageData d 0 0 w h)
                      raw (.-data data)
                      copy (js/Uint8ClampedArray. raw)]
                  (doall (for [x (range w)
                               y (range h)]
                           (let [px (if (f (fn [gx gy] (:g (get-pix data gx gy))) x y)
                                      (do
                                        (aset copy (* 4 (+ x (* w y))) 0)
                                        (aset copy (+ 1 (* 4 (+ x (* w y)))) 0)
                                        (aset copy(+ 2 (* 4 (+ x (* w y)))) 0)))]
)
                           ))
                  (.set raw copy)
                  (.putImageData d data 0 0))


                (reset! (cursor session/state [:imgs name]) (canvas->img c)))))
     [:span "Loading..."])))

(defn home-page []
  (session/put! :imgs {})
  [:div  [:h2 "Thing"]
   [loading-img "map.png" (fn [get x y] (or
                                         (< (get x y) (get (inc x) y))
                                         (< (get x y) (get (dec x) y))
                                         (< (get x y) (get  x (inc y)))
                                         (< (get x y) (get  x (dec y)))))]])


;; Initialize app
(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (session/put! :imgs {})
  (mount-root))
