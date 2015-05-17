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
                           (let [px (if (f (fn [gx gy] (aget raw (* 4 (+ gx (* w gy))))) x y)
                                      0
                                      255)]
                             (aset copy (* 4 (+ x (* w y))) px)
                             (aset copy (+ 1 (* 4 (+ x (* w y)))) px)
                             (aset copy(+ 2 (* 4 (+ x (* w y)))) px))
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
