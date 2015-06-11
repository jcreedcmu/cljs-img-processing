(ns coreas.core
  (:require [reagent.core :as reagent :refer [atom cursor]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [cljsjs.react :as react]
            [cljs.core.async :as ca :refer [chan put! timeout]]
            [ajax.core :refer [GET]]
            [cljs.core.match :refer-macros [match]])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop alt!]])
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

(defn get-pix [imdat x y]
  (let [w (.-width imdat)
        base (* 4 (+ x (* w y)))
        data (.-data imdat)]
    {:r (aget data base) :g (aget data (inc base))
     :b (aget data (+ 2 base)) :a (aget data (+ 3 base))}))

(defn img-bundle-future
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
                            :canvas c
                            :ctx d
                            :data (.getImageData d 0 0 w h)}))
                )))
      ch)))



(defn relpos [e]
  (let [x (- (.-pageX e) (-> e (.-target) (.-offsetLeft)))
        y (- (.-pageY e) (-> e (.-target) (.-offsetTop)))]
    {:x x :y y}))


(defn dispatch [msg])

(defn forever [c v]  (go-loop []  (>! c v) (recur)))

(defn json-future [url]
  (let [ch (chan)]
    (go
      (GET url {:handler #(forever ch %)
                :error-handler #(print "an error occurred: " %)
                :response-format :json
                :keywords? true}))

    ch))

(defn img-future [url]
  (let [img (js/Image.)
        ch (chan)]
    (set! (.-src img) url)
    (set! (.-onload img) (fn [] (print "loaded") (forever ch img)))
    ch))

(defn ch->atom [ch]
  (let [atm (atom nil)]
    (go (reset! atm (<! ch)))
    atm))

(def map-img (ch->atom (img-bundle-future "/map.png")))
(def map-pieces-info (ch->atom (json-future "/built/map-pieces.json")))
(def map-pieces-img (ch->atom (img-bundle-future "/built/map-pieces.png")))
(def outline-img (ch->atom (img-future "/built/map-outline.png")))


(defn paint-fn [info img w h]
  (fn [this d [cc]]
    (doto d
      (aset "fillStyle" "#eff")
      (.fillRect 0 0 w h))
    (doseq [n (range (count (:colors info)))]
      (let [color (get (:colors info) n)
            extent (get-in info [:extents (keyword color)])
            size (get-in info [:sizes (keyword color)])
            basex (* (:x (:cell_size info)) (mod n (:num_cells info)))
            basey (* (:y (:cell_size info)) (int (/ n (:num_cells info))))
            sizex (:x size)
            sizey (:y size)]
        (doto (:ctx @map-pieces-img)
          (aset "globalCompositeOperation" "source-atop")
          (aset "fillStyle" (if (= n cc)
                              "yellow"
                              (get ["#7a7" "#a7a" "#77a" "#aa7" "#7aa" "#a77" "#aaa" "#777"] (mod n 2) )))
          (.fillRect basex basey sizex sizey))

        (doto d
          (.drawImage img
                      basex basey sizex sizey
                      (:min_x extent) (:min_y extent) sizex sizey))))
    (doto d (.drawImage @outline-img 0 0))))

(def cur-country (atom 3))

(defn color->text [c] (str (:r c) "/" (:g c) "/" (:b c)))

(defn make-map
  "like _.object, convert a list of [key, value] pairs into a map."
  [pairs]
  (zipmap (map first pairs) (map second pairs)))


(defn map-component [w h img info cc-atm]
  (let [cc @cc-atm
        f (paint-fn info img w h)]
    [canvas-comp {:width w :height h
                  :paint f
                  :on-mouse-move
                  (fn [e] (let [{x :x y :y} (relpos e)]
                            (reset! cc-atm ((:color-ix info) (color->text (get-pix (:data @map-img) x y))))))}
     cc]))



(defn home-page []
  (let [info @map-pieces-info
        img (:canvas @map-pieces-img)
        w (:width (:orig_image_size info))
        h (:height (:orig_image_size info))]
    (if (and info img)
      (let
          [color-ix (make-map (for [n (range (count (:colors info)))]
                           [(get (:colors info) n) n]))
           more-info (assoc info :color-ix color-ix)]
       [map-component w h img more-info cur-country])
      [:span])))

(session/put! :labels
              [{:pos [64 393], :text "piada"}
               {:pos [113 414], :text "koennif"}
               {:pos [105 444], :text "yol"}
               {:pos [179 437], :text "ezdi"}
               {:pos [387 335], :text "naga suria"}
               {:pos [163 343], :text "nivdal"}
               {:pos [262 275], :text "shiao"}
               {:pos [126 197], :text "sikau"}
               {:pos [230 201], :text "shiaovets"}
               {:pos [238 121], :text "boshionai"}
               {:pos [342 110], :text "bissholmi"}
               {:pos [383 254], :text "mi yon"}
               {:pos [458 145], :text "stolups"}
               {:pos [621 250], :text "gurakand"}
               {:pos [883 536], :text "zoxiai"}
               {:pos [819 388], :text "zufkand"}
               {:pos [779 350], :text "ziem"}
               {:pos [767 414], :text "qunmi"}
               {:pos [698 470], :text "ashigam"}
               {:pos [700 373], :text "nyvev"}
               {:pos [575 373], :text "vlori"}
               {:pos [606 438], :text "munishtaq vu"}
               {:pos [544 490], :text "jdayalur"}
               {:pos [598 520], :text "negima"}
               {:pos [567 610], :text "rujokatch"}
               {:pos [541 564], :text "jatmiyan"}
               {:pos [423 444], :text "wolaraum"}
               {:pos [347 501], :text "gorvied"}
               {:pos [242 489], :text "zbegyanda"}
               {:pos [200 562], :text "nartu"}
               {:pos [200 662], :text "gorgobya"}
               {:pos [198 625], :text "noboro"}
               {:pos [453 578], :text "iwoalye"}
               {:pos [282 601], :text "syeg ims"}
               {:pos [378 561], :text "mayarapit"}
               {:pos [457 658], :text "huatosurak"}
               {:pos [385 675], :text "huiyepitnaya"}
               {:pos [384 747], :text "tlorukpit"}
               {:pos [492 697], :text "kunzi"}
               {:pos [555 671], :text "urrakeny"}
               {:pos [566 735], :text "juta"}
               {:pos [640 691], :text "narik"}
               {:pos [689 645], :text "unbara"}
               {:pos [705 685], :text "pengerra"}
               {:pos [834 738], :text "oskeny"}
               {:pos [776 730], :text "daloe"}
               {:pos [715 734], :text "gorik"}
               {:pos [738 631], :text "nonggadum"}
               {:pos [776 649], :text "ribbonnad"}
               {:pos [809 678], :text "minar"}
               {:pos [877 703], :text "cubre"}
               {:pos [902 768], :text "tarodaro"}])




;; Initialize app
(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (session/put! :imgs {})
  (mount-root))
