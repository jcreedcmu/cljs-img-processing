(ns coreas.core
  (:require [reagent.core :as reagent :refer [atom cursor]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [cljsjs.react :as react]
            [cljs.core.async :as ca :refer [chan put! timeout]]
            [ajax.core :refer [GET]]
            [cljs.core.match :refer-macros [match]]
            [coreas.label-data :as labels])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop alt!]])
  (:import goog.History))

(enable-console-print!)

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
#_              (print "loaded " url)
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
    (set! (.-onload img) (fn [] #_ (print "loaded " url) (forever ch img)))
    ch))

(defn ch->atom [ch]
  (let [atm (atom nil)]
    (go (reset! atm (<! ch)))
    atm))

(defn color->text [c] (str (:r c) "/" (:g c) "/" (:b c)))

(defn make-map
  "like _.object, convert a list of [key, value] pairs into a map."
  [pairs]
  (zipmap (map first pairs) (map second pairs)))

(defn res [kwd] (session/get-in [:res kwd]))

(defn ri [n] (int (* n (.random js/Math))))

(defn draw-resource-icon [d x y which sign]
  (.drawImage d (:img (res :icons-img)) (* 15 sign) (* 15 which) 15 15 x y 15 15))

(defn paint-fn [w h]
  (fn [this d [game-state]]
    (let [cc (:cc game-state)
          info (res :map-pieces-info)]

      ;; Draw countries
      (doseq [n (range (count (:colors info)))]
        (let [color (get (:colors info) n)
              extent (get-in info [:extents (keyword color)])
              size (get-in info [:sizes (keyword color)])
              basex (* (:x (:cell_size info)) (mod n (:num_cells info)))
              basey (* (:y (:cell_size info)) (int (/ n (:num_cells info))))
              sizex (:x size)
              sizey (:y size)]
          (doto (:ctx (res :map-pieces-img))
            (aset "globalCompositeOperation" "source-atop")
            (aset "fillStyle" (cond
                                (contains? (:countries game-state) n) "#e77"
                                (= n cc) "#cc7"
                                true "#777"))
            (.fillRect basex basey sizex sizey))

          (doto d
            (.drawImage (:canvas (res :map-pieces-img))
                        basex basey sizex sizey
                        (:min_x extent) (:min_y extent) sizex sizey))))

      ;; Draw oceans and country borders
      (doto d (.drawImage (res :outline-img) 0 0))

      ;; Draw resource icons
      (doseq [n (range (count (:colors info)))]
        (let [color (get (:colors info) n)
              [x y] ((res :centers) color)]
          (when (not (contains? (:countries game-state) n))
            (draw-resource-icon d (- x 16) (- y 7) (mod n 6) (mod n 2))
            (draw-resource-icon d x (- y 7) (mod n 5) (mod n 2)))))

      ;; Draw resources the player has
      (doseq [rix (range (count (:resources game-state)))]
        (doseq [ix (range ((:resources game-state) rix))]
          (draw-resource-icon d (inc (* 16 ix)) (inc (* 16 rix)) rix 0)))
      )
    )
  )

(defn xy->country-ix [x y]
  ((res :color-ix) (color->text (get-pix (:data (res :map-img)) x y))))

(defn color->country-name []
  (make-map (map (fn [{[x y] :pos text :text}] [(color->text (get-pix (:data (res :map-img)) x y)) text]) labels/pos->label)))

(defn xy->country-name [x y]
  ((color->country-name) (color->text (get-pix (:data (res :map-img)) x y))))


(defn map-component [w h]
  (let [f (paint-fn w h)]
    [canvas-comp {:width w :height h
                  :paint f
                  :on-mouse-move
                  (fn [e] (let [{x :x y :y} (relpos e)]
                            (reset! (cursor session/state [:game-state :cc]) (xy->country-ix x y))))
                  :on-mouse-down
                  (fn [e] (let [{x :x y :y} (relpos e)]
                            (print (xy->country-name x y))
                            (swap! (cursor session/state [:game-state :countries]) #(conj % (xy->country-ix x y)))))}
     (session/get :game-state)]))



(defn home-page []
  (let [info (res :map-pieces-info)
        img (:canvas (res :map-pieces-img))
        w (:width (:orig_image_size info))
        h (:height (:orig_image_size info))]
    (if (and info img)
      [:span
       [map-component w h]
       [:br]
       (pr-str @(cursor session/state [:game-state ]))]
      [:span])))


(defn init-game-state []
  (print "initting")
  (go (let [
            res
            {:map-img (<! (img-bundle-future "/map.png"))
             :icons-img (<! (img-bundle-future "/icons.png"))
             :map-pieces-info (<! (json-future "/built/map-pieces.json"))
             :map-pieces-img (<! (img-bundle-future "/built/map-pieces.png"))
             :outline-img (<! (img-future "/built/map-outline.png"))}
            res (assoc
                 res :centers
                 (make-map (for [{[x y] :pos} labels/pos->label]
                             [(color->text (get-pix (:data (res :map-img)) x y)) [x y]])))
            res (assoc
                 res :color-ix
                 (make-map (for [n (range (count (:colors (res :map-pieces-info))))]
                                 [(get (:colors (res :map-pieces-info)) n) n])))]

        (session/put! :res res)))

  (session/put! :game-state
                {:countries #{}
                 :resources [2 2 2 2 2 2]}))

;; Initialize app
(defn mount-root []
  (print "mounting")
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (init-game-state)
  (aset js/document "onkeydown"
        (fn [e]
          (case (.-keyCode e)
            82 (init-game-state)
            (.log js/console (.-keyCode e)))))
  (mount-root))
