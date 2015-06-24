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
            [clojure.core.reducers :refer [reduce]]
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

(defn raw-json-future [url]
  (let [ch (chan)]
    (go
      (GET url {:handler #(forever ch %)
                :error-handler #(print "an error occurred: " %)
                :response-format :json}))

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
(defn rcoin [p] (< (.random js/Math) p))

(defn draw-resource-icon [d x y which sign]
  (.drawImage d (:img (res :icons-img)) (* 15 sign) (* 15 which) 15 15 x y 15 15))

(def ICON_SIZE 15)

;; This :country-resources seems to miscount resource 0
;; {0 [{:which 3, :sign 1}], 32 [{:which 1, :sign 0} {:which 5, :sign 1}], 1 [{:which 1, :sign 1}], 33 [{:which 5, :sign 0} {:which 1, :sign 1}], 2 [{:which 5, :sign 0} {:which 4, :sign 1}], 34 [{:which 1, :sign 1}], 3 [{:which 5, :sign 1}], 35 [{:which 0, :sign 1}], 4 [{:which 1, :sign 1}], 36 [{:which 0, :sign 1}], 5 [{:which 4, :sign 1}], 37 [{:which 5, :sign 0} {:which 1, :sign 1}], 6 [{:which 2, :sign 1}], 38 [{:which 2, :sign 1}], 7 [{:which 4, :sign 1}], 39 [{:which 0, :sign 0} {:which 2, :sign 1}], 8 [{:which 0, :sign 0}], 40 [{:which 4, :sign 1}], 9 [{:which 4, :sign 0} {:which 2, :sign 1}], 41 [{:which 0, :sign 1}], 10 [{:which 1, :sign 1}], 42 [{:which 1, :sign 1}], 11 [{:which 1, :sign 1}], 43 [{:which 5, :sign 1}], 12 [{:which 0, :sign 1}], 44 [{:which 0, :sign 1}], 13 [{:which 5, :sign 0}], 45 [{:which 1, :sign 1}], 14 [{:which 1, :sign 1}], 46 [{:which 3, :sign 0} {:which 4, :sign 1}], 15 [{:which 4, :sign 1}], 47 [{:which 0, :sign 1}], 16 [{:which 1, :sign 0} {:which 3, :sign 1}], 48 [{:which 1, :sign 0} {:which 2, :sign 1}], 17 [{:which 4, :sign 0}], 49 [{:which 2, :sign 0}], 18 [{:which 1, :sign 1}], 50 [{:which 5, :sign 1}], 19 [{:which 3, :sign 1}], 51 [{:which 4, :sign 0} {:which 5, :sign 1}], 20 [{:which 2, :sign 0} {:which 1, :sign 1}], 21 [{:which 2, :sign 0} {:which 1, :sign 1}], 22 [{:which 5, :sign 1}], 23 [{:which 1, :sign 0} {:which 2, :sign 1}], 24 [{:which 3, :sign 1}], 25 [{:which 3, :sign 0} {:which 0, :sign 1}], 26 [{:which 5, :sign 1}], 27 [{:which 4, :sign 0} {:which 1, :sign 1}], 28 [{:which 1, :sign 1}], 29 [{:which 3, :sign 0} {:which 0, :sign 1}], 30 [{:which 5, :sign 1}], 31 [{:which 0, :sign 1}]}

(defn add-resource [resources {:keys [which sign]}]
  (update resources which (if (= sign 0) inc dec)))

(defn add-resources! [rs]
  (let [cur (cursor session/state [:game-state :resources])
        old-resources @cur
        new-resources (reduce add-resource old-resources rs)
        new-resources-ok (every? #(>= % 0) new-resources)]
    (if new-resources-ok
      (reset! cur new-resources))
    new-resources-ok))

(defn resources-of-country-ix [n]
  ((res :country-resources) n))

(defn ixfy [seq]
  (for [n (range (count seq))]
    [(get seq n) n]))

(defn accessible-from?
  "Is country `n` accessible in state `game-state`?"
  [game-state n]
  (some #((:countries game-state) %) ((res :adjacencies) n)))

(defn paint-fn [w h]
  (fn [this d [game-state]]
    (let [cc (:cc game-state)
          info (res :map-pieces-info)]

      ;; Draw countries
      (doseq [[color n] (ixfy (:colors info))]
        (let [extent (get-in info [:extents (keyword color)])
              size (get-in info [:sizes (keyword color)])
              basex (* (:x (:cell_size info)) (mod n (:num_cells info)))
              basey (* (:y (:cell_size info)) (int (/ n (:num_cells info))))
              sizex (:x size)
              sizey (:y size)]
          (doto (:ctx (res :map-pieces-img))
            (aset "globalCompositeOperation" "source-atop")
            (aset "fillStyle" (cond
                                (contains? (:countries game-state) n) "#a65"
                                (accessible-from? game-state n) (if (= n cc) "#aff" "#cc7")
                                true "#777"))
            (.fillRect basex basey sizex sizey))

          (doto d
            (.drawImage (:canvas (res :map-pieces-img))
                        basex basey sizex sizey
                        (:min_x extent) (:min_y extent) sizex sizey))))

      ;; Draw oceans and country borders
      (doto d (.drawImage (res :outline-img) 0 0))

      ;; Draw resource icons
      (doseq [[color n] (ixfy (:colors info))]
        (let [[x y] ((res :centers) color)]
          (when (not (contains? (:countries game-state) n))
            (let [resources (resources-of-country-ix n)
                  xoffset (int (/ (* (inc ICON_SIZE) (count resources)) 2))
                  yoffset (int (/ ICON_SIZE 2))]
              (doseq [[resource i] (ixfy resources)]
                (draw-resource-icon d (+ (* (inc ICON_SIZE) i) (- x xoffset)) (- y yoffset)
                                    (:which resource) (:sign resource)))
            ))))

      ;; Draw resources the player has
      (doseq [[resource-count rix] (ixfy (:resources game-state))]
        (doseq [ix (range resource-count)]
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


(defn map-component [w h game-state]
  (let [f (paint-fn w h)]
    [canvas-comp {:width w :height h
                  :paint f
                  :on-mouse-move
                  (fn [e] (let [{x :x y :y} (relpos e)]
                            (reset! (cursor session/state [:game-state :cc]) (xy->country-ix x y))))
                  :on-mouse-down
                  (fn [e] (let [{x :x y :y} (relpos e)]
                            (let [ix (xy->country-ix x y)]
                              (swap! (cursor session/state [:game-state :countries])
                                     (fn [conts] (if (and (accessible-from? game-state ix)
                                                          (not (contains? conts ix)))
                                                   (if (add-resources! (resources-of-country-ix ix))
                                                     (do (print ix (xy->country-name x y))
                                                         (conj conts ix))
                                                     conts)
                                                   (do (print "nope") conts))))
                              (.preventDefault e))))}
     game-state]))



(defn home-page []
  (let [info (res :map-pieces-info)
        img (:canvas (res :map-pieces-img))
        w (:width (:orig_image_size info))
        h (:height (:orig_image_size info))]
    (if (and info img)
      [:span
       [map-component w h (session/get :game-state)]
       [:br]
       (pr-str (res :map-sum-resources))]
      [:span])))

(defn color-bimap->ix-bimap
  "Convert a (color -> color -> 'a) map to a (ix -> ix set) map,
  where cix : color -> ix"
  [cix map]
  (make-map (for [[key value] map
                  :let [ix (cix key)]
                  :when ix]
              [ix (set (for [key2 (keys value)
                             :let [ix2 (cix key2)]
                             :when ix2] ix2))])))

(defn init-game-state []
  (session/put! :game-state
                {:countries #{39} ; urrakeny
                 :resources (vec (map #(if (< % 0) (- %) 0) (res :map-sum-resources)))}))

(defn distinct-res []
  (let [a (ri 6) b (ri 5)]
    (if (>= b a) [a (inc b)] [a b])))

(defn random-resources []
  (cond
    (rcoin 0.3) (let [[a b] (distinct-res)]
                  [{:which a :sign 0}
                   {:which b :sign 1}])
    (rcoin 0.9) [{:which (ri 6) :sign 1}]
    true [{:which (ri 6) :sign 0}]))


(defn random-resources-on [countries]
  (loop [count 100]
    (let [proposal (for [k countries]
                     [k (random-resources)])
          costs (reduce add-resource [0 0 0 0 0 0]
                        (apply concat (for [[k v] proposal] v)))
          cost (apply min costs)]
      (if (and (> count 0)
               (or (< cost -4) (< (get costs 5) 0)))
        (recur (dec count))
        (do (print count costs) {:country-resources (make-map proposal)
                                 :map-sum-resources costs})))))

(defn init-state []
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
                 (make-map (ixfy (:colors (res :map-pieces-info)))))
            res (assoc
                 res :adjacencies
                 (color-bimap->ix-bimap (res :color-ix) (<! (raw-json-future "/built/adjacencies.json"))))
            res (merge res (random-resources-on (keys (res :adjacencies))))]
        (session/put! :res res)
        (init-game-state))))


;; Initialize app
(defn mount-root []
  (print "mounting")
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (init-game-state)
  (aset js/document "onkeydown"
        (fn [e]
          (case (.-keyCode e)
            82 (init-state)
            83 (init-game-state)
            (.log js/console (.-keyCode e)))))
  (mount-root))
