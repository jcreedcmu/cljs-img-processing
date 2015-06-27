(ns coreas.prod
  (:require [coreas.core :as core]))

(enable-console-print!)
;;ignore println statements in prod
;; (set! *print-fn* (fn [& _]))

(core/init!)
