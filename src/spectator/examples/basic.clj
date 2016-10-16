(ns spectator.examples.basic
  (:require [spectator.core :refer [defregion] :as spectator]))


(defn test-region-update [old-val {:keys [motion presence]}]
  (-> (if (= motion 0)
        (- old-val 10)
        (+ old-val 10))
      (max 0)
      (min 250)))


(defregion test-region
  :contours [[100 100] [250 100] [100 250]]
  :update test-region-update)


(defregion test-region-2
  :contours [[300 100] [500 100] [500 400] [300 400]]
  :update (fn [_ diff] (if (zero? (:motion diff)) :still :moving)))


(spectator/start)
