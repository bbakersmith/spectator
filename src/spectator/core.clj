(ns spectator.core
  (:import [nu.pattern OpenCV]
           [org.opencv.core Core CvType Mat MatOfPoint Point Rect Scalar Size]
           [org.opencv.highgui VideoCapture]
           [org.opencv.imgproc Imgproc]
           [java.util ArrayList])
  (:require [spectator.debug :as debug])
  (:gen-class))


;; http://www.pyimagesearch.com/2015/05/25/basic-motion-detection-and-tracking-with-python-and-opencv/


(set! *warn-on-reflection* true)
(OpenCV/loadShared)


(def camera (VideoCapture. 0))
(def motion-reference-mat (atom nil))
(def presence-reference-mat (atom nil))
(def regions (atom {}))
(def tracking (atom false))


(defrecord Region
  [contours diff update value])


(defn- new-mat []
  (Mat. (Size. 640 480) CvType/CV_8U (Scalar. 0 0 0)))


(defn- preprocess-mat [mat]
  (let [out (new-mat)]
    (Imgproc/cvtColor mat out Imgproc/COLOR_RGB2GRAY)
    (Imgproc/GaussianBlur out out (Size. 21 21) 0)
    out))


(defn- diff-motion-mat [mat]
  (when (not @motion-reference-mat)
    (reset! motion-reference-mat mat))
  (let [out (new-mat)]
    (Core/absdiff mat @motion-reference-mat out)
    (Imgproc/threshold out out 25 255 Imgproc/THRESH_BINARY)
    (reset! motion-reference-mat mat) ;; comment out to calc on initial frame always
    out))


(defn- diff-presence-mat [mat]
  (when (not @presence-reference-mat)
    (reset! presence-reference-mat mat))
  (let [out (new-mat)]
    (Core/absdiff mat @presence-reference-mat out)
    (Imgproc/threshold out out 25 255 Imgproc/THRESH_BINARY)
    out))


(defn- update-region-diffs [mat k]
  (doseq [[id r] @regions]
    (let [region-mat (new-mat)
          result-mat (new-mat)]
      (Core/fillConvexPoly region-mat (:contours r) (Scalar. 255 0 0))
      (Core/bitwise_and mat region-mat result-mat)
      (swap! (:diff r) assoc k (Core/countNonZero result-mat))))
  mat)


(defn- update-region-values []
  (doseq [[id r] @regions]
    (swap! (:value r) (:update r) @(:diff r))))


(defn- create-contours [tuples]
  (let [a (ArrayList.)
        m (MatOfPoint.)]
    (doseq [[x y] tuples]
      (.add a (Point. x y)))
    (.fromList m a)
    m))


(def debug-layer (atom :motion))


(defn- process-next-frame []
  (let [mat (new-mat)
        _ (.read camera mat)
        _ (Core/flip mat mat 1)
        mat (preprocess-mat (Mat. mat (Rect. 0 0 640 480)))
        motion-diff (diff-motion-mat mat)
        presence-diff (diff-presence-mat mat)]
    (update-region-diffs motion-diff :motion)
    (update-region-diffs presence-diff :presence)
    (update-region-values)
    (case @debug-layer
      :motion (debug/debug motion-diff @regions)
      :presence (debug/debug presence-diff @regions)
      :raw (debug/debug mat @regions))))


(defn define-region
  "Create region and return the region value atom."
  [id params]
  (assert (every? (into #{} (keys params))
                  #{:contours :update})
          (str "define-region required fields: contours update"))
  (let [diff-atom (atom {:motion 0 :presence 0})
        value-atom (atom 0)
        region (-> params
                   (update :contours create-contours)
                   (assoc :diff diff-atom
                          :value value-atom)
                   map->Region)]
    (swap! regions assoc id region)
    value-atom))


(defmacro defregion
  "Create region and provide the region value atom as a var of the given id."
  [id & params]
  (list 'def id (list define-region (keyword id) (apply hash-map params))))


(defn reset-presence []
  (reset! presence-reference-mat nil))


(defn start [& params]
  (let [{:keys [debug]} (->> params
                             (apply hash-map)
                             (merge {:debug true}))]
    (when debug (debug/show))
    (reset! tracking true)
    (future
     (try (while @tracking (process-next-frame))
          (catch Exception e
            (let [stack-trace (clojure.string/join
                               "\n" (map str (.getStackTrace e)))]
              (spit "/tmp/spectator-error.log"
                    (str e "\n" stack-trace))))))))


(defn stop []
  (debug/hide)
  (reset! tracking false))


(defn -main [])
