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
(def motion-mat-prev (atom nil))
(def regions (atom {}))
(def tracking (atom false))


(defrecord Region
  [contours update value])


(defn- new-mat []
  (Mat. (Size. 640 480) CvType/CV_8U (Scalar. 0 0 0)))


(defn- preprocess-mat [mat]
  (let [out (new-mat)]
    (Imgproc/cvtColor mat out Imgproc/COLOR_RGB2GRAY)
    (Imgproc/GaussianBlur out out (Size. 21 21) 0)
    out))


(defn- diff-mat [mat]
  (when (not @motion-mat-prev)
    (reset! motion-mat-prev mat))
  (let [out (new-mat)]
    (Core/absdiff mat @motion-mat-prev out)
    (Imgproc/threshold out out 25 255 Imgproc/THRESH_BINARY)
    (reset! motion-mat-prev mat) ;; comment out to calc on initial frame always
    out))


(defn- update-regions [mat]
  (doseq [[id r] @regions]
    (let [region-mat (new-mat)
          result-mat (new-mat)]
      (Core/fillConvexPoly region-mat (:contours r) (Scalar. 255 0 0))
      (Core/bitwise_and mat region-mat result-mat)
      ;; TODO provide both :motion and :presence values
      (swap! (:value r) (:update r) {:motion (Core/countNonZero result-mat)})))
  mat)


(defn- create-contours [tuples]
  (let [a (ArrayList.)
        m (MatOfPoint.)]
    (doseq [[x y] tuples]
      (.add a (Point. x y)))
    (.fromList m a)
    m))


(defn- process-next-frame []
  (let [mat (new-mat)
        _ (.read camera mat)
        _ (Core/flip mat mat 1)
        mat (Mat. mat (Rect. 0 0 640 480))]
    (-> mat
        preprocess-mat
        diff-mat
        update-regions
        (debug/debug @regions))))


(defn define-region
  "Create region and return the region value atom."
  [id params]
  (assert (every? (into #{} (keys params))
                  #{:contours :update})
          (str "define-region required fields: contours update"))
  (let [value-atom (atom 0)
        region (-> params
                   (update :contours create-contours)
                   (assoc :value value-atom)
                   map->Region)]
    (swap! regions assoc id region)
    value-atom))


(defmacro defregion
  "Create region and provide the region value atom as a var of the given id."
  [id & params]
  (list 'def id (list define-region (keyword id) (apply hash-map params))))


(defn start [& params]
  (let [{:keys [debug]} (->> params
                             (apply hash-map)
                             (merge {:debug true}))]
    (when debug (debug/show))
    (reset! tracking true)
    (future
     (try (while @tracking (process-next-frame))
          (catch Exception e
            (spit "/tmp/spectator-error.log" e))))))


(defn stop []
  (debug/hide)
  (reset! tracking false))
