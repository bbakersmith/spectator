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


(defn new-mat []
  (Mat. (Size. 640 480) CvType/CV_8U (Scalar. 0 0 0)))


(defn preprocess-mat [mat]
  (let [out (new-mat)]
    (Imgproc/cvtColor mat out Imgproc/COLOR_RGB2GRAY)
    (Imgproc/GaussianBlur out out (Size. 21 21) 0)
    out))


(defn diff-mat [mat]
  (when (not @motion-mat-prev)
    (reset! motion-mat-prev mat))
  (let [out (new-mat)]
    (Core/absdiff mat @motion-mat-prev out)
    (Imgproc/threshold out out 25 255 Imgproc/THRESH_BINARY)
    (reset! motion-mat-prev mat) ;; comment out to calc on initial frame always
    out))


(defn increment-regions [mat]
  (doseq [[id r] @regions]
    (let [region-mat (new-mat)
          result-mat (new-mat)]
      (Core/fillConvexPoly region-mat (:contours r) (Scalar. 255 0 0))
      (Core/bitwise_and mat region-mat result-mat)
      (let [diff-count (Core/countNonZero result-mat)]
        (if (zero? diff-count)
          (swap! (:value r) (:decrement r))
          (swap! (:value r) #((:increment r) % diff-count))))))
  mat)


(defrecord Region
  [contours decrement increment value])


(defn create-contours [tuples]
  (let [a (ArrayList.)
        m (MatOfPoint.)]
    (doseq [[x y] tuples]
      (.add a (Point. x y)))
    (.fromList m a)
    m))


(defn process-next-frame []
  (let [mat (new-mat)
        _ (.read camera mat)
        _ (Core/flip mat mat 1)
        mat (Mat. mat (Rect. 0 0 640 480))]
    (-> mat
        preprocess-mat
        diff-mat
        increment-regions
        (debug/debug @regions))))


(def debug-toggle debug/debug-toggle)


(defn get-region-value [id]
  @(:value (@regions id)))


(defn define-region [id params]
  (assert (every? (into #{} (keys params))
                  #{:contours :decrement :increment})
          (str "define-region required fields: contours decrement increment"))
  (let [region (-> params
                   (update :contours create-contours)
                   (assoc :value (atom 0))
                   map->Region)]
    (swap! regions assoc id region)))


(defmacro defregion [id & params]
  (list define-region (keyword id) (apply hash-map params)))


(future
 (try (while true (process-next-frame))
      (catch Exception e
        (spit "/tmp/spectator-error.log" e))))
