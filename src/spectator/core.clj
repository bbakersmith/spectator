(ns spectator.core
  (:import [nu.pattern OpenCV]
           [org.opencv.core Core CvType Mat MatOfByte MatOfPoint MatOfRect
            Point Rect Scalar Size]
           [org.opencv.highgui Highgui VideoCapture]
           [org.opencv.imgproc Imgproc]
           [org.opencv.objdetect CascadeClassifier]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io ByteArrayInputStream]
           [java.util ArrayList])
  (:require [seesaw.core :refer :all]
            [spectator.debug :as debug]
            [mount.core :as mount])
  (:gen-class))


;; http://www.pyimagesearch.com/2015/05/25/basic-motion-detection-and-tracking-with-python-and-opencv/


(set! *warn-on-reflection* true)
(OpenCV/loadShared)


(def camera (VideoCapture. 0))
(def motion-mat-prev (atom nil))
(def regions (atom {}))
(def tracking (atom false))


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


(defn define-region [id region-map]
  (assert (every? (into #{} (keys region-map))
                  #{:contours :decrement :increment})
          (str "region-map required fields: contours decrement increment"))
  (let [region (map->Region
                (merge {:value (atom 0)} region-map))]
    (swap! regions assoc id region)
    region))


(defmacro defregion [id & params]
  (list define-region (keyword id) (apply hash-map params)))


(defn get-region-value [id]
  @(:value (@regions id)))


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


(mount/defstate motion-tracker
  :start (do
           (reset! tracking true)
           (future
            (try (while @tracking (process-next-frame))
                 (catch Exception e
                   (spit "/tmp/spectator-error.log" e)))))
  :stop (reset! tracking false))


(defregion test-region
  :contours (let [a (ArrayList.)
                  m (MatOfPoint.)]
              (.add a (Point. 100 100))
              (.add a (Point. 250 100))
              (.add a (Point. 100 250))
              (.fromList m a)
              m)
  :decrement #(if (< 0 %)
                (- % 10)
                %)
  :increment (fn [v _] (if (<= v 255)
                         (+ v 10)
                         v)))
