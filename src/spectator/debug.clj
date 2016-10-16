(ns spectator.debug
  (:import [nu.pattern OpenCV]
           [org.opencv.core Core Mat MatOfByte Point Scalar]
           [org.opencv.highgui Highgui]
           [org.opencv.imgproc Imgproc]
           [javax.imageio ImageIO]
           [java.io ByteArrayInputStream]
           [java.util ArrayList])
  (:require [seesaw.core :refer :all])
  (:gen-class))


(def debug-status (atom true))
(def video-player (label))


(def debug-frame
  (-> (frame :title "Spectator Debug"
             :content video-player
             :width 640
             :height 480)
      show!))


(defn- draw-rectangle [mat r]
  (Core/rectangle mat
                  (Point. (.x r) (.y r))
                  (Point. (+ (.x r) (.width r))
                          (+ (.y r) (.height r)))
                  (Scalar. 0 0 200)
                  -1)
  mat)


(defn- debug-text [mat [x y] txt]
  (Core/putText mat
                (str txt)
                (Point. x y)
                Core/FONT_HERSHEY_SIMPLEX
                0.5
                (Scalar. 255 255 255)))


(defn- draw-regions [mat regions]
  (doseq [[id r] regions]
    (let [contours (ArrayList.)
          top-left (first (.toArray (:contours r)))]
      (.add contours (:contours r))
      (debug-text mat [(+ 5 (.x top-left))
                       (+ 30 (.y top-left))] (name id))
      (debug-text mat [(+ 5 (.x top-left))
                       (+ 50 (.y top-left))] @(:value r))
      (Imgproc/drawContours mat [(:contours r)] -1 (Scalar. 200 4 0))))
  mat)


(defn- display-image [mat]
  (let [byte-mat (MatOfByte.)]
    (Highgui/imencode ".jpg" mat byte-mat)
    (config! video-player :icon (-> byte-mat
                                    .toArray
                                    ByteArrayInputStream.
                                    ImageIO/read
                                    icon))))


(defn debug-toggle [on-off]
  (if on-off
    (do
      (show! debug-frame)
      (reset! debug-status true))
    (do
      (hide! debug-frame)
      (reset! debug-status false))))


(defn debug [mat regions]
  (if @debug-status
    (display-image (draw-regions mat regions))
    mat))
