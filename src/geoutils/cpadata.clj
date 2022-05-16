(ns geoutils.cpadata
  (:gen-class)
  (:require [geoutils.geographicposition :as gp])
)

(defrecord cdata [position dist-to-cpa elapsed-time range-at-cpa code])

(def codes #(:valid :receding :no-relative-motion))

(defn get-geographic-position 
  "Returns the geographic position of the CPA"
  [cpa]
  (:position cpa)
)

(defn get-gp-latitude
  "Returns the latitude of the geographic position of the CPA."
  [cpa]
  (gp/get-latitude (:position cpa))
)

(defn get-gp-longitude
  "Returns the longitude of the geographic position of the CPA."
  [cpa]
  (gp/get-longitude (:position cpa))
)

(defn get-distance 
  "Returns the distance in nautical miles to the CPA."
  [cpa]
  (:dist-to-cpa cpa)
  )

(defn get-elapsed-time
  "Returns the elapsed time for the CPA."
  [cpa]
  (:elapsed-time cpa)
)

(defn get-nm-at-cpa
  "Returns the distance of the CPA in nautical miles."
  [cpa]
  (:range-at-cpa cpa)
)

(defn get-gp
  "Returns the geographic position of the CPA."
  [cpa]
  (:position cpa)
)

(defn get-code
  "Returns the status code of the CPA."
  [cpa]
  (:code cpa)
  )

(defn cpa-data
  "Creates the data record for a Closest-Point-of-Approach (CPA)."
  ([] (cpa-data (gp/geographic-position) 0.0 0.0 0.0 :valid))
  ([lat lon nm etime dist cde] (cpa-data (gp/geographic-position lat lon) 0.0 0.0 0.0 :valid))
  ([gp nm etime dist cde] (->cdata gp nm etime dist cde))
 )

(defn copy-cpa-data
  "Duplicates a CPA data record."
  [cpa]
  (->cdata (get-gp cpa) (get-distance cpa) (get-elapsed-time cpa) (get-nm-at-cpa cpa) (get-code cpa))
)

(defn abs
  [x]
  (max x (- x)))

(defn close-to
  (  [x y]
  (<= (abs (- x y)) 0.0005))
  ([x y epsilon]
  (<= (abs (- x y)) epsilon)))

(defn close-to-position
  ([testp expectedp]
  (and (close-to (gp/get-latitude testp) (gp/get-latitude expectedp) 0.005)
       (close-to (gp/get-longitude testp) (gp/get-longitude expectedp) 0.005)))
  ([testp expectedp epsilon]
  (and (close-to (gp/get-latitude testp) (gp/get-latitude expectedp) epsilon)
       (close-to (gp/get-longitude testp) (gp/get-longitude expectedp) epsilon))))

(defn is-same? 
  "Return true if all elements are the same."
  [cpa1 cpa2]
  (and (close-to-position (get-gp cpa1) (get-gp cpa2) 0.05)
       (close-to (get-distance cpa1) (get-distance cpa2) 0.5)
       (close-to (get-elapsed-time cpa1) (get-elapsed-time cpa2))
       (close-to (get-nm-at-cpa cpa1) (get-nm-at-cpa cpa2) 0.5)
       (= (get-code cpa1) (get-code cpa2))))

(defn to-string
  "A standard Java toString"
  [cpa]
  (println   (clojure.string/join 
              (str "Latitude      : " (gp/get-latitude (get-gp cpa)) 
                   "Longitude     : " (gp/get-longitude (get-gp cpa))
                   "\nDistance    : " (get-distance cpa) " nm"
                   "\nElapsed Time: " (get-elapsed-time cpa) " seconds"
                   "\nRange at CPA: " (get-nm-at-cpa cpa) " nm"
                   "\nStatus      : " 
                   (case (get-code cpa)
                     :valid "Valid"
                     :receding "Receding"
                     :no-relative-motion "No Relative Motion"))
              )))
                
