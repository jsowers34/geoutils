(ns geoutils.geographicposition
  (:gen-class)
     (:require [geoutils.constants :as nav])
)

(defrecord geopos [latitude longitude])

(defn round
  "Round a value to a given precision. Useful for setting lat/lon in geographic positions."
  [x & {p :precision}]
  (if p
    (let [scale (Math/pow 10 p)]
      (-> x (* scale) Math/round (/ scale)))
    (Math/round x)))

(defn close-to
  (  [x y]
  (<= (Math/abs (- x y)) 0.0005))
  ([x y epsilon]
  (<= (Math/abs (- x y)) epsilon)))


(defn geographic-position 
  "Creates a geographic position.
   Examples:   (geographic-position)
   ;;==>       {:latitude 0.0 :longitude 0.0}

               (geographic-position [40.45 -74.393])
    ;;==>      {:latitude 40.45 :longitude -74.393}

               (geographic-position 40.45 -74.393)
   ;;==>       {:latitude 40.45 :longitude -74.393}
"
([] (geographic-position 0.0 0.0))
([ll] (geographic-position (round (first ll) :precision 3)  (round (last ll) :precision 3) ))
([lat lon] (->geopos (round lat :precision 3) (round lon :precision 3))))

(defn get-latitude 
  "Returns the latitude of a geographic position."
  [pos] 
  (:latitude pos))


(defn get-longitude 
  "Returns the longitude of a geographic position."
  [pos] 
  (:longitude pos))

(defn copy-geographic-position 
  "Duplicates a geographic position."
  [pos]
  (->geopos (get-latitude pos) (get-longitude pos))
)

(defn normalize-latitude
  "Normalizes the latitude to keep within + or - 90.0"
  [pos]
  (if (> pos 90) (- 180 pos) (if (< pos -90) (+ 180 pos) pos))
 )

(defn normalize-longitude 
  "Normalizes the longitude to keep within + or - 180.0"
  [pos]
  (if (> pos 180) (- 360 pos) (if (< pos -180) (+ 360 pos) pos))
)

(defn normalize-latitude
  "Normalizes the latitude to keep within + or - Pi/2"
  [pos]
  (if (> pos nav/PI-OVER-2) (- nav/PI pos) (if (< pos (* -1.0 nav/PI-OVER-2)) (+ nav/PI pos) pos))
 )


(defn normalize-longitude-radians 
  "Normalizes the longitude to keep within + or - Pi"
  [pos]
  (if (> pos nav/PI) (- nav/PI2 pos) (if (< pos (* -1.0 nav/PI)) (+ nav/PI2 pos) pos))
)

(defn +latitude 
  "Adds a latitude to a position"
  [pos lat]
  (->geopos (normalize-latitude (+ (:latitude pos) lat)) (:longitude pos))
)

(defn +longitude 
  "Adds a longitude to a position"
  [pos lon]
  (->geopos (:latitude pos) (normalize-longitude (+ (:longitude pos) lon)))
)

(defn -latitude [pos lat]
  "Subtracts a latitude from a position"
  (->geopos (normalize-latitude (- (:latitude pos) lat)) (:longitude pos))
)

(defn -longitude [pos lon]
  "Subtracts a longitude from a position"
  (->geopos (:latitude pos) (normalize-longitude (- (:longitude pos) lon)))
)

(defn +position 
  "Adds latitude and longitude to a position."
  [pos lat lon]
  (->geopos (+latitude pos lat) (+longitude pos lon))
)

(defn get-lat-long 
  "Returns a vector '[lat, long] from a position"
  [pos]
  (conj [] (get-latitude pos) (get-longitude pos))
)

(defn same-latitude? [pos1 pos2]
  "Tests to see if two positions have the same latitude."
  (= (get-latitude pos1) (get-latitude pos2))
)

(defn same-longitude? [pos1 pos2]
  "Tests to see if two positions have the same longitude"
  (= (get-longitude pos1) (get-longitude pos2))
)

(defn same-position? [pos1 pos2]
  "Tests to see if two positions are at the same location."
  (and (same-latitude? pos1 pos2) (same-longitude? pos1 pos2))
)

(defn to-string 
  "A standard Java toString"
  [pos]
  (println (str "Latitude: " (get-latitude pos) " Longitude: " (get-longitude pos))) 
)

(defn is-right?
  "Returns true if pos is right of the (infinite) line between startpos and endpos"
  [startpos endpos pos]
  (<= 0.0 
(- (* (- (get-longitude endpos) (get-longitude startpos)) (- (get-latitude pos) (get-latitude startpos)))
   (* (- (get-longitude pos) (get-longitude startpos)) (- (get-latitude endpos) (get-latitude startpos))))))

(defn is-left?
  "Returns true if pos is left of the (infinite) line between startpos and endpos"
  [startpos endpos pos]
  (not (is-right? startpos endpos pos))
)

;; Conversion functions for convenience

(defn statutemiles->nauticalmiles 
  "Convert statute miles to nautical miles"
  [miles] 
  (/ miles 1.1508))

(defn nauticalmiles->statutemiles
  "Convert nautical miles to statute miles"
  [nm] 
  (* nm 1.1508))

(defn miles->kilometers 
  "Convert statute miles to kilometers"
  [miles]
  (* 1.60934 miles)
)

(defn kilometers->miles 
  "Convert kilometers to Statute miles."
  [km]
  (/ km 1.60934)
)

(defn nauticalmiles->degrees
  "Convert nautical miles to degrees"
  [nm]
  (/ nm 60.0)
)

(defn degrees->nauticalmiles
  "Convert degrees to nautical miles"
  [deg]
  (* 60.0 deg)
)

