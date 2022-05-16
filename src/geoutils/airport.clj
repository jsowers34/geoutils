(ns geoutils.airport
  (:gen-class)
  (:require [geoutils.geographicposition :as gp])
)

(def filename "resources/airports.txt")

(def apt-keys [:icao :iata :name :city :country :elevation :latitude :longitude])

(defn str->int
  "Convert string to integer"
    [str]
    (Integer/parseInt str))

(defn str->double
  "Convert string to double"
  [str]
  (Double/parseDouble str))

(def conversions {:icao identity
                  :iata identity
                  :name identity
                  :city identity
                  :country identity
                  :elevation str->int
                  :latitude str->double
                  :longitude str->double})

(defn convert
  "Convert input strings to appropriate types in record"
    [apt-key value]
    ((get conversions apt-key) value))
                  
(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #":")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:icao \"ZYYJ\", :iata \"N/A\", :name \"YANJI\", :city \"YANJI\",
                              :country \"CHINA\", :elevation 191, 
                              :latitude 42.882, :longitude 129.448}
  Adapted from 'Clojure for the Brave and True'"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [apt-key value]]
                   (assoc row-map apt-key (convert apt-key value)))
                 {}
                 (map vector apt-keys unmapped-row)))
       rows))

(def airport-data (parse (slurp filename)))

(def airports (into [] (mapify airport-data)))

(defn get-latitude [icao]
  (get (first (get (group-by :icao airports) icao)) :latitude)
)

(defn get-longitude [icao]
  (get (first (get (group-by :icao airports) icao)) :longitude)
)

(defn get-position [icao]
  (assoc (gp/geographic-position (get-latitude icao) (get-longitude icao)) :name icao) 
)

