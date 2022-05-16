(ns geoutils.core-test
  (:require [clojure.test :refer :all]
            [geoutils.core :refer :all]
            [geoutils.constants :as nav]
            [geoutils.geographicposition :as gp]
            [geoutils.locations :as loc]
            [geoutils.cpadata :as data]
))

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

(deftest test1
  (testing "Get Latitude and Location"
    (is (close-to 39.380 (gp/get-latitude loc/AtlanticCity-NJ)))))

(deftest test2
  (testing "Get Longitude and Location"
    (is (close-to -74.453 (gp/get-longitude loc/AtlanticCity-NJ)))))

(deftest test3
  (testing "isVisible?   An eyeheight of 15', object in the water, should see to about 4 NM"
    (is (is-visible? 15 0 1))))

(deftest test3a
  (testing "Test Horizon."
    (is (close-to (horizon 25.0) 5.72))))

(deftest test4
  (testing "Heading: South to North"
    (is (close-to 0.0 (bearing loc/equatorPM loc/Greenwich-UK)))))

(deftest test5
  (testing "Heading: North to South")
    (is (close-to 180.0 (bearing loc/Greenwich-UK loc/equatorPM ))))

(deftest test6
  (testing "Heading: West to East")
    (is (close-to 90.0 (bearing loc/equatorPM loc/equator60E))))

(deftest test7
  (testing "Heading: East to West")
    (is (close-to -90.0 (bearing loc/equatorPM loc/equator60W))))

(deftest test8
  (testing "Compute new location. Start at Equator/Prime Meridian. Course 0.0 Distance 60nm."
    (is (close-to-position (update-position loc/equatorPM 00.0 60.0) (gp/geographic-position 1.0 0.0)))))

(deftest test9
  (testing "Compute new location. Start at Equator/Prime Meridian. Course 90.0 Distance 60nm."
    (is (close-to-position (update-position loc/equatorPM 90.0 60.0) (gp/geographic-position 0.0 1.0)) 0.005)))

(deftest test10
  (testing "Compute new location. Start at Equator/Prime Meridian. Course 180.0 Distance 60nm."
    (is (close-to-position (update-position loc/equatorPM 180.0 60.0) (gp/geographic-position -1.0 0.0)) 0.005 )))

(deftest test11
  (testing "Compute new location. Start at Equator/Prime Meridian. Course 270.0 Distance 60nm."
    (is (close-to-position (update-position loc/equatorPM 270.0 60.0) (gp/geographic-position 0.0 -1.0)) 0.005)))

(deftest testj01
  (testing "Test Great Circle 1: Starting point: Lat: 0.0N   Long: 1.0W
    Heading: 090 degrees,  Distance:  120 nm; End point Lat 0.0 Long 1.0E"
    (is (close-to-position (update-position (gp/geographic-position 0.0, -1.0) 90.0 120.0)
           (gp/geographic-position 0.0, 1.0)) 0.005)))

(deftest testj02
  (testing "Test Great Circle 2 
    Starting point: Lat: 32.73N   Long: 117.19W
    Heading:        65.8 degrees
    Distance:       2053.8 nm
    Endpoint should be at 39.87, -75.24"
    (is (close-to-position (update-position (gp/geographic-position 32.73 -117.19) 65.8 2053.8)
                                            (gp/geographic-position 39.867, -75.240)
                                            0.5))))

(deftest testj3
  (testing "Test Great Circle Distance
    Starting point:  LA, Lat: 33.74150N   Long: 118.23083W
    Stopping point:  Pt Fermin, Lat: 33.70733N   Long: 118.29333W
    Correct distance in nautical miles is 3.7324 nm"
    (is (close-to  3.7324 (distance-nm (gp/geographic-position 33.74150 -118.23083) 
                                       (gp/geographic-position 33.70733 -118.29333)) 
                                       0.5))

    ;;    Starting point:  Lat: 0N   Long: 1W
    ;;    Stopping point:  Lat: 0N   Long: 1E
    ;;    Correct distance in nm is 120 nm
    (is (close-to  120.0 (distance-nm (gp/geographic-position 0.0 -1.0) 
                                      (gp/geographic-position 0.0  1.0)) 0.5)) )

  ;;     Starting point:  Lat: 40N   Long: 75W
  ;;     Stopping point:  Lat: 39N   Long: 74W
  ;;     Correct distance in nm is 75.8 nm
  (is (close-to  75.8 (distance-nm (gp/geographic-position 40.0 -75.0) 
                                     (gp/geographic-position 39.0 -74.0)) 1.0)))

(deftest testj4
  (testing "Calculate bearing"

    ;; Starting point:  Lat: 0.0N   Long: 75.0W
    ;; Stopping point:  Lat: 0.0N   Long: 74.0W
    ;; Heading: 045 degrees ; Return should be 90.0 absolute
    (is (close-to 90.0 (bearing (gp/geographic-position 0.0 -75.0) 
                                (gp/geographic-position 0.0 -74.0))))

    ;; Starting point:  Lat: 0.0N   Long: 75.0W
    ;; Stopping point:  Lat: 0.0N   Long: 74.0W
    ;; Heading: 045 degrees ; Return should be 45.0 relative
    (is (close-to 45.0 (relative-bearing (gp/geographic-position 0.0 -75.0) 45.0
                                         (gp/geographic-position 0.0 -74.0))))

    ;; Starting point:  Lat: 40.0N   Long: 75.0W
    ;; Stopping point:  Lat: 39.0N   Long: 74.0W
    ;; Heading: 090 degrees.  Return should be 52.346 relative.
    (is (close-to 52.346 (relative-bearing (gp/geographic-position 40.0 -75.0) 90.0
                                           (gp/geographic-position 39.0 -74.0)) 0.5))))

(deftest testj5
  (testing "Calculate position using course, speed and time from starting point."
    (is (close-to-position 
         (calculate-gp-using-course-speed-time (gp/geographic-position 0.0 -1.0) 90.0 60.0 1.0)
         (gp/geographic-position 0.0 0.0) 
         0.5))

    ;; Starting point: Lat: 0.0N   Long: 1.0W
    ;; Heading:        090 degrees
    ;; Speed:          240.0 knots
    ;; Time Interval:   30 minutes
    (is (close-to-position 
         (calculate-gp-using-course-speed-time (gp/geographic-position 0.0 1.0) 90.0 240.0 0.5)
         (gp/geographic-position 0.0 3.0) 0.5) 
        )))

