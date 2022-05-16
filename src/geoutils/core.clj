(ns geoutils.core
  (:gen-class)
  (:require [geoutils.constants :as nav] 
            [geoutils.geographicposition :as gp]
            [geoutils.locations :as loc]
            [geoutils.airport :as air]
            [geoutils.cpadata :as data]
            ))

(use '(incanter core))

;;; First some general utility functions
(defn to-radians 
  "Converts degrees to radians"
  [deg] 
  (* deg nav/PI-OVER-180)
)

(defn to-degrees
  "Convert radians to degrees."
  [rad]
  (/ rad nav/PI-OVER-180)
)

(defn km->nm
  "Kilometers to Nautical Miles"
  [km]
  (* km 0.53996)
)

(defn nm->km 
  "Nautical Miles to Kilometers"
  [nm]
  (* nm 1.852)
)

(defn- neg
  "Negate value"
  [x]
  (* -1.0 x)
)

(defn rel->abs 
  "Given a heading and a relative bearing, computes the absolute bearing. (I/O in degrees)"
  [heading relbearing]
  (mod (+ heading relbearing) 360.0))

(defn abs->rel
  "Given a heading and an absolute bearing, computes the relative bearing. (I/O in degrees)"
  ([heading absbearing]
   (mod (- absbearing heading) 360.0)))

(defn- sqr [x]  (* x x))

(defn within-tolerance
  "Determine if 2 floating point numbers are within a given tolerance (default 0.00005)."
  ([x y]
    (<= (abs (- x y)) 0.00005))
  ([x y tolerance]
 (<= (abs (- x y)) tolerance)))

(defn dms 
  "Convert from decimal degrees to degrees-minutes-seconds.
   Adapted from  R. Cambell (gethub)
   Example:    (dms :longitude 10.5)
               ;;==> {:degrees 10, :minutes 30, :seconds 0, :direction :east}

               (dms :latitude -10.55)
               ;;==> {:degrees 10, :minutes 33, :seconds 0, :direction :south}
"
[type degrees]
  {:pre [(#{:latitude :longitude} type)]}
  "Conversion from Decimal Degree to DMS (Degrees, Minutes, Seconds)"
  (let [d  (Math/abs degrees)
        dw (int d)
        df (- d dw)
        m  (* df 60)
        mw (int m)
        mf (- m mw)
        sw (Math/round (* mf 60))
        [sw mw] (if (== sw 60) [0 (inc mw)] [sw mw])
        [mw dw] (if (== mw 60) [0 (inc dw)] [mw dw])]
    {:degrees   dw
     :minutes   mw
     :seconds   sw
     :direction (case type
                      :latitude  (if (neg? degrees) :south :north)
                      :longitude (if (neg? degrees) :west  :east ))
}))

;;; Now the Navigational Utility Functions

(defn get-line-of-sight-distance 
  "Return line-of-sight distance (nm) from the eye to an object.  The heights
   of the eye and object are in feet."
  [eye-ht-ft obj-ht-ft]
  (* 1.144 (+ (Math/sqrt eye-ht-ft) (Math/sqrt obj-ht-ft)))
)
  
(defn is-visible? 
  "Returns a TRUE if the object is 'visible' from the eye when
   separated by a distance (NM); obj and eye are in feet"
  [eye-ht-ft obj-ht-ft distance]
  (<= distance (get-line-of-sight-distance eye-ht-ft obj-ht-ft))
)

(defn horizon 
  "Compute the distance (NM) to the horizon from an observer at a height given in feet."
  [eye-ht-ft]
  (get-line-of-sight-distance eye-ht-ft 0.0)
  )

(defn distance-nm
  "Computes the nautical miles between two points."
  [startp endp]
  (let [rlat1 (to-radians (gp/get-latitude  startp))
        rlon1 (to-radians (gp/get-longitude startp))
        rlat2 (to-radians (gp/get-latitude    endp))
        rlon2 (to-radians (gp/get-longitude   endp))
        dlat  (- rlat1 rlat2)
        dlon  (- rlon2 rlon1)
        a     (+ (sqr (sin (/ dlat 2.0))) (* (cos rlat1) (* (cos rlat2) (sqr (sin (/ dlon 2.0))))))
        c     (* 2.0 (atan2 (sqrt a), (sqrt (- 1.0 a))))]
    (* c nav/EARTH-RADIUS-NM)
    ))

(defn distance-km
  "Computes the kilometers between two points."
  [startp endp]
  (nm->km (distance-nm startp endp)))

(defn bearing
  "Returns the bearing (in degrees) from a starting point to and end point. Same as initial bearing."
  [startp endp]
  (let [rlat1 (to-radians (gp/get-latitude  startp))
        rlon1 (to-radians (gp/get-longitude startp))
        rlat2 (to-radians (gp/get-latitude    endp))
        rlon2 (to-radians (gp/get-longitude   endp))
        dlon  (- rlon2 rlon1)]
    (to-degrees (atan2 (* (sin dlon) (cos rlat2))
                       (- (* (cos rlat1) (sin rlat2)) (* (sin rlat1) (* (cos rlat2) (sqr (cos dlon)))))))
))

(defn initial-bearing
  "Returns the bearing (in degrees) from a starting point to and end point. At the start point."
  [startp endp]
  (bearing startp endp))

(defn final-bearing
  "Returns the bearing (in degrees) from a starting point to and end point. At the end point."
  [startp endp]
  (mod (+ 180.0 (bearing endp startp)) 360.0))

(defn relative-bearing
"Compute the initial relative bearing given the start point, heading from start and end point."
[startp heading endp]
  (abs->rel heading (bearing startp endp))
)

(defn midpoint
  "Computes the position along a great circle midway between two points."
  [startp endp]
  (let [rlat1 (to-radians (gp/get-latitude  startp))
        rlon1 (to-radians (gp/get-longitude startp))
        rlat2 (to-radians (gp/get-latitude    endp))
        rlon2 (to-radians (gp/get-longitude   endp))
        dlon  (- rlon2 rlon1)
        bx    (* (cos rlat2) (cos dlon))
        by    (* (cos rlat2) (sin dlon))
        rlatm (atan2 (+ (sin rlat1) (sin rlat2))
                     (sqrt (+ (sqr by) (sqr (+ bx (cos rlat1))))))
        rlonm (+ rlon1 (atan2 by (+ bx (cos rlat1))))
        ]
      (gp/geographic-position (to-degrees rlatm) (to-degrees rlonm))
	))

(defn- angular-distance 
  "Angular distance (radians) given separation in nautical miles"
  [d] 
  (/ d nav/EARTH-RADIUS-NM)
)

(defn intermediate-point
  "Computes the position along a great circle a given fraction of the way between two points.
  If fraction is 0, then the start point is returned; if fraction is 1.0 then the end point."
  [startp endp fraction]
  (let [rlat1 (to-radians (gp/get-latitude  startp))
        rlon1 (to-radians (gp/get-longitude startp))
        rlat2 (to-radians (gp/get-latitude    endp))
        rlon2 (to-radians (gp/get-longitude   endp))
        dlon  (- rlon2 rlon1)
        d     (distance-nm startp endp)
        ad    (angular-distance d)
        a     (/ (sin (* ad (- 1.0 fraction))) (sin ad))
        b     (/ (sin (* fraction ad)) (sin ad))
        x     (+ (* a (* (cos rlat1) (cos rlon1))) (* b (* (cos rlat2) (cos rlon2))))
        y     (+ (* a (* (cos rlat1) (sin rlon2))) (* b (* (cos rlat2) (sin rlon2))))
        z     (+ (* a (sin rlat2)) (* b (sin rlat2)))
        rlati (atan2 z (sqrt (+ (sqr x) (sqr y))))
        rloni (atan2 y x)
        ]
    (if (= fraction 0.0) 
      startp
      (if (= fraction 1.0)
        endp
        (gp/geographic-position (to-degrees rlati) (to-degrees rloni))
        ))))


(defn isLocation? 
  "Returns TRUE if the input item is a geographical position"
  [lc]
  (= geoutils.geographicposition.geopos (type lc))
)

(defn all-locations 
  "Filters out the functions from the list, leaving the locations"
  []
  (filter isLocation?(map eval (vec (map symbol  (map #(str "loc/" %) (loc/list-locations)))))))


(defn all-distances 
  "A vector of all the nautical mile distances between the input location and all others available."
  [pos] 
  (let [all-items    (map #(str "loc/" %) (loc/list-locations))]
    (vec (map #(distance-nm pos %) (all-locations)))
    ))

(defn closest-location 
  "Return the closest location to the input geographical position"
  [pos]
  (let [idx  (first (apply min-key second (map-indexed vector (all-distances pos))))]
       (get (into [] (all-locations)) idx)))

(defn closest-distance
  "Return the distance in nm and the closest location to the input geographical position."
  [pos]
  (let [idx  (first (apply min-key second (map-indexed vector (all-distances pos))))
        clpt (get (into [] (all-locations)) idx)]
    [(get (all-distances pos) idx) clpt]
))

(defn update-position
  "Get new geographical position from a starting point given a bearing and a distance(nm)"
  [startp heading distance]
  (let [rlat1 (to-radians (gp/get-latitude  startp))
        rlon1 (to-radians (gp/get-longitude startp))
        b     (to-radians heading)
        angd  (angular-distance distance)
        rlat2 (asin (+ (* (sin rlat1) (cos angd)) (* (cos rlat1) (* (sin angd) (cos b)))))
        rlon2 (+ rlon1 (atan2 (* (sin b) (* (sin angd) (cos rlat1))) 
                              (- (cos angd) (* (sin rlat1) (sin rlat2)))))]
    (gp/geographic-position (to-degrees rlat2) (to-degrees rlon2))))


(defn- great-circle-intersection
  "Tentative - some errors plus algorithm has issues depending on certain positions and headings."
  [sp1 hdg1 sp2 hdg2]
    (let [rlat1 (to-radians (gp/get-latitude  sp1))
          rlon1 (to-radians (gp/get-longitude sp1))
          rlat2 (to-radians (gp/get-latitude  sp2))
          rlon2 (to-radians (gp/get-longitude sp2))
          b1    (to-radians hdg1)
          b2    (to-radians hdg2)
          dlat  (- rlat1 rlat2)
          dlon  (- rlon1 rlon2)
          r     (* 2.0 (asin (sqrt (+ (sqr (sin (/ dlat 2.0))) (* (cos rlat1) (* (cos rlat2) (sqr (sin (/ dlon 2.0)))))))))
          rlata (acos (/ (- (sin rlat2) (* (sin rlat1) (cos r))) (* (sin r) (cos rlat1))))
          rlatb (acos (/ (- (sin rlat1) (* (sin rlat2) (cos r))) (* (sin r) (cos rlat2))))
          tmp   (if (> 0 (sin dlon)) (list rlata (- nav/PI2 rlatb)) (list (- nav/PI2 rlata) rlatb))
          rlat12 (first tmp)
          rlat21 (last tmp)
          a1     (- (mod (+ nav/PI (- b1 rlat12)) nav/PI2) nav/PI)
          a2     (- (mod (+ nav/PI (- rlat21 b2)) nav/PI2) nav/PI)
          a3     (acos (+ (* (neg (cos a1)) (cos a2)) (* (sin a1) (* (sin a2)) (cos r))))
          y      (atan2 (* (sin r) (sin a1) (sin a2)) (+ (cos a2) (* (cos a1) (cos a3))))
          rlat3  (asin (+ (* (sin rlat1) (cos y)) (* (cos rlat1) (* (sin y)) (cos b1))))
          dlon3  (atan2 (* (sin b1) (sin y) (cos rlat1)) 
                        (- (cos y) (* (sin rlat1) (sin rlat3))))
          rlon3  (- (mod (+ rlon1 dlon3 nav/PI) nav/PI2) nav/PI)
	 ]
      (gp/geographic-position (to-degrees rlat3) (to-degrees rlon3))
	 ))


(defn calculate-gp-using-course-speed-time
  "Compute a new location based on starting location and course, speed and time.
     course is in degrees, speed in knots and time-interval in hours"
  [startpos course speed time-interval]
  (let [local-lat (gp/get-latitude startpos)
        local-lon (gp/get-longitude startpos)]
    (if (<= speed 0.0 )
      startpos ;; no movement - endpos = startpos 
      (update-position startpos course (* speed time-interval))
      )))

(defn- calculate-cpa
  "Tentative: Compute the closest point of approach."
  [approachp approach-crs approach-speed targetp target-crs target-speed]
  (def ^:dynamic code-val)
  (def ^:dynamic approach-course)
  (def ^:dynamic approach-course-rel)
  (def ^:dynamic approach-rb)
  (binding [code-val :valid
            approach-course  (to-radians approach-crs)
            approach-course-rel 0.0
            approach-rb 0.0
            ]
    (let [
        epsilon          0.000001
        target-course    (to-radians target-crs)
        approach-sin     (sin approach-course)
        approach-cos     (cos approach-course) 
        approach-speed-x (* approach-speed approach-cos)
        approach-speed-y (* approach-speed approach-sin)
        target-sin       (sin target-course)  
        target-cos       (cos target-course) 
        target-speed-x   (* target-speed target-sin)
        target-speed-y   (* target-speed target-cos)
        range-to-target  (distance-nm approachp targetp)
        approach-speed-x-rel
              (- (+ (* approach-speed-x target-cos) (* approach-speed-y target-sin)) target-speed)
        approach-speed-y-rel
              (- (* approach-speed-y target-cos) (* approach-speed-x target-sin))
        rel-velocity (sqrt (+ (sqr approach-speed-x-rel) (sqr approach-speed-y-rel)))
        ]

      ;; DEBUG initial variables
      (println "Initial Variable values:")
      (print "Approach position: " )(gp/to-string approachp)
      (print "Target position  : ") (gp/to-string targetp)
      (println "approach-course = " (to-degrees approach-course))
      (println "target-course = " (to-degrees target-course))
      ;(println "approach-sin = " approach-sin)
      ;(println "approach-cos = " approach-cos)
      (println "approach-speed-x = " approach-speed-x)
      (println "approach-speed-y = " approach-speed-y)
      ;(println "target-sin      = " target-sin     )
      ;(println "target-cos      = " target-cos     )
      (println "target-speed-x  = " target-speed-x )
      (println "target-speed-y  = " target-speed-y )
      ;(println "range-to-target = " range-to-target)
      (println "approach-speed-x-rel = " approach-speed-x-rel)
      (println "approach-speed-y-rel = " approach-speed-y-rel)
      (println "rel-velocity = " rel-velocity)
      
      (if (< rel-velocity epsilon)
        (set! code-val :no-relative-motion)
        (do   ; Either :valid or :receding
          (if (<= approach-speed-x-rel epsilon)
            (if (<= approach-speed-y-rel epsilon)
              (set! approach-course-rel 0.0)
              (do
                (set! approach-course-rel (atan (/ approach-speed-y-rel approach-speed-x-rel)))
                (if (<= approach-speed-x-rel epsilon)
                  (if (<= approach-speed-y-rel epsilon)
                    (set! approach-course-rel 0.0)
                    (set! approach-course-rel  (- approach-course-rel nav/PI))
                    )
                  )
                )
              )
            )

          ;; DEBUG
          (printf "Target Course = %.5f\n" (to-degrees target-course))
          (printf "Approach Course Relative = %.5f\n" (to-degrees approach-course-rel))
          
          (set! approach-rb 
                (relative-bearing approachp (- (to-degrees target-course) (to-degrees approach-course-rel)) 
                                  targetp))

          (printf "Approach RelBearing: %.5f%s\n" approach-rb nav/deg-mark)

          (if (> (abs approach-rb) 90.0)
            (set! code-val :receding))))

      ;; Build the output data
      (if (= code-val :valid)
        (let [dist (* range-to-target (cos approach-rb))
              hours (/ dist rel-velocity)]
                  (data/cpa-data 
                   (calculate-gp-using-course-speed-time approachp approach-speed approach-course hours)
                   (* hours approach-speed) (* hours 3600.0) (* range-to-target (sin approach-rb)) :valid))
        (data/cpa-data approachp range-to-target 0.0 range-to-target code-val)))))


(defn northernmost-point
  "Computes the northermost point of a great circe between two locations"
  [startp endp]
  (let [rlat1 (to-radians (gp/get-latitude  startp))
        rlon1 (to-radians (gp/get-longitude startp))
        rlat2 (to-radians (gp/get-latitude    endp))
        rlon2 (to-radians (gp/get-longitude   endp))
        dlon  (- rlon2 rlon1)
        ax    (* (cos rlat1) (cos rlon1))
        ay    (* (cos rlat1) (sin rlon1))
        az    (sin rlat1)
        bx    (* (cos rlat2) (cos rlon2))
        by    (* (cos rlat2) (sin rlon2))
        bz    (sin rlat2)
        cx    (- (* ay bz) (* az by))
        cy    (- (* az bx) (* ax bz))
        cz    (- (* ax by) (* ay bx))
        mag   (sqrt (+ (sqr cx) (sqr cy) (sqr cz)))
        dx    (/ cx mag)
        dy    (/ cy mag)
        dz    (/ cz mag)
        rdlat (asin dz)
        rdlon (atan2 dy, dx)
        ]
    (if (and (= dx 0.0) (= dy 0.0))
      nil
      (gp/geographic-position (+ 90.0 (to-degrees rdlat)) (to-degrees rdlon) )
)))

(defn equator-points
  "Computes the 2 points where a great circle intersects the equator"
 [startp endp]
  (let [np   (northernmost-point startp endp)
        eq1  (gp/geographic-position 0.0 (+ 90.0 (gp/get-longitude np)))
        eq2  (gp/geographic-position 0.0 (- (gp/get-longitude np) 90.0))
        ]
    (list eq1 eq2)))




(defn -main
  "Geographic Utilities - handy for navigation problems."
  [& args]
  (println "Hello, Navigator"))

