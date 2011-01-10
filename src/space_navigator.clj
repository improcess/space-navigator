(ns
    #^{:doc "A library with which to read values from a 3DConnexion SpaceNavigator 3D mouse"
       :author "Sam Aaron"}
  space-navigator
  (:import (net.java.games.input AbstractController AbstractComponent ControllerEnvironment)))

(defrecord SpaceNavigator       [name controller x-com y-com z-com rx-com ry-com rz-com l-btn-com r-btn-com])
(defrecord SpaceNavigatorValues [x y z rx ry rz l-btn r-btn])

(defn- find-controller
  "Find a HID controller that matches the supplied regexp name-matcher. If there is more than one match, the first is selected"
  [name-matcher]
  (let [default-env (ControllerEnvironment/getDefaultEnvironment)
        matcher     (fn [contr] (re-find name-matcher (.toString contr)))
        controller  (first (filter matcher (.getControllers default-env)))]
    (if-not controller (throw (Exception. (str "Could not find HID controller matching " name-matcher ". Is the device connected?"))))

    controller))

(defn- components
  "Fetch a list of components associated with a given controller"
  [controller]
  (let [component-list (.getComponents controller)]
    (reduce #(assoc %1 (.getName %2) %2) {} component-list)))

(defn- find-component
  "Find a controller's component based on the component's name"
  [controller component-name]
  (let [components (components controller)]
    (components component-name)))

(defn- read-component
  "Read the value of a given controller's component without polling the controller to refresh the values"
  [#^AbstractComponent component]
  (.getPollData component))

(defn- space-navigator-controller
  "Find a HID controller matching the name SpaceNavigator. Raises an exception if none could be found or if the components of the device found don't match the expected capibilities"
  []
  (let [controller      (find-controller #"SpaceNavigator")
        expected-comps  ["x" "y" "z" "rx" "ry" "rz" "0" "1"]
        components      (components controller)
        has-comp?       (fn [comp-name] (contains? components comp-name))]
    (if (some false? (map has-comp? expected-comps))
      (throw (Exception. (str "Controller didn't have the required components. Expected " expected-comps ", found: " (keys components)))))

    controller))

(defn space-navigator
  "Create a SpaceNavigator record representing an attached device. If more than one attached device matches the name SpaceNavigator, the first is chosen."
  []
  (let [controller (space-navigator-controller)
        name       (.getName controller)
        x-com      (find-component controller "x")
        y-com      (find-component controller "y")
        z-com      (find-component controller "z")
        rx-com     (find-component controller "rx")
        ry-com     (find-component controller "ry")
        rz-com     (find-component controller "rz")
        l-btn-com  (find-component controller "0")
        r-btn-com  (find-component controller "1")]
    (SpaceNavigator. name controller x-com y-com z-com rx-com ry-com rz-com l-btn-com r-btn-com)))

(defn read-vals
  "Read all of the values for a given SpaceNavigator record. Returns a SpaceNavigatorValues record mapping component name to the current value."
  [space-navigator]
  (let [#^AbstractController controller (:controller space-navigator)
        _                               (.poll controller)
        x     (read-component (:x-com     space-navigator))
        y     (read-component (:y-com     space-navigator))
        z     (read-component (:z-com     space-navigator))
        rx    (read-component (:rx-com    space-navigator))
        ry    (read-component (:ry-com    space-navigator))
        rz    (read-component (:rz-com    space-navigator))
        l-btn (read-component (:l-btn-com space-navigator))
        r-btn (read-component (:r-btn-com space-navigator))]
    (SpaceNavigatorValues. x y z rx ry rz l-btn r-btn)))

(defn find-min-max-vals
  "Repeatedly polls the space-navigator for t seconds and returns a map of the max and min vals for each of the available axis."
  [space-nav t]
  (loop [min-x 0 max-x 0 min-y 0  max-y 0 min-z 0 max-z 0 min-rx 0 max-rx 0 min-ry 0 max-ry 0 min-rz 0 max-rz 0 iters (* time 100)]
    (let [vals (read-vals space-nav)]
      (Thread/sleep 10)
      (if (< iters 0)
        {:min-x min-x :max-x max-x :min-y min-y  :max-y max-y  :min-z min-z  :max-z max-z  :min-rx min-rx  :max-rx max-rx  :min-ry min-ry  :max-ry max-ry  :min-rz min-rz  :max-rz max-rz}
        (recur (min min-x (:x vals))
               (max max-x (:x vals))
               (min min-y (:y vals))
               (max max-y (:y vals))
               (min min-z (:z vals))
               (max max-z (:z vals))
               (min min-rx (:rx vals))
               (max max-rx (:rx vals))
               (min min-ry (:ry vals))
               (max max-ry (:ry vals))
               (min min-rz (:rz vals))
               (max max-rz (:rz vals))
               (dec iters))))))

(def SAMPLE-RANGES
  {:min-x -0.8482, :min-y -0.944, :min-z -0.884, :max-rz 0.75, :max-z 0.98, :max-y 0.92999995, :max-ry 0.87, :min-rx -0.856, :min-ry -0.854, :max-rx 0.88, :max-x 0.9})

(defn make-scale-vals
  "Returns a fn which given a set a of vals will return new SpaceNavigatorValues record with values scaled to
  the supplied ranges after being calibrated with calibrated-ranges.

  ranges:

  If you wish to scale the values to ranges other than the default (-1 to 1) then supply new min and max vals
  in this map. For example, a range of {:min-x 220 :max-x 440} will return an x val between 220 and 440 with
  the centre position as 330.

  calibrated-ranges:

  Typically all Space Nav values fall within the range -1 to 1, however it seems that these values aren't
  always attainable with a given device and it's assumed not all devices are exactly the same. To correct
  for this you may pass in calbration values representing the actual ranges available to your device.
  For example, to compensate for a device that appears to only be able to return x vals within the range
  -0.8 to 0.9 use:
  {:min-x -0.8 :max-x 0.9}"
  [ranges calibrated-ranges]
  (let [min-x (or (:min-x ranges) -1)
        max-x (or (:max-x ranges) 1)
        min-y (or (:min-y ranges) -1)
        max-y (or (:max-y ranges) 1)
        min-z (or (:min-z ranges) -1)
        max-z (or (:max-z ranges) 1)
        min-rx (or (:min-rx ranges) -1)
        max-rx (or (:max-rx ranges) 1)
        min-ry (or (:min-ry ranges) -1)
        max-ry (or (:max-ry ranges) 1)
        min-rz (or (:min-rz ranges) -1)
        max-rz (or (:max-rz ranges) 1)

        range-x (- max-x min-x)
        range-y (- max-y min-y)
        range-z (- max-z min-z)
        range-rx (- max-rx min-rx)
        range-ry (- max-ry min-ry)
        range-rz (- max-rz min-rz)

        calibrated-min-x (or (:min-x calibrated-ranges) -1)
        calibrated-max-x (or (:max-x calibrated-ranges) 1)
        calibrated-min-y (or (:min-y calibrated-ranges) -1)
        calibrated-max-y (or (:max-y calibrated-ranges) 1)
        calibrated-min-z (or (:min-z calibrated-ranges) -1)
        calibrated-max-z (or (:max-z calibrated-ranges) 1)
        calibrated-min-rx (or (:min-rx calibrated-ranges) -1)
        calibrated-max-rx (or (:max-rx calibrated-ranges) 1)
        calibrated-min-ry (or (:min-ry calibrated-ranges) -1)
        calibrated-max-ry (or (:max-ry calibrated-ranges) 1)
        calibrated-min-rz (or (:min-rz calibrated-ranges) -1)
        calibrated-max-rz (or (:max-rz calibrated-ranges) 1)

        neg-calibrated-min-x (- calibrated-min-x)
        neg-calibrated-min-y (- calibrated-min-y)
        neg-calibrated-min-z (- calibrated-min-z)
        neg-calibrated-min-rx (- calibrated-min-rx)
        neg-calibrated-min-ry (- calibrated-min-ry)
        neg-calibrated-min-rz (- calibrated-min-rz)

        calibrated-range-x (- calibrated-max-x calibrated-min-x)
        calibrated-range-y (- calibrated-max-y calibrated-min-y)
        calibrated-range-z (- calibrated-max-z calibrated-min-z)
        calibrated-range-rx (- calibrated-max-rx calibrated-min-rx)
        calibrated-range-ry (- calibrated-max-ry calibrated-min-ry)
        calibrated-range-rz (- calibrated-max-rz calibrated-min-rz)]

    (fn [vals]
      (let [x (+ min-x (* (/ (+ neg-calibrated-min-x (:x vals)) calibrated-range-x) range-x))
            y (+ min-y (* (/ (+ neg-calibrated-min-y (:y vals)) calibrated-range-y) range-y))
            z (+ min-z (* (/ (+ neg-calibrated-min-z (:z vals)) calibrated-range-z) range-z))
            rx (+ min-rx (* (/ (+ neg-calibrated-min-rx (:rx vals)) calibrated-range-rx) range-rx))
            ry (+ min-ry (* (/ (+ neg-calibrated-min-ry (:ry vals)) calibrated-range-ry) range-ry))
            rz (+ min-rz (* (/ (+ neg-calibrated-min-rz (:rz vals)) calibrated-range-rz) range-rz))]
        (SpaceNavigatorValues. x y z rx ry rz (:l-btn vals) (:r-btn vals))))))

(defn- diff
  "Given two numbers a and b returns the difference between them.
   (difference 1 2) ;=> 1
   (difference -1 2) ;=> 3
   (difference -1 -2) ;=> 1"
  [a b]
  (Math/abs (- a b)))

(def DEFAULT-TOLERANCE 0.01)

(defn different?
  "Given two sets of vals and a tolerance will determine whether the vals are sufficiently different"
  [vals1 vals2 tolerance]
  (or (< tolerance (diff (:x vals1) (:x vals2)))
      (< tolerance (diff (:y vals1) (:y vals2)))
      (< tolerance (diff (:z vals1) (:z vals2)))
      (< tolerance (diff (:rx vals1) (:rx vals2)))
      (< tolerance (diff (:ry vals1) (:ry vals2)))
      (< tolerance (diff (:rz vals1) (:rz vals2)))
      (not (= (:l-btn vals1) (:l-btn vals2)))
      (not (= (:r-btn vals1) (:r-btn vals2)))))

(def REFRESH-DELAY 50)

(defn refresh [vals sn f scaler tolerance]
  (Thread/sleep REFRESH-DELAY)
  (send-off *agent* refresh sn f scaler tolerance)
  (let [new-vals (read-vals sn)]
    (if (different? vals new-vals tolerance)
      (do
        (f (scaler vals))
        new-vals)
      vals)))

(defn on-diff-vals
  ([s f] (on-diff-vals s f {} {} DEFAULT-TOLERANCE))
  ([s f ranges] (on-diff-vals s f ranges {} DEFAULT-TOLERANCE))
  ([s f ranges calibrated-ranges] (on-diff-vals s f ranges calibrated-ranges DEFAULT-TOLERANCE))
  ([s f ranges calibrated-ranges tolerance]
     (let [scaler (make-scale-vals ranges calibrated-ranges)
           refresher (agent (read-vals s))]
       (send-off refresher refresh s f scaler tolerance)
       refresher)))
