(ns lpath
  (:gen-class)
  (:use clojure.pprint))
(require '[clojure.string :as str])

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(deftype route [#^int dest #^int cost])

(deftype node [#^clojure.lang.PersistentVector neighbours])

(defn mk-node [_] (->node []))

(defn read-places []
  (let [lines (str/split (slurp "agraph") #"\n")
        num-lines (Integer/parseInt (get lines 0))
        node-vec (vec (take num-lines (iterate mk-node (->node []))))]    
    (letfn [(my-loop [nodes i]
           (let [nums (str/split (get lines i) #" ")
                 len (count nums)]
             (if (and (> len 2) (> (count lines) (+ i 1))) 
               (let [node-id (Integer/parseInt (get nums 0))
                     neighbour (Integer/parseInt (get nums 1))
                     cost (Integer/parseInt (get nums 2))
                     new-node (->node 
                               (conj (.neighbours (get nodes node-id)) 
                                     (route. neighbour cost)))]
                 (my-loop (assoc nodes node-id new-node) (+ i 1)))
               nodes)))]
      (my-loop node-vec 1))))

(defn get-longest-path [#^clojure.lang.PersistentVector nodes #^long node-id #^booleans visited]
  (aset visited node-id true)
  (let [max (atom 0)]
    (dorun (map (fn [#^route neighbour]
                  (if (not (aget visited (.dest neighbour)))
                    (let [dist (+ (.cost neighbour) (get-longest-path nodes (.dest neighbour) visited))]
                      (if (> dist @max)
                        (reset! max dist)
                        nil))
                    nil))
                (.neighbours (get nodes node-id))))
    (aset visited node-id false)
    @max))

(def nodes (read-places))
(def visited (make-array Boolean/TYPE (count nodes)))
(print (get-longest-path nodes 0 visited))
