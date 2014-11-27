(ns lpath.core
  (:use lpath.core)
  (:gen-class)
  (:use clojure.pprint))
(require '[clojure.string :as str])

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defrecord route [#^int dest #^int cost])

(defrecord node [#^clojure.lang.PersistentVector neighbours])

(defn mk-node [_] (->node []))

(defn read-places []
  (let [lines (str/split (slurp "agraph") #"\n")
        num-lines (Integer/parseInt (get lines 0))
        node-vec (vec (take num-lines (iterate mk-node (->node []))))]    
    (letfn [(my-loop [nodes i]
              (let [nums (str/split (get lines i) #" ")
                    len (count nums)]
                (if (and (> len 2) (> (count lines) (+ i 0)))
                  (let [node-id (Integer/parseInt (get nums 0))
                        neighbour (Integer/parseInt (get nums 1))
                        cost (Integer/parseInt (get nums 2))
                        new-node (->node 
                                  (conj (:neighbours (get nodes node-id)) 
                                        (route. neighbour cost)))]
                    (if (= (count lines) (+ i 1))
                      (assoc nodes node-id new-node)
                      (my-loop (assoc nodes node-id new-node) (+ i 1))))
                  nodes)))]
      (my-loop node-vec 1))))

(defn get-longest-path [nodes #^long node-id-obj #^booleans visited]
  (let [node-id (long node-id-obj)]
    (aset visited node-id true)
    (let [max (atom (long 0))]
      (dorun (map (fn [#^route neighbour]
                    (if (not (aget visited (.dest neighbour)))
                       (let [dist (+ (.cost neighbour) (long (get-longest-path nodes (.dest neighbour) visited)))]
                        (if (> dist (long @max))
                          (reset! max dist)
                          nil))
                      nil))
                  (:neighbours (get nodes node-id))))
      (aset visited node-id false)
      @max)))

(defn -main []
  (let [nodes (to-array (read-places))
        visited (make-array Boolean/TYPE (count nodes))]
    (print (get-longest-path nodes 0 visited))))

(time (-main))
