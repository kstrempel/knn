(ns knn-incanter.core
  (:use [knn-incanter.cities :only (cities)])) 

(defn vector-subtract [v w]
  (map #(- %1 %2) v w))

(defn dot [v w]
  (->> (map #(* %1 %2) v w)
        (reduce +)))

(defn sum-of-squares [v]
  (dot v v))

(defn squared-distance [v w]
  (-> (vector-subtract v w)
      sum-of-squares))

(defn distance [v w]
  (-> (squared-distance v w)
      Math/sqrt))

(defn majority-vote [labels]
  (let [vote-counts (->> labels
                         frequencies
                         (sort-by second))
        [winner, winner-count] (last vote-counts)]
    (if (some
         #(and (= winner-count (second %))
               (not= winner (first %)))
         vote-counts)
      (majority-vote (take (dec (count labels)) labels))
      winner)))

(defn knn-classify [k labeled_points new_point]
  (let [by-distance (sort-by #(distance new_point (first %))
                             labeled_points)
        k-nearest-labels (->> by-distance
                              (take k)
                              (map #(second %)))]
    (majority-vote k-nearest-labels)))
