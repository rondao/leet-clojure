(ns leet-clojure.84 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn filter-lower-histogram-boxes
    "Parameter 'histogram-boxes' is a sorted map with histogram bars in accending order.
     The 'key' of this sorted map is the bar length, and the 'value' is how many times the bar can repeat (as a box).
     Return another sorted map with only the bars lower than the 'filter'."
    [histogram-boxes filter]
    (reduce-kv #(if (<= %2 filter) (assoc %1 %2 %3) (reduced %1))
               (sorted-map)
               histogram-boxes))
  (is (= {} (filter-lower-histogram-boxes {1 5 2 4 4 1} 0)))
  (is (= {1 5} (filter-lower-histogram-boxes {1 5 2 4 4 1} 1)))
  (is (= {1 5 2 4} (filter-lower-histogram-boxes {1 5 2 4 4 1} 3)))
  (is (= {1 5 2 4 4 1} (filter-lower-histogram-boxes {1 5 2 4 4 1} 4))))

(with-test
  (defn filter-higher-histogram-boxes
    "Parameter 'histogram-boxes' is a sorted map with histogram bars in accending order.
     The 'key' of this sorted map is the bar length, and the 'value' is how many times the bar can repeat (as a box).
     Return another sorted map with only the bars higher than the 'filter'."
    [histogram-boxes filter]
    (reduce-kv #(if (>= %2 filter) (assoc %1 %2 %3) %1)
               (sorted-map)
               histogram-boxes))
  (is (= {1 5 2 4 4 1} (filter-higher-histogram-boxes {1 5 2 4 4 1} 0)))
  (is (= {2 4 4 1} (filter-higher-histogram-boxes {1 5 2 4 4 1} 2)))
  (is (= {4 1} (filter-higher-histogram-boxes {1 5 2 4 4 1} 3)))
  (is (= {} (filter-higher-histogram-boxes {1 5 2 4 4 1} 5))))

(with-test
  (defn inc-histogram-boxes
    "Parameter 'histogram-boxes' is a sorted map with histogram bars in accending order.
     The 'key' of this sorted map is the bar length, and the 'value' is how many times the bar can repeat (as a box).
     Increment the 'value' of every item in 'histogram-boxes'.
     If 'new-key' is not present in 'histogram-boxes', add it with value 'new-value'."
    [histogram-boxes new-key new-value]
    (let [incremented-boxes (reduce-kv #(assoc %1 %2 (inc %3)) (sorted-map) histogram-boxes)]
      (if (contains? incremented-boxes new-key)
        incremented-boxes
        (assoc incremented-boxes new-key new-value))))
  (is (= {3 1} (inc-histogram-boxes {} 3 1)))
  (is (= {3 2 4 1} (inc-histogram-boxes {3 1} 4 1)))
  (is (= {2 4 3 3 4 2} (inc-histogram-boxes {3 2 4 1} 2 4))))

(with-test
  (defn find-largest-rectangle
    "Parameter 'histogram-boxes' is a sorted map with histogram bars in accending order.
     The key of this sorted map is the bar length, and the value is how many times the bar can repeat (as a box).
     A rectangle of a bar is the 'key' multiplied by it's 'value'.
     Return the largest rectangle possible from all the bars in 'histogram-boxes'"
    [histogram-boxes]
    (apply max 0 (map * (keys histogram-boxes) (vals histogram-boxes))))
  (is (= 0 (find-largest-rectangle {})))
  (is (= 5 (find-largest-rectangle {1 5})))
  (is (= 8 (find-largest-rectangle {1 5 2 4})))
  (is (= 8 (find-largest-rectangle {1 5 2 4 4 1}))))

(with-test
  (defn get-highest-key
    "Param 'map-sorted' is sorted in ascending order.
      so the last element has the highest 'key' value."
    [map-sorted]
    (first (or (last map-sorted)
               [0])))
  (is (= 0 (get-highest-key {})))
  (is (= 1 (get-highest-key {1 5})))
  (is (= 2 (get-highest-key {1 5 2 4})))
  (is (= 4 (get-highest-key {1 5 2 4 4 1}))))

(with-test
  (defn get-box-length
    "Parameter 'histogram-boxes' is a sorted map with histogram bars in accending order.
     The 'key' of this sorted map is the bar length, and the 'value' is how many times the bar can repeat (as a box).
     If 'bar' is not in 'histogram-boxes', it's box size is the size of the box in front plus one.
     Return this 'bar' box's size."
    [histogram-boxes bar]
    (or (inc (last (first (filter-higher-histogram-boxes histogram-boxes bar))))
        1))
  (is (= 3 (get-box-length {4 2} 2)))
  (is (= 4 (get-box-length {2 3 4 2} 1)))
  (is (= 3 (get-box-length {1 4 2 3 4 2} 3))))

(with-test
  (defn leet-84
    "Given an array of integers heights representing the histogram's bar height where the width of each bar is 1, return the area of the largest rectangle in the histogram.

   Example 1:
     Input: heights = [2,1,5,6,2,3]
     Output: 10
   Explanation: The above is a histogram where width of each bar is 1.
   The largest rectangle is shown in the red area, which has an area = 10 units.

   Example 2:
     Input: heights = [2,4]
     Output: 4

   Constraints:
    1 <= heights.length <= 105
    0 <= heights[i] <= 104
   "
;; Suppose bar B have value 2, for every other bar in front of B that has value higher than 2 
;;  B will fit a box of height 2 and length equal to the number of bars in front of it plus 1. 
;; The moment a bar appear that has value less than 4, then B cannot form a bigger box past it.
;; With this, we can find the max box size for every bar.
;;           B+1
;;            _  B+2
;;        B  | |  _
;;        _  | | | | B+3
;;       | | | | | |  _
;; . . . | | | | | | | | . . .
;;       |---------| Max box length for B is 3, so it's max box area is 6. 
;; We iterate all 'histograms' and use 'histogram-boxes' to store all boxes in front of B.
;; When iterating B+2, 'histogram-boxes' would be {B 3, B+1 1 B+2 1}, where the 'key' is the histogram height and the 'value' is it's length.
;; Upon reaching B+3, every histogram behind it cannot grow it's boxes, so 'histogram-boxes' would be {B+3 1}.
    [histogram]
    (loop [histogram histogram
           histogram-boxes (sorted-map)
           largest-rectangle 0]
      (let [bar (first histogram)]
        (cond (nil? bar)
              (max largest-rectangle (find-largest-rectangle histogram-boxes))
              (> bar (get-highest-key histogram-boxes))
              (recur (rest histogram)
                     (inc-histogram-boxes histogram-boxes bar 1)
                     largest-rectangle)
              :else
              (recur (rest histogram)
                     (inc-histogram-boxes (filter-lower-histogram-boxes histogram-boxes bar) bar (get-box-length histogram-boxes bar))
                     (max largest-rectangle (find-largest-rectangle histogram-boxes)))))))
  (is (= 10 (leet-84 [2 1 5 6 2 3])))
  (is (= 4 (leet-84 [2 4]))))