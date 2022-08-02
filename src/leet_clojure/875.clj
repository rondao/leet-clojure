(ns leet-clojure.875
  (:import [java.util Comparator PriorityQueue])
  (:require [clojure.test :refer [with-test is]]))

(defn pq-add!
  "Add 'elem' to 'pq' and return 'pq'."
  [pq elem]
  (.add pq elem)
  pq)

(with-test
  (defn leet-875
    "Koko loves to eat bananas. There are n piles of bananas, the ith pile has piles[i] bananas. The guards have gone and will come back in h hours.
     Koko can decide her bananas-per-hour eating speed of k. Each hour, she chooses some pile of bananas and eats k bananas from that pile. If the pile has less than k bananas, she eats all of them instead and will not eat any more bananas during this hour.
     Koko likes to eat slowly but still wants to finish eating all the bananas before the guards return.
     Return the minimum integer k such that she can eat all the bananas within h hours.

     Example 1:
       Input: piles = [3,6,7,11], h = 8
       Output: 4

     Example 2:
       Input: piles = [30,11,23,4,20], h = 5
       Output: 30

     Example 3:
       Input: piles = [30,11,23,4,20], h = 6
       Output: 23

     Constraints:
       1 <= piles.length <= 104
       piles.length <= h <= 109
       1 <= piles[i] <= 109
     "
    ;; If we only have one hour for each pile, then our bananas-per-hour (bph)
    ;; is the same as the biggest pile. If we have extra hours, we can use each 
    ;; of them to lower the biggest pile by half.
    [piles hours]
    (loop [extra-hours (- hours (count piles))
           pq (reduce pq-add!
                      (PriorityQueue. (Comparator/reverseOrder))
                      piles)]
      (if (zero? extra-hours)
        (.peek pq)
        (let [biggest-pile (.poll pq)
              half-biggest-pile (long (Math/ceil (/ biggest-pile 2)))]
          (recur (dec extra-hours)
                 (pq-add! pq half-biggest-pile))))))
    (is (= 4 (leet-875 [3 6 7 11] 8)))
    (is (= 30 (leet-875 [30 11 23 4 20] 5)))
    (is (= 23 (leet-875 [30 11 23 4 20] 6))))