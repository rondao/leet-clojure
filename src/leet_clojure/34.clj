(ns leet-clojure.34 
  (:require [clojure.test :refer [with-test is]]))

(defn find-target [asc-numbers target]
  (java.util.Collections/binarySearch asc-numbers target))

(with-test
  (defn find-most-left [asc-numbers target result]
    (let [target-index (find-target asc-numbers target)]
      (if (neg? target-index)
        result
        (recur (subvec asc-numbers 0 target-index) target target-index))))
  (is (= -1 (find-most-left [0 1 2 3 3 3 3 3 8 9] 100 -1)))
  (is (= 3 (find-most-left [0 1 2 3 3 3 3 3 8 9] 3 -1))))

(with-test
  (defn find-most-right [asc-numbers target result]
    (let [target-index (find-target asc-numbers target)]
      (if (neg? target-index)
        result
        (recur (subvec asc-numbers (+ target-index 1)) target (+ target-index result 1)))))
  (is (= -1 (find-most-right [0 1 2 3 3 3 3 3 8 9] 100 -1)))
  (is (= 7 (find-most-right [0 1 2 3 3 3 3 3 8 9] 3 -1))))

(with-test
  (defn leet-34
    "Given an array of integers nums sorted in non-decreasing order,
   find the starting and ending position of a given target value.
   If target is not found in the array, return [-1, -1].
   You must write an algorithm with O(log n) runtime complexity.

   Example 1:
     Input: nums = [5,7,7,8,8,10], target = 8
     Output: [3,4]

   Example 2:
     Input: nums = [5,7,7,8,8,10], target = 6
     Output: [-1,-1]

   Example 3:
     Input: nums = [], target = 0
     Output: [-1,-1]

   Constraints:
     0 <= nums.length <= 105
     -109 <= nums[i] <= 109
     nums is a non-decreasing array.
     -109 <= target <= 109
   "
    [asc-numbers target]
    [(find-most-left asc-numbers target -1)
     (find-most-right asc-numbers target -1)])
  (is (= [3  4] (leet-34 [5 7 7 8 8 10] 8)))
  (is (= [-1 -1] (leet-34 [5 7 7 8 8 10] 6)))
  (is (= [-1 -1] (leet-34 [] 0))))

(comment
  ;; Solution using 'partition-by', but it's not O(log(n)).
  (defn leet-34-not-logn
    [asc-numbers target]
    (let [partitions (partition-by #(= target %) asc-numbers)]
      (if (< (count partitions) 3)
        [-1 -1]
        [(count (first partitions))
         (- (+ (count (first partitions))
               (count (first (rest partitions))))
            1)])))
  )
