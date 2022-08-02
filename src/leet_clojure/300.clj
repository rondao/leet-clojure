(ns leet-clojure.300 
  (:require [clojure.test :refer [is with-test]]))

(with-test
  (defn longest-subseq
    "Params:
     - 'n' is the number to start the subseq finding, occuring before 'array'.
     - 'array' is the list of numbers following 'n'.
     - 'long-subseq-map' maps the numbers from 'array'
       to the result of 'longest-subseq' for that number."
    [n array long-subseq-map]
    (reduce #(if (> %2 n)
               (max %1 (inc (get long-subseq-map %2)))
               %1) 1 array))
  (is (= 1 (longest-subseq 5 [] [])))
  (is (= 2 (longest-subseq 4 [5] {5 1})))
  (is (= 3 (longest-subseq 3 [4 5] {5 1 4 2})))
  (is (= 2 (longest-subseq 4 [3 4 5] {5 1 4 2 3 3})))
  (is (= 4 (longest-subseq 2 [4 3 4 5] {5 1 4 2 3 3}))))

(with-test
  (defn leet-300
    "Given an integer array nums, return the length of the longest strictly increasing subsequence.
   A subsequence is a sequence that can be derived from an array by deleting some or no elements without changing the order of the remaining elements. For example, [3,6,2,7] is a subsequence of the array [0,3,1,6,2,2,7].

   Example 1:
    Input: nums = [10,9,2,5,3,7,101,18]
    Output: 4
   Explanation: The longest increasing subsequence is [2,3,7,101], therefore the length is 4.

   Example 2:
    Input: nums = [0,1,0,3,2,3]
    Output: 4

   Example 3:
    Input: nums = [7,7,7,7,7,7,7]
    Output: 1

   Constraints:
    1 <= nums.length <= 2500
    -104 <= nums[i] <= 104

   Follow up: Can you come up with an algorithm that runs in O(n log(n)) time complexity?
   "
    ;; We iterate 'array' in reverse order finding the longest strictly increasing
    ;; subsequence for every number, searching for the highest subsequence.
    [array]
    (loop [i (dec (count array))
           longest 0
           long-subseq-map {}]
      (if (neg? i)
        longest
        (let [elem (array i)
              elem-seq (longest-subseq elem
                                       (subvec array (inc i))
                                       long-subseq-map)]
          (recur (dec i)
                 (max longest elem-seq)
                 (assoc long-subseq-map elem elem-seq))))))
  (is (= 4 (leet-300 [10 9 2 5 3 7 101 18])))
  (is (= 4 (leet-300 [0 1 0 3 2 3])))
  (is (= 1 (leet-300 [7 7 7 7 7 7 7]))))
