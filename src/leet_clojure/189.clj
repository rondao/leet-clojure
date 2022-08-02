(ns leet-clojure.189 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn swap
    "Swap position 'i1' with position 'i2' within 'array'."
    [array i1 i2]
    (assoc array
           i1 (array i2)
           i2 (array i1)))
  (is (= [2 1] (swap [1 2] 0 1)))
  (is (= [1 4 3 2] (swap [1 2 3 4] 1 3))))

(with-test
  (defn swap-cycle
    "Swap 'init-pos' with 'init-pos' + 'k,'
     and then 'init-pos' + 'k' with 'init-pos' + '2 * k', and so on.
     Repeat it until a cycle is found, then return 'array' and the number of swaps."
    [array k init-pos]
    (loop [cur-pos (mod (+ init-pos k) (count array))
           array (swap array init-pos cur-pos)
           num-swaps 1]
      (if (= cur-pos init-pos)
        {:array array :num-swaps num-swaps}
        (let [next-pos (mod (+ cur-pos k) (count array))]
          (recur next-pos
                 (swap array init-pos next-pos)
                 (inc num-swaps))))))
  (is (= {:array [2 1], :num-swaps 2} (swap-cycle [1 2] 1 0)))
  (is (= {:array [3 2 1 4], :num-swaps 2} (swap-cycle [1 2 3 4] 2 0)))
  (is (= {:array [5 6 1 2 3 4], :num-swaps 3} (swap-cycle [5 2 1 4 3 6] 2 1))))
  

(with-test
  (defn leet-189
    "Given an array, rotate the array to the right by k steps, where k is non-negative.

   Example 1:
    Input: nums = [1,2,3,4,5,6,7], k = 3
    Output: [5,6,7,1,2,3,4]
   Explanation:
    rotate 1 steps to the right: [7,1,2,3,4,5,6]
    rotate 2 steps to the right: [6,7,1,2,3,4,5]
    rotate 3 steps to the right: [5,6,7,1,2,3,4]

   Example 2:
    Input: nums = [-1,-100,3,99], k = 2
    Output: [3,99,-1,-100]
   Explanation: 
    rotate 1 steps to the right: [99,-1,-100,3]
    rotate 2 steps to the right: [3,99,-1,-100]

   Constraints:
    1 <= nums.length <= 105
    -231 <= nums[i] <= 231 - 1
    0 <= k <= 105

   Follow up:
    Try to come up with as many solutions as you can. There are at least three different ways to solve this problem.
    Could you do it in-place with O(1) extra space?
   "
    ;; We try using 'swap-cycle' to swap as many positions as possible.
    ;; If it returns 'num-swaps' equal to the array size, then we swap all positions.
    ;; Otherwise, we found a cycle early, so we increment current position by one and repeat the process.
    [array k]
    (loop [init-pos 0
           {array :array num-swaps :num-swaps} (swap-cycle array k init-pos)]
      (if (= num-swaps (count array))
        array
        (let [next-pos (inc init-pos)]
          (recur next-pos
                 (update (swap-cycle array k next-pos)
                         :num-swaps + num-swaps))))))
  (is (= [5 6 7 1 2 3 4] (leet-189 [1 2 3 4 5 6 7] 3)))
  (is (= [3 99 -1 -100] (leet-189 [-1 -100 3 99] 2))))

(defn solution-with-extra-space
  [array k]
  (vec (concat (subvec array (inc k))
               (subvec array 0 (inc k)))))

