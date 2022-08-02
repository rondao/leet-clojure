:SIP(ns leet-clojure.198 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn leet-198
    "You are a professional robber planning to rob houses along a street. Each house has a certain amount of money stashed, the only constraint stopping you from robbing each of them is that adjacent houses have security systems connected and it will automatically contact the police if two adjacent houses were broken into on the same night.
   Given an integer array nums representing the amount of money of each house, return the maximum amount of money you can rob tonight without alerting the police.

   Example 1:
     Input: nums = [1,2,3,1]
     Output: 4
   Explanation: Rob house 1 (money = 1) and then rob house 3 (money = 3).
   Total amount you can rob = 1 + 3 = 4.

   Example 2:
     Input: nums = [2,7,9,3,1]
     Output: 12
   Explanation: Rob house 1 (money = 2), rob house 3 (money = 9) and rob house 5 (money = 1).
   Total amount you can rob = 2 + 9 + 1 = 12.

   Constraints:
    1 <= nums.length <= 100
    0 <= nums[i] <= 400
   "
    ;; The strategy is, we can accumulate the value of each other robbed house.
    ;; For example, all the 'As' houses with 'Current' or all the 'Bs' houses with 'Next'.
    ;; [A1] [B1] [A2] [B2] [Current] [Next] ... [Rest of Houses]
    ;; At 'Current' house, if 'Current' + 'A1 + A2 + .. + An' > 'Next' + 'B1 + B2 + .. + Bn'
    ;; then we can rob the first group of houses and apply same algorithm to 'Rest of Houses'.
    ;; If the second group of houses values more, we advance 'Current' and switch 'As' and 'Bs' to change the adjacent houses,
    ;; then we check again which group is valued more.
    ;;
    ;; Param 'houses' is the current houses to evaluate.
    ;; 'this-total' would be the sum of 'A1 + .. + An'.
    ;; 'next-total' would be the sum of 'B1 + .. + Bn'.
    ;; 'result' is the maximum value we can rob.
    [houses]
    (loop [houses houses
           this-total 0
           next-total 0
           result 0]
      (let [next-houses (rest houses)
            current-house (first houses)
            next-house (first next-houses)]
        (if (nil? current-house)
          result
          (let [total-with-this-house (+ current-house this-total)]
            (if (nil? next-house)
              (+ result total-with-this-house)
              (if (> total-with-this-house (+ next-house next-total))
                (recur (rest next-houses) 0 0 (+ result total-with-this-house))
                (recur next-houses next-total total-with-this-house result))))))))
  (is (= 4 (leet-198 [1 2 3 1])))
  (is (= 12 (leet-198 [2 7 9 3 1]))))