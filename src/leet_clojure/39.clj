(ns leet-clojure.39 
  (:require [clojure.test :refer [with-test is]]))

(defn find-combination-for-target
  "Returns a function that receive the 'candidates' to find combinations that sum up to 'target'.

   Param 'combinations' is a hashmap, where the 'value' are combination lists that sum up to 'key'.
   This hashmap should already have keys from 1 to ('target' - 1).

   Example for 'target' being 7 and we receive a 'candidate' 2.
   If 'combinations' have a combination to sum up to 5 '(- target candidate)', then
   a combination to sum up to 7 are the combinations for 5 and adding the 'candidate' 2.
   "
  [target combinations]
  (fn [output candidate]
    (let [known-target (- target candidate)]
      (cond (= known-target 0)
            (reduced (conj output (list target)))
            (pos? known-target)
            (let [known-combinations (get combinations known-target)]
              (if (empty? known-combinations)
                output
                (apply conj output (reduce #(conj %1 (sort (conj %2 candidate))) #{} known-combinations))))
            :else
            (reduced output)))))

(with-test
  (defn find-unique-combinations-for-target
    "Returns a set with lists that sum up to 'target'.

     Param 'combinations' is a hashmap, where the 'value' are combination lists that sum up to 'key'.
     This hashmap should already have keys from 1 to ('target' - 1).

     Param 'candidates' is a list with possible values to be used in the combinations.
    "
    [candidates target combinations]
    (reduce (find-combination-for-target target combinations)
            #{}
            candidates))
  (is (= #{}                  (find-unique-combinations-for-target [2 3 6 7] 1 {})))
  (is (= #{[2]}               (find-unique-combinations-for-target [2 3 6 7] 2 {})))
  (is (= #{[3]}               (find-unique-combinations-for-target [2 3 6 7] 3 {2 #{[2]}})))
  (is (= #{[2 2]}             (find-unique-combinations-for-target [2 3 6 7] 4 {2 #{[2]} 3 #{[3]}})))
  (is (= #{[2 3]}             (find-unique-combinations-for-target [2 3 6 7] 5 {2 #{[2]} 3 #{[3]} 4 #{[2 2]}})))
  (is (= #{[2 2 2] [3 3] [6]} (find-unique-combinations-for-target [2 3 6 7] 6 {2 #{[2]} 3 #{[3]} 4 #{[2 2]} 5 #{[2 3]}})))
  (is (= #{[2 2 3] [7]}       (find-unique-combinations-for-target [2 3 6 7] 7 {2 #{[2]} 3 #{[3]} 4 #{[2 2]} 5 #{[2 3]} 6 #{[2 2 2] [3 3] [6]}}))))

(with-test
  (defn leet-39
    "Given an array of distinct integers candidates and a target integer target, return a list of all unique combinations of candidates where the chosen numbers sum to target. You may return the combinations in any order.
   The same number may be chosen from candidates an unlimited number of times. Two combinations are unique if the frequency of at least one of the chosen numbers is different.
   It is guaranteed that the number of unique combinations that sum up to target is less than 150 combinations for the given input.

   Example 1:
    Input: candidates = [2,3,6,7], target = 7
    Output: [[2,2,3],[7]]
   Explanation:
    2 and 3 are candidates, and 2 + 2 + 3 = 7. Note that 2 can be used multiple times.
    7 is a candidate, and 7 = 7.
   These are the only two combinations.

   Example 2:
    Input: candidates = [2,3,5], target = 8
    Output: [[2,2,2,2],[2,3,3],[3,5]]

   Example 3:
    Input: candidates = [2], target = 1
    Output: []
 
   Constraints:
    1 <= candidates.length <= 30
    1 <= candidates[i] <= 200
    All elements of candidates are distinct.
    1 <= target <= 500
    "
    [candidates target]
    (let [candidates (sort candidates)]
      (loop [current 1
             combinations {}]
        (if (> current target)
          (or (get combinations target) #{})
          (recur (inc current)
                 (let [current-combinations (find-unique-combinations-for-target candidates
                                                                                 current
                                                                                 combinations)]
                   (if (empty? current-combinations)
                     combinations
                     (assoc combinations
                            current
                            current-combinations))))))))
  (is (= #{'(2 2 3) '(7)} (leet-39 [2 3 6 7] 7)))
  (is (= #{'(2 2 2 2) '(2 3 3) '(3 5)} (leet-39 [2 3 5] 8)))
  (is (= #{} (leet-39 [2] 1))))
