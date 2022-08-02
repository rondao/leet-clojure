(ns leet-clojure.739 
  (:require [clojure.test :refer [is with-test]]))

(with-test
  (defn how-many-days-until-warmer
    "From a list of 'temperature' integers, return how many days (indices)
     does it take to find a higher temperature than the first one on the list."
    [temperatures]
    (let [temp (first temperatures)
          temperatures (rest temperatures)]
      (:days
       (reduce
        #(if (> %2 temp)
           (reduced {:days (inc %1)})
           (inc %1))
        0 temperatures)
       0)))
  (is (= 1 (how-many-days-until-warmer [1 2])))
  (is (= 2 (how-many-days-until-warmer [2 1 3])))
  (is (= 0 (how-many-days-until-warmer [3 2]))))

(with-test
  (defn leet-739
    "Given an array of integers temperatures represents the daily temperatures,
   return an array answer such that answer[i] is the number of days you have to
   wait after the ith day to get a warmer temperature. If there is no future day
   for which this is possible, keep answer[i] == 0 instead.

   Example 1:
     Input: temperatures = [73,74,75,71,69,72,76,73]
     Output: [1,1,4,2,1,1,0,0]
   
   Example 2:
     Input: temperatures = [30,40,50,60]
     Output: [1,1,1,0]

   Example 3:
     Input: temperatures = [30,60,90]
     Output: [1,1,0]

   Constraints:
     1 <= temperatures.length <= 105
     30 <= temperatures[i] <= 100
   "
    [temperatures]
    (loop [temperatures temperatures
           result []]
      (if (empty? temperatures)
        result
        (recur
         (rest temperatures)
         (conj result (how-many-days-until-warmer temperatures))))))
  (is (= [1 1 4 2 1 1 0 0] (leet-739 [73 74 75 71 69 72 76 73])))
  (is (= [1 1 1 0] (leet-739 [30 40 50 60])))
  (is (= [1 1 0] (leet-739 [30 60 90]))))