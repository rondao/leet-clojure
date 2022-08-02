(ns leet-clojure.3 
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn leet-3
    "Given a string s, find the length of the longest substring without repeating characters.

     Example 1:
       Input: s = 'abcabcbb'
       Output: 3
     Explanation: The answer is 'abc', with the length of 3.

     Example 2:
       Input: s = 'bbbbb'
       Output: 1
     Explanation: The answer is 'b', with the length of 1.

     Example 3:
       Input: s = 'pwwkew'
       Output: 3
     Explanation: The answer is 'wke', with the length of 3.
     Notice that the answer must be a substring, 'pwke' is a subsequence and not a substring.

     Constraints:
      0 <= s.length <= 5 * 104
      s consists of English letters, digits, symbols and spaces.
   "
    [input]
    (loop [start 0
           current 0
           chars-position {}
           output 0]
      (if (>= current (count input))
        (inc output)
        (let [char (get input current)]
          (if (contains? chars-position char)
            (recur (inc (chars-position char))
                   (inc current)
                   (assoc chars-position char current)
                   output)
            (recur start
                   (inc current)
                   (assoc chars-position char current)
                   (max output (- current start))))))))
  (is (= 3 (leet-3 "abcabcbb")))
  (is (= 1 (leet-3 "bbbbb")))
  (is (= 3 (leet-3 "pwwkew"))))