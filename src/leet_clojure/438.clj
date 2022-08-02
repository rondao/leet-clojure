(ns leet-clojure.438 
  (:require [clojure.test :refer [with-test is]])) 

(with-test
  (defn assoc-or-increment
    "Add 'key' to 'hashmap' with value 1.
     If 'hashmap' already have 'key', increment it's value."
    [hashmap key]
    (let [value (get hashmap key 0)]
      (assoc hashmap key (inc value))))
  (is (= {:test 1} (assoc-or-increment {} :test)))
  (is (= {:test 2} (assoc-or-increment {:test 1} :test))))

(with-test
  (defn dissoc-or-decrement
    "Remove 'key' from 'hashmap' if value is 1.
     Otherwise decrement it's value."
    [hashmap key]
    (let [value (get hashmap key 1)
          new-value (dec value)]
      (if (zero? new-value)
        (dissoc hashmap key)
        (assoc hashmap key new-value))))
  (is (= {} (dissoc-or-decrement {} :test)))
  (is (= {} (dissoc-or-decrement {:test 1} :test)))
  (is (= {:test 1} (dissoc-or-decrement {:test 2} :test))))

(with-test
  (defn map-chars-occurances
    "Returns a map associating the chars in 'string' with how many occurances
     they happen within 'string'."
    [string]
    (reduce assoc-or-increment {} string))
  (is (= {} (map-chars-occurances "")))
  (is (= {\a 1 \b 1 \c 1} (map-chars-occurances "abc")))
  (is (= {\a 2 \b 2 \c 1} (map-chars-occurances "abcba"))))

(with-test
  (defn starts-with-anagram?
    [s chars]
    (if (empty? chars)
      true
      (if (empty? s)
        false
        (let [char (first s)]
          (if (contains? chars char)
            (recur (rest s)
                   (dissoc-or-decrement chars char))
            false)))))
  (is (= false (starts-with-anagram? "" {\a 2 \b 2 \c 1})))
  (is (= false (starts-with-anagram? "abc" {\a 2 \b 2 \c 1})))
  (is (= true (starts-with-anagram? "aabbc" {\a 2 \b 2 \c 1})))
  (is (= true (starts-with-anagram? "bcaabXYZ" {\a 2 \b 2 \c 1})))
  (is (= false (starts-with-anagram? "bcaXabYZ" {\a 2 \b 2 \c 1}))))

(with-test
  (defn leet-438
    "Given two strings s and p, return an array of all the start indices of p's anagrams in s. You may return the answer in any order.

     An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase, typically using all the original letters exactly once.

     Example 1:
       Input: s = 'cbaebabacd', p = 'abc'
       Output: [0,6]
     Explanation:
       The substring with start index = 0 is 'cba', which is an anagram of 'abc'.
       The substring with start index = 6 is 'bac', which is an anagram of 'abc'.

     Example 2:
       Input: s = 'abab', p = 'ab'
       Output: [0,1,2]
     Explanation:
       The substring with start index = 0 is 'ab' , which is an anagram of 'ab'.
       The substring with start index = 1 is 'ba' , which is an anagram of 'ab'.
       The substring with start index = 2 is 'ab' , which is an anagram of 'ab'.
   "
    [s p]
    (let [chars (map-chars-occurances p)
          size-s (count s)]
      (loop [rest-s s
             output []]
        (if (empty? rest-s)
          output
          (if (starts-with-anagram? rest-s chars)
            (recur (rest rest-s) (conj output
                                       (- size-s (count rest-s))))
            (recur (rest rest-s) output))))))
  (is (= [0 6] (leet-438 "cbaebabacd" "abc")))
  (is (= [0 1 2] (leet-438 "abab" "ab"))))