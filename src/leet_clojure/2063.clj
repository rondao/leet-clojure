(ns leet-clojure.2063
  (:require [clojure.test :refer [with-test is]]
            [clojure.string :as str])) 

(with-test
  (defn create-occurance-array
    "Return an array where each position is how many times a character would occur
     on every substring with length 'size'."
    ;; First character occurs N times, where N is the size of the string.
    ;; There is an increment K that start as N-2, this increment decreases by 2 each next character.
    ;; Each character after the first occurs N + K times.
    ;; After passing the middle position, K start increasing by 2 each next character.
    ;; If string has even size, K doesn't change at the middle position.
    ;;
    ;;   aba
    ;;  ab ba
    ;; a  b  a
    ;;  A = 3 
    ;;  B = 4      
    ;;
    ;;       abba
    ;;    abb    bba 
    ;;  ab    bb    ba 
    ;; a     b  b     a
    ;;  A = 4
    ;;  B = 6
    ;;      
    ;;             abcba
    ;;         abcb     bcba
    ;;      abc     bcb     cba
    ;;    ab     bc      cb    ba
    ;;   a    b      c       b   a 
    ;; A = 5
    ;; B = 8
    ;; C = 9
    ;;
    ;;                     abccba 
    ;;               abccb        bccba 
    ;;          abcc        bccb        ccba
    ;;      abc       bcc          ccb       cba
    ;;   ab      bc          cc          cb      ba
    ;; a     b         c            c         b     a
    ;;  A = 6
    ;;  B = 10
    ;;  C = 12
    ;;
    ;; With this information, we can create an array of occurances.
    ;; Each position is how many times a character occurs in all substrings.
    [size]
    (loop [occurances []
           increment size
           previous-occurance 0]
      (if (= size (count occurances))
        occurances
        (let [current-occurance (+ previous-occurance increment)]
          (recur (conj occurances current-occurance)
                 (- increment 2)
                 current-occurance)))))
  (is (= [3 4 3] (create-occurance-array 3)))
  (is (= [4 6 6 4] (create-occurance-array 4)))
  (is (= [5 8 9 8 5] (create-occurance-array 5)))
  (is (= [6 10 12 12 10 6] (create-occurance-array 6))))

(with-test
  (defn is-vowel
    "Returns 'true' if 'char' is a vowel, 'false' otherwise."
    [char]
    (str/includes? "aeiou" (str char)))
  (is (true?  (is-vowel "a")))
  (is (true?  (is-vowel "e")))
  (is (true?  (is-vowel "i")))
  (is (true?  (is-vowel "o")))
  (is (true?  (is-vowel "u")))
  (is (false? (is-vowel "x"))))

(with-test
  (defn get-value-if-vowel
    "Returns 'value' if 'char' is a vowel, 0 otherwise."
    [char value]
    (if (is-vowel char) value 0))
  (is (= 1 (get-value-if-vowel "a" 1)))
  (is (= 2 (get-value-if-vowel "e" 2)))
  (is (= 3 (get-value-if-vowel "i" 3)))
  (is (= 4 (get-value-if-vowel "o" 4)))
  (is (= 5 (get-value-if-vowel "u" 5)))
  (is (= 0 (get-value-if-vowel "x" 6))))

(with-test
  (defn leet-2063
    "Given a string word, return the sum of the number of vowels ('a', 'e', 'i', 'o', and 'u') in every substring of word.

     A substring is a contiguous (non-empty) sequence of characters within a string.

     Note: Due to the large constraints, the answer may not fit in a signed 32-bit integer. Please be careful during the calculations.

     Example 1:
       Input: word = 'aba'
       Output: 6
     Explanation: 
       All possible substrings are: 'a', 'ab', 'aba', 'b', 'ba', and 'a'.
       - 'b' has 0 vowels in it
       - 'a', 'ab', 'ba', and 'a' have 1 vowel each
       - 'aba' has 2 vowels in it
       Hence, the total sum of vowels = 0 + 1 + 1 + 1 + 1 + 2 = 6. 

     Example 2:
       Input: word = 'abc'
       Output: 3
     Explanation: 
       All possible substrings are: 'a', 'ab', 'abc', 'b', 'bc', and 'c'.
       - 'a', 'ab', and 'abc' have 1 vowel each
       - 'b', 'bc', and 'c' have 0 vowels each
       Hence, the total sum of vowels = 1 + 1 + 1 + 0 + 0 + 0 = 3.

     Example 3:
       Input: word = 'ltcd'
       Output: 0
     Explanation: There are no vowels in any substring of 'ltcd'.

     Constraints:
       1 <= word.length <= 105
       word consists of lowercase English letters.
   "
    [input]
    (apply + (map get-value-if-vowel
                  input
                  (create-occurance-array (count input)))))
  (is (= 6 (leet-2063 "aba")))
  (is (= 3 (leet-2063 "abc")))
  (is (= 0 (leet-2063 "ltcd"))))