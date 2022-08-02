(ns leet-clojure.2156
  (:require [clojure.test :refer [with-test is]]))

(with-test
  (defn alpha-val
    "Returns value 1 for 'a', 2 for 'b', until 26 for 'z'."
    [char]
    (- (int char) 96))
  (is (= 1 (alpha-val \a)))
  (is (= 2 (alpha-val \b)))
  (is (= 26 (alpha-val \z))))

(with-test
  (defn hash-from-values
    "Return a list with the hash value of each character position."
    [string power]
    (map #(* %1 (int (Math/pow power %2)))
         (map alpha-val string)
         (range)))
  (is (= '(1 2 4 8 16) (hash-from-values "aaaaa" 2)))
  (is (= '(1 2 3 4 5) (hash-from-values "abcde" 1)))
  (is (= '(1 4 12 32 80) (hash-from-values "abcde" 2))))

(defn roll-hash
  "Roll the 'hash' value by shifting out 'old-char-hash' and
   shifting in 'new-char' into the hash using 'power' and 'window-len' value."
  [hash old-char-hash new-char window-len power]
  (let [new-hash (- hash old-char-hash)
        new-hash (/ new-hash power)]
    (+ new-hash
       (* (alpha-val new-char)
          (int (Math/pow power (dec window-len)))))))

(with-test
  (defn leet-2156
    "The hash of a 0-indexed string s of length k, given integers p and m, is computed using the following function:
   hash(s, p, m) = (val(s[0]) * p0 + val(s[1]) * p1 + ... + val(s[k-1]) * pk-1) mod m.
   Where val(s[i]) represents the index of s[i] in the alphabet from val('a') = 1 to val('z') = 26.
   You are given a string s and the integers power, modulo, k, and hashValue. Return sub, the first substring of s of length k such that hash(sub, power, modulo) == hashValue.
   The test cases will be generated such that an answer always exists.
   A substring is a contiguous non-empty sequence of characters within a string.

   Example 1:
     Input: s = 'leetcode', power = 7, modulo = 20, k = 2, hashValue = 0
     Output: 'ee'
   Explanation: The hash of 'ee' can be computed to be hash('ee', 7, 20) = (5 * 1 + 5 * 7) mod 20 = 40 mod 20 = 0.
   'ee' is the first substring of length 2 with hashValue 0. Hence, we return 'ee'.

   Example 2:
     Input: s = 'fbxzaad', power = 31, modulo = 100, k = 3, hashValue = 32
     Output: 'fbx'
   Explanation: The hash of 'fbx' can be computed to be hash('fbx', 31, 100) = (6 * 1 + 2 * 31 + 24 * 312) mod 100 = 23132 mod 100 = 32.
   The hash of 'bxz' can be computed to be hash('bxz', 31, 100) = (2 * 1 + 24 * 31 + 26 * 312) mod 100 = 25732 mod 100 = 32.
   'fbx' is the first substring of length 3 with hashValue 32. Hence, we return 'fbx'.
   Note that 'bxz' also has a hash of 32 but it appears later than 'fbx'.

   Constraints:
    1 <= k <= s.length <= 2 * 104
    1 <= power, modulo <= 109
    0 <= hashValue < modulo
    s consists of lowercase English letters only.
    The test cases are generated such that an answer always exists."
    [s power modulo k hash-value]
    (loop [window (vec (take k s))
           window-hash (hash-from-values window power)
           hash-sum (apply + window-hash)
           s (vec (drop k s))]
      (if (= hash-value (rem hash-sum modulo))
        (apply str window)
        (recur (conj (subvec window 1) (first s))
               (conj (rest window-hash) (alpha-val (first s)))
               (roll-hash hash-sum (first window-hash) (first s) k power)
               (rest s)))))
  (is (= "ee" (leet-2156 "leetcode" 7 20 2 0)))
  (is (= "fbx" (leet-2156 "fbxzaad" 31 100 3 32))))

(with-test
  (defn -hash
    "hash(s, p, m) = (val(s[0]) * p0 + val(s[1]) * p1 + ... + val(s[k-1]) * pk-1) mod m"
    [s p m]
    (let [power-values (map #(* (alpha-val %1) (int (Math/pow p %2)))
                            s (range))]
      (rem (apply + power-values) m)))
  (is (= 0 (-hash "ee" 7 20)))
  (is (= 32 (-hash "fbx" 31 100))))

(with-test
  (defn solution-without-rolling-hash
    [s power modulo k hashValue]
    (let [s-len (+ (count s) 1)
          s-subs (map #(subs s %1 %2)
                      (range (- s-len k)) (range k s-len))]
      (loop [s-sub (first s-subs)
             s-subs (rest s-subs)]
        (if (= hashValue (-hash s-sub power modulo))
          s-sub
          (recur (first s-subs) (rest s-subs))))))
  (is (= "ee" (leet-2156 "leetcode" 7 20 2 0)))
  (is (= "fbx" (leet-2156 "fbxzaad" 31 100 3 32))))