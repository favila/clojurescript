(ns cljs.murmur3
  "Implementation of clojure.lang.Murmur3 in clojurescript.")

(def imul
  "32-bit signed integer multiply with overflow; alias of js/Math.imul.
  Does not follow unchecked-multiply-int semantics! Only two args accepted; if
  args are missing returns 0."
  ;; using aget because Math.imul does not have an extern declared.
  (if (exists? (aget js/Math "imul"))
    (aget js/Math "imul")
    (fn [a b]
      (let [ah (bit-and (unsigned-bit-shift-right a 16) 0xffff)
            al (bit-and a 0xffff)
            bh (bit-and (unsigned-bit-shift-right b 16) 0xffff)
            bl (bit-and b 0xffff)]
        (bit-or (+ (* al bl) (bit-shift-left (+ (* ah bl) (* al bh)) 16)) 0)))))

(def ^:private ^:const seed 0)
(def ^:private ^:const c1 (bit-or 0xcc9e2d51 0))
(def ^:private ^:const c2 (bit-or 0x1b873593 0))

(defn mix-k1 [k1]
  (let [k1 (imul k1 c1)
        ;; k1 = rotateLeft(k1, 15)
        k1 (bit-or (bit-shift-left k1 15) (unsigned-bit-shift-right k1 17))]
    (imul k1 c2)))

(defn mix-h1 [h1 k1]
  (let [h1 (bit-xor h1 k1)
        ;; h1 = rotateLeft(h1, 13)
        h1 (bit-or (bit-shift-left h1 13) (unsigned-bit-shift-right h1 19))]
    (bit-or (+ (imul h1 5) (bit-or 0xe6546b64 0)) 0)))

(defn fmix [h1 len]
  (as-> (bit-xor h1 len) h1
        (bit-xor h1 (unsigned-bit-shift-right h1 16))
        (imul h1 (bit-or 0x85ebca6b 0))
        (bit-xor h1 (unsigned-bit-shift-right h1 13))
        (imul h1 (bit-or 0xc2b2ae35 0))
        (bit-xor h1 (unsigned-bit-shift-right h1 16))))

(defn mix-coll-hash [hash count]
  (let [k1 (mix-k1 hash)
        h1 (mix-h1 seed k1)]
    (fmix h1 count)))

(defn hash-int [input]
  ;; Unused by clojure
  (if (zero? input)
    0
    (let [k1 (mix-k1 input)
          h1 (mix-h1 seed k1)]
      (fmix h1 4))))

(defn long-high-bits
  "Return the high 21 bits of a 53-bit js integer as a signed 32-bit integer.
  `(js/Number.isInteger i)` must be true for the results to be meaningful."
  [i]
  (bit-or (/ (- i (unsigned-bit-shift-right i 0)) 0x100000000) 0))

(defn hash-long [input]
  (if (zero? input)
    0
    (let [low (bit-or input 0)
          high (long-high-bits input)
          k1 (mix-k1 low)
          h1 (mix-h1 seed k1)
          k1 (mix-k1 high)
          h1 (mix-h1 h1 k1)]
      (fmix h1 8))))

(defn hash-string [input]
  (let [l (alength input)]
    (loop [i 1 h1 seed]
      (if (< i l)
        (let [k1 (mix-k1 (bit-or (.charCodeAt input (dec i))
                                 (bit-shift-left (.charCodeAt input i) 16)))]
          (recur (+ i 2) (mix-h1 h1 k1)))
        (let [h1 (if (== i l)
                   (->> (.charCodeAt input (dec i))
                        (mix-k1)
                        (bit-xor h1))
                   h1)]
          (fmix h1 (bit-or (+ l l) 0)))))))
