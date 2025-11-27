(ns spot.ratio
  "Very simple vector-based ratio impl, usable across Clojure and ClojureScript.")

(defn ratio [n]
  (if (vector? n) n [n 1]))

(defn radd [a b]
  (let [[an ad] (ratio a)
        [bn bd] (ratio b)]
    [(+ (* an bd) (* bn ad)) (* ad bd)]))

(defn rsub [a b]
  (let [[an ad] (ratio a)
        [bn bd] (ratio b)]
    [(- (* an bd) (* bn ad)) (* ad bd)]))

(defn rdiv [a b]
  (let [[an ad] (ratio a)
        [bn bd] (ratio b)]
    [(* an bd) (* ad bn)]))

(defn rmult [a b]
  (let [[an ad] (ratio a)
        [bn bd] (ratio b)]
    [(* an bn) (* ad bd)]))

(defn rquot [[n d]]
  (quot n d))

(defn rrem [[n d]]
  [(mod n d) d])

(defn rcomp [a b]
  (let [[n _] (rsub a b)]
    (cond
      (neg? n) -1
      (pos? n) 1
      :else 0)))
