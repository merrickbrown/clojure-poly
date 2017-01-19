(ns polynomials.core)

(defn poly
  "Returns a function evaluated using Horner's method"
  [coeffs]
  (with-meta (fn [x]
               (loop [r (rest (reverse coeffs))
                      val (first (reverse coeffs))]
                 (if (empty? r)
                   val
                   (recur
                    (rest r)
                    (+ (first r) (* x val))
                    ))))
    {:coeffs coeffs}))

(defn coeffs
  [p]
  (:coeffs (meta p)))

(defn p+
  "Add two polynomials"
  [p q]
  (poly (map + (coeffs p) (coeffs q))))

(defn p*
  "Multiply two polynomials using Karatsuba"
  [p q]
  (let [pcoeffs (coeffs p)
        qcoeffs (coeffs q)]
    ))

(defn- coeff*
  [c1 c2]
  (if
      (and (= (count c1) 1) (= (count c2) 1))
    (map * c1 c2)
    (let [cdiff (- (count c2) (count c1))
          degree (inc (+ (dec (count c1)) (dec (count c2))))
          add (if (even? (max (count c2) (count c1))) 0 1)
          c1p (concat c1 (repeat (+ cdiff) 0) (repeat add 0))
          c2p (concat c2 (repeat (- cdiff) 0) (repeat add 0))
          half (/ (count c1p) 2)
          p0 (take half c1p)
          p1 (drop half c1p)
          q0 (take half c2p)
          q1 (drop half c2p)
          z0 (coeff* p0 q0)
          z2 (coeff* p1 q1)]
      (take degree (map + (concat z0 (repeat (* 2 half) 0))
                        (concat
                         (repeat half 0)
                         (pmap -
                               (coeff* (map + p0 p1) (map + q0 q1))
                               z2
                               z0)
                         (repeat half 0))
                        (concat
                         (repeat (* 2 half) 0)
                         z2)))
      )))

(defn- brute-coeff*
  [c1 c2]
  (let [c1s (into (vector) c1)
        c2s (into (vector) c2)
        result (atom
                (int-array (dec (+ (count c1) (count c2))) 0))]
    (doseq [i (range (count c1s))
            j (range (count c2s))
            val (aget @result (+ i j))]
      (swap! result aset (+ i j) (+ val (* (c1s i) (c2s j)))))
    @result)
  )
