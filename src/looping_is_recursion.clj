(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [rest-seq (rest a-seq)]
    (if (empty? rest-seq)
      (first a-seq)
      (recur rest-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not= (first seq1) (first seq2)) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         the-seq a-seq]
    (cond
     (empty? the-seq) nil
     (pred (first the-seq)) idx
     :else (recur (inc idx) (rest the-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         size 0
         the-seq a-seq]
    (if (empty? the-seq)
      (if (zero? size) acc (/ acc size))
      (recur (+ acc (first the-seq)) (inc size) (rest the-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [a-set #{}
           the-seq a-seq]
      (if (empty? the-seq)
        a-set
        (recur (toggle a-set (first the-seq)) (rest the-seq))))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [n-1-el 0
           n-el 1
           cur-n 1]
      (if (== cur-n n)
        n-el
        (recur n-el (+ n-1-el n-el) (inc cur-n))))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         new-vec []
         the-seq a-seq]
    (let [elem (first the-seq)]
      (if (or (nil? elem) (contains? a-set elem))
        new-vec
        (recur (conj a-set elem) (conj new-vec elem) (rest the-seq))))))

