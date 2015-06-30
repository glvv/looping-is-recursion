(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp) 1 (let [c (fn [acc n] (if (zero? n) acc (recur (* acc base) (dec n))))]
    (c 1 exp))))

(defn last-element [a-seq]
  (if (empty? a-seq) nil
    (let [get-last (fn [last-elem b-seq] (if (empty? b-seq) last-elem (recur (first b-seq) (rest b-seq))))]
      (get-last nil a-seq))))

(defn seq= [seq1 seq2]
  (let [compare-seq
        (fn [sq1 sq2]
          (cond
           (and (empty? sq1) (empty? sq2)) true
           (or (empty? sq1) (empty? sq2)) false
           (not (= (first sq1) (first sq2))) false
           :else (recur (rest sq1) (rest sq2))))]
  (compare-seq seq1 seq2)))

(defn find-first-index [pred a-seq]
    (loop [index 0
         sq a-seq
           p pred]
    (cond
     (empty? a-seq) nil
     (nil? (first sq)) nil
     (p (first sq)) index
     :else (recur (inc index) (rest sq) p))))

(defn avg [a-seq]
  (loop [elements 0
         sum 0
         sq a-seq]
    (if (empty? sq) (/ sum elements)
      (recur (inc elements) (+ (first sq) sum) (rest sq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [sq a-seq
         s #{}]
    (if (empty? sq) s
      (recur (rest sq) (toggle s (first sq))))))

(defn fast-fibo [n]
  (if (zero? n) 0
    (if (< n 3) 1
      (loop [fn1 1
           fn2 1
           result 2
           i (- n 3)]
      (if (zero? i) result
        (recur result fn1 (+ result fn1) (dec i)))))))

(defn cut-at-repetition [a-seq]
  (reverse (loop [elem-map #{}
         result '()
         sq a-seq]
    (if (empty? sq) (if (= (count elem-map) (count result)) result (rest result))
      (cond
       (not (= (count elem-map) (count result))) (rest result)
       :else (recur (conj elem-map (first sq)) (conj result (first sq)) (rest sq)))))))

