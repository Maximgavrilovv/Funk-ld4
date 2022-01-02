(ns ld3)

(require '[clojure.string :as str])

(def message "WE ARE DISCOVERED. RUN AT ONCE")

(defn encrypt [message]
  (let [message (str/replace message #"\W" "")
        rails
        (for [[start skip] [[0 4] [1 2] [2 4]]]
          (apply str (take-nth skip (drop start message))))]
    (apply str rails)))
(encrypt message)

;;managed to do only do with certain rails count == 3, so as parameter takes only encrypted text
(defn decrypt [ciphertext]
  (let [rails-count 3
        k (-> (count ciphertext)
              (/ (* 2 (dec rails-count))))
        splitter (fn [s]
                   [(take k s)
                    (take (* 2 k) (drop k s))
                    (take k (drop (* 3 k) s))])
        [top middle bottom] (splitter ciphertext)]
    (loop [top top
           middle middle
           bottom bottom
           result (transient [])]
      (if (seq middle)
        (let [a (first top)
              b (first middle)
              c (first bottom)
              d (second middle)]
          (recur (rest top) (drop 2 middle) (rest bottom) (reduce conj! result [a b c d])))
        (apply str (persistent! result))))))

(decrypt (encrypt message))