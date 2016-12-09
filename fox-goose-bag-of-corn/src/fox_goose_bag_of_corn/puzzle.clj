(ns fox-goose-bag-of-corn.puzzle
  "Solve the fox, goose, bag of corn river-crossing problem using core.logic."
  (:require [clojure.core.logic :as l]))

(def pieces
  "Things which can move around."
  [:you :goose :fox :corn])

(def locations
  "Locations something can be."
  [:near :river :far])

(defn keys-at
  "Return a vector of keys at the given position."
  [entry pos]
  (mapv first (filter (fn [[_ v]] (= pos v)) (vec entry))))

(defn transform-one
  "Prepare output formatting for one part of a solution."
  [entry]
  [(set (keys-at entry :near))
   (conj (set (keys-at entry :river)) :boat)
   (set (keys-at entry :far))])

(defn transform-all
  "Prepare output formatting for each part of a solution."
  [rs]
  (mapv transform-one rs))

;; Constraint for the boat being used correctly.
(l/defnc boatc
  [s]
  (let [bs (filter #{:river} (vals s))]
    (or (empty? bs)
        (and (<= (count bs) 2) (= (:you s) :river)))))

;; Constraint for everything being safe.
(l/defnc safetyc
  [s]
  (or (= (:goose s) (:you s))
      (not (or (= (:corn s) (:goose s))
               (= (:fox s) (:goose s))))))

(defn cross-to-same-bank?
  "Predicate returning true if you and the thing you were carrying (if any)
  went to the same bank."
  [p n]
  (let [rs (map first (filter #(= (second %) :river) p))]
    (<= (count (distinct (map #(get n %) rs))) 1)))

(defn can-carry?
  "Predicate returning true if, in the previous state, you were standing on the bank with the thing you are now on the boat with."
  [p n]
  (let [otp (into {} (filter #(not= (second %) (:you p)) p))]
    (= (into {} (filter #(get otp (first %)) n)) otp)))

(defn step-only?
  "Predicating showing that a move has changed the state only the allowed
  amount from the prior state."
  [p n]
  (let [s {:near 0 :river 1 :far 2}]
    (not-any? #(> (Math/abs ^Integer (- (get s (get p %))
                                        (get s (get n %))))
                  1)
              pieces)))

;; Constrains the new state to be a valid move given the prior state.
(l/defnc valid-movec
  [p n]
  (and (step-only? p n)
       (cross-to-same-bank? p n)
       (can-carry? p n)))

(defn inito
  "Initialise possible location states for the things in `vs'."
  [vs]
  (if (seq vs)
    (l/all (l/membero (first vs) locations)
           (inito (next vs)))
    l/succeed))

(defn valid-states
  "Return all possible valid states."
  []
  (l/run* [q]
    (l/fresh [h c f g]
      (inito [h c f g])
      (boatc q)
      (safetyc q)
      (l/== q {:you h :corn c :fox f :goose g}))))

(defn chaino
  "Create fresh variables and solve for the head until the tail meets the
  given end state, recursively."
  [ss end p c]
  (l/fresh [h t]
    (l/membero h (into '() ss))
    (valid-movec p h)
    (l/conde
     [(l/== end h) (l/conso end nil c)]
     [(l/project [h]
                 (chaino (disj ss h) end h t)
                 (l/conso h t c))])))

(defn river-crossing-plan
  "Find the first solution to the river crossing problem."
  []
  (let [terminate-at (fn [k] (into {} (map #(hash-map % k) pieces)))]
    (-> (l/run 1 [q]
          (l/fresh [h t]
            (l/== h (terminate-at :near))
            (l/project [h]
                       (chaino (disj (set (valid-states)) h)
                               (terminate-at :far) h t)
                       (l/conso h t q))))
        first
        transform-all)))

;; (pprint (river-crossing-plan))
;; => [[#{:you :fox :goose :corn} #{:boat} #{}]
;;     [#{:fox :corn} #{:you :boat :goose} #{}]
;;     [#{:fox :corn} #{:boat} #{:you :goose}]
;;     [#{:fox :corn} #{:you :boat} #{:goose}]
;;     [#{:you :fox :corn} #{:boat} #{:goose}]
;;     [#{:corn} #{:you :fox :boat} #{:goose}]
;;     [#{:corn} #{:boat} #{:you :fox :goose}]
;;     [#{:corn} #{:you :boat :goose} #{:fox}]
;;     [#{:you :goose :corn} #{:boat} #{:fox}]
;;     [#{:goose} #{:you :boat :corn} #{:fox}]
;;     [#{:goose} #{:boat} #{:you :fox :corn}]
;;     [#{:goose} #{:you :boat} #{:fox :corn}]
;;     [#{:you :goose} #{:boat} #{:fox :corn}]
;;     [#{} #{:you :boat :goose} #{:fox :corn}]
;;     [#{} #{:boat} #{:you :fox :goose :corn}]]
