(ns where-to-go.data
  (:require [codax.core :as c]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

(def db (c/open-database! "data/database"))

;; THIS WAS USED TO POPULATE PLACES IN DB
;; Function to transform a single feature into schema
(defn feature->place [feature]
  (let [props  (:properties feature)
        coords (get-in feature [:geometry :coordinates])
        wheelchair-raw (:wheelchair props) 
        wheelchair (case wheelchair-raw
                     "yes" true
                     "no" false
                     nil)]  ;; leave nil if not specified
    {:place_id       (or (:id feature) (get props "@id"))
     :amenity_type   (:amenity props)
     :name           (:name props)
     :lat_coordinate (second coords)
     :long_coordinate (first coords)
     :wheelchair     wheelchair
     :reviews        []
     :avg_rating     0.0}))

;; Helper to compute average rating of a place
(defn avg-rating [reviews]
  (if (empty? reviews)
    0
    (/ (reduce + (map :rating reviews))
       (count reviews))))

;; Function to load a JSON file and extract places
(defn load-json-file [file-path]
  (with-open [r (io/reader file-path)]
    (let [data (json/parse-stream r true)] ;; keywordize keys
      (map feature->place (:features data)))))

;; Load all JSON files (bar.json, cafe.json, etc.)
(def files ["data/bar.json"
            "data/cafe.json"
            "data/restaurant.json"
            "data/casino.json"
            "data/cinema.json"
            "data/library.json"
            "data/nightclub.json"
            "data/pub.json"
            "data/theatre.json"])

(defn load-all-places []
  (mapcat load-json-file files))

;; Save into Codax DB under key :places
(defn save-places! []
  (let [places (vec (load-all-places))]
    (c/assoc-at! db [:places] places)))

;; Get all places from db
(defn get-all-places []
  (c/get-at! db [:places]))

(defn add-review-to-place! [place-id review]
  "Adds a review to a place in the DB and updates avg_rating with debug printing."
  ;; Load all places from DB
  (let [places (vec (get-all-places))]  ;; ensure vector
    (let [updated-places
          (mapv (fn [p]
                  (if (= (:place_id p) place-id)
                    (let [updated-reviews (conj (or (:reviews p) []) review)
                          avg (avg-rating updated-reviews)
                          updated-place (assoc p
                                               :reviews updated-reviews
                                               :avg_rating (/ (Math/round (* avg 10.0)) 10.0))]
                      updated-place)
                    p))
                places)]

      ;; Persist back to the same DB path
      (c/assoc-at! db [:places] updated-places)
      ;; Return updated places
      updated-places)))

(defn create-user
  "Add a new user. Throws exception if username exists."
  [username street-name street-number city country postal-code wheelchair lat lon]
  (c/with-write-transaction [db tx]
    (when (c/get-at tx [:users username])
      (throw (Exception. "Username already exists")))
    (let [user {:username username
                :street-name street-name
                :street-number street-number
                :city city
                :country country
                :postal-code postal-code
                :wheelchair wheelchair
                :lat_coordinate lat
                :long_coordinate lon}]
      (c/assoc-at tx [:users username] user))))

(defn read-user
  "Fetch a user by username. Returns nil if not found."
  [username]
  (c/with-read-transaction [db tx]
    (c/get-at tx [:users username])))

(defn update-user
  "Update an existing user. Fields can be provided as a map."
  [username updates]
  (c/with-write-transaction [db tx]
    (let [existing (c/get-at tx [:users username])]
      (when-not existing
        (throw (Exception. "User not found")))
      (c/merge-at tx [:users username] updates))))

(defn delete-user
  "Remove a user by username."
  [username]
  (c/with-write-transaction [db tx]
    (when-let [existing (c/get-at tx [:users username])]
      (c/dissoc-at tx [:users username]))))