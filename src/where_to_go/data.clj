(ns where-to-go.data
  (:require [codax.core :as c]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

(def db (c/open-database! "data/database"))

;; THIS WAS USED TO POPULATE PLACES IN DB
;; ;; Function to transform a single feature into your schema
;; (defn feature->place [feature]
;;   (let [props  (:properties feature)
;;         coords (get-in feature [:geometry :coordinates])]
;;     {:place_id         (or (:id feature) (get props "@id"))
;;      :amenity_type     (:amenity props)
;;      :name             (:name props)
;;      :lat_coordinate   (second coords)
;;      :long_coordinate  (first coords)
;;      :wheelchair (:n props)}))

;; ;; Function to load a JSON file and extract places
;; (defn load-json-file [file-path]
;;   (with-open [r (io/reader file-path)]
;;     (let [data (json/parse-stream r true)] ;; keywordize keys
;;       (map feature->place (:features data)))))

;; ;; Load all your JSON files (bar.json, cafe.json, etc.)
;; (def files ["data/bar.json"
;;             "data/cafe.json"
;;             "data/restaurant.json"
;;             "data/casino.json"
;;             "data/cinema.json"
;;             "data/library.json"
;;             "data/nightclub.json"
;;             "data/pub.json"
;;             "data/theatre.json"])

;; (defn load-all-places []
;;   (mapcat load-json-file files))

;; ;; Save into Codax DB under key :places
;; (defn save-places! []
;;   (let [places (vec (load-all-places))]
;;     (c/assoc-at! db [:places] places)))

;; Read the places back
(def loaded-places (c/get-at! db [:places]))

(defn create-user
  "Add a new user. Throws exception if username exists."
  [username street-name street-number city country postal-code wheelchair-status]
  (c/with-write-transaction [db tx]
    (when (c/get-at tx [:users username])
      (throw (Exception. "Username already exists")))
    (let [user {:username username
                :street-name street-name
                :street-number street-number
                :city city
                :country country
                :postal-code postal-code
                :wheelchair-status wheelchair-status}]
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