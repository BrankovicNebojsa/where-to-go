(ns where-to-go.core
    (:gen-class)
    (:require [clojure.string :as str]
              [where-to-go.data :as data]))

;; --- Data storage ---
(def users (atom {})) ;; username -> {:location "N/A", :wheelchair false, :history []} 
(def places (atom {})) ;; place-name -> [{:username "user1", :rating 4, :comment "Nice"} ...]
(def current-user (atom nil))

;; --- Helpers ---
(defn get-input [prompt] 
  (println prompt)
  (print "> ") (flush)
      (read-line))

(defn read-int [prompt valid?]
  (loop []
    (let [input (get-input prompt)
          n (try (Integer/parseInt input) (catch Exception _ nil))]
      (if (and n (valid? n))
        n
        (do (println "Invalid input, try again.") (recur))))))

(defn avg-rating [reviews]
  (if (empty? reviews) 0
      (/ (reduce + (map :rating reviews)) (count reviews))))

  
(defn read-boolean [prompt]
  (loop []
    (let [input (str/lower-case (get-input prompt))]
      (cond
        (#{"yes" "y"} input) true
        (#{"no" "n"} input) false
        :else (do (println "Please enter yes or no.") (recur))))))

;; --- User login / creation ---
(defn login []
  (let [username (get-input "Enter your username:")
        user (data/read-user username)]
    (if user
      (do
        (reset! current-user user)
        (println "Welcome back," (:username user)))
      ;; user not found, create new
      (do
        (println "This is your first time using WhereToGo. Please enter your data so we can find the best place for you.")
        (let [street-name (get-input "\nStreet name:")
              street-number (read-int "\nStreet number:" pos?)
              city (get-input "\nCity:")
              country (get-input "\nCountry:")
              postal-code (get-input "\nPostal code:")
              wheelchair-status (read-boolean "\nWheelchair accessible? (yes/no): ")]
          (data/create-user username street-name street-number city country postal-code wheelchair-status)
          (reset! current-user (data/read-user username))
          (println "\nYour user has been successfully created"))))))


;; --- Profile menu ---
(defn edit-profile []
  (println "\nThis is your current profile information:\n")
  (println "Username:" (:username @current-user))
  (println "Street name:" (:street-name @current-user))
  (println "Number:" (:street-number @current-user))
  (println "City:" (:city @current-user))
  (println "Postal code:" (:postal-code @current-user))
  (println "Country:" (:country @current-user))
  (println "Wheelchair status:"
          (case (:wheelchair-status @current-user)
            true "Yes"
            false "No"
            "N/A"))
  ;; Menu
  (println "\nWhat would you like to change?")
  (println "1. Street name")
  (println "2. Number")
  (println "3. City")
  (println "4. Postal code")
  (println "5. Country")
  (println "6. Wheelchair status")
  (println "0. Back")
  (case (read-int "\nSelect an option:" #{0 1 2 3 4 5 6})
    1 (let [new-street-name (get-input "\nEnter new street name:")]
        (data/update-user (:username @current-user) {:street-name new-street-name})
        (swap! current-user assoc :street-name new-street-name)
        (println "Street name changed to:" new-street-name))
    2 (let [new-street-number (get-input "\nEnter new street number:")]
        (data/update-user (:username @current-user) {:street-number new-street-number})
        (swap! current-user assoc :street-number new-street-number)
        (println "Street number changed to:" new-street-number))
    3 (let [new-city (get-input "\nEnter new city:")]
        (data/update-user (:username @current-user) {:city new-city})
        (swap! current-user assoc :city new-city)
        (println "City changed to:" new-city))
    4 (let [new-postal-code (get-input "\nEnter new postal code:")]
        (data/update-user (:username @current-user) {:postal-code new-postal-code})
        (swap! current-user assoc :postal-code new-postal-code)
        (println "Postal code changed to:" new-postal-code))
    5 (let [new-country (get-input "\nEnter new country:")]
        (data/update-user (:username @current-user) {:country new-country})
        (swap! current-user assoc :country new-country)
        (println "Country changed to:" new-country))
    6 (let [new-wheelchair-status (read-boolean "\nIs the user wheelchair accessible? (yes/no)")]
        (data/update-user (:username @current-user) {:wheelchair-status new-wheelchair-status})
        (swap! current-user assoc :wheelchair-status new-wheelchair-status)
        (println "Wheelchair status changed to:" new-wheelchair-status))
    0 nil))


;; --- Leave review ---
(defn leave-review []
(let [place-name (get-input "\nEnter a place name:")
      rating (read-int "Enter rating (1-5):" #(<= 1 % 5))
      comment (get-input "Enter comment:")]
  ;; Update place reviews
  (swap! places update place-name
          (fn [reviews] (conj (or reviews []) {:username @current-user
                                              :rating rating
                                              :comment comment})))
  (println "Review saved! Average rating for this place:"
            (avg-rating (@places place-name)))))

;; --- History ---
(defn show-history []
  (let [history (:history (@users @current-user))]
    (println "Your visited places:")
    (doseq [p history] (println "- " p))))

;; --- Find a place ---
(defn find-place []
  ;; For simplicity, we'll just show 5 fixed places
  (println "Here are 5 suggested places:")
  (doseq [i (range 1 6)] (println i ". Place" i)))

;; --- Main menu ---
(defn main-menu []
  (loop []
    (println "\nWhat can I do for you today?\n\n1. Find a place\n2. Leave a review\n3. Check history\n4. Edit profile\n0. Exit")
    (case (read-int "\nSelect an option:" #{0 1 2 3 4})
      1 (do (find-place) (recur))
      2 (do (leave-review) (recur))
      3 (do (show-history) (recur))
      4 (do (edit-profile) (recur))
      0 (println "Goodbye!"))))

;; --- Entry point ---
(defn -main []
  ;; (data/save-places!)
  ;; (doseq [p data/loaded-places]
  ;;   (println p))
  (login)
  (main-menu))

