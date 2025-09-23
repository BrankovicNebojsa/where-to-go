(ns where-to-go.core
    (:gen-class)
    (:require [clojure.string :as str]
              [clj-http.client :as client]
              [cheshire.core :as json]
              [where-to-go.data :as data]))

;; --- Data storage ---
(def users (atom {})) ;; username -> {:location "N/A", :wheelchair false, :history []} 
(def places (atom {})) ;; place-name -> [{:username "user1", :rating 4, :comment "Nice"} ...]
(def current-user (atom nil))
(def current-places (atom []))
(def user-agent "MyWhereToGoApp/1.0 (n.brankovic99@gmail.com)")

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
        (println "This is your first time using WhereToGo. 
                  Please enter your data so we can find the best place for you.")
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

(defn format-address [addr]
  (str (or (:road addr) "")
       " " (or (:house_number addr) "")
       ", " (or (:city addr) "")))

(defn get-address-from-coordinates [lat lon]
  (let [url "https://nominatim.openstreetmap.org/reverse"
        params {"format" "json"
                "lat" (str lat)
                "lon" (str lon)
                "zoom" "18"
                "addressdetails" "1"}
        response (client/get url
                             {:headers {"User-Agent" user-agent}
                              :query-params params
                              :as :json
                              :throw-exceptions false})
        status (:status response)]
    (if (= 200 status)
      (let [addr (get-in response [:body :address])]
        (if addr
          (format-address addr)
          (or (get-in response [:body :display_name])
              "Unknown address")))
      (do
        (println "API call failed with status:" status "body:" (:body response))
        "Unknown address"))))


(defn show-place-details [place]
  (println "\nHere is some more information:")
  (println "Name:" (:name place))
  (println "Type:" (name (:amenity_type place)))
  (println "Distance:" (:distance place))
  (println "Rating:" (or (:rating place) "N/A"))
  (println "Address:" (:address place))
  ;; TODO: print wheelchair + reviews when connected
  (println "\n1. Back")
  (println "0. Exit")
  (case (read-int "\nChoose an option:" #{0 1})
    0 (System/exit 0)
    1 :back))

(defn suggest-places
  "Suggest 5 places. Optionally filter by types (vector of keywords)."
  ([] (suggest-places nil))
  ([types]
   ;; pick 5 places only once and store in atom
   ;; --- I have to find an algorithm here to find a place (location, wheelchair and rating) ---
   (let [places (->> (data/get-all-places)
                     (filter #(or (nil? types) (some #{(keyword (:amenity_type %))} types)))
                     shuffle
                     (take 5)
                     vec)]
     (reset! current-places places)  ;; save to atom

     ;; Menu loop
     (loop []
       (println "\nHere are 5 suggested places where you can go today:\n")
       ;; Print column headers with proper alignment
       (println
        (format "%-3s %-35s %-12s %-10s %-8s %s"
                "NR" "Name" "Type" "Distance" "Rating" "Address"))
       (doseq [[i p] (map-indexed vector @current-places)]
         (println
          (format "%-3s %-35s %-12s %-10s %-8s %s"
                  (str (inc i) ".")          ;; NR
                  (:name p)                  ;; Name
                  (or (:amenity_type p) "N/A") ;; Type
                  (or (:distance p) "N/A")     ;; Distance
                  (or (:rating p) "N/A")       ;; Rating
                  (or (get-address-from-coordinates (:lat_coordinate p) (:long_coordinate p)) "N/A"))))   ;; Address
       (println "0.  Back")

       ;; Ask user choice
       (let [choice (read-int "\nChoose a place:" (set (range 0 (inc (count @current-places)))))]
         (if (= choice 0)
           :back
           (do
             ;; show details from current-places
             (show-place-details (@current-places (dec choice)))
             (recur))))))))

;; --- Submenus ---
;; --- Forward declarations ---
(declare menu-a22 menu-a221)

;; --- Has an idea menu (A1) ---
(defn menu-a1 []
  (let [type-map {1 :restaurant
                  2 :bar
                  3 :cafe
                  4 :pub
                  5 :casino
                  6 :library
                  7 :theatre
                  8 :cinema
                  9 :nightclub}]
    (loop []
      (println "\nGreat! Which type of place are you interested in today?")
      (println "1. Restaurant")
      (println "2. Bar")
      (println "3. Cafe")
      (println "4. Pub")
      (println "5. Casino")
      (println "6. Library")
      (println "7. Theatre")
      (println "8. Cinema")
      (println "9. Nightclub")
      (println "0. Back")

      (let [choice (read-int "\nSelect an option:" (set (range 10)))]
        (cond
          (= choice 0) :back
          (contains? type-map choice)
          (do
            (suggest-places [(type-map choice)])
            (recur))
          :else
          (do
            (println "Error, please try again.")
            (recur)))))))

;; --- Lively Submenu (A2212) ---
(defn menu-a2212 []
  (loop []
    (println "\nLet's find some lively place for you!")
    (println "What kind of experience are you looking for?\n")
    (println "1. Drinks & Chatting")
    (println "2. Party & Adrenaline")
    (println "0. Back")
    (let [lively-choice (read-int "\nSelect an option:" #{0 1 2})]
      (cond
        (= lively-choice 0) (menu-a221) ;; back to quiet/lively menu
        (= lively-choice 1)
        (do
          (suggest-places [:bar :pub])
          (recur))
        (= lively-choice 2)
        (do
          (suggest-places [:casino :nightclub])
          (recur))
        :else
        (do
          (println "Error, please try again.")
          (recur))))))

;; --- Eat/Drink Menu (A221)---
(defn menu-a221 []
  (loop []
    (println "\nEat/Drink it is!")
    (println "Do you want a quiet place or a lively place?\n")
    (println "1. Quiet")
    (println "2. Lively")
    (println "0. Back")
    (let [sub-choice (read-int "\nSelect an option:" #{0 1 2})]
      (cond
        (= sub-choice 0) (menu-a22)
        (= sub-choice 1)
        (do
          (suggest-places [:restaurant :cafe])
          (recur))
        (= sub-choice 2) (menu-a2212)
        :else
        (do
          (println "Error, please try again.")
          (recur))))))

;; --- Relax/Watch Menu (A222) ---
(defn menu-a222 []
  (loop []
    (suggest-places [:cinema :theatre :library])
    (recur)))

;; --- Bespoke Suggestion Menu (A22) ---
(defn menu-a22 []
  (loop []
    (println "\nAre you looking for a place to eat/drink or to relax/watch something?")
    (println "1. Eat/Drink")
    (println "2. Relax/Watch")
    (println "0. Back")
    (let [choice (read-int "\nSelect an option:" #{0 1 2})]
      (cond
        (= choice 0) :back
        (= choice 1) (menu-a221)
        (= choice 2) (menu-a222)
        :else
        (do
          (println "Error, please try again.")
          (recur))))))

;; --- No plan menu (A2)
(defn menu-a2 []
  (loop []
    (println "\nThat's okay! What would you prefer?")
    (println "1. Suggest 5 random places")
    (println "2. Give bespoke suggestion after couple of questions")
    (println "0. Back")
    (case (read-int "\nSelect an option:" #{0 1 2})
      0 :back
      1 (do (suggest-places) (recur))
      2 (do (menu-a22) (recur)))))

;; --- Menu for finding a place (A) ---
(defn find-place []
  (loop []
    (println "\nDo you already have a type of place in mind for today's outing?")
    (println "1. Yes")
    (println "2. No")
    (println "0. Back")
    (case (read-int "\nSelect an option:" #{0 1 2})
      0 :back
      1 (do (menu-a1) (recur))
      2 (do (menu-a2) (recur)))))

;; --- Main menu ---
(defn main-menu []
  (loop []
    (println "\nWhat can I do for you today?\n")
    (println "1. Find a place")
    (println "2. Leave a review")
    (println "3. Check history")
    (println "4. Edit profile")
    (println "0. Exit")
    (case (read-int "\nSelect an option:" #{0 1 2 3 4})
      1 (do (find-place) (recur))
      2 (do (leave-review) (recur))
      3 (do (show-history) (recur))
      4 (do (edit-profile) (recur))
      0 (println "\nThanks for using WhereToGo app. Have a nice day! :)"))))

;; --- Entry point ---
(defn -main []
  ;; (data/save-places!)
  ;; (doseq [p data/loaded-places]
  ;;   (println p))
  (login)
  (main-menu))

