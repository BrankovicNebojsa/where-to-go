(ns where-to-go.core
    (:gen-class)
    (:require [clojure.string :as str]
              [clj-http.client :as client]
              [where-to-go.data :as data]))

;; Data storage 
(def places (atom {})) ;; place-name -> [{:username "user1", :rating 4, :comment "Nice"} ...]
(def current-user (atom nil))
(def current-places (atom []))
(def user-agent "MyWhereToGoApp/1.0 (n.brankovic99@gmail.com)")
(def lambda 0.3)

;; Helpers for random reviews
(def sample-users ["Miroslav" "NikolaB" "Zoki6" "Pera1" "Nevena11" "Marija K" "Jovana" "Stefke"])
(def sample-comments ["Amazing!" "Pretty good." "It was okay." "Not great." "Decent." "I know better places."])

(defn random-rating []
  (-> (rand 4)        ;; random number between 0.0 and 4.0
      (+ 1)           ;; shift to 1.0 – 5.0
      (#(Math/round (* % 10))) ;; round to 1 decimal
      (/ 10.0)))      ;; divide to get float with 1 decimal

(defn random-review []
  {:username (rand-nth sample-users)
   :rating   (random-rating)
   :description (rand-nth sample-comments)})


;; This function is only supposed to be called when resetting database
;; Add random reviews 
(defn add-random-reviews-to-places! []
  (println "\n Adding random reviews to all places ")
  (doseq [[name place] @places]
    (let [num-reviews (+ 1 (rand-int 3))] ;; 1 to 3 random reviews per place
      (doseq [_ (range num-reviews)]
        (let [review (random-review)                  ;; just one call
              current-place (get @places name)
              all-reviews (conj (or (:reviews current-place) []) review)
              avg (/ (reduce + (map :rating all-reviews)) (count all-reviews))
              updated-place (assoc current-place
                                   :reviews (vec all-reviews)
                                   :avg_rating (/ (Math/round (* avg 10.0)) 10.0))]
          ;; Update atom
          (swap! places assoc name updated-place)
          ;; Persist each review separately
          (data/add-review-to-place! (:place_id place) review)
          (println "Added review to" name "New avg_rating:" (:avg_rating updated-place))))))
  (println " Done adding random reviews \n"))


;; Helpers 
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

(defn read-double [prompt valid?]
  (loop []
    (let [input (get-input prompt)
          n (try (Double/parseDouble input) (catch Exception _ nil))]
      (if (and n (valid? n))
        n
        (do (println "Invalid input, try again.") (recur))))))

(defn read-boolean [prompt]
  (loop []
    (let [input (str/lower-case (get-input prompt))]
      (cond
        (#{"yes" "y"} input) true
        (#{"no" "n"} input) false
        :else (do (println "Please enter yes or no.") (recur))))))

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

(defn get-coordinates-from-address [street-name street-number city country postal-code]
  (let [url "https://nominatim.openstreetmap.org/search"
        query (str street-name " " street-number ", " city ", " postal-code ", " country)
        response (client/get url
                             {:headers {"User-Agent" user-agent}
                              :query-params {"q" query
                                             "format" "json"
                                             "limit" "1"}
                              :as :json
                              :throw-exceptions false})]
    (when (= 200 (:status response))
      (let [result (first (:body response))]
        {:lat (Double/parseDouble (:lat result))
         :lon (Double/parseDouble (:lon result))}))))

;; User login / creation 
(defn login []
  (let [username (get-input "Enter your username:")
        user (data/read-user username)]
    (if user
      (do
        (reset! current-user user)
        (println "Welcome back," (:username user)))
      ;; user not found, create new
      (do
        (println "This is your first time using WhereToGo.") 
        (println "Please enter your data so we can find the best place for you.")
        (let [street-name (get-input "\nStreet name:")
              street-number (read-int "\nStreet number:" pos?)
              city (get-input "\nCity:")
              country (get-input "\nCountry:")
              postal-code (get-input "\nPostal code:")
              wheelchair (read-boolean "\nWheelchair accessible? (yes/no): ")
              coords (get-coordinates-from-address street-name street-number city country postal-code)
              lat (:lat coords)
              lon (:lon coords)]
          (data/create-user username street-name street-number city country postal-code wheelchair lat lon)
          (reset! current-user (data/read-user username))
          (println "\nYour user has been successfully created"))))))


;; Profile menu 
(defn edit-profile []
  (println "\nThis is your current profile information:\n")
  (println "Username:" (:username @current-user))
  (println "Street name:" (:street-name @current-user))
  (println "Number:" (:street-number @current-user))
  (println "City:" (:city @current-user))
  (println "Postal code:" (:postal-code @current-user))
  (println "Country:" (:country @current-user))
  (println "Wheelchair status:"
          (case (:wheelchair @current-user)
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
    6 (let [new-wheelchair (read-boolean "\nIs the user wheelchair accessible? (yes/no)")]
        (data/update-user (:username @current-user) {:wheelchair new-wheelchair})
        (swap! current-user assoc :wheelchair new-wheelchair)
        (println "Wheelchair status changed to:" new-wheelchair))
    0 nil))

;; Function that enables user to leave a review for a place
(defn leave-review []
  (if (empty? @places)
    (println "No places available to review.")
    (let [place-name (get-input "\nEnter the name of the place you want to review:")
          place (some #(when (= (:name %) place-name) %) (vals @places))] ;; find place by name
      (if (nil? place)
        (println "Place not found. Make sure you typed the name correctly.")
        (let [username (:username @current-user)
              rating (read-double "Enter rating (1-5):" #(<= 1 % 5))
              comment (get-input "Enter comment:")
              review {:username username
                      :rating rating
                      :description comment}
              place-id (:place_id place)]
           
          ;;  Update in-memory atom 
          (swap! places update (:name place)
                 (fn [p]
                   (let [updated-reviews (conj (or (:reviews p) []) review)
                         avg (/ (reduce + (map :rating updated-reviews)) (count updated-reviews))]
                     (assoc p
                            :reviews updated-reviews
                            :avg_rating (/ (Math/round (* avg 10.0)) 10.0)))))
          
          ;;  Persist to DB 
          (data/add-review-to-place! place-id review)
          
          ;;  Confirmation 
          (println "Review saved! New average rating for" place-name ":"
                   (get-in @places [(:name place) :avg_rating])))))))



;; Shows All Reviews of Current User 
(defn review-history []
  (let [current-username (:username @current-user)
        user-places (filter
                     (fn [place]
                       (some (fn [r] (= (:username r) current-username))
                             (:reviews place)))
                     (vals @places))]
    (if (empty? user-places)
      (println "\nYou haven't left any reviews yet.")
      (do
        (println "\nYour reviews:\n")
        (doseq [place user-places]
          (let [user-reviews (filter
                              (fn [r] (= (:username r) current-username))
                              (:reviews place))]
            (println "Place:" (:name place))
            (println "Type:" (name (:amenity_type place)))
            (doseq [r user-reviews]
              (println "Rating:" (:rating r))
              (println "Comment:" (:description r))
              (println))))))))


;; Gives a detailed preview of a place including reviews
(defn show-place-details [place]
  (println "\nHere is some more information:")
  (println "Name:" (:name place))
  (println "Type:" (name (:amenity_type place)))
  (println "Distance:" (:distance-str place))
  (println "Rating:" (or (:avg_rating place) "N/A"))
  (println "Wheelchair:" (case (:wheelchair place)
                           true "Yes"
                           false "No"
                           "N/A"))
  (println "Address:" (or (get-address-from-coordinates
                           (:lat_coordinate place)
                           (:long_coordinate place))
                          "N/A"))
  (println "\nReviews:")
  (if-let [reviews (:reviews place)]
    (doseq [r reviews]
      (println (format "- %s (Rating: %.1f) - %s"
                       (:username r)
                       (:rating r)
                       (:description r))))
    (println "No reviews yet."))
  (println "\n1. Back")
  (println "0. Exit")
  (case (read-int "\nChoose an option:" #{0 1})
    0 (System/exit 0)
    1 :back))

(defn distance_calc
  "Returns distance in km between two lat/lon pairs."
  [lat1 lon1 lat2 lon2]
  (let [R 6371.0 ;; Earth radius in km
        to-rad (fn [deg] (* deg (/ Math/PI 180)))
        dlat (to-rad (- lat2 lat1))
        dlon (to-rad (- lon2 lon1))
        a (+ (Math/pow (Math/sin (/ dlat 2)) 2)
             (* (Math/cos (to-rad lat1))
                (Math/cos (to-rad lat2))
                (Math/pow (Math/sin (/ dlon 2)) 2)))
        c (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))]
    (* R c)))

(defn exp-decay-distance [d-km]
  (Math/exp (* -1 lambda d-km)))

(defn score-place [p user-lat user-lon]
  (let [dist (distance_calc user-lat user-lon
                            (:lat_coordinate p)
                            (:long_coordinate p))
        norm-dist (exp-decay-distance dist)
        norm-rating (double (or (:avg_rating p) 0))

        ;; wheelchair preference
        needs-wheelchair (:wheelchair @current-user)
        wheelchair-score (cond
                           ;; user requires wheelchair but place explicitly says no → strong penalty
                           (and needs-wheelchair (= false (:wheelchair p))) -10
                           ;; user requires wheelchair and place is yes → bonus
                           (and needs-wheelchair (= true (:wheelchair p))) 2
                           ;; user requires wheelchair but unknown → small penalty
                           (and needs-wheelchair (nil? (:wheelchair p))) -1
                           ;; user doesn’t care → neutral
                           :else 0)

        ;; weights
        alpha 0.7   ;; rating weight
        beta  0.3   ;; distance weight

        score (+ (* alpha norm-rating)
                 (* beta norm-dist)
                 wheelchair-score)]
    (assoc p
           :distance dist
           :distance-str (format "%.1f km" dist)
           :score score)))

;; Suggests 5 places based on entered criteria and algorithm that makes a decision on which places
;; are best for this user. (It uses attributes such as distance to the place and average rating)
(defn suggest-places
   ([] (suggest-places nil))
   ([types]
    (let [user-lat (:lat_coordinate @current-user)
          user-lon (:long_coordinate @current-user)
          needs-wheelchair (:wheelchair @current-user)

          places (->> (data/get-all-places)
                      ;; filter by type if provided
                      (filter #(or (nil? types)
                                   (some #{(keyword (:amenity_type %))} types)))
                      ;; filter wheelchair if required
                      (filter #(or (not needs-wheelchair)
                                   (not= false (:wheelchair %))))
                      ;; add distance + score
                      (map #(score-place % user-lat user-lon))
                      ;; sort by score descending
                      (sort-by :score >)
                      (take 5)
                      vec)]
      (reset! current-places places)

      ;; Menu loop
      (loop []
        (println "\nHere are 5 suggested places where you can go today:\n")

        ;; Header
        (if needs-wheelchair
          (println
           (format "%-3s %-35s %-12s %-10s %-8s %-12s %s"
                   "NR" "Name" "Type" "Distance" "Rating" "Wheelchair" "Address"))
          (println
           (format "%-3s %-35s %-12s %-10s %-8s %s"
                   "NR" "Name" "Type" "Distance" "Rating" "Address")))

        ;; Rows
        (doseq [[i p] (map-indexed vector @current-places)]
          (if needs-wheelchair
            (println
             (format "%-3s %-35s %-12s %-10s %-8s %-12s %s"
                     (str (inc i) ".")
                     (:name p)
                     (or (:amenity_type p) "N/A")
                     (or (:distance-str p) "N/A")
                     (or (:avg_rating p) "N/A")
                     (case (:wheelchair p)
                       true  "Yes"
                       false "No"
                       "N/A")
                     (or (get-address-from-coordinates
                          (:lat_coordinate p)
                          (:long_coordinate p))
                         "N/A")))
            (println
             (format "%-3s %-35s %-12s %-10s %-8s %s"
                     (str (inc i) ".")
                     (:name p)
                     (or (:amenity_type p) "N/A")
                     (or (:distance-str p) "N/A")
                     (or (:avg_rating p) "N/A")
                     (or (get-address-from-coordinates
                          (:lat_coordinate p)
                          (:long_coordinate p))
                         "N/A")))))

      (println "0.  Back")

      (let [choice (read-int "\nChoose a place:" (set (range 0 (inc (count @current-places)))))]
        (if (= choice 0)
          :back
          (do
            (show-place-details (@current-places (dec choice)))
            (recur))))))))

;; Submenus 
;; Forward declarations 
(declare menu-a22 menu-a221)

;; Has an idea menu (A1) 
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

;; Lively Submenu (A2212) 
(defn menu-a2212 []
  (loop []
    (println "\nLet's find some lively place for you!")
    (println "What kind of experience are you looking for?\n")
    (println "1. Drinks & Chatting")
    (println "2. Party & Adrenaline")
    (println "0. Back")
    (let [lively-choice (read-int "\nSelect an option:" #{0 1 2})]
      (cond
        (= lively-choice 0) :back
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

;; Eat/Drink Menu (A221)
(defn menu-a221 []
  (loop []
    (println "\nEat/Drink it is!")
    (println "Do you want a quiet place or a lively place?\n")
    (println "1. Quiet")
    (println "2. Lively")
    (println "0. Back")
    (let [sub-choice (read-int "\nSelect an option:" #{0 1 2})]
      (cond
        (= sub-choice 0) :back
        (= sub-choice 1)
        (do
          (suggest-places [:restaurant :cafe])
          (recur))
        (= sub-choice 2) 
        (let [res (menu-a2212)]
          (if (= res :back) (recur) (recur)))
        :else
        (do
          (println "Error, please try again.")
          (recur))))))

;; Relax/Watch Menu (A222) 
(defn menu-a222 []
  (loop []
    (let [result (suggest-places [:cinema :theatre :library])]
      (if (= result :back)
        :back   ;; return to previous menu
        (recur)))))

;; Bespoke Suggestion Menu (A22) 
(defn menu-a22 []
  (loop []
    (println "\nAre you looking for a place to eat/drink or to relax/watch something?")
    (println "1. Eat/Drink")
    (println "2. Relax/Watch")
    (println "0. Back")
    (print "\nSelect an option: ")
    (flush)
    (let [choice (read-line)]
      (case choice
        "1" (let [res (menu-a221)]
              (if (= res :back)
                (recur)
                (recur)))
        "2" (let [res (menu-a222)]
              (if (= res :back)
                (recur)
                (recur)))
        "0" :back
        (do (println "Invalid choice, try again.") (recur))))))

;; No plan menu (A2)
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

;; Menu for finding a place (A) 
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

;; Main menu 
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
      3 (do (review-history) (recur))
      4 (do (edit-profile) (recur))
      0 (println "\nThanks for using WhereToGo app. Have a nice day! :)"))))

;; Entry point 
(defn -main []
  ;; ;; Save places if needed
  ;; (data/save-places!)

  ;; Load all places from DB into the atom
  (reset! places
          (->> (data/get-all-places)
               (map (fn [p] [(:name p) p]))  ;; map name -> place map
               (into {})))

  ;; ;; Add random reviews before login
  ;; ;; Only to be done once!
  ;; (add-random-reviews-to-places!)

  ;; ;; Optional: print loaded places for debugging
  ;; (doseq [[name place] @places]
  ;;   (println name ":" place))

  ;; Login user
  (login)
  ;; Main Menu
  (main-menu))
