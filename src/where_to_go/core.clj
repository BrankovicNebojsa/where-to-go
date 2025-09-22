(ns where-to-go.core
    (:gen-class)
    (:require [clojure.string :as str]))

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

  ;; --- User login ---
  (defn login []
    (let [username (get-input "Enter your username:")]
      (swap! users #(if (contains? % username) % (assoc % username {:location "N/A" :wheelchair false :history []})))
      (reset! current-user username)
      (println (str "\nWelcome to WhereToGo, " username "!"))))

 ;; --- Profile menu ---
 (defn edit-profile []
   (let [user-data (@users @current-user)]
     (println "This is your current profile information:\n")
     (println "Username:" @current-user)
     (println "Location:" (:location user-data))
     (println "Wheelchair status:" (:wheelchair user-data))
     (println "\nWhat would you like to change?\n")
     (println "1. Username\n2. Location\n3. Wheelchair status\n0. Back\n")
     (case (read-int "Select an option:" #{0 1 2 3})
       1 (let [new-username (get-input "\nEnter new username:")]
           (swap! users #(assoc % new-username (get % @current-user)))
           (swap! users #(dissoc % @current-user))
           (reset! current-user new-username)
           (println "\nUsername has been changed successfully") (edit-profile))
       2 (let [new-loc (get-input "\nEnter new location:")]
           (swap! users update @current-user assoc :location new-loc)
           (println "Location updated!") (edit-profile))
       3 (let [wcs (read-int "\nWheelchair? \n\n1. Yes\n2. No\n\nSelect an option:" #{1 2})]
           (swap! users update @current-user assoc :wheelchair (= wcs 1))
           (println "Wheelchair status updated!") (edit-profile))
       0 nil)))

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
    (println "What can I do for you today?\n\n1. Find a place\n2. Leave a review\n3. Check history\n4. Edit profile\n0. Exit")
    (case (read-int "Select an option:" #{0 1 2 3 4})
      1 (do (find-place) (recur))
      2 (do (leave-review) (recur))
      3 (do (show-history) (recur))
      4 (do (edit-profile) (recur))
      0 (println "Goodbye!"))))

;; --- Entry point ---
(defn -main [& args]
  (login)
  (main-menu))

