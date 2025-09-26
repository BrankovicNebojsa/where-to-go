# WhereToGo

**WhereToGo** is a Clojure-based CLI application that helps users discover the best places nearby based on distance, ratings, and wheelchair accessibility if needed.  
It uses real location data from Belgrade (fetched via **Overpass Turbo** and stored in JSON files) and integrates them into a local **Codax embedded database**.

## Features
- 🧮 **Recommendation algorithm** – Combines average rating, distance (with exponential decay), and accessibility preferences to provide personalized suggestions.
- 🔍 **Find places** – Get the top 5 recommendations filtered by type (restaurants, bars, cafes, cinemas, etc.) or let the system suggest random places.
- ⭐ **Reviews** – Leave ratings and comments for places; average scores are updated automatically.
- 📜 **History** – View your personal review history.
- 👤 **Profile management** – Edit your user profile (address, wheelchair accessibility, etc.).
- 📍 **Geolocation** – Uses **Nominatim OpenStreetMap API** to convert between addresses and coordinates.

## Tech Stack
- **Clojure** (main logic & CLI)
- **Codax** (embedded database)
- **Cheshire** (JSON parsing)
- **clj-http** (API calls)
- **OpenStreetMap Nominatim API**

## Getting Started
1. Install [Leiningen](https://leiningen.org/).
2. Clone this repository:
    ```bash
    git clone https://github.com/your-username/where-to-go.git
    cd where-to-go
3. Set the terminal to UTF-8 encoding (required for displaying Serbian characters correctly).
On Windows, run:
    ```bash
    chcp 65001

On Linux/macOS, UTF-8 is usually the default.
4. Run the application:
    ```bash
    lein run
