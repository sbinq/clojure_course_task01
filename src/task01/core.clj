(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

;;; can we use advanced functions like tree-seq at this point?
;;; or the idea is to work more with map/filter/reduce - so we should not skip them?
(defn all-nodes [data]
  (tree-seq vector? #(filter vector? (children %)) data))

;;; more "dumb" implementation, not using advanced features
;;; calling this in the following code, but verified both solutions
(defn all-nodes-dumb-impl [data]
  (cons data
        (reduce concat
                (map all-nodes-dumb-impl
                     (filter vector? (children data))))))

(defn link-containers [nodes]
  (filter (fn [[tag attrs]] (and (= tag :h3) (= (:class attrs) "r"))) nodes))

(defn container-href [container]
  ;; can we use threading macros? sticking to simplest clojure features
  ;; not sure if (-> container children first attributes :href) reads better or worse for me
  (:href (attributes (first (children container)))))

(defn get-links []
  "1) Find all elements containing {:class \"r\"}.

   Example:
   [:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                         :href \"https://github.com/clojure/clojure\",
                         :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                     [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

   2) Extract href from the element :a.

   The link from the example above is 'https://github.com/clojure/clojure'.

   3) Return vector of all 10 links.

   Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]"
  ;; (->> (parse "clojure_google.html") all-nodes link-containers (map container-href))
  ;; and ->> would be fine here; again, sticking to basic features
  (let [data (parse "clojure_google.html")
        nodes (all-nodes-dumb-impl data)
        containers (link-containers nodes)]
    (vec (map container-href containers))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))
