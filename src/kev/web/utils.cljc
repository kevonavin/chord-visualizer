(ns kev.web.utils
  (:require
   [cljs.pprint :as pprint]
   [odoyle.rules :as o]
   [reagent.core :as r]
   ))

(defn ezip
  "like zip, but adds nils if one is shorter"
  [s1 s2]
  (loop [[h1 & t1] s1
         [h2 & t2] s2
         sr []]
    (if (or h1 h2)
      (recur t1 t2 (conj sr [h1 h2]))
      sr)))


(defn params-for [[in & remains :as _query]]
  (when (= :in in)
    (->> remains
         (take-while (complement keyword?)))))

(defn ->pquery-rule
  "create a parameterized query rule. This can be used later with pquery rname.
  rname is a qualified keyword. params are a list of params as symbols to be used.
  These are meant to match up to things in the what clause probably."
  [rname [in & remains :as query]]
  (let [params (params-for query)
        query' (if (= :in in)
                 (drop-while symbol? remains)
                 query)]
    (o/->rule
     rname
     (-> (into [] (take 1 query')) ;; due to perf issues, we gotta do this first
         (into (into [['pquery-aux-id :pquery/pquery-rname rname]
                      ['pquery-aux-id :pquery/pquery-aux-atom 'pquery-aux-atom]]
                     (map (fn [p]
                            ['pquery-aux-id (keyword "pquery" p) p]))
                     params))
         (into (drop 1 query'))
         (into [:then
                (fn [{:keys [pquery-aux-atom] :as match}]
                  (reset! pquery-aux-atom (dissoc match
                                                  :pquery-aux-atom)))])
         (doto pprint/pprint)))))

(defn setup-query [sesh rule-name id ratom param-map]
  (let [fax (merge param-map
                   {:pquery-rname rule-name
                    :pquery-aux-atom ratom})
        fax' (into {}
                   (map (fn [[k v]]
                          [(keyword "pquery" k) v]))
                   fax)]
    (o/insert sesh id fax')))

(defn insert-facts [sesh facts]
  (reduce #(apply o/insert %1 %2) sesh facts))

(defn remove-facts [sesh id attrs]
  (reduce #(apply o/retract %1 %2)
          sesh
          (zipmap (repeat id) attrs)))

(defn maybe-add-rule [sesh rule-name query]
  (if (get-in sesh [:rule-name->node-id rule-name])
    sesh
    (as-> sesh $
      (o/add-rule $ (->pquery-rule rule-name query))
      (reduce o/insert $ (o/query-all $))
      (o/fire-rules $))))

(defn rule-react [rule-name query sesh & args]
  (r/with-let [atm (r/atom nil)
               id (random-uuid)
               param-map (zipmap (params-for query)
                                 args)
               _ (swap! sesh maybe-add-rule rule-name query)
               _ (swap! sesh
                        setup-query
                        rule-name
                        id
                        atm
                        param-map)
               _ (swap! sesh o/fire-rules)]
    @atm
    (finally
      (swap! sesh remove-facts id (map (fn [[k _v]]
                                         (keyword "pquery" k))
                                       param-map)))))
