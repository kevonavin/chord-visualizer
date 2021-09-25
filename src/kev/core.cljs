(ns kev.core
  (:require
   [datascript.core :as d]
   [net.cgrand.xforms :as x]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [reagent.ratom :as ratom]
   [odoyle.rules :as o])
  (:require-macros [odoyle.rules :as o]))

(defn ezip
  "like zip, but adds nils if one is shorter"
  [s1 s2]
  (loop [[h1 & t1] s1
         [h2 & t2] s2
         sr []]
    (if (or h1 h2)
      (recur t1 t2 (conj sr [h1 h2]))
      sr)))

(def getid
  (let [atm (atom 0)]
    (fn []
      (swap! atm inc))))

(def fret-multiplier (Math/pow 0.5 (/ 1 12)))

(defn fret-height [fret]
  (- (Math/pow fret-multiplier (dec fret))
     (Math/pow fret-multiplier fret)))

;; insert a fact
;; domid {:reagent/fnum 'stuff :reagent/atom ratom}
;; then the rule would fire, call the ratom w/ new data
;; (->make-rule {:my-param 'fnum} [...query])
(def rules
  (o/ruleset
   {::test
    [:what
     [::fretboard :fretboard/height fbh]
     [fid :fret/number fnum]
     [cid ::fnum fnum]
     [cid ::ratom ratom]
     :then
     (reset! ratom {:fnum fnum
                    :fh (* fbh (fret-height (inc fnum)))})
     (prn "fret rule fired" {:fnum fnum
                             :fbh fbh})]

    #_#_::fret-height
      [:what
       [::fretboard :fretboard/height fbh]
       [fid :fret/number fnum]
       :then
       (prn "fh fired")
       (o/insert! fid :fret/height (* fbh (fret-height (inc fnum))))]

    ::fret-seq
    [:what
     [fid :fret/number fnum]
     :then-finally
     (->> (o/query-all o/*session* ::fret-seq)
          (#(do (prn {:fret-seq2-rule %}) %))
          (transduce (comp
                      (x/sort-by :fnum)
                      (map :fid))
                     conj [])
          (#(do (prn {:fret-seq2-post %}) %))
          (o/insert! ::fretboard :fretboard/fret-seq))]

    ::num-frets
    [:what
     [::fretboard :fretboard/num-frets num-frets]
     [::fretboard :fretboard/fret-seq fseq {:then not=}]
     :then
     (prn "fseq" fseq)
     (doseq [[fnum fsf] (ezip (range num-frets) fseq)]
       (cond
         (nil? fsf)
         (o/insert! (getid) :fret/number fnum)
         (nil? fnum)
         (o/retract! fsf :fret/number)))]}))

(def initial-facts [[::fretboard {:fretboard/num-frets 20
                                  :fretboard/height  850
                                  :fretboard/width 100
                                  :fretboard/fret-seq nil}]])

(defn insert-facts [sesh facts]
  (reduce #(apply o/insert %1 %2) sesh facts))

(defn remove-facts [sesh id attrs]
  (reduce #(apply o/retract %1 %2)
          sesh
          (zipmap (repeat id) attrs)))

(def sesh (atom (as-> (o/->session) $
                 (reduce o/add-rule $ rules)
                 (insert-facts $ initial-facts)
                 (o/fire-rules $))))

(defn rule-react [sesh fact-map]
  (r/with-let [atm (r/atom nil)
               id (random-uuid)
               _ (swap! sesh insert-facts
                        [[id (assoc fact-map
                                    ::ratom atm)]])
               _ (swap! sesh o/fire-rules)]
    @atm
    (finally
      (swap! sesh remove-facts id (into [::ratom]
                                        (keys fact-map))))))

(def schema {:fretboard/height {:db/valueType :db.type/ref}
             :fretboard/width {:db/valueType :db.type/long}
             :fret/ndots {:db/valueType :db.type/long}
             :fret/number {:db/valueType :db.type/long}})

(def conn (d/create-conn))

;; see posh https://github.com/denistakeda/posh
;; which implements this, probably better than I
(d/transact! conn [{:db/ident :fretboard
                    :fretboard/height 850
                    :fretboard/width 100}])

(d/transact! conn (->> [0 0 1 0 1 0 1 0 1 0 0 2]
                       (cycle)
                       (map-indexed (fn [idx dots]
                                      {:fret/number idx
                                       :db/ident idx
                                       :fret/ndots  dots}))
                       (take 20)
                       (into [])))

(defn reactq [conn q & {:keys [xform in]}]
  (letfn [(runq [db]
            (->> (apply d/q q db in)
                 (into [] (or xform identity))
                 first))]
    (r/with-let [ratom (r/atom (runq @conn))
                 lid (d/listen! conn
                                (fn [{:keys [db-before db-after] :as _tx-report}]
                                  (reset! ratom (runq db-after))))]
      @ratom
      (finally
        (d/unlisten! conn lid)))))

(defn fret [w h marks notes]
  (let [mark-height (/ h 3.5)
        key-height (/ w 5)
        inner-style {:height          h
                     :width           w
                     :border          "0.5px solid black"
                     :display         "flex"
                     :justify-content "space-evenly"
                     :position "absolute"
                     :top 0
                     :left 0
                     :align-items     "center"}]
    [:div {:style {:position "relative"}}
     [:div {:style (merge inner-style
                          {:z-index 9})}
      (for [note notes]
        [:div {:style (merge {:height        key-height
                              :width         key-height
                              :text-align "center"}
                             (when note
                               {:border        "1px solid black"
                                :border-radius key-height
                                :background    "lime"}))}
         note])]
     [:div {:style (merge inner-style {:order "0.5px solid black"})}
      (for [_ (range marks)]
        [:div {:style {:height        mark-height
                       :width         mark-height
                       :border        "1px solid black"
                       :border-radius mark-height
                       :background    "grey"}}])]]))

(defn fret3 [sesh fnum]
  (let [{:keys [fh] :as match}
        @(r/track rule-react sesh {::fnum fnum})]
    [fret 100 fh 1 nil]))

(comment
  (doseq [_ (range 10)]
    (println "."))
  (rdom/render
   [:div [fret3 sesh 0]
    [fret3 sesh 1]]
   js/document.body)
  (o/query-all @sesh)
  (swap! sesh o/fire-rules)
  (swap! sesh insert-facts [[::fretboard {:fretboard/height 950}]])
  99)

(defn fret-2 [conn fret-num]
  (let [[dots fbh fbw]
        @(r/track reactq
                  conn
                  '[:find ?dots ?fbh ?fbw
                    :in $ ?fret-num
                    :where [?e :fret/number ?fret-num]
                    [?e :fret/ndots ?dots]
                    [?e2 :fretboard/height ?fbh]
                    [?e2 :fretboard/width ?fbw]]
                  :in [fret-num])]
    [fret fbw (* fbh (fret-height (inc fret-num))) dots]))

(defn fretboard [conn]
  (let [fret-nums @(r/track reactq
                            conn
                            '[:find ?fret-num
                              :where [_ :fret/number ?fret-num]]
                            :xform (comp cat
                                         (x/sort)
                                         (x/into [])))]
    (prn fret-nums)
    [:div
     (for [n fret-nums]
       [fret-2 conn n])]))

(defn init []
  (.log js/console "big wow")
  (rdom/render
   [:div [fretboard conn]]
   js/document.body))

(comment
  "boom"

  (d/q '[:find ?number ?dots
         :keys number dots
         :where [?e :fret/number ?number]
         [?e :fret/ndots ?dots]]
       @conn)

  (defn tester []
    [:div (str "wenis " (->
                         @(r/track ->reactq conn '[:find ?height
                                                   :where [?e :db/ident :fretboard]
                                                   [?e :fretboard/height ?height]]
                                   cat)
                         (doto prn)))])

  (rdom/render
   [:div
    [fretboard conn]
    [tester]
    [:input {:style {:width "150px"}
             :type "range"
             :value (-> '[:find ?height
                          :where [?e :db/ident :fretboard]
                          [?e :fretboard/height ?height]]
                        (d/q @conn)
                        first first)
             :min 1 :max 1000
             :on-change (fn [e]
                          (let [newv (.. e -target -value)]
                            (d/transact! conn [{:db/ident :fretboard
                                                :fretboard/height newv}])))}]]
   js/document.body))
