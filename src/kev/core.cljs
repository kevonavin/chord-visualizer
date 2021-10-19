(ns kev.core
  (:require
   [net.cgrand.xforms :as x]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [reagent.ratom :as ratom]
   [odoyle.rules :as o]
   [cljs.pprint :as pprint]
   [kev.web.utils :as kwu])
  (:require-macros [odoyle.rules :as o]))

(def fret-multiplier (Math/pow 0.5 (/ 1 12)))

(defn fret-height
  ([fret]
   (- (Math/pow fret-multiplier (dec fret))
      (Math/pow fret-multiplier fret)))
  ([fret num-frets]
   (/ (fret-height fret)
      (- 1 (Math/pow fret-multiplier num-frets)))))

(def notes '("a" "a#" "b" "c" "c#" "d" "d#" "e" "f" "f#" "g" "g#"))

;; insert a fact
;; domid {:reagent/fnum 'stuff :reagent/atom ratom}
;; then the rule would fire, call the ratom w/ new data
;; (->make-rule {:my-param 'fnum} [...query])
(def rules
  (o/ruleset
   {::fret-dots
    [:what
     [fid :fret/number fnum]
     :then
     (let [dots
           (nth (cycle [0 0 0 1 0 1 0 1 0 1 0 0 2]) fnum)]
       (o/insert! fid :fret/dots dots))]

    ::fret-notes
    [:what
     [fid :fret/number fnum]
     [::fretboard :fretboard/string-seq string-seq]
     [::fretboard :fretboard/enabled-notes enabled-notes]
     :then
     (o/insert!
      fid :fret/notes
      (for [{:keys [note]} string-seq]
        (->> notes
             (cycle)
             (drop-while (partial not= note))
             (drop fnum)
             first
             (enabled-notes))))]

    ::fret-height
    [:what
     [::fretboard :fretboard/height fbh]
     [fid :fret/number fnum]
     :then
     (o/insert! fid :fret/height (* fbh (fret-height (inc fnum))))]

    ::string-notes
    [:what
     [id :string/number num]
     [id :string/note note]
     :then-finally
     (->> (o/query-all o/*session* ::string-notes)
          (sort-by :num)
          (o/insert! ::fretboard :fretboard/string-seq))]

    ::fret-seq
    [:what
     [fid :fret/number fnum]
     :then-finally
     (->> (o/query-all o/*session* ::fret-seq)
          (transduce (comp
                      (x/sort-by :fnum)
                      (map :fid))
                     conj [])
          (o/insert! ::fretboard :fretboard/fret-seq))]

    ::num-frets
    [:what
     [::fretboard :fretboard/num-frets num-frets]
     [::fretboard :fretboard/fret-seq fseq {:then not=}]
     :then
     (doseq [[fnum fsf] (kwu/ezip (range num-frets) fseq)]
       (cond
         (nil? fsf)
         (o/insert! (random-uuid) :fret/number fnum)
         (nil? fnum)
         (o/retract! fsf :fret/number)))]}))

(def initial-facts (into [[::fretboard {:fretboard/num-frets 20
                                        :fretboard/height  850
                                        :fretboard/width 100
                                        :fretboard/fret-seq nil
                                        :fretboard/enabled-notes #{}}]]
                         (map-indexed
                          (fn [i n]
                            [(random-uuid)
                             {:string/number i
                              :string/note n}])
                          ["e" "a" "d" "g" "b" "e"])))

(def sesh (atom (as-> (o/->session) $
                  (reduce o/add-rule $ rules)
                  (kwu/insert-facts $ initial-facts)
                  (o/fire-rules $))))


(defn fret [w h dots notes {:keys [no-border?]}]
  (let [mark-height (/ h 3.5)
        border-bottom (if no-border? 0 1)
        key-height (min (* h 0.9) (/ w 8))
        inner-style {:border          "0.5px solid black"
                     :border-top 0
                     :border-bottom (str border-bottom "px solid black")
                     :display         "flex"
                     :justify-content "space-evenly"
                     :position "absolute"
                     :top 0
                     :left 0
                     :width "100%"
                     :height "100%"
                     :align-items     "center"}]
    [:div {:style {:position "relative"
                   :height          h
                   :width           w}}
     [:div {:style (merge inner-style
                          {:z-index 9})}
      (for [[i note] (map-indexed vector notes)]
        ^{:key i} [:div {:style (merge {:height        key-height
                                        :width         key-height
                                        :align-self "flex-end"
                                        :display "flex"
                                        :align-items "center"
                                        :border 1
                                        :justify-content "center"}
                                       (when note
                                         {:border        "1px solid black"
                                          :border-radius key-height
                                          :background    "lime"}))}
                   [:p note]])]
     [:div {:style (merge inner-style {:order "0.5px solid black"})}
      (for [i (range dots)]
        ^{:key i} [:div {:style {:height        mark-height
                                 :width         mark-height
                                 :border        "1px solid black"
                                 :border-radius mark-height
                                 :background    "grey"}}])]]))

(defn fret4 [sesh fret-id width height opts]
  (let [{:keys [dots notes]}
        @(r/track kwu/rule-react
                  ::fret4
                  '[:in fid
                    :what
                    [fid :fret/notes notes]
                    [fid :fret/dots dots]]
                  sesh fret-id)]
    [fret width height dots notes opts]))

(defn fretboard [sesh]
  (let [{:keys [fret-seq width height]}
        @(r/track kwu/rule-react
                  ::fretboard-render3
                  '[:what
                    [::fretboard :fretboard/width width]
                    [::fretboard :fretboard/height height]
                    [::fretboard :fretboard/fret-seq fret-seq]]
                  sesh)
        [ffret remaining] (split-at 2 (map-indexed vector fret-seq))
        fret-count        (dec (count fret-seq))
        ffret-height      (fret-height 1 fret-count)
        [f1-id f2-id] (map second ffret)]
    [:div {:style {}}
     (concat
      [^{:key f1-id} [fret4 sesh f1-id width (* 0.4 ffret-height height) {:no-border? true}]
       ^{:key f2-id} [fret4 sesh f2-id width (* 0.7 ffret-height height)]]
      (for [[i fret-id] remaining]
        ^{:key fret-id} [fret4 sesh fret-id width (* height (fret-height i fret-count))]))]))

(defn setting [sesh [id attr] current min max]
  [:input {:style {:width 150}
           :type "range"
           :value current
           :in min :max max
           :on-change (fn [e]
                        (let [newv (.. e -target -value)]
                          (swap! sesh o/insert id attr (js/parseInt newv))
                          (swap! sesh o/fire-rules)))}])

(defn settings [sesh]
  (let [{:keys [nfrets width height enabled-notes]}
        @(r/track kwu/rule-react
                  ::settings2
                  '[:what
                    [::fretboard :fretboard/num-frets nfrets]
                    [::fretboard :fretboard/width width]
                    [::fretboard :fretboard/enabled-notes enabled-notes]
                    [::fretboard :fretboard/height height]]
                  sesh)]
    [:div {:style {:float "left"}}
     [:p "num frets: " nfrets]
     [setting sesh [::fretboard :fretboard/num-frets] nfrets 3 40]
     [:p "height: " height]
     [setting sesh [::fretboard :fretboard/height] height 50 2000]
     [:p "width: " width]
     [setting sesh [::fretboard :fretboard/width] width 10 800]
     [:div
      (for [n notes]
        ^{:key n}
        [:div {:style {:display "inline-block"}}
         [:div n]
         [:input {:type "checkbox"
                  :checked (boolean (enabled-notes n))
                  :on-change (fn [e]
                               (let [checked (.. e -target -checked)]
                                 (swap! sesh o/insert
                                        ::fretboard :fretboard/enabled-notes
                                        ((if checked conj disj) enabled-notes n))
                                 (swap! sesh o/fire-rules)))}]])]]))

(defn tuners [sesh]
  (let [{:keys [width strings]}
        @(r/track kwu/rule-react
                  ::tuners
                  '[:what
                    [::fretboard :fretboard/string-seq strings]
                    [::fretboard :fretboard/width width]]
                  sesh)]
    [:div
     (for [{:keys [id note]} strings]
       ^{:key id}
       [:select {:style {:width (/ width (count strings))}
                 :on-change (fn [e]
                              (let [newv (.. e -target -value)]
                                (swap! sesh o/insert id :string/note newv)
                                (swap! sesh o/fire-rules)))}
        (for [n notes]
          ^{:key n}
          [:option {:value n
                    :selected (= note n)}
           n])])]))

(defn init []
  (rdom/render
   [:div
    [:div {:style {:float "left"
                   :width "50%"
                   :height "100%"}}
     [tuners sesh]
     [fretboard sesh]]
    [settings sesh]]
   js/document.body)
  79)
