(ns cb-proto.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.fs :as file]
            [cljs.core.async :as async :refer [put! chan <!]]
            [clojure.data :as cd]
            [cb-proto.data :as data]))

(enable-console-print!)

;; Functions for parsing raw chromosome browser data from FTDNA
(def dnakeys [:name :match :chromosome :start :end :length :snp])

(defn parse
  [string]
  (map #(clojure.string/split % #"\"")
       (clojure.string/split string #"\r?\n|\r")))

(defn vectorizeFile [file]
  (into [] (seq (rest (parse file)))))

(defn parse_numbers
  [s]
  (into [] (seq (rest (clojure.string/split s #",")))))

(defn parse_array
  [arr]
  (let [myvector (into [] (seq (flatten (vector (get arr 1) (get arr 3) (parse_numbers (get arr 4))))))]
    (zipmap dnakeys myvector)))

(defn first-parse [file]
  (map #(parse_array %) (vectorizeFile file)))

(defn keyify [s]
  (keyword (clojure.string/replace s #"\s+|,|/" "_")))

(defn add-match [kit chromosome match kits]
  (let [matches (get-in kits [kit chromosome])]
    (if matches
      (conj matches match)
      [match])))

(defn add-matches
  ([matches]
   (add-matches matches {}))
  ([matches kits]
   (if (empty? matches)
     kits
     (recur (rest matches)
           (let [match (first matches)
                 name (keyify (:name match))
                 chromosome (keyword (:chromosome match))
                 new-kits (assoc-in kits [name :username] (:name match))]
             (assoc-in new-kits [name chromosome] (add-match name chromosome match new-kits)))))))

(defn mykits [file]
  (add-matches (first-parse file)))


;; --------------- AppData ------------------
(def app-data
  (assoc {}
    :kits      (mykits data/demofile)
    :settings {
               :selectedChromosome "1"
               :selectedKitA       ""
               :selectedKitB       ""
               :selectedKitC       ""
               :selectedFilter     "15"
               :displaymode         ":1"
               }
    :browser {
              :width              1600
              :height             650
              :chromosome-xoffset 40
              :chromosome-yoffset 40
              :chromosome-xpadding 10 ;; Border in x-direction Chromobackground vs. matchdata
              :chromosome-padding 6   ;; Padding between chromosomes of different kits in comparison
              }))

(defonce app-state (atom app-data))

(defn state []
  (om/ref-cursor (om/root-cursor app-state)))

(defn kits []
  ;; Defines reference cursor to kits in appstate
  (om/ref-cursor (:kits (om/root-cursor app-state))))

(defn settings []
  ;; Defines reference cursor to settings in appstate
  (om/ref-cursor (:settings (om/root-cursor app-state))))

(defn browser []
  ;; Defines reference cursor to browserdims in appstate
  (om/ref-cursor (:browser (om/root-cursor app-state))))
;; -------------------------------------------

(defn data-start []
  (let [browser (browser)]
    (+ (:chromosome-xoffset browser) (:chromosome-xpadding browser))))

(defn data-end []
  (let [browser (browser)]
    (- (+ (:chromosome-xoffset browser) (:width browser)) (:chromosome-xpadding browser) )))

(defn data-scale []
  ;; Scales given chromosome to screensize
  (let [settings (settings)]
    (/ (- (data-end) (data-start)) (get data/chromolength36 (- (js/parseInt (:selectedChromosome settings)) 1)))))

(defn scale-to-browser [data]
  (* (js/parseInt data) (data-scale)))

(defn browser-offset []
  (let [browser (browser)]
    (+ (:chromosome-xoffset browser) (:chromosome-xpadding browser))))

(defn match-xpos [data]
  (+ (scale-to-browser data) (browser-offset)))

(defn match-length [start end]
  (- (scale-to-browser end) (scale-to-browser start)))

(defn kitnames
  ;; Creates a vector of unique user names in kits
  ([]
   (kitnames (kits) #{}))
  ([mykits myset]
   (if (empty? mykits)
     (vec myset)
     (recur (rest mykits) (conj myset (:username (get (first mykits) 1)))))))

(defn chromoheight [rows]
  (let [b (browser)
        height (:height b)
        padding (:chromosome-padding b)]
    (/ (- height (* padding (- rows 1))) rows)))

(defn chromo-ypos [row rows]
  (let [yoffset (:chromosome-yoffset (browser))
        padding (:chromosome-padding (browser))
        rowheight (chromoheight rows)]
    (+ yoffset (* (+ rowheight padding) row))))

(defn putSetting
  [e key]
  (let [selection (.. e -target -value)
        settings (settings)]
    (om/transact! settings key (fn [_] selection))))

(defn getMatches [kitselection]
  ;; filters matches by length and sorts by startposition
  (let [kits (kits)
        settings (settings)
        limit (:selectedFilter settings)
        matches (get-in kits [(keyify (kitselection settings)) (keyword (:selectedChromosome settings))])]
    (sort-by #(js/parseInt (:start %)) (filter #(>= (js/parseInt (:length %)) (js/parseInt limit)) matches))))

(defn getMatchNames [data]
  (into #{} (map #(:match %)) data))

(defn findInCommon [setA setB]
  (let [a (getMatchNames setA)
        b (getMatchNames setB)]
    (last (cd/diff a b))))

(defn filterMatches
  ([matches filterset inCommon?]
   (filterMatches matches filterset [] inCommon?))
  ([matches filterset newmap inCommon?]
   (if (empty? matches)
     newmap
     (let [match (first matches)]
       (if (= (contains? filterset (:match match)) inCommon?)
         (recur (rest matches) filterset (conj newmap match) inCommon?)
         (recur (rest matches) filterset newmap inCommon?))))))

(defn check-row [sortarray match]
  (loop [index 0]
    (let [x (get sortarray index)
          end (js/parseInt (:end x))
          start (js/parseInt (:start match))]
      (if (or (empty? x) (<= end start))
        index
        (recur (inc index))))))

(defn arrange-by-row
  ([data]
    (arrange-by-row [] data []))
  ([sortarray matches newmap]
   (if (empty? matches)
     {:matches newmap :rows (count sortarray)}
     (let [currentMatch (first matches)
           index (check-row sortarray currentMatch)]
       (recur (assoc sortarray index currentMatch) (rest matches) (conj newmap (assoc currentMatch :row index)))))))

(defn row-height [num-of-rows chromorows]
  (/ (chromoheight chromorows) num-of-rows))

(defn match-ypos [num-of-rows row chromorow chromorows]
  (+ (chromo-ypos chromorow chromorows) (* (row-height num-of-rows chromorows) row)))

(defn matchblockstyle [{:keys [xpos width rows row chromorow chromorows]}]
  (let [ypos (- (match-ypos rows row chromorow chromorows) 0) ;; adjusted for border pixels
        height (- (row-height rows chromorows) 2)] ;; adjusted for border pixels
    #js {:position     "absolute" :left xpos :width width :backgroundColor "#ccffcc"
         :height height :top ypos
         :border       "1px solid black"
         :fontFamily "Arial" :fontSize "small" :overflow "hidden"
         :borderRadius "5px" :padding "0px" :margin "0px"
         :userSelect "none" :MozUserSelect "none" :WebkitUserSelect "none" :msUserSelect "none"}))

(defn matchblock [match owner {:keys [rows chromorow chromorows] :as opts}]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:style (matchblockstyle {:xpos (match-xpos (:start match))
                                             :width (match-length (:start match) (:end match))
                                             :rows rows
                                             :row (:row match)
                                             :chromorow chromorow
                                             :chromorows chromorows})} (:match match)))))

(defn matches-display [keys owner]
  (reify
    om/IRender
    (render [_]
      (let [matchdata (arrange-by-row (:kit keys))
            matches (:matches matchdata)
            rows (:rows matchdata)]
        (dom/div nil
                 (om/build-all matchblock matches {:opts {:rows rows
                                                          :chromorow (:row keys)
                                                          :chromorows (:rows keys)}}))))))

(defn chromostyle
  [row rows]
  (let [b (browser)
        width (:width b)
        chromosome-xoffset (:chromosome-xoffset b)
        rowheight (chromoheight rows)
        fontSize (str (- rowheight 20) "px")
        row-top (chromo-ypos row rows)]
    #js {:position     "absolute" :top row-top :left chromosome-xoffset :width width
         :height       rowheight :backgroundColor "#24478f" :color "#2e5bb7"
         :borderRadius "10px" :padding "0px" :margin "0px" :fontSize fontSize
         :userSelect "none" :MozUserSelect "none" :WebkitUserSelect "none" :msUserSelect "none"}))

(defn select_option
  [option owner]
  (reify
    om/IRender
    (render [_]
      (dom/option #js {:value option} option))))

(defn select-matchmode
  [[k v] owner]
  (reify
    om/IRender
    (render [_]
      (dom/option #js {:value k} v))))

(defn chromobackground [row rows]
  (dom/div #js {:style (chromostyle row rows)} (:selectedChromosome (settings))))

(defn display-option-1 [] ;; Displays single kit
  (let [data (getMatches :selectedKitA)]
    (dom/div nil
             (chromobackground 0 1)
             (om/build matches-display {:kit data :row 0 :rows 1}))))

(defn display-option-2 [] ;; Displays two kits with matches in common
  (let [rownums 2
        kitA (getMatches :selectedKitA)
        kitB (getMatches :selectedKitB)
        incommon (findInCommon kitA kitB)
        filterA (filterMatches kitA incommon true)
        filterB (filterMatches kitB incommon true)]
    (dom/div nil
             (apply dom/div nil
                    (map #(chromobackground % rownums) [0 1]))
             (apply dom/div nil
                    (om/build-all matches-display [{:kit filterA :row 0 :rows rownums}
                                                   {:kit filterB :row 1 :rows rownums}])))))

(defn display-option-3 []
  (let [rownums 4
        kitA (getMatches :selectedKitA)
        kitB (getMatches :selectedKitB)
        incommon (findInCommon kitA kitB)
        filter0 (filterMatches kitA incommon false)
        filter1 (filterMatches kitA incommon true)
        filter2 (filterMatches kitB incommon true)
        filter3 (filterMatches kitB incommon false)]
    (dom/div nil
             (apply dom/div nil
                    (map #(chromobackground % rownums) [0 1 2 3]))
             (apply dom/div nil
                    (om/build-all matches-display [{:kit filter0 :row 0 :rows rownums}
                                                   {:kit filter1 :row 1 :rows rownums}
                                                   {:kit filter2 :row 2 :rows rownums}
                                                   {:kit filter3 :row 3 :rows rownums}])))))

(defn display-option-4 []
  (let [rownums 4
        kitA (getMatches :selectedKitA)
        kitB (getMatches :selectedKitB)
        kitC (getMatches :selectedKitC)
        incommon1 (findInCommon kitA kitB)
        incommon2 (findInCommon kitB kitC)
        filter0 (filterMatches kitA incommon1 true)
        filter1 (filterMatches kitB incommon1 true)
        filter2 (filterMatches kitB incommon2 true)
        filter3 (filterMatches kitC incommon2 false)]
    (dom/div nil
             (apply dom/div nil
                    (map #(chromobackground % rownums) [0 1 2 3]))
             (apply dom/div nil
                    (om/build-all matches-display [{:kit filter0 :row 0 :rows rownums}
                                                   {:kit filter1 :row 1 :rows rownums}
                                                   {:kit filter2 :row 2 :rows rownums}
                                                   {:kit filter3 :row 3 :rows rownums}])))))

(defn matchDisplay
  [_ owner]
  (reify
    om/IRender
    (render [_]
      (let [settings (om/observe owner (settings))
            browser (om/observe owner (browser))
            displaymode (:displaymode settings)]
        (cond
          (= displaymode ":1") (display-option-1)
          (= displaymode ":2") (display-option-2)
          (= displaymode ":3") (display-option-3)
          (= displaymode ":4") (display-option-4)
          :else (dom/div nil))))))

(defn importFTDNACBdata [string]
  (let [kits (kits)
        state (state)
        data (mykits string)
        newmap (merge kits data)]
    (om/transact! state :kits (fn [_] newmap))))

(defn readfile [e]
  (let [f (.. e -target -files)
        myfile (js/FileReader. )]
    (set! (.-onload myfile) (fn [e]
                              (importFTDNACBdata (.-result myfile))))
    (.readAsText myfile (aget f 0))))



(defn chromosomeDisplay
  [_ owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
               (dom/input #js {:type "file" :multipe "" :onChange #(readfile %)})
               (apply dom/select #js {:onChange #(putSetting % :selectedChromosome)}
                      (om/build-all select_option [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 "X"]))
               (apply dom/select #js {:onChange #(putSetting % :selectedFilter)}
                      (om/build-all select_option [1 3 5 10]))
               (apply dom/select #js {:onChange #(putSetting % :displaymode)}
                      (om/build-all select-matchmode {:1 "Single kit"
                                                      :2 "Compare two, only in common"
                                                      :3 "Compare two kits"
                                                      :4 "Compare three kits, in common"}))
               (apply dom/select #js {:onChange #(putSetting % :selectedKitA)}
                      (om/build-all select_option (kitnames)))
               (apply dom/select #js {:onChange #(putSetting % :selectedKitB)}
                      (om/build-all select_option (kitnames)))
               (apply dom/select #js {:onChange #(putSetting % :selectedKitC)}
                      (om/build-all select_option (kitnames)))
               (om/build matchDisplay 0)))))

(om/root chromosomeDisplay app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
