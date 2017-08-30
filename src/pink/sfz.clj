(ns pink.sfz
  (:require [instaparse.core :refer [defparser]]
            [score.freq :refer [str->notenum
                                cents->scaler 
                                hertz]]
            [pink.sfz.sound-file :refer :all]
            [pink.util :refer :all]
            [pink.simple]
            )
  (:import [java.io File]
           [java.io RandomAccessFile]
           [java.nio MappedByteBuffer ByteOrder]
           [java.nio.channels FileChannel 
            FileChannel$MapMode]
           ))


;; TODO - sfz allows spaces in filesnames for samples.
;; This parser code works if the sample is defined on its own line
(defparser parse-sfz
    "<sfz> = <ws-or-comment*> section* <ws-or-comment*> 
    section = <'<'> #'[^\\s>]+' <'>'> <ws-or-comment*> opcode*
    opcode = (word <'='> (spaced-word | word)) <ws-or-comment*>
    <word> = #'[^\\s=\\n\\r]+'
    <spaced-word> = #'[^=\\r\\n]+' <#'[\\r\\n|\\n]'>
    ws-or-comment = ws | comment
    ws = #'\\s+'
    comment = #'//[^\\r\\n]*[\\r\\n|\\n]?'")

(defn- opcodes->map
  [oplist]
  (reduce
    (fn [a [_ x y]]
      (assoc a (keyword x) (clojure.string/trim y))) 
    {}
    oplist))

(defmulti handle-section (fn [a b] (keyword (second b))))

(defmethod handle-section :control [sfz-state section]
  (let [params (opcodes->map (rest (rest section)))] 
    (update sfz-state :control merge params)))

(defmethod handle-section :global
  [sfz-state section]
  (let [params (opcodes->map (rest (rest section)))] 
    (update sfz-state :global merge params)))

(defmethod handle-section :group
  [sfz-state section]
  (update sfz-state :groups
          conj {:params (opcodes->map (rest (rest section)))
                :regions [] }))

(defn get-notenum [str-val]
  (try 
    (Long/valueOf str-val)
    (catch Exception e
      (str->notenum str-val))))

(defn setup-defaults-for-region
  [sfz-file pre-map]
  (let [sfz-dir (.getParent (File. sfz-file))
        region-sample-file (str sfz-dir File/separator (:sample pre-map))
        wav-data (get-wav-data region-sample-file)
        k (:key pre-map)
        start (if k 
                (let [kdefault (get-notenum k)]
                  {:lokey kdefault
                   :hikey kdefault
                   :pitch_keycenter kdefault
                   } )
                {}) 
        init (assoc start :wav-data wav-data)]
      (if-let [loop-data (get-in wav-data ["smpl" :sample-loops 0])]
        (-> 
          init 
          (assoc :loop_start (:start loop-data))
          (assoc :loop_end (:end loop-data)))
        init ) ))

(defn process-region
  [init-map rest-map]
  (reduce
    (fn [a [k v]]
      (case k
        :lokey (assoc a :lokey (get-notenum v)) 
        :hikey (assoc a :hikey (get-notenum v)) 
        :pitch_keycenter (assoc a :pitch_keycenter (get-notenum v)) 
        :tune (assoc a :tune (cents->scaler (Long/valueOf v)))
        (do 
          (println "Unsupported opcode found [" k "] skipping...")
          a)
        )) 
    init-map rest-map))

(defn add-region-analysis
  "Converts keys to midi note numbers (long), and..."
  [sfz-file region-map]
  (let [
        split-keys [:key :sample]
        pre-map (select-keys region-map split-keys)
        post-map (apply dissoc region-map split-keys) ]
    (process-region (setup-defaults-for-region sfz-file pre-map)
                    post-map)
    ))

(defmethod handle-section :region
  [sfz-state section]
  (let [indx (dec (count (sfz-state :groups)))]
    (if (neg? indx)
      (throw (Exception. "No group defined before region"))
      (->>
        (opcodes->map (rest (rest section)))
        (add-region-analysis (:sfz-file sfz-state))
        (update-in sfz-state [:groups indx :regions] conj)
        ))))

(defmethod handle-section :default 
  [sfz-state section]
  (throw (Exception. "Unknown section found: " (second section))))

(defn transform-parse-tree
  [sfz-file sfz]
  (reduce handle-section
   {:control {}
    :global {}
    :groups []
    :sfz-file sfz-file } 
   sfz
   ))


;; 

(defn load-sfz
  [sfz-file]
  (let [sfz-txt (slurp sfz-file)
        sfz-parse (parse-sfz sfz-txt) 
        sfz (transform-parse-tree sfz-file sfz-parse)
        ]
    sfz
    ))

(defn sfz-lookup 
  "Finds regions to play within sfz based upon group, midi-key, and midi-vel."
  [sfz-data group-num midi-key midi-vel]
  (let [regions (get-in sfz-data [:groups group-num :regions])]
    (filter #(and (>= midi-key (:lokey %)) 
                  (<= midi-key (:hikey %))) 
            regions)))

;; PLAYBACK 

(defn vel->amp
  ([midi-vel] (vel->amp midi-vel 20))
  ([midi-vel db-range]
   (let [r (Math/pow 10.0 (/ db-range 20.0))
         b (- (/ 127. (* 126.0 (Math/sqrt r))) 
              (/ 1.0 126.0))
         m (/ (- 1.0 b) 127.0)
         ]
     (Math/pow (+ (* m midi-vel ) b) 2))))

(defn calc-read-incr
  [region-data midi-key]
  ;; TODO - handle different SR's here?
  (let [key-center (:pitch_keycenter region-data)
        tuning (get region-data :tune 1.0)]
    (* (/ (hertz midi-key) (hertz key-center))
       tuning)))

(defn region-player 
  "Audio function for playing SFZ soundfile."
  [regions midi-key midi-vel]
  (let [^doubles outl (create-buffer)
        ^doubles outr (create-buffer)
        out (into-array [outl outr])
        wav-data (:wav-data regions) 
        num-channels (long (get-in wav-data ["fmt " :num-channels]))
        data-start (get-in wav-data ["DATA" :data-start])
        raf (RandomAccessFile. (:wav-file-name wav-data) "r") 
        channel (.getChannel raf)
        start-ptr 0.0 
        map-size (- (.size channel) data-start)
        mapped-byte-buffer
        (.map channel
              FileChannel$MapMode/READ_ONLY
              data-start
              map-size)
        ;; hardcode to stereo short for now
        short-len (long (/ map-size (* num-channels 2)))
        amp (vel->amp midi-vel) 
        read-incr (calc-read-incr regions midi-key)      
        buf-limit (- short-len (* 2 num-channels))
        ]
    ;;(println (.size channel) short-len wav-data)
    (.order mapped-byte-buffer ByteOrder/LITTLE_ENDIAN) 
    (if (= 2 num-channels)
      (generator
        [read-ptr start-ptr] [] 
        (if (and (= 0 int-indx) (>= read-ptr short-len)) 
          (do 
            (.close channel)
            (.close raf)
            nil)
          (if (< read-ptr buf-limit) 
            (let [start (int read-ptr)
                  frac (if (zero? start) 0.0 (rem read-ptr start))
                  _ (.position mapped-byte-buffer (* start 4))
                  left0 (.getShort mapped-byte-buffer)
                  right0 (.getShort mapped-byte-buffer)
                  left1 (.getShort mapped-byte-buffer)
                  right1 (.getShort mapped-byte-buffer)
                  left (+ left0 (* frac (- left1 left0)))
                  right (+ right0 (* frac (- right1 right0)))
                  ]

              (aset outl int-indx (* amp (/ left 32768.0)))
              (aset outr int-indx (* amp (/ right 32768.0)))
              (gen-recur (+ read-ptr read-incr)))
            (do 
              (aset outl int-indx 0.0)
              (aset outr int-indx 0.0)
              (gen-recur (+ read-ptr read-incr)))
            ))
        (yield out))
      (generator
        [read-ptr start-ptr] [] 
        (if (and (= 0 int-indx) (>= read-ptr short-len)) 
          (do 
            (.close channel)
            (.close raf)
            nil)
          (if (< read-ptr buf-limit) 
            (let [start (int read-ptr)
                  frac (if (zero? start) 0.0 (rem read-ptr start))
                  _ (.position mapped-byte-buffer (* start 2))
                  left0 (.getShort mapped-byte-buffer)
                  left1 (.getShort mapped-byte-buffer)
                  left (+ left0 (* frac (- left1 left0)))
                  ]

              (aset outl int-indx (* amp (/ left 32768.0)))
              (aset outr int-indx (* amp (/ left 32768.0)))
              (gen-recur (+ read-ptr read-incr)))
            (do 
              (aset outl int-indx 0.0)
              (aset outr int-indx 0.0)
              (gen-recur (+ read-ptr read-incr)))
            ))
        (yield out))
      )))


(defn play-sfz 
  [sfz-data group midi-key midi-vel]
  (let [regions (sfz-lookup sfz-data group midi-key midi-vel)]
    (pink.simple/add-afunc (region-player (first regions) midi-key midi-vel))
    ))


;; DEV TEST


(def test-sfz " // group\n<group> <region> filter=4\n b=3\r\n sample=../abc/def.wav
               <region> filter=4 b=3 sample=../abc/fgh.wav")

;; http://virtualplaying.com/virtual-playing-orchestra/ 
(def VPO2-root
  (str (System/getenv "PINK_RESOURCE_ROOT") 
  "/sfz/Virtual-Playing-Orchestra2"))

(def test-sfz-file
  (str VPO2-root
       "/Strings/1st-violin-SEC-sustain.sfz"))

#_(transform-parse-tree (parse-sfz test-sfz))

(def sfz1 
  (load-sfz 
    (str VPO2-root "/Strings/1st-violin-SEC-sustain.sfz")))

(def sfz2
  (load-sfz 
    (str VPO2-root "/Brass/trumpet-SOLO-sustain.sfz")))

#_(clojure.pprint/pprint (parse-sfz (slurp test-sfz-file)))
#_(clojure.pprint/pprint sfz-data)


;; Process
;; 1. Lookup regions to play based upon MIDI key (allow fractional?) and MIDI velocity (again, fractional?)
;; 2. setup playback depending upon settings from global, group, and region
;; 3. Playback


;; TODOS
;; * get frequency from midi-key
;; * implement phasor for freq, figure out what interpolation is used by other SFZ players
;; * get amp for midi-vel and multiply
;; 



(comment

  (pink.simple/start-engine)
  (play-sfz sfz1 0 60 100)
  (play-sfz sfz1 0 67 100)
  (play-sfz sfz1 0 72 100)
  (play-sfz sfz1 0 77 100)
  (play-sfz sfz1 0 80 100)
  (play-sfz sfz1 0 82 100)

  (play-sfz sfz2 0 72 64)
  (play-sfz sfz2 0 64 100)
  (play-sfz sfz2 0 60 100)


  )

