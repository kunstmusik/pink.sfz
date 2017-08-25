(ns pink.sfz.sound-file
  (:import [java.io FileOutputStream File BufferedOutputStream
            DataOutputStream RandomAccessFile]
           [java.nio MappedByteBuffer ByteOrder]
           [java.nio.channels FileChannel 
            FileChannel$MapMode]))

(defn get-string 
  [^MappedByteBuffer buffer ^long num-chars]
  (let [barray (byte-array 4)]
    (loop [i 0]
      (when (< i num-chars)
        (aset barray i (.get buffer))
        (recur (inc i))))
    (String. barray)))

(defmulti read-chunk 
  (fn [mapped-byte-buffer] (get-string mapped-byte-buffer 4)))

(defmethod read-chunk "RIFF"
  [mapped-byte-buffer]
  (let [file-size (.getInt mapped-byte-buffer) 
        wave (get-string mapped-byte-buffer 4)]

    (when (not= "WAVE" wave) 
      (throw 
        (Exception. "Error: Not a WAV file")))
    {:chunk "RIFF"
     :header-file-size file-size}))

(defmethod read-chunk "fmt "
  [mapped-byte-buffer]
  (let [chunk-size (.getInt mapped-byte-buffer)
        extra-data (- chunk-size 16)
        comp-code (.getShort mapped-byte-buffer)  
        num-channels (.getShort mapped-byte-buffer)
        sample-rate (.getInt mapped-byte-buffer)
        bytes-per-second (.getInt mapped-byte-buffer)
        block-align (.getShort mapped-byte-buffer)
        bits-per-sample (.getShort mapped-byte-buffer)
        extra-vals (when (pos? extra-data)
                    (let [barray (byte-array extra-data)]
                      (.get mapped-byte-buffer barray) 
                      barray))
        ]  
    {
     :chunk "fmt " 
     :chunk-size  chunk-size
     :compression-code comp-code 
     :num-channels  num-channels 
     :sample-rate sample-rate 
     :bytes-per-second bytes-per-second 
     :block-align block-align 
     :bits-per-sample bits-per-sample 
     :extra-vals extra-vals 
     }))

(defmethod read-chunk "data"
  [mapped-byte-buffer]
  (let [chunk-size (.getInt mapped-byte-buffer)
        data-start (.position mapped-byte-buffer)]  
    (println "data: " data-start " " chunk-size)
    (.position mapped-byte-buffer (+ data-start chunk-size))
    {
     :chunk "DATA" 
     :chunk-size  chunk-size
     :data-start data-start 
     }))

(defn get-sample-loops
  [mapped-byte-buffer num-loops]
  (loop [ret [] i 0]
    (if (< i num-loops)
      (let [cue-point-id (.getInt mapped-byte-buffer)
            cue-type (.getInt mapped-byte-buffer)
  cue-start (.getInt mapped-byte-buffer)
  cue-end (.getInt mapped-byte-buffer)
  cue-fraction (.getInt mapped-byte-buffer)
  play-count (.getInt mapped-byte-buffer)

            ]
        (recur 
          (conj ret
                {:cue-point-id cue-point-id
                 :type cue-type
                 :start cue-start
                 :end cue-end
                 :fraction cue-fraction
                 :play-count play-count
                 })
          (inc i)
          )) 
      ret
      )))

(defmethod read-chunk "smpl"
  [mapped-byte-buffer]
  (let [chunk-size (.getInt mapped-byte-buffer)
        extra-size (- chunk-size 36)
        manufacturer (.getInt mapped-byte-buffer)
        product (.getInt mapped-byte-buffer)
        sample-period (.getInt mapped-byte-buffer)
        midi-unity-note (.getInt mapped-byte-buffer)
        midi-pitch-frac (.getInt mapped-byte-buffer)
        smpte-format (.getInt mapped-byte-buffer)
        smpte-offset (.getInt mapped-byte-buffer)
        num-samp-loops (.getInt mapped-byte-buffer)
        sampler-data (.getInt mapped-byte-buffer)
        sample-loops (get-sample-loops mapped-byte-buffer num-samp-loops)]  
    (.position mapped-byte-buffer (+ (.position mapped-byte-buffer) sampler-data))
    {
     :chunk "smpl" 
     :chunk-size  chunk-size
     :manufacturer      manufacturer          
     :product           product 
     :sample-period     sample-period 
     :midi-unity-note   midi-unity-note
     :midi-pitch-frac   midi-pitch-frac
     :smpte-format      smpte-format 
     :smpte-offset      smpte-offset 
     :num-samp-loops    num-samp-loops 
     :sampler-data-size      sampler-data   
     :sample-loops sample-loops
     }))

(defmethod read-chunk :default
  [mapped-byte-buffer]
  (.position mapped-byte-buffer (- (.position mapped-byte-buffer) 4))
  (let [chunk-name (get-string mapped-byte-buffer 4)
        chunk-size (.getInt mapped-byte-buffer)]  
    (.position mapped-byte-buffer (+ (.position mapped-byte-buffer)
                                     chunk-size ))
    {:chunk chunk-name
     :chunk-size chunk-size
     :skipped true
     }))

(defn get-wav-data
  [wav-file-name]
  (let [raf (RandomAccessFile. 
              ^String wav-file-name "r")

        channel (.getChannel raf)
        channel-size (.size channel)
        mapped-byte-buffer
        (.map channel
              FileChannel$MapMode/READ_ONLY
              0 
              channel-size)]
    (.order mapped-byte-buffer ByteOrder/LITTLE_ENDIAN) 

    (loop [ret {:wav-file-name wav-file-name}]
      (if (< (.position mapped-byte-buffer) channel-size)
        (if-let [v (read-chunk mapped-byte-buffer)]
          (recur (assoc ret (:chunk v) v)) 
          ret
          )
        ret
        ))))

(def test-file
  "C:\\Users\\stevenyi\\work\\audio\\sfz\\Virtual-Playing-Orchestra2\\libs\\SSO\\Samples\\1st Violins\\1st-violins-sus-a#3.wav"
  )

#_(get-wav-data test-file)
