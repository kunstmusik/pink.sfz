(ns pink.sfz
  (:require [instaparse.core :refer [defparser]]))


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
          conj {:params {}
                :regions [] }))

(defmethod handle-section :region
  [sfz-state section]
  (let [indx (dec (count (sfz-state :groups)))]
    (if (neg? indx)
      (throw (Exception. "No group defined before region"))
      (let [opmap (opcodes->map (rest (rest section)))]
        (update-in sfz-state [:groups indx :regions] conj opmap)))))

(defmethod handle-section :default 
  [sfz-state section]
  (throw (Exception. "Unknown section found: " (second section))))

(defn transform-parse-tree
  [sfz]
  (reduce handle-section
   {:control {}
    :global {}
    :groups []} 
   sfz
   ))


;; 

(defn load-sfz
  [sfz-file]
  (let [sfz-txt (slurp sfz-file)
        sfz-parse (parse-sfz sfz-txt)
        sfz (transform-parse-tree sfz-parse)
        ]
    sfz
    ))

(def test-sfz " // group\n<group> <region> filter=4\n b=3\r\n sample=../abc/def.wav
               <region> filter=4 b=3 sample=../abc/fgh.wav")

;; http://virtualplaying.com/virtual-playing-orchestra/ 
(def VPO2-root
  (str (System/getenv "PINK_RESOURCE_ROOT") 
  "/sfz/Virtual-Playing-Orchestra2"))

(def test-sfz-file
  (str VPO2-root
       "/Strings/1st-violin-SEC-sustain.sfz"))

(transform-parse-tree (parse-sfz test-sfz))

(clojure.pprint/pprint (load-sfz test-sfz-file))
