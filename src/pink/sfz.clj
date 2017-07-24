(ns pink.sfz
  (:require [instaparse.core :refer [defparser]]))

(defparser parse-sfz
    "<sfz> = <ws-or-comment?> section* <ws-or-comment?> 
    section = <'<'> #'[^\\s>]+' <'>'> <ws-or-comment?> opcode*
    opcode = (word <'='> word) <ws-or-comment?>
    <word> = #'[^\\s=\\n\\r]+'
    ws-or-comment = ws | comment
    ws = #'\\s+'
    comment = #'//[^\\r\\n]*[\\r\\n|\\n]?'")

(defn- opcodes->map
  [oplist]
  (reduce
    (fn [a [_ x y]]
      (assoc a (keyword x) y)) 
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

(def test-sfz "// group\n<group> <region> filter=4 b=3 sample=../abc/def.wav
               <region> filter=4 b=3 sample=../abc/fgh.wav")

(let [sfz-state {}
      sfz (parse-sfz test-sfz)]
  (transform-parse-tree sfz ))
