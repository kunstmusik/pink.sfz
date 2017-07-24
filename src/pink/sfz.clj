(ns pink.sfz
  (:require [instaparse.core :refer [defparser]])
  )

(defparser parse-sfz
    "sfz = <ws-or-comment?> section* <ws-or-comment?> 
    section = #'<[^\\s>]+>' <ws-or-comment?> opcode*
    opcode = (word <'='> word) <ws-or-comment?>
    <word> = #'[^\\s=\\n\\r]+'
    ws-or-comment = ws | comment
    ws = #'\\s+'
    comment = #'//[^\\r\\n]*[\\r\\n|\\n]?'"
    )

(parse-sfz "// group\n<group> <region> filter=4 b=3 sample=../abc/def.wav // ")
