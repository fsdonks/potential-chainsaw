(ns btest
  (:require [clojure.java.io :as io])
  (:import [java.util Base64]))

(defn file->b64 [in tgt]
  (let [f   (io/file in)
        fis (io/input-stream f)
        bs  (byte-array (.length f))
        _   (.read fis bs)
        enc (Base64/getEncoder)]
    (spit tgt (.encodeToString enc bs))))

(defn b64->file [in tgt]
  (let [d  (Base64/getDecoder)
        bs (.decode d (slurp in))
        newfile  (io/file tgt)]
    (with-open [fos (io/output-stream newfile)]
      (.write fos bs))))
