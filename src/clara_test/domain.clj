(ns clara-test.domain)

(defrecord OS [family maj min patch])

(defrecord Method [name])

(defrecord Maven [maj min patch])

(defrecord SourceURL [url-type url])

(defrecord MavenURLTemplates [download md5])
