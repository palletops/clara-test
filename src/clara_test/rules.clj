(ns clara-test.rules
  (:refer-clojure :exclude [==])
  (:require [clara.rules.accumulators :as acc]
            [clara.rules :refer :all]
            [schema.core :as s]))

(def Fact
  {(s/required-key :type)  s/Keyword
   s/Keyword s/Keyword})

(def Os
  ;;  {:type :os :family :ubuntu :maj "10" :min "04" :patch nil}
  (assoc Fact
    (s/required-key :family) s/Keyword
    (s/required-key :maj) s/String
    (s/required-key :min) s/String
    :patch (s/maybe s/String)))

(defn print-url!
  [s]
  (doseq [mvn (query s 'get-url)] (println "URL:" (:?url mvn))) s)

(defn print-chosen-strategies!
  [s]
  (doseq [strategy (query s 'get-chosen-strategy)]
    (println "CHOSEN STRATEGY:" (:?strategy strategy)))
  s)

(defn prefer-fn [pref-seq]
  (fn [avail]
    (let [chosen
          (first (filterv (set avail) pref-seq))]
      (println (format "chose: %s from: %s" chosen avail)))))

(defn prefer-reducer [pref-seq select-fn ]
  (let [index
        ;; fn to get the index of the first occurence of value in a sequence
        (fn [val coll]
          (or
           (first (keep-indexed #(when (= val %2) %1) coll))
           Integer/MAX_VALUE ;; not found
           ))]
    (fn [a b]
      (let [v-a (when a (select-fn a)) ;; value of the selector
            v-b (when b (select-fn b)) ;; value of the selector
            r (if (nil? v-a)
                (if (nil? v-b) nil b) ;; deal with nil vals
                ;; if they're not nil, then select the one with higher
                ;; preference (lower index value)
                (let [index-a (index v-a pref-seq)
                      index-b (index v-b pref-seq)]
                  (if (< index-a index-b) a b)))]
        r))))

(defn prefer
  "returns an accumulator that given a sequence of values by
  preference and a set of available values, it returns the most
  preferred of the available values"
  [pref-seq select-fn]
  (accumulate
   :reduce-fn (prefer-reducer pref-seq select-fn)))

(defmacro os-family-rule [sym-name fam fam-set]
  `(make-rule
   ~sym-name
   [:os-family [{~'family :family}] (~fam-set ~'family)]
   ~'=>
   (insert! {:type :os-family :family ~fam})))

(def os-rules
  [(os-family-rule 'identify-linux :linux
    #{:rh-base :debian-base :arch-base :suse-base :bsd-base :gentoo-base})
   (os-family-rule 'identify-rh-base :rh-base #{:centos :rhel :amzn-linux :fedora})
   (os-family-rule 'identify-debian-base :debian-base #{:debian :ubuntu :jeos})
   (os-family-rule 'identify-suse-base :suse-base #{:suse})
   (os-family-rule 'identify-arch-base :arch-base #{:arch})
   (os-family-rule 'identify-gentoo-base :gentoo-base #{:gentoo})
   (os-family-rule 'identify-bsd-base :bsd-base #{:darwin :os-x})

   (make-rule 'add-the-declared-os-family
              [?os <- :os]
              =>
              (insert! {:type :os-family :family (:family ?os)}))
   (make-query 'get-os-family []
               [?os-family <- :os-family])])

(defn print-os-family! [s]
  (doseq [os-family (query s 'get-os-family)]
    (println "os-family:" (:family (:?os-family os-family)))) s)

(def rules
  [(make-rule
    'test-url
    "generates the maven urls"
    [?mvn <- :maven]
    [?template <-  :maven-url-templates]
    =>
    (let [ version  (clojure.string/join "." [ (:maj ?mvn) (:min ?mvn) (:patch ?mvn)])]
      (do (insert! {:type :archive-url
                    :url (format (:download ?template) version version version)})
          (insert! {:type :archive-md5-url
                    :url (format (:md5 ?template) version version version)}))))
   (make-rule
    'preferred-strategy
    "When different strategies are available, choose the preferred one"
    [?chosen <- (prefer [:archive :package] :install-strategy) :from [:strategy]]
    [?chosen-strategy <- :chosen-strategy]
    =>
    (do
      #_(retract! ?chosen-strategy)
      (insert! {:type :chosen-strategy :install-strategy ?chosen})))
   (make-query
    'get-url []
    [?url <- :source-url])
   (make-query
    'get-chosen-strategy []
    [?strategy <- :chosen-strategy]
    ;;[?preferred-strategies <- :preferred-strategy]
    ;;[?strategy <- (prefer ?preferred-strategies :install-strategy)
    ;:from [:strategy]]
    )
   (make-rule
    'maven3-on-debian
    [:os-family [{family :family}] (= family :debian)]
    [:maven [{maj :maj}] (= maj 3)]
    =>
    (insert! {:type :strategy
              :install-strategy :package-source
              :repository {:id :debian-backports}
              :packages ["maven3"]}))
   (make-rule
    'maven3-on-ubuntu
    [:os-family [{family :family}] (= family :ubuntu)]
    [:maven [{maj :maj}] (= maj 3)]
    =>
    (insert! {:type :strategy
              :install-strategy :package-source
              :package-source {:apt {:url "ppa:natecarlson/maven3"}
                               :name "maven3"}
              :packages ["maven3"]
              :link ["/usr/bin/mvn3" "/usr/bin/mvn"]}))
   (make-rule 'install-dir
              [?mvn <- :maven]
              =>
              (insert! {:type :install-dir
                        :dir (format "/opt/maven%s" (:maj ?mvn))}))
   (make-rule
    'maven-from-archive
    [:os-family [{family :family}] (= family :linux)]
    [?mvn <- :maven]
    [?archive-url <- :archive-url]
    [?md5-url <- :archive-md5-url]
    [?install-dir <- :install-dir]
    =>
    (let [{:keys [ maj min p]} ?mvn
          version (apply str (clojure.string/join "." [maj min p]))
          install-dir (:dir ?install-dir)]
      (insert!
       {:type :strategy
        :install-strategy :archive
        :install-dir install-dir
        :install-source
        {:url (:url ?archive-url)
         :md5-url (:url ?md5-url)
         :unpack :tar
         :tar-options "xz"}
        :link [(format "%s/bin/mvn" install-dir)
               "/usr/bin/mvn"]}))
    (insert! {:install-strategy :archive}))
   (make-query 'get-strategy [] [?strategy <- :strategy])])

(defn print-strategy! [s]
  (doseq [strategy (query s 'get-strategy)]
    (println "strategy:" (:?strategy strategy))) s)

(s/defn my-insert [s fact :- Fact]
  #_(try (s/validate Fact fact)
       (catch clojure.lang.ExceptionInfo e
         (clojure.pprint/pprint (ex-data e))
         (throw (ex-info (format "Invalid fact: %s" fact)
                         (assoc (ex-data e) :cause fact)))))
  (insert s fact))

(defn run-test
  []
  (-> (mk-session (concat rules os-rules) :cache false :fact-type-fn :type)
      (my-insert {:type :os :family :ubuntu :maj "10" :min "04" :patch nil})
      (my-insert {:type :maven :maj 3 :min 2 :patch 1})
      (my-insert {:type :preferred-strategies
                  :strategies [:package :archive]})
      (my-insert
       {:type :maven-url-templates
        :md5 "http://www.apache.org/dist/maven/maven-%s/%s/binaries/apache-maven-%s-bin.tar.gz.md5"
        :download "http://mirrors.ibiblio.org/apache/maven/maven-%s/%s/binaries/apache-maven-%s-bin.tar.gz"})
      (fire-rules)
      (print-url!)
      (print-strategy!)
      (print-chosen-strategies!)
      (print-os-family!)))

(defn facts [s]
  (let [content (:content (clara.rules.engine/working-memory s))
        get-keys (fn [type-key]
                   (filter (fn [k]
                             (and
                              (not (nil? k))
                              (vector? k)
                              (= type-key (first k)))) (keys content)))
        pm-keys (get-keys :production-memory)
        alpha-keys (get-keys :alpha-memory)
        alpha-nodes (vals (select-keys content alpha-keys))]
    (into #{}
          (concat
           (map :fact (mapcat #(filter :fact %) alpha-nodes))
           (mapcat :facts (mapcat #(filter :facts %) pm-keys))))))
