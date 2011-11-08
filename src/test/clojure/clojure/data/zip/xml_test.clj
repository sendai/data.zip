(ns clojure.data.zip.xml-test
  (:use [clojure.test]
        [clojure.data.zip.xml])
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))


(defn parse-str [s]
  (zip/xml-zip (xml/parse (new org.xml.sax.InputSource
                               (new java.io.StringReader s)))))

(def atom1 (parse-str "<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns='http://www.w3.org/2005/Atom'>
  <id>tag:blogger.com,1999:blog-28403206</id>
  <updated>2008-02-14T08:00:58.567-08:00</updated>
  <title type='text'>n01senet</title>
  <link rel='alternate' type='text/html' href='http://n01senet.blogspot.com/'/>
  <entry>
    <id>1</id>
    <published>2008-02-13</published>
    <title type='text' show='true'>clojure is the best lisp yet</title>
    <author><name>Chouser</name></author>
  </entry>
  <entry>
    <id>2</id>
    <published>2008-02-07</published>
    <title type='html' show='true'><![CDATA[<h1>experimenting with vnc</h1>]]></title>
    <author><name>agriffis</name></author>
  </entry>
</feed>
"))

(deftest test-simple-single-function filter
  (is (= (xml-> atom1 #((zip/node %) :tag))
         '(:feed))))

(deftest test-two-stage-filter
  (testing "using helpful query prediates"
    (is (= (xml-> atom1 (tag= :title) text)
           '("n01senet"))))
  (testing "keyword shortcut"
    (is (= (xml-> atom1 :title text)
           '("n01senet")))))

(deftest test-multi-stage-filter
  (testing "basic functionality"
    (is (= (xml-> atom1 :entry :author :name text)
           '("Chouser" "agriffis"))))
  (testing "subquery specified using a vector"
    (is (= (xml-> atom1 :entry [:author :name (text= "agriffis")]
                  :id text)
           '("2"))))
  (testing "with string shortcut"
    (is (= (xml-> atom1 :entry [:author :name "agriffis"] :id text)
           '("2")))))

(deftest test-xml1->
  (is (= (xml1-> atom1 :entry :author :name text)
         "Chouser")))

(deftest test-attribute-access
  (is (= (xml-> atom1 :title (attr :type))
         '("text"))))

(deftest test-attribute-filtering
  (is (= (xml-> atom1 :link [(attr= :rel "alternate")] (attr :type))
         '("text/html"))))

(deftest test-attribute-filtering-by-regexp
  (is (= (xml-> atom1 :entry :title (attr-re :type #"ml") text)
         '("<h1>experimenting with vnc</h1>"))))

(deftest test-text-matching
  (is (= (xml-> atom1 :entry [:title (text-re #"vnc")] :published text)
         '("2008-02-07"))))

(deftest test-regexp-shortcut
  (is (= (xml-> atom1 :entry :published #"^\d\d\d\d-\d\d-\d\d$" text)
         '("2008-02-13" "2008-02-07"))))

(deftest test-map-shortcut
  (is (= (xml-> atom1 :entry :title {} text)
         '("clojure is the best lisp yet" "<h1>experimenting with vnc</h1>")))
  (is (= (xml-> atom1 :link {:rel "alternate"} (attr :type))
         '("text/html")))
  (is (= (xml-> atom1 :entry :title {:show "true"} text)
         '("clojure is the best lisp yet" "<h1>experimenting with vnc</h1>")))
  (is (= (xml-> atom1 :entry :title {:type "html"} text)
         '("<h1>experimenting with vnc</h1>")))
  (is (= (xml-> atom1 :entry :title {:none "ouch"} text)
         '()))
  (is (= (xml-> atom1 :entry :title {:show "true" :type "html"} text)
         '("<h1>experimenting with vnc</h1>")))
  (is (= (xml-> atom1 :entry :title {:show "true" :type "text"} text)
         '("clojure is the best lisp yet")))
  (is (= (xml-> atom1 :entry :title {:show "true" :type "image"} text)
         '())))

;; This was in the original code under a comment, but is fails
#_(deftest test-ancestors
  (testing "basic function"
    (is (= (xml-> atom1 descendants :id "2" ancestors zip/node #(:tag %))
           '(:id :entry :feed))))
  (testing "with non-auto tag= (:entry), followed by auto tag= (:id)"
    (is (= (xml-> atom1 descendants :name "Chouser" ancestors
                  :entry :id text)
           '("1")))))
