(ns edn-to-jsonc.core
  (:require [parcera.core :as parcera]))
;;;;;; TODO emplement comma seperation in Maps Sets and Vectors

(defn  resolve-map  [[token & tokens] state result]
  (if (empty? token)
    (str result  "}" )
    (case state
      :start
      (str "{" (resolve-map (cons token tokens) :look-for-key ""))
      :look-for-key
      (case (first token)
        (:comment :whitespace)
        (resolve-map tokens state (str result (ast-dispatch (list token))))
        (:set :vector :string :keyword :map :number)
        (resolve-map tokens :look-for-value (str result  (ast-dispatch (list token)) ":" )))
      :look-for-value
      (case (first token)
        :whitespace
        (case (second token)
          " " (resolve-map tokens :look-for-value result)
          (resolve-map tokens state result))
        (:set :vector :map :string :number :keyword)
        (resolve-map tokens :look-for-more-keys (str result (ast-dispatch (list token)))))
      :look-for-more-keys
      (resolve-map (cons token tokens) :look-for-key result))))

(defn  resolve-vector  [[token & tokens] state result]
  (if (empty? token)
    (str result  "]" )
    (case state
      :start
      (str "[" (resolve-vector (cons token tokens) :look-for-element ""))
      :look-for-element
      (case (first token)
        :whitespace
        (case (second token)
          " " (resolve-vector tokens :look-for-more-elements result)
          (resolve-vector tokens state result))
        (:set :vector :map :string :number :keyword)
        (resolve-vector tokens :look-for-more-elements (str result (ast-dispatch (list token)))))
      :look-for-more-elements
      (resolve-vector (cons token tokens) :look-for-element result))))

(defn ast-dispatch [[token & rest-tokens]]
  (if (empty? token)
    ""
    (let [token-name (first token)
          token-value (second token)]
      (let [result
            (case token-name
              :comment
              (str "\\" token-value)
              (:whitespace :number :string :keyword)
              token-value
              :map
              (resolve-map (rest token) :start "")
              (:set :vector)
              (resolve-vector (rest token) :start ""))]
        (str result (ast-dispatch rest-tokens))))))
