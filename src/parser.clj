(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
  "Función que realiza el parser "
  [exp] 
  (cond
    (number? exp)
    (wae/numG exp)

    (list? exp)
    (case (first exp)
      + (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
      - (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))
      :else (throw (IllegalArgumentException. "Syntax Error")))))

(defn parser-WAE
  [exp]
  (cond
    (symbol? exp)
    (wae/idG exp)

    (number? exp)
    (wae/numG exp)

    (list? exp)
    (case (first exp)
      + (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))

      - (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))

      with (wae/withG
            (wae/bindings (first (first (rest exp))) (parser-WAE (first (rest (first (rest exp))))))
            (parser-WAE (nth exp 2)))
      :else (throw (IllegalArgumentException. "Syntax Error"))
      )))

