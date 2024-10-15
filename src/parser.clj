(ns parser
  (:require [grammars :as wae]))

;; Para esto, se debe de considerar el primer elemento de la lista, si es
;; un valor atomico o se debe de realizar recursión con respecto a la estructura
;; del lenguaje
(defn parser-AE
  "Función que realiza el parser del lenguaje AE de un listado de variables"
  [exp]
  (cond
    (number? exp)
    (wae/numG exp)

    (list? exp)
    (case (first exp)
      + (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
      - (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))
      :else (throw (IllegalArgumentException. "Syntax Error")))))


;; Para esto, se debe de considerar los valores atomicos del lenguaje
;; con respecto a la cabeza de la lista, sino realizar recursión
(defn parser-WAE
  "Función que realiza el parser del lenguaje WAE de un listado de variables"
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
      :else (throw (IllegalArgumentException. "Syntax Error")))))

