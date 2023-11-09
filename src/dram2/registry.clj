(ns dram2.registry
  "Units registry.

   Tracks relationships between units using datascript.

   Pint-style units definition syntax, from the default_en.txt file:

   Syntax
   ======
   Units
   -----
   <canonical name> = <relation to another unit or dimension> [= <symbol>] [= <alias>] [ = <alias> ] [...]

   The canonical name and aliases should be expressed in singular form.
   Pint automatically deals with plurals built by adding 's' to the singular form; plural
   forms that don't follow this rule should be instead explicitly listed as aliases.

   If a unit has no symbol and one wants to define aliases, then the symbol should be
   conventionally set to _.

   Example:
   millennium = 1e3 * year = _ = millennia


   Prefixes
   --------
   <prefix>- = <amount> [= <symbol>] [= <alias>] [ = <alias> ] [...]

   Example:
   deca- =  1e+1  = da- = deka-


   Derived dimensions
   ------------------
   [dimension name] = <relation to other dimensions>

   Example:
   [density] = [mass] / [volume]

   Note that primary dimensions don't need to be declared; they can be
   defined for the first time in a unit definition.
   E.g. see below `meter = [length]`


   Additional aliases
   ------------------
   @alias <canonical name or previous alias> = <alias> [ = <alias> ] [...]

   Used to add aliases to already existing unit definitions.
   Particularly useful when one wants to enrich definitions
   from defaults_en.txt with custom aliases.

   Example:
   @alias meter = my_meter

   See also: https://pint.readthedocs.io/en/latest/defining.html"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [datascript.core :as d]
   [instaparse.core :as insta]
   [instaparse.transform :as t]))

(def short-registry-text
  "A shorter registry with stuff that is useful for building energy."
  ;; this exists b/c we can't quite parse the full pint syntax yet.
  (slurp (io/resource "dram_en.txt")))

(def long-registry-text
  ;; FIXME this doesn't parse yet, need the shorter one above
  "The full pint registry."
  (slurp (io/resource "default_en.txt")))

(defn parse-registry
  "Parse a pint-style defaults file into a hiccup tree."
  [s]
  (let [grammar (io/resource "pint_registry.bnf")
        parse (insta/parser grammar :auto-whitespace :standard)]
    (as-> s $
      (str/trim $)
      (str $ "\n")
      (parse $ :total true))))

(defn transform-registry
  "Turn hiccup registry entries into maps for transacting into datascript."
  [xs]
  (let [xfs {:reg  (fn [& xs] xs)
             :defn (fn [lhs & rhss]
                     (let [[rel sym & aliases] rhss
                           m (assoc lhs :relation rel)]
                       (cond-> m
                         (some? sym) (assoc :symbol sym)
                         (seq aliases) (assoc :aliases (set aliases)))))
             :lhs  (fn [s] (if (str/ends-with? s "-")
                             {:prefix s}
                             {:unit s}))
             :rhs  identity}]
    (t/transform xfs xs)))

(defrecord Registry [*conn])

(def schema {:ualias {:db/cardinality :db.cardinality/many}
             :palias {:db.cardinality :db.cardinality/many}})

(defn make-registry [s]
  (let [definitions (-> s parse-registry transform-registry)
        *conn (d/create-conn schema)]
    (tap> definitions)
    (d/transact *conn definitions)
    (map->Registry {:*conn *conn})))

(comment
  (def registry (make-registry short-registry-text))

  (->> (d/q '[:find ?unit :where [_ :unit ?unit]] @(:*conn registry))
       (map first)
       (count))

  ;; find unit given an alias
  (d/q '[:find ?unit :where [?e :unit ?unit] [?e :ualias "feet"]]
       @(:*conn registry))

  ;; all units and aliases
  (->> (d/q '[:find ?x ?y :where [?e :unit ?x] [?e :ualias ?y]]
            @(:*conn registry))
       (sort-by first)))

;; auto-whitespace?
;; start datascript db
;; fix doubles in formulas
;; fix parens, precedence in formula relationships

(defn eq?
  "Are the registries equivalent? (Papering over a pint limitation, where it
   seems to want the exact same registry object in memory."
  [_registry-a _registry-b]
  false)
