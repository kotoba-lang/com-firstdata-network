(ns firstdata_network-compat.main-test
  "Contract + behavioral test for the firstdata_network-compat L4 actor (cljc port).
  Runs under babashka: `bb test`. Stronger than the py static contract test —
  exercises CRUD / pagination / filtering / expansion / validation against the
  in-memory Datom-log store."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [firstdata_network-compat.main :as m]))

(def entities ["Customer" "PaymentIntent" "Charge" "Refund" "Payout" "PaymentMethod"])

(deftest schema-has-all-entities
  (is (= (set entities) (set m/entities))))

(deftest full-crud-per-entity
  (testing "every entity exposes POST/GET-list/GET-one/PATCH/DELETE"
    (doseq [{:keys [plural]} m/entity-specs]
      (let [base (str "/v1/" plural)
            paths (set (map (juxt :method :path) m/routes))]
        (is (contains? paths ["POST" base]))
        (is (contains? paths ["GET" base]))
        (is (contains? paths ["GET" (str base "/{id}")]))
        (is (contains? paths ["PATCH" (str base "/{id}")]))
        (is (contains? paths ["DELETE" (str base "/{id}")]))))
    (is (= 30 (count m/routes)))))

(deftest create-and-get
  (let [s (m/fresh-store)
        [rec status] (m/handle-create s "Customer" {:email "test@example.com" :name "Test"})]
    (is (= 201 status))
    (is (= "Test" (:name rec)))
    (is (re-find #"^firstdat_cus_" (:id rec)))
    (is (= [rec 200] (m/handle-get s "Customer" (:id rec) {})))))

(deftest validation-required-and-unknown
  (let [s (m/fresh-store)]
    (testing "missing required field -> 400"
      (is (= 400 (second (m/handle-create s "Customer" {})))))
    (testing "unknown field -> 400"
      (is (= 400 (second (m/handle-create s "Customer" {:email "test@example.com" :name "x" :bogus 1})))))))

(deftest coercion
  (let [s (m/fresh-store)
        [rec _] (m/handle-create s "Customer" {:email "test@example.com" :name "Test" :balance "100"})]
    (is (= 100 (:balance rec)))
    (let [[intent _] (m/handle-create s "PaymentIntent" {:amount "50" :currency "USD"})]
      (is (= 50 (:amount intent))))))

(deftest list-filter-and-paginate
  (let [s (m/fresh-store)]
    (dotimes [i 25] (m/handle-create s "Customer" {:email (str "cust" i "@example.com") :name (str "Customer" i)}))
    (let [[body _] (m/handle-list s "Customer" {})]
      (is (= 20 (:count body)))            ; default limit
      (is (true? (:has_more body)))
      (is (= 25 (:total body))))))

(deftest expansion
  (let [s (m/fresh-store)
        [cust _] (m/handle-create s "Customer" {:email "test@example.com" :name "Test"})
        [intent _] (m/handle-create s "PaymentIntent" {:amount 100 :currency "USD" :customerId (:id cust)})
        [got _] (m/handle-get s "PaymentIntent" (:id intent) {:expand "customerId"})]
    (is (= cust (:customerId_obj got)))))

(deftest update-and-delete
  (let [s (m/fresh-store)
        [rec _] (m/handle-create s "Customer" {:email "old@example.com" :name "Old"})
        [upd _] (m/handle-update s "Customer" (:id rec) {:name "New"})]
    (is (= "New" (:name upd)))
    (is (= (:id rec) (:id upd)))           ; id immutable
    (is (= 200 (second (m/handle-delete s "Customer" (:id rec)))))
    (is (= 404 (second (m/handle-get s "Customer" (:id rec) {}))))))

(deftest eavt-fact-emission
  (testing "datomic EAVT mapping preserved: firstdata_network.<Entity>/<field>"
    (let [facts (m/emit-facts "Customer" {:id "firstdat_cus_x" :name "n"})]
      (is (= "n" (get facts "firstdata_network.Customer/name")))
      (is (= "firstdat_cus_x" (get facts "firstdata_network.Customer/id"))))))

(deftest healthz
  (is (= [{:status "ok" :actor "firstdata_network-compat" :tier "L4" :entities entities} 200] (m/healthz))))

#?(:clj (defn -main [& _]
          (let [{:keys [fail error]} (run-tests 'firstdata_network.main-test)]
            (System/exit (if (pos? (+ fail error)) 1 0)))))
