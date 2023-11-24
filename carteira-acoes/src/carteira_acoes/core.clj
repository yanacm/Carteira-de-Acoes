(ns carteira-acoes.core
  (:gen-class)
  (:require [clojure.string :as str]))

(require '[clj-http.client :as client])
(require '[cheshire.core :as json])
(require '[clj-time.format :as fmt] '[clj-time.coerce :as coerce] '[clj-time.core :as time])

(def transacoes (atom []))

(defn req-get [endpoint params]
  (try
    (let [url (str "https://brapi.dev/api" endpoint "?function=OVERVIEW&symbol=MGLU3.SA&apikey=pXzsiZXMAkPotpQTNoPnbB")
          parametros (assoc params :token "pXzsiZXMAkPotpQTNoPnbB")
          response (client/get url {:query-params parametros})]
      {:status (:status response)
       :body   (json/parse-string (:body response))})
    (catch Exception e
      (let [response (ex-data e)]
        {:status (:status response)
         :body   (get (json/parse-string (:body response)) "message")}))
    ))

(defn extrair-numero [codigo]
  (if (or (= (str (nth codigo (- (count codigo) 1))) "F")
          (= (str (nth codigo (- (count codigo) 1))) "T")
          (= (str (nth codigo (- (count codigo) 1))) "B"))
    (Integer/parseInt (subs codigo 0 (dec (count codigo))))
    (Integer/parseInt codigo)))

(defn classificar-tipo-ativo [codigo]
  (let [numero-codigo (extrair-numero (subs codigo 4))]
    (cond
      (= numero-codigo 1) "Direito de Subscrição – Ação Ordinária"
      (= numero-codigo 2) "Direito de Subscrição – Ação Preferencial"
      (= numero-codigo 3) "Ordinárias"
      (= numero-codigo 4) "Preferenciais"
      (= numero-codigo 5) "Preferenciais Classe A"
      (= numero-codigo 6) "Preferenciais Classe B"
      (= numero-codigo 7) "Preferenciais Classe C"
      (= numero-codigo 8) "Preferenciais Classe D"
      (= numero-codigo 9) "Recibo de Subscrição – Ação Ordinária"
      (= numero-codigo 10) "Recibo de Subscrição – Ação Preferencial"
      (>= numero-codigo 11) "BDRs, ETs e Units"
      :else "Desconhecido")))

(defn listar-companhias []
  (let [response (req-get "/quote/list" {"sortOrder" "asc"})
        body (:body response)
        status (:status response)
        content (sort-by #(get % "name") (get body "stocks"))]
    (cond (= status 200)
          (map #(str "Nome: " (get % "name") " | Código: " (get % "stock")) content)
          :else [body])))

(defn get-companhia [nome]
  (let [response (req-get (format "/quote/%s" nome) {})
        body (:body response)
        status (:status response)]
    (cond (= status 200)
          body
          :else (str "Erro: " body))))

(defn get-dados-companhia [nome]
  (let [body (get-companhia nome)]
    (cond (string? body)
          [body]
          :else
          (let [nome-acao (get-in body ["results" 0 "longName"])
                codigo-acao (get-in body ["results" 0 "symbol"])
                tipo-ativo (classificar-tipo-ativo codigo-acao)
                descricao-acao (str "Nome da ação: " nome-acao
                                    " | Código da ação: " codigo-acao
                                    " | Tipo de ativo: " tipo-ativo
                                    " | Descrição da ação: " (get-in body ["results" 0 "longName"])
                                    " | " (get-in body ["results" 0 "shortName"]))
                variacao-dia (format "Variação do dia: R$ %.2f | %.2f %%"
                                     (float (get-in body ["results" 0 "regularMarketChange"]))
                                     (float (get-in body ["results" 0 "regularMarketChangePercent"])))
                ultimo-preco (format "Último preço: R$ %.2f" (float (get-in body ["results" 0 "regularMarketPrice"])))
                preco-maximo (format "Preço máximo: R$ %.2f" (float (get-in body ["results" 0 "regularMarketDayHigh"])))
                preco-minimo (format "Preço mínimo: R$ %.2f" (float (get-in body ["results" 0 "regularMarketDayLow"])))
                preco-abertura (format "Preço de abertura: R$ %.2f" (float (get-in body ["results" 0 "regularMarketOpen"])))
                preco-fechamento (format "Preço de fechamento: R$ %.2f" (float (get-in body ["results" 0 "regularMarketPreviousClose"])))
                hora-atualizacao (format "Hora: %s"
                                         (fmt/unparse (fmt/formatter "dd/MM/yyyy HH:mm:ss")
                                                      (fmt/parse (fmt/formatter "yyyy-MM-dd HH:mm:ss.SSSZ")
                                                                 (get-in body ["results" 0 "updatedAt"]))))]
            [descricao-acao variacao-dia ultimo-preco preco-maximo preco-minimo
             preco-abertura preco-fechamento hora-atualizacao]))))

(defn comprar-acao [codigo qtd]
  (let [acao (get-companhia codigo)]
    (cond (string? acao) (str "Não foi possível realizar a compra")
          (< qtd 0) (str "A quantidade precisa ser maior que 0")
          :else (let [data (fmt/unparse (fmt/formatter "dd/MM/yyyy HH:mm:ss") (time/now))]
                  (dosync
                    (swap! transacoes conj {:data data
                                            :acao (get-in acao ["results" 0 "symbol"])
                                            :tipo "compra"
                                            :valor (float (get-in acao ["results" 0 "regularMarketPrice"]))
                                            :qtd qtd}))
                  (str "Compra realizada com sucesso!")))))

(defn vender-acao [codigo qtd]
  (let [acao (get-companhia codigo)
        qtdAtualAcoes (reduce + (map :qtd (filter #(and (= codigo (:acao %)) (= "compra" (:tipo %))) @transacoes)))]
    (cond (string? acao) (str "Não foi possível realizar a venda")
          (< qtd 0) (str "A quantidade precisa ser maior que 0")
          (> qtd qtdAtualAcoes) (str "Você não possui ações suficientes para realizar a venda")
          :else (let [data (fmt/unparse (fmt/formatter "dd/MM/yyyy HH:mm:ss") (time/now))]
                  (dosync
                    (swap! transacoes conj {:data data
                                            :acao (get-in acao ["results" 0 "symbol"])
                                            :tipo "venda"
                                            :valor (float (get-in acao ["results" 0 "regularMarketPrice"]))
                                            :qtd qtd}))
                  (str "Venda realizada com sucesso!")))))

(defn get-transacoes []
  @transacoes)

(defn saldo-carteira []
  (apply + (map #(* (:valor %) (:qtd %)) @transacoes)))

(defn extrato-todas-transacoes []
  (let [transacoes-ordenadas (sort-by :data @transacoes)]
    (doseq [transacao transacoes-ordenadas]
      (println (format "Data: %s | Ação: %s | Tipo: %s | Valor: R$ %.2f | Quantidade: %d"
                       (:data transacao) (:acao transacao) (:tipo transacao) (:valor transacao) (:qtd transacao)))))
  (println (format "Valor Total da Carteira: R$ %.2f" (saldo-carteira))))

(defn extrato-por-tipo [tipo]
  (let [transacoes-filtradas (filter #(= tipo (:tipo %)) @transacoes)
        transacoes-ordenadas (sort-by :data transacoes-filtradas)]
    (doseq [transacao transacoes-ordenadas]
      (println (format "Data: %s | Ação: %s | Tipo: %s | Valor: R$ %.2f | Quantidade: %d"
                       (:data transacao) (:acao transacao) (:tipo transacao) (:valor transacao) (:qtd transacao)))))
  (println (format "Valor Total da Carteira: R$ %.2f" (saldo-carteira))))

(defn read-write [opcao]
  (cond
    (= opcao 1) (let [companhias (listar-companhias)]
                  (dorun (map println companhias))
                  (println (format "\nMenu\n%d - Listar Companhias\n%d - Detalhes de Ação\n%d - Comprar Ações\n%d - Vender Ações\n%d - Exibir Extrato" 1 2 3 4 5))
                  (recur (read)))
    (= opcao 2) (do
                  (println "Digite o código da ação: ")
                  (let [codigo (str/upper-case (read))
                        companhia (get-dados-companhia codigo)]
                    (dorun (map println companhia))
                    (println (format "\nMenu\n%d - Listar Companhias\n%d - Detalhes de Ação\n%d - Comprar Ações\n%d - Vender Ações\n%d - Exibir Extrato" 1 2 3 4 5))
                    (recur (read))))
    (= opcao 3) (do
                  (println "Digite o código da ação e quantidade:")
                  (let [codigo (str/upper-case (read))
                        qtd (read)]
                    (println (comprar-acao codigo qtd))
                    (println (format "\nMenu\n%d - Listar Companhias\n%d - Detalhes de Ação\n%d - Comprar Ações\n%d - Vender Ações\n%d - Exibir Extrato" 1 2 3 4 5))
                    (recur (read))))
    (= opcao 4) (do
                  (println "Digite o código da ação e quantidade:")
                  (let [codigo (str/upper-case (read))
                        qtd (read)]
                    (println (vender-acao codigo qtd))
                    (println (format "\nMenu\n%d - Listar Companhias\n%d - Detalhes de Ação\n%d - Comprar Ações\n%d - Vender Ações\n%d - Exibir Extrato" 1 2 3 4 5))
                    (recur (read))))
    (= opcao 5) (do
                  (println "Escolha o tipo de extrato:")
                  (println "1 - Extrato de Compras")
                  (println "2 - Extrato de Vendas")
                  (println "3 - Extrato Completo")
                  (let [tipo-extrato (read)]
                    (cond
                      (= tipo-extrato 1) (extrato-por-tipo "compra")
                      (= tipo-extrato 2) (extrato-por-tipo "venda")
                      (= tipo-extrato 3) (extrato-todas-transacoes)
                      :else (println "Opção inválida.")))
                  (println (format "\nMenu\n%d - Listar Companhias\n%d - Detalhes de Ação\n%d - Comprar Ações\n%d - Vender Ações\n%d - Exibir Extrato" 1 2 3 4 5))
                  (recur (read)))
    :else (println "Fim.")))

(println (format "Menu\n%d - Listar Companhias\n%d - Detalhes de Ação\n%d - Comprar Ações\n%d - Vender Ações\n%d - Exibir Extrato" 1 2 3 4 5))
(def opcao-inicial (read))
(read-write opcao-inicial)