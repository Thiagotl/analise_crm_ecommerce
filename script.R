library(readr)
library(tidyverse)

df <- read_csv("project1_df.csv")

df$`Purchase Date` <- as.POSIXct(df$`Purchase Date`, format = "%d/%m/%Y %H:%M:%S")

clientes <- df %>%
  group_by(CID, `Age Group`, Gender, Location) %>%
  summarise(
    total_compras = n(),
    valor_total = sum(`Net Amount`, na.rm = TRUE),
    ticket_medio = mean(`Net Amount`, na.rm = TRUE),
    descontos_utilizados = sum(`Discount Availed` == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

clientes_segmentados <- clientes %>%
  mutate(
    categoria_valor = case_when(
      valor_total >= 5000 ~ "Alto Valor",
      valor_total >= 2000 ~ "Médio Valor",
      TRUE ~ "Baixo Valor"
    ),
    categoria_frequencia = case_when(
      total_compras >= 10 ~ "Frequente",
      total_compras >= 5 ~ "Ocasional",
      TRUE ~ "Raro"
    ),
    categoria_desconto = case_when(
      descontos_utilizados / total_compras > 0.7 ~ "Muito Sensível a Preço",
      descontos_utilizados / total_compras > 0.3 ~ "Moderadamente Sensível",
      TRUE ~ "Pouco Sensível"
    )
  )


ggplot(clientes_segmentados, aes(x = categoria_valor, fill = categoria_valor)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, size = 3.5) +
  labs(title = "Clientes por Categoria de Valor Total",
       x = "Categoria de Valor", y = "Quantidade") +
  theme_minimal(base_size = 14)


df$`Purchase Date` <- dmy_hms(df$`Purchase Date`)

data_ref <- max(df$`Purchase Date`, na.rm = TRUE) + days(1)

rfm_df <- df %>%
  group_by(CID) %>%
  summarise(
    recencia = as.numeric(difftime(data_ref, max(`Purchase Date`), units = "days")),
    frequencia = n(),
    valor_monetario = sum(`Net Amount`, na.rm = TRUE),
    .groups = "drop"
  )

rfm_df <- rfm_df %>%
  mutate(
    r_score = ntile(-recencia, 5),  # menor recência = melhor score
    f_score = ntile(frequencia, 5),
    m_score = ntile(valor_monetario, 5),
    rfm_score = paste0(r_score, f_score, m_score)
  )


rfm_df <- rfm_df %>%
  mutate(
    segmento = case_when(
      r_score >= 4 & f_score >= 4 & m_score >= 4 ~ "Melhores Clientes",
      r_score >= 3 & f_score >= 3 ~ "Clientes Fiéis",
      r_score >= 4 & f_score <= 2 ~ "Clientes Recentes",
      r_score <= 2 & f_score >= 4 ~ "Clientes Frequentes",
      TRUE ~ "Outros"
    )
  )


ggplot(rfm_df, aes(x = segmento, fill = segmento)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, size = 3.5) +
  labs(title = "Distribuição de Clientes por Segmento RFM",
       x = "Segmento", y = "Número de Clientes") +
  theme_minimal(base_size = 14)




# IMPORTANTE - CRIAR TABELA SOBRE
resumo <- df %>%
  inner_join(rfm_df, by = "CID") %>%
  group_by(segmento) %>%
  summarise(
    n_clientes = n_distinct(CID),
    total_vendas = sum(`Net Amount`, na.rm = TRUE),
    ticket_medio = mean(`Net Amount`, na.rm = TRUE),
    uso_desconto = mean(`Discount Availed` == "Yes", na.rm = TRUE)
  )

# Descontos------------


df$`Discount Availed` <- ifelse(df$`Discount Availed` == "Yes", "Com Desconto", "Sem Desconto")

resumo_ticket <- df %>%
  group_by(`Discount Availed`) %>%
  summarise(
    media_ticket = mean(`Net Amount`, na.rm = TRUE),
    sd_ticket = sd(`Net Amount`, na.rm = TRUE),
    n = n(),
    erro_padrao = sd_ticket / sqrt(n),
    .groups = "drop"
  )

categoria_desconto <- df %>%
  group_by(`Product Category`, `Discount Availed`) %>%
  summarise(
    total_transacoes = n(),
    media_desconto = mean(`Discount Amount (INR)`, na.rm = TRUE),
    media_valor_liquido = mean(`Net Amount`, na.rm = TRUE),
    .groups = "drop"
  )

cupons <- df %>%
  filter(!is.na(`Discount Name`)) %>%
  group_by(`Discount Name`) %>%
  summarise(
    n = n(),
    valor_medio_desconto = mean(`Discount Amount (INR)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))


ggplot(resumo_ticket, aes(x = `Discount Availed`, y = media_ticket, fill = `Discount Availed`)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = media_ticket - erro_padrao,
                    ymax = media_ticket + erro_padrao),
                width = 0.2) +
  labs(title = "Ticket Médio com e sem Desconto",
       x = "", y = "Valor Médio da Compra (INR)") +
  theme_minimal(base_size = 14)



ggplot(categoria_desconto, aes(x = `Product Category`, y = total_transacoes, fill = `Discount Availed`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = total_transacoes),
    position = position_dodge(width = 0.9),
    hjust = -0.1, size = 3
  ) +
  coord_flip() +
  labs(
    title = "Uso de Desconto por Categoria de Produto",
    x = "Categoria de Produto",
    y = "Total de Transações",
    fill = "Tipo de Desconto"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# analise cesta de produtos-------

cliente_categoria <- df %>%
  distinct(CID, `Product Category`) %>%
  mutate(presenca = 1) %>%
  tidyr::pivot_wider(names_from = `Product Category`, values_from = presenca, values_fill = 0)


# library(arules)
# 
# # Converter para formato transacional
# transacoes <- as(as.matrix(cliente_categoria[,-1]), "transactions")
# 
# # Rodar algoritmo Apriori
# regras <- apriori(transacoes, parameter = list(supp = 0.01, conf = 0.1, minlen = 2))
# 
# inspect(head(sort(regras, by = "lift"), 10))
# 
# library(arulesViz)
# 
# plot(regras, method = "graph", engine = "htmlwidget")
# 
# 


# Clientes e categorias
cliente_categoria <- df %>%
  distinct(CID, `Product Category`)

# Gerar combinações de categorias por cliente
pares <- cliente_categoria %>%
  inner_join(cliente_categoria, by = "CID") %>%
  filter(`Product Category.x` < `Product Category.y`) %>%
  count(`Product Category.x`, `Product Category.y`, sort = TRUE) %>%
  top_n(10, wt = n)

ggplot(pares, aes(x = reorder(paste(`Product Category.x`, "&", `Product Category.y`), n), 
                  y = n, fill = paste(`Product Category.x`, "&", `Product Category.y`))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Pares de Categorias Compradas Juntas",
    x = "Par de Categorias",
    y = "Número de Clientes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


# library(reshape2)
# 
# # Matriz de coocorrência
# mat <- cliente_categoria %>%
#   inner_join(cliente_categoria, by = "CID") %>%
#   count(`Product Category.x`, `Product Category.y`) %>%
#   acast(`Product Category.x` ~ `Product Category.y`, fill = 0)
# 
# # Heatmap
# 
# ggplot(melt(mat), aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "darkred") +
#   labs(title = "Coocorrência entre Categorias de Produto", x = "", y = "") +
#   theme_minimal(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Analise temporal -------


# df$`Purchase Date` <- dmy_hms(df$`Purchase Date`)
# df <- df %>% mutate(ano_mes = floor_date(`Purchase Date`, unit = "month"))
# 
# resumo_temporal <- df %>%
#   group_by(ano_mes) %>%
#   summarise(
#     total_vendas = sum(`Net Amount`, na.rm = TRUE),
#     numero_transacoes = n(),
#     ticket_medio = mean(`Net Amount`, na.rm = TRUE),
#     clientes_unicos = n_distinct(CID)
#   )
# 
# 
# ggplot(resumo_temporal, aes(x = ano_mes, y = total_vendas)) +
#   geom_line(color = "steelblue", size = 1) +
#   labs(title = "Faturamento Mensal", x = "Mês", y = "Total Vendido (INR)") +
#   theme_minimal(base_size = 14)
# 
# 
# ggplot(resumo_temporal, aes(x = ano_mes, y = clientes_unicos)) +
#   geom_line(color = "darkgreen", size = 1) +
#   labs(title = "Clientes Únicos por Mês", x = "Mês", y = "Nº de Clientes") +
#   theme_minimal(base_size = 14)
# 
# ggplot(resumo_temporal, aes(x = ano_mes, y = ticket_medio)) +
#   geom_line(color = "darkred", size = 1) +
#   labs(title = "Ticket Médio Mensal", x = "Mês", y = "Valor Médio (INR)") +
#   theme_minimal(base_size = 14)

