#############################
# Análise de completude - SIH-SUS
# Período total: 2014-2023
# ISABELA FREITAS VAZ
# Internações de crianças menores de 5 anos representam 15,33% das internações
############################

### Instalando pacotes ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load (tidyverse, ggplot2, readr, summarytools, gtsummary,
                stringr, ggpubr, lubridate, read.dbc, readxl, openxlsx,
                devtools, patchwork)

### Diretório e base ----
setwd("~/FIOCRUZ/DISSERTAÇÃO FINAL/Dados - novo/SIH")
sih <- load("sih_menores_limp.RData")

### Análise de completude ----
# Selecionar variáveis preenchimento obrigatório
sih_obrigatorias <- sih %>% select(MUNIC_MOV, DT_INTER, DT_SAIDA, PROC_REA, CNES, NAT_JUR, ESPEC, IDENT, N_AIH, COD_IDADE, IDADE, SEXO, NASC, MUNIC_RES, DIAG_PRINC, MORTE, RACA_COR)
sih_obrigatorias$RACA_COR <- as.factor(sih_obrigatorias$RACA_COR)

# Calcular proporção não preenchida
na_obrigat <- sih_obrigatorias %>% 
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>% 
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "percent_na") %>%
  arrange(desc(percent_na))

# Visualizar a distribuição temporal do preenchimento
anos_interesse <- 2008:2024

na_por_ano <- sih_obrigatorias %>%
  filter(format(DT_INTER, "%Y") %in% as.character(anos_interesse)) %>%  # Filtrar os anos de 2008 a 2024
  mutate(ano = as.integer(format(DT_INTER, "%Y"))) %>%  # Criar a coluna ano corretamente
  group_by(ano) %>%  # Agrupar por ano
  summarise(across(
    everything(),
    ~ sum(is.na(.)) / n() * 100,  # Calcular porcentagem de "não preenchidos"
    .names = "percent_na_{.col}")) %>% 
  pivot_longer(
    cols = starts_with("percent_na_"),
    names_to = "variavel",
    values_to = "percent_na") %>% 
  mutate(
    variavel = gsub("percent_na_", "", variavel)  # Limpar os nomes das variáveis
  ) %>% 
  arrange(desc(percent_na))  # Ordenar pelo maior percentual de NA

# Visualizar os resultados - Para todas
comp_obrig <- ggplot(na_por_ano, aes(x = ano, y = percent_na, color = variavel)) +
  geom_line(linewidth = 1) +  # Atualizado para usar linewidth
  geom_point(size = 2) +  # Ajusta o tamanho dos pontos
  scale_x_continuous(breaks = seq(min(na_por_ano$ano), max(na_por_ano$ano), by = 1)) +  # Garante que todos os anos apareçam no eixo x
  labs(
    x = "Ano",
    y = "Proporção de Não Preenchimento (%)",
    color = "Variáveis obrigatórias"
  ) +
  theme_minimal() +  # Tema clean
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),  # Rotaciona e centraliza os rótulos do eixo x
    panel.grid.major = element_line(color = "grey90"),  # Linhas de grade discretas
    panel.grid.minor = element_blank(),  # Remove linhas de grade menores
    legend.position = "bottom",  # Move a legenda para a parte inferior
    legend.title = element_text(size = 10, face = "bold"),  # Estiliza o título da legenda
    legend.text = element_text(size = 9)  # Ajusta o tamanho do texto da legenda
  )

# Visualizar os resultados - Para incompletude "Raça/Cor"
na_por_ano_obg <- na_por_ano %>% filter(variavel %in% c("RACA_COR"))

comp_obrig <- ggplot(na_por_ano_obg, aes(x = factor(ano), y = percent_na, fill = variavel)) +
  geom_col(
    position = position_dodge(width = 0.8), 
    width = 0.7, 
    alpha = 0.9,
    color = "white",  # Borda branca para destacar as barras
    linewidth = 0.3) + 
  geom_text(
    aes(label = paste0(round(percent_na, 1))),  # Formatação com "%"
    position = position_dodge(width = 0.8),
    vjust = -0.3,  # Ajuste vertical mais preciso
    size = 2.8,    # Redução sutil do tamanho
    color = "black",
    family = "Times New Roman") +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    limits = c(0, 105),  # Espaço extra para os labels
    expand = c(0, 0),
    name = "% de incompletude campo Raça/Cor") +
  scale_fill_manual(
    values = "#D62728",
    name = NULL,
    labels = "Raça/Cor",
    guide = guide_legend(override.aes = list(alpha = 0.9))) +  # Controle da legenda
  labs(x = "Ano de internação") +  # Remove título do eixo X (rótulos dos anos são autoexplicativos)
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 8)),  # Título eixo X
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 8)),  # Título eixo Y
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      margin = margin(t = 2, b = 2),  
      size = 9),
    axis.text.y = element_text(size = 10),
    axis.line = element_line(color = "black", linewidth = 0.3),  
    axis.ticks = element_line(linewidth = 0.3),  
    panel.grid.major.y = element_line(
      color = "grey95",  
      linewidth = 0.2),
    legend.position = "bottom",
    legend.margin = margin(t = -8),  
    legend.text = element_text(margin = margin(r = 10)),  
    plot.margin = margin(10, 15, 5, 15)  
  )

ggsave("completuderaca.jpg", comp_obrig,
       width = 15, height = 10, units = "cm", dpi = 600)

### Indicadores de Assistência ----
## Proporção de transferências intermunicipais
## Estimando frequências
transf <- sih %>% 
  group_by(ano_inter, DUMMY_TRANS, RACA_COR2) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  group_by(ano_inter, RACA_COR2) %>% 
  mutate(
    proportion = count / sum(count) * 100  # Calcula a proporção como porcentagem
  ) %>% 
  ungroup() %>% 
  arrange(RACA_COR2, as.numeric(ano_inter))  # Ordena por `RACA_COR` e `ano_inter`
table(transf$DUMMY_TRANS)

## Salvando dataframe para análise em joinpoint
TRANSF <- transf %>%
  filter(DUMMY_TRANS == "Sim", as.numeric(ano_inter) >= 2008)
write.xlsx(TRANSF, file = "TRANSF_27_05.xlsx", rowNames = TRUE)

## Proporção de internações por Condições Sensíveis à Atenção Primária

## Estimando frequências
icsap_prop <- sih %>%
  group_by(ano_inter, icsap_dummy, RACA_COR2) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  group_by(ano_inter, RACA_COR2) %>% 
  mutate(
    proportion = count / sum(count) * 100  # Calcula a proporção como porcentagem
  ) %>% 
  ungroup() %>% 
  arrange(RACA_COR2, as.numeric(ano_inter))  # Ordena por `RACA_COR` e `ano_inter`

## Salvando dataframe para análise em joinpoint
icsap <- icsap_prop %>%
  filter(icsap_dummy == "Sim", as.numeric(ano_inter) >= 2008)
write.xlsx(icsap, file = "icsap2_27_05.xlsx", rowNames = TRUE)


#### Resultados do joinpoint ----
joinpoint_indicadores <- read_excel("joinpoint_indicadores27_05.xlsx")
glimpse(joinpoint_indicadores)

transf <- ggplot(joinpoint_indicadores, aes(x = ano_inter, group = RACA_COR)) +
  geom_line(aes(y = transf_mod, color = RACA_COR), size = 0.5, alpha = 0.8) +  # Cor mapeada aqui
  geom_point(aes(y = transf_obs, shape = RACA_COR, size = RACA_COR, color = RACA_COR)) +
  geom_point(data = subset(joinpoint_indicadores, !is.na(Joinpoin_transf)), aes(y = transf_mod), shape = 21, fill = "white", size = 3, color = "black") +
  scale_shape_manual(values = c("Indígena" = 17, "Não Indígena" = 16, "Ignorado/Não Informado" = 16)) +
  scale_size_manual(values = c("Indígena" = 2, "Não Indígena" = 1, "Ignorado/Não Informado" = 2), guide = "none") +
  scale_color_manual(values = c("Indígena" = "#D62728", "Não Indígena" = "#1F77B4", "Ignorado/Não Informado" = "#7F7F7F")) +
  scale_x_continuous(breaks = seq(2008, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(
    x = "Ano de internação",
    y = "% de internações fora de domicilio",
    shape = "Raça",
    color = "Raça"  # Legenda para cor
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

icsap <- ggplot(joinpoint_indicadores, aes(x = ano_inter, group = RACA_COR)) +
  geom_line(aes(y = icsap_mod, color = RACA_COR), size = 0.5, alpha = 0.8) +  # Cor mapeada aqui
  geom_point(aes(y = icsap_obs, shape = RACA_COR, size = RACA_COR, color = RACA_COR)) +
  geom_point(data = subset(joinpoint_indicadores, !is.na(Joinpoin_icsap)), aes(y = icsap_mod), shape = 21, fill = "white", size = 3, color = "black") +
  scale_shape_manual(values = c("Indígena" = 17, "Não Indígena" = 16, "Ignorado/Não Informado" = 16)) +
  scale_size_manual(values = c("Indígena" = 2, "Não Indígena" = 1, "Ignorado/Não Informado" = 2), guide = "none") +
  scale_color_manual(values = c("Indígena" = "#D62728", "Não Indígena" = "#1F77B4", "Ignorado/Não Informado" = "#7F7F7F")) +
  scale_x_continuous(breaks = seq(2008, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(
    x = "Ano de internação",
    y = "% de ICSAP",
    shape = "Raça"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Combinando gráficos
combined_plot <- (transf + icsap) & 
  theme(legend.position = "bottom")

## Salvando gráficos
ggsave("transf.jpg", transf,
       width = 15, height = 10, units = "cm", dpi = 600)

ggsave("icsap.jpg", icsap,
       width = 15, height = 10, units = "cm", dpi = 600)

ggsave("indicadores.jpg", combined_plot, 
       width = 23, height = 11, units = "cm", dpi = 1200)
