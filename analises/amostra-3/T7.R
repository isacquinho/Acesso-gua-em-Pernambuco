# ============================================================================
# ANALISE DE ABASTECIMENTO DE AGUA EM PERNAMBUCO
# MODELAGEM ESTATISTICA: MQO LINEAR, LOG-LOG E BETA
# ============================================================================

# ----------------------------------------------------------------------------
# 0. INSTALACAO E CARREGAMENTO DE PACOTES
# ----------------------------------------------------------------------------

pacotes <- c("readxl", "writexl", "ggplot2", "dplyr", "tidyr", "car", "nortest", "gridExtra", "patchwork", "lmtest", "betareg")

for(p in pacotes) {
  if(!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

set.seed(42)
if(!dir.exists("graficos")) dir.create("graficos")

# ----------------------------------------------------------------------------
# 1. FUNCOES AUXILIARES
# ----------------------------------------------------------------------------

limpar_nome_municipio <- function(nome) {
  if(is.na(nome)) return("")
  nome <- toupper(trimws(as.character(nome)))
  nome <- gsub(" - AGRESTE CENTRAL", "", nome)
  nome <- gsub(" - MATA", "", nome)
  nome <- gsub(" - METROPOLITANA", "", nome)
  nome <- gsub("POMBOS - AGRESTE CENTRAL", "POMBOS", nome)
  nome <- gsub("'", "", nome)
  nome <- gsub("`", "", nome)
  return(nome)
}

# ----------------------------------------------------------------------------
# 2. CARREGAMENTO DOS DADOS
# ----------------------------------------------------------------------------

carregar_dados_completos <- function() {
  
  arquivos_necessarios <- c(
    "ABASTECIMENTO_AGUA_POR_COR_RACA(PE).xlsx",
    "PIB_CIDADES_DE_PERNAMBUCO_2022.xlsx",
    "amostra_fixa_municipios.xlsx"
  )
  
  for(arq in arquivos_necessarios) {
    if(!file.exists(arq)) {
      stop(paste("Arquivo nao encontrado:", arq))
    }
  }
  
  df_agua <- read_excel("ABASTECIMENTO_AGUA_POR_COR_RACA(PE).xlsx", 
                        sheet = "MORADORES ABASTECIMENTO DE AGUA")
  
  if(ncol(df_agua) >= 5) {
    names(df_agua)[1:5] <- c("MUNICIPIO", "COR_RACA", "TOTAL_MORADORES", "POSSUI_ACESSO", "NAO_POSSUI_ACESSO")
  }
  
  df_agua <- df_agua %>%
    tidyr::fill(MUNICIPIO, .direction = "down") %>%
    filter(!is.na(COR_RACA), trimws(COR_RACA) != "") %>%
    mutate(
      COR_RACA = case_when(
        tolower(COR_RACA) %in% c("branca", "branco") ~ "Branca",
        tolower(COR_RACA) %in% c("preta", "preto") ~ "Preta",
        tolower(COR_RACA) %in% c("parda", "pardo") ~ "Parda",
        tolower(COR_RACA) %in% c("amarela", "amarelo") ~ "Amarela",
        tolower(COR_RACA) %in% c("indigena", "indígena") ~ "Indigena",
        TRUE ~ COR_RACA
      ),
      MUNICIPIO = sapply(MUNICIPIO, limpar_nome_municipio),
      POSSUI_ACESSO = as.numeric(POSSUI_ACESSO),
      NAO_POSSUI_ACESSO = as.numeric(NAO_POSSUI_ACESSO),
      TOTAL_MORADORES = as.numeric(TOTAL_MORADORES),
      PCT_ACESSO = (POSSUI_ACESSO / TOTAL_MORADORES) * 100
    ) %>%
    filter(
      COR_RACA %in% c("Branca", "Preta", "Parda", "Amarela", "Indigena"),
      MUNICIPIO != "PERNAMBUCO",
      MUNICIPIO != "PE",
      !is.na(PCT_ACESSO),
      PCT_ACESSO >= 0,
      PCT_ACESSO <= 100
    )
  
  df_pib <- read_excel("PIB_CIDADES_DE_PERNAMBUCO_2022.xlsx")
  names(df_pib)[1:2] <- c("MUNICIPIO", "PIB_PER_CAPITA")
  df_pib$MUNICIPIO <- sapply(df_pib$MUNICIPIO, limpar_nome_municipio)
  df_pib$PIB_PER_CAPITA <- as.numeric(df_pib$PIB_PER_CAPITA)
  df_pib <- df_pib[!is.na(df_pib$MUNICIPIO) & !is.na(df_pib$PIB_PER_CAPITA), ]
  
  df_wide <- df_agua %>%
    select(MUNICIPIO, COR_RACA, PCT_ACESSO) %>%
    pivot_wider(
      names_from = COR_RACA, 
      values_from = PCT_ACESSO, 
      names_prefix = "ACESSO_"
    )
  
  df_completo <- df_wide %>%
    inner_join(df_pib, by = "MUNICIPIO") %>%
    mutate(DIFERENCA_RACIAL = ACESSO_Branca - ACESSO_Preta)
  
  municipios_agua <- unique(df_agua$MUNICIPIO)
  municipios_pib <- unique(df_pib$MUNICIPIO)
  municipios_sem_pib <- municipios_agua[!municipios_agua %in% municipios_pib]
  
  return(list(
    agua_original = df_agua,
    df_completo = df_completo,
    df_pib = df_pib,
    df_wide = df_wide,
    municipios_sem_pib = municipios_sem_pib
  ))
}

# ----------------------------------------------------------------------------
# 3. CARREGAR AMOSTRA FIXA
# ----------------------------------------------------------------------------

carregar_amostra_fixa <- function() {
  
  df_amostra_municipios <- read_excel("amostra_fixa_municipios.xlsx")
  
  if("MUNICIPIO" %in% names(df_amostra_municipios)) {
    municipios <- df_amostra_municipios$MUNICIPIO
  } else {
    municipios <- df_amostra_municipios[,1]
  }
  
  municipios <- sapply(municipios, limpar_nome_municipio)
  
  return(municipios)
}

# ----------------------------------------------------------------------------
# 4. FUNCAO PARA IDENTIFICAR MUNICIPIOS COM DADOS FALTANTES POR RACA
# ----------------------------------------------------------------------------

identificar_faltantes_por_raca <- function(df_amostra, municipios_amostra) {
  
  racas <- c("Branca", "Preta", "Parda", "Amarela", "Indigena")
  colunas_raca <- paste0("ACESSO_", racas)
  
  colunas_existentes <- colunas_raca[colunas_raca %in% names(df_amostra)]
  racas_existentes <- racas[colunas_raca %in% colunas_existentes]
  
  lista_faltantes <- list()
  
  for(i in 1:length(racas_existentes)) {
    raca <- racas_existentes[i]
    coluna <- colunas_existentes[i]
    municipios_na <- df_amostra$MUNICIPIO[is.na(df_amostra[[coluna]])]
    
    if(length(municipios_na) > 0) {
      lista_faltantes[[raca]] <- municipios_na
    } else {
      lista_faltantes[[raca]] <- character(0)
    }
  }
  
  matriz_presenca <- df_amostra %>%
    select(MUNICIPIO, all_of(colunas_existentes)) %>%
    mutate(across(starts_with("ACESSO_"), ~ ifelse(is.na(.), "FALTANTE", "PRESENTE")))
  
  for(raca in racas_existentes) {
    col_antiga <- paste0("ACESSO_", raca)
    col_nova <- raca
    if(col_antiga %in% names(matriz_presenca)) {
      names(matriz_presenca)[names(matriz_presenca) == col_antiga] <- col_nova
    }
  }
  
  write_xlsx(matriz_presenca, "detalhamento_dados_faltantes.xlsx")
  
  return(list(
    faltantes_por_raca = lista_faltantes,
    matriz_presenca = matriz_presenca
  ))
}

# ----------------------------------------------------------------------------
# 5. DIAGNOSTICO DA AMOSTRA
# ----------------------------------------------------------------------------

diagnosticar_amostra <- function(df_completo, df_wide, df_pib, municipios_amostra) {
  
  df_completo_df <- df_completo
  df_wide_df <- df_wide
  df_pib_df <- df_pib
  
  municipios_banco <- df_completo_df$MUNICIPIO
  encontrados <- municipios_amostra[municipios_amostra %in% municipios_banco]
  nao_encontrados <- municipios_amostra[!municipios_amostra %in% municipios_banco]
  
  colunas_raca <- c("ACESSO_Branca", "ACESSO_Preta", "ACESSO_Parda", 
                    "ACESSO_Amarela", "ACESSO_Indigena")
  colunas_existentes <- colunas_raca[colunas_raca %in% names(df_completo_df)]
  
  if(length(encontrados) > 0) {
    df_filtrado <- df_completo_df %>%
      filter(MUNICIPIO %in% encontrados)
    
    df_long_temp <- df_filtrado %>%
      select(all_of(colunas_existentes)) %>%
      pivot_longer(everything(), names_to = "RACA", values_to = "PCT_ACESSO") %>%
      mutate(RACA = gsub("ACESSO_", "", RACA)) %>%
      filter(!is.na(PCT_ACESSO))
    
    obs_por_raca <- table(df_long_temp$RACA)
    total_obs <- sum(obs_por_raca)
    
    resultados_faltantes <- identificar_faltantes_por_raca(df_filtrado, encontrados)
    
  } else {
    total_obs <- 0
    obs_por_raca <- NULL
    resultados_faltantes <- NULL
  }
  
  diagnostico_list <- list(
    Resumo = data.frame(
      Municipios_Na_Amostra = length(municipios_amostra),
      Municipios_Encontrados = length(encontrados),
      Municipios_Nao_Encontrados = length(nao_encontrados),
      Total_Observacoes_Esperadas = length(encontrados) * 5,
      Total_Observacoes_Reais = total_obs,
      Diferenca = (length(encontrados) * 5) - total_obs
    ),
    Municipios_Nao_Encontrados = if(length(nao_encontrados) > 0) {
      data.frame(MUNICIPIO = nao_encontrados)
    } else {
      data.frame(Mensagem = "Todos os municipios foram encontrados")
    },
    Observacoes_Por_Raca = if(!is.null(obs_por_raca)) as.data.frame(obs_por_raca) else data.frame(),
    Municipios_Encontrados = data.frame(MUNICIPIO = encontrados),
    Detalhamento_Faltantes = if(!is.null(resultados_faltantes)) resultados_faltantes$matriz_presenca else data.frame()
  )
  
  write_xlsx(diagnostico_list, "diagnostico_amostra.xlsx")
  
  return(list(
    encontrados = encontrados,
    nao_encontrados = nao_encontrados,
    total_obs = total_obs,
    obs_por_raca = obs_por_raca,
    faltantes = resultados_faltantes
  ))
}

# ============================================================================
# 6. FUNCOES PARA GERACAO DE GRAFICOS
# ============================================================================

criar_boxplot_acesso <- function(df_long, estatisticas) {
  
  n_municipios <- length(unique(df_long$MUNICIPIO))
  
  ordem_racas <- estatisticas %>%
    arrange(desc(Media)) %>%
    pull(RACA)
  
  df_long$RACA <- factor(df_long$RACA, levels = ordem_racas)
  
  medias <- estatisticas %>%
    filter(RACA %in% ordem_racas) %>%
    mutate(RACA = factor(RACA, levels = ordem_racas)) %>%
    arrange(RACA)
  
  p <- ggplot(df_long, aes(x = RACA, y = PCT_ACESSO, fill = RACA)) +
    geom_boxplot(alpha = 0.8, outlier.size = 1.5, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
    labs(
      title = "Distribuição do acesso à água por raça/cor",
      subtitle = paste0("Municípios analisados: ", n_municipios, 
                       " | Total de observações: ", nrow(df_long)),
      y = "Percentual de acesso (%)",
      x = "Raça/Cor",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    scale_fill_manual(
      values = c("Branca" = "#4169E1", "Preta" = "#8B4513", "Parda" = "#DAA520", 
                 "Amarela" = "#FFD700", "Indigena" = "#228B22"),
      name = "Raça/Cor"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  y_max <- max(df_long$PCT_ACESSO, na.rm = TRUE)
  for(i in 1:nrow(medias)) {
    y_pos <- min(medias$Media[i] + 5, y_max - 2)
    if(y_pos > y_max - 2) y_pos <- y_max - 2
    p <- p + annotate("text", 
                     x = i, 
                     y = y_pos, 
                     label = sprintf("Média: %.1f%%", medias$Media[i]),
                     size = 3, 
                     fontface = "bold",
                     color = "darkred")
  }
  
  ggsave("graficos/boxplot_acesso_por_raca.png", plot = p, width = 10, height = 7, dpi = 300)
  
  return(p)
}

criar_ranking_barras <- function(df_ranking) {
  
  cores_classificacao <- c(
    "Excelente" = "#2E8B57",
    "Bom" = "#4682B4", 
    "Regular" = "#FFA500",
    "Ruim" = "#CD5C5C",
    "Critico" = "#8B0000"
  )
  
  top10 <- df_ranking %>%
    head(10) %>%
    mutate(MUNICIPIO = factor(MUNICIPIO, levels = rev(MUNICIPIO)))
  
  p_top <- ggplot(top10, aes(x = MUNICIPIO, y = ACESSO_MEDIO, fill = CLASSIFICACAO)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", ACESSO_MEDIO)), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = cores_classificacao, name = "Classificação") +
    scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    labs(
      title = "TOP 10 MUNICÍPIOS - MELHOR ACESSO À ÁGUA",
      subtitle = "Maiores percentuais médios de acesso considerando todas as raças",
      x = "", 
      y = "Percentual médio de acesso (%)",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#2E8B57"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.title.x = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom"
    )
  
  bottom10 <- df_ranking %>%
    tail(10) %>%
    mutate(MUNICIPIO = factor(MUNICIPIO, levels = rev(MUNICIPIO)))
  
  p_bottom <- ggplot(bottom10, aes(x = MUNICIPIO, y = ACESSO_MEDIO, fill = CLASSIFICACAO)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", ACESSO_MEDIO)), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = cores_classificacao, name = "Classificação") +
    scale_y_continuous(limits = c(0, max(bottom10$ACESSO_MEDIO) + 10), expand = c(0, 0)) +
    labs(
      title = "BOTTOM 10 MUNICÍPIOS - PIOR ACESSO À ÁGUA",
      subtitle = "Menores percentuais médios de acesso considerando todas as raças",
      x = "", 
      y = "Percentual médio de acesso (%)",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#CD5C5C"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.title.x = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom"
    )
  
  df_ranking_completo <- df_ranking %>%
    mutate(
      cor_flag = ifelse(ACESSO_MEDIO >= 80, "Excelente",
                 ifelse(ACESSO_MEDIO >= 65, "Bom",
                 ifelse(ACESSO_MEDIO >= 50, "Regular",
                 ifelse(ACESSO_MEDIO >= 35, "Ruim", "Critico")))),
      cor_flag = factor(cor_flag, levels = c("Excelente", "Bom", "Regular", "Ruim", "Critico"))
    )
  
  p_completo <- ggplot(df_ranking_completo, aes(x = reorder(MUNICIPIO, ACESSO_MEDIO), y = ACESSO_MEDIO)) +
    geom_segment(aes(xend = MUNICIPIO, yend = 0), color = "gray70", alpha = 0.5) +
    geom_point(aes(color = cor_flag), size = 2.5, alpha = 0.8) +
    coord_flip() +
    scale_color_manual(values = cores_classificacao, name = "Classificação") +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
    labs(
      title = "RANKING COMPLETO DE ACESSO À ÁGUA POR MUNICÍPIO",
      subtitle = paste0("Total de municípios analisados: ", nrow(df_ranking)),
      x = "", 
      y = "Percentual médio de acesso (%)",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.text.y = element_text(size = 6),
      axis.title.x = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8)
    )
  
  ggsave("graficos/ranking_top10_municipios.png", plot = p_top, width = 10, height = 6, dpi = 300)
  ggsave("graficos/ranking_bottom10_municipios.png", plot = p_bottom, width = 10, height = 6, dpi = 300)
  ggsave("graficos/ranking_completo_municipios.png", plot = p_completo, width = 14, height = 12, dpi = 300)
  
  return(list(top = p_top, bottom = p_bottom, completo = p_completo))
}

criar_histograma_diferenca <- function(df_amostra) {
  
  df_diff <- df_amostra %>%
    filter(!is.na(DIFERENCA_RACIAL))
  
  if(nrow(df_diff) == 0) {
    return(NULL)
  }
  
  media_diff <- mean(df_diff$DIFERENCA_RACIAL)
  mediana_diff <- median(df_diff$DIFERENCA_RACIAL)
  sd_diff <- sd(df_diff$DIFERENCA_RACIAL)
  n_municipios <- nrow(df_diff)
  
  municipio_maior_desigualdade <- df_diff %>%
    filter(DIFERENCA_RACIAL == max(DIFERENCA_RACIAL)) %>%
    pull(MUNICIPIO)
  
  municipio_menor_desigualdade <- df_diff %>%
    filter(DIFERENCA_RACIAL == min(DIFERENCA_RACIAL)) %>%
    pull(MUNICIPIO)
  
  valor_max <- max(df_diff$DIFERENCA_RACIAL)
  valor_min <- min(df_diff$DIFERENCA_RACIAL)
  
  p <- ggplot(df_diff, aes(x = DIFERENCA_RACIAL)) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 15, 
                   fill = "steelblue", 
                   color = "black", 
                   alpha = 0.7) +
    geom_density(color = "darkred", linewidth = 1.2, alpha = 0.3) +
    geom_vline(xintercept = 0, color = "darkgreen", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = media_diff, color = "darkred", linetype = "solid", linewidth = 1.2) +
    scale_x_continuous(breaks = seq(floor(min(df_diff$DIFERENCA_RACIAL)/10)*10, 
                                     ceiling(max(df_diff$DIFERENCA_RACIAL)/10)*10, 10)) +
    labs(
      title = "Desigualdade racial no acesso à água em Pernambuco",
      subtitle = paste0("Distribuição da diferença (Brancos - Pretos) entre ", n_municipios, " municípios"),
      x = "Diferença percentual (Brancos - Pretos)",
      y = "Densidade",
      caption = paste0(
        "Fonte: Dados de abastecimento de água de Pernambuco\n",
        "Município com maior desigualdade: ", municipio_maior_desigualdade[1], " (", sprintf("%.1f", valor_max), "%)\n",
        "Município com menor desigualdade: ", municipio_menor_desigualdade[1], " (", sprintf("%.1f", valor_min), "%)"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 7, hjust = 0, color = "gray50", lineheight = 1.2),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  dens_data <- density(df_diff$DIFERENCA_RACIAL, na.rm = TRUE)
  dens_max <- max(dens_data$y)
  
  p <- p +
    annotate("text", 
             x = media_diff + 3, 
             y = dens_max * 0.9,
             label = sprintf("Média = %.1f%%", media_diff),
             color = "darkred", 
             fontface = "bold", 
             size = 4,
             hjust = 0) +
    annotate("text", 
             x = media_diff + 3, 
             y = dens_max * 0.8,
             label = sprintf("Mediana = %.1f%%", mediana_diff),
             color = "darkorange", 
             fontface = "bold", 
             size = 4,
             hjust = 0) +
    annotate("text", 
             x = 0 + 2, 
             y = dens_max * 0.7,
             label = "Igualdade racial",
             color = "darkgreen", 
             fontface = "bold", 
             size = 3.5,
             angle = 90,
             hjust = 0) +
    annotate("rect", 
             xmin = media_diff - sd_diff, 
             xmax = media_diff + sd_diff, 
             ymin = 0, 
             ymax = dens_max * 0.25,
             alpha = 0.2, 
             fill = "gray") +
    annotate("text", 
             x = media_diff, 
             y = dens_max * 0.2,
             label = paste0("±1 dp (", sprintf("%.1f", sd_diff), "%)"),
             size = 3,
             color = "gray30",
             hjust = 0.5)
  
  ggsave("graficos/histograma_diferenca_racial.png", plot = p, width = 12, height = 8, dpi = 300)
  
  t_test <- t.test(df_diff$DIFERENCA_RACIAL, mu = 0)
  
  return(p)
}

criar_barras_empilhadas_acesso <- function(tabela_contingencia) {
  
  df_barras1 <- tabela_contingencia %>%
    pivot_longer(c(POSSUI, NAO_POSSUI), names_to = "SITUACAO", values_to = "PESSOAS") %>%
    group_by(COR_RACA) %>%
    mutate(Percentual = PESSOAS / sum(PESSOAS) * 100,
           Situacao_Legivel = ifelse(SITUACAO == "POSSUI", "Possui acesso", "Não possui acesso"))
  
  ordem_racas <- df_barras1 %>%
    filter(SITUACAO == "POSSUI") %>%
    arrange(desc(Percentual)) %>%
    pull(COR_RACA)
  
  df_barras1$COR_RACA <- factor(df_barras1$COR_RACA, levels = ordem_racas)
  
  p1 <- ggplot(df_barras1, aes(x = COR_RACA, y = Percentual, fill = Situacao_Legivel)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", Percentual)), 
              position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("Possui acesso" = "#2E8B57", "Não possui acesso" = "#CD5C5C"),
                      name = "Situação") +
    labs(
      title = "Proporção de acesso à água por raça/cor",
      subtitle = "Percentual de pessoas com e sem acesso à água em cada grupo racial",
      y = "Percentual (%)", 
      x = "Raça/Cor",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  ggsave("graficos/barras_empilhadas_acesso_vs_nao_acesso.png", plot = p1, width = 10, height = 7, dpi = 300)
  
  return(p1)
}

criar_barras_empilhadas_qualidade <- function(df_amostra) {
  
  colunas_acesso <- names(df_amostra)[grepl("^ACESSO_", names(df_amostra))]
  
  if(length(colunas_acesso) == 0) {
    return(NULL)
  }
  
  df_qualidade <- df_amostra %>%
    select(MUNICIPIO, all_of(colunas_acesso)) %>%
    pivot_longer(-MUNICIPIO, names_to = "RACA", values_to = "PCT_ACESSO") %>%
    mutate(RACA = gsub("ACESSO_", "", RACA),
           CLASSIFICACAO = case_when(
             PCT_ACESSO >= 80 ~ "Excelente",
             PCT_ACESSO >= 65 ~ "Bom",
             PCT_ACESSO >= 50 ~ "Regular",
             PCT_ACESSO >= 35 ~ "Ruim",
             TRUE ~ "Critico"
           )) %>%
    filter(!is.na(PCT_ACESSO))
  
  df_barras2 <- df_qualidade %>%
    group_by(RACA, CLASSIFICACAO) %>%
    summarise(Contagem = n(), .groups = "drop") %>%
    group_by(RACA) %>%
    mutate(Percentual = Contagem / sum(Contagem) * 100)
  
  ordem_racas_qualidade <- df_barras2 %>%
    filter(CLASSIFICACAO %in% c("Excelente", "Bom")) %>%
    group_by(RACA) %>%
    summarise(Pct_Bom_Excelente = sum(Percentual), .groups = "drop") %>%
    arrange(desc(Pct_Bom_Excelente)) %>%
    pull(RACA)
  
  if(length(ordem_racas_qualidade) > 0) {
    df_barras2$RACA <- factor(df_barras2$RACA, levels = ordem_racas_qualidade)
  }
  
  ordem_categorias <- c("Excelente", "Bom", "Regular", "Ruim", "Critico")
  df_barras2$CLASSIFICACAO <- factor(df_barras2$CLASSIFICACAO, levels = ordem_categorias)
  
  cores_qualidade <- c("Excelente" = "#2E8B57", "Bom" = "#4682B4", 
                       "Regular" = "#FFA500", "Ruim" = "#CD5C5C", "Critico" = "#8B0000")
  
  p2 <- ggplot(df_barras2, aes(x = RACA, y = Percentual, fill = CLASSIFICACAO)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", Percentual)), 
              position = position_stack(vjust = 0.5), size = 3, fontface = "bold") +
    scale_fill_manual(values = cores_qualidade, name = "Classificação") +
    labs(
      title = "Qualidade do acesso à água por raça/cor",
      subtitle = "Distribuição dos municípios por nível de acesso em cada grupo racial",
      y = "Percentual de municípios (%)", 
      x = "Raça/Cor",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  ggsave("graficos/barras_empilhadas_qualidade_por_raca.png", plot = p2, width = 10, height = 7, dpi = 300)
  
  return(p2)
}

criar_barras_empilhadas_composicao <- function(tabela_contingencia) {
  
  df_composicao_acesso <- tabela_contingencia %>%
    select(COR_RACA, POSSUI) %>%
    mutate(Percentual = POSSUI / sum(POSSUI) * 100) %>%
    arrange(desc(Percentual))
  
  p3_acesso <- ggplot(df_composicao_acesso, aes(x = "População com acesso", y = Percentual, fill = COR_RACA)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%\n(%s pessoas)", Percentual, format(POSSUI, big.mark = "."))), 
              position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("Branca" = "#4169E1", "Preta" = "#8B4513", 
                                  "Parda" = "#DAA520", "Amarela" = "#FFD700", 
                                  "Indigena" = "#228B22"),
                      name = "Raça/Cor") +
    labs(
      title = "Composição racial da população COM acesso à água",
      subtitle = paste0("Total: ", format(sum(tabela_contingencia$POSSUI), big.mark = "."), " pessoas"),
      y = "Percentual (%)", 
      x = "",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  ggsave("graficos/barras_empilhadas_composicao_com_acesso.png", plot = p3_acesso, width = 10, height = 6, dpi = 300)
  
  df_composicao_sem_acesso <- tabela_contingencia %>%
    select(COR_RACA, NAO_POSSUI) %>%
    mutate(Percentual = NAO_POSSUI / sum(NAO_POSSUI) * 100) %>%
    arrange(desc(Percentual))
  
  p3_sem_acesso <- ggplot(df_composicao_sem_acesso, aes(x = "População sem acesso", y = Percentual, fill = COR_RACA)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%\n(%s pessoas)", Percentual, format(NAO_POSSUI, big.mark = "."))), 
              position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("Branca" = "#4169E1", "Preta" = "#8B4513", 
                                  "Parda" = "#DAA520", "Amarela" = "#FFD700", 
                                  "Indigena" = "#228B22"),
                      name = "Raça/Cor") +
    labs(
      title = "Composição racial da população SEM acesso à água",
      subtitle = paste0("Total: ", format(sum(tabela_contingencia$NAO_POSSUI), big.mark = "."), " pessoas"),
      y = "Percentual (%)", 
      x = "",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  ggsave("graficos/barras_empilhadas_composicao_sem_acesso.png", plot = p3_sem_acesso, width = 10, height = 6, dpi = 300)
  
  return(list(com_acesso = p3_acesso, sem_acesso = p3_sem_acesso))
}

criar_barras_empilhadas_distribuicao <- function(df_ranking) {
  
  df_distribuicao <- df_ranking %>%
    group_by(CLASSIFICACAO) %>%
    summarise(Contagem = n(), .groups = "drop") %>%
    mutate(Percentual = Contagem / sum(Contagem) * 100)
  
  ordem_categorias <- c("Excelente", "Bom", "Regular", "Ruim", "Critico")
  df_distribuicao$CLASSIFICACAO <- factor(df_distribuicao$CLASSIFICACAO, levels = ordem_categorias)
  
  cores_dist <- c("Excelente" = "#2E8B57", "Bom" = "#4682B4", 
                  "Regular" = "#FFA500", "Ruim" = "#CD5C5C", "Critico" = "#8B0000")
  
  p4 <- ggplot(df_distribuicao, aes(x = "Todos os municípios", y = Percentual, fill = CLASSIFICACAO)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%\n(%d municípios)", Percentual, Contagem)), 
              position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
    scale_fill_manual(values = cores_dist, name = "Classificação") +
    labs(
      title = "Distribuição geral da qualidade do acesso à água",
      subtitle = paste0("Total de municípios analisados: ", nrow(df_ranking)),
      y = "Percentual de municípios (%)", 
      x = "",
      caption = "Fonte: Dados de abastecimento de água de Pernambuco\nMédia do acesso considerando todas as raças"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  ggsave("graficos/barras_empilhadas_distribuicao_geral.png", plot = p4, width = 10, height = 6, dpi = 300)
  
  return(p4)
}

criar_dashboard_graficos <- function(df_long, df_ranking, df_amostra, estatisticas, tabela_contingencia) {
  
  p_boxplot <- criar_boxplot_acesso(df_long, estatisticas)
  p_histograma <- criar_histograma_diferenca(df_amostra)
  p_ranking <- criar_ranking_barras(df_ranking)
  p_barras1 <- criar_barras_empilhadas_acesso(tabela_contingencia)
  p_barras2 <- criar_barras_empilhadas_qualidade(df_amostra)
  p_barras3 <- criar_barras_empilhadas_composicao(tabela_contingencia)
  p_barras4 <- criar_barras_empilhadas_distribuicao(df_ranking)
  
  if(!is.null(p_histograma)) {
    dashboard_principal <- (p_boxplot + p_histograma) + 
      plot_layout(widths = c(1, 1)) +
      plot_annotation(
        title = "ANÁLISE DO ACESSO À ÁGUA EM PERNAMBUCO",
        theme = theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        )
      )
    
    ggsave("graficos/dashboard_principal.png", plot = dashboard_principal, width = 16, height = 8, dpi = 300)
  }
  
  dashboard_barras <- (p_barras1 + p_barras2) + 
    plot_layout(widths = c(1, 1)) +
    plot_annotation(
      title = "ANÁLISE DE ACESSO À ÁGUA POR RAÇA",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
    )
  
  ggsave("graficos/dashboard_barras_acesso_qualidade.png", plot = dashboard_barras, width = 14, height = 8, dpi = 300)
  
  return(list(
    boxplot = p_boxplot,
    histograma = p_histograma,
    ranking = p_ranking,
    barras_acesso = p_barras1,
    barras_qualidade = p_barras2,
    barras_composicao = p_barras3,
    barras_distribuicao = p_barras4
  ))
}

# ============================================================================
# MODELO 1: REGRESSAO LINEAR (MQO)
# ============================================================================

realizar_modelo_linear <- function(df_amostra, df_agua_original) {
  
  pib_municipio <- df_amostra %>% select(MUNICIPIO, PIB_PER_CAPITA)
  
  df_regressao <- df_agua_original %>%
    left_join(pib_municipio, by = "MUNICIPIO") %>%
    filter(!is.na(PIB_PER_CAPITA)) %>%
    select(MUNICIPIO, COR_RACA, PCT_ACESSO, PIB_PER_CAPITA) %>%
    drop_na()
  
  df_regressao <- df_regressao %>%
    mutate(
      RACA_Preta = ifelse(COR_RACA == "Preta", 1, 0),
      RACA_Parda = ifelse(COR_RACA == "Parda", 1, 0),
      RACA_Amarela = ifelse(COR_RACA == "Amarela", 1, 0),
      RACA_Indigena = ifelse(COR_RACA == "Indigena", 1, 0)
    )
  
  modelo_linear <- lm(PCT_ACESSO ~ RACA_Preta + RACA_Parda + RACA_Amarela + RACA_Indigena + PIB_PER_CAPITA, 
                      data = df_regressao)
  
  print(summary(modelo_linear))
  
  r2_ajustado_linear <- summary(modelo_linear)$adj.r.squared
  aic_linear <- AIC(modelo_linear)
  
  residuos_linear <- residuals(modelo_linear)
  shapiro_linear <- shapiro.test(residuos_linear[1:min(5000, length(residuos_linear))])
  bp_linear <- bptest(modelo_linear)
  
  df_res <- data.frame(Ajustados = fitted(modelo_linear), Residuos = residuos_linear)
  p_res <- ggplot(df_res, aes(x = Ajustados, y = Residuos)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
    geom_smooth(method = "loess", se = TRUE, color = "darkgreen", alpha = 0.2) +
    labs(title = "Modelo Linear - Resíduos vs Valores Ajustados",
         x = "Valores Ajustados (%)", y = "Resíduos") +
    theme_minimal()
  ggsave("graficos/modelo_linear_residuos.png", plot = p_res, width = 8, height = 6, dpi = 300)
  
  return(list(
    modelo = modelo_linear,
    r2_ajustado = r2_ajustado_linear,
    aic = aic_linear,
    shapiro_p = shapiro_linear$p.value,
    bp_p = bp_linear$p.value,
    df_dados = df_regressao
  ))
}

# ============================================================================
# MODELO 2: REGRESSAO LOG-LOG
# ============================================================================

realizar_modelo_loglog <- function(df_dados) {
  
  df_log <- df_dados %>%
    mutate(
      ln_ACESSO = log(PCT_ACESSO + 1),
      ln_PIB = log(PIB_PER_CAPITA)
    )
  
  modelo_loglog <- lm(ln_ACESSO ~ RACA_Preta + RACA_Parda + RACA_Amarela + RACA_Indigena + ln_PIB, 
                      data = df_log)
  
  print(summary(modelo_loglog))
  
  r2_ajustado_loglog <- summary(modelo_loglog)$adj.r.squared
  aic_loglog <- AIC(modelo_loglog)
  
  residuos_loglog <- residuals(modelo_loglog)
  shapiro_loglog <- shapiro.test(residuos_loglog[1:min(5000, length(residuos_loglog))])
  bp_loglog <- bptest(modelo_loglog)
  
  df_res <- data.frame(Ajustados = fitted(modelo_loglog), Residuos = residuos_loglog)
  p_res <- ggplot(df_res, aes(x = Ajustados, y = Residuos)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
    geom_smooth(method = "loess", se = TRUE, color = "darkgreen", alpha = 0.2) +
    labs(title = "Modelo Log-Log - Resíduos vs Valores Ajustados",
         x = "Valores Ajustados (log)", y = "Resíduos") +
    theme_minimal()
  ggsave("graficos/modelo_loglog_residuos.png", plot = p_res, width = 8, height = 6, dpi = 300)
  
  return(list(
    modelo = modelo_loglog,
    df_log = df_log,
    r2_ajustado = r2_ajustado_loglog,
    aic = aic_loglog,
    shapiro_p = shapiro_loglog$p.value,
    bp_p = bp_loglog$p.value
  ))
}

# ============================================================================
# MODELO 3: REGRESSAO BETA
# ============================================================================

realizar_modelo_beta <- function(df_dados) {
  
  n_obs <- nrow(df_dados)
  
  df_beta <- df_dados %>%
    mutate(
      PROPORCAO = PCT_ACESSO / 100,
      PROPORCAO_CORRIGIDA = (PROPORCAO * (n_obs - 1) + 0.5) / n_obs
    )
  
  modelo_beta <- betareg(PROPORCAO_CORRIGIDA ~ RACA_Preta + RACA_Parda + RACA_Amarela + RACA_Indigena + PIB_PER_CAPITA,
                         data = df_beta,
                         link = "logit")
  
  print(summary(modelo_beta))
  
  aic_beta <- AIC(modelo_beta)
  
  residuos_beta <- residuals(modelo_beta, type = "sweighted2")
  shapiro_beta <- shapiro.test(residuos_beta[1:min(5000, length(residuos_beta))])
  
  df_res <- data.frame(Ajustados = fitted(modelo_beta), Residuos = residuos_beta)
  p_res <- ggplot(df_res, aes(x = Ajustados, y = Residuos)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
    geom_smooth(method = "loess", se = TRUE, color = "darkgreen", alpha = 0.2) +
    labs(title = "Modelo Beta - Resíduos vs Valores Ajustados",
         x = "Valores Ajustados (logit)", y = "Resíduos") +
    theme_minimal()
  ggsave("graficos/modelo_beta_residuos.png", plot = p_res, width = 8, height = 6, dpi = 300)
  
  return(list(
    modelo = modelo_beta,
    aic = aic_beta,
    pseudo_r2 = modelo_beta$pseudo.r.squared,
    shapiro_p = shapiro_beta$p.value
  ))
}

# ============================================================================
# ANALISES ESTATISTICAS PRINCIPAL
# ============================================================================

realizar_analises <- function(df_completo, df_agua_original, diagnosticos) {
  
  if(length(diagnosticos$encontrados) == 0) {
    return(NULL)
  }
  
  df_amostra <- df_completo %>%
    filter(MUNICIPIO %in% diagnosticos$encontrados)
  
  agua_amostra <- df_agua_original %>%
    filter(MUNICIPIO %in% diagnosticos$encontrados)
  
  colunas_raca <- c("ACESSO_Branca", "ACESSO_Preta", "ACESSO_Parda", 
                    "ACESSO_Amarela", "ACESSO_Indigena")
  colunas_existentes <- colunas_raca[colunas_raca %in% names(df_amostra)]
  
  df_long <- df_amostra %>%
    select(MUNICIPIO, all_of(colunas_existentes)) %>%
    pivot_longer(-MUNICIPIO, names_to = "RACA", values_to = "PCT_ACESSO") %>%
    mutate(RACA = gsub("ACESSO_", "", RACA)) %>%
    filter(!is.na(PCT_ACESSO))
  
  estatisticas <- df_long %>%
    group_by(RACA) %>%
    summarise(
      n = n(),
      Media = mean(PCT_ACESSO),
      Mediana = median(PCT_ACESSO),
      Desvio_Padrao = sd(PCT_ACESSO),
      .groups = "drop"
    ) %>%
    arrange(desc(Media))
  
  print(estatisticas, digits = 4)
  
  modelo_anova <- aov(PCT_ACESSO ~ RACA, data = df_long)
  print(summary(modelo_anova))
  
  tabela_contingencia <- agua_amostra %>%
    group_by(COR_RACA) %>%
    summarise(
      POSSUI = sum(POSSUI_ACESSO, na.rm = TRUE),
      NAO_POSSUI = sum(NAO_POSSUI_ACESSO, na.rm = TRUE),
      .groups = "drop"
    )
  
  matriz <- as.matrix(tabela_contingencia[, c("POSSUI", "NAO_POSSUI")])
  rownames(matriz) <- tabela_contingencia$COR_RACA
  qui <- chisq.test(matriz)
  cat(paste("\nQui-quadrado: χ² =", sprintf("%.2f", qui$statistic), 
            ", gl =", qui$parameter, 
            ", p =", sprintf("%.6f", qui$p.value), "\n"))
  
  df_ranking <- df_amostra %>%
    mutate(
      ACESSO_MEDIO = rowMeans(select(., all_of(colunas_existentes)), na.rm = TRUE),
      CLASSIFICACAO = case_when(
        ACESSO_MEDIO >= 80 ~ "Excelente",
        ACESSO_MEDIO >= 65 ~ "Bom",
        ACESSO_MEDIO >= 50 ~ "Regular",
        ACESSO_MEDIO >= 35 ~ "Ruim",
        TRUE ~ "Critico"
      )
    ) %>%
    arrange(desc(ACESSO_MEDIO)) %>%
    mutate(POSICAO = row_number())
  
  cat("\nTOP 10:\n")
  print(df_ranking %>% head(10) %>% select(POSICAO, MUNICIPIO, ACESSO_MEDIO, CLASSIFICACAO))
  
  cat("\nBOTTOM 10:\n")
  print(df_ranking %>% tail(10) %>% select(POSICAO, MUNICIPIO, ACESSO_MEDIO, CLASSIFICACAO))
  
  graficos <- criar_dashboard_graficos(df_long, df_ranking, df_amostra, estatisticas, tabela_contingencia)
  
  linear_results <- realizar_modelo_linear(df_amostra, agua_amostra)
  loglog_results <- realizar_modelo_loglog(linear_results$df_dados)
  beta_results <- realizar_modelo_beta(linear_results$df_dados)
  
  comparacao <- data.frame(
    Modelo = c("MQO Linear", "MQO Log-Log", "Beta"),
    AIC = c(linear_results$aic, loglog_results$aic, beta_results$aic),
    R2_Ajustado_Pseudo = c(linear_results$r2_ajustado, loglog_results$r2_ajustado, beta_results$pseudo_r2),
    Shapiro_Wilk_p = c(linear_results$shapiro_p, loglog_results$shapiro_p, beta_results$shapiro_p),
    Breusch_Pagan_p = c(linear_results$bp_p, loglog_results$bp_p, NA)
  )
  
  comparacao <- comparacao %>% arrange(AIC)
  
  cat("\n=== COMPARACAO DE MODELOS ===\n")
  print(comparacao)
  
  resultados_list <- list(
    Estatisticas_Descritivas = estatisticas,
    ANOVA = as.data.frame(summary(modelo_anova)[[1]]),
    Qui_Quadrado = data.frame(Chi2 = qui$statistic, gl = qui$parameter, p_valor = qui$p.value),
    Ranking = df_ranking %>% select(POSICAO, MUNICIPIO, ACESSO_MEDIO, CLASSIFICACAO),
    Comparacao_Modelos = comparacao
  )
  
  write_xlsx(resultados_list, "resultado_analise_completa.xlsx")
  
  gama_coef <- summary(linear_results$modelo)$coefficients
  gama_coef_df <- data.frame(
    Variavel = rownames(gama_coef),
    Estimate = gama_coef[,1],
    Std_Error = gama_coef[,2],
    t_value = gama_coef[,3],
    p_value = gama_coef[,4]
  )
  
  beta_coef <- summary(beta_results$modelo)$coefficients$mean
  beta_coef_df <- data.frame(
    Variavel = rownames(beta_coef),
    Estimate = beta_coef[,1],
    Std_Error = beta_coef[,2],
    z_value = beta_coef[,3],
    p_value = beta_coef[,4]
  )
  
  write_xlsx(gama_coef_df, "resultado_modelo_linear.xlsx")
  write_xlsx(beta_coef_df, "resultado_modelo_beta.xlsx")
}

# ============================================================================
# EXECUCAO PRINCIPAL
# ============================================================================

executar_analise_completa <- function() {
  
  tryCatch({
    dados <- carregar_dados_completos()
    municipios_amostra <- carregar_amostra_fixa()
    
    diagnosticos <- diagnosticar_amostra(
      dados$df_completo, 
      dados$df_wide, 
      dados$df_pib, 
      municipios_amostra
    )
    
    realizar_analises(dados$df_completo, dados$agua_original, diagnosticos)
    
  }, error = function(e) {
    cat("\nERRO:", e$message, "\n")
  })
}

executar_analise_completa()