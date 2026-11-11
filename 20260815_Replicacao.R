suppressMessages({
  library(readxl)
  library(psych)
  library(GPArotation)
  library(MatchIt)
  library(MASS)
  library(rbounds)
  library(lmtest)
  library(sandwich)
  library(car)
  library(tidyr)
  library(dplyr)
})

setwd("")

arquivo <- read_excel("BANCO_UFPA.xlsx", .name_repair = "minimal")


# TEXTO PRINCIPAL | p. 9 | texto corrido

banco <- arquivo
c(entrevistas = nrow(banco),
  ufs = length(unique(banco$soc_1)),
  entrevistas_norte = sum(banco$soc_reg == 1))
c(inicio_do_campo = format(min(as.Date(banco$VStart)), "%d/%m/%Y"),
  fim_do_campo    = format(max(as.Date(banco$VEnd)),   "%d/%m/%Y"))
round(c(margem_de_erro_total = 100 * 1.96 * sqrt(0.25 / nrow(banco)),
        margem_de_erro_norte = 100 * 1.96 * sqrt(0.25 / sum(banco$soc_reg == 1))), 2)


# TEXTO PRINCIPAL | p. 10 | Quadro 1

banco <- arquivo
data.frame(
  codigo = c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4"),
  politica = c("Aumentar o número de agrotóxicos permitidos no Brasil.",
               "Aumentar o número de terras destinadas aos indígenas e quilombolas.",
               "Permitir garimpo em terras indígenas.",
               "Diminuir as regras de licença ambiental para as obras de governos e empresas.",
               "Os serviços de saneamento e abastecimento d'água sejam oferecidos apenas por empresas privadas."),
  casos_validos = sapply(c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4"),
                         function(v) sum(banco[[v]] <= 5)),
  row.names = NULL)


# TEXTO PRINCIPAL | p. 12 | Tabela 1

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
af <- fa(banco[, itens], nfactors = 1, cor = "poly")
round(af$loadings[itens, 1], 3)
round(c(ss_loadings = sum(af$loadings[, 1]^2),
        proporcao_da_variancia = mean(af$loadings[, 1]^2)), 3)
rho <- polychoric(banco[, itens])$rho
round(KMO(rho)$MSA, 3)
bartlett <- cortest.bartlett(rho, n = sum(complete.cases(banco[, itens])))
c(qui_quadrado = round(bartlett$chisq, 2), gl = bartlett$df, p = bartlett$p.value)


# TEXTO PRINCIPAL | p. 12 | texto corrido

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
af <- fa(banco[, itens], nfactors = 1, cor = "poly")
round(range(af$loadings[, 1]), 2)
round(KMO(polychoric(banco[, itens])$rho)$MSA, 3)
af2 <- fa(banco[, itens], nfactors = 2, rotate = "oblimin", cor = "poly")
round(af2$Phi[1, 2], 3)
round(alpha(banco[, itens], warnings = FALSE)$total$raw_alpha, 3)


# TEXTO PRINCIPAL | p. 13 | Gráfico 1

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$att_1  <- ifelse(banco$att_1  > 2, NA, banco$att_1)
banco$att_1  <- dplyr::recode(banco$att_1, `2` = 1, `1` = 0)
mod1 <- lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
data.frame(
  termo = c("Intercepto", "Voto em Lula", "Idade", "Escolaridade", "Masculino", "Renda familiar"),
  coef = round(coeftest(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 1], 3),
  ic_inferior = round(coefci(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 1], 3),
  ic_superior = round(coefci(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 2], 3),
  p = round(coeftest(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 4], 4),
  row.names = NULL)
nobs(mod1)


# TEXTO PRINCIPAL | p. 13 | Gráfico 2

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7,  NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2,  NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6,  NA, banco$soc_10)
banco$att_3  <- ifelse(banco$att_3  > 11, NA, banco$att_3)
banco$att_4  <- ifelse(banco$att_4  > 11, NA, banco$att_4)
mod2 <- lm(escore ~ att_3 + att_4 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
data.frame(
  termo = c("Intercepto", "Identificação com Lula", "Identificação com Bolsonaro",
            "Idade", "Escolaridade", "Masculino", "Renda familiar"),
  coef = round(coeftest(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 1], 3),
  ic_inferior = round(coefci(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 1], 3),
  ic_superior = round(coefci(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 2], 3),
  p = round(coeftest(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 4], 4),
  row.names = NULL)
nobs(mod2)


# TEXTO PRINCIPAL | p. 14 | texto corrido

banco <- arquivo
bolsonaristas <- subset(banco, att_1 == 1)
lulistas <- subset(banco, att_1 == 2)
polarizado_bol <- as.integer(bolsonaristas$att_3 %in% 1:3 & bolsonaristas$att_4 %in% 9:11 &
                             bolsonaristas$att_9 %in% 1:2 & bolsonaristas$att_10 %in% 4:5)
polarizado_lul <- as.integer(lulistas$att_3 %in% 9:11 & lulistas$att_4 %in% 1:3 &
                             lulistas$att_9 %in% 4:5 & lulistas$att_10 %in% 1:2)
data.frame(
  eleitorado = c("Eleitores de Bolsonaro", "Eleitores de Lula"),
  n = c(nrow(bolsonaristas), nrow(lulistas)),
  polarizados = c(sum(polarizado_bol), sum(polarizado_lul)),
  percentual = round(100 * c(mean(polarizado_bol), mean(polarizado_lul)), 1))


# TEXTO PRINCIPAL | p. 15 | texto corrido

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
efeitos <- do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados,
                 weights = pareados$weights)
    teste <- coeftest(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               coef = round(teste["afetiva", 1], 3),
               p = round(teste["afetiva", 4], 3))
  }))
}))
efeitos


# TEXTO PRINCIPAL | p. 15 | Gráfico 3

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
preditos <- do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados,
                 weights = pareados$weights)
    p <- predict(ajuste, newdata = data.frame(afetiva = c(0, 1)), interval = "confidence")
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               condicao = c("Não polarizado", "Polarizado"),
               predito = round(p[, "fit"], 3),
               ic_inferior = round(p[, "lwr"], 3),
               ic_superior = round(p[, "upr"], 3))
  }))
}))
preditos


# TEXTO PRINCIPAL | p. 17 | texto corrido

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pares <- matchit(formula_ps, data = d, method = "nearest", ratio = 1,
                   caliper = 0.2)$match.matrix
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    tratado  <- d[[dv]][as.integer(rownames(pares))]
    controle <- d[[dv]][as.integer(pares[, 1])]
    completo <- complete.cases(tratado, controle)
    tratado  <- tratado[completo]
    controle <- controle[completo]
    sinal <- if (mean(tratado) - mean(controle) < 0) -1 else 1
    limites_g <- as.data.frame(psens(sinal * tratado, sinal * controle,
                                     Gamma = 3, GammaInc = 0.05)$bounds)
    acima <- which(limites_g[, 3] > 0.05)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               pares = length(tratado),
               diferenca = round(mean(tratado) - mean(controle), 3),
               gama_critico = ifelse(length(acima), limites_g[acima[1], 1], NA))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 4 | Tabela S1

banco <- arquivo
data.frame(
  item = c("Período de campo", "Abrangência", "Universo", "Tamanho da amostra",
           "Sobreamostragem", "Margem de erro total", "Margem de erro Norte",
           "Intervalo de confiança"),
  especificacao = c(
    paste(format(min(as.Date(banco$VStart)), "%d/%m/%Y"), "a",
          format(max(as.Date(banco$VEnd)), "%d/%m/%Y")),
    paste(length(unique(banco$soc_1)), "unidades da federação"),
    paste("População com", min(banco$soc_3), "anos ou mais"),
    paste(nrow(banco), "entrevistas"),
    paste("Região Norte:", sum(banco$soc_reg == 1), "entrevistas"),
    paste0(round(100 * 1.96 * sqrt(0.25 / nrow(banco)), 2), "%"),
    paste0(round(100 * 1.96 * sqrt(0.25 / sum(banco$soc_reg == 1)), 2), "%"),
    "95%"))


# MATERIAL SUPLEMENTAR | p. 5 | Tabela S2

banco <- arquivo
data.frame(
  regiao = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
  n = as.integer(table(banco$soc_reg)),
  percentual = round(100 * as.numeric(table(banco$soc_reg)) / nrow(banco), 1),
  ufs_distintas = as.integer(tapply(banco$soc_1, banco$soc_reg, function(x) length(unique(x)))))
c(total = nrow(banco), ufs = length(unique(banco$soc_1)))


# MATERIAL SUPLEMENTAR | p. 6 | Tabela S3

banco <- arquivo
rbind(
  data.frame(variavel = "Gênero (soc_4)",
             categoria = c("Feminino", "Masculino", "Outro"),
             n = as.integer(table(banco$soc_4))),
  data.frame(variavel = "Faixa etária (soc_3a)",
             categoria = c("16 a 24 anos", "25 a 34 anos", "35 a 44 anos",
                           "45 a 59 anos", "60 anos ou mais"),
             n = as.integer(table(banco$soc_3a))),
  data.frame(variavel = "Porte do município (soc_2a)",
             categoria = c("Interior", "Capital", "Região metropolitana"),
             n = as.integer(table(banco$soc_2a))),
  data.frame(variavel = "Escolaridade (soc_9)",
             categoria = c("Nunca foi à escola", "Fundamental incompleto",
                           "Fundamental completo", "Médio incompleto", "Médio completo",
                           "Superior incompleto", "Superior completo", "Não sabe / Não respondeu"),
             n = c(as.integer(table(banco$soc_9))[1:7], sum(banco$soc_9 > 7))),
  data.frame(variavel = "Renda familiar (soc_10)",
             categoria = c("Até 1/2 salário mínimo", "De 1/2 a 1", "De 1 a 2", "De 2 a 5",
                           "De 5 a 10", "Mais de 10", "Não sabe / Não respondeu"),
             n = c(as.integer(table(banco$soc_10))[1:6], sum(banco$soc_10 > 6))),
  data.frame(variavel = "Cor ou raça (soc_8)",
             categoria = c("Branca", "Preta", "Amarela", "Parda", "Indígena",
                           "Não sabe / Não respondeu"),
             n = c(as.integer(table(banco$soc_8))[1:5], sum(banco$soc_8 > 5)))) %>%
  mutate(percentual = round(100 * n / nrow(banco), 1))
round(c(idade_media = mean(banco$soc_3), idade_minima = min(banco$soc_3),
        idade_maxima = max(banco$soc_3)), 1)


# MATERIAL SUPLEMENTAR | p. 7 | texto corrido

banco <- arquivo
round(c(minimo = min(banco$Peso), maximo = max(banco$Peso),
        media = mean(banco$Peso), soma = sum(banco$Peso)), 2)
round(c(soma_peso_norte = sum(banco$Peso_Norte),
        nulos_fora_do_norte = sum(banco$Peso_Norte == 0 & banco$soc_reg != 1)), 2)


# MATERIAL SUPLEMENTAR | p. 7 | texto corrido

banco <- arquivo
administrativas <- c("SbjNum", "Duration", "VStart", "VEnd", "Peso", "Peso_Norte",
                     "Q_1", "Q_71", "Q_73", "Q_74", "soc_2")
perguntas <- names(banco)[!names(banco) %in% administrativas &
                          !grepl("_outros$", names(banco))]
perguntas <- unique(ifelse(grepl("^A_", perguntas),
                           sub("^A_(.*)_[0-9]+$", "\\1", perguntas), perguntas))
c(perguntas_fechadas = length(perguntas),
  aplicadas_so_no_norte = sum(grepl("amt_norte", perguntas)))
table(sub("_.*$", "", perguntas))


# MATERIAL SUPLEMENTAR | p. 9 | Tabela S4

banco <- arquivo
variaveis <- c(poli_2 = "Agrotóxicos", ind_3 = "Terras indígenas e quilombolas",
               ind_4 = "Garimpo em terras indígenas", poli_3 = "Licenciamento ambiental",
               poli_4 = "Privatização do saneamento",
               valc_4 = "Direito de expressão — ambientalistas",
               valc_5 = "Direito de expressão — agronegócio",
               att_1 = "Voto no 2º turno de 2022", att_2 = "Autoposicionamento ideológico",
               att_3 = "Identificação com Lula", att_4 = "Identificação com Bolsonaro",
               att_9 = "Casamento com eleitor de Bolsonaro",
               att_10 = "Casamento com eleitor de Lula",
               valc_1 = "Interesse por questões ambientais", soc_8 = "Cor ou raça",
               soc_9 = "Escolaridade", soc_10 = "Renda familiar")
data.frame(
  codigo = names(variaveis),
  variavel = unname(variaveis),
  nao_sabe = sapply(names(variaveis), function(v) sum(banco[[v]] == 98)),
  nao_respondeu = sapply(names(variaveis), function(v) sum(banco[[v]] == 99)),
  total = sapply(names(variaveis), function(v) sum(banco[[v]] %in% c(98, 99))),
  percentual = round(100 * sapply(names(variaveis),
                                  function(v) mean(banco[[v]] %in% c(98, 99))), 1),
  row.names = NULL)
sapply(c("soc_3", "soc_4", "soc_1", "soc_2a"), function(v) sum(banco[[v]] %in% c(98, 99)))
c(branco_ou_nulo = sum(banco$att_1 == 3), nao_foi_votar = sum(banco$att_1 == 4))


# MATERIAL SUPLEMENTAR | p. 10 | Tabela S5

banco <- arquivo
data.frame(
  codigo = c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4"),
  casos_validos = sapply(c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4"),
                         function(v) sum(!banco[[v]] %in% c(98, 99))),
  row.names = NULL)


# MATERIAL SUPLEMENTAR | p. 10 | Tabela S6

banco <- arquivo
banco$ind_3 <- ifelse(banco$ind_3 > 5, NA, banco$ind_3)
data.frame(
  categoria = c("Concorda totalmente", "Concorda parcialmente",
                "Não concorda nem discorda", "Discorda parcialmente", "Discorda totalmente"),
  codigo_original = 1:5,
  codigo_recodificado = c(5, 4, 3, 2, 1),
  n = as.integer(table(banco$ind_3)))
table(original = banco$ind_3,
      recodificado = dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1))


# MATERIAL SUPLEMENTAR | p. 11 | Gráfico S1

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
c(casos_com_escore = sum(!is.na(banco$escore)))
round(summary(banco$escore[!is.na(banco$escore)]), 3)
round(sd(banco$escore, na.rm = TRUE), 3)
table(cut(banco$escore, breaks = seq(-3.5, 1.5, by = 0.5)))


# MATERIAL SUPLEMENTAR | p. 12 | texto corrido

banco <- arquivo
table(banco$att_1)
c(branco_ou_nulo = sum(banco$att_1 == 3),
  nao_foi_votar = sum(banco$att_1 == 4),
  restantes = sum(banco$att_1 %in% 1:2),
  bolsonaro = sum(banco$att_1 == 1),
  lula = sum(banco$att_1 == 2))


# MATERIAL SUPLEMENTAR | p. 12 | Tabela S7

banco <- arquivo
data.frame(
  ponto_da_escala = 0:10,
  codigo_no_arquivo = 1:11,
  n_att_3 = as.integer(table(factor(banco$att_3[banco$att_3 <= 11], levels = 1:11))),
  n_att_4 = as.integer(table(factor(banco$att_4[banco$att_4 <= 11], levels = 1:11))))


# MATERIAL SUPLEMENTAR | p. 13 | Tabela S8

banco <- arquivo
bolsonaristas <- subset(banco, att_1 == 1)
lulistas <- subset(banco, att_1 == 2)
data.frame(
  condicao = c("Identificação com a liderança do próprio campo",
               "Identificação com a liderança adversária",
               "Casamento da filha com membro do próprio grupo",
               "Casamento da filha com membro do grupo rival",
               "As quatro condições simultaneamente"),
  eleitores_de_bolsonaro = c(sum(bolsonaristas$att_4 %in% 9:11),
                             sum(bolsonaristas$att_3 %in% 1:3),
                             sum(bolsonaristas$att_9 %in% 1:2),
                             sum(bolsonaristas$att_10 %in% 4:5),
                             sum(bolsonaristas$att_3 %in% 1:3 & bolsonaristas$att_4 %in% 9:11 &
                                 bolsonaristas$att_9 %in% 1:2 & bolsonaristas$att_10 %in% 4:5)),
  eleitores_de_lula = c(sum(lulistas$att_3 %in% 9:11),
                        sum(lulistas$att_4 %in% 1:3),
                        sum(lulistas$att_10 %in% 1:2),
                        sum(lulistas$att_9 %in% 4:5),
                        sum(lulistas$att_3 %in% 9:11 & lulistas$att_4 %in% 1:3 &
                            lulistas$att_9 %in% 4:5 & lulistas$att_10 %in% 1:2)))


# MATERIAL SUPLEMENTAR | p. 13 | texto corrido

banco <- arquivo
bolsonaristas <- subset(banco, att_1 == 1)
lulistas <- subset(banco, att_1 == 2)
data.frame(
  eleitorado = c("Eleitores de Bolsonaro", "Eleitores de Lula"),
  n = c(nrow(bolsonaristas), nrow(lulistas)),
  polarizados = c(sum(bolsonaristas$att_3 %in% 1:3 & bolsonaristas$att_4 %in% 9:11 &
                      bolsonaristas$att_9 %in% 1:2 & bolsonaristas$att_10 %in% 4:5),
                  sum(lulistas$att_3 %in% 9:11 & lulistas$att_4 %in% 1:3 &
                      lulistas$att_9 %in% 4:5 & lulistas$att_10 %in% 1:2)),
  percentual = round(100 * c(mean(bolsonaristas$att_3 %in% 1:3 & bolsonaristas$att_4 %in% 9:11 &
                                  bolsonaristas$att_9 %in% 1:2 & bolsonaristas$att_10 %in% 4:5),
                             mean(lulistas$att_3 %in% 9:11 & lulistas$att_4 %in% 1:3 &
                                  lulistas$att_9 %in% 4:5 & lulistas$att_10 %in% 1:2)), 1),
  sem_informacao_em_algum_item = c(
    sum(apply(bolsonaristas[, c("att_3", "att_4", "att_9", "att_10")] > 90, 1, any)),
    sum(apply(lulistas[, c("att_3", "att_4", "att_9", "att_10")] > 90, 1, any))))


# MATERIAL SUPLEMENTAR | p. 14 | Tabela S9

banco <- arquivo
data.frame(
  codigo = c("valc_4", "valc_5"),
  variavel = c("Direito de ambientalistas expressarem suas opiniões",
               "Direito de produtores do agronegócio expressarem suas opiniões"),
  casos_validos = c(sum(!banco$valc_4 %in% c(98, 99)), sum(!banco$valc_5 %in% c(98, 99))))
table(valc_4 = banco$valc_4[banco$valc_4 <= 5])
table(valc_5 = banco$valc_5[banco$valc_5 <= 5])


# MATERIAL SUPLEMENTAR | p. 15 | Tabela S10

banco <- arquivo
teto <- c(soc_1 = 27, soc_2a = 3, soc_3 = 120, soc_4 = 3, soc_8 = 5,
          soc_9 = 7, soc_10 = 6, valc_1 = 4, att_2 = 7)
data.frame(
  codigo = names(teto),
  uso = c("Pareamento", "Pareamento", "Controle e pareamento", "Controle e pareamento",
          "Pareamento", "Controle e pareamento", "Controle e pareamento",
          "Pareamento", "Pareamento"),
  categorias_validas = sapply(names(teto),
                              function(v) length(unique(banco[[v]][banco[[v]] <= teto[[v]]]))),
  minimo = sapply(names(teto), function(v) min(banco[[v]][banco[[v]] <= teto[[v]]])),
  maximo = sapply(names(teto), function(v) max(banco[[v]][banco[[v]] <= teto[[v]]])),
  row.names = NULL)
c(genero_outro = sum(banco$soc_4 == 3))


# MATERIAL SUPLEMENTAR | p. 15-16 | texto corrido

banco <- arquivo
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
c(bolsonaro = sum(apply(subset(banco, att_1 == 1)[, names(limites)], 1,
                        function(x) any(x > c(4, 7, 5, 7, 6)))),
  lula = sum(apply(subset(banco, att_1 == 2)[, names(limites)], 1,
                   function(x) any(x > c(4, 7, 5, 7, 6)))))
bolsonaristas <- subset(banco, att_1 == 1)
bolsonaristas$afetiva <- as.integer(bolsonaristas$att_3 %in% 1:3 & bolsonaristas$att_4 %in% 9:11 &
                                    bolsonaristas$att_9 %in% 1:2 & bolsonaristas$att_10 %in% 4:5)
bolsonaristas$att_2_declarado <- ifelse(bolsonaristas$att_2 > 7, NA, bolsonaristas$att_2)
round(c(so_valores_declarados = coef(glm(afetiva ~ soc_1 + soc_2a + soc_3 + soc_4 + valc_1 +
                                         att_2_declarado + soc_8 + soc_9 + soc_10,
                                         data = bolsonaristas, family = binomial))["att_2_declarado"],
        codigos_como_validos = coef(glm(afetiva ~ soc_1 + soc_2a + soc_3 + soc_4 + valc_1 +
                                        att_2 + soc_8 + soc_9 + soc_10,
                                        data = bolsonaristas, family = binomial))["att_2"]), 4)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  balanco <- summary(matchit(formula_ps, data = d, method = "nearest",
                             ratio = 2, caliper = 0.2))
  data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
             dpm_ideologia_antes = round(balanco$sum.all["att_2_v", "Std. Mean Diff."], 3),
             dpm_ideologia_depois = round(balanco$sum.matched["att_2_v", "Std. Mean Diff."], 3))
}))


# MATERIAL SUPLEMENTAR | p. 16 | Tabela S11

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7,  NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2,  NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6,  NA, banco$soc_10)
banco$att_3  <- ifelse(banco$att_3  > 11, NA, banco$att_3)
banco$att_4  <- ifelse(banco$att_4  > 11, NA, banco$att_4)
banco$voto   <- ifelse(banco$att_1  > 2,  NA, banco$att_1)
data.frame(
  etapa = c("Arquivo bruto", "Escore de valores ambientais", "Eleitores de Lula ou de Bolsonaro",
            "Modelo 1 — valores ambientais e voto", "Modelo 2 — valores ambientais e identificação",
            "Subamostra de eleitores de Bolsonaro", "dos quais, afetivamente polarizados",
            "Subamostra de eleitores de Lula", "dos quais, afetivamente polarizados"),
  n = c(nrow(banco),
        sum(!is.na(banco$escore)),
        sum(!is.na(banco$voto)),
        sum(complete.cases(banco[, c("escore", "voto", "soc_3", "soc_4", "soc_9", "soc_10")])),
        sum(complete.cases(banco[, c("escore", "att_3", "att_4", "soc_3", "soc_4", "soc_9", "soc_10")])),
        sum(banco$voto == 1, na.rm = TRUE),
        sum(banco$voto == 1 & banco$att_3 %in% 1:3 & banco$att_4 %in% 9:11 &
            banco$att_9 %in% 1:2 & banco$att_10 %in% 4:5, na.rm = TRUE),
        sum(banco$voto == 2, na.rm = TRUE),
        sum(banco$voto == 2 & banco$att_3 %in% 9:11 & banco$att_4 %in% 1:3 &
            banco$att_9 %in% 4:5 & banco$att_10 %in% 1:2, na.rm = TRUE)))


# MATERIAL SUPLEMENTAR | p. 19 | Tabela S12

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
af <- fa(banco[, itens], nfactors = 1, cor = "poly")
data.frame(
  item = c("Aumentar agrotóxicos (poli_2)",
           "Aumentar terras indígenas e quilombolas (ind_3)",
           "Permitir garimpo em terras indígenas (ind_4)",
           "Diminuir regras de licenciamento (poli_3)",
           "Saneamento apenas por empresas privadas (poli_4)"),
  carga = round(af$loadings[itens, 1], 3),
  comunalidade = round(af$communality[itens], 3),
  unicidade = round(af$uniquenesses[itens], 3),
  row.names = NULL)
round(c(soma_dos_quadrados = sum(af$loadings[, 1]^2),
        proporcao_da_variancia = mean(af$loadings[, 1]^2)), 3)
sum(complete.cases(banco[, itens]))


# MATERIAL SUPLEMENTAR | p. 20 | Tabela S13

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
round(polychoric(banco[, itens])$rho, 3)
round(cor(banco[, itens], use = "pairwise.complete.obs"), 3)


# MATERIAL SUPLEMENTAR | p. 20 | Tabela S14

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
rho <- polychoric(banco[, itens])$rho
bartlett <- cortest.bartlett(rho, n = sum(complete.cases(banco[, itens])))
c(qui_quadrado = round(bartlett$chisq, 2), gl = bartlett$df, p = bartlett$p.value)
round(KMO(rho)$MSA, 3)
round(KMO(rho)$MSAi, 3)
round(KMO(cor(banco[, itens], use = "pairwise.complete.obs"))$MSA, 3)


# MATERIAL SUPLEMENTAR | p. 21 | Tabela S15

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
af_poli <- fa(banco[, itens], nfactors = 1, cor = "poly")
af_pearson <- fa(banco[, itens], nfactors = 1)
acp <- principal(banco[, itens], nfactors = 1)
data.frame(
  item = itens,
  af_policorica = round(af_poli$loadings[itens, 1], 3),
  af_pearson = round(af_pearson$loadings[itens, 1], 3),
  componentes_principais = round(acp$loadings[itens, 1], 3),
  row.names = NULL)
round(rbind(
  soma_dos_quadrados = c(af_policorica = sum(af_poli$loadings[, 1]^2),
                         af_pearson = sum(af_pearson$loadings[, 1]^2),
                         componentes_principais = sum(acp$loadings[, 1]^2)),
  proporcao_da_variancia = c(mean(af_poli$loadings[, 1]^2),
                             mean(af_pearson$loadings[, 1]^2),
                             mean(acp$loadings[, 1]^2))), 3)


# MATERIAL SUPLEMENTAR | p. 23 | Gráfico S2

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$att_1 <- ifelse(banco$att_1 > 2, NA, banco$att_1)
banco %>%
  dplyr::select(all_of(itens), att_1) %>%
  filter(!is.na(att_1)) %>%
  pivot_longer(all_of(itens), names_to = "item", values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  mutate(voto = factor(att_1, levels = 1:2, labels = c("Bolsonaro", "Lula")),
         categoria = c("Concorda totalmente", "Concorda parcialmente",
                       "Não concorda nem discorda", "Discorda parcialmente",
                       "Discorda totalmente")[resposta],
         posicao_no_eixo = ifelse(item == "ind_3", 6 - resposta, resposta)) %>%
  count(item, voto, categoria, posicao_no_eixo) %>%
  group_by(item, voto) %>%
  mutate(percentual = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(match(item, itens), voto, posicao_no_eixo) %>%
  as.data.frame()


# MATERIAL SUPLEMENTAR | p. 24 | Tabela S16

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$att_1  <- ifelse(banco$att_1  > 2, NA, banco$att_1)
banco$att_1  <- dplyr::recode(banco$att_1, `2` = 1, `1` = 0)
mod1 <- lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
data.frame(
  termo = c("Intercepto", "Voto em Lula", "Idade", "Escolaridade", "Masculino", "Renda familiar"),
  coef = round(coeftest(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 1], 3),
  erro_padrao = round(coeftest(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 2], 3),
  t = round(coeftest(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 3], 2),
  p = round(coeftest(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 4], 4),
  ic_inferior = round(coefci(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 1], 3),
  ic_superior = round(coefci(mod1, vcov. = vcovHC(mod1, type = "HC3"))[, 2], 3),
  row.names = NULL)
c(n = nobs(mod1), r2 = round(summary(mod1)$r.squared, 3),
  r2_ajustado = round(summary(mod1)$adj.r.squared, 3))


# MATERIAL SUPLEMENTAR | p. 24 | Tabela S17

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7,  NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2,  NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6,  NA, banco$soc_10)
banco$att_3  <- ifelse(banco$att_3  > 11, NA, banco$att_3)
banco$att_4  <- ifelse(banco$att_4  > 11, NA, banco$att_4)
mod2 <- lm(escore ~ att_3 + att_4 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
data.frame(
  termo = c("Intercepto", "Identificação com Lula", "Identificação com Bolsonaro",
            "Idade", "Escolaridade", "Masculino", "Renda familiar"),
  coef = round(coeftest(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 1], 3),
  erro_padrao = round(coeftest(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 2], 3),
  t = round(coeftest(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 3], 2),
  p = round(coeftest(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 4], 4),
  ic_inferior = round(coefci(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 1], 3),
  ic_superior = round(coefci(mod2, vcov. = vcovHC(mod2, type = "HC3"))[, 2], 3),
  row.names = NULL)
c(n = nobs(mod2), r2 = round(summary(mod2)$r.squared, 3),
  r2_ajustado = round(summary(mod2)$adj.r.squared, 3))


# MATERIAL SUPLEMENTAR | p. 27 | Tabela S18

banco <- arquivo
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  contagem <- summary(matchit(formula_ps, data = d, method = "nearest",
                              ratio = 2, caliper = 0.2))$nn
  data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
             etapa = c("Antes do pareamento", "Pareados", "Não pareados"),
             nao_polarizados = contagem[c("All (ESS)", "Matched", "Unmatched"), "Control"],
             polarizados = contagem[c("All (ESS)", "Matched", "Unmatched"), "Treated"])
}))


# MATERIAL SUPLEMENTAR | p. 28 | Tabela S19

banco <- arquivo
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
bolsonaristas <- subset(banco, att_1 == 1)
bolsonaristas$afetiva <- as.integer(bolsonaristas$att_3 %in% 1:3 & bolsonaristas$att_4 %in% 9:11 &
                                    bolsonaristas$att_9 %in% 1:2 & bolsonaristas$att_10 %in% 4:5)
for (v in names(limites)) {
  x <- ifelse(bolsonaristas[[v]] > limites[[v]], NA, bolsonaristas[[v]])
  bolsonaristas[[paste0(v, "_nr")]] <- as.integer(is.na(x))
  x[is.na(x)] <- median(x, na.rm = TRUE)
  bolsonaristas[[paste0(v, "_v")]] <- x
}
balanco <- summary(matchit(formula_ps, data = bolsonaristas, method = "nearest",
                           ratio = 2, caliper = 0.2))
data.frame(
  covariavel = rownames(balanco$sum.all),
  trat_antes = round(balanco$sum.all[, "Means Treated"], 3),
  contr_antes = round(balanco$sum.all[, "Means Control"], 3),
  dpm_antes = round(balanco$sum.all[, "Std. Mean Diff."], 3),
  trat_depois = round(balanco$sum.matched[, "Means Treated"], 3),
  contr_depois = round(balanco$sum.matched[, "Means Control"], 3),
  dpm_depois = round(balanco$sum.matched[, "Std. Mean Diff."], 3),
  row.names = NULL)


# MATERIAL SUPLEMENTAR | p. 28 | Tabela S20

banco <- arquivo
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
lulistas <- subset(banco, att_1 == 2)
lulistas$afetiva <- as.integer(lulistas$att_3 %in% 9:11 & lulistas$att_4 %in% 1:3 &
                               lulistas$att_9 %in% 4:5 & lulistas$att_10 %in% 1:2)
for (v in names(limites)) {
  x <- ifelse(lulistas[[v]] > limites[[v]], NA, lulistas[[v]])
  lulistas[[paste0(v, "_nr")]] <- as.integer(is.na(x))
  x[is.na(x)] <- median(x, na.rm = TRUE)
  lulistas[[paste0(v, "_v")]] <- x
}
balanco <- summary(matchit(formula_ps, data = lulistas, method = "nearest",
                           ratio = 2, caliper = 0.2))
data.frame(
  covariavel = rownames(balanco$sum.all),
  trat_antes = round(balanco$sum.all[, "Means Treated"], 3),
  contr_antes = round(balanco$sum.all[, "Means Control"], 3),
  dpm_antes = round(balanco$sum.all[, "Std. Mean Diff."], 3),
  trat_depois = round(balanco$sum.matched[, "Means Treated"], 3),
  contr_depois = round(balanco$sum.matched[, "Means Control"], 3),
  dpm_depois = round(balanco$sum.matched[, "Std. Mean Diff."], 3),
  row.names = NULL)


# MATERIAL SUPLEMENTAR | p. 29 | Tabela S21

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    teste <- coeftest(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
    intervalo <- coefci(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               coef = round(teste["afetiva", 1], 3),
               erro_padrao = round(teste["afetiva", 2], 3),
               p = round(teste["afetiva", 4], 3),
               ic_inferior = round(intervalo["afetiva", 1], 3),
               ic_superior = round(intervalo["afetiva", 2], 3),
               n = nobs(ajuste),
               r2 = round(summary(ajuste)$r.squared, 3))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 30 | Tabela S22

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    p <- predict(ajuste, newdata = data.frame(afetiva = c(0, 1)), interval = "confidence")
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               condicao = c("Não polarizado", "Polarizado"),
               valor_predito = round(p[, "fit"], 3),
               ic_inferior = round(p[, "lwr"], 3),
               ic_superior = round(p[, "upr"], 3))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 32 | Gráfico S3

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
set.seed(1789)
paralela <- fa.parallel(polychoric(banco[, itens])$rho, n.obs = sum(complete.cases(banco[, itens])),
                        fa = "fa", n.iter = 100, plot = FALSE)
data.frame(fator = 1:5,
           autovalor_observado = round(paralela$fa.values[1:5], 3),
           autovalor_simulado = round(paralela$fa.sim[1:5], 3))
paralela$nfact


# MATERIAL SUPLEMENTAR | p. 32 | Tabela S23

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
af2 <- fa(banco[, itens], nfactors = 2, rotate = "oblimin", cor = "poly")
round(unclass(af2$loadings)[itens, ], 3)
round(c(correlacao_entre_fatores = af2$Phi[1, 2],
        variancia_do_segundo_fator = af2$Vaccounted[2, 2]), 3)


# MATERIAL SUPLEMENTAR | p. 33 | Tabela S24

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
af_poli <- fa(banco[, itens], nfactors = 1, cor = "poly")
af_pearson <- fa(banco[, itens], nfactors = 1)
data.frame(
  indice = c("RMSEA", "IC 90% inferior do RMSEA", "IC 90% superior do RMSEA",
             "TLI", "RMSR", "BIC"),
  policorica = round(c(af_poli$RMSEA[1], af_poli$RMSEA[2], af_poli$RMSEA[3],
                       af_poli$TLI, af_poli$rms, af_poli$BIC), 3),
  pearson = round(c(af_pearson$RMSEA[1], af_pearson$RMSEA[2], af_pearson$RMSEA[3],
                    af_pearson$TLI, af_pearson$rms, af_pearson$BIC), 3))


# MATERIAL SUPLEMENTAR | p. 34 | Tabela S25

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
consistencia <- alpha(banco[, itens], warnings = FALSE)
round(c(alfa_de_cronbach = consistencia$total$raw_alpha,
        alfa_ordinal = alpha(polychoric(banco[, itens])$rho, warnings = FALSE)$total$raw_alpha,
        omega_total = omega(banco[, itens], nfactors = 1, plot = FALSE, warnings = FALSE)$omega.tot,
        correlacao_media_entre_itens = consistencia$total$average_r), 3)
round(consistencia$alpha.drop[, "raw_alpha", drop = FALSE], 3)


# MATERIAL SUPLEMENTAR | p. 35 | Tabela S26

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
externos <- list(poli_6 = c(1, 5), val_6 = c(1, 5), valc_3 = c(1, 5),
                 val_5 = c(1, 2), ind_6 = c(1, 5))
do.call(rbind, lapply(names(externos), function(v) {
  y <- ifelse(banco[[v]] < externos[[v]][1] | banco[[v]] > externos[[v]][2], NA, banco[[v]])
  valido <- complete.cases(banco$escore, y)
  teste <- cor.test(banco$escore[valido], y[valido])
  data.frame(item_externo = v,
             direcao_prevista = c(poli_6 = "positiva", val_6 = "positiva", valc_3 = "negativa",
                                  val_5 = "negativa", ind_6 = "positiva")[v],
             r = round(unname(teste$estimate), 3),
             p = round(teste$p.value, 3),
             n = sum(valido),
             row.names = NULL)
}))


# MATERIAL SUPLEMENTAR | p. 36 | Gráfico S4

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$att_2  <- ifelse(banco$att_2  > 7, NA, banco$att_2)
banco$valc_1 <- ifelse(banco$valc_1 > 4, NA, banco$valc_1)
banco$att_1  <- ifelse(banco$att_1  > 2, NA, banco$att_1)
banco$att_1  <- dplyr::recode(banco$att_1, `2` = 1, `1` = 0)
banco$evangelico <- ifelse(ifelse(banco$soc_5 > 9, NA, banco$soc_5) == 2, 1, 0)
banco$regiao <- factor(banco$soc_reg)
especificacoes <- list(
  "Referência (HC3)" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco),
  "Erros-padrão clássicos" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco),
  "Agrupados por estado" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco),
  "Ponderado pelo peso nacional" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10,
                                      data = banco, weights = Peso),
  "Excluindo a região Norte" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10,
                                  data = subset(banco, soc_reg != 1)),
  "Efeitos fixos de região" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10 + regiao, data = banco),
  "+ ideologia" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10 + att_2, data = banco),
  "+ religião evangélica" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10 + evangelico, data = banco),
  "+ interesse ambiental" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10 + valc_1, data = banco),
  "Todos os controles" = lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10 + att_2 +
                              evangelico + valc_1 + regiao, data = banco))
do.call(rbind, lapply(names(especificacoes), function(nome) {
  ajuste <- especificacoes[[nome]]
  matriz <- if (nome == "Erros-padrão clássicos") NULL else
    if (nome == "Agrupados por estado") vcovCL(ajuste, cluster = ~ soc_1) else
      vcovHC(ajuste, type = "HC3")
  teste <- if (is.null(matriz)) coeftest(ajuste) else coeftest(ajuste, vcov. = matriz)
  intervalo <- if (is.null(matriz)) confint(ajuste) else coefci(ajuste, vcov. = matriz)
  data.frame(especificacao = nome,
             coef = round(teste["att_1", 1], 3),
             erro_padrao = round(teste["att_1", 2], 3),
             ic_inferior = round(intervalo["att_1", 1], 3),
             ic_superior = round(intervalo["att_1", 2], 3),
             p = signif(teste["att_1", 4], 3),
             n = nobs(ajuste))
}))


# MATERIAL SUPLEMENTAR | p. 37 | Tabela S27

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$att_1  <- ifelse(banco$att_1  > 2, NA, banco$att_1)
banco$att_1  <- dplyr::recode(banco$att_1, `2` = 1, `1` = 0)
do.call(rbind, lapply(c("(nenhum)", itens), function(retirado) {
  usados <- if (retirado == "(nenhum)") itens else setdiff(itens, retirado)
  d <- banco
  d$escore <- factor.scores(banco[, usados], fa(banco[, usados], nfactors = 1, cor = "poly"))$scores[, 1]
  ajuste <- lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = d)
  data.frame(construcao = ifelse(retirado == "(nenhum)", "Cinco itens (referência)",
                                 paste("Sem", retirado)),
             coef = round(coef(ajuste)["att_1"], 3),
             erro_padrao = round(summary(ajuste)$coefficients["att_1", 2], 3),
             n = nobs(ajuste),
             row.names = NULL)
}))
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$aditivo <- rowMeans(banco[, itens])
aditivo <- lm(scale(aditivo) ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
data.frame(construcao = "Índice aditivo simples, padronizado",
           coef = round(coef(aditivo)["att_1"], 3),
           erro_padrao = round(summary(aditivo)$coefficients["att_1", 2], 3),
           n = nobs(aditivo),
           row.names = NULL)
round(cor(banco$escore, banco$aditivo, use = "complete.obs"), 3)


# MATERIAL SUPLEMENTAR | p. 38 | Gráfico S5

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$att_1  <- ifelse(banco$att_1  > 2, NA, banco$att_1)
banco$att_1  <- dplyr::recode(banco$att_1, `2` = 1, `1` = 0)
do.call(rbind, lapply(itens, function(v) {
  ajuste <- lm(banco[[v]] ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
  teste <- coeftest(ajuste, vcov. = vcovHC(ajuste, type = "HC3"))
  intervalo <- coefci(ajuste, vcov. = vcovHC(ajuste, type = "HC3"))
  data.frame(item = v,
             coef = round(teste["att_1", 1], 3),
             erro_padrao = round(teste["att_1", 2], 3),
             ic_inferior = round(intervalo["att_1", 1], 3),
             ic_superior = round(intervalo["att_1", 2], 3),
             p = signif(teste["att_1", 4], 3),
             n = nobs(ajuste))
}))


# MATERIAL SUPLEMENTAR | p. 39 | Tabela S28

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$voto <- factor(ifelse(banco$att_1 > 4, NA, banco$att_1), levels = 1:4,
                     labels = c("Bolsonaro", "Lula", "Branco/nulo", "Não votou"))
mod_voto <- lm(escore ~ voto + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
round(tapply(banco$escore, banco$voto, mean, na.rm = TRUE), 3)
round(coeftest(mod_voto, vcov. = vcovHC(mod_voto, type = "HC3"))[2:4, ], 4)
nobs(mod_voto)


# MATERIAL SUPLEMENTAR | p. 39 | Tabela S29

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$soc_9  <- ifelse(banco$soc_9  > 7,  NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2,  NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6,  NA, banco$soc_10)
banco$att_3  <- ifelse(banco$att_3  > 11, NA, banco$att_3)
banco$att_4  <- ifelse(banco$att_4  > 11, NA, banco$att_4)
banco$att_1  <- ifelse(banco$att_1  > 2,  NA, banco$att_1)
banco$att_1  <- dplyr::recode(banco$att_1, `2` = 1, `1` = 0)
mod1 <- lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
mod2 <- lm(escore ~ att_3 + att_4 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
round(c(vif_maximo_modelo_1 = max(vif(mod1)), vif_maximo_modelo_2 = max(vif(mod2))), 3)
c(bp_modelo_1 = round(bptest(mod1)$statistic, 2), p = signif(bptest(mod1)$p.value, 3))
c(bp_modelo_2 = round(bptest(mod2)$statistic, 2), p = signif(bptest(mod2)$p.value, 3))
cook <- cooks.distance(mod1)
c(cook_maxima = round(max(cook), 3),
  casos_acima_de_4_sobre_n = sum(cook > 4 / nobs(mod1)),
  percentual = round(100 * mean(cook > 4 / nobs(mod1)), 1))
round(coef(lm(escore ~ att_1 + soc_3 + soc_9 + soc_4 + soc_10,
              data = model.frame(mod1)[cook <= 4 / nobs(mod1), ]))["att_1"], 3)


# MATERIAL SUPLEMENTAR | p. 40 | Tabela S30

banco <- arquivo
itens <- c("poli_2", "ind_3", "ind_4", "poli_3", "poli_4")
for (v in itens) banco[[v]] <- ifelse(banco[[v]] > 5, NA, banco[[v]])
banco$ind_3 <- dplyr::recode(banco$ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)
banco$escore <- factor.scores(banco[, itens], fa(banco[, itens], nfactors = 1, cor = "poly"))$scores[, 1]
banco$escolaridade <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$sexo         <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$renda        <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$voto         <- ifelse(banco$att_1  > 2, NA, banco$att_1)
banco$fora <- as.integer(!complete.cases(banco[, c("escore", "voto", "soc_3",
                                                   "escolaridade", "sexo", "renda")]))
c(incluidos = sum(banco$fora == 0), excluidos = sum(banco$fora == 1),
  percentual_excluido = round(100 * mean(banco$fora), 1))
do.call(rbind, lapply(c("soc_3", "escolaridade", "renda"), function(v) {
  data.frame(caracteristica = v,
             incluidos = round(mean(banco[[v]][banco$fora == 0], na.rm = TRUE), 2),
             excluidos = round(mean(banco[[v]][banco$fora == 1], na.rm = TRUE), 2),
             diferenca_padronizada = round(
               (mean(banco[[v]][banco$fora == 0], na.rm = TRUE) -
                mean(banco[[v]][banco$fora == 1], na.rm = TRUE)) / sd(banco[[v]], na.rm = TRUE), 3))
}))
round(100 * c(mulheres_incluidas = mean(banco$sexo[banco$fora == 0] == 1, na.rm = TRUE),
              mulheres_excluidas = mean(banco$sexo[banco$fora == 1] == 1, na.rm = TRUE)), 1)


# MATERIAL SUPLEMENTAR | p. 41 | Gráfico S6

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
definicoes <- list(
  "Quatro condições" = function(d, lado) if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2),
  "Três das quatro" = function(d, lado) if (lado == "bol")
    as.integer((d$att_3 %in% 1:3) + (d$att_4 %in% 9:11) + (d$att_9 %in% 1:2) + (d$att_10 %in% 4:5) >= 3)
  else as.integer((d$att_3 %in% 9:11) + (d$att_4 %in% 1:3) + (d$att_9 %in% 4:5) + (d$att_10 %in% 1:2) >= 3),
  "Apenas termômetros" = function(d, lado) if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11)
  else as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3),
  "Apenas distância social" = function(d, lado) if (lado == "bol")
    as.integer(d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else as.integer(d$att_9 %in% 4:5 & d$att_10 %in% 1:2))
do.call(rbind, lapply(names(definicoes), function(nome) {
  do.call(rbind, lapply(c("bol", "lul"), function(lado) {
    d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
    d$afetiva <- definicoes[[nome]](d, lado)
    for (v in names(limites)) {
      x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
      d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
      x[is.na(x)] <- median(x, na.rm = TRUE)
      d[[paste0(v, "_v")]] <- x
    }
    pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                   ratio = 2, caliper = 0.2))
    do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
      ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
      teste <- coeftest(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
      intervalo <- coefci(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
      data.frame(definicao = nome,
                 eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
                 alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
                 tratados = sum(d$afetiva),
                 coef = round(teste["afetiva", 1], 3),
                 ic_inferior = round(intervalo["afetiva", 1], 3),
                 ic_superior = round(intervalo["afetiva", 2], 3),
                 p = round(teste["afetiva", 4], 3))
    }))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 42 | Tabela S31

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
banco$soc_9  <- ifelse(banco$soc_9  > 7, NA, banco$soc_9)
banco$soc_4  <- ifelse(banco$soc_4  > 2, NA, banco$soc_4)
banco$soc_10 <- ifelse(banco$soc_10 > 6, NA, banco$soc_10)
banco$att_2  <- ifelse(banco$att_2  > 7, NA, banco$att_2)
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  identificacao_lula <- ifelse(d$att_3 > 11, NA, d$att_3) - 1
  identificacao_bolsonaro <- ifelse(d$att_4 > 11, NA, d$att_4) - 1
  d$distancia <- if (lado == "bol") identificacao_bolsonaro - identificacao_lula
                 else identificacao_lula - identificacao_bolsonaro
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ distancia + soc_3 + soc_9 + soc_4 + soc_10 + att_2")),
                 data = d)
    teste <- coeftest(ajuste, vcov. = vcovHC(ajuste, type = "HC3"))
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               desfecho = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               coef_por_ponto = round(teste["distancia", 1], 3),
               erro_padrao = round(teste["distancia", 2], 3),
               p = signif(teste["distancia", 4], 3),
               n = nobs(ajuste))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 42 | Tabela S32

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    linear <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    duplo <- lm(as.formula(paste(dv, "~ afetiva +", paste(covariaveis, collapse = " + "))),
                data = pareados, weights = pareados$weights)
    ordinal <- polr(as.formula(paste("factor(", dv, ") ~ afetiva")), data = pareados, Hess = TRUE)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               desfecho = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               modelo_linear = round(coef(linear)["afetiva"], 3),
               linear_com_covariaveis = round(coef(duplo)["afetiva"], 3),
               logito_ordinal = round(coef(ordinal)["afetiva"], 3),
               p_logito = signif(2 * pnorm(abs(coef(summary(ordinal))["afetiva", "t value"]),
                                           lower.tail = FALSE), 3),
               row.names = NULL)
  }))
}))


# MATERIAL SUPLEMENTAR | p. 43 | Tabela S33

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pares <- matchit(formula_ps, data = d, method = "nearest", ratio = 1,
                   caliper = 0.2)$match.matrix
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    tratado  <- d[[dv]][as.integer(rownames(pares))]
    controle <- d[[dv]][as.integer(pares[, 1])]
    completo <- complete.cases(tratado, controle)
    tratado  <- tratado[completo]
    controle <- controle[completo]
    sinal <- if (mean(tratado) - mean(controle) < 0) -1 else 1
    limites_g <- as.data.frame(psens(sinal * tratado, sinal * controle,
                                     Gamma = 3, GammaInc = 0.05)$bounds)
    acima <- which(limites_g[, 3] > 0.05)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               desfecho = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               pares = length(tratado),
               diferenca_entre_pares = round(mean(tratado) - mean(controle), 3),
               gama_critico = ifelse(length(acima), limites_g[acima[1], 1], NA))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 44 | Tabela S34

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d <- d[!apply(d[, c("att_3", "att_4", "att_9", "att_10")] > 90, 1, any), ]
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    teste <- coeftest(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               coef = round(teste["afetiva", 1], 3),
               erro_padrao = round(teste["afetiva", 2], 3),
               p = signif(teste["afetiva", 4], 3),
               n = nobs(ajuste))
  }))
}))
c(excluidos_bolsonaro = sum(apply(subset(banco, att_1 == 1)[, c("att_3", "att_4", "att_9", "att_10")] > 90, 1, any)),
  excluidos_lula = sum(apply(subset(banco, att_1 == 2)[, c("att_3", "att_4", "att_9", "att_10")] > 90, 1, any)))


# MATERIAL SUPLEMENTAR | p. 45 | Tabela S35

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
nao_usadas <- c(soc_5 = "Religião", soc_6 = "Situação de trabalho",
                clim_1 = "Percepção de aumento de temperatura",
                clim_6 = "Já ouviu falar em aquecimento global",
                att_5 = "Identificação partidária")
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(names(nao_usadas), function(v) {
    x <- ifelse(pareados[[v]] > 20, NA, pareados[[v]])
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               variavel = unname(nao_usadas[v]),
               dpm = round((mean(x[pareados$afetiva == 1], na.rm = TRUE) -
                            mean(x[pareados$afetiva == 0], na.rm = TRUE)) /
                           sd(x[pareados$afetiva == 1], na.rm = TRUE), 3))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 45 | texto corrido

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4", "att_5_v",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
  else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  d$att_5_v <- ifelse(d$att_5 > 2, 2, d$att_5)
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                 ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    teste <- coeftest(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               coef = round(teste["afetiva", 1], 3),
               p = round(teste["afetiva", 4], 3))
  }))
}))


# MATERIAL SUPLEMENTAR | p. 46 | Tabela S36

banco <- arquivo
for (v in c("valc_4", "valc_5", "val_7", "valc_3")) {
  banco[[v]] <- ifelse(banco[[v]] < 1 | banco[[v]] > 5, NA, banco[[v]])
}
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4",
                 paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))
do.call(rbind, lapply(c("val_7", "valc_3", "valc_4", "valc_5"), function(dv) {
  linha <- lapply(c("bol", "lul"), function(lado) {
    d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
    d$afetiva <- if (lado == "bol")
      as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5)
    else
      as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
    for (v in names(limites)) {
      x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
      d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
      x[is.na(x)] <- median(x, na.rm = TRUE)
      d[[paste0(v, "_v")]] <- x
    }
    pareados <- match.data(matchit(formula_ps, data = d, method = "nearest",
                                   ratio = 2, caliper = 0.2))
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    coeftest(ajuste, vcov. = sandwich::vcovCL, cluster = ~ subclass)["afetiva", c(1, 4)]
  })
  data.frame(desfecho = c(val_7 = "Ocupação de terras por trabalhadores do campo",
                          valc_3 = "Confiança no IBAMA",
                          valc_4 = "Referência: ambientalistas",
                          valc_5 = "Referência: agronegócio")[dv],
             bolsonaro = round(linha[[1]][1], 3), p_bolsonaro = round(linha[[1]][2], 3),
             lula = round(linha[[2]][1], 3), p_lula = round(linha[[2]][2], 3),
             row.names = NULL)
}))


# MATERIAL SUPLEMENTAR | p. 49 | Tabela S38

inventario <- data.frame(
  plano = c(rep("Pelo Bem do Brasil", 9), rep("Brasil da Esperança", 12)),
  secao = c("Valores e princípios, 1.1", "Economia", "Economia", "Economia",
            "Social", "Social", "Social",
            "Sustentabilidade ambiental", "Sustentabilidade ambiental",
            "Desenvolvimento econômico", "Sustentabilidade socioambiental",
            "Sustentabilidade socioambiental", "Desenvolvimento econômico",
            "Sustentabilidade socioambiental", "Desenvolvimento econômico",
            "Compromisso com o projeto de nação", "Sustentabilidade socioambiental",
            "Desenvolvimento econômico", "Desenvolvimento econômico",
            "Desenvolvimento econômico", "Desenvolvimento econômico"),
  matriz = c(rep("Antropocêntrica", 7), rep("Ecocêntrica", 2),
             rep("Ecocêntrica", 6), rep("Ambientalismo dos pobres", 4),
             rep("Ambivalente", 2)))
table(inventario$plano, inventario$matriz)
c(proposicoes = nrow(inventario), por_plano = table(inventario$plano))
