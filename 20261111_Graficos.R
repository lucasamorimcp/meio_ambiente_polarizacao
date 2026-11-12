## ============================================================
## Valores ambientais, polarização afetiva e tolerância política no Brasil
## Gráficos 1, 2 e 3 do texto principal
## Requer o arquivo BANCO_UFPA.xlsx na pasta de trabalho.
## As especificações reproduzem as do arquivo de replicação.
## ============================================================

suppressMessages({
  library(readxl)
  library(psych)
  library(lmtest)
  library(sandwich)
  library(MatchIt)
  library(dplyr)
  library(ggplot2)
})

# setwd("caminho/para/a/pasta/com/os/dados")

arquivo <- read_excel("BANCO_UFPA.xlsx", .name_repair = "minimal")
eixo_x  <- "Valores ambientais (← Mais antropocêntrico | Mais ecocêntrico →)"


# Escore de valores ambientais e controles (Gráficos 1 e 2)

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
banco$voto   <- dplyr::recode(banco$voto, `2` = 1, `1` = 0)

coeficientes <- function(modelo, rotulos) {
  teste <- coeftest(modelo, vcov. = vcovHC(modelo, type = "HC3"))
  intervalo <- coefci(modelo, vcov. = vcovHC(modelo, type = "HC3"))
  d <- data.frame(termo = rotulos, coef = teste[-1, 1], inf = intervalo[-1, 1],
                  sup = intervalo[-1, 2], p = teste[-1, 4])
  d$termo <- factor(d$termo, levels = rev(rotulos))
  d$signif <- factor(ifelse(d$p < 0.05, "Significativo", "Não significativo"),
                     levels = c("Não significativo", "Significativo"))
  d
}

grafico_coef <- function(d, precisao) {
  ggplot(d, aes(x = coef, y = termo, colour = signif)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(xmin = inf, xmax = sup), width = 0, orientation = "y", linewidth = 0.6) +
    geom_point(size = 2.6) +
    scale_colour_manual(values = c("Não significativo" = "grey60", "Significativo" = "black"), drop = FALSE) +
    scale_x_continuous(labels = scales::label_number(accuracy = precisao, decimal.mark = ",")) +
    labs(x = eixo_x, y = NULL, colour = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

mod1 <- lm(escore ~ voto + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
g1 <- grafico_coef(coeficientes(mod1, c("Voto em Lula", "Idade", "Escolaridade", "Masculino", "Renda familiar")), 0.01)
ggsave("20261111_Grafico1.png", g1, width = 16, height = 10.5, units = "cm", dpi = 300)

mod2 <- lm(escore ~ att_3 + att_4 + soc_3 + soc_9 + soc_4 + soc_10, data = banco)
g2 <- grafico_coef(coeficientes(mod2, c("Identificação com Lula", "Identificação com Bolsonaro",
                                        "Idade", "Escolaridade", "Masculino", "Renda familiar")), 0.1)
ggsave("20261111_Grafico2.png", g2, width = 16, height = 10.5, units = "cm", dpi = 300)


# Valores preditos de intolerância após o pareamento (Gráfico 3)

banco <- arquivo
banco$valc_4 <- ifelse(banco$valc_4 > 5, NA, banco$valc_4)
banco$valc_5 <- ifelse(banco$valc_5 > 5, NA, banco$valc_5)
limites <- c(valc_1 = 4, att_2 = 7, soc_8 = 5, soc_9 = 7, soc_10 = 6)
covariaveis <- c("soc_1", "soc_2a", "soc_3", "soc_4", paste0(names(limites), "_v"), paste0(names(limites), "_nr"))
formula_ps <- as.formula(paste("afetiva ~", paste(covariaveis, collapse = " + ")))

preditos <- do.call(rbind, lapply(c("bol", "lul"), function(lado) {
  d <- subset(banco, att_1 == ifelse(lado == "bol", 1, 2))
  d$afetiva <- if (lado == "bol")
    as.integer(d$att_3 %in% 1:3 & d$att_4 %in% 9:11 & d$att_9 %in% 1:2 & d$att_10 %in% 4:5) else
    as.integer(d$att_3 %in% 9:11 & d$att_4 %in% 1:3 & d$att_9 %in% 4:5 & d$att_10 %in% 1:2)
  for (v in names(limites)) {
    x <- ifelse(d[[v]] > limites[[v]], NA, d[[v]])
    d[[paste0(v, "_nr")]] <- as.integer(is.na(x))
    x[is.na(x)] <- median(x, na.rm = TRUE)
    d[[paste0(v, "_v")]] <- x
  }
  pareados <- match.data(matchit(formula_ps, data = d, method = "nearest", ratio = 2, caliper = 0.2))
  do.call(rbind, lapply(c("valc_4", "valc_5"), function(dv) {
    ajuste <- lm(as.formula(paste(dv, "~ afetiva")), data = pareados, weights = pareados$weights)
    V <- vcovCL(ajuste, cluster = ~subclass)
    X <- rbind(c(1, 0), c(1, 1))
    ajustado <- as.vector(X %*% coef(ajuste))
    ep <- sqrt(diag(X %*% V %*% t(X)))
    q <- qt(0.975, df = ajuste$df.residual)
    data.frame(eleitorado = ifelse(lado == "bol", "Eleitores de Bolsonaro", "Eleitores de Lula"),
               alvo = ifelse(dv == "valc_4", "Ambientalistas", "Agronegócio"),
               condicao = c("Não polarizado", "Polarizado"),
               predito = ajustado, inf = ajustado - q * ep, sup = ajustado + q * ep,
               p = coeftest(ajuste, vcov. = V)["afetiva", 4])
  }))
}))
preditos$diferenca <- factor(ifelse(preditos$p < 0.05, "Significativa", "Não significativa"),
                             levels = c("Não significativa", "Significativa"))
preditos$alvo <- factor(preditos$alvo, levels = c("Agronegócio", "Ambientalistas"))

g3 <- ggplot(preditos, aes(x = condicao, y = predito, group = alvo, colour = diferenca)) +
  geom_line(aes(linetype = alvo), linewidth = 0.9, position = position_dodge(width = 0.12)) +
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0, linewidth = 0.6, position = position_dodge(width = 0.12)) +
  geom_point(aes(shape = alvo), size = 2.8, position = position_dodge(width = 0.12)) +
  facet_wrap(~ eleitorado) +
  scale_colour_manual(values = c("Não significativa" = "grey65", "Significativa" = "black"), drop = FALSE) +
  scale_linetype_manual(values = c("Agronegócio" = "solid", "Ambientalistas" = "dashed")) +
  scale_shape_manual(values = c("Agronegócio" = 16, "Ambientalistas" = 17)) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, shape = 15, size = 4))) +
  labs(x = NULL, y = "Valor predito de intolerância", colour = "Diferença (p < 0,05)",
       linetype = "Grupo-alvo", shape = "Grupo-alvo") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "vertical", strip.text = element_text(face = "bold"),
        strip.background = element_rect(colour = "black"))
ggsave("20261111_Grafico3.png", g3, width = 16, height = 11, units = "cm", dpi = 300)

print(preditos[, c("eleitorado", "alvo", "condicao", "predito", "inf", "sup", "p")], digits = 4)
