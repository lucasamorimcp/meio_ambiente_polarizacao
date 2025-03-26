
###Importando dados para ambiente de trabalho

library(readxl)

BANCO_UFPA <- read_excel("BANCO_UFPA.xlsx")

###Avaliando e recodificando atitudes ambientais

#[ [poli_2] Sobre a proposta de aumentar o n?mero de agrot?xicos permitidos no
#Brasil, voc? diria que:

table(BANCO_UFPA$poli_2)

BANCO_UFPA$poli_2 <- ifelse(BANCO_UFPA$poli_2 > 5, NA, BANCO_UFPA$poli_2)

#[ind_3] Sobre a proposta de aumentar o n?mero de terras destinadas aos
#ind?genas e quilombolas, voc? diria que: 

table(BANCO_UFPA$ind_3)

BANCO_UFPA$ind_3 <- ifelse(BANCO_UFPA$ind_3 > 5, NA, BANCO_UFPA$ind_3)

library(dplyr)

BANCO_UFPA <- BANCO_UFPA %>%
  mutate(ind_3 = recode(ind_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) #invertendo pois tem uma dire??o invertida em rela??o a poli_2

#[ind_4] Sobre a proposta de permitir garimpo em terras ind?genas, voc? diria
#que:

table(BANCO_UFPA$ind_4)

BANCO_UFPA$ind_4 <- ifelse(BANCO_UFPA$ind_4 > 5, NA, BANCO_UFPA$ind_4)

#[poli_3] Sobre a proposta de diminuir as regras de licen?a ambiental para as
#obras de governos e empresas, voc? diria que:

table(BANCO_UFPA$poli_3)

BANCO_UFPA$poli_3 <- ifelse(BANCO_UFPA$poli_3 > 5, NA, BANCO_UFPA$poli_3)

#[poli_4] Sobre a proposta de que os servi?os de saneamento e abastecimento
#d??gua sejam oferecidos apenas por empresas privadas, e n?o pelo governo,
#voc? diria que:

table(BANCO_UFPA$poli_4)

BANCO_UFPA$poli_4 <- ifelse(BANCO_UFPA$poli_4 > 5, NA, BANCO_UFPA$poli_4)

###An?lise Fatorial

library(psych)

af_ambiente <- fa(
  BANCO_UFPA[, c("ind_3", "ind_4", "poli_2", "poli_3", "poli_4")],
  nfactors = 1,
  rotate = "varimax"
)
print(af_ambiente)

###Teste de ajuste AF

#KMO

kmo_result <- KMO(BANCO_UFPA[, c("ind_3", "ind_4", "poli_2", "poli_3", "poli_4")])
print(kmo_result)

# Teste de Bartlett
bartlett_result <- cortest.bartlett(BANCO_UFPA[, c("ind_3", "ind_4", "poli_2", "poli_3", "poli_4")])
print(bartlett_result)

###Regress?o

#Factor scores

scores <- factor.scores(BANCO_UFPA[, c("ind_3", "ind_4", "poli_2", "poli_3", "poli_4")], af_ambiente)
BANCO_UFPA$af_ambiente_score <- scores$scores[, 1]

table(BANCO_UFPA$af_ambiente_score)

#[soc_3] Qual ? a sua idade?

table(BANCO_UFPA$soc_3)

#[soc_9] Qual ? o seu grau de escolaridade?

table(BANCO_UFPA$soc_9)

BANCO_UFPA$soc_9 <- ifelse(BANCO_UFPA$soc_9 > 7, NA, BANCO_UFPA$soc_9)

#[soc_4] Qual ? o seu g?nero?

table(BANCO_UFPA$soc_4)

BANCO_UFPA$soc_4 <- ifelse(BANCO_UFPA$soc_4 > 2, NA, BANCO_UFPA$soc_4)

#[soc_10] Somando todas as suas fontes de renda e dos familiares que moram
#com voc?, qual ? a renda mensal da sua fam?lia?

table(BANCO_UFPA$soc_10)

BANCO_UFPA$soc_10 <- ifelse(BANCO_UFPA$soc_10 > 6, NA, BANCO_UFPA$soc_10)

#[att_1] No segundo turno das elei??es para presidente de 2022, voc?

table(BANCO_UFPA$att_1)

BANCO_UFPA$att_1 <- ifelse(BANCO_UFPA$att_1 > 2, NA, BANCO_UFPA$att_1)

BANCO_UFPA <- BANCO_UFPA %>%
  mutate(att_1 = recode(att_1, `2` = 1, `1` = 0))

#[att_3] Numa escala de 0 a 10, o quanto voc? se identifica com o Presidente
#Lula?

table(BANCO_UFPA$att_3)

BANCO_UFPA$att_3 <- ifelse(BANCO_UFPA$att_3 > 11, NA, BANCO_UFPA$att_3)

#[att_4] Numa escala de 0 a 10, o quanto voc? se identifica com o ex-Presidente
#Jair Bolsonaro?

table(BANCO_UFPA$att_4)

BANCO_UFPA$att_4 <- ifelse(BANCO_UFPA$att_4 > 11, NA, BANCO_UFPA$att_4)

#Regress?o

#Voto

reg1 <- lm(af_ambiente_score ~ att_1+soc_3+soc_9+soc_4+soc_10, data = BANCO_UFPA)
summary(reg1)

table(BANCO_UFPA$soc_2)

library(broom)
library(ggplot2)

resultados_reg1 <- tidy(reg1, conf.int = TRUE)

resultados_reg1 <- resultados_reg1 %>%
  filter(term != "(Intercept)") %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2)))


print(resultados_reg1)

novas_labels <- c(
  "att_1" = "Voto em Lula", 
  "soc_3" = "Idade", 
  "soc_9" = "Escolaridade", 
  "soc_4" = "Masculino", 
  "soc_10" = "Renda"
)

# Criar o gr?fico
ggplot(resultados_reg1, aes(x = estimate, y = factor(term, levels = rev(names(novas_labels))))) +
  geom_point(color = "#1f77b4", size = 4) +  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, color = "#1f77b4") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.7) +  
  labs(title = "Coeficientes da Regressão Linear - Valores Ambientais",
       subtitle = "Estimativas dos coeficientes com intervalos de confiança (95%)",
       x = "Estimativa do Coeficiente (Utilitaristas -> Ecológicos)",
       y = "Variáveis Preditoras") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),  
    panel.grid.major.x = element_line(color = "gray90", size = 0.5)  
  ) +
  scale_y_discrete(labels = novas_labels)

#FT

reg2 <- lm(af_ambiente_score ~ att_3+att_4+soc_3+soc_9+soc_4+soc_10, data = BANCO_UFPA)
summary(reg2)

resultados_reg2 <- tidy(reg2, conf.int = TRUE)

resultados_reg2 <- resultados_reg2 %>%
  filter(term != "(Intercept)") %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2)))


print(resultados_reg2)

novas_labels2 <- c(
  "att_3" = "Identifica??o com Lula",
  "att_4" = "Identifica??o com Bolsonaro",
  "soc_3" = "Idade", 
  "soc_9" = "Escolaridade", 
  "soc_4" = "Masculino", 
  "soc_10" = "Renda"
)

# Criar o gr?fico
ggplot(resultados_reg2, aes(x = estimate, y = factor(term, levels = rev(names(novas_labels2))))) +
  geom_point(color = "#1f77b4", size = 4) +  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, color = "#1f77b4") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.7) +  
  labs(title = "Coeficientes da Regress?o Linear - Valores Ambientais",
       subtitle = "Estimativas dos coeficientes com intervalos de confian?a (95%)",
       x = "Estimativa do Coeficiente (Utilitaristas -> Ecol?gicos)",
       y = "Vari?veis Preditoras") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),  
    panel.grid.major.x = element_line(color = "gray90", size = 0.5)  
  ) +
  scale_y_discrete(labels = novas_labels2)

###Matiching

BANCO_UFPA <- read_excel("BANCO_UFPA.xlsx")

#Recodificando variaveis de intolerancia

BANCO_UFPA$valc_4 <- ifelse(BANCO_UFPA$valc_4 > 5, NA, BANCO_UFPA$valc_4) #Meio ambiente
BANCO_UFPA$valc_5 <- ifelse(BANCO_UFPA$valc_5 > 5, NA, BANCO_UFPA$valc_5) #Agronegócio

#Medindo polarização afetiva entre bolsonaristas

Bolsonaro <- subset(BANCO_UFPA, att_1 == 1)

table(Bolsonaro$att_3) #Id. Lula
table(Bolsonaro$att_4) #Id. Bolsonaro
table(Bolsonaro$att_9) #Intolerancia com Bolsonaristas
table(Bolsonaro$att_10) #Intolerancia com Lulistas

Bolsonaro$afetiva <- ifelse(
  Bolsonaro$att_3 %in% c(1, 2, 3) & 
    Bolsonaro$att_4 %in% c(9, 10, 11) & 
    Bolsonaro$att_9 %in% c(1, 2) & 
    Bolsonaro$att_10 %in% c(4, 5), 
  1, 
  0
)

prop.table(table(Bolsonaro$afetiva))

#Medindo polarização afetiva entre lulistas

Lula <- subset(BANCO_UFPA, att_1 == 2)

Lula$afetiva <- ifelse(
  Lula$att_3 %in% c(9, 10, 11) & 
    Lula$att_4 %in% c(1, 2, 3) & 
    Lula$att_9 %in% c(4, 5) & 
    Lula$att_10 %in% c(1, 2), 
  1, 
  0
)

prop.table(table(Lula$afetiva))


#Matiching Bolsonaro

library(MatchIt)

m.out.bolsonaro <- matchit(afetiva ~ soc_1+soc_2a+soc_3+soc_4+valc_1+att_2+soc_8+soc_9+soc_10, 
                 data = Bolsonaro, 
                 method = "nearest", 
                 ratio = 2)

summary(m.out.bolsonaro)

plot(m.out.bolsonaro, type = "jitter")

#Matiching Lula

m.out.lula <- matchit(afetiva ~ soc_1+soc_2a+soc_3+soc_4+valc_1+att_2+soc_8+soc_9+soc_10, 
                 data = Lula, 
                 method = "nearest", 
                 ratio = 2)
summary(m.out.lula)

plot(m.out.lula, type = "jitter")

#Determinantes intolerência

#Bolsonaristas

matched_data_bolsonaro <- match.data(m.out.bolsonaro)

library(gridExtra)

modelo_bolsonaro_amb <- lm(valc_4 ~ afetiva, data = matched_data_bolsonaro) #intolerancia aos ambientalistas
summary(modelo_bolsonaro_amb)
modelo_bolsonaro_agro <- lm(valc_5 ~ afetiva, data = matched_data_bolsonaro) #intolerancia aos produtores do agronegócio
summary(modelo_bolsonaro_agro)

pred_bolsonaro <- data.frame(
  afetiva = c(0, 1),
  valc_4 = predict(modelo_bolsonaro_amb, newdata = data.frame(afetiva = c(0, 1)), interval = "confidence")[,1:3],
  valc_5 = predict(modelo_bolsonaro_agro, newdata = data.frame(afetiva = c(0, 1)), interval = "confidence")[,1:3]
)

colnames(pred_bolsonaro) <- c("afetiva", "valc_4_pred", "valc_4_lwr", "valc_4_upr", 
                              "valc_5_pred", "valc_5_lwr", "valc_5_upr")

g1_bolsonaro <- ggplot(pred_bolsonaro, aes(x = factor(afetiva), y = valc_4_pred)) +
  geom_col(fill = "#1f78b4", alpha = 0.8, width = 0.6) +  
  geom_errorbar(aes(ymin = valc_4_lwr, ymax = valc_4_upr), 
                width = 0.2, size = 0.6, color = "black") +  
  labs(x = "Polarização Afetiva", 
       y = "Intolerância aos Ambientalistas (Predito)") +  
  scale_x_discrete(labels = c("Não Polarizado", "Polarizado")) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +  
  theme_classic(base_size = 14) +  
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

g2_bolsonaro <- ggplot(pred_bolsonaro, aes(x = factor(afetiva), y = valc_5_pred)) +
  geom_col(fill = "#e31a1c", alpha = 0.8, width = 0.6) +  
  geom_errorbar(aes(ymin = valc_5_lwr, ymax = valc_5_upr), 
                width = 0.2, size = 0.6, color = "black") +  
  labs(x = "Polarização Afetiva", 
       y = "Intolerância aos Produtores do Agronegócio (Predito)") +  
  scale_x_discrete(labels = c("Não Polarizado", "Polarizado")) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +  
  theme_classic(base_size = 14) +  
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

grid.arrange(g1_bolsonaro, g2_bolsonaro, ncol = 2)

#Lulista

matched_data_lula <- match.data(m.out.lula)

modelo_lula_amb <- lm(valc_4 ~ afetiva, data = matched_data_lula) #intolerancia aos ambientalistas
summary(modelo_lula_amb)
modelo_lula_agro <- lm(valc_5 ~ afetiva, data = matched_data_lula) #intolerancia aos produtores do agronegócio
summary(modelo_lula_agro)

pred_lula <- data.frame(
  afetiva = c(0, 1),
  valc_4 = predict(modelo_lula_amb, newdata = data.frame(afetiva = c(0, 1)), interval = "confidence")[,1:3],
  valc_5 = predict(modelo_lula_agro, newdata = data.frame(afetiva = c(0, 1)), interval = "confidence")[,1:3]
)

colnames(pred_lula) <- c("afetiva", "valc_4_pred", "valc_4_lwr", "valc_4_upr", 
                              "valc_5_pred", "valc_5_lwr", "valc_5_upr")

g1_lula <- ggplot(pred_lula, aes(x = factor(afetiva), y = valc_4_pred)) +
  geom_col(fill = "#1f78b4", alpha = 0.8, width = 0.6) +  
  geom_errorbar(aes(ymin = valc_4_lwr, ymax = valc_4_upr), 
                width = 0.2, size = 0.6, color = "black") +  
  labs(x = "Polarização Afetiva", 
       y = "Intolerância aos Ambientalistas (Predito)") +  
  scale_x_discrete(labels = c("Não Polarizado", "Polarizado")) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +  
  theme_classic(base_size = 14) +  
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

g2_lula <- ggplot(pred_lula, aes(x = factor(afetiva), y = valc_5_pred)) +
  geom_col(fill = "#e31a1c", alpha = 0.8, width = 0.6) +  
  geom_errorbar(aes(ymin = valc_5_lwr, ymax = valc_5_upr), 
                width = 0.2, size = 0.6, color = "black") +  
  labs(x = "Polarização Afetiva", 
       y = "Intolerância aos Produtores do Agronegócio (Predito)") +  
  scale_x_discrete(labels = c("Não Polarizado", "Polarizado")) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +  
  theme_classic(base_size = 14) +  
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

grid.arrange(g1_lula, g2_lula, ncol = 2)
