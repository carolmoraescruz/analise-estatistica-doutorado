# **************************** Configurações gerais **************************** #

# Diretório de trabalho
setwd("C:/Users/Dell/Documents/Cursos/Academicos/Doutorado em Ciencia e Engenharia de Materiais/Tese/analise-estatistica-doutorado-repo/analise-r/etapa-3")

# Importação das bibliotecas - ANOVA
install.packages("compute.es"); install.packages("multcomp")
library(compute.es); library(multcomp)


# ============================================================================== #


# ************************** Resultados após 7 dias **************************** #

# ********************** Extração e tratamento dos dados *********************** #

# Importar os dados
MEA_7dias <- read.csv("MEA_A_7dias_B.csv", header = TRUE)
RFTA_7dias <- read.csv("RFTA_A_7dias_B.csv", header = TRUE)
RCTA_7dias <- read.csv("RCTA_A_7dias_B.csv", header = TRUE)


# Mudar o tipo dos dados de "wide" para "long"
MEA7_stack <- stack(MEA_7dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(MEA7_stack) <- c("MEA", "Formula")

RFTA7_stack <- stack(RFTA_7dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(RFTA7_stack) <- c("RFTA", "Formula")

RCTA7_stack <- stack(RCTA_7dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(RCTA7_stack) <- c("RCTA", "Formula")

# DataFrame unificado dos dados
df_7dias <- data.frame(MEA7_stack, RFTA7_stack, RCTA7_stack)
df_7dias <- df_7dias[, c("Formula", "MEA", "RFTA", "RCTA")]


# ********************************** MANOVA *********************************** #

saida7 <- cbind(df_7dias$MEA, df_7dias$RFTA, df_7dias$RCTA)
modelo7 <- manova(saida7 ~ Formula, data = df_7dias)
summary(modelo7, intercept = TRUE) #teste de Pillai


# *********************************** ANOVA *********************************** #

# MEA
modMEA7 <- aov(MEA ~ Formula, data = df_7dias)
summary(modMEA7)


# RFTA
modRFTA7 <- aov(RFTA ~ Formula, data = df_7dias)
summary(modRFTA7)


# RCTA
modRCTA7 <- aov(RCTA ~ Formula, data = df_7dias)
summary(modRCTA7)


# ******************** Testes Post hoc - Bonferroni e Tukey ******************* #

# MEA
pairwise.t.test(df_7dias$MEA, df_7dias$Formula, p.adjust.method = "bonferroni")
postHocsMEA7 <- glht(modMEA7, linfct = mcp(Formula = "Tukey"))
summary(postHocsMEA7)


# RFTA
pairwise.t.test(df_7dias$RFTA, df_7dias$Formula, p.adjust.method = "bonferroni")
postHocsRFTA7 <- glht(modRFTA7, linfct = mcp(Formula = "Tukey"))
summary(postHocsRFTA7)


# RCTA
pairwise.t.test(df_7dias$RCTA, df_7dias$Formula, p.adjust.method = "bonferroni")
postHocsRCTA7 <- glht(modRCTA7, linfct = mcp(Formula = "Tukey"))
summary(postHocsRCTA7)


# ============================================================================== #


# ************************** Resultados após 28 dias *************************** #

# ********************** Extração e tratamento dos dados *********************** #

# Importar os dados
MEA_28dias <- read.csv("MEA_A_28dias_B.csv", header = TRUE)
RFTA_28dias <- read.csv("RFTA_A_28dias_B.csv", header = TRUE)
RCTA_28dias <- read.csv("RCTA_A_28dias_B.csv", header = TRUE)


# Mudar o tipo dos dados de "wide" para "long"
MEA28_stack <- stack(MEA_28dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(MEA28_stack) <- c("MEA", "Formula")

RFTA28_stack <- stack(RFTA_28dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(RFTA28_stack) <- c("RFTA", "Formula")

RCTA28_stack <- stack(RCTA_28dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(RCTA28_stack) <- c("RCTA", "Formula")


# DataFrame unificado dos dados
df_28dias <- data.frame(MEA28_stack, RFTA28_stack, RCTA28_stack)
df_28dias <- df_28dias[, c("Formula", "MEA", "RFTA", "RCTA")] 


# ********************************** MANOVA *********************************** #

saida28 <- cbind(df_28dias$MEA, df_28dias$RFTA, df_28dias$RCTA)
modelo28 <- manova(saida28 ~ Formula, data = df_28dias)
summary(modelo28, intercept = TRUE) #teste de Pillai


# *********************************** ANOVA *********************************** #

# MEA
modMEA28 <- aov(MEA ~ Formula, data = df_28dias)
summary(modMEA28)


# RFTA
modRFTA28 <- aov(RFTA ~ Formula, data = df_28dias)
summary(modRFTA28)


# RCTA
modRCTA28 <- aov(RCTA ~ Formula, data = df_28dias)
summary(modRCTA28)


# ******************** Testes Post hoc - Bonferroni e Tukey ******************* #

# MEA
pairwise.t.test(df_28dias$MEA, df_28dias$Formula, p.adjust.method = "bonferroni")
postHocsMEA28 <- glht(modMEA28, linfct = mcp(Formula = "Tukey"))
summary(postHocsMEA28)


# RFTA
pairwise.t.test(df_28dias$RFTA, df_28dias$Formula, p.adjust.method = "bonferroni")
postHocsRFTA28 <- glht(modRFTA28, linfct = mcp(Formula = "Tukey"))
summary(postHocsRFTA28)


# RCTA
pairwise.t.test(df_28dias$RCTA, df_28dias$Formula, p.adjust.method = "bonferroni")
postHocsRCTA28 <- glht(modRCTA28, linfct = mcp(Formula = "Tukey"))
summary(postHocsRCTA28)


# ============================================================================== #


# ************************** Resultados após 91 dias *************************** #

# ********************** Extração e tratamento dos dados *********************** #

# Importar os dados
MEA_91dias <- read.csv("MEA_A_91dias_B.csv", header = TRUE)
RFTA_91dias <- read.csv("RFTA_A_91dias_B.csv", header = TRUE)
RCTA_91dias <- read.csv("RCTA_A_91dias_B.csv", header = TRUE)


# Mudar o tipo dos dados de "wide" para "long"
MEA91_stack <- stack(MEA_91dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(MEA91_stack) <- c("MEA", "Formula")

RFTA91_stack <- stack(RFTA_91dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(RFTA91_stack) <- c("RFTA", "Formula")

RCTA91_stack <- stack(RCTA_91dias, select = c("A_N", "AA_0.5C_B", "AA_1.0C_B"))
names(RCTA91_stack) <- c("RCTA", "Formula")


# DataFrame unificado dos dados
df_91dias <- data.frame(MEA91_stack, RFTA91_stack, RCTA91_stack)
df_91dias <- df_91dias[, c("Formula", "MEA", "RFTA", "RCTA")]


# ********************************** MANOVA *********************************** #

saida91 <- cbind(df_91dias$MEA, df_91dias$RFTA, df_91dias$RCTA)
modelo91 <- manova(saida91 ~ Formula, data = df_91dias)
summary(modelo91, intercept = TRUE) #teste de Pillai


# *********************************** ANOVA *********************************** #

# MEA
modMEA91 <- aov(MEA ~ Formula, data = df_91dias)
summary(modMEA91)


# RFTA
modRFTA91 <- aov(RFTA ~ Formula, data = df_91dias)
summary(modRFTA91)


# RCTA
modRCTA91 <- aov(RCTA ~ Formula, data = df_91dias)
summary(modRCTA91)


# ******************** Testes Post hoc - Bonferroni e Tukey ******************* #

# MEA
pairwise.t.test(df_91dias$MEA, df_91dias$Formula, p.adjust.method = "bonferroni")
postHocsMEA91 <- glht(modMEA91, linfct = mcp(Formula = "Tukey"))
summary(postHocsMEA91)


# RFTA
pairwise.t.test(df_91dias$RFTA, df_91dias$Formula, p.adjust.method = "bonferroni")
postHocsRFTA91 <- glht(modRFTA91, linfct = mcp(Formula = "Tukey"))
summary(postHocsRFTA91)


# RCTA
pairwise.t.test(df_91dias$RCTA, df_91dias$Formula, p.adjust.method = "bonferroni")
postHocsRCTA91 <- glht(modRCTA91, linfct = mcp(Formula = "Tukey"))
summary(postHocsRCTA91)