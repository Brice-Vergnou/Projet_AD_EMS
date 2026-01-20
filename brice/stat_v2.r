
# Projet : Analyse du nombre de calories brûlées
# Auteur : A remplir
# Date : A remplir

# ================================
# 1. Chargement des packages
# ================================

library(corrplot)
library(tidyverse)
library(gridExtra)
library(reshape2)

# ================================
# 2. Import des données
# ================================

data <- read.csv("../DataGym-projet4modIA-2526.csv", sep = " ")

# Aperçu des données
head(data)
str(data)

# Mise au bon format des variables qualitatives
data$gender <- as.factor(data$gender)
data$type   <- as.factor(data$type)
data$level  <- as.factor(data$level)  # important : level = facteur

str(data)
head(data)

# ================================
# 3. Statistiques descriptives
# ================================

# 3.1 Variables qualitatives : gender, type, level

# ---- Gender ----
EffGender  <- as.vector(table(data$gender))
FreqGender <- data.frame(
  Eff  = EffGender,
  Freq = EffGender / length(data$gender)
)
rownames(FreqGender) <- levels(data$gender)
knitr::kable(FreqGender,
             caption = "Description de la variable Gender",
             booktabs = TRUE, digits = 3)

# ---- Type ----
EffType  <- as.vector(table(data$type))
FreqType <- EffType / length(data$type)
knitr::kable(
  data.frame(modalite = levels(data$type),
             Eff      = EffType,
             Freq     = FreqType),
  caption = "Description de la variable Type",
  booktabs = TRUE, digits = 3
)

# ---- Level ----
EffLevel  <- as.vector(table(data$level))
FreqLevel <- EffLevel / length(data$level)
knitr::kable(
  data.frame(modalite = levels(data$level),
             Eff      = EffLevel,
             Freq     = FreqLevel),
  caption = "Description de la variable Level",
  booktabs = TRUE, digits = 3
)

# Table de contingence Gender x Type
tab_gender_type <- table(data$gender, data$type)
knitr::kable(tab_gender_type,
             caption = "Table de contingence Gender x Type")

# Plot qualitatif x qualitatif : proportion de type selon le genre
ggplot(data, aes(x = type, fill = gender)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  ggtitle("Répartition du type d'exercice selon le genre") +
  theme_minimal()

# 3.2 Variables quantitatives

num_vars <- data %>%
  select(where(is.numeric))

summary(num_vars)

# Histogrammes ciblés
g_age_hist <- ggplot(data, aes(x = age)) +
  geom_histogram(bins = 20) +
  ggtitle("Histogramme de l'âge") +
  theme_minimal()

g_bmi_hist <- ggplot(data, aes(x = bmi)) +
  geom_histogram(bins = 20) +
  ggtitle("Histogramme du BMI") +
  theme_minimal()

g_cal_box <- ggplot(data, aes(y = calories)) +
  geom_boxplot() +
  ggtitle("Boxplot des calories brûlées") +
  theme_minimal()

grid.arrange(g_age_hist, g_bmi_hist, g_cal_box, ncol = 3)

# Histogrammes de toutes les variables quantitatives
num_vars_long <- num_vars %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valeur")

ggplot(num_vars_long, aes(x = valeur)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  ggtitle("Histogrammes des variables quantitatives")

# 3.3 Liens quanti/quali avec violon plots (préférés aux barplots)

# Calories selon le type d'exercice
ggplot(data, aes(x = type, y = calories, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.size = 0.8) +
  ggtitle("Calories brûlées selon le type d'exercice") +
  theme_minimal() +
  theme(legend.position = "none")

# Calories selon le niveau d'exercice
ggplot(data, aes(x = level, y = calories, fill = level)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.size = 0.8) +
  ggtitle("Calories brûlées selon le niveau") +
  theme_minimal() +
  theme(legend.position = "none")

# Plusieurs variables quantitatives selon type
data %>%
  pivot_longer(cols = c(calories, duration, bpm_ave, bmi),
               names_to = "variable", values_to = "valeur") %>%
  ggplot(aes(x = type, y = valeur, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.08, outlier.size = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Variables quantitatives selon le type d'exercice")

# 3.4 Corrélations entre variables quantitatives

M <- cor(num_vars, use = "pairwise.complete.obs")

corrplot(M,
         method = "ellipse",
         order = "hclust")

# Quelques nuages de points utiles

# Calories vs durée
ggplot(data, aes(x = duration, y = calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  ggtitle("Calories en fonction de la durée")

# BMI vs poids
ggplot(data, aes(x = weight, y = bmi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  ggtitle("BMI en fonction du poids")
