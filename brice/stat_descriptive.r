library(corrplot)
library(tidyverse)
library(gridExtra)
library(reshape2)

# Stat descriptive

## récupération

data <- read.csv("../DataGym-projet4modIA-2526.csv",
                 sep = " ")

head(data)

attributes(data)

str(data)

data$gender=as.factor(data$gender)
data$type=as.factor(data$type)
str(data)

summary(data)

## descriptif

### qualitatif

# Gender
EffGender  <- as.vector(table(data$gender))
FreqGender <- data.frame(Eff = EffGender,
                         Freq = EffGender / length(data$gender))
rownames(FreqGender) <- levels(data$gender)
knitr::kable(FreqGender, caption = "Description de la variable Gender",
             booktabs = TRUE, digits = 3)

# Type
EffType <- as.vector(table(data$type))
FreqType <- EffType / length(data$type)
knitr::kable(data.frame(modalite = levels(data$type),
                        Eff = EffType,
                        Freq = FreqType),
             caption = "Description de la variable Type",
             booktabs = TRUE, digits = 3)


tab_gender_type <- table(data$gender, data$type)
tab_gender_type
knitr::kable(tab_gender_type,
             caption = "Table de contingence Gender x Type")

ggplot(data, aes(x = type, fill = gender)) +
  geom_bar(position = "fill") +   # proportions
  ylab("Proportion") +
  ggtitle("Répartition du type d'exercice selon le genre")

### quantitatif

num_vars <- data %>%
  select(where(is.numeric))   
summary(num_vars)

g_age_hist <- ggplot(data, aes(x = age)) + geom_histogram(bins = 20) +
  ggtitle("Histogramme de l'âge")

g_bmi_hist <- ggplot(data, aes(x = bmi)) + geom_histogram(bins = 20) +
  ggtitle("Histogramme du BMI")

g_cal_box <- ggplot(data, aes(y = calories)) + geom_boxplot() +
  ggtitle("Boxplot des calories brûlées")

grid.arrange(g_age_hist, g_bmi_hist, g_cal_box, ncol = 3)

num_vars_long <- num_vars %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valeur")

ggplot(num_vars_long, aes(x = valeur)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  ggtitle("Histogrammes des variables quantitatives")


### quanti et quali

ggplot(data, aes(x = type, y = calories, fill = type)) +
  geom_boxplot() +
  ggtitle("Calories brûlées selon le type d'exercice") +
  theme_minimal() +
  theme(legend.position = "none")

data %>%
  pivot_longer(cols = c(calories, duration, bpm_ave, bmi),
               names_to = "variable", values_to = "valeur") %>%
  ggplot(aes(x = type, y = valeur, fill = type)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Comparaison de plusieurs variables quantitatives selon le type d'exercice")


## liaison / corrélation

num_vars <- data %>% select(where(is.numeric))
M <- cor(num_vars, use = "pairwise.complete.obs")

corrplot(M, 
         method = "ellipse",
         order = "hclust")

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


### autre

chisq.test(tab_gender_type)

