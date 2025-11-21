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
data$level <-as.factor(data$level)
str(data)

model <- lm(calories ~ age + weight + height + bpm_ave +
                duration + fat + water + freq + bmi,
              data = data)
summary(model)

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# courbure pas ok, on ajoute les interactions

model2 <- lm(calories ~ (age + weight + height + bpm_ave +
               duration + fat + water + freq + bmi)^2,
               data = data)

summary(model2)

par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

# good mais overfit faudra faire de la selection ( et puis y a bcp trop de parametres )


# --------------------------------------

ggplot(data, aes(x = level, y = calories, fill = type)) +
  geom_boxplot() +
  labs(
    title = "Calories brûlées selon level et type",
    x = "level",
    y = "calories",
    fill = "type"
  )

par(mfrow=c(1,2))

interaction.plot(
  x.factor     = data$level,
  trace.factor = data$type,
  response     = data$calories,
  col = c(2,4,3,6),       # autant de couleurs que types
  pch = c(18,24,20,17),
  type = "b",
  main = "Interaction : level × type",
  xlab = "Level",
  ylab = "Calories",
  trace.label = "Type"
)

interaction.plot(
  x.factor     = data$type,
  trace.factor = data$level,
  response     = data$calories,
  col = c(2,4,3),         # autant de couleurs que levels
  pch = c(18,24,20),
  type = "b",
  main = "Interaction : type × level",
  xlab = "Type",
  ylab = "Calories",
  trace.label = "Level"
)

model_cat_inter <- lm(calories ~ level * type, data = data)
summary(model_cat_inter)
anova(model_cat_inter)

# pas significatif

datanew = data[,c("calories","type","level")]
datanew = datanew %>%
  mutate(type = str_replace(type,"HIIT", "HIIT-Force")) %>%
  mutate(type = str_replace(type,"Strength", "HIIT-Force"))

model_cat_inter <- lm(calories ~ level * type, data = datanew)
summary(model_cat_inter)
anova(model_cat_inter)

# tjr pas significatif mais pas mal
