library(corrplot)
library(tidyverse)
library(gridExtra)
library(reshape2)

# Stat descriptive

## récupération

data <- read.csv("DataGym-projet4modIA-2526.csv",
                 sep = " ")

head(data)

attributes(data)

str(data)

data$gender=as.factor(data$gender)
data$type=factor(data$type)
str(data)

summary(data)

## descriptif

summary(data$type)
EffType = as.vector(table(data$type))
EffType

Freq = EffType/length(data$type)
knitr::kable(data.frame(modalite=levels(data$type),
                        Eff=EffType,
                        Freq=Freq), 
             caption = 'Description de la variable Type',
             booktabs = TRUE,
             digits=3)

EffGender=as.vector(table(data$gender))
FreqGender= data.frame(Eff = EffGender, 
                     Freq = EffGender/length(data$gender))
rownames(FreqGender)=levels(data$gender)
knitr::kable(FreqGender, caption = 'Description de la variable Gender',booktabs = TRUE,digits=3)

g1<-ggplot(data, aes(x=type))+ 
  geom_bar()+ylab("")+
  ggtitle("Effectifs")
g2<-ggplot(data, aes(x = type)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ylab("")+ggtitle("Frequences")
quan <- as.vector(table(data$type)/nrow(data))
df <- data.frame(group = levels(data$type),value = quan)
g3<-ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+ 
  theme(legend.position="bottom")
grid.arrange(g3,g1,g2,ncol=3)
