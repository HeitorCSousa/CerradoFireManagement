#Read data
data.spec <- read.csv("Respostas_Esps.csv", h=T, stringsAsFactors = T)

data.rural <- read.csv("Respostas_PropsRurais.csv", h=T, stringsAsFactors = T)

head(data.spec)

head(data.rural)

# Analytical Hierarchical Process -----------------------------------------

#Select only variables related to the AHP
ahp.spec <- data.spec[,c(17:50)]
ahp.rural <- data.rural[,c(35:68)]

#Standardize to Saaty scale (-5 to 5), 1 = equal importance
ahp.spec <- ahp.spec -4
ahp.spec[ahp.spec<1] <- ahp.spec[ahp.spec<1] -2

ahp.rural <- ahp.rural -4
ahp.rural[ahp.rural<1] <- ahp.rural[ahp.rural<1] -2

#Load packages
library(ahpsurvey)
library(ggplot2)
library(tidyverse)
library(knitr)

#AHP for main objectives (Biodiversity, Economy, Health, and Water)
#Creating pairwise comparison matrices
main.atts <- c("Bio", "Econ", "Heal", "Wat")


#Individual preference weights
specahp <- ahp.spec %>% 
  ahp.mat(main.atts, negconvert = T)

ruralahp <- ahp.rural %>% 
  ahp.mat(main.atts, negconvert = T)

#Aggregated preference weights
specmean <- ahp.aggpref(specahp, main.atts, method = "arithmetic")
specmean

specsd <- ahp.aggpref(specahp, main.atts, method = "arithmetic", aggmethod = "sd")
specsd

ruralmean <- ahp.aggpref(ruralahp, main.atts, method = "arithmetic")
ruralmean

ruralsd <- ahp.aggpref(ruralahp, main.atts, method = "arithmetic", aggmethod = "sd")
ruralsd

t(data.frame(specmean, specsd, ruralmean, ruralsd))

dict <- c("Bio" = "Biodiversity", 
          "Econ" = "Economy", 
          "Heal" = "Health", 
          "Wat" = "Water")

#Canned routine
#Main objectives
ahp.data <- rbind(ahp.spec, ahp.rural)
ahp.data$Class <- c(rep("Specialist",49),rep("RuralProp", 48))

#Eigenvalue
canned.main.eigen.trunc <- ahp(df = ahp.data[,c(1:6,35)], 
                         atts = main.atts, 
                         negconvert = TRUE, 
                         reciprocal = TRUE,
                         method = 'eigen', 
                         aggmethod = "eigen", 
                         censorcr = 0.11,
                         agg = TRUE,
                         ID = c("Class"))

canned.main.eigen <- ahp(df = ahp.data[,c(1:6,35)], 
                         atts = main.atts, 
                         negconvert = TRUE, 
                         reciprocal = TRUE,
                         method = 'eigen', 
                         aggmethod = "eigen", 
                         agg = TRUE,
                         ID = c("Class"))
canned.main.eigen$aggpref
canned.main.eigen.trunc$aggpref
canned.main.eigen.trunc$indpref



canned.main.eigen.trunc$indpref %>%
  group_by(cID) %>%
  dplyr::summarize(MeanBio = mean(Bio, na.rm=TRUE),
                   SDBio = sd(Bio, na.rm=TRUE),
                   MeanEcon = mean(Econ, na.rm=TRUE),
                   SDEcon = sd(Econ, na.rm=TRUE),
                   MeanHeal = mean(Heal, na.rm=TRUE),
                   SDHeal = sd(Heal, na.rm=TRUE),
                   MeanWat = mean(Wat, na.rm=TRUE),
                   SDWat = sd(Wat, na.rm=TRUE))

canned.main.eigen$indpref %>%
  group_by(cID) %>%
  dplyr::summarize(MeanBio = mean(Bio, na.rm=TRUE),
                   SDBio = sd(Bio, na.rm=TRUE),
                   MeanEcon = mean(Econ, na.rm=TRUE),
                   SDEcon = sd(Econ, na.rm=TRUE),
                   MeanHeal = mean(Heal, na.rm=TRUE),
                   SDHeal = sd(Heal, na.rm=TRUE),
                   MeanWat = mean(Wat, na.rm=TRUE),
                   SDWat = sd(Wat, na.rm=TRUE))

#Boxplots
quartz(8,8)
canned.main.eigen$indpref %>% 
  mutate(rowid = 1:nrow(canned.main.eigen$indpref)) %>%
  gather(Bio, Econ, Heal, Wat, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref, fill = cID)) + 
  geom_violin(alpha = 0.4, width = 0.8, color = "transparent", aes(fill = cID)) +
  geom_jitter(alpha = 0.5, height = 0, width = 0.1, aes(color = cID)) +
  geom_boxplot(alpha = 0.4, width = 0.3) +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(labels = levels(as.factor(canned.main.eigen$indpref$cID))) +
  labs(NULL, caption = paste("n =", nrow(canned.main.eigen$indpref), ",", "Mean CR =",
                             round(mean(canned.main.eigen$indpref$CR),3)))+
  theme_minimal()

quartz(8,8)
canned.main.eigen.trunc$indpref %>% 
  mutate(rowid = 1:nrow(canned.main.eigen.trunc$indpref)) %>%
  gather(Bio, Econ, Heal, Wat, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref, fill = cID)) + 
  geom_violin(alpha = 0.4, width = 0.8, color = "transparent", aes(fill = cID)) +
  geom_jitter(alpha = 0.5, height = 0, width = 0.1, aes(color = cID)) +
  geom_boxplot(alpha = 0.4, width = 0.3) +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(labels = levels(as.factor(canned.main.eigen.trunc$indpref$cID))) +
  labs(NULL, caption = paste("n =", nrow(canned.main.eigen.trunc$indpref), ",", "Mean CR =",
                             round(mean(canned.main.eigen.trunc$indpref$CR),3)))+
  theme_minimal()

#Only Rural landowners
canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="RuralProp",] %>% 
  mutate(rowid = 1:nrow(canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="RuralProp",])) %>%
  gather(Bio, Econ, Heal, Wat, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  labs(NULL, caption = paste("n =", nrow(canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="RuralProp",]), ",", "Mean CR =",
                             round(mean(canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="RuralProp",]$CR),3)))+
  theme_minimal()

#Only specialist
canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="Specialist",] %>% 
  mutate(rowid = 1:nrow(canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="Specialist",])) %>%
  gather(Bio, Econ, Heal, Wat, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  labs(NULL, caption = paste("n =", nrow(canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="Specialist",]), ",", "Mean CR =",
                             round(mean(canned.main.eigen.trunc$indpref[canned.main.eigen.trunc$indpref$cID=="Specialist",]$CR),3)))+
  theme_minimal()

#Fire objectives
#Eigenvalue
fire.atts <- c("Riv", "Dis","Clim", "Cost", "Graz", "Pest", "Trad", "Cons")

canned.fire.eigen.trunc <- ahp(df = ahp.data[,c(7:35)], 
                         atts = fire.atts, 
                         negconvert = TRUE, 
                         reciprocal = TRUE,
                         method = 'eigen', 
                         aggmethod = "eigen", 
                         censorcr = 0.11,
                         agg = TRUE,
                         ID = c("Class"))

canned.fire.eigen <- ahp(df = ahp.data[,c(7:35)], 
                         atts = fire.atts, 
                         negconvert = TRUE, 
                         reciprocal = TRUE,
                         method = 'eigen', 
                         aggmethod = "eigen",
                         agg = TRUE,
                         ID = c("Class"))
canned.fire.eigen$aggpref
canned.fire.eigen.trunc$aggpref
canned.fire.eigen.trunc$indpref


canned.fire.eigen.trunc$indpref %>%
  group_by(cID) %>%
  dplyr::summarize(MeanRiv = mean(Riv, na.rm=TRUE),
                   SDRiv = sd(Riv, na.rm=TRUE),
                   MeanDis = mean(Dis, na.rm=TRUE),
                   SDDis = sd(Dis, na.rm=TRUE),
                   MeanClim = mean(Clim, na.rm=TRUE),
                   SDClim = sd(Clim, na.rm=TRUE),
                   MeanCost = mean(Cost, na.rm=TRUE),
                   SDCost = sd(Cost, na.rm=TRUE),
                   MeanGraz = mean(Graz, na.rm=TRUE),
                   SDGraz = sd(Graz, na.rm=TRUE),
                   MeanPest = mean(Pest, na.rm=TRUE),
                   SDPest = sd(Pest, na.rm=TRUE),
                   MeanTrad = mean(Trad, na.rm=TRUE),
                   SDTrad = sd(Trad, na.rm=TRUE),
                   MeanCons = mean(Cons, na.rm=TRUE),
                   SDCons = sd(Cons, na.rm=TRUE))

canned.fire.eigen$indpref %>%
  group_by(cID) %>%
  dplyr::summarize(MeanRiv = mean(Riv, na.rm=TRUE),
                   SDRiv = sd(Riv, na.rm=TRUE),
                   MeanDis = mean(Dis, na.rm=TRUE),
                   SDDis = sd(Dis, na.rm=TRUE),
                   MeanClim = mean(Clim, na.rm=TRUE),
                   SDClim = sd(Clim, na.rm=TRUE),
                   MeanCost = mean(Cost, na.rm=TRUE),
                   SDCost = sd(Cost, na.rm=TRUE),
                   MeanGraz = mean(Graz, na.rm=TRUE),
                   SDGraz = sd(Graz, na.rm=TRUE),
                   MeanPest = mean(Pest, na.rm=TRUE),
                   SDPest = sd(Pest, na.rm=TRUE),
                   MeanTrad = mean(Trad, na.rm=TRUE),
                   SDTrad = sd(Trad, na.rm=TRUE),
                   MeanCons = mean(Cons, na.rm=TRUE),
                   SDCons = sd(Cons, na.rm=TRUE))


#Boxplots
quartz(8,8)
canned.fire.eigen$indpref %>% 
  mutate(rowid = 1:nrow(canned.fire.eigen$indpref)) %>%
  gather(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref, fill = cID)) + 
  geom_violin(alpha = 0.4, width = 0.8, color = "transparent", aes(fill = cID)) +
  geom_jitter(alpha = 0.5, height = 0, width = 0.1, aes(color = cID)) +
  geom_boxplot(alpha = 0.4, width = 0.3) +
  scale_x_discrete("Attribute", label = fire.atts) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(labels = levels(as.factor(canned.fire.eigen$indpref$cID))) +
  labs(NULL, caption = paste("n =", nrow(canned.fire.eigen$indpref), ",", "Mean CR =",
                             round(mean(canned.fire.eigen$indpref$CR),3)))+
  theme_minimal()

quartz(8,8)
canned.fire.eigen.trunc$indpref %>% 
  mutate(rowid = 1:nrow(canned.fire.eigen.trunc$indpref)) %>%
  gather(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref, fill = cID)) + 
  geom_violin(alpha = 0.4, width = 0.8, color = "transparent", aes(fill = cID)) +
  geom_jitter(alpha = 0.5, height = 0, width = 0.1, aes(color = cID)) +
  geom_boxplot(alpha = 0.4, width = 0.3) +
  scale_x_discrete("Attribute", label = fire.atts) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(labels = levels(as.factor(canned.fire.eigen.trunc$indpref$cID))) +
  labs(NULL, caption = paste("n =", nrow(canned.fire.eigen.trunc$indpref), ",", "Mean CR =",
                             round(mean(canned.fire.eigen.trunc$indpref$CR),3)))+
  theme_minimal()

#Only Rural landowners
canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="RuralProp",] %>% 
  mutate(rowid = 1:nrow(canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="RuralProp",])) %>%
  gather(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = fire.atts) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  labs(NULL, caption = paste("n =", nrow(canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="RuralProp",]), ",", "Mean CR =",
                             round(mean(canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="RuralProp",]$CR),3)))+
  theme_minimal()

#Only specialist
canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="Specialist",] %>% 
  mutate(rowid = 1:nrow(canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="Specialist",])) %>%
  gather(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = fire.atts) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,1,0.1))) +
  guides(color=guide_legend(title=NULL))+
  labs(NULL, caption = paste("n =", nrow(canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="Specialist",]), ",", "Mean CR =",
                             round(mean(canned.fire.eigen.trunc$indpref[canned.fire.eigen.trunc$indpref$cID=="Specialist",]$CR),3)))+
  theme_minimal()


# Factorial analysis ------------------------------------------------------
library(psych)
library(psychTools)
library(ggfortify)
library(grid)
library(gridExtra)
library(cowplot)

#Rural Props##############

#Binomial variables: Sust_Develop, Biodiv,Clim_Change
data.rural$Sust_Develop <- factor(data.rural$Sust_Develop, levels = c("Nao", "Sim"))
data.rural$Biodiv <- factor(data.rural$Biodiv, levels = c("Nao", "Sim"))

#3-scale Likert:  Clim_Life, Fire_Use,  Avoid_Late_Fire, App_Early_Fire
data.rural$Clim_Life <- factor(data.rural$Clim_Life, levels = c("Nao,_nem_um_pouco", "Sim,_um_pouco", "Sim,_muito"))
data.rural$Fire_Use <- factor(data.rural$Fire_Use, levels = c("Nao,_nunca", "Às_vezes,_casualmente", "Sim,_todo_ano"))
data.rural$Clim_Change <- factor(data.rural$Clim_Change, levels = c("Nao", "Talvez", "Sim"))
data.rural$Avoid_Late_Fire <- factor(data.rural$Avoid_Late_Fire, levels = c("Nao", "Talvez", "Sim"))
data.rural$App_Early_Fire <- factor(data.rural$App_Early_Fire, levels = c("Nao", "Talvez", "Sim"))

#4-scale Likert: Fire_Out
data.rural$Fire_Out <- factor(data.rural$Fire_Out, levels = c("Nao,_nunca", "Sim,_raramente", "Sim,_às_vezes", "Sim,_todo_ano"))

#5-scale Likert : Sust_Imp, Biodiv_Imp, Fire_Imp
data.rural$Sust_Imp <- factor(data.rural$Sust_Imp, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

data.rural$Biodiv_Imp <- factor(data.rural$Biodiv_Imp, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

data.rural$Fire_Imp <- factor(data.rural$Fire_Imp, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

#Ordinal: Fire_Apps, Fire_Month, Fire_Freq, Fire_Freq_RL, Fire_Freq_RL_Acc, Fire_Out, Fire_Month_Out
data.rural$Fire_Freq <- factor(data.rural$Fire_Freq, levels = c("Nunca", "De_três_em_três_anos", "Um_ano_sim,_outro_ano_nao_(de_dois_em_dois_anos)", "Todo_ano"))

data.rural$Fire_Freq_RL <- factor(data.rural$Fire_Freq_RL, levels = c("Nunca", "Todo_ano"))
data.rural$Fire_Freq_RL_Acc <- factor(data.rural$Fire_Freq_RL_Acc, levels = c("Nao,_nunca", "Nao,_mas_raramente_isso_acontece", "Nao,_mas_às_vezes_isso_acontece"))

data.rural$Fire_Out <- factor(data.rural$Fire_Out, levels = c("Nao,_nunca", "Sim,_raramente", "Sim,_às_vezes", "Sim,_todo_ano"))

#Possible predictors: Gender, Age, Ethnicity, Income, SpecsxRuralProps, Scolarity, Prop_Area_ha, Time_Rural
data.rural$Scolarity <- factor(data.rural$Scolarity, levels = c("Nao_alfabetizado", "Alfabetizaçao", "Fundamental", "Medio", "Superior", "Mestrado", "Doutorado"))

data.rural$Prop_Contrib_Income <- factor(data.rural$Prop_Contrib_Income, levels = c("Insignificante(0-9%)", "Poucodependente(10-39%)", "Intermediario(40a59%)", "Muitodependente(60%a89%)" , "Totalmentedependente(90a100%)"))

data.rural$Income[data.rural$Income == "Prefironaoresponder"] <- NA
data.rural$Income <- factor(data.rural$Income, levels = c("R$501,00-R$1.000,00", "R$1.001,00-R$2.500,00", "R$2.501,00-R$5.000,00", "R$5.001,00-R$10.000,00", "R$10.001,00-R$20.000,00", "AcimadeR$20.000,00"))

data.rural$Time_Rural <- factor(data.rural$Time_Rural, levels = c("Nao_moro_na_zona_rural", "Menos_de_1_ano", "Entre_1_e_2_anos", "Entre_3_e_5_anos", "Entre_6_e_10_anos", "Mais_de_10_anos"))

data.rural$Prop_Area_ha <- factor(data.rural$Prop_Area_ha, levels = c("0-5ha", "6-10ha", "11-20ha", "21-50ha", "50-100ha", "Maisde100ha"))

summary(data.rural)

data.test.rural <- data.rural[,c(3:9,11:18,20:28,30:34,69:79)]
data.test.rural[,-c(30,32,36,39,40)] <- lapply(data.test.rural[,-c(30,32,36,39,40)], as.integer)
data.test.rural <- cbind(data.test.rural, 
                         canned.main.eigen$indpref[canned.main.eigen$indpref$cID=="RuralProp",c(1:5)],
                         canned.fire.eigen$indpref[canned.main.eigen$indpref$cID=="RuralProp",c(2:9)])

quartz(8,12)
pairs.panels(data.pca.rural[,c(1:7,15,21:24,28,29)])

#dev.off()

quartz(8,8)
cor.plot(data.pca.rural[,c(1:7,15,21:24,28,29)], diag = F, upper = F)


#Specialists##############

#Binomial variables: Sust_Develop, Biodiv

data.spec$Sust_Develop <- factor(data.spec$Sust_Develop, levels = c("Nao", "Sim"))
data.spec$Biodiv <- factor(data.spec$Biodiv, levels = c("Nao", "Sim"))

#3-scale Likert:  Clim_Change, Clim_Life, Fire_Use, Fire_Out, Avoid_Late_Fire, App_Early_Fire
data.spec$Clim_Change <- factor(data.spec$Clim_Change, levels = c("Nao", "Talvez","Sim"))
data.spec$Clim_Life <- factor(data.spec$Clim_Life, levels = c("Nao,_nem_um_pouco", "Sim,_um_pouco", "Sim,_muito"))

#5-scale Likert : Sust_Imp, Biodiv_Imp, Fire_Imp, Fire_Imp_Bio, Actual_Fire_Effects_Eco, Late_Fire_Effects_Eco, Early_Fire_Effects_Eco, MIF_Approv_UCTI, MIF_Approv_PrivProp, MIF_Eval
data.spec$Sust_Imp <- factor(data.spec$Sust_Imp, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

data.spec$Biodiv_Imp <- factor(data.spec$Biodiv_Imp, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

data.spec$Fire_Imp <- factor(data.spec$Fire_Imp, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

data.spec$Fire_Imp_Bio <- factor(data.spec$Fire_Imp_Bio, levels = c("Nada_importante", "Pouco_importante", "Neutro_(indiferente)", "Importante", "Muito_importante"))

summary(data.spec$MIF_Eval)

data.spec$MIF_Eval[data.spec$MIF_Eval == "Nao_conheço_o_MIF"] <- NA
data.spec$MIF_Eval <- factor(data.spec$MIF_Eval, levels = c("Pessimo", "Ruim", "Regular", "Bom", "Excelente"))

data.spec$MIF_Approv_UCTI <- factor(data.spec$MIF_Approv_UCTI, levels = c("Muito_desfavoravel", "Um_pouco_desfavoravel", "Neutro", "Um_pouco_favoravel", "Muito_favoravel"))

data.spec$MIF_Approv_PrivProp <- factor(data.spec$MIF_Approv_PrivProp, levels = c("Muito_desfavoravel", "Um_pouco_desfavoravel", "Neutro", "Um_pouco_favoravel", "Muito_favoravel"))

#7-scale Likert
data.spec$Actual_Fire_Effects_Eco <- factor(data.spec$Actual_Fire_Effects_Eco, levels = c("Definitivamente_prejudiciais", "Muito_prejudiciais", "Um_pouco_prejudiciais", "Neutro", "Um_pouco_beneficos", "Muito_beneficos", "Definitivamente_beneficos"))

data.spec$Late_Fire_Effects_Eco <- factor(data.spec$Late_Fire_Effects_Eco, levels = c("Definitivamente_prejudiciais", "Muito_prejudiciais", "Um_pouco_prejudiciais", "Neutro", "Um_pouco_beneficos", "Muito_beneficos", "Definitivamente_beneficos"))

data.spec$Early_Fire_Effects_Eco <- factor(data.spec$Early_Fire_Effects_Eco, levels = c("Definitivamente_prejudiciais", "Muito_prejudiciais", "Um_pouco_prejudiciais", "Neutro", "Um_pouco_beneficos", "Muito_beneficos", "Definitivamente_beneficos"))

#Possible predictors
data.spec$Scolarity[data.spec$Scolarity == "Alfabetizaçao_completa"] <- "Ensino_Superior_completo"
data.spec$Scolarity <- factor(data.spec$Scolarity, levels = c("Nao_alfabetizado", "Alfabetizaçao", "Fundamental", "Medio", "Ensino_Superior_completo", "Mestrado_completo", "Doutorado_completo"))


data.spec$Income[data.spec$Income == "Prefironaoresponder"] <- NA
data.spec$Income <- factor(data.spec$Income, levels = c("R$501,00-R$1.000,00", "R$1.001,00-R$2.500,00", "R$2.501,00-R$5.000,00", "R$5.001,00-R$10.000,00", "R$10.001,00-R$20.000,00", "AcimadeR$20.000,00"))

data.spec$Time_Occupation <- factor(data.spec$Time_Occupation, levels = c("Menos_de_1_ano", "Entre_1_e_2_anos", "Entre_3_e_5_anos", "Entre_6_e_10_anos", "Mais_de_10_anos"))


data.test.spec <- data.spec[,-c(1:2,17:50)]
data.test.spec[,-c(14,15,18,19,21,23,24)] <- lapply(data.test.spec[,-c(14,15,18,19,21,23,24)], as.integer)
data.test.spec <- cbind(data.test.spec, 
                         canned.main.eigen$indpref[canned.main.eigen$indpref$cID=="Specialist",c(1:5)],
                         canned.fire.eigen$indpref[canned.main.eigen$indpref$cID=="Specialist",c(2:9)])


quartz(8,12)
pairs.panels(data.pca.spec[,-c(1,2,5)])

quartz(8,8)
cor.plot(data.pca.spec[,-c(1,2,5)], diag = F, upper = F)


#Both rural landowners and specialists
data.rural.spec.test <- rbind(data.test.rural[,c(1:6,15,30:33,35,41:53)],data.test.spec[,c(1:7,20:22,18,23,26:38)])

quartz(8,12)
pairs.panels(data.rural.spec.test)

quartz(8,8)
pairs.panels(data.rural.spec.test[,c(1:7)])

#Number of factors
quartz(8,8)
fa.parallel(data.rural.spec.test[,c(1:6)])

quartz(8,8)
vss(data.rural.spec.test[,c(1:6)])

#PCA

plot(prcomp(data.rural.spec.test[,c(1:6)], scale. = T, center = T))

quartz(8,8)
autoplot(prcomp(data.rural.spec.test[,c(1:6)], scale. = T, center = T),alpha=.4,
         data = data.rural.spec.test,loadings = TRUE, loadings.colour = 'blue', colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3)

quartz(8,8)
autoplot(prcomp(data.rural.spec.test[,c(1:6)], scale. = T, center = T),alpha=.4,
         data = data.rural.spec.test,loadings = TRUE, loadings.colour = 'blue', colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3, x=3, y=4)

#Factor analysis
fa(data.rural.spec.test[,c(1:6)],nfactors = 2)
(fa.test <- fa(data.rural.spec.test[,c(1:6)],nfactors = 2, rotate = "promax"))

quartz(8,8)
plot(fa.test)

quartz(8,8)
fa.diagram(fa.test)

#Cluster analysis
quartz(8,8)
clust.rural.spec <- iclust(data.rural.spec.test[,c(1:6)])

#Exploratory factor analysis
quartz(8,8)
omega(data.rural.spec.test[,c(1:6)])

omega(data.rural.spec.test[,c(1:6)], rotate = "Promax")
omega(data.rural.spec.test[,c(1:6)], rotate = "Promax", sl = F)

quartz(8,8)
omega(data.rural.spec.test[,c(1:6)], nfactors = 2)

omega(data.rural.spec.test[,c(1:6)], nfactors = 2, rotate = "Promax")
omega(data.rural.spec.test[,c(1:6)], nfactors = 2, rotate = "Promax", sl = F)

quartz(8,8)
omegaSem(data.rural.spec.test[,c(1:6)])

quartz(8,8)
omegaSem(data.rural.spec.test[,c(1:6)], nfactors = 2)

data.rural.spec.test <- cbind(data.rural.spec.test,fa.test$scores)

saveRDS(data.rural.spec.test,"data_rural_spec.rds")
saveRDS(data.test.rural,"data_rural.rds")
saveRDS(data.test.spec,"data_spec.rds")

# Multilevel Models ------------------------------------------------------------------
rm(list=ls())
detach("package:psych", unload = TRUE)
detach("package:psychTools", unload = TRUE)
library(brms)
library(sjPlot)
library(viridis)

data.rural.spec.test <- readRDS("data_rural_spec.rds")
data.test.rural <- readRDS("data_rural.rds")
data.test.spec <- readRDS("data_spec.rds")

data.rural.spec.test$person <- 1:nrow(data.rural.spec.test)
data.test.rural$person <- 1:nrow(data.test.rural)
data.test.spec$person <- 1:nrow(data.test.spec)

data.test.rural$MR1 <- data.rural.spec.test$MR1[data.rural.spec.test$cID=="RuralProp"]
data.test.rural$MR2 <- data.rural.spec.test$MR2[data.rural.spec.test$cID=="RuralProp"]

data.test.spec$MR1 <- data.rural.spec.test$MR1[data.rural.spec.test$cID=="Specialist"]
data.test.spec$MR2 <- data.rural.spec.test$MR2[data.rural.spec.test$cID=="Specialist"]

summary(data.rural.spec.test)
summary(data.test.rural)
summary(data.test.spec)

load("brms_models.RData")

#Cumulative models
#Dirichlet for multivariate models - AHP

#Possible predictors: Gender, Age, Income, SpecsxRuralProps, Time_Occupation (specialists), Scolarity

# Rural Props and Specialists (both) --------------------------------------

#Judgement

#AHP_main - Dirichlet for multivariate models#
##############################################

cbind(data.rural.spec.test[,c(14:17)])

#Only specialists x rural props
fit.mainAHP <- brm(cbind(Bio, Econ, Heal, Wat) ~ (1|person) + cID, 
                   data.rural.spec.test, brmsfamily("dirichlet"),
                   cores = 4, iter = 4000)

plot(fit.mainAHP)
#pairs(fit.mainAHP)
summary(fit.mainAHP)
#bayes_R2(fit.mainAHP)
conditional_effects(fit.mainAHP, categorical = T)

#Only social
fit.mainAHP.social <- brm(cbind(Bio, Econ, Heal, Wat) ~ (1|person) +
                            Gender + mo(Age) +mo(Income) + mo(Scolarity),
                   data.rural.spec.test, brmsfamily("dirichlet"),
                   cores = 4, iter = 4000)

plot(fit.mainAHP.social)
summary(fit.mainAHP.social)
#bayes_R2(fit.mainAHP.social)
quartz(height = 8, width = 10)
conditional_effects(fit.mainAHP.social, categorical = T, effects = "Gender")
quartz(height = 8, width = 10)
conditional_effects(fit.mainAHP.social, categorical = T, effects = "Age")
quartz(height = 8, width = 10)
conditional_effects(fit.mainAHP.social, categorical = T, effects = "Income")
quartz(height = 8, width = 10)
conditional_effects(fit.mainAHP.social, categorical = T, effects = "Scolarity")

#Only constructs
fit.mainAHP.constr <- brm(cbind(Bio, Econ, Heal, Wat) ~ (1|person) +
                            MR1 + MR2,
                          data.rural.spec.test, brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

plot(fit.mainAHP.constr)
summary(fit.mainAHP.constr)
#bayes_R2(fit.mainAHP.constr)
quartz(height = 8, width = 10)
conditional_effects(fit.mainAHP.constr, categorical = T, effects = "MR1")
quartz(height = 8, width = 10)
conditional_effects(fit.mainAHP.constr, categorical = T, effects = "MR2")


#Full model
fit.mainAHP.full <- brm(cbind(Bio, Econ, Heal, Wat) ~ cID + (1|person) +
                          Gender + mo(Age) +mo(Income) + mo(Scolarity) +
                          MR1 + MR2,
                        data.rural.spec.test, brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)

plot(fit.mainAHP.full)
summary(fit.mainAHP.full)
#bayes_R2(fit.mainAHP.full)
conditional_effects(fit.mainAHP.full, categorical = T, effects = "cID")
conditional_effects(fit.mainAHP.full, categorical = T, effects = "Gender")
conditional_effects(fit.mainAHP.full, categorical = T, effects = "Age")
conditional_effects(fit.mainAHP.full, categorical = T, effects = "Income")
conditional_effects(fit.mainAHP.full, categorical = T, effects = "Scolarity")
conditional_effects(fit.mainAHP.full, categorical = T, effects = "MR1")
conditional_effects(fit.mainAHP.full, categorical = T, effects = "MR2")

#Refit models removing NAs in the data to compare them
#Only specialists x rural props
fit.mainAHP <- brm(cbind(Bio, Econ, Heal, Wat) ~ (1|person) + cID, 
                   na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                   cores = 4, iter = 4000)

#Only social
fit.mainAHP.social <- brm(cbind(Bio, Econ, Heal, Wat) ~ (1|person) +
                            Gender + mo(Age) +mo(Income) + mo(Scolarity),
                          na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

#Only constructs
fit.mainAHP.constr <- brm(cbind(Bio, Econ, Heal, Wat) ~ (1|person) +
                            MR1 + MR2,
                          na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

#Full model
fit.mainAHP.full <- brm(cbind(Bio, Econ, Heal, Wat) ~ cID + (1|person) +
                          Gender + mo(Age) +mo(Income) + mo(Scolarity) +
                          MR1 + MR2,
                        na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)

#Null model
fit.mainAHP.null <- brm(cbind(Bio, Econ, Heal, Wat) ~ 1 + (1|person),
                        na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)


loo(fit.mainAHP.social, fit.mainAHP.full,fit.mainAHP,fit.mainAHP.constr, fit.mainAHP.null)
#Socioeconomical differences are more important to explain differences in general AHP

#AHP_fire - Dirichlet for multivariate models#
##############################################

#Only specialists x rural props
fit.fireAHP <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ (1|person) + cID, 
                   data.rural.spec.test, brmsfamily("dirichlet"),
                   cores = 4, iter = 4000)

plot(fit.fireAHP)
summary(fit.fireAHP)
#bayes_R2(fit.fireAHP)
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP, categorical = T)


#Only social
fit.fireAHP.social <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ (1|person) +
                            Gender + mo(Age) +mo(Income) + mo(Scolarity),
                          data.rural.spec.test, brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

plot(fit.fireAHP.social)
summary(fit.fireAHP.social)
#bayes_R2(fit.fireAHP.social)
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.social, categorical = T, effects = "Gender")
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.social, categorical = T, effects = "Age")
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.social, categorical = T, effects = "Income")
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.social, categorical = T, effects = "Scolarity")

#Only constructs
fit.fireAHP.constr <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                            (1|person) + MR1 + MR2,
                          data.rural.spec.test, brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

plot(fit.fireAHP.constr)
summary(fit.fireAHP.constr)
#bayes_R2(fit.fireAHP.constr)
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.constr, categorical = T, effects = "MR1")
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.constr, categorical = T, effects = "MR2")


#Full model
fit.fireAHP.full <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                          (1|person) + cID + 
                          Gender + mo(Age) +mo(Income) + mo(Scolarity) +
                          MR1 + MR2, 
                   data.rural.spec.test, brmsfamily("dirichlet"),
                   cores = 4, iter = 4000)

plot(fit.fireAHP.full)
summary(fit.fireAHP.full)
#bayes_R2(fit.fireAHP.full)
conditional_effects(fit.fireAHP.full, categorical = T, effects = "cID")
conditional_effects(fit.fireAHP.full, categorical = T, effects = "Gender")
conditional_effects(fit.fireAHP.full, categorical = T, effects = "Age")
conditional_effects(fit.fireAHP.full, categorical = T, effects = "Income")
conditional_effects(fit.fireAHP.full, categorical = T, effects = "Scolarity")
conditional_effects(fit.fireAHP.full, categorical = T, effects = "MR1")
conditional_effects(fit.fireAHP.full, categorical = T, effects = "MR2")

#Refit models removing NAs in the data.frame to compare them
#Only specialists x rural props
fit.fireAHP <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ (1|person) + cID, 
                   na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                   cores = 4, iter = 4000)
#Only social
fit.fireAHP.social <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ (1|person) +
                            Gender + mo(Age) +mo(Income) + mo(Scolarity),
                          na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

#Only constructs
fit.fireAHP.constr <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                            (1|person) + MR1 + MR2,
                          na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

#Full model
fit.fireAHP.full <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                          (1|person) + cID + 
                          Gender + mo(Age) +mo(Income) + mo(Scolarity) +
                          MR1 + MR2, 
                        na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)

#null model
fit.fireAHP.null <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                          1 + (1|person), 
                        na.omit(data.rural.spec.test), brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)

loo(fit.fireAHP.null, fit.fireAHP, fit.fireAHP.constr, fit.fireAHP.social, fit.fireAHP.full)
#Socioeconomical differences are more important to explain differences in fire AHP

#Attitudes
#Fire_Imp - 5-scale Likert - Cumulative models#
###############################################

#Only specialists x rural props
fit.FireImp <- brm(Fire_Imp ~ (1|person) + cID, 
                   data.rural.spec.test, brmsfamily("cumulative"),
                   cores = 4, iter = 20000, warmup = 10000, control = list(adapt_delta = 0.9))

plot(fit.FireImp)
summary(fit.FireImp)
bayes_R2(fit.FireImp)

conditional_effects(fit.FireImp, method = "posterior_epred")
plot(ce.fireImp <- conditional_effects(fit.FireImp, categorical = T))


#Only social
fit.FireImp.social <- brm(Fire_Imp ~ (1|person) +
                          Gender + mo(Age) +mo(Income) + mo(Scolarity), 
                        data.rural.spec.test, brmsfamily("cumulative"),
                        cores = 4, iter = 4000)

plot(fit.FireImp.social)
summary(fit.FireImp.social)
bayes_R2(fit.FireImp.social)
quartz(height = 8, width = 10)
plot(ce.fireImp.gender <- conditional_effects(fit.FireImp.social, categorical = T, effects = "Gender"))
ggplot(ce.fireImp.gender$`Gender:cats__`, aes(y = estimate__, x = as.integer(effect2__), colour = Gender))+
  geom_path()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__,fill = Gender), alpha=0.5, colour = NA) +
  ylab("Probability") + xlab("Fire importance")

quartz(height = 8, width = 8)
conditional_effects(fit.FireImp.social,  effects = "Age")
quartz(height = 8, width = 8)
conditional_effects(fit.FireImp.social,  effects = "Income")
quartz(height = 8, width = 8)
conditional_effects(fit.FireImp.social,  effects = "Scolarity")

#Only constructs
fit.FireImp.constr <- brm(Fire_Imp ~ (1|person) + MR1 + MR2, 
                        data.rural.spec.test, brmsfamily("cumulative"),
                        cores = 4, iter = 20000, warmup = 10000)

plot(fit.FireImp.constr)
summary(fit.FireImp.constr)
bayes_R2(fit.FireImp.constr)
quartz(height = 8, width = 8)
conditional_effects(fit.FireImp.constr, effects = "MR1")
quartz(height = 8, width = 8)
conditional_effects(fit.FireImp.constr, effects = "MR2")

#Full model
fit.FireImp.full <- brm(Fire_Imp ~ (1|person) + cID + 
                          Gender + mo(Age) +mo(Income) + mo(Scolarity) +
                          MR1 + MR2, 
                        data.rural.spec.test, brmsfamily("cumulative"),
                        cores = 4, iter = 4000)

plot(fit.FireImp.full)
summary(fit.FireImp.full)
bayes_R2(fit.FireImp.full)
conditional_effects(fit.FireImp.full, categorical = T, effects = "cID")
conditional_effects(fit.FireImp.full, categorical = T, effects = "Gender")
conditional_effects(fit.FireImp.full, categorical = T, effects = "Age")
conditional_effects(fit.FireImp.full, categorical = T, effects = "Income")
conditional_effects(fit.FireImp.full, categorical = T, effects = "Scolarity")
conditional_effects(fit.FireImp.full, categorical = T, effects = "MR1")
conditional_effects(fit.FireImp.full, categorical = T, effects = "MR2")

#Refit models removing NAs in the data.frame to compare them
#Only specialists x rural props
fit.FireImp <- brm(Fire_Imp ~ (1|person) + cID, 
                   na.omit(data.rural.spec.test), brmsfamily("cumulative"),
                   cores = 4, iter = 20000, warmup = 10000, control = list(adapt_delta = 0.9))

#Only social
fit.FireImp.social <- brm(Fire_Imp ~ (1|person) +
                            Gender + mo(Age) +mo(Income) + mo(Scolarity), 
                          na.omit(data.rural.spec.test), brmsfamily("cumulative"),
                          cores = 4, iter = 4000)

#Only constructs
fit.FireImp.constr <- brm(Fire_Imp ~ (1|person) + MR1 + MR2, 
                          na.omit(data.rural.spec.test), brmsfamily("cumulative"),
                          cores = 4, iter = 20000, warmup = 10000)

#Full model
fit.FireImp.full <- brm(Fire_Imp ~ (1|person) + cID + 
                          Gender + mo(Age) +mo(Income) + mo(Scolarity) +
                          MR1 + MR2, 
                        na.omit(data.rural.spec.test), brmsfamily("cumulative"),
                        cores = 4, iter = 4000)

#Null model
fit.FireImp.null <- brm(Fire_Imp ~ 1 + (1|person), 
                        na.omit(data.rural.spec.test), brmsfamily("cumulative"),
                        cores = 4, iter = 4000)

loo(fit.FireImp.null, fit.FireImp.social, fit.FireImp.full,fit.FireImp,fit.FireImp.constr)


# Rural Props -------------------------------------------------------------

#Judgment

#AHP fire#
##########
#Fire Freq
fit.fireAHP.FireFreq <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                          (1|person) + mo(Fire_Freq), 
                        data.test.rural, brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)

plot(fit.fireAHP.FireFreq)
summary(fit.fireAHP.FireFreq)
#bayes_R2(fit.fireAHP.full)
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.FireFreq, categorical = T, effects = "Fire_Freq")

#Early fires
fit.fireAHP.EarlyFire <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                              (1|person) + mo(App_Early_Fire), 
                            data.test.rural, brmsfamily("dirichlet"),
                            cores = 4, iter = 4000)

plot(fit.fireAHP.EarlyFire)
summary(fit.fireAHP.EarlyFire)
#bayes_R2(fit.fireAHP.full)
quartz(height = 8, width = 12)
conditional_effects(fit.fireAHP.EarlyFire, categorical = T, effects = "App_Early_Fire")

#Late fires
fit.fireAHP.LateFire <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                               (1|person) + mo(Avoid_Late_Fire), 
                             data.test.rural, brmsfamily("dirichlet"),
                             cores = 4, iter = 4000)

plot(fit.fireAHP.LateFire)
summary(fit.fireAHP.LateFire)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.LateFire, categorical = T, effects = "Avoid_Late_Fire")

##Wildfires perception
fit.fireAHP.WildFire <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                              (1|person) + mo(Fire_Out), 
                            data.test.rural, brmsfamily("dirichlet"),
                            cores = 4, iter = 4000)

plot(fit.fireAHP.WildFire)
summary(fit.fireAHP.WildFire)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.WildFire, categorical = T, effects = "Fire_Out")

#Full social model
fit.fireAHP.rural.social <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                          (1|person) + 
                          mo(Age) +mo(Income) + mo(Scolarity) + 
                          mo(Time_Rural) + mo(Prop_Area_ha) + mo(Prop_Contrib_Income), 
                        data.test.rural, brmsfamily("dirichlet"),
                        cores = 4, iter = 4000)

plot(fit.fireAHP.rural.social)
summary(fit.fireAHP.rural.social)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.rural.social, categorical = T, effects = "Prop_Contrib_Income")
quartz(8,12)
conditional_effects(fit.fireAHP.rural.social, categorical = T, effects = "Age")
quartz(8,12)
conditional_effects(fit.fireAHP.rural.social, categorical = T, effects = "Income")
quartz(8,12)
conditional_effects(fit.fireAHP.rural.social, categorical = T, effects = "Scolarity")
quartz(8,12)
conditional_effects(fit.fireAHP.rural.social, categorical = T, effects = "Time_Rural")
quartz(8,12)
conditional_effects(fit.fireAHP.rural.social, categorical = T, effects = "Prop_Area_ha")

#Attitudes/Behavior

#Fire_use - multinomial bernoulli#
##################################
colSums(data.test.rural[,c("Fire_Never", 
                           "Fire_Clean", 
                           "Fire_Pest", 
                           "Fire_Agr", 
                           "Trash", 
                           "Fire_break", 
                           "Fire_ProtectForest")])


#Fire_freq - ordinal - cumulative models
table(data.test.rural$Fire_Freq)

#Null model
fit.FireFreq.null <- brm(Fire_Freq ~ 1 + (1|person), 
                           data.test.rural, brmsfamily("cumulative"),
                           cores = 4, iter = 4000)

plot(fit.FireFreq.null)
summary(fit.FireFreq.null)
bayes_R2(fit.FireFreq.null)

quartz(8,8)
plot_model(fit.FireFreq.null)

#App_Early_Fire - 3-scale Likert - cumulative models
table(data.test.rural$App_Early_Fire)

#Null model
fit.EarlyFire.null <- brm(App_Early_Fire ~ 1 + (1|person), 
                         data.test.rural, brmsfamily("cumulative"),
                         cores = 4, iter = 4000)

plot(fit.EarlyFire.null)
summary(fit.EarlyFire.null)
bayes_R2(fit.EarlyFire.null)
quartz(8,8)
plot_model(fit.EarlyFire.null)

#Avoid_Late_Fire - 3-scale Likert - cumulative models
table(data.test.rural$Avoid_Late_Fire)

#Null model
fit.LateFire.null <- brm(Avoid_Late_Fire ~ 1 + (1|person), 
                          data.test.rural, brmsfamily("cumulative"),
                          cores = 4, iter = 4000)

plot(fit.LateFire.null)
summary(fit.LateFire.null)
bayes_R2(fit.LateFire.null)
quartz(8,8)
plot_model(fit.LateFire.null)


#Fire_month - multinomial bernoulli
#Just plots
colSums(data.test.rural[,c("Fire_Rain",
                           "Fire_Early",
                           "Fire_Mid",
                           "Fire_Late",
                           "Fire_Oct")])
# Specialists -------------------------------------------------------------

#Judgement
#AHP Fire#
##########

#Fire Importance for biodiversity#
##################################
prop.table(table(data.test.spec$Fire_Imp_Bio))
fit.fireAHP.ImpBio <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                              (1|person) + mo(Fire_Imp_Bio), 
                            data.test.spec, brmsfamily("dirichlet"),
                            cores = 4, iter = 4000)

plot(fit.fireAHP.ImpBio)
summary(fit.fireAHP.ImpBio)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.ImpBio, categorical = T, effects = "Fire_Imp_Bio")

#Current fire regimes effects#
##############################
prop.table(table(data.test.spec$Actual_Fire_Effects_Eco))
barplot(prop.table(table(data.test.spec$Actual_Fire_Effects_Eco)))

fit.fireAHP.FireToday <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                            (1|person) + mo(Actual_Fire_Effects_Eco), 
                          data.test.spec, brmsfamily("dirichlet"),
                          cores = 4, iter = 4000)

plot(fit.fireAHP.FireToday)
summary(fit.fireAHP.FireToday)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.FireToday, categorical = T, effects = "Actual_Fire_Effects_Eco")

#Late fire regimes effects#
###########################
prop.table(table(data.test.spec$Late_Fire_Effects_Eco))
barplot(prop.table(table(data.test.spec$Late_Fire_Effects_Eco)))

fit.fireAHP.LateFire.spec <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                               (1|person) + mo(Late_Fire_Effects_Eco), 
                             data.test.spec, brmsfamily("dirichlet"),
                             cores = 4, iter = 4000)

plot(fit.fireAHP.LateFire.spec)
summary(fit.fireAHP.LateFire.spec)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.LateFire.spec, categorical = T, 
                    effects = "Late_Fire_Effects_Eco")

#Early fire regimes effects#
############################
prop.table(table(data.test.spec$Early_Fire_Effects_Eco))
barplot(prop.table(table(data.test.spec$Early_Fire_Effects_Eco)))

fit.fireAHP.EarlyFire.spec <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                                   (1|person) + mo(Early_Fire_Effects_Eco), 
                                 data.test.spec, brmsfamily("dirichlet"),
                                 cores = 4, iter = 4000)

plot(fit.fireAHP.EarlyFire.spec)
summary(fit.fireAHP.EarlyFire.spec)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.EarlyFire.spec, categorical = T, 
                    effects = "Early_Fire_Effects_Eco")

#MIF approval in protected areas#
#################################
table(data.test.spec$MIF_Approv_UCTI)
barplot(prop.table(table(data.test.spec$MIF_Approv_UCTI)))

fit.fireAHP.MIF.UCTI <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                                    (1|person) + mo(MIF_Approv_UCTI), 
                                  data.test.spec, brmsfamily("dirichlet"),
                                  cores = 4, iter = 4000)

plot(fit.fireAHP.MIF.UCTI)
summary(fit.fireAHP.MIF.UCTI)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.MIF.UCTI, categorical = T, 
                    effects = "MIF_Approv_UCTI")

#MIF approval in private areas#
###############################
table(data.test.spec$MIF_Approv_PrivProp)
barplot(prop.table(table(data.test.spec$MIF_Approv_PrivProp)))

fit.fireAHP.MIF.priv <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                              (1|person) + mo(MIF_Approv_PrivProp), 
                            data.test.spec, brmsfamily("dirichlet"),
                            cores = 4, iter = 4000)

plot(fit.fireAHP.MIF.priv)
summary(fit.fireAHP.MIF.priv)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.MIF.priv, categorical = T, 
                    effects = "MIF_Approv_PrivProp", iter = 4000)

#MIF evaluation#
################
table(data.test.spec$MIF_Eval)
barplot(prop.table(table(data.test.spec$MIF_Eval)))

fit.fireAHP.MIF.eval <- brm(cbind(Riv, Dis, Clim, Cost, Graz, Pest, Trad, Cons) ~ 
                              (1|person) + mo(MIF_Eval), 
                            data.test.spec, brmsfamily("dirichlet"),
                            cores = 4, iter = 4000)

plot(fit.fireAHP.MIF.eval)
summary(fit.fireAHP.MIF.eval)
#bayes_R2(fit.fireAHP.full)
quartz(8,12)
conditional_effects(fit.fireAHP.MIF.eval, categorical = T, 
                    effects = "MIF_Eval")


#Fire_Imp_Bio - 5-scale Likert - cumulative models#
###################################################
table(data.test.spec$Fire_Imp_Bio)

#Null model
fit.FireImpBio.null <- brm(Fire_Imp_Bio ~ (1|person), 
                         data.test.spec, brmsfamily("cumulative"),
                         cores = 4, iter = 4000)

plot(fit.FireImpBio.null)
summary(fit.FireImpBio.null)
bayes_R2(fit.FireImpBio.null)
quartz(8,8)
plot_model(fit.FireImpBio.null)

#Actual_Fire_Effects_Eco# - 7-scale Likert - cumulative models
#########################

table(data.test.spec$Actual_Fire_Effects_Eco)

#Null model
fit.FireToday.null <- brm(Actual_Fire_Effects_Eco ~ (1|person), 
                           data.test.spec, brmsfamily("cumulative"),
                           cores = 4, iter = 4000)

plot(fit.FireToday.null)
summary(fit.FireToday.null)
bayes_R2(fit.FireToday.null)
quartz(8,8)
plot_model(fit.FireToday.null)

#Late_Fire_Effects_Eco#- 7-scale Likert - cumulative models
#######################
table(data.test.spec$Late_Fire_Effects_Eco)


#Full model
fit.LateFireEff.null <- brm(Late_Fire_Effects_Eco ~ (1|person), 
                          data.test.spec, brmsfamily("cumulative"),
                          cores = 4, iter = 4000)

plot(fit.LateFireEff.null)
summary(fit.LateFireEff.null)
bayes_R2(fit.LateFireEff.null)
quartz(8,8)
plot_model(fit.LateFireEff.null)

#Early_Fire_Effects_Eco# - 7-scale Likert - cumulative models
########################

table(data.test.spec$Early_Fire_Effects_Eco)

#Null model
fit.EarlyFireEff.null <- brm(Early_Fire_Effects_Eco ~ (1|person), 
                            data.test.spec, brmsfamily("cumulative"),
                            cores = 4, iter = 10000)

plot(fit.EarlyFireEff.null)
summary(fit.EarlyFireEff.null)
bayes_R2(fit.EarlyFireEff.null)
quartz(8,8)
plot_model(fit.EarlyFireEff.null)

#MIF_Approv_UCTI# - 5-scale Likert - cumulative models
#################

table(data.test.spec$MIF_Approv_UCTI)

#Null model
fit.MIFUCTI.null <- brm(MIF_Approv_UCTI ~ (1|person), 
                             data.test.spec, brmsfamily("cumulative"),
                             cores = 4, iter = 10000)

plot(fit.MIFUCTI.null)
summary(fit.MIFUCTI.null)
bayes_R2(fit.MIFUCTI.null)
quartz(8,8)
plot_model(fit.MIFUCTI.null)


#MIF_Approv_PrivProp# - 5-scale Likert - cumulative models
#####################

table(data.test.spec$MIF_Approv_PrivProp)

#Null model
fit.MIFPriv.null <- brm(MIF_Approv_PrivProp ~ (1|person), 
                        data.test.spec, brmsfamily("cumulative"),
                        cores = 4, iter = 10000)

plot(fit.MIFPriv.null)
summary(fit.MIFPriv.null)
bayes_R2(fit.MIFPriv.null)
quartz(8,8)
plot_model(fit.MIFPriv.null)


#MIF_Eval - 5-scale Likert -  cumulative models

table(data.test.spec$MIF_Eval)

#Null model
fit.MIFEval.null <- brm(MIF_Eval ~ (1|person), 
                        data.test.spec, brmsfamily("cumulative"),
                        cores = 4, iter = 10000)

plot(fit.MIFEval.null)
summary(fit.MIFEval.null)
bayes_R2(fit.MIFEval.null)
quartz(8,8)
plot_model(fit.MIFEval.null)
