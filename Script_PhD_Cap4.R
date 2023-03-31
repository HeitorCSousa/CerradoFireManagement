#Read data
data.spec <- read.csv("Respostas_Esps.csv", h=T, stringsAsFactors = T)

data.rural <- read.csv("Respostas_PropsRurais.csv", h=T, stringsAsFactors = T)

head(data.spec)

head(data.rural)

# Analytical Hierarchical Process -----------------------------------------



#Select only variables related to the AHP
ahp.spec <- data.spec[,c(18:51)]
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
speceigentrue <- ahp.indpref(specahp, main.atts, method = "eigen")
specgeom <- ahp.indpref(specahp, main.atts, method = "arithmetic")
specerror <- data.frame(id = 1:length(specahp), maxdiff = apply(abs(speceigentrue - specgeom), 1, max))
specerror %>%
  ggplot(aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum difference") +
  theme_minimal()

ruralahp <- ahp.rural %>% 
  ahp.mat(main.atts, negconvert = T)
ruraleigentrue <- ahp.indpref(ruralahp, main.atts, method = "eigen")
ruralgeom <- ahp.indpref(ruralahp, main.atts, method = "arithmetic")
ruralerror <- data.frame(id = 1:length(ruralahp), maxdiff = apply(abs(ruraleigentrue - ruralgeom), 1, max))
ruralerror %>%
  ggplot(aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum difference") +
  theme_minimal()

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

qtresults <- matrix(nrow = 50, ncol = 4, data = NA)
for (q in 1:50){
  qtresults[q,] <- ahp.aggpref(specahp, main.atts, method = "arithmetic", 
                               aggmethod = "tmean", qt = (q-1)/100)
}
colnames(qtresults) <- main.atts
qtresults %>%
  as.data.frame() %>%
  mutate(trimperc = 1:nrow(qtresults)-1) %>%
  mutate(Bio = Bio - specmean[1],
         Econ = Econ - specmean[2],
         Heal = Heal - specmean[3],
         Wat = Wat - specmean[4]) %>%
  gather(Bio, Econ, Heal, Wat, key = "main.att", value = "weight") %>%
  ggplot(aes(x = trimperc, y = weight, group = main.att, shape = main.att, color = main.att, fill = main.att)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Quantile (from top and bottom) trimmed") +
  scale_y_continuous("Change from untrimmed mean") +
  geom_hline(yintercept = 0, color = "gray") +
  theme_minimal()

qtresults <- matrix(nrow = 50, ncol = 4, data = NA)
for (q in 1:50){
  qtresults[q,] <- ahp.aggpref(ruralahp, main.atts, method = "arithmetic", 
                               aggmethod = "tmean", qt = (q-1)/100)
}
colnames(qtresults) <- main.atts
qtresults %>%
  as.data.frame() %>%
  mutate(trimperc = 1:nrow(qtresults)-1) %>%
  mutate(Bio = Bio - ruralmean[1],
         Econ = Econ - ruralmean[2],
         Heal = Heal - ruralmean[3],
         Wat = Wat - ruralmean[4]) %>%
  gather(Bio, Econ, Heal, Wat, key = "main.att", value = "weight") %>%
  ggplot(aes(x = trimperc, y = weight, group = main.att, shape = main.att, color = main.att, fill = main.att)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Quantile (from top and bottom) trimmed") +
  scale_y_continuous("Change from untrimmed mean") +
  geom_hline(yintercept = 0, color = "gray") +
  theme_minimal()

#Aggregated individual judgements
ahp.aggjudge(specahp,main.atts, aggmethod = "geometric")
ahp.aggjudge(ruralahp,main.atts, aggmethod = "geometric")

#Measuring consistency
spec.cr <- ahp.cr(specahp, main.atts)
table(spec.cr <= 0.1)

(RI <- ahp.ri(nsims = 1000, dim = 4, seed = 30000))

ahp.cr(specahp, main.atts, RI)
table(spec.cr <= RI)

rural.cr <- ahp.cr(ruralahp, main.atts)
table(rural.cr <= 0.1)

ahp.cr(ruralahp, main.atts, RI)
table(rural.cr <= RI)

#Visualising individual priorities and consistency ratios

thres <- 0.1
dict <- c("Bio" = "Biodiversity", 
          "Econ" = "Economy", 
          "Heal" = "Health", 
          "Wat" = "Water")

speccr.df <- 
  spec.cr %>% 
  data.frame() %>%
  mutate(rowid = 1:length(spec.cr), cr.dum = as.factor(ifelse(spec.cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

specahp %>% 
  ahp.indpref(main.atts, method = "eigen") %>% 
  mutate(rowid = 1:nrow(speceigentrue)) %>%
  left_join(speccr.df, by = 'rowid') %>%
  gather(Bio, Econ, Heal, Wat, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(ahp.spec), ",", "Mean CR =",
                             round(mean(spec.cr),3)))+
  theme_minimal()


ruralcr.df <- 
  rural.cr %>% 
  data.frame() %>%
  mutate(rowid = 1:length(rural.cr), cr.dum = as.factor(ifelse(rural.cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

ruralahp %>% 
  ahp.indpref(main.atts, method = "eigen") %>% 
  mutate(rowid = 1:nrow(ruraleigentrue)) %>%
  left_join(ruralcr.df, by = 'rowid') %>%
  gather(Bio, Econ, Heal, Wat, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(ahp.rural), ",", "Mean CR =",
                             round(mean(rural.cr),3)))+
  theme_minimal()

#Finding inconsistent pairwise comparisons by maximum

ahp.pwerror(specahp,main.atts) %>%
gather(top1, top2, top3, key = "max", value = "pair") %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(x = pair, y = Freq, fill = max)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous("Frequency", breaks = c(seq(0,180,20))) +
  scale_fill_discrete(breaks = c("top1", "top2", "top3"), labels = c("1", "2", "3")) +
  scale_x_discrete("Pair") +
  guides(fill = guide_legend(title="Rank")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.ontop = FALSE)

ahp.pwerror(ruralahp,main.atts) %>%
  gather(top1, top2, top3, key = "max", value = "pair") %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(x = pair, y = Freq, fill = max)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous("Frequency", breaks = c(seq(0,180,20))) +
  scale_fill_discrete(breaks = c("top1", "top2", "top3"), labels = c("1", "2", "3")) +
  scale_x_discrete("Pair") +
  guides(fill = guide_legend(title="Rank")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.ontop = FALSE)

# #Transforming inconsistent matrices
# ahp.harker(specahp,main.atts, iterations = 5, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
#   ahp.aggpref(main.atts, method = "eigen")
# 
# ahp.aggpref(specahp,main.atts, method = "eigen")
# 
# ahp.harker(ruralahp,main.atts, iterations = 5, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
#   ahp.aggpref(main.atts, method = "eigen")
# 
# ahp.aggpref(ruralahp,main.atts, method = "eigen")


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

#PCA
data.pca.rural <- data.test.rural[,c(1:29)]

pca.rural <- prcomp(data.pca.rural[,c(1:7,15,21:24,28,29)], scale. = T, center = T)
summary(pca.rural)
plot(pca.rural)

quartz(8,8)
autoplot(pca.rural,data = data.test.rural,loadings = TRUE, loadings.colour = 'blue', #colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3)

quartz(8,8)
autoplot(pca.rural,data = data.test.rural,loadings = TRUE, loadings.colour = 'blue', #colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3, x=3, y=4)


cor.rural <- cor(data.pca.rural[,c(1:7,15,21:24,28,29)])

#Number of factors
fa.parallel(data.pca.rural[,c(1:7,15,21:24,28,29)])
vss(data.pca.rural[,c(1:7,15,21:24,28,29)])

#Factor analysis
(fa.rural <- fa(data.pca.rural[,c(1:7,15,21:24,28,29)], nfactors = 4, scores = "regression"))
(fa.rural <- fa(data.pca.rural[,c(1:7,15,21,24,28,29)], nfactors = 3, scores = "regression", rotation = "promax"))

fa.rural$scores
quartz(8,8)
plot(fa.rural)

quartz(8,8)
fa.diagram(fa.rural)

quartz(8,8)
omega(data.pca.rural[,c(1:7,15,21,24,28,29)])

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


data.test.spec <- data.spec[,-c(1:3,18:51)]
data.test.spec[,-c(15,16,19,20,22,24,25)] <- lapply(data.test.spec[,-c(15,16,19,20,22,24,25)], as.integer)
data.test.spec <- cbind(data.test.spec, 
                         canned.main.eigen$indpref[canned.main.eigen$indpref$cID=="Specialist",c(1:5)],
                         canned.fire.eigen$indpref[canned.main.eigen$indpref$cID=="Specialist",c(2:9)])


# PCA ---------------------------------------------------------------------


data.pca.spec <- data.test.spec[,c(1:14)]
summary(data.pca.spec)

pca.spec <- prcomp(na.omit(data.pca.spec[,-c(1,2,5)]), scale. = T, center = T)
summary(pca.spec)
plot(pca.spec)

quartz(8,8)
autoplot(pca.spec,loadings = TRUE, loadings.colour = 'blue', #colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3)

quartz(8,8)
autoplot(pca.spec,loadings = TRUE, loadings.colour = 'blue', #colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3, x=3, y=4)

(cor.spec <- cor(na.omit(data.pca.spec)))


# Factor analysis, clustering and SEM -------------------------------------

#Number of factors
fa.parallel(data.pca.spec[,-c(1,2,5)])
vss(data.pca.spec[,-c(1,2,5)])

#Factor analysis
(fa.spec <- fa(data.pca.spec[,-c(1,2,5)], nfactors = 2, scores = "regression"))
(fa.spec <- fa(data.pca.spec[,-c(1,2,5)], nfactors = 2, scores = "regression", rotation = "promax"))

fa.spec$scores
quartz(8,8)
plot(fa.spec)

quartz(8,8)
fa.diagram(fa.spec)

quartz(8,8)
omega(data.pca.spec[,-c(1,2,5)], nfactors = 2)

quartz(8,12)
pairs.panels(data.pca.spec[,-c(1,2,5)])

#dev.off()

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

plot(prcomp(data.rural.spec.test[,c(1:6)], scale. = T, center = T))

quartz(8,8)
autoplot(prcomp(data.rural.spec.test[,c(1:6)], scale. = T, center = T),alpha=.4,
         data = data.rural.spec.test,loadings = TRUE, loadings.colour = 'blue', colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3)

quartz(8,8)
autoplot(prcomp(data.rural.spec.test[,c(1:6)], scale. = T, center = T),alpha=.4,
         data = data.rural.spec.test,loadings = TRUE, loadings.colour = 'blue', colour = "cID",
         loadings.label = TRUE, loadings.label.size = 3, x=3, y=4)


fa(data.rural.spec.test[,c(1:6)],nfactors = 2)
(fa.test <- fa(data.rural.spec.test[,c(1:6)],nfactors = 2, rotate = "promax"))

quartz(8,8)
plot(fa.test)

quartz(8,8)
fa.diagram(fa.test)

quartz(8,8)
clust.rural.spec <- iclust(data.rural.spec.test[,c(1:6)])

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

# Models ------------------------------------------------------------------
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
#Categorical or Bernoulli models
#Dirichlet for multivariate models or beta for binary (0-1) - AHP

#Possible predictors: Gender, Age, Income, SpecsxRuralProps, Time_Occupation (specialists), Scolarity, Prop_Area_ha (rural props), Time_Rural(rural props)

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

# quanteda -  content analysis --------------------------------------------

library(quanteda)
library(stopwords)
library(readtext)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.textstats)
stopwords_getsources()

text_spec <- read.csv("Open_Answers_Specs.csv", h=T)
text_spec$Type <- rep("Specialist", nrow(text_spec))

text_rural <- read.csv("Open_Answers_Rural.csv", h=T)
text_rural$MIF_Sugg <- rep(NA, nrow(text_rural))
text_rural$Type <- rep("RuralProp", nrow(text_rural))

text_rural_spec <- rbind(text_rural, text_spec)

Cerrado.pos <- corpus(text_rural_spec[is.na(text_rural_spec$Cerrado_Pos)==FALSE,], 
                      text_field = "Cerrado_Pos")
Cerrado.pos
summary(Cerrado.pos)

Cerrado.neg <- corpus(text_rural_spec[is.na(text_rural_spec$Cerrado_Neg)==FALSE,], 
                      text_field = "Cerrado_Neg")
Cerrado.neg
summary(Cerrado.neg)

Cerrado.MIF <- corpus(text_rural_spec[is.na(text_rural_spec$MIF_Sugg)==FALSE,], 
                      text_field = "MIF_Sugg")
Cerrado.MIF
summary(Cerrado.MIF)


stopwords("portuguese")

#Create a document feature matrix
#Negative
mytokens.neg <- tokens(Cerrado.neg,remove_punct = TRUE,remove_numbers = TRUE)
doc.neg.dfm <- dfm(mytokens.neg)
doc.neg.dfm <- dfm_remove(doc.neg.dfm, 
                          pattern=c(stopwords("portuguese"),"deste","desta","desse","dessa"))
doc.neg.dfm <- dfm_select(doc.neg.dfm,selection="remove",min_nchar=3)
doc.neg.dfm <- dfm_wordstem(doc.neg.dfm, language = "pt")
doc.neg.dfm

topfeatures(doc.neg.dfm, 50)  # 50 top words
topfeatures(doc.neg.dfm, 100)  # 100 top words
topfeatures(doc.neg.dfm, 50, groups=Type)  # 25 top words

#Positive
mytokens.pos <- tokens(Cerrado.pos,remove_punct = TRUE,remove_numbers = TRUE)
doc.pos.dfm <- dfm(mytokens.pos)
doc.pos.dfm <- dfm_remove(doc.pos.dfm, 
                          pattern=c(stopwords("portuguese"),"deste","desta","desse","dessa"))
doc.pos.dfm <- dfm_select(doc.pos.dfm,selection="remove",min_nchar=3)
doc.pos.dfm <- dfm_wordstem(doc.pos.dfm, language = "pt")
doc.pos.dfm

topfeatures(doc.pos.dfm, 50)  # 50 top words
topfeatures(doc.pos.dfm, 100)  # 100 top words
topfeatures(doc.pos.dfm, 50, groups=Type)  # 25 top words

#MIF
mytokens.MIF <- tokens(Cerrado.MIF,remove_punct = TRUE,remove_numbers = TRUE)
doc.MIF.dfm <- dfm(mytokens.MIF)
doc.MIF.dfm <- dfm_remove(doc.MIF.dfm, 
                          pattern=c(stopwords("portuguese"),"deste","desta","desse","dessa"))
doc.MIF.dfm <- dfm_select(doc.MIF.dfm,selection="remove",min_nchar=3)
doc.MIF.dfm <- dfm_wordstem(doc.MIF.dfm, language = "pt")
doc.MIF.dfm

topfeatures(doc.MIF.dfm, 50)  # 50 top words
topfeatures(doc.MIF.dfm, 100)  # 100 top words

# 
# dic<-dictionary(list(natural = c("bio*", "espéc*","socioambient*","veget*","desmat*","extin*","ambient*","natur*","diversid*","sol*","serr*","faun"),
#                      RH = c("águ*","nível","d'águ","bacia","rio","montant","jusant","cot*"),
#                      social = c("soci*", "popul*","cidad*","munic*","terr*","fam*","socioambient*","reassent*","audiência","urb","servic*"),
#                      economia = c("econom*","produ*","mercado*","cust*","sistema","cota","paulo","paran","setor","ecôn*"),
#                      energia = c("energ*","elétric*","gera*","usin*","sistema*","program*","plan*","aproveitament*","angra","termel*","nuclear*"),
#                      local = c("áre*","loc*","onde*","trech*"),
#                      regional = c("estado","regiã","nort*"),
#                      nacional = c("brasil*","nacion*"),
#                      tocantins = "tocantin*",
#                      importancia = c("import*","relev*","necess*","permanent*","principal","consider*"),
#                      empreend = c("formac*","implant*","instal*","ahe","empreend*","hidrelétric*","obra*","ench*","barrag*","constr*","projet*","reservatóri","oper*","prazo","inund*","canteir*"),
#                      impacto = c("impact*","interf*","conseq*","afet*","magnitud*","interligad*","alter","interfer*","indir*","diret*","durant*"),
#                      negativo = c("negat*","preju*","perd*"),
#                      médio = c("méd*","med*","part*"),
#                      positivo = c("positiv*","benef*"),
#                      alto = c("alt*","grand*","aument*","cumulativ*","maior*"),
#                      baixo = c("baix*","peq*","diminui*","mín*","queda","men*"),
#                      alternativa = c("alternativ*","cenári","outr*"),
#                      ciencia = c("estud*","fator","projeto","cerca","refer*","potência","total","eixo","tabel*","carac*","aprox*","relaç*"),
#                      mitigacao = "mitiga*",
#                      compens = c("compens*","dev*","inclu*","prefeit*","necess*","acordo"),
#                      viabilidade = "via*",
#                      renovavel = "renov*",
#                      peixe = "peix*",
#                      lajeado = "laj*",
#                      ipueiras = "ipueir*"))

# dfm.subset<-dfm_select(doc.dfm,pattern=dic,valuetype="glob")
# dfm.subset
# head(dfm.subset, n = 7, nf = 15)
# tail(dfm.subset, n = 7, nf = 15)
# dfm.subset<-dfm_lookup(dfm.subset,dic,exclusive=TRUE)
# dfm.subset
# head(dfm.subset, n = 7, nf = 15)
# tail(dfm.subset, n = 7, nf = 15)

#Positive
fcm.subset.pos<-fcm(doc.pos.dfm, context = "document", count = "frequency")
fcm.subset.pos
rowSums(fcm.subset.pos> 2)

head(fcm.subset.pos, n = 15, nf = 15)
tail(fcm.subset.pos, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.pos,edge_alpha = 0.4, edge_size = 1,min_freq=2, max.overlaps=5)

doc.pos.dfm.group <- dfm_group(doc.pos.dfm, groups = Type)

quartz(8,12)
textplot_network(doc.pos.dfm.group,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5)

quartz(8,8)
textplot_wordcloud(doc.pos.dfm.group, min_count = 2, random_order = FALSE,
                   rotation = .25, comparison = T,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

quartz(8,8)
textplot_wordcloud(doc.pos.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

#Negative
fcm.subset.neg<-fcm(doc.neg.dfm, context = "document", count = "frequency")
fcm.subset.neg
rowSums(fcm.subset.neg> 0)

head(fcm.subset.neg, n = 15, nf = 15)
tail(fcm.subset.neg, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.neg,edge_alpha = 0.4, edge_size = 1,min_freq=2, max.overlaps=5)

doc.neg.dfm.group <- dfm_group(doc.neg.dfm, groups = Type)

quartz(8,12)
textplot_network(doc.neg.dfm.group,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5,
                 comparison = T)

quartz(8,8)
textplot_wordcloud(doc.neg.dfm.group, min_count = 2, random_order = FALSE,
                   rotation = .25, comparison = T,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

quartz(8,8)
textplot_wordcloud(doc.neg.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

#MIF
fcm.subset.mif<-fcm(doc.MIF.dfm, context = "document", count = "frequency")
fcm.subset.mif
rowSums(fcm.subset.mif> 0)

head(fcm.subset.mif, n = 15, nf = 15)
tail(fcm.subset.mif, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.mif,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5)

quartz(8,8)
textplot_wordcloud(doc.MIF.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))


#################
#Text statistics#
#################
library(quanteda.textstats)

#Negative
freq.stats.neg<-textstat_frequency(doc.neg.dfm)
write.table(freq.stats.neg,"freq.stats.neg.txt")

freq.stats.neg.type<-textstat_frequency(doc.neg.dfm,groups=Type)
write.table(freq.stats.neg.type,"freq.stats.neg.type.txt")

#Positive
freq.stats.pos<-textstat_frequency(doc.pos.dfm)
write.table(freq.stats.pos,"freq.stats.pos.txt")

freq.stats.pos.type<-textstat_frequency(doc.pos.dfm,groups=Type)
write.table(freq.stats.pos.type,"freq.stats.pos.type.txt")

#MIF
freq.stats.mif<-textstat_frequency(doc.MIF.dfm)
write.table(freq.stats.mif,"freq.stats.mif.txt")

# #Calculate readability of a corpus
# textstat_readability(x, measure = "Flesch")
# textstat_readability(x, measure = "Flesch.Kincaid")
# 
# #Calculate lexical diversity of a dfm
# textstat_lexdiv(doc.dfm, measure = "TTR")
# 
# #Compute entropy of documents
# textstat_entropy(doc.dfm)

#Negative
#Measure distance or similarity from a dfm
simil<-textstat_simil(doc.neg.dfm, method = "cosine")
simil

# hierarchical clustering - get distances on normalized dfm
dist_mat <- textstat_dist(dfm_weight(doc.neg.dfm, "prop"))

# hierarchical clustering the distance object
doc_cluster <- hclust(as.dist(dist_mat))

# plot as a dendrogram
quartz(8,8)
plot(doc_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Token Frequency")

#Positive
#Measure distance or similarity from a dfm
simil<-textstat_simil(doc.pos.dfm, method = "cosine")
simil

# hierarchical clustering - get distances on normalized dfm
dist_mat <- textstat_dist(dfm_weight(doc.pos.dfm, "prop"))

# hierarchical clustering the distance object
doc_cluster <- hclust(as.dist(dist_mat))

# plot as a dendrogram
quartz(8,8)
plot(doc_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Token Frequency")

#MIF
#Measure distance or similarity from a dfm
simil<-textstat_simil(doc.MIF.dfm, method = "cosine")
simil

# hierarchical clustering - get distances on normalized dfm
dist_mat <- textstat_dist(dfm_weight(doc.MIF.dfm, "prop"))

# hierarchical clustering the distance object
doc_cluster <- hclust(as.dist(dist_mat))

# plot as a dendrogram
quartz(8,8)
plot(doc_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Token Frequency")

#Calculate keyness statistics
head(tstat1 <- textstat_keyness(doc.neg.dfm,
                                target = docvars(Cerrado.neg, "Type")=="Specialist")
     , 25)
tail(tstat1, 25)
quartz(8,8)
textplot_keyness(tstat1)

head(tstat2 <- textstat_keyness(doc.pos.dfm,
                                target = docvars(Cerrado.pos, "Type")=="Specialist"), 25)
tail(tstat2, 25)
quartz(8,8)
textplot_keyness(tstat2)

#Locate keywords-in-context
loc.impacto<-kwic(Cerrado.pos, "bio*")
kwic(mytokens.pos, "bio*")

#################
#Fit text models#
#################
library(quanteda.textmodels)

#######################
#Por tipo de documento#
#######################
#Positive
(tmod.pos <- textmodel_nb(x=doc.pos.dfm, y = Cerrado.pos$Type, 
                        distribution = "Bernoulli",prior="termfreq"))
summary(tmod.pos)
coef.tmod.pos<-coef(tmod.pos)
write.table(coef.tmod.pos,"coef.tmod.pos.txt")

#Inspect the quality of classification
actual_class <- docvars(doc.pos.dfm, "Type")
predicted_class <- predict(tmod.pos, newdata = doc.pos.dfm)
tab_class <- table(actual_class, predicted_class)
tab_class #91%!!!
prop.table(table(predict(tmod.pos) == docvars(doc.pos.dfm, "Type"))) * 100

r <- 1:10000
for(i in 1:10000){
  r[i] <- prop.table(table(sample(predict(tmod.pos)) == docvars(doc.pos.dfm, "Type")))[1] * 100 #random
}
mean(r)

library(caret)
(confusion<-confusionMatrix(tab_class, mode = "everything"))

# Save confusion matrix as data frame
confusion.data <- as.data.frame(confusion[["table"]])

#Negative
(tmod.neg <- textmodel_nb(x=doc.neg.dfm, y = Cerrado.neg$Type, 
                          distribution = "Bernoulli",prior="termfreq"))
summary(tmod.neg)
coef.tmod.neg<-coef(tmod.neg)
write.table(coef.tmod.neg,"coef.tmod.neg.txt")

#Inspect the quality of classification
actual_class <- docvars(doc.neg.dfm, "Type")
predicted_class <- predict(tmod.neg, newdata = doc.neg.dfm)
tab_class <- table(actual_class, predicted_class)
tab_class #78.8%!!!
prop.table(table(predict(tmod.neg) == docvars(doc.neg.dfm, "Type"))) * 100

r <- 1:10000
for(i in 1:10000){
  r[i] <- prop.table(table(sample(predict(tmod.neg)) == docvars(doc.neg.dfm, "Type")))[1] * 100 #random
}
mean(r)

library(caret)
(confusion<-confusionMatrix(tab_class, mode = "everything"))

# Save confusion matrix as data frame
confusion.data <- as.data.frame(confusion[["table"]])

#Latent Semantic Analysis#
##########################
tmod8<-textmodel_lsa(doc.dfm,margin="both")
head(tmod8$docs)
str(tmod8)


tmod8$matrix_low_rank[,1:10]
write.table(tmod8$matrix_low_rank,"tmod8_matrix_low_rank.txt")
coef.tmod8<-coef(tmod8,se=T)
str(coef.tmod8)
coef.tmod8$coef_document
write.table(coef.tmod8$coef_feature,"coef.tmod8.txt")

#Esse tipo de modelo ? usado para reduzir a dimensionalidade (tipo uma PCA). Portanto, n?o nos serve

#Topic Models
if (require(topicmodels)) {
  my_lda_fit20 <- LDA(convert(doc.neg.dfm, to = "topicmodels"), k = 20)
  get_terms(my_lda_fit20, 10)
}

lda.similarity <- as.data.frame(my_lda_fit20@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

# par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

if (require(topicmodels)) {
  my_lda_fit20 <- LDA(convert(doc.pos.dfm[1,], to = "topicmodels"), k = 20)
  get_terms(my_lda_fit20, 8)
}

if (require(topicmodels)) {
  my_lda_fit20 <- LDA(convert(doc.MIF.dfm[2,], to = "topicmodels"), k = 20)
  get_terms(my_lda_fit20, 8)
}


set.seed(100)
if (require("stm")) {
  my_lda_fit20 <- stm(doc.neg.dfm, K = 5, verbose = FALSE)
  plot(my_lda_fit20)
}


#English translated by Google Translate service#
################################################

stopwords_getsources()

text_spec <- read.csv("Open_Answers_Specs_En.csv", h=T)
text_spec$Type <- rep("Specialist", nrow(text_spec))

text_rural <- read.csv("Open_Answers_Rural_En.csv", h=T)
text_rural$MIF_Sugg <- rep(NA, nrow(text_rural))
text_rural$Type <- rep("RuralProp", nrow(text_rural))

text_rural_spec <- rbind(text_rural, text_spec)

Cerrado.pos <- corpus(text_rural_spec[is.na(text_rural_spec$Cerrado_Pos)==FALSE,], 
                      text_field = "Cerrado_Pos")
Cerrado.pos
summary(Cerrado.pos)

Cerrado.neg <- corpus(text_rural_spec[is.na(text_rural_spec$Cerrado_Neg)==FALSE,], 
                      text_field = "Cerrado_Neg")
Cerrado.neg
summary(Cerrado.neg)

Cerrado.MIF <- corpus(text_rural_spec[is.na(text_rural_spec$MIF_Sugg)==FALSE,], 
                      text_field = "MIF_Sugg")
Cerrado.MIF
summary(Cerrado.MIF)


stopwords("en")

#Create a document feature matrix
#Negative
mytokens.neg <- tokens(Cerrado.neg,remove_punct = TRUE,remove_numbers = TRUE)
doc.neg.dfm <- dfm(mytokens.neg)
doc.neg.dfm <- dfm_remove(doc.neg.dfm, 
                          pattern=c(stopwords("en")))
doc.neg.dfm <- dfm_select(doc.neg.dfm,selection="remove",min_nchar=3)
doc.neg.dfm <- dfm_wordstem(doc.neg.dfm, language = "en")
doc.neg.dfm

topfeatures(doc.neg.dfm, 50)  # 50 top words
topfeatures(doc.neg.dfm, 100)  # 100 top words
topfeatures(doc.neg.dfm, 50, groups=Type)  # 25 top words

#Positive
mytokens.pos <- tokens(Cerrado.pos,remove_punct = TRUE,remove_numbers = TRUE)
doc.pos.dfm <- dfm(mytokens.pos)
doc.pos.dfm <- dfm_remove(doc.pos.dfm, 
                          pattern=c(stopwords("en")))
doc.pos.dfm <- dfm_select(doc.pos.dfm,selection="remove",min_nchar=3)
doc.pos.dfm <- dfm_wordstem(doc.pos.dfm, language = "en")
doc.pos.dfm

topfeatures(doc.pos.dfm, 50)  # 50 top words
topfeatures(doc.pos.dfm, 100)  # 100 top words
topfeatures(doc.pos.dfm, 50, groups=Type)  # 25 top words

#MIF
mytokens.MIF <- tokens(Cerrado.MIF,remove_punct = TRUE,remove_numbers = TRUE)
doc.MIF.dfm <- dfm(mytokens.MIF)
doc.MIF.dfm <- dfm_remove(doc.MIF.dfm, 
                          pattern=c(stopwords("en")))
doc.MIF.dfm <- dfm_select(doc.MIF.dfm,selection="remove",min_nchar=3)
doc.MIF.dfm <- dfm_wordstem(doc.MIF.dfm, language = "en")
doc.MIF.dfm

topfeatures(doc.MIF.dfm, 50)  # 50 top words
topfeatures(doc.MIF.dfm, 100)  # 100 top words

# 
# dic<-dictionary(list(natural = c("bio*", "espéc*","socioambient*","veget*","desmat*","extin*","ambient*","natur*","diversid*","sol*","serr*","faun"),
#                      RH = c("águ*","nível","d'águ","bacia","rio","montant","jusant","cot*"),
#                      social = c("soci*", "popul*","cidad*","munic*","terr*","fam*","socioambient*","reassent*","audiência","urb","servic*"),
#                      economia = c("econom*","produ*","mercado*","cust*","sistema","cota","paulo","paran","setor","ecôn*"),
#                      energia = c("energ*","elétric*","gera*","usin*","sistema*","program*","plan*","aproveitament*","angra","termel*","nuclear*"),
#                      local = c("áre*","loc*","onde*","trech*"),
#                      regional = c("estado","regiã","nort*"),
#                      nacional = c("brasil*","nacion*"),
#                      tocantins = "tocantin*",
#                      importancia = c("import*","relev*","necess*","permanent*","principal","consider*"),
#                      empreend = c("formac*","implant*","instal*","ahe","empreend*","hidrelétric*","obra*","ench*","barrag*","constr*","projet*","reservatóri","oper*","prazo","inund*","canteir*"),
#                      impacto = c("impact*","interf*","conseq*","afet*","magnitud*","interligad*","alter","interfer*","indir*","diret*","durant*"),
#                      negativo = c("negat*","preju*","perd*"),
#                      médio = c("méd*","med*","part*"),
#                      positivo = c("positiv*","benef*"),
#                      alto = c("alt*","grand*","aument*","cumulativ*","maior*"),
#                      baixo = c("baix*","peq*","diminui*","mín*","queda","men*"),
#                      alternativa = c("alternativ*","cenári","outr*"),
#                      ciencia = c("estud*","fator","projeto","cerca","refer*","potência","total","eixo","tabel*","carac*","aprox*","relaç*"),
#                      mitigacao = "mitiga*",
#                      compens = c("compens*","dev*","inclu*","prefeit*","necess*","acordo"),
#                      viabilidade = "via*",
#                      renovavel = "renov*",
#                      peixe = "peix*",
#                      lajeado = "laj*",
#                      ipueiras = "ipueir*"))

# dfm.subset<-dfm_select(doc.dfm,pattern=dic,valuetype="glob")
# dfm.subset
# head(dfm.subset, n = 7, nf = 15)
# tail(dfm.subset, n = 7, nf = 15)
# dfm.subset<-dfm_lookup(dfm.subset,dic,exclusive=TRUE)
# dfm.subset
# head(dfm.subset, n = 7, nf = 15)
# tail(dfm.subset, n = 7, nf = 15)

#Positive
fcm.subset.pos<-fcm(doc.pos.dfm, context = "document", count = "frequency")
fcm.subset.pos
rowSums(fcm.subset.pos> 2)

head(fcm.subset.pos, n = 15, nf = 15)
tail(fcm.subset.pos, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.pos,edge_alpha = 0.4, edge_size = 1,min_freq=2, max.overlaps=5)

doc.pos.dfm.group <- dfm_group(doc.pos.dfm, groups = Type)

quartz(8,12)
textplot_network(doc.pos.dfm.group,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5)

quartz(8,8)
textplot_wordcloud(doc.pos.dfm.group, min_count = 2, random_order = FALSE,
                   rotation = .25, comparison = T,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

quartz(8,8)
textplot_wordcloud(doc.pos.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

#Negative
fcm.subset.neg<-fcm(doc.neg.dfm, context = "document", count = "frequency")
fcm.subset.neg
rowSums(fcm.subset.neg> 0)

head(fcm.subset.neg, n = 15, nf = 15)
tail(fcm.subset.neg, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.neg,edge_alpha = 0.4, edge_size = 1,min_freq=2, max.overlaps=5)

doc.neg.dfm.group <- dfm_group(doc.neg.dfm, groups = Type)

quartz(8,12)
textplot_network(doc.neg.dfm.group,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5,
                 comparison = T)

quartz(8,8)
textplot_wordcloud(doc.neg.dfm.group, min_count = 2, random_order = FALSE,
                   rotation = .25, comparison = T,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

quartz(8,8)
textplot_wordcloud(doc.neg.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

#MIF
fcm.subset.mif<-fcm(doc.MIF.dfm, context = "document", count = "frequency")
fcm.subset.mif
rowSums(fcm.subset.mif> 0)

head(fcm.subset.mif, n = 15, nf = 15)
tail(fcm.subset.mif, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.mif,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5)

quartz(8,8)
textplot_wordcloud(doc.MIF.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))


#################
#Text statistics#
#################
library(quanteda.textstats)

#Negative
freq.stats.neg<-textstat_frequency(doc.neg.dfm)
write.table(freq.stats.neg,"freq.stats.neg.en.txt")

freq.stats.neg.type<-textstat_frequency(doc.neg.dfm,groups=Type)
write.table(freq.stats.neg.type,"freq.stats.neg.type.en.txt")

#Positive
freq.stats.pos<-textstat_frequency(doc.pos.dfm)
write.table(freq.stats.pos,"freq.stats.pos.en.txt")

freq.stats.pos.type<-textstat_frequency(doc.pos.dfm,groups=Type)
write.table(freq.stats.pos.type,"freq.stats.pos.type.en.txt")

#MIF
freq.stats.mif<-textstat_frequency(doc.MIF.dfm)
write.table(freq.stats.mif,"freq.stats.mif.en.txt")


#Measure distance or similarity from a dfm#
###########################################
#Negative
simil<-textstat_simil(doc.neg.dfm, method = "cosine")
simil

# hierarchical clustering - get distances on normalized dfm
dist_mat <- textstat_dist(dfm_weight(doc.neg.dfm, "prop"))

# hierarchical clustering the distance object
doc_cluster <- hclust(as.dist(dist_mat))

# plot as a dendrogram
quartz(8,8)
plot(doc_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Negative Token Frequency")

#Positive
#Measure distance or similarity from a dfm
simil<-textstat_simil(doc.pos.dfm, method = "cosine")
simil

# hierarchical clustering - get distances on normalized dfm
dist_mat <- textstat_dist(dfm_weight(doc.pos.dfm, "prop"))

# hierarchical clustering the distance object
doc_cluster <- hclust(as.dist(dist_mat))

# plot as a dendrogram
quartz(8,8)
plot(doc_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Positive Token Frequency")

#MIF
#Measure distance or similarity from a dfm
simil<-textstat_simil(doc.MIF.dfm, method = "cosine")
simil

# hierarchical clustering - get distances on normalized dfm
dist_mat <- textstat_dist(dfm_weight(doc.MIF.dfm, "prop"))

# hierarchical clustering the distance object
doc_cluster <- hclust(as.dist(dist_mat))

# plot as a dendrogram
quartz(8,8)
plot(doc_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Token Frequency")

#Calculate keyness statistics
head(tstat1 <- textstat_keyness(doc.neg.dfm,
                                target = docvars(Cerrado.neg, "Type")=="Specialist")
     , 25)
tail(tstat1, 25)
quartz(8,8)
textplot_keyness(tstat1)

head(tstat2 <- textstat_keyness(doc.pos.dfm,
                                target = docvars(Cerrado.pos, "Type")=="Specialist"), 25)
tail(tstat2, 25)
quartz(8,8)
textplot_keyness(tstat2)

#Locate keywords-in-context
loc.impacto<-kwic(Cerrado.pos, "bio*")
kwic(mytokens.pos, "bio*")

#################
#Fit text models#
#################
library(quanteda.textmodels)

#######################
#Por tipo de documento#
#######################

#Positive
(tmod.pos <- textmodel_nb(x=doc.pos.dfm, y = Cerrado.pos$Type, 
                          distribution = "Bernoulli",prior="termfreq"))
summary(tmod.pos)
coef.tmod.pos<-coef(tmod.pos)
write.table(coef.tmod.pos,"coef.tmod.pos.eng.txt")

#Inspect the quality of classification
actual_class <- docvars(doc.pos.dfm, "Type")
predicted_class <- predict(tmod.pos, newdata = doc.pos.dfm)
tab_class <- table(actual_class, predicted_class)
tab_class #91%!!!
prop.table(table(predict(tmod.pos) == docvars(doc.pos.dfm, "Type"))) * 100

r <- 1:10000
for(i in 1:10000){
  r[i] <- prop.table(table(sample(predict(tmod.pos)) == docvars(doc.pos.dfm, "Type")))[1] * 100 #random
}
mean(r)

library(caret)
(confusion<-confusionMatrix(tab_class, mode = "everything"))

library(tidyverse)
nb.terms <- as.data.frame(t(tmod.pos$param)) %>% 
  rownames_to_column("Word") %>% 
  gather(Type, Association, -Word) %>% 
  arrange(Type, desc(Association)) %>% 
  group_by(Type) %>% 
  mutate(Rank = row_number()) %>% 
  filter(Rank <= 25)

quartz(8,8)
ggplot(filter(nb.terms, Type == "RuralProp"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Rural landowners")

quartz(8,8)
ggplot(filter(nb.terms, Type == "Specialist"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Specialists")

#Negative
(tmod.neg <- textmodel_nb(x=doc.neg.dfm, y = Cerrado.neg$Type, 
                          distribution = "Bernoulli",prior="termfreq"))
summary(tmod.neg)
coef.tmod.neg<-coef(tmod.neg)
write.table(coef.tmod.neg,"coef.tmod.neg.eng.txt")

#Inspect the quality of classification
actual_class <- docvars(doc.neg.dfm, "Type")
predicted_class <- predict(tmod.neg, newdata = doc.neg.dfm)
tab_class <- table(actual_class, predicted_class)
tab_class #82.35%!!!
prop.table(table(predict(tmod.neg) == docvars(doc.neg.dfm, "Type"))) * 100

r <- 1:10000
for(i in 1:10000){
  r[i] <- prop.table(table(sample(predict(tmod.neg)) == docvars(doc.neg.dfm, "Type")))[1] * 100 #random
}
mean(r)

library(caret)
(confusion<-confusionMatrix(tab_class, mode = "everything"))

nb.terms <- as.data.frame(t(tmod.neg$param)) %>% 
  rownames_to_column("Word") %>% 
  gather(Type, Association, -Word) %>% 
  arrange(Type, desc(Association)) %>% 
  group_by(Type) %>% 
  mutate(Rank = row_number()) %>% 
  filter(Rank <= 25)

quartz(8,8)
ggplot(filter(nb.terms, Type == "RuralProp"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Rural landowners")

quartz(8,8)
ggplot(filter(nb.terms, Type == "Specialist"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Specialists")

#Topic Models#
##############
if (require(topicmodels)) {
  set.seed(123)
  neg_lda_fit20 <- LDA(convert(doc.neg.dfm, to = "topicmodels"), k = 20)
  get_terms(neg_lda_fit20, 10)
}


lda.similarity <- as.data.frame(neg_lda_fit20@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

# par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

if (require(topicmodels)) {
  set.seed(123)
  pos_lda_fit20 <- LDA(convert(doc.pos.dfm, to = "topicmodels"), k = 20)
  get_terms(pos_lda_fit20, 10)
}

lda.similarity <- as.data.frame(pos_lda_fit20@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

# par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

if (require(topicmodels)) {
  set.seed(123)
  mif_lda_fit20 <- LDA(convert(doc.MIF.dfm, to = "topicmodels"), k = 20)
  get_terms(mif_lda_fit20, 10)
}

lda.similarity <- as.data.frame(mif_lda_fit20@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

# par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

#Using STM#
###########
library(stm)

#Negative
dfm.neg2stm <- convert(doc.neg.dfm, to = "stm")

neg.stm.idealK <- searchK(dfm.neg2stm$documents, 
                          dfm.neg2stm$vocab, 
                          prevalence = ~Type, content = ~Type,
                          data = dfm.neg2stm$meta,
                          K = seq(3, 20, by = 1))

plot(neg.stm.idealK)

set.seed(123)
neg_lda_fit10 <- stm(dfm.neg2stm$documents, 
                     dfm.neg2stm$vocab, 
                     prevalence = ~Type, content = ~Type,
                     data = dfm.neg2stm$meta, 
                     K = 13, verbose = FALSE)

summary(neg_lda_fit10)

prep.neg <- estimateEffect(~ Type, neg_lda_fit10, metadata = dfm.neg2stm$meta)
summary(prep.neg)#Only topics 5 and 8

quartz(8,8)
plot(neg_lda_fit10)

# quartz(12,8)
# par(mar=c(1,1,1,1))
# plot(neg_lda_fit10,"labels")

quartz(8,8)
plot.topicCorr(topicCorr(neg_lda_fit10))

#quartz(8,8)
plot(neg_lda_fit10, "perspectives", topics = c(6,2))

#quartz(8,8)
plot(neg_lda_fit10, "perspectives", topics = c(8,3))

#quartz(8,8)
plot(neg_lda_fit10, "hist")

#quartz(8,8)
cloud(neg_lda_fit10, topic = 12)

#Positive
dfm.pos2stm <- convert(doc.pos.dfm, to = "stm")

pos.stm.idealK <- searchK(dfm.pos2stm$documents, 
                          dfm.pos2stm$vocab, 
                          prevalence = ~Type, content = ~Type,
                          data = dfm.pos2stm$meta,
                          K = seq(3, 20, by = 1))

plot(pos.stm.idealK)

set.seed(123)
pos_lda_fit10 <- stm(dfm.pos2stm$documents, 
                     dfm.pos2stm$vocab, K = 8, verbose = FALSE,
                     prevalence = ~Type, content = ~Type,
                     data = dfm.pos2stm$meta)

summary(pos_lda_fit10)

prep.pos <- estimateEffect(~ Type, pos_lda_fit10, metadata = dfm.pos2stm$meta)
summary(prep.pos)#Only topics 5 and 8

quartz(8,8)
plot(pos_lda_fit10)

quartz(12,8)
par(mar=c(1,1,1,1))
plot(pos_lda_fit10,"labels")

quartz(8,8)
plot.topicCorr(topicCorr(pos_lda_fit10))

quartz(8,8)
plot(pos_lda_fit10, "perspectives", topics = 8)

quartz(8,8)
plot(pos_lda_fit10, "perspectives", topics = 5)

quartz(8,8)
plot(pos_lda_fit10, "hist")

#MIF
dfm.mif2stm <- convert(doc.MIF.dfm, to = "stm")

mif.stm.idealK <- searchK(dfm.mif2stm$documents, 
                          dfm.mif2stm$vocab, K = seq(3, 20, by = 1))

plot(mif.stm.idealK)

set.seed(123)
mif_lda_fit10 <- stm(doc.MIF.dfm, K = 14, verbose = FALSE, seed = 123)

summary(mif_lda_fit10)

quartz(8,8)
plot(mif_lda_fit10)

quartz(12,8)
par(mar=c(1,1,1,1))
plot(mif_lda_fit10,"labels")

quartz(8,8)
plot.topicCorr(topicCorr(mif_lda_fit10))

quartz(8,8)
plot(mif_lda_fit10, "perspectives", topics = c(1,2))

quartz(8,8)
plot(mif_lda_fit10, "hist")