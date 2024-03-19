# Testing hypotheses based on White & Rawlins' MegaVeridicality I dataset 
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
library(ggforce)
library(ordinal)
theme_set(theme_bw())

# load data
d = read.csv("../data/d.csv")
nrow(d) # 21692
names(d)

# load predicate coding
y = read.csv("../data/predicate-coding.csv")
nrow(y) # 544
names(y)

d = left_join(d, y, by = c("verb", "voice"))
nrow(d) # 21692

# specify colours for predicate types
cols =  c(cognitive = "coral",
          communicative = "deepskyblue2",
          emotive = "darkgreen",
          evidential = "purple")

cols2 =  c(cognitive = "coral",
           nonEmoComm = "deepskyblue2",
           emoComm = "green3",
           emotive = "darkgreen",
           evidential = "purple")


# H1: predicate type and projection ----
## H1.1 all predicate types ----
# We find that emotive predicates project more (by and large) than cognitive, evidential and communicative ones.

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType column
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                 emotive == "yes" ~ "emotive",
                                 cognitive == "yes" ~ "cognitive",
                                 evidential == "yes" ~ "evidential",
                                 TRUE ~ "other"),
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))

table(d.proj$predicateType)

# how many predicates in which type and which voice?
d.proj %>%
  select(predicateType, verb, voice) %>% 
  unique() %>% 
  group_by(predicateType,voice) %>% 
  summarize(count=n())


### by-predicateType ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 6
levels(mean.proj$predicateType)

#### plot ----
mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType.pdf", height = 6, width = 9)


### by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) #544
levels(mean.proj$verb_renamed)

# add predicateType, verb and voice to the means
tmp = d.proj %>%
  select(c(verb,verb_renamed,voice,predicateType)) %>%
  distinct(verb,verb_renamed,voice,predicateType)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) #523

#### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate.pdf", height = 4, width = 13)

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  labs(x = "Predicate",
    y = "Mean projection rating", 
    colour = "Predicate type") + 
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-faceted.pdf", height = 4, width = 9)

#### linear model ----
lm(Mean.Proj ~ predicateType, data = mean.proj) %>% 
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.21871    0.02756   7.937 1.29e-14 ***
# predicateTypecommunicative  0.08920    0.03039   2.935  0.00348 ** 
# predicateTypeemotive        0.50393    0.03195  15.771  < 2e-16 ***
# predicateTypeevidential     0.07522    0.03463   2.172  0.03031 * 

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

# set emotives as reference level for predicate type
d.proj = d.proj %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ predicateType + (1 | participant) +
       (1 | environment), data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: 
# as.factor(veridicality_num) ~ predicateType + (1 | participant) +  
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter    
# logit flexible  15662 -11343.24 22700.49 549(2742)
# max.grad
# 3.04e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7750   0.8803  
# environment (Intercept) 0.4843   0.6959  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#     predicateTypecognitive 
#                     -2.260 
# predicateTypecommunicative 
#                     -1.875 
#    predicateTypeevidential 
#                     -1.954 
# 
# Thresholds:
#   -1|0    0|1 
# -4.871 -1.240 


# with predicate type with emotive component distinction
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: 
# as.factor(veridicality_num) ~ predicateType2 + (1 | participant) +  
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC     
# logit flexible  15662 -11306.94 22629.88
# niter       max.grad
# 2316(11576) 2.12e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7755   0.8806  
# environment (Intercept) 0.4883   0.6988  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#   predicateType2cognitive    predicateType2emoComm 
#                    -2.262                   -1.444 
#  predicateType2evidential predicateType2nonEmoComm 
#                    -1.955                   -1.984 
# 
# Thresholds:
#   -1|0    0|1 
# -4.883 -1.238 


## H1.2: communicatives: with/without emotive component ----
# Amongst the communication predicates, does the CC of those that have an emotive 
# meaning component project more than the CCs of those that don't?

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType and emotiveComponent columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))


### by-predicateType ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType, emotiveComponent) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 7
levels(mean.proj$predicateType)

#### plot ----
mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
ggplot(aes(x = predicateType, y = Mean.Proj, colour = emotiveComponent)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour ="Emotive component") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = c("no" = "sienna4", "yes" = "green3"))
ggsave("../graphs/projection-by-predicateType-emotiveComponent.pdf", height = 6, width = 9)

### by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) #544
levels(mean.proj$verb_renamed)

# add predicateType, verb and emotiveComponent to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, emotiveComponent)) %>%
  distinct(verb, verb_renamed, predicateType, emotiveComponent)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) #544

# only communicative predicates
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) #236

#### plot ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = emotiveComponent)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
    theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Emotive component") +
  facet_wrap( ~ emotiveComponent, 
              labeller = as_labeller(c("no" = "communicatives\nwithout emotive component", 
                                       "yes" = "communicatives\nwith emotive component"))) +
  scale_colour_manual(values = c("deepskyblue2", "green3"))
ggsave("../graphs/projection-by-communicative-predicate.pdf", height = 6, width = 9)

#### linear model ----
lm(Mean.Proj ~ emotiveComponent, data = mean.proj) %>% 
  summary()
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.27881    0.01271  21.939  < 2e-16 ***
# emotiveComponentyes  0.14614    0.02848   5.132 6.03e-07 ***

#### ordinal model ----
# only communicative predicates
d.proj = d.proj %>%
  filter(predicateType == "communicative")

clmm(as.factor(veridicality_num) ~ emotiveComponent + (1 | participant) + 
       (1 | environment), data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ emotiveComponent + (1 | participant) + 
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs logLik   AIC      niter     max.grad
# logit flexible  7069 -5259.36 10528.71 272(1609) 3.09e-04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7489   0.8654  
# environment (Intercept) 0.9045   0.9510  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#  emotiveComponentyes 
#               0.5849 
# 
# Thresholds:
#    -1|0     0|1 
# -3.1856  0.7887 


# H2: valence and arousal ----

# load valence and arousal data
z = read.csv("../data/BRM-emot-submit.csv")
nrow(z) # 13915

d2 = z %>%
  filter(Word %in% d$verb) %>%
  rename(verb = Word) %>%
  inner_join(d, by = "verb")
nrow(d2) # 17747
n_distinct(d2$verb) # 423

# create predicate type, emotive component and environment columns
d2 = d2 %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"))

# remove "other" and "comPriv" predicates
d3 = d2 %>%
  filter(predicateType != "other" & predicateType != "comPriv")

# how many predicates?
n_distinct(d3$verb_renamed) # 428
n_distinct(d3$verb) # 410

d3 = d3 %>%
  distinct(verb_renamed, verb, voice, predicateType, predicateType2, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum)

table(d3$predicateType)
# cognitive communicative       emotive    evidential 
#        45           205           101            77 

table(d3$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        45         41        101         77        164 

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) # 544

# combine projection and valence/arousal/dominance ratings in one data frame
mean.proj2 = left_join(d3, mean.proj, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj2) # 428

## rescale V + A ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings for valence and arousal are rescaled to range from 0 to 1. 
new.scale <- mean.proj2 %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8)

# Valence: everything in order? - yes.
(max(mean.proj2$V.Mean.Sum)-min(mean.proj2$V.Mean.Sum))/4
# [1] 1.7025
new.scale %>% 
  group_by(V.Mean.Sum2.direction) %>% 
  slice_max(V.Mean.Sum2) %>% 
  ungroup() %>% 
  pull(V.Mean.Sum2) %>% 
  sum()
# [1] 1.7025

# arousal: everything in order? - yes.
(max(mean.proj2$A.Mean.Sum)-min(mean.proj2$A.Mean.Sum))/8
# [1] 0.5775
max(new.scale$A.Mean.Sum2)-min(new.scale$A.Mean.Sum2)
# [1] 0.5775


## H2.1 valence ----
### by predicate ----
#### plots ----
ggplot(new.scale, aes(x = reorder(verb_renamed, V.Mean.Sum2), y = V.Mean.Sum2, 
                      colour = predicateType)) +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate", 
       y = "Mean valence rating", 
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/valence-by-predicate.pdf", height = 4, width = 13)

# valence by predicate with emotive component distinction for communicatives
ggplot(new.scale, aes(x = reorder(verb_renamed, V.Mean.Sum2), y = V.Mean.Sum2, 
                      colour = predicateType2)) +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate", 
       y = "Mean valence rating", 
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = c("cognitive", "communicative with\nemotive component", 
                                                 "emotive", "evidential", 
                                                 "communicative without\nemotive component"))
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### linear model ----
lm(V.Mean.Sum2 ~ predicateType2, new.scale) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.32411    0.02572  12.602  < 2e-16 ***
# predicateType2emoComm     0.02997    0.03725   0.805 0.421427    
# predicateType2emotive     0.12034    0.03092   3.892 0.000115 ***
# predicateType2evidential -0.11612    0.03237  -3.587 0.000373 ***
# predicateType2nonEmoComm -0.12048    0.02903  -4.150 4.02e-05 ***

### by predicate type ----
#### plots ----
# calculate valence by predicateType means
mean.valence = new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Valence))
mean.valence

ggplot(mean.valence, aes(x = predicateType, y = Mean.Valence, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_colour_manual(values = cols)
ggsave("../graphs/valence-by-predicateType.pdf", height = 8, width = 10)

# calculate valence by predicateType2 means (communicatives with/without emotive component)
mean.valence2 = new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = c("communicative without\nemotive component", "evidential",
                              "cognitive", "communicative with\nemotive component", 
                              "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2.pdf", height = 8, width = 10)

### with direction ----
# calculate valence by predicateType2 means and direction of valence
mean.valence3 = new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 10

# valence by predicate type and direction of valence
#### plot ----
ggplot(mean.valence3, aes(x = predicateType2, y = Mean.Valence, colour = V.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean valence rating",
       colour = "Direction of valence") +
  scale_x_discrete(labels = c("cognitive", "communicative with \n emotive component",
                              "emotive", "evidential", 
                              "communicative without \n emotive component")) +
  scale_colour_manual(values = c("negative" = "orangered", "positive" = "seagreen3"))
ggsave("../graphs/valence-by-predicateType-and-direction.pdf", height = 8, width = 10)

#### linear model ----
lm(V.Mean.Sum2 ~ predicateType2 + V.Mean.Sum2.direction, new.scale) %>% 
  summary()
# Coefficients:
#                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.318320   0.028711  11.087  < 2e-16 ***
# predicateType2emoComm          0.033382   0.038026   0.878 0.380512    
# predicateType2emotive          0.123152   0.031558   3.902 0.000111 ***
# predicateType2evidential      -0.116573   0.032418  -3.596 0.000361 ***
# predicateType2nonEmoComm      -0.119807   0.029098  -4.117 4.61e-05 ***
# V.Mean.Sum2.directionpositive  0.008144   0.017880   0.455 0.649005  


## H2.2 arousal ----
### by predicate ----
#### plots ----
ggplot(new.scale, aes(x = reorder(verb_renamed, A.Mean.Sum2), y = A.Mean.Sum2, 
                      colour = predicateType)) +
  geom_point() +
  theme(legend.position="top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate", 
       y = "Mean arousal rating", 
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8), 
                     labels = c("completely\ncalm", "completely\nexcited")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/arousal-by-predicate.pdf", height = 4, width = 13)

# arousal by predicate with emotive component
ggplot(new.scale, aes(x = reorder(verb_renamed, A.Mean.Sum2), y = A.Mean.Sum2, 
                      colour = predicateType2)) +
  geom_point() +
  theme(legend.position="top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate", 
       y = "Mean arousal rating", 
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8), 
                     labels = c("completely\ncalm", "completely\nexcited")) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", "communicative without\nemotive component"))
ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)

#### linear models ----
lm(A.Mean.Sum2 ~ predicateType, new.scale) %>% 
  summary()
# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.374028   0.015927  23.483  < 2e-16 ***
# predicateTypecommunicative  0.027747   0.017589   1.578    0.115    
# predicateTypeemotive        0.119611   0.019150   6.246 1.02e-09 ***
# predicateTypeevidential    -0.008102   0.020048  -0.404    0.686  

lm(A.Mean.Sum2 ~ predicateType2, new.scale) %>% 
  summary()
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.374028   0.015697  23.827  < 2e-16 ***
# predicateType2emoComm     0.081826   0.022734   3.599 0.000357 ***
# predicateType2emotive     0.119611   0.018873   6.338 5.98e-10 ***
# predicateType2evidential -0.008102   0.019759  -0.410 0.681963    
# predicateType2nonEmoComm  0.014227   0.017721   0.803 0.422517 

### by predicate type ----
#### plots ----
# calculate arousal by predicateType means
mean.arousal = new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Arousal))
mean.arousal

ggplot(mean.arousal, aes(x = predicateType, y = Mean.Arousal, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
    axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean arousal rating (calm >>> excited)") +
  scale_colour_manual(values = cols)
ggsave("../graphs/arousal-by-predicateType.pdf", height = 8, width = 10)

# calculate arousal by predicateType2 means (communicatives with/without emotive component)
mean.arousal2 = new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal2

ggplot(mean.arousal2, aes(x = predicateType2, y = Mean.Arousal, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("cognitive", "evidential", "communicative without \n emotive component",
                              "communicative with \n emotive component", "emotive")) +
  labs(x = "Predicate type",
       y = "Mean arousal rating (calm >>> excited)") +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2.pdf", height = 8, width = 10)


## H2.3 valence + arousal ----
# valence against arousal: no patterns (clusters) emerge.
#### plot ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2, colour = predicateType2, 
                      fill = predicateType2)) +
  geom_point(size = 2) +
  geom_mark_hull(alpha = 0.1) +
  xlim(0, 1) + 
  ylim(0, 1) +
  theme(legend.position = "top",
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  labs(x = "Mean arousal rating", 
       y = "Mean valence rating",
       colour = "Predicate type", 
       fill = "Predicate type") +
  scale_colour_manual(values = cols2, 
                      labels=c("cognitive", "communicative with\nemotive component", 
                               "emotive", "evidential", 
                               "communicative without\nemotive component")) +
  scale_fill_manual(values = cols2, 
                    labels=c("cognitive", "communicative with\nemotive component", 
                             "emotive", "evidential", 
                             "communicative without\nemotive component"))
ggsave("../graphs/valence-by-arousal.pdf", height = 9, width = 10)

### by predicate type ----
##### plots ----
# distribution of valence and arousal ratings by predicate type
new.scale %>% 
  select(verb_renamed, predicateType, Valence = V.Mean.Sum2, Arousal = A.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Arousal), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType)) +
  geom_violin() +
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 10)) + 
  labs(y = "Mean valence / arousal rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal")) +
  scale_fill_manual(values = cols)
ggsave("../graphs/valence-arousal-by-predicateType.pdf", height = 6, width = 10)

# distribution of valence and arousal ratings by predicate type including emotive component
new.scale %>% 
  select(verb_renamed, predicateType2, Valence = V.Mean.Sum2, Arousal = A.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Arousal), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType2)) +
  geom_violin() +
  theme(legend.position = "top",
    axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 10)) + 
  labs(y = "Mean valence / arousal rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal")) +
  scale_fill_manual(values = cols2, 
                    labels = c("cognitive", "communicative with\nemotive component", 
                               "emotive", "evidential", 
                               "communicative without\nemotive component"))
ggsave("../graphs/valence-arousal-by-predicateType2.pdf", height = 6, width = 10)


## H2.4 projection: valence and arousal against projection ratings ----
### H2.4.1 valence ----
#### plots ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  xlim(0, 1) +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence.pdf", height = 6, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  xlim(0, 1) +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component"))
ggsave("../graphs/projection-by-valence.pdf", height = 6, width = 8)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  xlim(0, 1) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-faceted.pdf", height = 6, width = 10)

# projection by valence faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  xlim(0, 1) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2.pdf", height = 6, width = 12)

#### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.26078    0.02190  11.905  < 2e-16 ***
# V.Mean.Sum2  0.46441    0.06252   7.428 6.06e-13 ***

lm(Mean.Proj ~ V.Mean.Sum2 + predicateType2, data = new.scale) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.20938    0.03420   6.122 2.12e-09 ***
# V.Mean.Sum2               0.04522    0.05513   0.820   0.4126    
# predicateType2emoComm     0.18532    0.04227   4.384 1.47e-05 ***
# predicateType2emotive     0.50114    0.03569  14.043  < 2e-16 ***
# predicateType2evidential  0.06526    0.03726   1.751   0.0806 .  
# predicateType2nonEmoComm  0.06398    0.03359   1.905   0.0575 . 

lm(Mean.Proj ~ V.Mean.Sum2 * predicateType2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.2313635  0.0590803   3.916 0.000105 ***
# V.Mean.Sum2                          -0.0226087  0.1583924  -0.143 0.886565    
# predicateType2emoComm                 0.1021673  0.0861543   1.186 0.236350    
# predicateType2emotive                 0.4932607  0.0763843   6.458 2.96e-10 ***
# predicateType2evidential              0.0574599  0.0706088   0.814 0.416235    
# predicateType2nonEmoComm              0.0387308  0.0642963   0.602 0.547248    
# V.Mean.Sum2:predicateType2emoComm     0.2405746  0.2212810   1.087 0.277579    
# V.Mean.Sum2:predicateType2emotive     0.0360841  0.1871523   0.193 0.847205    
# V.Mean.Sum2:predicateType2evidential -0.0003783  0.2193152  -0.002 0.998624    
# V.Mean.Sum2:predicateType2nonEmoComm  0.0838729  0.1869497   0.449 0.653925 

##### largest residuals ----
# For which predicates does the linear model make the least accurate predictions?
residual <- resid(lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale))
cbind(new.scale, residual) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual)
#      verb_renamed predicateType2   residual
# 142         dream      cognitive -0.7429154
# 178     be fooled     evidential -0.6946395
# 96       daydream      cognitive -0.6770731
# 291       pretend     nonEmoComm -0.6715696
# 254     be misled     evidential -0.6697701
# 45    be bothered        emotive  0.6408651
# 395    be tricked     evidential -0.6402216
# 30  be astonished        emotive  0.6086928
# 166     fantasize      cognitive -0.5929909
# 177    be floored        emotive  0.5896286

# predicates grouped by predicate type
cbind(new.scale, residual) %>% 
  group_by(predicateType2) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual) %>% 
  print(n = 50)
# Groups:   predicateType2 [5]
#   verb_renamed  predicateType2 residual
#   <fct>         <chr>             <dbl>
# 1 dream         cognitive        -0.743
# 2 daydream      cognitive        -0.677
# 3 fantasize     cognitive        -0.593
# 4 imagine       cognitive        -0.585
# 5 be deluded    cognitive        -0.554
# 6 picture       cognitive        -0.528
# 7 believe       cognitive        -0.499
# 8 think         cognitive        -0.489
# 9 hope          cognitive        -0.482
# 10 forget        cognitive         0.458
# 11 joke          emoComm          -0.495
# 12 quarrel       emoComm          -0.378
# 13 cry           emoComm           0.366
# 14 cringe        emoComm           0.335
# 15 jest          emoComm          -0.333
# 16 giggle        emoComm          -0.303
# 17 curse         emoComm          -0.238
# 18 grin          emoComm          -0.236
# 19 flaunt        emoComm           0.235
# 20 bicker        emoComm          -0.215
# 21 be bothered   emotive           0.641
# 22 be astonished emotive           0.609
# 23 be floored    emotive           0.590
# 24 be startled   emotive           0.565
# 25 be offended   emotive           0.507
# 26 resent        emotive           0.506
# 27 be astounded  emotive           0.488
# 28 be stunned    emotive           0.487
# 29 be jarred     emotive           0.484
# 30 be shocked    emotive           0.478
# 31 be fooled     evidential       -0.695
# 32 be misled     evidential       -0.670
# 33 be tricked    evidential       -0.640
# 34 be promised   evidential       -0.551
# 35 be warned     evidential        0.494
# 36 be duped      evidential       -0.444
# 37 realize       evidential        0.439
# 38 spot          evidential        0.425
# 39 be informed   evidential        0.418
# 40 check         evidential       -0.407
# 41 pretend       nonEmoComm       -0.672
# 42 fake          nonEmoComm       -0.504
# 43 suggest       nonEmoComm       -0.491
# 44 feign         nonEmoComm       -0.458
# 45 apologize     nonEmoComm        0.456
# 46 reject        nonEmoComm       -0.432
# 47 lie           nonEmoComm       -0.397
# 48 claim         nonEmoComm       -0.378
# 49 promise       nonEmoComm       -0.351
# 50 sing          nonEmoComm       -0.344

#### ordinal models ----
# data frame for ordinal models
d.proj2 = droplevels(subset(d2, d2$polarity == "negative" | 
                              d2$conditional2 == "conditional")) %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg")) %>% 
  distinct(verb_renamed, verb, voice, participant, predicateType, predicateType2, 
           veridicality, veridicality_num, V.Mean.Sum2, V.Mean.Sum2.direction, 
           A.Mean.Sum2, environment)

# remove "other" and "comPriv" predicates
d.proj2 = d.proj2 %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant) + (1 | environment),
     data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant) +  
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  12817 -10002.21 20014.42 256(1285) 1.55e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6856   0.8280  
# environment (Intercept) 0.4495   0.6704  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# V.Mean.Sum2 
# 1.988 
# 
# Thresholds:
#    -1|0     0|1 
# -2.6331  0.8454 

# with predicate type
# set emotives as reference level
d.proj2 = d.proj2 %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * predicateType + (1 | participant) + 
       (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 * predicateType + 
#   (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter      max.grad
# logit flexible  12817 -9339.41 18700.82 1059(5275) 1.08e-02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7829   0.8848  
# environment (Intercept) 0.5317   0.7292  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                          V.Mean.Sum2                 predicateTypecognitive 
#                              0.11479                               -2.29976 
#           predicateTypecommunicative                predicateTypeevidential 
#                             -2.06256                               -2.03960 
#   V.Mean.Sum2:predicateTypecognitive V.Mean.Sum2:predicateTypecommunicative 
#                              0.02889                                0.74117 
#  V.Mean.Sum2:predicateTypeevidential 
#                              0.15116 
# 
# Thresholds:
#   -1|0    0|1 
# -4.913 -1.216 

#### with direction ----
# projection by valence with direction
##### plot ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm", aes(linetype = V.Mean.Sum2.direction)) +
  xlim(0, 1) +
  theme(panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-valence-with-direction.pdf", height = 6, width = 8)

##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.44404    0.01987  22.351  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.08694    0.02637  -3.297  0.00106 ** 

lm(Mean.Proj ~ V.Mean.Sum2 + V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.30372    0.02724  11.148  < 2e-16 ***
# V.Mean.Sum2                    0.44505    0.06253   7.117 4.72e-12 ***
# V.Mean.Sum2.directionpositive -0.06578    0.02513  -2.618  0.00916 ** 

lm(Mean.Proj ~ V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.27882    0.03362   8.294 1.47e-15 ***
# V.Mean.Sum2                                0.52402    0.08841   5.927 6.38e-09 ***
# V.Mean.Sum2.directionpositive             -0.01977    0.04425  -0.447    0.655    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.15782    0.12498  -1.263    0.207    

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant) + 
       (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant) + 
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  12817 -10165.16 20340.33 697(2818) 1.68e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6506   0.8066  
# environment (Intercept) 0.4298   0.6556  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# V.Mean.Sum2.directionpositive 
# -0.3851 
# 
# Thresholds:
#     -1|0      0|1 
# -3.35891  0.05278 

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * predicateType + 
       (1 | participant) + (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2.direction * predicateType +  
#   (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad
# logit flexible  12817 -9323.44 18668.88 2231(11123) 5.55e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7807   0.8836  
# environment (Intercept) 0.5298   0.7278  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# V.Mean.Sum2.directionpositive 
# 0.2351 
# predicateTypecognitive 
# -1.8422 
# predicateTypecommunicative 
# -1.7418 
# predicateTypeevidential 
# -2.4270 
# V.Mean.Sum2.directionpositive:predicateTypecognitive 
# -0.7685 
# V.Mean.Sum2.directionpositive:predicateTypecommunicative 
# -0.3941 
# V.Mean.Sum2.directionpositive:predicateTypeevidential 
# 0.3716 
# 
# Thresholds:
#   -1|0    0|1 
# -4.886 -1.183 

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction +  
#   (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter     max.grad
# logit flexible  12817 -9963.00 19940.00 518(2587) 8.67e-04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6926   0.8322  
# environment (Intercept) 0.4552   0.6747  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                               V.Mean.Sum2             V.Mean.Sum2.directionpositive 
#                                   2.30633                                  -0.08326 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive 
#                                  -0.77539 
# 
# Thresholds:
#    -1|0     0|1 
# -2.7210  0.7709

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction * predicateType + 
       (1 | participant) + (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction *  
#   predicateType + (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad
# logit flexible  12817 -9305.11 18648.22 4334(21673) 6.44e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7871   0.8872  
# environment (Intercept) 0.5338   0.7306  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# V.Mean.Sum2 
# -0.274440 
# V.Mean.Sum2.directionpositive 
# -0.269435 
# predicateTypecognitive 
# -2.117461 
# predicateTypecommunicative 
# -2.119193 
# predicateTypeevidential 
# -2.514091 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive 
# 1.180177 
# V.Mean.Sum2:predicateTypecognitive 
# 0.852998 
# V.Mean.Sum2:predicateTypecommunicative 
# 1.264739 
# V.Mean.Sum2:predicateTypeevidential 
# -0.007226 
# V.Mean.Sum2.directionpositive:predicateTypecognitive 
# -0.254990 
# V.Mean.Sum2.directionpositive:predicateTypecommunicative 
# 0.218646 
# V.Mean.Sum2.directionpositive:predicateTypeevidential 
# 0.822475 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:predicateTypecognitive 
# -1.355088 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:predicateTypecommunicative 
# -1.518771 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:predicateTypeevidential 
# -0.835773 
# 
# Thresholds:
#   -1|0    0|1 
# -5.022 -1.309 


### H2.4.2 arousal ----
# projection by arousal
#### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8), 
                     labels = c("completely\ncalm", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-arousal.pdf", height = 6, width = 6)

# projection by arousal faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8), 
                     labels = c("completely\ncalm", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType, ncol = 4)
ggsave("../graphs/projection-by-arousal-faceted.pdf", height = 6, width = 10)

# projection by arousal faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8), 
                     labels = c("completely\ncalm", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-faceted2.pdf", height = 6, width = 12)

#### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13319    0.04723    2.82  0.00502 ** 
# A.Mean.Sum2  0.63149    0.10982    5.75  1.7e-08 ***

lm(Mean.Proj ~ A.Mean.Sum2 + predicateType2, data = new.scale) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.24646    0.04464   5.521 5.91e-08 ***
# A.Mean.Sum2              -0.05996    0.09036  -0.664   0.5073    
# predicateType2emoComm     0.19158    0.04289   4.467 1.02e-05 ***
# predicateType2emotive     0.51375    0.03670  13.999  < 2e-16 ***
# predicateType2evidential  0.05952    0.03673   1.621   0.1058    
# predicateType2nonEmoComm  0.05939    0.03296   1.802   0.0723 .

lm(Mean.Proj ~ A.Mean.Sum2 * predicateType2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                           0.369642   0.123176   3.001  0.00285 **
# A.Mean.Sum2                          -0.389292   0.319972  -1.217  0.22443   
# predicateType2emoComm                -0.137042   0.179015  -0.766  0.44439   
# predicateType2emotive                 0.349429   0.146751   2.381  0.01771 * 
# predicateType2evidential              0.009649   0.156016   0.062  0.95072   
# predicateType2nonEmoComm             -0.037551   0.137295  -0.274  0.78460   
# A.Mean.Sum2:predicateType2emoComm     0.780008   0.423200   1.843  0.06602 . 
# A.Mean.Sum2:predicateType2emotive     0.412674   0.356292   1.158  0.24742   
# A.Mean.Sum2:predicateType2evidential  0.128998   0.408837   0.316  0.75252   
# A.Mean.Sum2:predicateType2nonEmoComm  0.261742   0.353883   0.740  0.45994 

#### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) +
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik    AIC      niter    max.grad
# logit flexible  12817 -10097.57 20205.14 237(985) 2.72e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6520   0.8074  
# environment (Intercept) 0.4343   0.6590  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# A.Mean.Sum2 
# 2.518 
# 
# Thresholds:
#   -1|0    0|1 
# -2.126  1.313 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * predicateType + (1 | participant) +
       (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * predicateType + 
#   (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter     max.grad
# logit flexible  12817 -9351.48 18724.97 975(4722) 2.58e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7769   0.8814  
# environment (Intercept) 0.5284   0.7269  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                           A.Mean.Sum2                 predicateTypecognitive 
#                                0.1030                                -1.7109 
#            predicateTypecommunicative                predicateTypeevidential 
#                               -1.9705                                -1.8554 
#    A.Mean.Sum2:predicateTypecognitive A.Mean.Sum2:predicateTypecommunicative 
#                               -1.5503                                 0.1705 
#   A.Mean.Sum2:predicateTypeevidential 
#                               -0.4523 
# 
# Thresholds:
#   -1|0    0|1 
# -4.903 -1.214 

### H2.4.3 valence + arousal ----
#### linear models ----
# original scale
lm(Mean.Proj ~ V.Mean.Sum + A.Mean.Sum, data = new.scale) %>% 
  summary()
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.183416   0.088044   2.083   0.0378 *  
# V.Mean.Sum  -0.018966   0.009414  -2.015   0.0446 *  
# A.Mean.Sum   0.071253   0.014201   5.018  7.7e-07 ***

# new scale
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13313    0.04564   2.917  0.00373 ** 
# V.Mean.Sum2  0.37719    0.06768   5.573 4.45e-08 ***
# A.Mean.Sum2  0.36902    0.11612   3.178  0.00159 ** 

#### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
       (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter     max.grad
# logit flexible  12817 -9970.04 19954.07 557(2765) 6.24e-04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6829   0.8264  
# environment (Intercept) 0.4519   0.6722  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#   A.Mean.Sum2             V.Mean.Sum2 A.Mean.Sum2:V.Mean.Sum2 
#        0.8755                  0.9661                  1.5885 
# 
# Thresholds:
#   -1|0    0|1 
# -2.373  1.116  

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * predicateType +
       (1 | participant) + (1 | environment), data = d.proj2)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * predicateType +  
#   (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad
# logit flexible  12817 -9323.78 18685.57 4376(21845) 3.76e-04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7891   0.8883  
# environment (Intercept) 0.5337   0.7305  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                      A.Mean.Sum2                                        V.Mean.Sum2 
#                                          -1.7379                                            -1.7131 
#                           predicateTypecognitive                         predicateTypecommunicative 
#                                          -4.6417                                            -2.6759 
#                          predicateTypeevidential                            A.Mean.Sum2:V.Mean.Sum2 
#                                          -2.5772                                             3.7650 
#               A.Mean.Sum2:predicateTypecognitive             A.Mean.Sum2:predicateTypecommunicative 
#                                           5.8393                                             1.1949 
#              A.Mean.Sum2:predicateTypeevidential                 V.Mean.Sum2:predicateTypecognitive 
#                                           0.9035                                             7.6438 
#           V.Mean.Sum2:predicateTypecommunicative                V.Mean.Sum2:predicateTypeevidential 
#                                           2.1078                                             1.5716 
#   A.Mean.Sum2:V.Mean.Sum2:predicateTypecognitive A.Mean.Sum2:V.Mean.Sum2:predicateTypecommunicative 
#                                         -19.0356                                            -2.6265 
#  A.Mean.Sum2:V.Mean.Sum2:predicateTypeevidential 
#                                          -2.5030 
# 
# Thresholds:
#   -1|0    0|1 
# -5.743 -2.039 


## possible issues with the analysis ----

# predicates in both active and passive sentence frames with same predicate type
d3[duplicated(d3[,cbind(2,4)]),] %>%
  select(verb, predicateType)

#       verb predicateType
# 195 grieve       emotive
# 251 marvel       emotive
# 276  panic       emotive
# 427  worry       emotive

# Of the 101 emotives with valence/arousal/dominance ratings, four predicates occur in both
# sentence frames in the MV data set. The "active" and "passive voice" versions of these 
# predicates were assigned the same valence/arousal/dominance ratings. Due to the small number
# of these cases, this should not affect the overall distributions plotted above.


# H3: dynamic/activity/CoS predicates and projection----
## H3.1: dynamicity ----

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

# create predicate type, emotive component, dynamicity, CoS, state/activity/CoS columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         dynamicity = case_when(stative_predicate == "yes" ~ "stative",
                                dynamic_predicate == "yes" ~ "dynamic",
                                TRUE ~ "unclear"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))

### by predicate type ----

# # how many predicates in which predicateType and with which dynamicity?
d.proj %>%
  select(predicateType, verb, dynamicity) %>%
  unique() %>%
  group_by(predicateType, dynamicity) %>%
  summarize(count=n())

mean.proj = d.proj %>%
  group_by(predicateType, dynamicity) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 11

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 6

#### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, colour = dynamicity)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating", 
       colour = "Dynamicity") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = c("dynamic" = "violetred3", "stative" = "aquamarine4"))
ggsave("../graphs/projection-by-predicateType-and-Dynamicity.pdf", height = 6, width = 9)


### by predicate ---- 
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) #544
levels(mean.proj$verb_renamed)

# add verb and predicateType to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, dynamicity)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, dynamicity)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) #544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) #523

#### plots ----
# projection by predicate with dynamic predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_point(data = mean.proj %>% filter(dynamicity == "dynamic"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = dynamicity), 
             size = 4,  shape = 1, colour = "violetred3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("dynamic predicate")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-dynamic.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
  geom_point(data = mean.proj %>% filter(dynamicity == "dynamic"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = dynamicity), 
             size = 4,  shape = 1, colour = "violetred3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("dynamic predicate")) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType) 
ggsave("../graphs/projection-by-predicate-dynamic-faceted.pdf", height = 6, width = 6)

# projection by predicate with stative predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_point(data = mean.proj %>% filter(dynamicity == "stative"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = dynamicity), 
             size = 4,  shape = 1, colour = "aquamarine4", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("stative predicate")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-stative.pdf", height = 4, width = 13)

#### linear models ----
lm(Mean.Proj ~ dynamicity, data = mean.proj) %>%  
  summary()
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.29756    0.01270   23.44   <2e-16 ***
# dynamicitystative  0.31777    0.02095   15.16   <2e-16 ***

lm(Mean.Proj ~ dynamicity + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.17310    0.04455   3.886 0.000115 ***
# dynamicitystative           0.06286    0.04827   1.302 0.193381    
# predicateTypecommunicative  0.13481    0.04635   2.908 0.003789 ** 
# predicateTypeemotive        0.48668    0.03457  14.077  < 2e-16 ***
# predicateTypeevidential     0.11582    0.04658   2.486 0.013216 *  

lm(Mean.Proj ~ dynamicity * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   0.14310    0.05255   2.723  0.00668 ** 
# dynamicitystative                             0.10421    0.06170   1.689  0.09181 .  
# predicateTypecommunicative                    0.16481    0.05409   3.047  0.00243 ** 
# predicateTypeemotive                          0.47533    0.03614  13.152  < 2e-16 ***
# predicateTypeevidential                       0.15101    0.05691   2.653  0.00821 ** 
# dynamicitystative:predicateTypecommunicative       NA         NA      NA       NA    
# dynamicitystative:predicateTypeemotive             NA         NA      NA       NA    
# dynamicitystative:predicateTypeevidential    -0.10653    0.09903  -1.076  0.28255  

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ dynamicity + (1 | participant) + (1 | environment), 
     data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ dynamicity + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -11833.45 23676.90 349(1749) 3.15e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6966   0.8346  
# environment (Intercept) 0.4338   0.6587  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#   dynamicitystative 
#               1.312 
# 
# Thresholds:
#   -1|0    0|1 
# -2.859  0.657

# with predicate type
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ dynamicity * predicateType + (1 | participant) +
       (1 | environment), data = d.proj)
# design is column rank deficient so dropping 2 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ dynamicity * predicateType + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -11338.67 22695.33 846(4234) 8.45e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7765   0.8812  
# environment (Intercept) 0.4844   0.6960  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                        dynamicitystative                   predicateTypecognitive 
#                                 -0.06604                                 -2.59982 
#               predicateTypecommunicative                  predicateTypeevidential 
#                                 -1.94137                                 -2.01538 
# dynamicitystative:predicateTypecognitive 
#                                  0.43691 
# 
# Thresholds:
#   -1|0    0|1 
# -4.940 -1.306 

# with predicate type with emotive component distinction
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ dynamicity * predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj)
# design is column rank deficient so dropping 3 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ dynamicity * predicateType2 + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -11302.32 22624.65 1169(5850) 5.46e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7770   0.8815  
# environment (Intercept) 0.4884   0.6989  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#         dynamicitystative                   predicateType2cognitive 
#                  -0.07058                                  -2.60753 
#     predicateType2emoComm                  predicateType2evidential 
#                  -1.51483                                  -2.02081 
#  predicateType2nonEmoComm dynamicitystative:predicateType2cognitive 
#                  -2.05478                                   0.44300 
# 
# Thresholds:
#   -1|0    0|1 
# -4.957 -1.309 


## H3.2 change of state ----
# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType, emotiveComponent, change-of-state, environment columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))

### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType, changeOfState) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 8

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 6

##### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, colour = changeOfState)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Change-of-state predicate") +
  scale_colour_manual(values = c("no" = "darkblue", "yes" = "gold3"))
ggsave("../graphs/projection-by-predicateType-and-CoS.pdf", height = 6, width = 9)

### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) # 544
levels(mean.proj$verb_renamed)

# add verb and predicateType to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, changeOfState)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, changeOfState)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

#### plots -----
# projection by predicate with change-of-state predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_point(data = mean.proj %>% filter(changeOfState == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = changeOfState), 
             size = 4,  shape = 1, colour = "gold3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("change-of-state predicate")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-CoS.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
  geom_point(data = mean.proj %>% filter(changeOfState == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = changeOfState), 
             size = 4,  shape = 1, colour = "gold3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("change-of-state predicate")) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType) 
ggsave("../graphs/projection-by-predicate-CoS-faceted.pdf", height = 6, width = 6)

#### linear models ----
lm(Mean.Proj ~ changeOfState, data = mean.proj) %>%  
  summary()
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.42120    0.01258  33.471   <2e-16 ***
# changeOfStateyes -0.08904    0.04494  -1.981   0.0481 * 

lm(Mean.Proj ~ changeOfState + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.21288    0.02766   7.697 7.12e-14 ***
# changeOfStateyes            0.07435    0.03916   1.899  0.05817 .  
# predicateTypecommunicative  0.09503    0.03047   3.119  0.00191 ** 
# predicateTypeemotive        0.50976    0.03202  15.920  < 2e-16 ***
# predicateTypeevidential     0.04979    0.03705   1.344  0.17958  

lm(Mean.Proj ~ changeOfState * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (2 not defined because of singularities)
#                                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                  0.18909    0.02837   6.664 6.83e-11 ***
# changeOfStateyes                             0.37757    0.10132   3.727 0.000215 ***
# predicateTypecommunicative                   0.11882    0.03107   3.824 0.000147 ***
# predicateTypeemotive                         0.53355    0.03257  16.382  < 2e-16 ***
# predicateTypeevidential                      0.09549    0.03933   2.428 0.015530 *  
# changeOfStateyes:predicateTypecommunicative       NA         NA      NA       NA    
# changeOfStateyes:predicateTypeemotive             NA         NA      NA       NA    
# changeOfStateyes:predicateTypeevidential    -0.35535    0.10968  -3.240 0.001272 **  

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ changeOfState + (1 | participant) + (1 | environment), 
     data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation

# formula: as.factor(veridicality_num) ~ changeOfState + (1 | participant) +  
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -12469.05 24948.11 249(1243) 1.54e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6208   0.7879  
# environment (Intercept) 0.3816   0.6177  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#  changeOfStateyes 
#           -0.3673 
# 
# Thresholds:
#    -1|0     0|1 
# -3.1684  0.1523 

# with predicate type
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ changeOfState * predicateType + (1 | participant) + 
       (1 | environment), data = d.proj)
# design is column rank deficient so dropping 2 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ changeOfState * predicateType +  
#   (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -11311.51 22641.02 852(4255) 4.56e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7835   0.8852  
# environment (Intercept) 0.4880   0.6986  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                         changeOfStateyes                  predicateTypecognitive 
#                                  0.06689                                -2.38912 
#               predicateTypecommunicative                 predicateTypeevidential 
#                                 -1.87847                                -1.98605 
#  changeOfStateyes:predicateTypecognitive 
#                                  1.56224 
# 
# Thresholds:
#   -1|0    0|1 
# -4.885 -1.241

# with predicate type with emotive component distinction
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ changeOfState * predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj)
# design is column rank deficient so dropping 3 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ changeOfState * predicateType2 +  
#   (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -11275.36 22570.73 970(4844) 3.68e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7837   0.8853  
# environment (Intercept) 0.4920   0.7014  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#         changeOfStateyes                  predicateType2cognitive 
#                  0.06689                                 -2.39112 
#    predicateType2emoComm                 predicateType2evidential 
#                 -1.44825                                 -1.98728 
# predicateType2nonEmoComm changeOfStateyes:predicateType2cognitive 
#                 -1.98713                                  1.55916 
# 
# Thresholds:
#   -1|0    0|1 
# -4.897 -1.239 


## H3.3: activity ----

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

# create predicateType, emotiveComponent, activity, environment columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
                              TRUE ~ "no"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))


### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType, activity) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 11

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 7

#### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, colour = activity)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Activity") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = c("no" = "slateblue", "yes" = "red3"))
ggsave("../graphs/projection-by-predicateType-and-activity.pdf", height = 6, width = 9)

### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) # 544
levels(mean.proj$verb_renamed)

# add verb and predicateType to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, activity)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, activity)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

#### plots ----
# projection by predicate with activity predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_point(data = mean.proj %>% filter(activity == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = activity), 
             size = 4,  shape = 1, colour = "red3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Predicate type") +
  scale_fill_discrete(name = element_blank(), labels = c("activity")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-activity.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
  geom_point(data = mean.proj %>% filter(activity == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = activity), 
             size = 4,  shape = 1, colour = "red3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  labs(y = "Mean projection rating", 
       x = "Predicate") + 
  scale_fill_discrete(name = element_blank(), labels = c("activity")) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType) 
ggsave("../graphs/projection-by-predicate-activity-faceted.pdf", height = 6, width = 6)

#### linear models ----
lm(Mean.Proj ~ activity, data = mean.proj) %>%  
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.46198    0.01413  32.702  < 2e-16 ***
# activityyes -0.15325    0.02531  -6.056 2.67e-09 ***

lm(Mean.Proj ~ activity + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.21450    0.02807   7.642 1.04e-13 ***
# activityyes                 0.01789    0.02248   0.796   0.4264    
# predicateTypecommunicative  0.08287    0.03142   2.638   0.0086 ** 
# predicateTypeemotive        0.50814    0.03240  15.684  < 2e-16 ***
# predicateTypeevidential     0.07699    0.03472   2.218   0.0270 *   

lm(Mean.Proj ~ activity * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (1 not defined because of singularities)
#                                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                             0.278220   0.030959   8.987  < 2e-16 ***
# activityyes                            -0.252933   0.063823  -3.963 8.44e-05 ***
# predicateTypecommunicative              0.001991   0.036658   0.054    0.957    
# predicateTypeemotive                    0.444419   0.034799  12.771  < 2e-16 ***
# predicateTypeevidential                 0.002467   0.038082   0.065    0.948    
# activityyes:predicateTypecommunicative  0.299960   0.068758   4.363 1.55e-05 ***
# activityyes:predicateTypeemotive              NA         NA      NA       NA    
# activityyes:predicateTypeevidential     0.350024   0.087636   3.994 7.44e-05 *** 

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ activity + (1 | participant) + (1 | environment), 
     data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ activity + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -12348.98 24707.97 288(1445) 6.14e-04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6339   0.7962  
# environment (Intercept) 0.3889   0.6237  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#   activityyes 
#       -0.5989 
# 
# Thresholds:
#      -1|0       0|1 
# -3.368996 -0.007945 

# with predicate type
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ activity * predicateType + (1 | participant) + 
       (1 | environment), data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ activity * predicateType + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -11306.23 22632.47 1068(5323) 6.08e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7815   0.8840  
# environment (Intercept) 0.4866   0.6976  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                        activityyes                 predicateTypecognitive 
#                             0.2923                                -2.0389 
#         predicateTypecommunicative                predicateTypeevidential 
#                            -1.9786                                -1.9996 
# activityyes:predicateTypecognitive activityyes:predicateTypecommunicative 
#                            -1.2720                                -0.1214 
# 
# Thresholds:
#   -1|0    0|1 
# -4.892 -1.240 

# with predicate type with emotive component distinction
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ activity * predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ activity * predicateType2 + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -11275.35 22574.71 1178(5882) 7.02e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7826   0.8846  
# environment (Intercept) 0.4901   0.7001  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                         activityyes              predicateType2cognitive                predicateType2emoComm 
#                             0.02598                             -2.04032                             -1.38758 
#            predicateType2evidential             predicateType2nonEmoComm  activityyes:predicateType2cognitive 
#                            -1.99994                             -2.00045                             -1.00777 
#   activityyes:predicateType2emoComm activityyes:predicateType2evidential 
#                            -0.08907                              0.26513 
# 
# Thresholds:
#   -1|0    0|1 
# -4.902 -1.239 

## H3.4 dynamic/activity/CoS comparison ----
# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

# create predicateType, emotiveComponent, activity, environment columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         stateActivityCoS = case_when(stative_predicate == "yes" ~ "state",
                                      change_of_state_predicate == "yes" ~ "change-of-state predicate",
                                      activity_predicate == "yes" ~ "activity",
                                      TRUE ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))


### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType, stateActivityCoS) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 16

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 10

#### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, 
                      colour = fct_relevel(stateActivityCoS, 
                                           "activity", "change-of-state predicate",
                                           "state", "other"))) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
    theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Type of predicate") +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"))
ggsave("../graphs/projection-by-predicateType-and-stateActivityCoS.pdf", height = 6, width = 9)

### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) # 544
levels(mean.proj$verb_renamed)

# add verb and predicateType to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, stateActivityCoS)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, stateActivityCoS)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

#### plots ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = stateActivityCoS)) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Type of predicate") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"),
                      breaks = c("activity", "change-of-state predicate", "state", "other"))
ggsave("../graphs/projection-by-predicate-stateActivityCoS.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, 
                                   colour = stateActivityCoS), show.legend = FALSE) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"), 
                      breaks = c("activity", "change-of-state predicate", "state", "other")) +
  facet_wrap(~ fct_relevel(stateActivityCoS, "activity", "change-of-state predicate",
                           "state", "other")) 
ggsave("../graphs/projection-by-predicate-stateActivityCoS-faceted.pdf", height = 6, width = 6)


#### linear models ----
lm(Mean.Proj ~ stateActivityCoS, data = mean.proj) %>%  
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.30873    0.01809  17.068   <2e-16 ***
# stateActivityCoSchange-of-state predicate  0.01807    0.04160   0.434    0.664    
# stateActivityCoSother                     -0.03372    0.02716  -1.242    0.215    
# stateActivityCoSstate                      0.30660    0.02460  12.466   <2e-16 ***

lm(Mean.Proj ~ stateActivityCoS + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.16572    0.04581   3.618 0.000327 ***
# stateActivityCoSchange-of-state predicate  0.05143    0.04440   1.158 0.247252    
# stateActivityCoSother                     -0.03776    0.02372  -1.592 0.112001    
# stateActivityCoSstate                      0.07026    0.04990   1.408 0.159713    
# predicateTypecommunicative                 0.15771    0.04728   3.336 0.000912 ***
# predicateTypeemotive                       0.48666    0.03450  14.105  < 2e-16 ***
# predicateTypeevidential                    0.11574    0.04813   2.405 0.016527 * 

lm(Mean.Proj ~ stateActivityCoS * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (6 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                           0.02529    0.05493   0.460 0.645438    
# stateActivityCoSchange-of-state predicate                             0.82471    0.14532   5.675 2.32e-08 ***
# stateActivityCoSother                                                -0.11804    0.06414  -1.840 0.066293 .  
# stateActivityCoSstate                                                 0.22203    0.06321   3.513 0.000483 ***
# predicateTypecommunicative                                            0.30195    0.05725   5.274 1.97e-07 ***
# predicateTypeemotive                                                  0.47533    0.03497  13.591  < 2e-16 ***
# predicateTypeevidential                                               0.35249    0.07768   4.538 7.09e-06 ***
# stateActivityCoSchange-of-state predicate:predicateTypecommunicative       NA         NA      NA       NA    
# stateActivityCoSother:predicateTypecommunicative                      0.07101    0.06890   1.031 0.303200    
# stateActivityCoSstate:predicateTypecommunicative                           NA         NA      NA       NA    
# stateActivityCoSchange-of-state predicate:predicateTypeemotive             NA         NA      NA       NA    
# stateActivityCoSother:predicateTypeemotive                                 NA         NA      NA       NA    
# stateActivityCoSstate:predicateTypeemotive                                 NA         NA      NA       NA    
# stateActivityCoSchange-of-state predicate:predicateTypeevidential    -0.90476    0.15856  -5.706 1.96e-08 ***
# stateActivityCoSother:predicateTypeevidential                              NA         NA      NA       NA    
# stateActivityCoSstate:predicateTypeevidential                        -0.30801    0.11038  -2.790 0.005460 ** 

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ stateActivityCoS + (1 | participant) + (1 | environment), 
     data = d.proj)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ stateActivityCoS + (1 | participant) + 
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -11829.72 23673.44 1817(9089) 5.76e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6953   0.8338  
# environment (Intercept) 0.4338   0.6587  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#   stateActivityCoSchange-of-state predicate                     stateActivityCoSother 
#                                     0.04006                                  -0.10736 
#                       stateActivityCoSstate 
#                                     1.27397 
# 
# Thresholds:
#    -1|0     0|1 
# -2.8975  0.6196 

# with predicate type
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ stateActivityCoS * predicateType + (1 | participant) + 
       (1 | environment), data = d.proj)
# design is column rank deficient so dropping 6 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ stateActivityCoS * predicateType + 
#   (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -11263.34 22552.69 1705(8529) 5.18e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7897   0.8887  
# environment (Intercept) 0.4917   0.7012  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# stateActivityCoSchange-of-state predicate 
# -0.2607 
# stateActivityCoSother 
# -0.3385 
# stateActivityCoSstate 
# -0.3257 
# predicateTypecognitive 
# -3.3513 
# predicateTypecommunicative 
# -2.1377 
# predicateTypeevidential 
# -2.0295 
# stateActivityCoSchange-of-state predicate:predicateTypecognitive 
# 4.0798 
# stateActivityCoSstate:predicateTypecognitive 
# 1.1840 
# stateActivityCoSother:predicateTypecommunicative 
# 0.1685 
# 
# Thresholds:
#   -1|0    0|1 
# -5.232 -1.567

# with predicate type with emotive component distinction
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ stateActivityCoS * predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj)
# design is column rank deficient so dropping 8 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ stateActivityCoS * predicateType2 +  
#   (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -11232.51 22495.02 1878(9395) 7.82e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7907   0.8892  
# environment (Intercept) 0.4951   0.7036  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
# stateActivityCoSchange-of-state predicate 
# -0.25927 
# stateActivityCoSother 
# -0.02492 
# stateActivityCoSstate 
# -0.32841 
# predicateType2cognitive 
# -3.35767 
# predicateType2emoComm 
# -1.78334 
# predicateType2evidential 
# -2.03365 
# predicateType2nonEmoComm 
# -2.30739 
# stateActivityCoSchange-of-state predicate:predicateType2cognitive 
# 4.07932 
# stateActivityCoSstate:predicateType2cognitive 
# 1.18881 
# stateActivityCoSother:predicateType2emoComm 
# 0.08842 
# stateActivityCoSother:predicateType2evidential 
# -0.31195 
# 
# Thresholds:
#   -1|0    0|1 
# -5.245 -1.568 


# H4: veridicality and projection ----
## H4.1 general ----
# positive correlation of veridicality and projection ratings  

# create datasets for projection and veridicality inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291
d.verid = droplevels(subset(d, d$polarity != "negative" & d$conditional2 != "conditional"))
nrow(d.verid) # 5401

# create columns for predicate type, emotive component, projectivity, change-of-state, environment
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         canonicallyProjective = case_when(canonically_projective == "yes" ~ "yes", 
                                           TRUE ~ "no"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg")) 

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) # 544
levels(mean.proj$verb_renamed)

# add verb and predicateType to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, canonicallyProjective)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, canonicallyProjective)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# calculate by-predicate veridicality means 
mean.verid = d.verid %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Verid = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Verid = Mean.Verid - CILow, YMax.Verid = Mean.Verid + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Verid))
mean.verid
nrow(mean.verid) # 544
levels(mean.verid$verb_renamed)

mean.proj.verid = mean.proj %>%
  inner_join(mean.verid, by = ("verb_renamed"))
mean.proj.verid

# remove "other" and "comPriv" predicates
mean.proj.verid = mean.proj.verid %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.verid) #523

# Mean projection by mean veridicality
### plots ----
ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.1) +
  geom_errorbarh(aes(xmin = YMin.Verid, xmax = YMax.Verid), alpha = 0.3, colour = "grey") +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), alpha = 0.3, colour ="grey") +
  geom_point(size = 0.8, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Mean veridicality rating",
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality-with-CIs.pdf", height = 6, width = 6)

ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality.pdf", height = 6, width = 6)

### linear models ----
lm(Mean.Proj ~ Mean.Verid, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.10027    0.01622   6.183 1.27e-09 ***
# Mean.Verid   0.51962    0.02278  22.811  < 2e-16 ***

lm(Mean.Proj ~ Mean.Verid + predicateType2, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.11604    0.02050   5.660 2.51e-08 ***
# Mean.Verid                0.38407    0.01835  20.926  < 2e-16 ***
# predicateType2emoComm     0.04015    0.02982   1.347   0.1787    
# predicateType2emotive     0.29318    0.02518  11.642  < 2e-16 ***
# predicateType2evidential -0.02098    0.02544  -0.825   0.4098    
# predicateType2nonEmoComm -0.04679    0.02300  -2.034   0.0425 *  

lm(Mean.Proj ~ Mean.Verid * predicateType2, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          6.720e-02  2.306e-02   2.914  0.00373 ** 
# Mean.Verid                           5.668e-01  4.651e-02  12.186  < 2e-16 ***
# predicateType2emoComm                2.573e-02  6.282e-02   0.409  0.68236    
# predicateType2emotive                3.768e-01  4.870e-02   7.738 5.39e-14 ***
# predicateType2evidential            -6.697e-05  3.367e-02  -0.002  0.99841    
# predicateType2nonEmoComm             4.884e-02  2.909e-02   1.679  0.09373 .  
# Mean.Verid:predicateType2emoComm    -9.229e-02  9.111e-02  -1.013  0.31157    
# Mean.Verid:predicateType2emotive    -2.254e-01  6.878e-02  -3.277  0.00112 ** 
# Mean.Verid:predicateType2evidential -1.288e-01  5.994e-02  -2.148  0.03216 *  
# Mean.Verid:predicateType2nonEmoComm -2.685e-01  5.363e-02  -5.006 7.67e-07 ***


### ordinal models ----
d.proj.mean.verid = left_join(d.proj, mean.verid, by = c("verb_renamed")) %>%
  distinct()
nrow(d.proj.mean.verid) # 16291

# remove "other" and "comPriv" predicates
d.proj.mean.verid = d.proj.mean.verid %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(d.proj.mean.verid) # 15662

clmm(as.factor(veridicality_num) ~ Mean.Verid + (1 | participant) + (1 | environment), 
     data = d.proj.mean.verid)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ Mean.Verid + (1 | participant) + (1 | environment)
# data:    d.proj.mean.verid
# 
# link  threshold nobs  logLik    AIC      niter     max.grad
# logit flexible  15662 -11471.23 22952.46 312(1565) 4.46e-04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7168   0.8467  
# environment (Intercept) 0.4389   0.6625  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#  Mean.Verid 
#       2.177 
# 
# Thresholds:
#   -1|0    0|1 
# -2.151  1.535 

# with predicate type
# set emotives as reference level
d.proj.mean.verid = d.proj.mean.verid %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ Mean.Verid * predicateType + (1 | participant) +
       (1 | environment), data = d.proj.mean.verid)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ Mean.Verid * predicateType + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj.mean.verid
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -10830.87 21683.74 1834(9175) 9.11e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.8388   0.9158  
# environment (Intercept) 0.5184   0.7200  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                          Mean.Verid                predicateTypecognitive 
#                              1.9710                               -1.4094 
#          predicateTypecommunicative               predicateTypeevidential 
#                             -1.1555                               -1.3837 
#   Mean.Verid:predicateTypecognitive Mean.Verid:predicateTypecommunicative 
#                              0.4451                               -0.5961 
#  Mean.Verid:predicateTypeevidential 
#                             -0.1916 
# 
# Thresholds:
#    -1|0     0|1 
# -3.5283  0.3261 

# with predicate type with emotive component distinction
# set emotives as reference level
d.proj.mean.verid = d.proj.mean.verid %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ Mean.Verid * predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj.mean.verid)
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ Mean.Verid * predicateType2 + (1 | participant) +
#   (1 | environment)
# data:    d.proj.mean.verid
# 
# link  threshold nobs  logLik    AIC      niter      max.grad
# logit flexible  15662 -10810.30 21646.60 1920(9605) 2.55e-03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.8387   0.9158  
# environment (Intercept) 0.5219   0.7224  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                       Mean.Verid             predicateType2cognitive               predicateType2emoComm 
#                          1.96870                            -1.41271                            -1.20920 
#         predicateType2evidential            predicateType2nonEmoComm  Mean.Verid:predicateType2cognitive 
#                         -1.38672                            -1.15359                             0.44972 
# Mean.Verid:predicateType2emoComm Mean.Verid:predicateType2evidential Mean.Verid:predicateType2nonEmoComm 
#                         -0.09059                            -0.18650                            -0.73468 
# 
# Thresholds:
#    -1|0     0|1 
# -3.5351  0.3248 

## H4.2 with "factives" ----
# mean projection by mean veridicality with canonically projective predicates highlighted
### plot ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(data = mean.proj.verid %>% 
               filter(canonicallyProjective == "yes"),
             aes(x = Mean.Verid, y = Mean.Proj, fill = "canonically projective predicate"), 
             size = 3, colour = "deeppink") +
  geom_point(data = mean.proj.verid, 
             aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType), alpha = 0.7) +
  geom_smooth(data = mean.proj.verid, 
              aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType), 
              method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 9, hjust = 0)) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type",
       fill = element_blank()) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality-with-factives.pdf", height = 6, width = 9)

## H4.3 veridical non-projective predicates ----
# What are the predicates with negative mean projection rating and positive mean veridicality rating?
### plot ----
ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType), alpha = 0.7) +
  geom_label_repel(data = subset(mean.proj.verid, Mean.Verid > 0 & Mean.Proj < 0),
                   aes(label = verb_renamed), 
                   min.segment.length = 0) +
  geom_smooth(method = "lm", linewidth = 0.5) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols) +
  coord_fixed(ratio = 1) + 
  facet_wrap(~predicateType)
ggsave("../graphs/projection-by-veridicality2.pdf", height = 6, width = 6)







#  end of script for now ------------------------------------------------------

tmp = mean.proj %>% 
  left_join(mean.verid)
View(tmp)

ggplot(tmp, aes(x=Mean.Verid, y=Mean.Proj)) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  #scale_color_manual(values = colors) +
  #geom_line(aes(color=type), size=.5) + 
  geom_hline(yintercept=.3) +
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/H1.pdf",height=4,width=13)

# create data relevant to investigate projection (embedding is negation, conditional or both)
d_tmp <- droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
t <- table(d_tmp$verb)
min(t)
mean(t)
max(t) #29-60 ratings per predicate under negation, cond or both
length(unique(d_tmp$verb)) #517 verbs
length(unique(d_tmp$participant)) #290 participants gave ratings

table(d_tmp$polarity, d_tmp$conditional2)

d_tmp$embedding = case_when(d_tmp$polarity == "positive" & d_tmp$conditional2 == "conditional" ~ "condPos",
                              d_tmp$polarity == "negative" & d_tmp$conditional2 == "conditional" ~ "condNeg",
                              d_tmp$polarity == "negative" & d_tmp$conditional2 == "matrix" ~ "matrixNeg",
                              TRUE ~ "error")

table(d_tmp$embedding)

# calculate mean for all 517 predicates, and embedding
p_means = d_tmp %>%
  group_by(verb, embedding) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
levels(p_means$verb) # verbs sorted by projectivity mean 

# create data subsets for the factives
p_meansFactives <- droplevels(subset(p_means,p_means$verb %in% factives))
p_meansFactives
str(p_meansFactives$verb)
levels(p_meansFactives$verb) # sorted by projectivity mean (pretend...be annoyed)

p_means <- mutate(p_means, verb = fct_reorder(verb, Mean, .fun = mean))
p_means <- mutate(p_means, embedding = fct_reorder(embedding, Mean, .fun = mean))
p_meansFactives <- mutate(p_meansFactives, verb = fct_reorder(verb, Mean, .fun = mean))
p_meansFactives <- mutate(p_meansFactives, embedding = fct_reorder(embedding, Mean, .fun = mean))

# Color blind friendly palette with black (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(p_meansFactives,aes(x=fct_reorder(verb, Mean), y=Mean, group = embedding, fill = embedding, color = embedding)) +
coord_cartesian(ylim=c(-1,1)) +
geom_point(aes(shape = embedding), size = 3) + 
scale_shape_manual(values=rev(c(23, 24, 25)),
                     labels=rev(c("cond/polar","negation","cond/neg/polar")),
                     name="Embedding") +
scale_fill_manual(values=rev(c("#000000", "#E69F00", "#56B4E9")),
                  labels=rev(c("cond/polar","negation","cond/neg/polar")),
                   name="Embedding") +
scale_colour_manual(values=rev(c("#000000", "#E69F00", "#56B4E9"))) +
#geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
geom_line() +
guides(color = "none") +
#geom_text_repel(data=p_meansFactives,aes(x=verb,y=Mean,label=verb,
                                           #color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.6) +
#scale_color_manual(values=rev(c("tomato","black"))) +
scale_y_continuous(limits = c(-1,2),breaks = c(-1,0,1,2)) +
ylab("Mean projection rating") +
xlab("Predicate")
ggsave("../graphs/proj-by-both.pdf",height=4.7,width=12)
