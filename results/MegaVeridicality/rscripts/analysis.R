# Testing hypotheses based on 
# White & Rawlins' MegaVeridicality I dataset 
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

# load data
d = read.csv("../data/d.csv")
nrow(d) #21692
names(d)

# load predicate coding
y = read.csv("../data/predicate-coding.csv")
nrow(y) #544
names(y)

d = left_join(d,y,by=c("verb","voice"))
nrow(d) #21692


# H1: The lexical meaning of the predicate matters ----
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
                                 TRUE ~ "other"))

table(d.proj$predicateType)

# how many predicates in which type and which voice?
d.proj %>%
  select(predicateType, verb, voice) %>% 
  unique() %>% 
  group_by(predicateType,voice) %>% 
  summarize(count=n())

# color code the predicates
# cols = d.proj %>%
#   select(c(verb_renamed,predicateType)) %>%
#   distinct(verb_renamed,predicateType)
# cols
# nrow(cols) #544

# color-code the predicates
# cols$Colors =  ifelse(cols$predicateType == "comPriv", "pink",
#                       ifelse(cols$predicateType == "emotive", "#D55E00", 
#                           ifelse(cols$predicateType == "cognitive", "#5b43c4", 
#                              ifelse(cols$predicateType == "communicative", "gray", 
#                                     ifelse(cols$predicateType == "inferential", "green", "black")))))
# cols

## by-predicateType ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 6
levels(mean.proj$predicateType)

ggplot(mean.proj, aes(x=predicateType, y=Mean.Proj)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-by-predicateType.pdf",height=4,width=5)

## by-predicate ----
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

# cols$verb_renamed = factor(cols$verb_renamed, levels = mean.proj$verb_renamed[order(mean.proj$Mean.Proj)], ordered = TRUE)

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) #524

ggplot(mean.proj, aes(x=verb_renamed, y=Mean.Proj, fill = predicateType, color = predicateType)) +
  geom_point() +
  #geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  #theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-by-predicatel.pdf",height=4,width=13)

ggplot(mean.proj, aes(x=verb_renamed, y=Mean.Proj, fill = predicateType, color = predicateType)) +
  geom_point() +
  #geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  facet_grid(. ~ predicateType) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-faceted-by-predicatel.pdf",height=4,width=13)

### lm ----
lm(Mean.Proj ~ predicateType, data = mean.proj) %>% 
  summary()
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.24612    0.02020  12.183  < 2e-16 ***
# predicateTypecommunicative  0.06204    0.02391   2.595  0.00974 ** 
# predicateTypeemotive        0.47652    0.02589  18.408  < 2e-16 ***
# predicateTypeevidential     0.06156    0.03591   1.714  0.08705 .

# H1a: emotive component ----
# Among the communication predicates, does the CC of the ones that have an emotive meaning component
# project more than the CCs of those that don't?

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
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
                                      TRUE ~ "no")) 

## by-predicateType ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType, emotiveComponent) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) #7
levels(mean.proj$predicateType)

ggplot(mean.proj, aes(x=predicateType, y=Mean.Proj, color = emotiveComponent)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       color ="Emotive component")
ggsave("../graphs/projection-by-predicateType-emotiveComponent.pdf",height = 8, width = 12)

## by-predicate ----
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
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

# only communicative predicates
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) #237

ggplot(mean.proj, aes(x=verb_renamed, y=Mean.Proj, color = emotiveComponent)) +
  geom_point() +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       color ="Emotive component") +
  facet_wrap( ~ emotiveComponent)
ggsave("../graphs/projection-by-communicative-predicate.pdf", height = 4, width = 13)

lm(Mean.Proj ~ emotiveComponent, data = mean.proj) %>% 
  summary()
### lm ----
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.27927    0.01265  22.069  < 2e-16 ***
# emotiveComponentyes  0.14568    0.02842   5.126 6.17e-07 ***


# H1b: valence and arousal ----

# load valence and arousal data
z = read.csv("../data/BRM-emot-submit.csv")
nrow(z) #13915

d2 = z %>%
  filter(Word %in% d$verb) %>%
  rename(verb = Word) %>%
  inner_join(d, by = "verb")
nrow(d2) #17747
n_distinct(d2$verb) #423

# create predicateType and emotiveComponent columns
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
d2 = d2 %>%
  filter(predicateType != "other" & predicateType != "comPriv")

# how many predicates?
n_distinct(d2$verb_renamed) # 429
n_distinct(d2$verb) # 411

d3 = d2 %>%
  distinct(verb_renamed, verb, voice, predicateType, predicateType2, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum)

table(d3$predicateType)
# cognitive communicative       emotive    evidential 
#        82           205           101            41 

table(d3$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        82         41        101         41        164 

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

# combine projection and valence/arousal/dominance ratings in one df
mean.proj2 = left_join(d3, mean.proj, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj2) # 429

# rescale V + A ratings
# With valence and dominance there are the extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle of the
# "calm - aroused" scale, but at the lower extreme: calmness is the absence of arousal; there 
# is no such thing as "negative" arousal. 
new.scale <- mean.proj2 %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5),
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = A.Mean.Sum - 1)

# Valence: everything in order? - yes.
max(mean.proj2$V.Mean.Sum)-min(mean.proj2$V.Mean.Sum)
# [1] 6.81
new.scale %>% 
  group_by(V.Mean.Sum2.direction) %>% 
  slice_max(V.Mean.Sum2) %>% 
  ungroup() %>% 
  pull(V.Mean.Sum2) %>% 
  sum()
# [1] 6.81

# arousal: everything in order? - yes.
max(mean.proj2$A.Mean.Sum)-min(mean.proj2$A.Mean.Sum)
# [1] 4.62
max(new.scale$A.Mean.Sum)-min(new.scale$A.Mean.Sum)
# [1] 4.62


## valence ----
### by predicate ----
ggplot(new.scale, aes(x = reorder(verb_renamed, V.Mean.Sum2), y=V.Mean.Sum2, colour = predicateType)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean valence rating", colour = "Predicate type") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
ggsave("../graphs/valence-by-predicate.pdf", height = 4, width = 13)

# valence by predicate with emotive component distinction for communicatives
ggplot(new.scale, aes(x = reorder(verb_renamed, V.Mean.Sum2), y=V.Mean.Sum2, colour = predicateType2)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean valence rating", colour = "Predicate type") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_colour_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                 "evidential", "communicative without\nemotive component"))
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### lm ----
lm(V.Mean.Sum2 ~ predicateType2, new.scale) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               1.05793    0.07724  13.697  < 2e-16 ***
# predicateType2emoComm     0.35841    0.13378   2.679  0.00767 ** 
# predicateType2emotive     0.71989    0.10397   6.924 1.63e-11 ***
# predicateType2evidential -0.18573    0.13378  -1.388  0.16577    
# predicateType2nonEmoComm -0.24341    0.09460  -2.573  0.01042 * 

### by predicate type ----
# calculate valence by predicateType means
mean.valence = new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Valence))
mean.valence

ggplot(mean.valence, aes(x = predicateType, y = Mean.Valence)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylab("Mean valence rating") +
  xlab("Predicate type")
ggsave("../graphs/valence-by-predicateType.pdf", height = 8, width = 10)

# calculate valence by predicateType2 means (communicatives with/without emotive component)
mean.valence2 = new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2, aes(x = predicateType2, y = Mean.Valence)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("communicative without \n emotive component", "evidential",
                              "cognitive", "communicative with \n emotive component", "emotive")) +
  ylab("Mean valence rating") +
  xlab("Predicate type")
ggsave("../graphs/valence-by-predicateType2.pdf", height = 8, width = 10)

#### with direction ----
# calculate valence by predicateType2 means and direction of valence
mean.valence3 = new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 10

# valence by predicate type and direction of valence
ggplot(mean.valence3, aes(x = predicateType2, y = Mean.Valence, colour = V.Mean.Sum2.direction)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("cognitive", "communicative with \n emotive component",
                              "emotive", "evidential", "communicative without \n emotive component")) +
  labs(y ="Mean valence rating as distance from neutral",
       x = "Predicate type",
       colour = "direction of valence") 
ggsave("../graphs/valence-by-predicateType-and-direction.pdf", height = 8, width = 10)


## arousal ----
### by predicate ----
ggplot(new.scale, aes(x = reorder(verb_renamed, A.Mean.Sum2), y = A.Mean.Sum2, colour = predicateType)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean arousal rating", colour = "Predicate type") +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0,8), breaks = c(0, 8), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "completely\nexcited"))
ggsave("../graphs/arousal-by-predicate.pdf", height = 4, width = 13)

# arousal by predicate with emotive component
ggplot(new.scale, aes(x = reorder(verb_renamed, A.Mean.Sum2), y = A.Mean.Sum2, colour = predicateType2)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean arousal rating", colour = "Predicate type") +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0,8), breaks = c(0, 8), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "completely\nexcited")) +
  scale_colour_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                   "evidential", "communicative without\nemotive component"))
ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)

#### lm ----
lm(A.Mean.Sum2 ~ predicateType, new.scale) %>% 
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 2.88549    0.09418  30.639  < 2e-16 ***
# predicateTypecommunicative  0.32871    0.11143   2.950  0.00335 ** 
# predicateTypeemotive        1.06362    0.12677   8.390 7.25e-16 ***
# predicateTypeevidential     0.18768    0.16312   1.151  0.25055 

lm(A.Mean.Sum2 ~ predicateType2, new.scale) %>% 
  summary()
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               2.88549    0.09281  31.089  < 2e-16 ***
# predicateType2emoComm     0.76134    0.16076   4.736 2.98e-06 ***
# predicateType2emotive     1.06362    0.12493   8.514 2.95e-16 ***
# predicateType2evidential  0.18768    0.16076   1.168    0.244    
# predicateType2nonEmoComm  0.22055    0.11367   1.940    0.053 . 

### by predicate type ----
# calculate arousal by predicateType means
mean.arousal = new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Arousal))
mean.arousal

ggplot(mean.arousal, aes(x = predicateType, y = Mean.Arousal)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylab("Mean arousal rating (calm >>> excited)") +
  xlab("Predicate type")
ggsave("../graphs/arousal-by-predicateType.pdf", height = 8, width = 10)

# calculate arousal by predicateType2 means (communicatives with/without emotive component)
mean.arousal2 = new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal2

ggplot(mean.arousal2, aes(x = predicateType2, y = Mean.Arousal)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("cognitive", "evidential", "communicative without \n emotive component",
                              "communicative with \n emotive component", "emotive")) +
  ylab("Mean arousal rating (calm >>> excited)") +
  xlab("Predicate type") 
ggsave("../graphs/arousal-by-predicateType2.pdf", height = 8, width = 10)


## valence + arousal ----
# valence against arousal: no patterns (clusters) emerge.
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2)) +
  labs(
    x = "Mean arousal rating", y = "Mean valence rating",
    colour = "Predicate type") 
ggsave("../graphs/valence-by-arousal.pdf", height = 10, width = 10)

### by predicate type ----
# distribution of valence and arousal ratings by predicate type

new.scale %>% 
  select(verb_renamed, predicateType, Valence = V.Mean.Sum2, Arousal = A.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Arousal), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType)) +
  geom_violin() +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(y = "Mean valence / arousal rating", fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy\narousal rating: completely calm (0) to completely excited (8)") +
   scale_x_discrete(limits = c("Valence", "Arousal"))
ggsave("../graphs/valence-arousal-by-predicateType.pdf", height = 8, width = 16)

# distribution of valence and arousal ratings by predicate type including emotive component
new.scale %>% 
  select(verb_renamed, predicateType2, Valence = V.Mean.Sum2, Arousal = A.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Arousal), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType2)) +
  geom_violin() +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0)) + 
  labs(y = "Mean valence / arousal rating", fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy\narousal rating: completely calm (0) to completely excited (8)") +
  scale_x_discrete(limits = c("Valence", "Arousal")) +
  scale_fill_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                 "evidential", "communicative without\nemotive component"))
ggsave("../graphs/valence-arousal-by-predicateType2.pdf", height = 8, width = 16)


## projection: valence and arousal against projection ratings ----

### valence ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean valence rating", y = "Mean projection rating",
    colour = "Predicate type") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-valence.pdf", height = 10, width = 10)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean valence rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType)
ggsave("../graphs/projection-by-valence-faceted.pdf", height = 8, width = 20)

# projection by valence faceted with emotive component
new.scale %>%
  mutate(predicateType2 = recode(predicateType2, "cognitive" = "cognitive", 
                                 "emoComm" = "communicative with\nemotive component", 
                                 "emotive" = "emotive", 
                                 "evidential" = "evidential", 
                                 "nonEmoComm" = "communicative without\nemotive component")) %>%
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean valence rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType2)
ggsave("../graphs/projection-by-valence-faceted2.pdf", height = 8, width = 20)

#### lm ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.26074    0.02181  11.955  < 2e-16 ***
# V.Mean.Sum2  0.11613    0.01558   7.454  5.1e-13 ***

lm(Mean.Proj ~ V.Mean.Sum2 + predicateType2, data = new.scale) %>% 
  summary()

# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.228570   0.025892   8.828  < 2e-16 ***
# V.Mean.Sum2              0.008901   0.013555   0.657   0.5117    
# predicateType2emoComm    0.169532   0.037655   4.502 8.71e-06 ***
# predicateType2emotive    0.486218   0.030616  15.881  < 2e-16 ***
# predicateType2evidential 0.073534   0.037425   1.965   0.0501 .  
# predicateType2nonEmoComm 0.046749   0.026609   1.757   0.0797 .  

lm(Mean.Proj ~ V.Mean.Sum2 * predicateType2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.226211   0.038666   5.850 9.90e-09 ***
# V.Mean.Sum2                           0.011132   0.030348   0.367   0.7140    
# predicateType2emoComm                 0.107320   0.073389   1.462   0.1444    
# predicateType2emotive                 0.498413   0.061763   8.070 7.51e-15 ***
# predicateType2evidential              0.148465   0.065131   2.279   0.0231 *  
# predicateType2nonEmoComm              0.043884   0.046172   0.950   0.3424    
# V.Mean.Sum2:predicateType2emoComm     0.043360   0.048966   0.886   0.3764    
# V.Mean.Sum2:predicateType2emotive    -0.007763   0.039187  -0.198   0.8431    
# V.Mean.Sum2:predicateType2evidential -0.085435   0.057543  -1.485   0.1384    
# V.Mean.Sum2:predicateType2nonEmoComm  0.004185   0.039127   0.107   0.9149   


#### with direction ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm", aes(linetype = V.Mean.Sum2.direction)) +
  labs(
    x = "Mean valence rating", y = "Mean projection rating",
    colour = "Predicate type") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) 
ggsave("../graphs/projection-by-valence.pdf", height = 10, width = 10)

##### lm ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.44309    0.01980  22.378  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.08598    0.02631  -3.268  0.00117 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm(Mean.Proj ~ V.Mean.Sum2 + V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.30319    0.02707  11.198  < 2e-16 ***
# V.Mean.Sum2                    0.11147    0.01558   7.156 3.65e-12 ***
# V.Mean.Sum2.directionpositive -0.06548    0.02505  -2.614  0.00927 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


### arousal ----
# projection by arousal
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean arousal rating", y = "Mean projection rating",
    colour = "Predicate type") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        aspect.ratio = 1) +
  scale_x_continuous(limits = c(0, 8), breaks = c(0, 4, 8), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-arousal.pdf", height = 10, width = 10)

# projection by arousal faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean arousal rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(0, 8), breaks = c(0, 4, 8), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType)
ggsave("../graphs/projection-by-arousal-faceted.pdf", height = 8, width = 20)

# projection by arousal faceted with emotive component
new.scale %>%
  mutate(predicateType2 = recode(predicateType2, "cognitive" = "cognitive", 
                                 "emoComm" = "communicative with\nemotive component", 
                                 "emotive" = "emotive", 
                                 "evidential" = "evidential", 
                                 "nonEmoComm" = "communicative without\nemotive component")) %>%
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean arousal rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(0, 8), breaks = c(0, 4, 8), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType2)
ggsave("../graphs/projection-by-arousal-faceted2.pdf", height = 8, width = 20)

#### lm ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13257    0.04711   2.814  0.00512 ** 
# A.Mean.Sum2  0.07908    0.01370   5.771 1.52e-08 ***

lm(Mean.Proj ~ A.Mean.Sum2 + predicateType2, data = new.scale) %>% 
  summary()

# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.264298   0.039032   6.771 4.28e-11 ***
# A.Mean.Sum2              -0.009118   0.011278  -0.809   0.4192    
# predicateType2emoComm     0.179664   0.038306   4.690 3.69e-06 ***
# predicateType2emotive     0.502325   0.031394  16.001  < 2e-16 ***
# predicateType2evidential  0.073592   0.037391   1.968   0.0497 *  
# predicateType2nonEmoComm  0.046593   0.026514   1.757   0.0796 . 

lm(Mean.Proj ~ A.Mean.Sum2 * predicateType2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.289064   0.090316   3.201 0.001476 ** 
# A.Mean.Sum2                          -0.017701   0.030405  -0.582 0.560761    
# predicateType2emoComm                -0.056464   0.157469  -0.359 0.720096    
# predicateType2emotive                 0.430007   0.120133   3.579 0.000385 ***
# predicateType2evidential              0.347911   0.163373   2.130 0.033791 *  
# predicateType2nonEmoComm              0.043027   0.108552   0.396 0.692031    
# A.Mean.Sum2:predicateType2emoComm     0.066541   0.045897   1.450 0.147866    
# A.Mean.Sum2:predicateType2emotive     0.020624   0.036096   0.571 0.568057    
# A.Mean.Sum2:predicateType2evidential -0.088738   0.052816  -1.680 0.093678 .  
# A.Mean.Sum2:predicateType2nonEmoComm  0.001758   0.035729   0.049 0.960789  

### valence + arousal ----
#### lm ----
# original scale
lm(Mean.Proj ~ V.Mean.Sum + A.Mean.Sum, data = new.scale) %>% 
  summary()
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.182230   0.087856   2.074   0.0387 *  
# V.Mean.Sum  -0.018918   0.009403  -2.012   0.0448 *  
# A.Mean.Sum   0.071431   0.014172   5.040 6.89e-07 ***

# new scale
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13318    0.04553   2.925  0.00363 ** 
# V.Mean.Sum2  0.09428    0.01688   5.586 4.15e-08 ***
# A.Mean.Sum2  0.04612    0.01450   3.182  0.00157 ** 


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


# H2: Eventive vs stative ----
# plot projection by dynamicity to see if dynamicity modulates projection

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType and Dynamicity columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         Dynamicity = case_when(stative_predicate == "yes" ~ "stative",
                                dynamic_predicate == "yes" ~ "dynamic",
                                TRUE ~ "unclear"))

# how many predicates in which predicateType and with which Dynamicity?
d.proj %>%
  select(predicateType, verb, Dynamicity) %>% 
  unique() %>% 
  group_by(predicateType, Dynamicity) %>% 
  summarize(count=n()) 

mean.proj = d.proj %>%
  group_by(predicateType, Dynamicity) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) #11

ggplot(mean.proj, aes(x=predicateType, y=Mean.Proj, color = Dynamicity)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-.2,1),breaks = c(-.2,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate type")
ggsave("../graphs/projection-by-predicateType-and-Dynamicity.pdf",height=4,width=5)


# H3: Veridicality and projection ratings: positive correlation ----

# create datasets for projection and veridicality inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291
d.verid = droplevels(subset(d,d$polarity != "negative" & d$conditional2 != "conditional"))
nrow(d.verid) #5401

# create predicateType column
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"))

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
  #filter(voice == "active") %>%
  select(c(verb,verb_renamed,voice,predicateType)) %>%
  distinct(verb,verb_renamed,voice,predicateType)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

# calculate by-predicate veridicality means 
mean.verid = d.verid %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Verid = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Verid = Mean.Verid - CILow, YMax.Verid = Mean.Verid + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Verid))
mean.verid
nrow(mean.verid) #544
levels(mean.verid$verb_renamed)

mean.proj.verid = mean.proj %>%
  inner_join(mean.verid,by=("verb_renamed"))
mean.proj.verid

# remove "other" and "comPriv" predicates
mean.proj.verid = mean.proj.verid %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.verid) #524

# Mean projection by mean veridicality
ggplot(mean.proj.verid, aes(x=Mean.Verid,y=Mean.Proj, group = predicateType)) +
  geom_point(shape=21) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbarh(aes(xmin=YMin.Verid,xmax=YMax.Verid),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),alpha=.8,color="gray") +
  #geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  xlab("Mean veridicality rating") +
  ylab("Mean projection rating") +
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(-1,1)) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-by-veridicality-with-CIs.pdf", height = 4,width = 5)

ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, color = predicateType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean veridicality rating", y = "Mean projection rating",
    color = "Predicate type") +
  theme(legend.position = "top") +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality.pdf", height = 4,width = 8)

# What are the predicates with negative mean projection rating and positive mean veridicality rating?
ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, color = predicateType)) +
  geom_point(alpha = .3) +
  geom_label_repel(data=subset(mean.proj.verid, Mean.Verid > 0 & Mean.Proj < 0),
                   aes(x = Mean.Verid, y = Mean.Proj, label=verb_renamed)) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.5) +
  labs(
    x = "Mean veridicality rating", y = "Mean projection rating",
    color = "Predicate type") +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  geom_abline(intercept=0,slope=0, color="gray", linetype="dashed") +
  #geom_abline(intercept=0,slope=0, color="gray", linetype="dashed") +
  geom_vline(xintercept=0, color="gray", linetype="dashed") +
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(-1,1)) +
  coord_fixed(ratio = 1) + 
  facet_wrap(~predicateType)
ggsave("../graphs/projection-by-veridicality2.pdf", height = 4,width = 6)






#  end of script for now ----

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
