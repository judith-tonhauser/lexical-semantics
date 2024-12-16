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
library(patchwork)
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
nrow(d.proj) # 16291

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))

table(d.proj$predicateType)

### by-predicateType ----
#### plots ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj))
mean.proj
nrow(mean.proj) # 6
levels(mean.proj$predicateType)

mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 5.5, 22, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType.pdf", height = 4, width = 10)

# calculate by-predicateType2 means
mean.proj = d.proj %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj
nrow(mean.proj) # 7
levels(mean.proj$predicateType2)

mean.proj %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
  ggplot(aes(
    x = factor(predicateType2, c("cognitive", "evidential", "nonEmoComm", 
                                 "emoComm", "emotive")), 
    y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = c("cognitive", "evidential", 
                              "communicative without\nemotive component", 
                              "communicative with\nemotive component", "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2.pdf", height = 4, width = 10)




### by-predicate ----
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

# add predicateType and verb to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# how many predicates in which type?
mean.proj %>%
  select(predicateType, verb_renamed) %>% 
  unique() %>% 
  group_by(predicateType) %>% 
  summarize(count=n())
# predicateType count
#   <chr>         <int>
# 1 cognitive        53
# 2 comPriv           9
# 3 communicative   236
# 4 emotive         148
# 5 evidential       86
# 6 other            12

# predicateType2 count
#   <chr>          <int>
# 1 cognitive         53
# 2 comPriv            9
# 3 emoComm           47
# 4 emotive          148
# 5 evidential        86
# 6 nonEmoComm       189
# 7 other             12


# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

#### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate.pdf", height = 4, width = 13)

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating", 
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType, ncol = 4)
ggsave("../graphs/projection-by-predicate-faceted.pdf", height = 4, width = 9)

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.8) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap( ~ predicateType2, 
              labeller = as_labeller(predicateType2_names), ncol = 5)
ggsave("../graphs/projection-by-predicate-faceted2.pdf", height = 4, width = 10)



#### linear models ----
lm(Mean.Proj ~ fct_relevel(predicateType, "emotive"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                                    Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(predicateType, "emotive")cognitive     -2.27319    0.06720  -33.83   <2e-16 ***
# fct_relevel(predicateType, "emotive")communicative -1.87493    0.04689  -39.98   <2e-16 ***
# fct_relevel(predicateType, "emotive")evidential    -1.93886    0.05782  -33.53   <2e-16 ***

lm(Mean.Proj ~ fct_relevel(predicateType2, "emotive"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.72264    0.01585   45.59   <2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.50635    0.03087  -16.40   <2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.29769    0.03229   -9.22   <2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential -0.42547    0.02615  -16.27   <2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.44383    0.02117  -20.97   <2e-16 ***

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

# set emotives as reference level for predicate type
# d.proj = d.proj %>% 
#   mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ fct_relevel(predicateType, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(predicateType2, "emotive")cognitive  -2.27552    0.06727  -33.83   <2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm    -1.44396    0.06900  -20.93   <2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential -1.93997    0.05786  -33.53   <2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -1.98376    0.04874  -40.70   <2e-16 ***

# The output for ordinal models with categorical variables as predictors is not
# informative as there is no intercept.

# with predicate type with emotive component distinction
# set emotives as reference level
d.proj = d.proj %>% 
  mutate(predicateType2 = fct_relevel(predicateType2, "emotive"))

clmm(as.factor(veridicality_num) ~ predicateType2 + (1 | participant) +
       (1 | environment), data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ predicateType2 + (1 | participant) +  
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  15662 -11304.76 22625.53 633(3161) 4.91e-03 4.2e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7760   0.8809  
# environment (Intercept) 0.4886   0.6990  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    
# predicateType2cognitive  -2.27552    0.06727  -33.83   <2e-16 ***
# predicateType2emoComm    -1.44396    0.06900  -20.93   <2e-16 ***
# predicateType2evidential -1.93997    0.05786  -33.53   <2e-16 ***
# predicateType2nonEmoComm -1.98376    0.04874  -40.70   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#       Estimate Std. Error z value
# -1|0  -4.8836     0.4112 -11.877
# 0|1   -1.2377     0.4089  -3.027


## H1.2: communicatives: ----
### H1.2.1 with/without emotive component ----
# Amongst the communication predicates, does the CC of those that have an emotive 
# meaning component project more than the CCs of those that don't?

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))


#### by-predicateType ----
##### plot ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType, emotiveComponent) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 7
levels(mean.proj$predicateType)

mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
ggplot(aes(x = predicateType, y = Mean.Proj, colour = emotiveComponent)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour ="Emotive component") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = c("no" = "sienna4", "yes" = "green3"))
ggsave("../graphs/projection-by-predicateType-emotiveComponent.pdf", height = 4, width = 5)


#### by-predicate ----
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

# add predicateType, verb and emotiveComponent to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# only communicative predicates
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) # 236

##### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = emotiveComponent)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
    theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Emotive component") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("deepskyblue2", "green3")) +
  facet_wrap( ~ emotiveComponent, 
              labeller = as_labeller(c("no" = "communicatives\nwithout emotive component", 
                                       "yes" = "communicatives\nwith emotive component")))
ggsave("../graphs/projection-by-communicative-with-out-emo.pdf", height = 6, width = 9)


##### linear models ----
lm(Mean.Proj ~ emotiveComponent, data = mean.proj) %>% 
  summary()
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.27881    0.01271  21.939  < 2e-16 ***
# emotiveComponentyes  0.14614    0.02848   5.132 6.03e-07 ***

##### ordinal model ----
# only communicative predicates
d.proj = d.proj %>%
  filter(predicateType == "communicative")

clmm(as.factor(veridicality_num) ~ emotiveComponent + (1 | participant) + 
       (1 | environment), data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ emotiveComponent + (1 | participant) + 
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs logLik   AIC      niter     max.grad cond.H 
# logit flexible  7069 -5259.36 10528.71 272(1609) 3.09e-04 3.8e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7489   0.8654  
# environment (Intercept) 0.9045   0.9510  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# emotiveComponentyes  0.58492    0.06701   8.728   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -3.1856     0.5546  -5.744
# 0|1    0.7887     0.5526   1.427 

### H1.2.2 pure/discourse participation/state changing ----
# Amongst the communication predicates, does the CC of any particular subtype/s 
# project more strongly?

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"),
         commType = case_when(pure_comm == "yes" ~ "pure",
                              discourse_participation_comm == "yes" ~ "discourse participation",
                              state_changing_comm == "yes" ~ "state changing"))


#### by-predicateType ----
##### plot ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  filter(predicateType == "communicative") %>% 
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj$commType) # 3

mean.proj %>%
  ggplot(aes(x = commType, y = Mean.Proj, colour = commType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132)) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state\nchanging", "discourse\nparticipation", "pure")) + 
  scale_colour_manual(values = c("pure" = "orange", 
                                 "discourse participation" = "darkorange", 
                                 "state changing" = "orangered"))
ggsave("../graphs/projection-by-predicateType-commType.pdf", height = 4, width = 10)


#### by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# add predicateType, verb and emotiveComponent to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, commType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, commType)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# only communicatives
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) # 236

mean.proj %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  summarize(count=n())

#   commType                count
#   <chr>                   <int>
# 1 discourse participation   114
# 2 pure                       95
# 3 state changing             27

##### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = commType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("pure" = "orange",
                                 "discourse participation" = "darkorange",
                                 "state changing" = "orangered")) +
  facet_wrap( ~ factor(commType, levels = c("pure", "discourse participation", 
                                            "state changing")))
ggsave("../graphs/projection-by-communicative-predicate.pdf", height = 4, width = 10)


##### linear model ----
lm(Mean.Proj ~ fct_relevel(commType, "state changing"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.29914    0.01671  17.898  < 2e-16 ***
# commTypepure            0.05140    0.02479   2.073  0.03926 *  
# commTypestate changing -0.10421    0.03820  -2.728  0.00685 ** 

# *** significant for all three types of communicatives.

##### ordinal model ----
# only communicative predicates
d.proj = d.proj %>%
  filter(predicateType == "communicative")

clmm(as.factor(veridicality_num) ~ commType + (1 | participant) + 
       (1 | environment), data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ commType + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs logLik   AIC      niter     max.grad cond.H 
# logit flexible  7069 -5280.66 10573.32 348(2057) 8.19e-05 3.7e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7444   0.8628  
# environment (Intercept) 0.8876   0.9421  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
# commTypepure            0.17559    0.05609   3.130 0.001746 ** 
# commTypestate changing -0.34393    0.08849  -3.887 0.000102 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -3.2547     0.5500  -5.918
# 0|1    0.7010     0.5479   1.279


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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
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
  distinct(verb_renamed, verb, voice, predicateType, predicateType2, V.Mean.Sum, 
           A.Mean.Sum, D.Mean.Sum)

table(d3$predicateType)
# cognitive communicative       emotive    evidential 
#        46           205           101            76 

table(d3$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        46         41        101         76        164 

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component"))
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emotive"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.44446    0.01720  25.846  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.12581    0.03074  -4.093 5.11e-05 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.09037    0.03200  -2.824  0.00497 ** 
# fct_relevel(predicateType2, "emotive")evidential -0.23469    0.02624  -8.943  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.24083    0.02186 -11.017  < 2e-16 ***

### by predicate type ----
#### plots ----
# calculate valence by predicateType means
mean.valence = new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
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
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
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
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 10

# valence by predicate type and direction of valence
#### plot ----
ggplot(mean.valence3, aes(x = predicateType2, y = Mean.Valence, 
                          colour = V.Mean.Sum2.direction)) +
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
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emotive") + V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.441194   0.018421  23.951  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.128746   0.031329  -4.110 4.77e-05 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.089714   0.032058  -2.798  0.00537 ** 
# fct_relevel(predicateType2, "emotive")evidential -0.238335   0.027274  -8.738  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.243157   0.022376 -10.867  < 2e-16 ***
# V.Mean.Sum2.directionpositive                     0.008902   0.017919   0.497  0.61960 


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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", "communicative without\nemotive component"))
ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)

#### linear models ----
lm(A.Mean.Sum2 ~ predicateType, new.scale) %>% 
  summary()
# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.373071   0.015754  23.681  < 2e-16 ***
# predicateTypecommunicative  0.028704   0.017432   1.647    0.100    
# predicateTypeemotive        0.120568   0.019006   6.344 5.76e-10 ***
# predicateTypeevidential    -0.006673   0.019960  -0.334    0.738   

lm(A.Mean.Sum2 ~ predicateType2, new.scale) %>% 
  summary()
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.373071   0.015527  24.028  < 2e-16 ***
# predicateType2emoComm     0.082783   0.022618   3.660 0.000284 ***
# predicateType2emotive     0.120568   0.018732   6.437 3.32e-10 ***
# predicateType2evidential -0.006673   0.019672  -0.339 0.734635    
# predicateType2nonEmoComm  0.015184   0.017570   0.864 0.387966 

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
       y = "Mean arousal rating") +
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
  scale_x_discrete(labels = c("cognitive", "evidential", 
                              "communicative without \n emotive component",
                              "communicative with \n emotive component", "emotive")) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2.pdf", height = 8, width = 10)


## H2.3 valence + arousal ----
# valence against arousal: no patterns (clusters) emerge.
#### plot ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2, colour = predicateType2, 
                      fill = predicateType2)) +
  geom_point(size = 2) +
  geom_mark_hull(alpha = 0.1) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 24)) +
  labs(x = "Mean arousal rating", 
       y = "Mean valence rating",
       colour = "Predicate type", 
       fill = "Predicate type") +
  coord_equal() + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels=c("cognitive", "communicative with\nemotive component", 
                               "emotive", "evidential", 
                               "communicative without\nemotive component")) +
  scale_fill_manual(values = cols2, 
                    labels=c("cognitive", "communicative with\nemotive component", 
                             "emotive", "evidential", 
                             "communicative without\nemotive component"))
ggsave("../graphs/valence-by-arousal.pdf", height = 8, width = 12)

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
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 12)) + 
  labs(y = "Mean valence / arousal rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely aroused (1)") +
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
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence.pdf", height = 6, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component"))
ggsave("../graphs/projection-by-valence2.pdf", height = 6, width = 8)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
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
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2.pdf", height = 4, width = 9)

#### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.26078    0.02190  11.905  < 2e-16 ***
# V.Mean.Sum2  0.46441    0.06252   7.428 6.06e-13 ***

lm(Mean.Proj ~ V.Mean.Sum2 + fct_relevel(predicateType2, "emotive"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.71014    0.03123  22.736   <2e-16 ***
# V.Mean.Sum2                                       0.04606    0.05499   0.838    0.403    
# fct_relevel(predicateType2, "emotive")cognitive  -0.50493    0.03545 -14.244   <2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.31574    0.03653  -8.642   <2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential -0.43246    0.03236 -13.362   <2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.43695    0.02805 -15.580   <2e-16 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emotive"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.724624   0.048371  14.981  < 2e-16 ***
# V.Mean.Sum2                                                   0.013475   0.099596   0.135    0.892    
# fct_relevel(predicateType2, "emotive")cognitive              -0.507184   0.074957  -6.766 4.48e-11 ***
# fct_relevel(predicateType2, "emotive")emoComm                -0.391093   0.079149  -4.941 1.13e-06 ***
# fct_relevel(predicateType2, "emotive")evidential             -0.428041   0.062186  -6.883 2.15e-11 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm             -0.454530   0.054609  -8.323 1.23e-15 ***
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive  -0.005788   0.184362  -0.031    0.975    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm     0.204490   0.183718   1.113    0.266    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential -0.057535   0.182061  -0.316    0.752    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm  0.047789   0.140582   0.340    0.734  

##### largest residuals ----
# For which predicates does the linear model make the least accurate predictions?
# I don't remember why we were wondering about this. :)
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
           veridicality, veridicality_num, V.Mean.Sum, A.Mean.Sum, V.Mean.Sum2, 
           V.Mean.Sum2.direction, A.Mean.Sum2, environment)

# remove "other" and "comPriv" predicates
d.proj2 = d.proj2 %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant) + (1 | environment),
     data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant) + 
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  12817 -10002.21 20014.42 256(1285) 1.55e-03 3.4e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6856   0.8280  
# environment (Intercept) 0.4495   0.6704  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2  1.98765    0.09765   20.35   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.6331     0.3930  -6.700
# 0|1    0.8454     0.3918   2.158 

# with predicate type
# set emotives as reference level
d.proj2 = d.proj2 %>% 
  mutate(predicateType = fct_relevel(predicateType, "emotive"))

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * predicateType + (1 | participant) + 
       (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 * predicateType + 
#   (1 |  participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter      max.grad cond.H 
# logit flexible  12817 -9337.43 18696.86 1045(5205) 6.99e-03 4.5e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7831   0.8850  
# environment (Intercept) 0.5322   0.7295  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                        Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                              0.1154     0.2371   0.487  0.62646    
# predicateTypecognitive                  -2.3579     0.1618 -14.577  < 2e-16 ***
# predicateTypecommunicative              -2.0626     0.1244 -16.577  < 2e-16 ***
# predicateTypeevidential                 -2.0055     0.1396 -14.364  < 2e-16 ***
# V.Mean.Sum2:predicateTypecognitive       0.1545     0.3879   0.398  0.69030    
# V.Mean.Sum2:predicateTypecommunicative   0.7406     0.2840   2.608  0.00911 ** 
# V.Mean.Sum2:predicateTypeevidential      0.0578     0.3866   0.150  0.88114    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -4.9131     0.4425 -11.102
# 0|1   -1.2155     0.4399  -2.763

#### with direction ----
# projection by valence with direction
##### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-valence-with-direction.pdf", height = 3.5, width = 6)

# projection by valence faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) + 
  # plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

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
       (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant) +  
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  12817 -10165.16 20340.33 697(2818) 1.68e-03 3.3e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6506   0.8066  
# environment (Intercept) 0.4298   0.6556  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.38510    0.03787  -10.17   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0 -3.35891    0.38461  -8.733
# 0|1   0.05278    0.38275   0.138

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2,
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter      max.grad cond.H 
# logit flexible  12817 -9301.52 18629.03 1856(9284) 8.36e-03 4.5e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7849   0.8859  
# environment (Intercept) 0.5338   0.7306  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                                Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive                                                   0.23379    0.09755   2.397 0.016543 *  
# fct_relevel(predicateType2, "emotive")cognitive                                -1.93736    0.12063 -16.060  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                                  -1.33776    0.09329 -14.340  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential                               -2.38802    0.11560 -20.658  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                               -1.93638    0.07728 -25.058  < 2e-16 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive  -0.67512    0.15917  -4.242 2.22e-05 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm    -0.57056    0.16557  -3.446 0.000569 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential  0.33189    0.14784   2.245 0.024775 *  
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm -0.22773    0.11583  -1.966 0.049293 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -4.8948     0.4318 -11.335
# 0|1   -1.1831     0.4291  -2.757

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + 
#   (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter     max.grad cond.H 
# logit flexible  12817 -9963.00 19940.00 518(2587) 8.67e-04 3.5e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6926   0.8322  
# environment (Intercept) 0.4552   0.6747  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                2.30633    0.14125  16.328  < 2e-16 ***
# V.Mean.Sum2.directionpositive             -0.08326    0.06704  -1.242    0.214    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.77539    0.19396  -3.998  6.4e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.7210     0.3976  -6.844
# 0|1    0.7709     0.3963   1.945

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emotive") +  (1 | participant) + (1 | environment), 
     data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction *  
#   fct_relevel(predicateType2, "emotive") + (1 | participant) +  (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad cond.H 
# logit flexible  12817 -9285.06 18616.13 3580(17904) 5.14e-03 2.9e+03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7894   0.8885  
# environment (Intercept) 0.5367   0.7326  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                                            Estimate Std. Error z value
# V.Mean.Sum2                                                                                -0.28373    0.30119  -0.942
# V.Mean.Sum2.directionpositive                                                              -0.27539    0.23537  -1.170
# fct_relevel(predicateType2, "emotive")cognitive                                            -2.34061    0.23817  -9.828
# fct_relevel(predicateType2, "emotive")emoComm                                              -2.06669    0.22201  -9.309
# fct_relevel(predicateType2, "emotive")evidential                                           -2.45000    0.20487 -11.959
# fct_relevel(predicateType2, "emotive")nonEmoComm                                           -2.09417    0.16884 -12.403
# V.Mean.Sum2:V.Mean.Sum2.directionpositive                                                   1.18838    0.49662   2.393
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                                 1.40089    0.69424   2.018
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                                   2.08293    0.53550   3.890
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential                               -0.15928    0.66644  -0.239
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm                                0.40676    0.40681   1.000
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive              -0.03438    0.33260  -0.103
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm                 0.32990    0.35514   0.929
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential              0.75686    0.29019   2.608
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm              0.19495    0.25724   0.758
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive  -1.89680    0.87879  -2.158
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm    -2.47369    0.80086  -3.089
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential -0.67952    0.85691  -0.793
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm -0.73837    0.63579  -1.161
#                                                                                            Pr(>|z|)    
# V.Mean.Sum2                                                                                 0.34617    
# V.Mean.Sum2.directionpositive                                                               0.24199    
# fct_relevel(predicateType2, "emotive")cognitive                                             < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                                               < 2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential                                            < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                                            < 2e-16 ***
# V.Mean.Sum2:V.Mean.Sum2.directionpositive                                                   0.01671 *  
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                                 0.04360 *  
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                                   0.00010 ***
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential                                0.81111    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm                                0.31737    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive               0.91766    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm                 0.35293    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential              0.00910 ** 
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm              0.44853    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive   0.03089 *  
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm     0.00201 ** 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential  0.42778    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm  0.24550    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#       Estimate Std. Error z value
# -1|0  -5.0330     0.4546 -11.072
# 0|1   -1.3133     0.4519  -2.906


### H2.4.2 arousal ----
# projection by arousal
#### H2.4.2.1 overall ----
##### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-arousal2.pdf", height = 3.5, width = 6)

# projection by arousal faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = c(0, 1)),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
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
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-faceted2.pdf", height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13319    0.04723    2.82  0.00502 ** 
# A.Mean.Sum2  0.63149    0.10982    5.75  1.7e-08 ***

lm(Mean.Proj ~ A.Mean.Sum2 + fct_relevel(predicateType2, "emotive"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.76030    0.04862  15.637   <2e-16 ***
# A.Mean.Sum2                                      -0.06015    0.09027  -0.666    0.506    
# fct_relevel(predicateType2, "emotive")cognitive  -0.51798    0.03644 -14.214   <2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.32218    0.03637  -8.859   <2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential -0.45093    0.03183 -14.165   <2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.45438    0.02650 -17.149   <2e-16 ***

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emotive"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.71907    0.07970   9.022  < 2e-16 ***
# A.Mean.Sum2                                                   0.02338    0.15659   0.149 0.881369    
# fct_relevel(predicateType2, "emotive")cognitive              -0.36285    0.14607  -2.484 0.013380 *  
# fct_relevel(predicateType2, "emotive")emoComm                -0.48647    0.15231  -3.194 0.001510 ** 
# fct_relevel(predicateType2, "emotive")evidential             -0.33047    0.12472  -2.650 0.008361 ** 
# fct_relevel(predicateType2, "emotive")nonEmoComm             -0.38698    0.10012  -3.865 0.000129 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive  -0.38882    0.35527  -1.094 0.274397    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm     0.36733    0.31798   1.155 0.248658    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential -0.29975    0.29887  -1.003 0.316461    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm -0.15093    0.21756  -0.694 0.488228  

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) +  
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik    AIC      niter    max.grad cond.H 
# logit flexible  12817 -10097.57 20205.14 237(985) 2.72e-03 3.4e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6520   0.8074  
# environment (Intercept) 0.4343   0.6590  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.5182     0.1646    15.3   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.1261     0.3909  -5.439
# 0|1    1.3130     0.3901   3.366

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2,
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad cond.H 
# logit flexible  12817 -9319.27 18664.53 3708(18515) 5.44e-01 1.3e+03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7813   0.8839  
# environment (Intercept) 0.5455   0.7386  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                    0.1796     0.3762   0.477   0.6331    
# fct_relevel(predicateType2, "emotive")cognitive               -1.7259     0.3105  -5.559 2.71e-08 ***
# fct_relevel(predicateType2, "emotive")emoComm                 -2.0103     0.3245  -6.194 5.86e-10 ***
# fct_relevel(predicateType2, "emotive")evidential              -1.7623     0.2734  -6.445 1.15e-10 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm              -1.6825     0.2256  -7.459 8.74e-14 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive   -1.5457     0.7408  -2.087   0.0369 *  
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm      1.1015     0.6759   1.630   0.1032    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential  -0.6425     0.6441  -0.997   0.3186    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm  -0.8006     0.4807  -1.665   0.0959 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -4.8115     0.4755 -10.119
# 0|1   -1.1099     0.4732  -2.346

#### H2.4.2.2 with direction of valence ----
##### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-arousal-with-direction.pdf", height = 3.5, width = 6)

# projection by arousal faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) + 
       # plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                     "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)


##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                 2.3729     0.2514   9.439   <2e-16 ***
# V.Mean.Sum2.directionpositive              -0.1158     0.1484  -0.780    0.435    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.2808     0.3422  -0.821    0.412   

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction *  
#   fct_relevel(predicateType2, "emotive") + (1 | participant) +      (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad cond.H 
# logit flexible  12817 -9280.62 18607.25 4238(21169) 2.92e-03 1.7e+04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7928   0.8904  
# environment (Intercept) 0.5377   0.7333  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                                            Estimate Std. Error z value
# A.Mean.Sum2                                                                                 -1.1438     0.5111  -2.238
# V.Mean.Sum2.directionpositive                                                               -1.3151     0.3865  -3.403
# fct_relevel(predicateType2, "emotive")cognitive                                             -2.3311     0.8863  -2.630
# fct_relevel(predicateType2, "emotive")emoComm                                               -2.7789     0.4168  -6.668
# fct_relevel(predicateType2, "emotive")evidential                                            -3.0259     0.4481  -6.753
# fct_relevel(predicateType2, "emotive")nonEmoComm                                            -2.3869     0.3361  -7.103
# A.Mean.Sum2:V.Mean.Sum2.directionpositive                                                    3.2468     0.7813   4.156
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                                  0.6294     2.2488   0.280
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                                    3.1297     0.8829   3.545
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential                                 1.2588     0.9961   1.264
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm                                 0.8274     0.6803   1.216
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive                1.2713     0.9637   1.319
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm                  0.6973     0.7653   0.911
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential               1.8385     0.5826   3.156
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm               1.5685     0.4669   3.359
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive   -4.3340     2.4205  -1.791
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm     -3.0235     1.5340  -1.971
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential  -3.0965     1.3473  -2.298
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm  -3.9895     1.0073  -3.961
# Pr(>|z|)    
# A.Mean.Sum2                                                                                0.025218 *  
# V.Mean.Sum2.directionpositive                                                              0.000667 ***
# fct_relevel(predicateType2, "emotive")cognitive                                            0.008537 ** 
# fct_relevel(predicateType2, "emotive")emoComm                                              2.60e-11 ***
# fct_relevel(predicateType2, "emotive")evidential                                           1.45e-11 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                                           1.23e-12 ***
# A.Mean.Sum2:V.Mean.Sum2.directionpositive                                                  3.25e-05 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                                0.779585    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                                  0.000393 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential                               0.206353    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm                               0.223900    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive              0.187100    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm                0.362205    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential             0.001602 ** 
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm             0.000782 ***
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive  0.073371 .  
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm    0.048723 *  
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential 0.021545 *  
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm 7.47e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -5.4885     0.5070 -10.825
# 0|1   -1.7692     0.5044  -3.508



# data frames for only negative/positive models
d.proj2.neg <- d.proj2 %>% filter(V.Mean.Sum2.direction == "negative")
nrow(d.proj2.neg) # 5536
d.proj2.pos <- d.proj2 %>% filter(V.Mean.Sum2.direction == "positive")
nrow(d.proj2.pos) # 7281

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2   n
# 1               negative      cognitive  14
# 2               negative        emoComm  29
# 3               negative        emotive  64
# 4               negative     evidential  17
# 5               negative     nonEmoComm  61

# 6               positive      cognitive  32
# 7               positive        emoComm  12
# 8               positive        emotive  37
# 9               positive     evidential  59
# 10              positive     nonEmoComm 103

###### negative valence ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj2.neg) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + 
#   (1 | environment)
# data:    d.proj2.neg
# 
# link  threshold nobs logLik   AIC     niter     max.grad cond.H 
# logit flexible  5536 -4514.00 9038.01 293(1473) 5.39e-03 1.1e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6790   0.8240  
# environment (Intercept) 0.2248   0.4742  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.1924     0.2483   8.831   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.1010     0.3041  -6.910
# 0|1    0.9844     0.3014   3.266

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj2.neg) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                   -0.9701     0.5121  -1.894  0.05817 .  
# fct_relevel(predicateType2, "emotive")cognitive               -2.1614     0.8672  -2.492  0.01269 *  
# fct_relevel(predicateType2, "emotive")emoComm                 -2.3653     0.4168  -5.674 1.39e-08 ***
# fct_relevel(predicateType2, "emotive")evidential              -2.7906     0.4420  -6.314 2.73e-10 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm              -2.1627     0.3356  -6.445 1.16e-10 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive    0.4910     2.1969   0.223  0.82317    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm      2.3866     0.8803   2.711  0.00671 ** 
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential   1.0132     0.9749   1.039  0.29868    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm   0.6547     0.6765   0.968  0.33319  
 

###### positive valence ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj2.pos) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                    2.0290     0.5986   3.390 0.000700 ***
# fct_relevel(predicateType2, "emotive")cognitive               -1.2462     0.3879  -3.212 0.001317 ** 
# fct_relevel(predicateType2, "emotive")emoComm                 -2.5019     0.6621  -3.779 0.000158 ***
# fct_relevel(predicateType2, "emotive")evidential              -1.3093     0.3833  -3.416 0.000635 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm              -0.9913     0.3308  -2.997 0.002730 ** 
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive   -3.4989     0.9205  -3.801 0.000144 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm      0.8182     1.2931   0.633 0.526898    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential  -1.8043     0.9347  -1.930 0.053565 .  
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm  -3.0534     0.7594  -4.021  5.8e-05 ***

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
       (1 | environment), data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
#   (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter     max.grad cond.H 
# logit flexible  12817 -9970.04 19954.07 557(2765) 6.24e-04 1.0e+03
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6829   0.8264  
# environment (Intercept) 0.4519   0.6722  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2               0.8755     0.3247   2.696  0.00701 **
# V.Mean.Sum2               0.9661     0.3786   2.552  0.01071 * 
# A.Mean.Sum2:V.Mean.Sum2   1.5885     0.8274   1.920  0.05488 . 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.3733     0.4135  -5.739
# 0|1    1.1157     0.4128   2.703

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emotive") + (1 | participant) + (1 | environment), 
     data = d.proj2) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
#   fct_relevel(predicateType2, "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj2
# 
# link  threshold nobs  logLik   AIC      niter       max.grad cond.H 
# logit flexible  12817 -9298.41 18642.82 3518(17546) 8.24e-04 4.8e+04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7912   0.8895  
# environment (Intercept) 0.5380   0.7335  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                          Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                               -1.7285     1.1109  -1.556 0.119716    
# V.Mean.Sum2                                                               -1.7239     1.0962  -1.573 0.115824    
# fct_relevel(predicateType2, "emotive")cognitive                           -4.7666     0.7417  -6.426 1.31e-10 ***
# fct_relevel(predicateType2, "emotive")emoComm                             -3.7346     0.7608  -4.909 9.15e-07 ***
# fct_relevel(predicateType2, "emotive")evidential                          -2.4985     0.6270  -3.985 6.75e-05 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                          -2.5897     0.5770  -4.488 7.18e-06 ***
# A.Mean.Sum2:V.Mean.Sum2                                                    3.7770     2.1992   1.717 0.085906 .  
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                6.0329     1.7844   3.381 0.000722 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                  4.1514     1.6364   2.537 0.011182 *  
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential               0.7733     1.4196   0.545 0.585931    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm               0.9905     1.2317   0.804 0.421281    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                7.9517     1.6901   4.705 2.54e-06 ***
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                  5.4070     1.8488   2.925 0.003448 ** 
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential               1.3441     1.6253   0.827 0.408225    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm               2.2152     1.4017   1.580 0.114025    
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive  -19.5512     4.0359  -4.844 1.27e-06 ***
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm     -9.3454     3.6288  -2.575 0.010013 *  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential  -2.1323     3.6509  -0.584 0.559190    
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm  -3.8938     2.9564  -1.317 0.187805    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -5.7500     0.6861  -8.381
# 0|1   -2.0355     0.6842  -2.975

##### positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
       (1 | environment), data = d.proj2.pos) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2              -0.1573     0.4570  -0.344    0.731    
# V.Mean.Sum2              -0.6874     0.4996  -1.376    0.169    
# A.Mean.Sum2:V.Mean.Sum2   4.9661     1.1372   4.367 1.26e-05 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emotive")+ (1 | participant) + (1 | environment), 
     data = d.proj2.pos) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
#   fct_relevel(predicateType2, "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj2.pos
# 
# link  threshold nobs logLik   AIC      niter       max.grad cond.H 
# logit flexible  7281 -5154.25 10354.50 6691(26840) 4.49e-01 7.6e+04
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7518   0.8671  
# environment (Intercept) 0.8056   0.8975  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                          Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                               -4.6269     1.8590  -2.489 0.012812 *  
# V.Mean.Sum2                                                               -5.2751     1.6688  -3.161 0.001572 ** 
# fct_relevel(predicateType2, "emotive")cognitive                           -5.9638     1.0166  -5.866 4.45e-09 ***
# fct_relevel(predicateType2, "emotive")emoComm                             -3.5637     1.6684  -2.136 0.032680 *  
# fct_relevel(predicateType2, "emotive")evidential                          -3.3507     0.9696  -3.456 0.000549 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                          -3.8522     0.8824  -4.365 1.27e-05 ***
# A.Mean.Sum2:V.Mean.Sum2                                                   13.2162     3.6156   3.655 0.000257 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive                8.4712     2.4767   3.420 0.000625 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                  4.4644     3.3479   1.333 0.182377    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential               3.1371     2.3123   1.357 0.174873    
# A.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm               3.7575     1.9993   1.879 0.060186 .  
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive               11.0554     2.1922   5.043 4.58e-07 ***
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm                  0.6984     4.0563   0.172 0.863306    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential               2.8567     2.3817   1.199 0.230360    
# V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm               6.4515     2.1208   3.042 0.002349 ** 
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")cognitive  -27.7129     5.3002  -5.229 1.71e-07 ***
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")emoComm     -5.0419     7.4321  -0.678 0.497524    
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")evidential  -6.6536     5.6962  -1.168 0.242771    
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emotive")nonEmoComm -14.4054     4.8492  -2.971 0.002971 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -7.2356     0.9851  -7.345
# 0|1   -3.1942     0.9821  -3.252


## possible issues with the analysis ----

# predicates in both active and passive sentence frames with same predicate type
d3[duplicated(d3[,cbind(2,4)]),] %>%
  select(verb, predicateType2)

#       verb predicateType2
# 195 grieve        emotive
# 251 marvel        emotive
# 276  panic        emotive
# 426  worry        emotive

# Of the 101 emotives with valence/arousal/dominance ratings, four predicates occur in both
# sentence frames in the MV data set. The "active" and "passive voice" versions of these 
# predicates were assigned the same valence/arousal/dominance ratings. Due to the small number
# of these cases, this should not affect the overall distributions investigated above.


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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
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
  select(predicateType2, verb, dynamicity) %>%
  unique() %>%
  group_by(predicateType2, dynamicity) %>%
  summarize(count=n())

mean.proj = d.proj %>%
  group_by(predicateType2, dynamicity) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
mean.proj
nrow(mean.proj) # 12

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 7

#### plot ----
ggplot(mean.proj, aes(x = predicateType2, y = Mean.Proj, colour = dynamicity)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating", 
       colour = "Dynamicity") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("dynamic" = "violetred3", "stative" = "aquamarine4"))
ggsave("../graphs/projection-by-predicateType-and-Dynamicity.pdf", height = 4, width = 10)


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
  select(c(verb, verb_renamed, predicateType, predicateType2, dynamicity)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, dynamicity)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

# how many stative predicates are in each predicate type category?
mean.proj %>% 
  filter(dynamicity == "stative") %>% 
  group_by(predicateType) %>% 
  summarise(n())
# # A tibble: 3 × 2
# predicateType `n()`
#   <chr>         <int>
# 1 cognitive        39
# 2 emotive         148
# 3 evidential        3

#### plots ----
# projection by predicate with dynamic predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
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
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType), 
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
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
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

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
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
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType) 
ggsave("../graphs/projection-by-predicate-stative-faceted.pdf", height = 4, width = 13)

#### linear models ----
lm(Mean.Proj ~ dynamicity, data = mean.proj) %>%  
  summary()
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.29798    0.01263   23.59   <2e-16 ***
# dynamicitystative  0.31997    0.02096   15.27   <2e-16 ***

lm(Mean.Proj ~ dynamicity + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.15311    0.04805   3.186  0.00153 ** 
# dynamicitystative           0.08586    0.05405   1.589  0.11278    
# predicateTypecommunicative  0.15480    0.04972   3.113  0.00195 ** 
# predicateTypeemotive        0.48367    0.03452  14.011  < 2e-16 ***
# predicateTypeevidential     0.14106    0.05110   2.761  0.00597 **   

lm(Mean.Proj ~ fct_relevel(dynamicity, "dynamic") * fct_relevel(predicateType2, "evidential"), 
   data = mean.proj) %>%  
  summary()
# Coefficients: (2 not defined because of singularities)
#                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   0.14310    0.05251   2.725  0.00665 ** 
# dynamicitystative                             0.09945    0.06122   1.625  0.10486    
# predicateTypecommunicative                    0.16481    0.05405   3.049  0.00241 ** 
# predicateTypeemotive                          0.48008    0.03537  13.574  < 2e-16 ***
# predicateTypeevidential                       0.15276    0.05677   2.691  0.00736 ** 
# dynamicitystative:predicateTypecommunicative       NA         NA      NA       NA    
# dynamicitystative:predicateTypeemotive             NA         NA      NA       NA    
# dynamicitystative:predicateTypeevidential    -0.06198    0.13070  -0.474  0.63554 

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ dynamicity + (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ dynamicity + (1 | participant) + 
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  15662 -11826.64 23663.29 357(1789) 1.13e-03 4.0e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6973   0.8350  
# environment (Intercept) 0.4342   0.6589  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                              Estimate Std. Error z value Pr(>|z|)    
# dynamicitystative  1.32238    0.03778      35   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.8612     0.3856  -7.420
# 0|1    0.6559     0.3844   1.706

# with predicate type
clmm(as.factor(veridicality_num) ~ dynamicity * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj) %>% 
  summary()
# design is column rank deficient so dropping 3 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ dynamicity * fct_relevel(predicateType2,  
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter       max.grad cond.H 
# logit flexible  15662 -11300.72 22621.43 2584(12925) 2.64e-03 7.9e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7771   0.8815  
# environment (Intercept) 0.4886   0.6990  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                   Estimate Std. Error z value Pr(>|z|)    
# dynamicitystative                                                  0.04538    0.22778   0.199    0.842    
# fct_relevel(predicateType2, "emotive")cognitive                   -2.49147    0.25572  -9.743  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                     -1.39887    0.23812  -5.875 4.24e-09 ***
# fct_relevel(predicateType2, "emotive")evidential                  -1.89680    0.22734  -8.343  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                  -1.93891    0.23299  -8.322  < 2e-16 ***
# dynamicitystative:fct_relevel(predicateType2, "emotive")cognitive  0.30347    0.25993   1.168    0.243    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -4.8409     0.4701 -10.297
# 0|1   -1.1928     0.4681  -2.548


## H3.2 change of state ----
# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
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
  group_by(predicateType2, changeOfState) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
mean.proj
nrow(mean.proj) # 9

# # how many predicates in which predicateType and CoS / no CoS?
d.proj %>%
  select(predicateType2, verb, changeOfState) %>%
  unique() %>%
  group_by(predicateType2, changeOfState) %>%
  summarize(count=n())
# predicateType2 changeOfState count
#   <chr>          <chr>         <int>
# 1 cognitive      no               51
# 2 cognitive      yes               2
# 3 comPriv        no                9
# 4 emoComm        no               47
# 5 emotive        no              143
# 6 evidential     no               48
# 7 evidential     yes              38
# 8 nonEmoComm     no              189
# 9 other          no               12

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 7

##### plot ----
ggplot(mean.proj, aes(x = predicateType2, y = Mean.Proj, colour = changeOfState)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Change-of-state predicate") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("no" = "darkblue", "yes" = "gold3"))
ggsave("../graphs/projection-by-predicateType-and-CoS.pdf", height = 4, width = 10)

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

# how many change-of-state predicates are in each predicate type category?
mean.proj %>% 
  filter(changeOfState == "yes") %>% 
  group_by(predicateType) %>% 
  summarise(n())
# # A tibble: 2 × 2
# predicateType `n()`
#   <chr>         <int>
# 1 cognitive         2 <<< !!!
# 2 evidential       38

# The small number of CoS predicates amongst the cognitives is the reason for the
# different pattern.

#### plots -----
# projection by predicate with change-of-state predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
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
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
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
# (Intercept)       0.42129    0.01257  33.520   <2e-16 ***
# changeOfStateyes -0.09250    0.04545  -2.035   0.0423 * 

lm(Mean.Proj ~ changeOfState + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.21396    0.02702   7.918 1.47e-14 ***
# changeOfStateyes            0.06168    0.04083   1.511  0.13152    
# predicateTypecommunicative  0.09395    0.02989   3.143  0.00177 ** 
# predicateTypeemotive        0.50868    0.03148  16.161  < 2e-16 ***
# predicateTypeevidential     0.05595    0.03806   1.470  0.14211

lm(Mean.Proj ~ changeOfState * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (2 not defined because of singularities)
#                                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                  0.19144    0.02701   7.088 4.47e-12 ***
# changeOfStateyes                             0.65856    0.13903   4.737 2.81e-06 ***
# predicateTypecommunicative                   0.11647    0.02978   3.911 0.000104 ***
# predicateTypeemotive                         0.53120    0.03132  16.963  < 2e-16 ***
# predicateTypeevidential                      0.10241    0.03879   2.640 0.008530 ** 
# changeOfStateyes:predicateTypecommunicative       NA         NA      NA       NA    
# changeOfStateyes:predicateTypeemotive             NA         NA      NA       NA    
# changeOfStateyes:predicateTypeevidential    -0.65105    0.14520  -4.484 9.03e-06 *** 

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ changeOfState + (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ changeOfState + (1 | participant) +  
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  15662 -12467.87 24945.74 277(1383) 9.09e-05 3.6e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6215   0.7883  
# environment (Intercept) 0.3817   0.6178  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# changeOfStateyes -0.38345    0.06216  -6.169 6.86e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -3.1691     0.3618  -8.759
# 0|1    0.1519     0.3603   0.422

# with predicate type
clmm(as.factor(veridicality_num) ~ changeOfState * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj) %>% 
  summary()
# design is column rank deficient so dropping 3 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ changeOfState * fct_relevel(predicateType2,  
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad cond.H 
# logit flexible  15662 -11255.08 22530.16 1180(5894) 3.75e-03 4.2e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7859   0.8865  
# environment (Intercept) 0.4943   0.7030  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                    Estimate Std. Error z value Pr(>|z|)    
# changeOfStateyes                                                  0.0005463  0.0845258   0.006    0.995    
# fct_relevel(predicateType2, "emotive")cognitive                  -2.3805307  0.0683504 -34.828  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                    -1.4488496  0.0690772 -20.974  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential                 -1.9441683  0.0691270 -28.125  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                 -1.9885229  0.0488169 -40.734  < 2e-16 ***
# changeOfStateyes:fct_relevel(predicateType2, "emotive")cognitive  3.1627527  0.3961746   7.983 1.43e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -4.9027     0.4135 -11.856
# 0|1   -1.2395     0.4112  -3.015


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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
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
  group_by(predicateType2, activity) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
mean.proj
nrow(mean.proj) # 13

# how many predicates in which predicateType and activity / no activity?
d.proj %>%
  select(predicateType2, verb, activity) %>%
  unique() %>%
  group_by(predicateType2, activity) %>%
  summarize(count=n())
# predicateType2 activity count
#   <chr>          <chr>    <int>
# 1 cognitive      no          41
# 2 cognitive      yes         12
# 3 comPriv        no           7
# 4 comPriv        yes          2
# 5 emoComm        no           3
# 6 emoComm        yes         44
# 7 emotive        no         143
# 8 evidential     no          74
# 9 evidential     yes         12
# 10 nonEmoComm     no          94
# 11 nonEmoComm     yes         95
# 12 other          no          10
# 13 other          yes          2

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 9

#### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, colour = activity)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Activity") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("no" = "slateblue", "yes" = "red3"))
ggsave("../graphs/projection-by-predicateType-and-activity.pdf", height = 4, width = 10)

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

# how many activity predicates are in each predicate type category?
mean.proj %>% 
  filter(activity == "yes") %>% 
  group_by(predicateType2) %>% 
  summarise(n())
# # A tibble: 4 × 2
# predicateType2 `n()`
#   <chr>          <int>
# 1 cognitive         12
# 2 emoComm           44
# 3 evidential        12
# 4 nonEmoComm        95

#### plots ----
# projection by predicate with activity predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_point(data = mean.proj %>% filter(activity == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = activity), 
             size = 4,  shape = 1, colour = "red3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("activity")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-activity.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
  geom_point(data = mean.proj %>% filter(activity == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = activity), 
             size = 4,  shape = 1, colour = "red3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
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

lm(Mean.Proj ~ activity + predicateType2, data = mean.proj) %>%  
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|) 
# (Intercept)               0.21961    0.02702   8.129 3.22e-15 ***
# activityyes              -0.01468    0.02312  -0.635   0.5257    
# predicateType2emoComm     0.21908    0.04200   5.216 2.64e-07 ***
# predicateType2emotive     0.50303    0.03133  16.057  < 2e-16 ***
# predicateType2evidential  0.07960    0.03375   2.358   0.0187 *  
# predicateType2nonEmoComm  0.06657    0.03066   2.171   0.0304 *   

lm(Mean.Proj ~ activity * fct_relevel(predicateType2, "cognitive"), data = mean.proj) %>%  
  summary()
# Coefficients: (1 not defined because of singularities)
#                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.272190   0.029703   9.164  < 2e-16 ***
# activityyes                          -0.246902   0.062424  -3.955 8.72e-05 ***
# predicateType2emoComm                 0.205588   0.113754   1.807  0.07130 .  
# predicateType2emotive                 0.450450   0.033566  13.420  < 2e-16 ***
# predicateType2evidential              0.011905   0.037028   0.322  0.74796    
# predicateType2nonEmoComm              0.001716   0.035596   0.048  0.96157    
# activityyes:predicateType2emoComm     0.190467   0.129524   1.471  0.14204    
# activityyes:predicateType2emotive           NA         NA      NA       NA    
# activityyes:predicateType2evidential  0.340585   0.086023   3.959 8.58e-05 ***
# activityyes:predicateType2nonEmoComm  0.256651   0.068281   3.759  0.00019 ***

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ activity + (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ activity + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  15662 -12348.98 24707.97 288(1445) 6.14e-04 3.7e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6339   0.7962  
# environment (Intercept) 0.3889   0.6237  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# activityyes -0.59887    0.03622  -16.53   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#       Estimate Std. Error z value
# -1|0 -3.368996   0.365502  -9.217
# 0|1  -0.007945   0.363860  -0.022

# with predicate type
clmm(as.factor(veridicality_num) ~ activity * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj) %>% 
  summary()
# design is column rank deficient so dropping 1 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ activity * fct_relevel(predicateType2,  
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad cond.H 
# logit flexible  15662 -11274.84 22573.68 1556(7772) 4.39e-03 4.3e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7825   0.8846  
# environment (Intercept) 0.4902   0.7001  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)    
# activityyes                                                   0.02590    0.05607   0.462   0.6441    
# fct_relevel(predicateType2, "emotive")cognitive              -2.06924    0.07262 -28.495  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                -1.38744    0.23340  -5.944 2.78e-09 ***
# fct_relevel(predicateType2, "emotive")evidential             -1.98268    0.06042 -32.815  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm             -2.00045    0.05623 -35.576  < 2e-16 ***
# activityyes:fct_relevel(predicateType2, "emotive")cognitive  -0.97880    0.14101  -6.941 3.88e-12 ***
# activityyes:fct_relevel(predicateType2, "emotive")emoComm    -0.08912    0.24447  -0.365   0.7155    
# activityyes:fct_relevel(predicateType2, "emotive")evidential  0.24774    0.13279   1.866   0.0621 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -4.9019     0.4119 -11.902
# 0|1   -1.2388     0.4095  -3.025



## H3.4 dynamic/activity/CoS comparison ----
### H3.4.1 overall ----
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         stateActivityCoS = case_when(stative_predicate == "yes" ~ "state",
                                      change_of_state_predicate == "yes" ~ 
                                        "change-of-state predicate",
                                      activity_predicate == "yes" ~ "activity",
                                      TRUE ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))


#### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType2, stateActivityCoS) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj
nrow(mean.proj) # 18

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 12

##### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, 
                      colour = fct_relevel(stateActivityCoS, 
                                           "activity", "change-of-state predicate",
                                           "state", "other"))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
    theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Type of predicate") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"))
ggsave("../graphs/projection-by-predicateType-and-stateActivityCoS.pdf", height = 4, width = 10)

#### by predicate ----
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

mean.proj %>%
  select(predicateType2, verb_renamed, stateActivityCoS) %>%
  group_by(predicateType2, stateActivityCoS) %>%
  summarize(count=n()) %>% 
  pivot_wider(names_from = stateActivityCoS, values_from = count)
#   predicateType2 activity `change-of-state predicate` state other
#   <chr>             <int>                       <int> <int> <int>
# 1 cognitive            12                           2    39    NA
# 2 emoComm              44                          NA    NA     3
# 3 emotive              NA                          NA   148    NA
# 4 evidential           12                          38     3    33
# 5 nonEmoComm           95                          NA    NA    94


##### plots ----
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
        plot.margin = unit(c(5.5, 44, 5.5, 44), "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"), 
                      breaks = c("activity", "change-of-state predicate", "state", "other")) +
  facet_wrap(~ fct_relevel(stateActivityCoS, "activity", "change-of-state predicate",
                           "state", "other"), ncol = 4) 
ggsave("../graphs/projection-by-predicate-stateActivityCoS-faceted.pdf", height = 4, width = 10)


##### linear models ----
lm(Mean.Proj ~ fct_relevel(stateActivityCoS, "other"), data = mean.proj) %>%  
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.30873    0.01805  17.106   <2e-16 ***
# stateActivityCoSchange-of-state predicate  0.02006    0.04066   0.493    0.622    
# stateActivityCoSother                     -0.03372    0.02710  -1.244    0.214    
# stateActivityCoSstate                      0.30922    0.02460  12.570   <2e-16 ***

lm(Mean.Proj ~ stateActivityCoS + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.14651    0.04874   3.006 0.002775 ** 
# stateActivityCoSchange-of-state predicate  0.04743    0.04406   1.076 0.282240    
# stateActivityCoSother                     -0.04033    0.02378  -1.696 0.090574 .  
# stateActivityCoSstate                      0.09239    0.05480   1.686 0.092427 .  
# predicateTypecommunicative                 0.17797    0.05044   3.529 0.000455 ***
# predicateTypeemotive                       0.48374    0.03440  14.061  < 2e-16 ***
# predicateTypeevidential                    0.14195    0.05327   2.665 0.007944 ** 

lm(Mean.Proj ~ stateActivityCoS * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (6 not defined because of singularities)
#                                                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                           0.02529    0.05488   0.461 0.645183    
# stateActivityCoSchange-of-state predicate                             0.82471    0.14521   5.679 2.27e-08 ***
# stateActivityCoSother                                                -0.11804    0.06409  -1.842 0.066084 .  
# stateActivityCoSstate                                                 0.21727    0.06276   3.462 0.000581 ***
# predicateTypecommunicative                                            0.30195    0.05720   5.278 1.93e-07 ***
# predicateTypeemotive                                                  0.48008    0.03422  14.029  < 2e-16 ***
# predicateTypeevidential                                               0.35249    0.07762   4.541 6.97e-06 ***
# stateActivityCoSchange-of-state predicate:predicateTypecommunicative       NA         NA      NA       NA    
# stateActivityCoSother:predicateTypecommunicative                      0.07101    0.06885   1.031 0.302826    
# stateActivityCoSstate:predicateTypecommunicative                           NA         NA      NA       NA    
# stateActivityCoSchange-of-state predicate:predicateTypeemotive             NA         NA      NA       NA    
# stateActivityCoSother:predicateTypeemotive                                 NA         NA      NA       NA    
# stateActivityCoSstate:predicateTypeemotive                                 NA         NA      NA       NA    
# stateActivityCoSchange-of-state predicate:predicateTypeevidential    -0.90113    0.15827  -5.694 2.10e-08 ***
# stateActivityCoSother:predicateTypeevidential                              NA         NA      NA       NA    
# stateActivityCoSstate:predicateTypeevidential                        -0.26172    0.13784  -1.899 0.058172 .

##### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ fct_relevel(stateActivityCoS, "activity") + (1 | participant) +
       (1 | environment), data = d.proj) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ stateActivityCoS + (1 | participant) + 
#   (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter      max.grad cond.H 
# logit flexible  15662 -11822.61 23659.22 1059(5299) 5.20e-03 4.1e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.6959   0.8342  
# environment (Intercept) 0.4342   0.6589  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# stateActivityCoSchange-of-state predicate  0.05124    0.06763   0.758   0.4487    
# stateActivityCoSother                     -0.10748    0.04498  -2.390   0.0169 *  
# stateActivityCoSstate                      1.28655    0.04340  29.642   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.8981     0.3862  -7.504
# 0|1    0.6204     0.3850   1.612


# with predicate type
clmm(as.factor(veridicality_num) ~ stateActivityCoS * fct_relevel(predicateType2, "emotive")
     + (1 | participant) + (1 | environment), data = d.proj) %>% 
  summary()
# design is column rank deficient so dropping 8 coef
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ stateActivityCoS * fct_relevel(predicateType2,  
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj
# 
# link  threshold nobs  logLik    AIC      niter       max.grad cond.H 
# logit flexible  15662 -11230.91 22491.83 3588(17945) 3.81e-03 9.8e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7907   0.8892  
# environment (Intercept) 0.4951   0.7036  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                                                           Estimate Std. Error z value
# stateActivityCoSchange-of-state predicate                                                 -0.24133    0.12808  -1.884
# stateActivityCoSother                                                                     -0.02517    0.05615  -0.448
# stateActivityCoSstate                                                                     -0.20050    0.25039  -0.801
# fct_relevel(predicateType2, "emotive")cognitive                                           -3.22954    0.27958 -11.552
# fct_relevel(predicateType2, "emotive")emoComm                                             -1.65548    0.26028  -6.360
# fct_relevel(predicateType2, "emotive")evidential                                          -1.90561    0.22787  -8.363
# fct_relevel(predicateType2, "emotive")nonEmoComm                                          -2.17944    0.25686  -8.485
# stateActivityCoSchange-of-state predicate:fct_relevel(predicateType2, "emotive")cognitive  4.06145    0.42084   9.651
# stateActivityCoSstate:fct_relevel(predicateType2, "emotive")cognitive                      1.03630    0.28323   3.659
# stateActivityCoSother:fct_relevel(predicateType2, "emotive")emoComm                        0.08913    0.24468   0.364
# stateActivityCoSother:fct_relevel(predicateType2, "emotive")evidential                    -0.31249    0.14276  -2.189
# Pr(>|z|)    
# stateActivityCoSchange-of-state predicate                                                 0.059526 .  
# stateActivityCoSother                                                                     0.654027    
# stateActivityCoSstate                                                                     0.423270    
# fct_relevel(predicateType2, "emotive")cognitive                                            < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                                             2.01e-10 ***
# fct_relevel(predicateType2, "emotive")evidential                                           < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                                           < 2e-16 ***
# stateActivityCoSchange-of-state predicate:fct_relevel(predicateType2, "emotive")cognitive  < 2e-16 ***
# stateActivityCoSstate:fct_relevel(predicateType2, "emotive")cognitive                     0.000253 ***
# stateActivityCoSother:fct_relevel(predicateType2, "emotive")emoComm                       0.715653    
# stateActivityCoSother:fct_relevel(predicateType2, "emotive")evidential                    0.028602 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -5.1180     0.4839  -10.57
# 0|1   -1.4407     0.4818   -2.99


### H3.4.2 evidentials only ----
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         stateActivityCoS = case_when(stative_predicate == "yes" ~ "state",
                                      change_of_state_predicate == "yes" ~ 
                                        "change-of-state predicate",
                                      activity_predicate == "yes" ~ "activity",
                                      TRUE ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"),
         evidenceType = case_when(perceptual_evidence == "yes" ~ "perceptual",
                                    reportative_evidence == "yes" ~	"reportative",
                                    inferential_evidence == "yes" ~ "inferential",
                                    TRUE ~ NA))

d.proj = droplevels(subset(d.proj, predicateType == "evidential"))
nrow(d.proj) # 2635

# how many predicates in which evidence type?
d.proj %>%
  select(evidenceType, verb_renamed) %>% 
  unique() %>% 
  group_by(evidenceType) %>% 
  summarize(count=n())
# # A tibble: 3 × 2
# evidenceType count
#   <chr>        <int>
# 1 inferential     40
# 2 perceptual      10
# 3 reportative     36

# how many predicates in which type of predicate?
d.proj %>%
  select(stateActivityCoS, verb_renamed) %>% 
  unique() %>% 
  group_by(stateActivityCoS) %>% 
  summarize(count=n())
# # A tibble: 4 × 2
# stateActivityCoS          count
# <chr>                     <int>
# 1 activity                     12
# 2 change-of-state predicate    37
# 3 other                        33
# 4 state                         3

#### by evidence type ----
mean.proj = d.proj %>%
  group_by(evidenceType, stateActivityCoS) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         evidenceType = fct_reorder(as.factor(evidenceType), Mean.Proj))
mean.proj
nrow(mean.proj) # 9

##### plot ----
ggplot(mean.proj, aes(x = evidenceType, y = Mean.Proj, 
                      colour = fct_relevel(stateActivityCoS, 
                                           "activity", "change-of-state predicate",
                                           "state", "other"))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Evidence type",
       colour = "Type of predicate") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"))
ggsave("../graphs/projection-by-evidenceType-and-stateActivityCoS.pdf", height = 4, width = 10)

# only evidence type
mean.proj = d.proj %>%
  group_by(evidenceType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         evidenceType = fct_reorder(as.factor(evidenceType), Mean.Proj))
mean.proj
nrow(mean.proj) # 3

ggplot(mean.proj, aes(x = evidenceType, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Evidence type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) 
ggsave("../graphs/projection-by-evidenceType.pdf", height = 4, width = 10)


#### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) # 86

# add verb and evidence type to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, evidenceType, stateActivityCoS)) %>%
  distinct(verb, verb_renamed, evidenceType, stateActivityCoS)
nrow(tmp) # 86

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 86

mean.proj %>%
  select(evidenceType, verb_renamed, stateActivityCoS) %>%
  group_by(evidenceType, stateActivityCoS) %>%
  summarize(count=n()) %>% 
  pivot_wider(names_from = stateActivityCoS, values_from = count)
# evidenceType activity `change-of-state predicate` other state
#   <chr>           <int>                       <int> <int> <int>
# 1 inferential         3                          35     2    NA
# 2 perceptual          4                          NA     3     3
# 3 reportative         5                           3    28    NA

##### plots ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = stateActivityCoS)) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Evidential predicate",
       colour = "Type of predicate") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"),
                      breaks = c("activity", "change-of-state predicate", "state", "other"))
ggsave("../graphs/projection-by-evidential-stateActivityCoS.pdf", height = 4, width = 13)


##### linear model ----
lm(Mean.Proj ~ fct_relevel(stateActivityCoS, "other"), data = mean.proj) %>%  
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.37778    0.06869   5.500 4.21e-07 ***
# fct_relevel(stateActivityCoS, "activity")change-of-state predicate -0.07642    0.07879  -0.970    0.335    
# fct_relevel(stateActivityCoS, "activity")other                     -0.11804    0.08021  -1.472    0.145    
# fct_relevel(stateActivityCoS, "activity")state                     -0.04444    0.15359  -0.289    0.773    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## activity, CoS and other *** significant, state ** significant

##### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(stateActivityCoS, "activity") 
     + (1 | participant) + (1 | environment), data = d.proj) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error z value Pr(>|z|)   
# fct_relevel(stateActivityCoS, "activity")change-of-state predicate  -0.2529     0.1286  -1.966  0.04926 * 
# fct_relevel(stateActivityCoS, "activity")other                      -0.3706     0.1329  -2.789  0.00529 **
# fct_relevel(stateActivityCoS, "activity")state                      -0.1762     0.2493  -0.707  0.47966  

# Interpretation?


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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
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
nrow(mean.proj.verid) # 523

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

ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2,
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality2.pdf", height = 4, width = 7)

# faceted
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")
ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(18, "pt")) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2) +
  coord_fixed(ratio = 1) + 
  facet_wrap( ~ predicateType2, labeller = as_labeller(predicateType2_names), ncol = 5)
ggsave("../graphs/projection-by-veridicality2-faceted.pdf", height = 3, width = 10.5)


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
# (Intercept)               0.11581    0.02011   5.759 1.45e-08 ***
# Mean.Verid                0.38433    0.01840  20.889  < 2e-16 ***
# predicateType2emoComm     0.04019    0.02960   1.358   0.1751    
# predicateType2emotive     0.29319    0.02494  11.757  < 2e-16 ***
# predicateType2evidential -0.02129    0.02530  -0.841   0.4005    
# predicateType2nonEmoComm -0.04670    0.02270  -2.057   0.0402 * 

lm(Mean.Proj ~ Mean.Verid * predicateType2, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          0.067958   0.022572   3.011  0.00273 ** 
# Mean.Verid                           0.567387   0.046311  12.252  < 2e-16 ***
# predicateType2emoComm                0.024967   0.062629   0.399  0.69032    
# predicateType2emotive                0.376074   0.048455   7.761 4.59e-14 ***
# predicateType2evidential            -0.002378   0.033785  -0.070  0.94391    
# predicateType2nonEmoComm             0.048083   0.028695   1.676  0.09441 .  
# Mean.Verid:predicateType2emoComm    -0.092912   0.090988  -1.021  0.30766    
# Mean.Verid:predicateType2emotive    -0.225984   0.068630  -3.293  0.00106 ** 
# Mean.Verid:predicateType2evidential -0.128161   0.060110  -2.132  0.03347 *  
# Mean.Verid:predicateType2nonEmoComm -0.269076   0.053454  -5.034 6.67e-07 ***


### ordinal models ----
d.proj.mean.verid = left_join(d.proj, mean.verid, by = c("verb_renamed")) %>%
  distinct()
nrow(d.proj.mean.verid) # 16291

# remove "other" and "comPriv" predicates
d.proj.mean.verid = d.proj.mean.verid %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(d.proj.mean.verid) # 15662

clmm(as.factor(veridicality_num) ~ Mean.Verid + (1 | participant) + (1 | environment), 
     data = d.proj.mean.verid) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ Mean.Verid + (1 | participant) + 
#   (1 | environment)
# data:    d.proj.mean.verid
# 
# link  threshold nobs  logLik    AIC      niter     max.grad cond.H 
# logit flexible  15662 -11471.23 22952.46 312(1565) 4.46e-04 3.9e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.7168   0.8467  
# environment (Intercept) 0.4389   0.6625  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
# Mean.Verid  2.17693    0.05093   42.74   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -2.1511     0.3881  -5.542
# 0|1    1.5347     0.3877   3.959

# with predicate type
clmm(as.factor(veridicality_num) ~ Mean.Verid * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant) + (1 | environment), data = d.proj.mean.verid) %>% 
  summary()
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: as.factor(veridicality_num) ~ Mean.Verid * fct_relevel(predicateType2,  
#   "emotive") + (1 | participant) + (1 | environment)
# data:    d.proj.mean.verid
# 
# link  threshold nobs  logLik    AIC      niter      max.grad cond.H 
# logit flexible  15662 -10810.07 21646.13 1528(7645) 1.18e-02 5.2e+02
# 
# Random effects:
# Groups      Name        Variance Std.Dev.
# participant (Intercept) 0.8387   0.9158  
# environment (Intercept) 0.5218   0.7224  
# Number of groups:  participant 290,  environment 3 
# 
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)    
# Mean.Verid                                                   1.96865    0.15938  12.352  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive             -1.41422    0.14597  -9.689  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm               -1.20913    0.21043  -5.746 9.13e-09 ***
# fct_relevel(predicateType2, "emotive")evidential            -1.38650    0.15062  -9.205  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm            -1.15368    0.14108  -8.177 2.90e-16 ***
# Mean.Verid:fct_relevel(predicateType2, "emotive")cognitive   0.45539    0.21133   2.155   0.0312 *  
# Mean.Verid:fct_relevel(predicateType2, "emotive")emoComm    -0.09073    0.27474  -0.330   0.7412    
# Mean.Verid:fct_relevel(predicateType2, "emotive")evidential -0.18880    0.19632  -0.962   0.3362    
# Mean.Verid:fct_relevel(predicateType2, "emotive")nonEmoComm -0.73462    0.17850  -4.115 3.86e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#      Estimate Std. Error z value
# -1|0  -3.5352     0.4422  -7.995
# 0|1    0.3249     0.4403   0.738



## H4.2 with "factives" ----
# mean projection by mean veridicality with canonically projective predicates highlighted
### plot ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(data = mean.proj.verid %>% 
               filter(canonicallyProjective == "yes"),
             aes(x = Mean.Verid, y = Mean.Proj, fill = "canonically projective predicate"), 
             size = 3, colour = "red", alpha = 0.7) +
  geom_point(data = mean.proj.verid, 
             aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2), alpha = 0.7) +
  geom_smooth(data = mean.proj.verid, 
              aes(x = Mean.Verid, y = Mean.Proj, colour = "grey10"), 
              method = "lm", se = FALSE, show.legend = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type",
       fill = element_blank()) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2,
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality-with-factives2.pdf", height = 4, width = 7)

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



# H5: environment ----
## by predicate type ----
# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))

#### plot ----
# calculate by predicate type means
mean.proj = d.proj %>%
  group_by(predicateType2, environment) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, Mean.Proj)
mean.proj
nrow(mean.proj) # 21

mean.proj = mean.proj %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv")
nrow(mean.proj) # 15

ggplot(mean.proj, aes(x = predicateType2, y = Mean.Proj, colour = environment)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Environment") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("deeppink", "chocolate4", "turquoise"),
                      labels = c("negation", "question + conditional", 
                                 "question + conditional + negation"))
ggsave("../graphs/projection-by-predicateType-and-environment.pdf", height = 4, width = 10)

ggplot(mean.proj, aes(x = environment, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Environment",
       colour = "Predicate type") +
  scale_x_discrete(labels = c("negation", "question +\nconditional", "question +\nconditional +\nnegation")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols2,
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component"))
ggsave("../graphs/projection-by-environment-and-predicateType.pdf", height = 4, width = 10)

## by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
mean.proj
nrow(mean.proj) # 544
levels(mean.proj$verb_renamed)

# add predicateType and verb to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, environment)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, environment)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 1569

# how many predicates in which environment?
mean.proj %>%
  select(environment, verb_renamed) %>% 
  unique() %>% 
  group_by(environment) %>% 
  summarize(count=n())
#   environment count
#   <chr>       <int>
# 1 neg           523
# 2 q+cond        523
# 3 q+cond+neg    523


#### linear model ----
lm(Mean.Proj ~ fct_relevel(environment, "neg"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                4.142e-01  1.211e-02   34.19   <2e-16 ***
# fct_relevel(environment, "neg")q+cond      1.120e-17  1.713e-02    0.00        1    
# fct_relevel(environment, "neg")q+cond+neg -1.030e-17  1.713e-02    0.00        1    

# (Intercept)                                  4.142e-01  1.211e-02   34.19   <2e-16 ***
# fct_relevel(environment, "q+cond")neg        9.212e-17  1.713e-02    0.00        1    
# fct_relevel(environment, "q+cond")q+cond+neg 5.492e-17  1.713e-02    0.00        1   

# (Intercept)                                  4.142e-01  1.211e-02   34.19   <2e-16 ***
# fct_relevel(environment, "q+cond+neg")neg    8.311e-17  1.713e-02    0.00        1    
# fct_relevel(environment, "q+cond+neg")q+cond 3.690e-17  1.713e-02    0.00        1  

lm(Mean.Proj ~ fct_relevel(environment, "neg") * fct_relevel(predicateType2, "nonEmoComm"),
   data = mean.proj) %>% 
  summary()
# emotives: all environment *** significant
# cognitives: all environments *** significant
# evidential: all environments *** significant
# emoComm: all environments *** significant
# nonEmoComm: all environments *** significant

# Interpretation?


#### plots ----
##### neg environment ----
d.proj.neg <- d.proj %>% 
  filter(environment == "neg")
nrow(d.proj.neg) # 5411

# calculate by-predicate projection means 
mean.proj.neg = d.proj.neg %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj.neg) # 544

# add predicateType and verb to the means
tmp = d.proj.neg %>%
  select(c(verb, verb_renamed, predicateType, predicateType2)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2)
nrow(tmp) # 544

mean.proj.neg = left_join(mean.proj.neg, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.neg) # 544

# remove "other" and "comPriv" predicates
mean.proj.neg = mean.proj.neg %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.neg) # 523

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

a <- ggplot(mean.proj.neg, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.6) +
  theme(plot.margin = unit(c(38 , 5.5, 5.5, 5.5), "pt"),
        legend.position = c(0.48, 1.25),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.75, "cm"),
        plot.title = element_text(hjust = .01),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "deeppink", linewidth = 2)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       title = "Negation") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap( ~ predicateType2, ncol = 5)

##### q+cond environment ----
d.proj.qcond <- d.proj %>% 
  filter(environment == "q+cond")
nrow(d.proj.qcond) # 5440

# calculate by-predicate projection means 
mean.proj.qcond = d.proj.qcond %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj.qcond) # 544

# add predicateType and verb to the means
tmp = d.proj.qcond %>%
  select(c(verb, verb_renamed, predicateType, predicateType2)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2)
nrow(tmp) # 544

mean.proj.qcond = left_join(mean.proj.qcond, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.qcond) # 544

# remove "other" and "comPriv" predicates
mean.proj.qcond = mean.proj.qcond %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.qcond) # 523

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

b <- ggplot(mean.proj.qcond, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.6) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(hjust = .01),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "chocolate4", linewidth = 2)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       title = "Question + conditional") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap( ~ predicateType2, ncol = 5)

##### q+cond+neg environment ----
d.proj.qcondneg <- d.proj %>% 
  filter(environment == "q+cond+neg")
nrow(d.proj.qcondneg) # 5440

# calculate by-predicate projection means 
mean.proj.qcondneg = d.proj.qcondneg %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj.qcondneg) # 544

# add predicateType and verb to the means
tmp = d.proj.qcondneg %>%
  select(c(verb, verb_renamed, predicateType, predicateType2)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2)
nrow(tmp) # 544

mean.proj.qcondneg = left_join(mean.proj.qcondneg, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.qcondneg) # 544

# remove "other" and "comPriv" predicates
mean.proj.qcondneg = mean.proj.qcondneg %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.qcondneg) # 523

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

c <- ggplot(mean.proj.qcondneg, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.6) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(hjust = .01),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "turquoise", linewidth = 2)) +
  labs(x = "Predicate",
       y = "Mean projection rating", 
       title = "Question + conditional + negation") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_wrap( ~ predicateType2, 
              labeller = as_labeller(predicateType2_names), ncol = 5)

##### combined plot ----
a / b / c + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-predicateType-and-environment-faceted.pdf", 
       height = 10, width = 10)

## ordinal models ----
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ fct_relevel(environment, "q+cond") + (1 | participant), 
     data = d.proj) %>% 
  summary()

# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(environment, "neg")q+cond      0.83178    0.06940  11.985   <2e-16 ***
# fct_relevel(environment, "neg")q+cond+neg -0.67112    0.06832  -9.824   <2e-16 ***

# fct_relevel(environment, "q+cond")neg        -0.83179    0.06942  -11.98   <2e-16 ***
# fct_relevel(environment, "q+cond")q+cond+neg -1.50291    0.04274  -35.16   <2e-16 ***

# fct_relevel(environment, "q+cond+neg")neg     0.67111    0.06834    9.82   <2e-16 ***
# fct_relevel(environment, "q+cond+neg")q+cond  1.50291    0.04274   35.17   <2e-16 ***

clmm(as.factor(veridicality_num) ~ fct_relevel(environment, "q+cond") * 
       fct_relevel(predicateType2, "cognitive") + (1 | participant), data = d.proj) %>% 
  summary()

# most interactions are *** significant.

# emotives:
#                                                  z value Pr(>|z|)    
# fct_relevel(environment, "neg")q+cond              3.034 0.002413 ** 
# fct_relevel(environment, "neg")q+cond+neg         -9.124  < 2e-16 ***

# fct_relevel(environment, "q+cond")neg             -3.034 0.002410 ** 
# fct_relevel(environment, "q+cond")q+cond+neg     -14.211  < 2e-16 ***

# fct_relevel(environment, "q+cond+neg")neg          9.127  < 2e-16 ***
# fct_relevel(environment, "q+cond+neg")q+cond      14.212  < 2e-16 ***

# cognitives:

# fct_relevel(environment, "neg")q+cond                     0.25600    
# fct_relevel(environment, "neg")q+cond+neg                 0.00105 **  

# fct_relevel(environment, "q+cond")neg              1.134  0.25688    
# fct_relevel(environment, "q+cond")q+cond+neg      -2.332  0.01968 * 

# fct_relevel(environment, "q+cond+neg")neg          3.277  0.00105 ** 
# fct_relevel(environment, "q+cond+neg")q+cond       2.338  0.01940 *




# >>> NEGATION ONLY ----------------------------------------------------------------------------
# copy of code above with only negation as embedding environment. 

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
library(patchwork)
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
d.proj = droplevels(subset(d,d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"))

table(d.proj$predicateType)

### by-predicateType ----
#### plots ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj))
mean.proj
nrow(mean.proj) # 6
levels(mean.proj$predicateType)

mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
  ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 5.5, 22, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType-NO.pdf", height = 4, width = 10)

# calculate by-predicateType2 means
mean.proj = d.proj %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj
nrow(mean.proj) # 7
levels(mean.proj$predicateType2)

mean.proj %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
  ggplot(aes(
    x = factor(predicateType2, c("cognitive", "evidential", "nonEmoComm", 
                                 "emoComm", "emotive")), 
    y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = c("cognitive", "evidential", 
                              "communicative without\nemotive component", 
                              "communicative with\nemotive component", "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2-NO.pdf", height = 4, width = 10)




### by-predicate ----
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

# add predicateType and verb to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# how many predicates in which type?
mean.proj %>%
  select(predicateType, verb_renamed) %>% 
  unique() %>% 
  group_by(predicateType) %>% 
  summarize(count=n())
# predicateType count
#   <chr>         <int>
# 1 cognitive        53
# 2 comPriv           9
# 3 communicative   236
# 4 emotive         148
# 5 evidential       86
# 6 other            12

# predicateType2 count
#   <chr>          <int>
# 1 cognitive         53
# 2 comPriv            9
# 3 emoComm           47
# 4 emotive          148
# 5 evidential        86
# 6 nonEmoComm       189
# 7 other             12


# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

#### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-NO.pdf", height = 4, width = 13)

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating", 
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType, ncol = 4)
ggsave("../graphs/projection-by-predicate-faceted-NO.pdf", height = 4, width = 9)

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.8) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap( ~ predicateType2, 
              labeller = as_labeller(predicateType2_names), ncol = 5)
ggsave("../graphs/projection-by-predicate-faceted2-NO.pdf", height = 4, width = 10)



#### linear models ----
lm(Mean.Proj ~ fct_relevel(predicateType, "emotive"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                         0.73904    0.02014   36.69   <2e-16 ***
# fct_relevel(predicateType, "emotive")cognitive     -0.50864    0.03922  -12.97   <2e-16 ***
# fct_relevel(predicateType, "emotive")communicative -0.52689    0.02569  -20.51   <2e-16 ***
# fct_relevel(predicateType, "emotive")evidential    -0.46798    0.03322  -14.09   <2e-16 ***

lm(Mean.Proj ~ fct_relevel(predicateType2, "emotive"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.73904    0.01989   37.15   <2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.50864    0.03874  -13.13   <2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.40831    0.04052  -10.08   <2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential -0.46798    0.03281  -14.26   <2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.55638    0.02656  -20.95   <2e-16 ***

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")


clmm(as.factor(veridicality_num) ~ fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(predicateType2, "emoComm")cognitive   -0.3784     0.1366  -2.770   0.0056 ** 
# fct_relevel(predicateType2, "emoComm")emotive      1.9489     0.1235  15.779  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential  -0.2814     0.1243  -2.264   0.0235 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm  -0.6121     0.1118  -5.474 4.41e-08 ***

# The output for ordinal models with categorical variables as predictors is not
# informative as there is no intercept.


## H1.2: communicatives: ----
### H1.2.1 with/without emotive component ----
# Amongst the communication predicates, does the CC of those that have an emotive 
# meaning component project more than the CCs of those that don't?

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"))


#### by-predicateType ----
##### plot ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType, emotiveComponent) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) # 7
levels(mean.proj$predicateType)

mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
  ggplot(aes(x = predicateType, y = Mean.Proj, colour = emotiveComponent)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour ="Emotive component") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = c("no" = "sienna4", "yes" = "green3"))
ggsave("../graphs/projection-by-predicateType-emotiveComponent-NO.pdf", height = 4, width = 5)


#### by-predicate ----
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

# add predicateType, verb and emotiveComponent to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# only communicative predicates
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) # 236

##### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = emotiveComponent)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Emotive component") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("deepskyblue2", "green3")) +
  facet_wrap( ~ emotiveComponent, 
              labeller = as_labeller(c("no" = "communicatives\nwithout emotive component", 
                                       "yes" = "communicatives\nwith emotive component")))
ggsave("../graphs/projection-by-communicative-with-out-emo-NO.pdf", height = 6, width = 9)


##### linear models ----
lm(Mean.Proj ~ emotiveComponent, data = mean.proj) %>% 
  summary()
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.18266    0.01763  10.360  < 2e-16 ***
# emotiveComponentyes  0.14808    0.03951   3.748 0.000225 ***

##### ordinal model ----
# only communicative predicates
d.proj = d.proj %>%
  filter(predicateType == "communicative")

clmm(as.factor(veridicality_num) ~ emotiveComponent + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# emotiveComponentyes   0.6511     0.1155   5.637 1.73e-08 ***

### H1.2.2 pure/discourse participation/state changing ----
# Amongst the communication predicates, does the CC of any particular subtype/s 
# project more strongly?

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         commType = case_when(pure_comm == "yes" ~ "pure",
                              discourse_participation_comm == "yes" ~ "discourse participation",
                              state_changing_comm == "yes" ~ "state changing"))


#### by-predicateType ----
##### plot ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  filter(predicateType == "communicative") %>% 
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj) # 3

mean.proj %>%
  ggplot(aes(x = commType, y = Mean.Proj, colour = commType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132)) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state\nchanging", "discourse\nparticipation", "pure")) + 
  scale_colour_manual(values = c("pure" = "orange", 
                                 "discourse participation" = "darkorange", 
                                 "state changing" = "orangered"))
ggsave("../graphs/projection-by-predicateType-commType-NO.pdf", height = 4, width = 10)


#### by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# add predicateType, verb and emotiveComponent to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, commType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, commType)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# only communicatives
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) # 236

mean.proj %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  summarize(count=n())

#   commType                count
#   <chr>                   <int>
# 1 discourse participation   114
# 2 pure                       95
# 3 state changing             27

##### plots ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = commType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("pure" = "orange",
                                 "discourse participation" = "darkorange",
                                 "state changing" = "orangered")) +
  facet_wrap( ~ factor(commType, levels = c("pure", "discourse participation", 
                                            "state changing")))
ggsave("../graphs/projection-by-communicative-predicate-NO.pdf", height = 4, width = 10)


##### linear model ----
lm(Mean.Proj ~ fct_relevel(commType, "discourse participation"), data = mean.proj) %>% 
  summary()
# significances: state changing **; pure ***; discourse participation ***.

##### ordinal model ----
# only communicative predicates
d.proj = d.proj %>%
  filter(predicateType == "communicative")

clmm(as.factor(veridicality_num) ~ commType + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)
# commTypepure            0.12557    0.09766   1.286    0.199
# commTypestate changing -0.20146    0.15467  -1.303    0.193


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

# create predicate type and emotive component columns
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
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
  distinct(verb_renamed, verb, voice, predicateType, predicateType2, V.Mean.Sum, 
           A.Mean.Sum, D.Mean.Sum)

table(d3$predicateType)
# cognitive communicative       emotive    evidential 
#        46           205           101            76 

table(d3$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        46         41        101         76        164 

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component"))
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emotive"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.44446    0.01720  25.846  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.12581    0.03074  -4.093 5.11e-05 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.09037    0.03200  -2.824  0.00497 ** 
# fct_relevel(predicateType2, "emotive")evidential -0.23469    0.02624  -8.943  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.24083    0.02186 -11.017  < 2e-16 ***

### by predicate type ----
#### plots ----
# calculate valence by predicateType means
mean.valence = new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
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
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
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
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 10

# valence by predicate type and direction of valence
#### plot ----
ggplot(mean.valence3, aes(x = predicateType2, y = Mean.Valence, 
                          colour = V.Mean.Sum2.direction)) +
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
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emotive") + V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.441194   0.018421  23.951  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.128746   0.031329  -4.110 4.77e-05 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.089714   0.032058  -2.798  0.00537 ** 
# fct_relevel(predicateType2, "emotive")evidential -0.238335   0.027274  -8.738  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.243157   0.022376 -10.867  < 2e-16 ***
# V.Mean.Sum2.directionpositive                     0.008902   0.017919   0.497  0.61960 


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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", "communicative without\nemotive component"))
ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)

#### linear models ----
lm(A.Mean.Sum2 ~ predicateType, new.scale) %>% 
  summary()
# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.373071   0.015754  23.681  < 2e-16 ***
# predicateTypecommunicative  0.028704   0.017432   1.647    0.100    
# predicateTypeemotive        0.120568   0.019006   6.344 5.76e-10 ***
# predicateTypeevidential    -0.006673   0.019960  -0.334    0.738   

lm(A.Mean.Sum2 ~ predicateType2, new.scale) %>% 
  summary()
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.373071   0.015527  24.028  < 2e-16 ***
# predicateType2emoComm     0.082783   0.022618   3.660 0.000284 ***
# predicateType2emotive     0.120568   0.018732   6.437 3.32e-10 ***
# predicateType2evidential -0.006673   0.019672  -0.339 0.734635    
# predicateType2nonEmoComm  0.015184   0.017570   0.864 0.387966 

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
       y = "Mean arousal rating") +
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
  scale_x_discrete(labels = c("cognitive", "evidential", 
                              "communicative without \n emotive component",
                              "communicative with \n emotive component", "emotive")) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2.pdf", height = 8, width = 10)


## H2.3 valence + arousal ----
# valence against arousal: no patterns (clusters) emerge.
#### plot ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2, colour = predicateType2, 
                      fill = predicateType2)) +
  geom_point(size = 2) +
  geom_mark_hull(alpha = 0.1) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 24)) +
  labs(x = "Mean arousal rating", 
       y = "Mean valence rating",
       colour = "Predicate type", 
       fill = "Predicate type") +
  coord_equal() + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels=c("cognitive", "communicative with\nemotive component", 
                               "emotive", "evidential", 
                               "communicative without\nemotive component")) +
  scale_fill_manual(values = cols2, 
                    labels=c("cognitive", "communicative with\nemotive component", 
                             "emotive", "evidential", 
                             "communicative without\nemotive component"))
ggsave("../graphs/valence-by-arousal.pdf", height = 8, width = 12)

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
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 12)) + 
  labs(y = "Mean valence / arousal rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely aroused (1)") +
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
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-NO.pdf", height = 6, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component"))
ggsave("../graphs/projection-by-valence2-NO.pdf", height = 6, width = 8)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-faceted-NO.pdf", height = 6, width = 10)

# projection by valence faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2-NO.pdf", height = 4, width = 9)

#### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.16168    0.02622   6.166 1.63e-09 ***
# V.Mean.Sum2  0.64054    0.07484   8.559  < 2e-16 ***

lm(Mean.Proj ~ V.Mean.Sum2 + fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.25738    0.04512   5.704 2.21e-08 ***
# V.Mean.Sum2                                       0.15099    0.06853   2.203   0.0281 *  
# fct_relevel(predicateType2, "emoComm")cognitive  -0.06225    0.05237  -1.189   0.2353    
# fct_relevel(predicateType2, "emoComm")emotive     0.42315    0.04553   9.294  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.02662    0.04823  -0.552   0.5812    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.11217    0.04376  -2.563   0.0107 *  

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.18974    0.07763   2.444   0.0149 *  
# V.Mean.Sum2                                                   0.34201    0.19130   1.788   0.0745 .  
# fct_relevel(predicateType2, "emoComm")cognitive              -0.09183    0.10517  -0.873   0.3831    
# fct_relevel(predicateType2, "emoComm")emotive                 0.54850    0.09808   5.593 4.05e-08 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.09567    0.09150   1.046   0.2963    
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.05179    0.08374  -0.618   0.5366    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive   0.11408    0.27121   0.421   0.6742    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive    -0.32087    0.22765  -1.409   0.1594    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential -0.45158    0.26881  -1.680   0.0937 .  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15541    0.22740  -0.683   0.4947  

##### largest residuals ----
# For which predicates does the linear model make the least accurate predictions?
# I don't remember why we were wondering about this. :)
residual <- resid(lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale))
cbind(new.scale, residual) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual)
#      verb_renamed predicateType2   residual
# 19      apologize     nonEmoComm  0.8287084
# 293   be promised     evidential -0.8243038
# 177    be floored        emotive  0.8158976
# 178     be fooled     evidential -0.7922769
# 30  be astonished        emotive  0.7502426
# 191        giggle        emoComm -0.7340796
# 247   be maddened        emotive  0.6982828
# 107   demonstrate     nonEmoComm -0.6813911
# 339   be revolted        emotive  0.6781822
# 9      be alarmed        emotive  0.6557634


#### ordinal models ----
# data frame for ordinal models
d.proj2 = droplevels(subset(d2, d2$polarity == "negative" & 
                              d2$conditional == "False")) %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8) %>% 
  distinct(verb_renamed, verb, voice, participant, predicateType, predicateType2, 
           veridicality, veridicality_num, V.Mean.Sum, A.Mean.Sum, V.Mean.Sum2, 
           V.Mean.Sum2.direction, A.Mean.Sum2)

# remove "other" and "comPriv" predicates
d.proj2 = d.proj2 %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   2.6725     0.1731   15.44   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj2) %>% 
  summary()
# Coefficients:
#                                                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                                   1.73598    0.54859   3.164  0.00155 ** 
# fct_relevel(predicateType2, "emoComm")cognitive              -0.25829    0.29654  -0.871  0.38375    
# fct_relevel(predicateType2, "emoComm")emotive                 2.61819    0.30763   8.511  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.30333    0.25923   1.170  0.24196    
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.14837    0.23696  -0.626  0.53121    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive  -0.01509    0.76744  -0.020  0.98431    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive    -1.70810    0.70339  -2.428  0.01517 *  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential -1.71225    0.76581  -2.236  0.02536 *  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.94444    0.65066  -1.452  0.14664  

#### with direction ----
# projection by valence with direction
##### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-valence-with-direction-NO.pdf", height = 3.5, width = 6)

# projection by valence faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) + 
  # plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.42865    0.02395  17.894  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.14493    0.03179  -4.559 6.74e-06 ***

lm(Mean.Proj ~ V.Mean.Sum2 + V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.23746    0.03230   7.351 1.02e-12 ***
# V.Mean.Sum2                    0.60636    0.07415   8.178 3.37e-15 ***
# V.Mean.Sum2.directionpositive -0.11610    0.02980  -3.897 0.000113 ***

lm(Mean.Proj ~ V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.22358    0.03992   5.601 3.83e-08 ***
# V.Mean.Sum2                                0.65036    0.10498   6.195 1.38e-09 ***
# V.Mean.Sum2.directionpositive             -0.09047    0.05255  -1.722   0.0859 .  
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.08793    0.14841  -0.593   0.5538  

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.53366    0.06559  -8.136 4.09e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "emotive") + 
       (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                                                                Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive                                                   0.08556    0.17554   0.487    0.626    
# fct_relevel(predicateType2, "emotive")cognitive                                -2.12451    0.21269  -9.989   <2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                                  -1.88790    0.16579 -11.388   <2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential                               -2.48678    0.20319 -12.239   <2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                               -2.43014    0.14002 -17.356   <2e-16 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive  -0.35000    0.28024  -1.249    0.212    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm    -0.43845    0.29343  -1.494    0.135    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential  0.16841    0.26102   0.645    0.519    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm -0.32748    0.20745  -1.579    0.114  

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                 2.9419     0.2512  11.712   <2e-16 ***
# V.Mean.Sum2.directionpositive              -0.2245     0.1191  -1.885   0.0594 .  
# V.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.7270     0.3426  -2.122   0.0338 *  

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                                                                                                     Estimate Std. Error z value
# V.Mean.Sum2                                                                                                          2.69227    0.76556   3.517
# fct_relevel(V.Mean.Sum2.direction, "negative")positive                                                               0.12766    0.46760   0.273
# fct_relevel(predicateType2, "emoComm")cognitive                                                                     -0.03287    0.43377  -0.076
# fct_relevel(predicateType2, "emoComm")emotive                                                                        2.71788    0.39254   6.924
# fct_relevel(predicateType2, "emoComm")evidential                                                                     0.25716    0.37542   0.685
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                                     0.31290    0.31716   0.987
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive                                                  -1.67191    1.10124  -1.518
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                                                          0.19407    1.35739   0.143
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                                                           -2.51562    0.94002  -2.676
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential                                                        -2.46083    1.26242  -1.949
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm                                                        -2.45307    0.90536  -2.709
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")cognitive              -0.32907    0.62668  -0.525
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")emotive                 0.09226    0.63952   0.144
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")evidential              0.22096    0.55280   0.400
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.59601    0.50378  -1.183
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")cognitive   0.61573    1.69666   0.363
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")emotive     1.35065    1.42976   0.945
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")evidential  1.21112    1.62232   0.747
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")nonEmoComm  2.77035    1.31107   2.113
#                                                                                                                     Pr(>|z|)    
# V.Mean.Sum2                                                                                                         0.000437 ***
# fct_relevel(V.Mean.Sum2.direction, "negative")positive                                                              0.784844    
# fct_relevel(predicateType2, "emoComm")cognitive                                                                     0.939594    
# fct_relevel(predicateType2, "emoComm")emotive                                                                       4.39e-12 ***
# fct_relevel(predicateType2, "emoComm")evidential                                                                    0.493341    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                                    0.323855    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive                                                  0.128961    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                                                         0.886314    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                                                           0.007448 ** 
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential                                                        0.051260 .  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm                                                        0.006739 ** 
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")cognitive              0.599518    
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")emotive                0.885294    
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")evidential             0.689366    
# fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")nonEmoComm             0.236776    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")cognitive  0.716676    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")emotive    0.344826    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")evidential 0.455342    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive:fct_relevel(predicateType2, "emoComm")nonEmoComm 0.034597 *  


### H2.4.2 arousal ----
# projection by arousal
#### H2.4.2.1 overall ----
##### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-arousal2-NO.pdf", height = 3.5, width = 6)

# projection by arousal faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = c(0, 1)),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType, ncol = 4)
ggsave("../graphs/projection-by-arousal-faceted-NO.pdf", height = 6, width = 10)

# projection by arousal faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-faceted2-NO.pdf", height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.04514    0.05644  -0.800    0.424    
# A.Mean.Sum2  0.94548    0.13125   7.204 2.68e-12 ***

lm(Mean.Proj ~ A.Mean.Sum2 + fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.20753    0.06390   3.248  0.00126 ** 
# A.Mean.Sum2                                       0.22664    0.11257   2.013  0.04472 *  
# fct_relevel(predicateType2, "emoComm")cognitive  -0.04884    0.05319  -0.918  0.35901    
# fct_relevel(predicateType2, "emoComm")emotive     0.42823    0.04535   9.443  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.02814    0.04831  -0.583  0.56053    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.11957    0.04325  -2.765  0.00594 ** 

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.11362    0.16234   0.700  0.48437   
# A.Mean.Sum2                                                   0.43263    0.34614   1.250  0.21205   
# fct_relevel(predicateType2, "emoComm")cognitive               0.02911    0.22315   0.130  0.89626   
# fct_relevel(predicateType2, "emoComm")emotive                 0.51174    0.19050   2.686  0.00751 **
# fct_relevel(predicateType2, "emoComm")evidential              0.22193    0.20186   1.099  0.27222   
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.04877    0.17916  -0.272  0.78557   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive  -0.16325    0.52812  -0.309  0.75739   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive    -0.18494    0.39771  -0.465  0.64217   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential -0.63222    0.47031  -1.344  0.17959   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14648    0.39434  -0.371  0.71048 

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.6403     0.2918   12.48   <2e-16 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                   1.65917    0.97399   1.703 0.088480 .  
# fct_relevel(predicateType2, "emoComm")cognitive               0.03448    0.62645   0.055 0.956108    
# fct_relevel(predicateType2, "emoComm")emotive                 2.19936    0.57267   3.841 0.000123 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.45068    0.56722   0.795 0.426883    
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.06540    0.50319  -0.130 0.896591    
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive  -0.60659    1.48248  -0.409 0.682412    
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive    -0.49520    1.19521  -0.414 0.678639    
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential -1.64964    1.32340  -1.247 0.212574    
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm -1.08804    1.10921  -0.981 0.326632 

#### H2.4.2.2 with direction of valence ----
##### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) 
ggsave("../graphs/projection-by-arousal-with-direction-NO.pdf", height = 3.5, width = 6)

# projection by arousal faceted with emotive component
# labels for facets
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) + 
  # plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)


##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                3.53215    0.44553   7.928 2.23e-15 ***
# V.Mean.Sum2.directionpositive             -0.08666    0.26115  -0.332    0.740    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive -0.54969    0.60119  -0.914    0.361     

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                                                                                                     Estimate Std. Error z value
# A.Mean.Sum2                                                                                                          1.77103    1.84930   0.958
# fct_relevel(V.Mean.Sum2.direction, "positive")negative                                                               0.08650    1.10049   0.079
# fct_relevel(predicateType2, "emoComm")cognitive                                                                      0.39109    1.06509   0.367
# fct_relevel(predicateType2, "emoComm")emotive                                                                        2.45212    1.09505   2.239
# fct_relevel(predicateType2, "emoComm")evidential                                                                     1.31113    1.06347   1.233
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                                     0.18486    1.01452   0.182
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative                                                   0.97122    2.19781   0.442
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                                                         -0.74265    2.17612  -0.341
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                                                           -0.03362    2.13928  -0.016
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential                                                        -2.86530    2.20567  -1.299
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm                                                        -0.94087    2.01418  -0.467
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")cognitive               0.33669    1.85774   0.181
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")emotive                 0.17383    1.30840   0.133
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")evidential             -1.67448    1.33528  -1.254
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")nonEmoComm              0.63409    1.19447   0.531
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")cognitive  -1.41272    4.46786  -0.316
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")emotive    -1.82519    2.62443  -0.695
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")evidential  2.42871    2.90218   0.837
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")nonEmoComm -2.22036    2.47131  -0.898
#                                                                                                                      Pr(>|z|)  
# A.Mean.Sum2                                                                                                           0.3382  
# fct_relevel(V.Mean.Sum2.direction, "positive")negative                                                                0.9374  
# fct_relevel(predicateType2, "emoComm")cognitive                                                                       0.7135  
# fct_relevel(predicateType2, "emoComm")emotive                                                                         0.0251 *
# fct_relevel(predicateType2, "emoComm")evidential                                                                      0.2176  
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                                      0.8554  
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative                                                    0.6586  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                                                           0.7329  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                                                             0.9875  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential                                                          0.1939  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm                                                          0.6404  
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")cognitive                0.8562  
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")emotive                  0.8943  
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")evidential               0.2098  
# fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")nonEmoComm               0.5955  
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")cognitive    0.7519  
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")emotive      0.4868  
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")evidential   0.4027  
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative:fct_relevel(predicateType2, "emoComm")nonEmoComm   0.3689 


# data frames for only negative/positive models
d.proj2.neg <- d.proj2 %>% filter(V.Mean.Sum2.direction == "negative")
nrow(d.proj2.neg) # 1836
d.proj2.pos <- d.proj2 %>% filter(V.Mean.Sum2.direction == "positive")
nrow(d.proj2.pos) # 2421

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2   n
# 1               negative      cognitive  14
# 2               negative        emoComm  29
# 3               negative        emotive  64
# 4               negative     evidential  17
# 5               negative     nonEmoComm  61

# 6               positive      cognitive  32
# 7               positive        emoComm  12
# 8               positive        emotive  37
# 9               positive     evidential  59
# 10              positive     nonEmoComm 103

###### negative valence ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), data = d.proj2.neg) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.4823     0.4499   7.739    1e-14 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj2.neg) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2                                                    2.0993     1.2553   1.672  0.09444 . 
# fct_relevel(predicateType2, "emoComm")cognitive                0.4798     1.5924   0.301  0.76316   
# fct_relevel(predicateType2, "emoComm")emotive                  2.2618     0.7470   3.028  0.00246 **
# fct_relevel(predicateType2, "emoComm")evidential              -0.4425     0.8375  -0.528  0.59727   
# fct_relevel(predicateType2, "emoComm")nonEmoComm               0.4994     0.6623   0.754  0.45082   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive   -1.6660     4.0581  -0.411  0.68142   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive     -1.1192     1.5834  -0.707  0.47967   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential  -0.3994     1.9350  -0.206  0.83647   
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm  -2.2680     1.4879  -1.524  0.12744  


###### positive valence ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj2.pos) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error z value Pr(>|z|)  
# A.Mean.Sum2                                                    2.1555     1.9391   1.112   0.2663  
# fct_relevel(predicateType2, "emoComm")cognitive                0.5483     1.1184   0.490   0.6239  
# fct_relevel(predicateType2, "emoComm")emotive                  2.8872     1.1436   2.525   0.0116 *
# fct_relevel(predicateType2, "emoComm")evidential               1.5722     1.1154   1.410   0.1587  
# fct_relevel(predicateType2, "emoComm")nonEmoComm               0.3413     1.0629   0.321   0.7482  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive   -0.8945     2.2852  -0.391   0.6955  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive     -0.7384     2.2324  -0.331   0.7408  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential  -3.3207     2.3054  -1.440   0.1497  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm  -1.1977     2.1077  -0.568   0.5699  

### H2.4.3 valence + arousal ----
#### linear models ----
# original scale
lm(Mean.Proj ~ V.Mean.Sum + A.Mean.Sum, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.01831    0.10503   0.174    0.862    
# V.Mean.Sum  -0.02667    0.01123  -2.375    0.018 *  
# A.Mean.Sum   0.10738    0.01694   6.339 5.92e-10 ***

# new scale
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.04523    0.05410  -0.836    0.404    
# V.Mean.Sum2  0.49915    0.08021   6.223 1.17e-09 ***
# A.Mean.Sum2  0.59814    0.13762   4.346 1.74e-05 ***

#### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2               2.1550     0.5723   3.765 0.000166 ***
# V.Mean.Sum2               2.1157     0.6642   3.185 0.001445 ** 
# A.Mean.Sum2:V.Mean.Sum2   0.1431     1.4530   0.098 0.921545  

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                                                          Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                                                3.8892     2.0050   1.940 0.052413 .  
# V.Mean.Sum2                                                                7.0299     2.4556   2.863 0.004199 ** 
# fct_relevel(predicateType2, "emoComm")cognitive                           -0.7990     1.2694  -0.629 0.529076    
# fct_relevel(predicateType2, "emoComm")emotive                              4.8395     1.3308   3.637 0.000276 ***
# fct_relevel(predicateType2, "emoComm")evidential                           2.4035     1.0730   2.240 0.025091 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm                           1.6242     0.9706   1.673 0.094247 .  
# A.Mean.Sum2:V.Mean.Sum2                                                  -10.6758     4.7356  -2.254 0.024174 *  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                2.5850     3.1447   0.822 0.411063    
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                 -4.6719     2.8624  -1.632 0.102647    
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential              -4.6822     2.5442  -1.840 0.065719 .  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm              -3.7882     2.1984  -1.723 0.084864 .  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                0.6497     3.3270   0.195 0.845166    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                 -9.1537     3.1358  -2.919 0.003510 ** 
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential              -8.2337     3.2090  -2.566 0.010292 *  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm              -6.7047     2.8825  -2.326 0.020016 *  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive   -5.5859     7.5394  -0.741 0.458760    
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive     14.8248     6.1627   2.406 0.016147 *  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential  13.7861     6.8746   2.005 0.044925 *  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm  11.6664     5.8539   1.993 0.046267 *  

##### positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant), data = d.proj2.pos) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)  
# A.Mean.Sum2               1.4635     0.7949   1.841   0.0656 .
# V.Mean.Sum2               1.5511     0.8649   1.793   0.0729 .
# A.Mean.Sum2:V.Mean.Sum2   1.2004     1.9617   0.612   0.5406 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") +
       (1 | participant), data = d.proj2.pos) %>% 
  summary()
# Coefficients:
#                                                                          Estimate Std. Error z value Pr(>|z|)  
# A.Mean.Sum2                                                               -0.6414     4.1980  -0.153   0.8786  
# V.Mean.Sum2                                                               -0.5135     5.4478  -0.094   0.9249  
# fct_relevel(predicateType2, "emoComm")cognitive                           -3.5752     2.3447  -1.525   0.1273  
# fct_relevel(predicateType2, "emoComm")emotive                              4.0833     2.6457   1.543   0.1227  
# fct_relevel(predicateType2, "emoComm")evidential                           1.6368     2.3365   0.701   0.4836  
# fct_relevel(predicateType2, "emoComm")nonEmoComm                          -0.4573     2.2042  -0.207   0.8357  
# A.Mean.Sum2:V.Mean.Sum2                                                    2.9523     9.4681   0.312   0.7552  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                9.0079     4.9208   1.831   0.0672 .
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                 -2.2439     5.4004  -0.415   0.6778  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential              -3.3739     4.8378  -0.697   0.4856  
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm               0.2615     4.3326   0.060   0.9519  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive                9.6081     5.8339   1.647   0.0996 .
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive                 -4.3094     6.1995  -0.695   0.4870  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential              -3.7033     6.1754  -0.600   0.5487  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm              -0.6385     5.8087  -0.110   0.9125  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive  -23.1706    11.1707  -2.074   0.0381 *
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive      6.2999    11.4603   0.550   0.5825  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential   7.9519    12.0655   0.659   0.5099  
# A.Mean.Sum2:V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm   3.9861    10.7788   0.370   0.7115  


## possible issues with the analysis ----

# predicates in both active and passive sentence frames with same predicate type
d3[duplicated(d3[,cbind(2,4)]),] %>%
  select(verb, predicateType2)

#       verb predicateType2
# 195 grieve        emotive
# 251 marvel        emotive
# 276  panic        emotive
# 426  worry        emotive

# Of the 101 emotives with valence/arousal/dominance ratings, four predicates occur in both
# sentence frames in the MV data set. The "active" and "passive voice" versions of these 
# predicates were assigned the same valence/arousal/dominance ratings. Due to the small number
# of these cases, this should not affect the overall distributions investigated above.


# H3: dynamic/activity/CoS predicates and projection----
## H3.1: dynamicity ----

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         dynamicity = case_when(stative_predicate == "yes" ~ "stative",
                                dynamic_predicate == "yes" ~ "dynamic",
                                TRUE ~ "unclear"))

### by predicate type ----

# # how many predicates in which predicateType and with which dynamicity?
d.proj %>%
  select(predicateType2, verb, dynamicity) %>%
  unique() %>%
  group_by(predicateType2, dynamicity) %>%
  summarize(count=n())

mean.proj = d.proj %>%
  group_by(predicateType2, dynamicity) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
mean.proj
nrow(mean.proj) # 12

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 7

#### plot ----
ggplot(mean.proj, aes(x = predicateType2, y = Mean.Proj, colour = dynamicity)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating", 
       colour = "Dynamicity") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("dynamic" = "violetred3", "stative" = "aquamarine4"))
ggsave("../graphs/projection-by-predicateType-and-Dynamicity-NO.pdf", height = 4, width = 10)


### by predicate ---- 
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) # 544
levels(mean.proj$verb_renamed)

# add verb and predicateType to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, dynamicity)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, dynamicity)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

# how many stative predicates are in each predicate type category?
mean.proj %>% 
  filter(dynamicity == "stative") %>% 
  group_by(predicateType) %>% 
  summarise(n())
# # A tibble: 3 × 2
# predicateType `n()`
#   <chr>         <int>
# 1 cognitive        39
# 2 emotive         148
# 3 evidential        3

#### plots ----
# projection by predicate with dynamic predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicate-dynamic-NO.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType), 
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
ggsave("../graphs/projection-by-predicate-dynamic-faceted-NO.pdf", height = 6, width = 6)

# projection by predicate with stative predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicate-stative-NO.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
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
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType) 
ggsave("../graphs/projection-by-predicate-stative-faceted-NO.pdf", height = 4, width = 13)

#### linear models ----
lm(Mean.Proj ~ dynamicity, data = mean.proj) %>%  
  summary()
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.22733    0.01512   15.03   <2e-16 ***
# dynamicitystative  0.40039    0.02509   15.96   <2e-16 ***

lm(Mean.Proj ~ dynamicity + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.225370   0.060026   3.755 0.000193 ***
# dynamicitystative           0.006833   0.067514   0.101 0.919425    
# predicateTypecommunicative -0.013223   0.062113  -0.213 0.831494    
# predicateTypeemotive        0.506836   0.043122  11.754  < 2e-16 ***
# predicateTypeevidential     0.045451   0.063829   0.712 0.476742    


#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ dynamicity + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# dynamicitystative  1.71634    0.06816   25.18   <2e-16 ***


## H3.2 change of state ----
# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# create predicateType, emotiveComponent, change-of-state columns
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"))

### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType2, changeOfState) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj
nrow(mean.proj) # 9

# # how many predicates in which predicateType and CoS / no CoS?
d.proj %>%
  select(predicateType2, verb, changeOfState) %>%
  unique() %>%
  group_by(predicateType2, changeOfState) %>%
  summarize(count=n())
# predicateType2 changeOfState count
#   <chr>          <chr>         <int>
# 1 cognitive      no               51
# 2 cognitive      yes               2
# 3 comPriv        no                9
# 4 emoComm        no               47
# 5 emotive        no              143
# 6 evidential     no               48
# 7 evidential     yes              38
# 8 nonEmoComm     no              189
# 9 other          no               12

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 7

##### plot ----
ggplot(mean.proj, aes(x = predicateType2, y = Mean.Proj, colour = changeOfState)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Change-of-state predicate") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("no" = "darkblue", "yes" = "gold3"))
ggsave("../graphs/projection-by-predicateType-and-CoS-NO.pdf", height = 4, width = 10)

### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
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

# how many change-of-state predicates are in each predicate type category?
mean.proj %>% 
  filter(changeOfState == "yes") %>% 
  group_by(predicateType) %>% 
  summarise(n())
# # A tibble: 2 × 2
# predicateType `n()`
#   <chr>         <int>
# 1 cognitive         2 <<< !!!
# 2 evidential       38

# The small number of CoS predicates amongst the cognitives is the reason for the
# different pattern.

#### plots -----
# projection by predicate with change-of-state predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicate-CoS-NO.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
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
ggsave("../graphs/projection-by-predicate-CoS-faceted-NO.pdf", height = 6, width = 6)

#### linear models ----
lm(Mean.Proj ~ changeOfState, data = mean.proj) %>%  
  summary()
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.37992    0.01528  24.865   <2e-16 ***
# changeOfStateyes -0.09325    0.05525  -1.688    0.092 .

lm(Mean.Proj ~ changeOfState + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.22925    0.03373   6.796 2.97e-11 ***
# changeOfStateyes            0.03050    0.05098   0.598    0.550    
# predicateTypecommunicative -0.01710    0.03732  -0.458    0.647    
# predicateTypeemotive        0.50979    0.03930  12.973  < 2e-16 ***
# predicateTypeevidential     0.02833    0.04751   0.596    0.551  

lm(Mean.Proj ~ changeOfState * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (2 not defined because of singularities)
#                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                  0.206100   0.033927   6.075 2.41e-09 ***
# changeOfStateyes                             0.643900   0.174650   3.687 0.000251 ***
# predicateTypecommunicative                   0.006047   0.037414   0.162 0.871671    
# predicateTypeemotive                         0.532939   0.039341  13.547  < 2e-16 ***
# predicateTypeevidential                      0.076076   0.048724   1.561 0.119051    
# changeOfStateyes:predicateTypecommunicative        NA         NA      NA       NA    
# changeOfStateyes:predicateTypeemotive              NA         NA      NA       NA    
# changeOfStateyes:predicateTypeevidential    -0.669058   0.182402  -3.668 0.000270 ***

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ changeOfState + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# changeOfStateyes  -0.4537     0.1093   -4.15 3.32e-05 ***


## H3.3: activity ----

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# create predicateType, emotiveComponent, activity columns
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
                              TRUE ~ "no"))


### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType2, activity) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj
nrow(mean.proj) # 13

# how many predicates in which predicateType and activity / no activity?
d.proj %>%
  select(predicateType2, verb, activity) %>%
  unique() %>%
  group_by(predicateType2, activity) %>%
  summarize(count=n())
# predicateType2 activity count
#   <chr>          <chr>    <int>
# 1 cognitive      no          41
# 2 cognitive      yes         12
# 3 comPriv        no           7
# 4 comPriv        yes          2
# 5 emoComm        no           3
# 6 emoComm        yes         44
# 7 emotive        no         143
# 8 evidential     no          74
# 9 evidential     yes         12
# 10 nonEmoComm     no          94
# 11 nonEmoComm     yes         95
# 12 other          no          10
# 13 other          yes          2

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 9

#### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, colour = activity)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Activity") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("no" = "slateblue", "yes" = "red3"))
ggsave("../graphs/projection-by-predicateType-and-activity-NO.pdf", height = 4, width = 10)

### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
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

# how many activity predicates are in each predicate type category?
mean.proj %>% 
  filter(activity == "yes") %>% 
  group_by(predicateType2) %>% 
  summarise(n())
# # A tibble: 4 × 2
# predicateType2 `n()`
#   <chr>          <int>
# 1 cognitive         12
# 2 emoComm           44
# 3 evidential        12
# 4 nonEmoComm        95

#### plots ----
# projection by predicate with activity predicates highlighted
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_point(data = mean.proj %>% filter(activity == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = activity), 
             size = 4,  shape = 1, colour = "red3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("activity")) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-activity-NO.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = predicateType),
             show.legend = FALSE) +
  geom_point(data = mean.proj %>% filter(activity == "yes"), 
             aes(x = verb_renamed, y = Mean.Proj, fill = activity), 
             size = 4,  shape = 1, colour = "red3", alpha = 0.7, stroke = 1.2) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_discrete(name = element_blank(), labels = c("activity")) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType) 
ggsave("../graphs/projection-by-predicate-activity-faceted-NO.pdf", height = 6, width = 6)

#### linear models ----
lm(Mean.Proj ~ activity, data = mean.proj) %>%  
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.43420    0.01708  25.422  < 2e-16 ***
# activityyes -0.19705    0.03059  -6.441  2.7e-10 ***

lm(Mean.Proj ~ activity + predicateType2, data = mean.proj) %>%  
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|) 
# (Intercept)               0.22801    0.03391   6.724  4.7e-11 ***
# activityyes               0.01053    0.02902   0.363   0.7169    
# predicateType2emoComm     0.09286    0.05272   1.762   0.0787 .  
# predicateType2emotive     0.51102    0.03932  12.995  < 2e-16 ***
# predicateType2evidential  0.04158    0.04237   0.981   0.3269    
# predicateType2nonEmoComm -0.05065    0.03849  -1.316   0.1888   

lm(Mean.Proj ~ activity * fct_relevel(predicateType2, "emoComm"), data = mean.proj) %>%  
  summary()
# Coefficients: (1 not defined because of singularities)
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.23333    0.13932   1.675 0.094587 .  
# activityyes                                                   0.10404    0.14399   0.723 0.470294    
# fct_relevel(predicateType2, "emoComm")cognitive               0.02981    0.14433   0.207 0.836448    
# fct_relevel(predicateType2, "emoComm")emotive                 0.50571    0.14073   3.594 0.000358 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.01952    0.14212   0.137 0.890810    
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.05532    0.14153  -0.391 0.696055    
# activityyes:fct_relevel(predicateType2, "emoComm")cognitive  -0.24867    0.16434  -1.513 0.130861    
# activityyes:fct_relevel(predicateType2, "emoComm")emotive          NA         NA      NA       NA    
# activityyes:fct_relevel(predicateType2, "emoComm")evidential  0.02644    0.16240   0.163 0.870733    
# activityyes:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.09480    0.14821  -0.640 0.522686 

#### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ activity + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# activityyes  -0.7405     0.0637  -11.62   <2e-16 ***


## H3.4 dynamic/activity/CoS comparison ----
### H3.4.1 overall ----
# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# create predicateType, emotiveComponent, activity columns
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         stateActivityCoS = case_when(stative_predicate == "yes" ~ "state",
                                      change_of_state_predicate == "yes" ~ 
                                        "change-of-state predicate",
                                      activity_predicate == "yes" ~ "activity",
                                      TRUE ~ "other"))


#### by predicate type ----
mean.proj = d.proj %>%
  group_by(predicateType2, stateActivityCoS) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj
nrow(mean.proj) # 18

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 12

##### plot ----
ggplot(mean.proj, aes(x = predicateType, y = Mean.Proj, 
                      colour = fct_relevel(stateActivityCoS, 
                                           "activity", "change-of-state predicate",
                                           "state", "other"))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Type of predicate") +
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"))
ggsave("../graphs/projection-by-predicateType-and-stateActivityCoS-NO.pdf", height = 4, width = 10)

#### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
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

mean.proj %>%
  select(predicateType2, verb_renamed, stateActivityCoS) %>%
  group_by(predicateType2, stateActivityCoS) %>%
  summarize(count=n()) %>% 
  pivot_wider(names_from = stateActivityCoS, values_from = count)
#   predicateType2 activity `change-of-state predicate` state other
#   <chr>             <int>                       <int> <int> <int>
# 1 cognitive            12                           2    39    NA
# 2 emoComm              44                          NA    NA     3
# 3 emotive              NA                          NA   148    NA
# 4 evidential           12                          38     3    33
# 5 nonEmoComm           95                          NA    NA    94


##### plots ----
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
ggsave("../graphs/projection-by-predicate-stateActivityCoS-NO.pdf", height = 4, width = 13)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, 
                                   colour = stateActivityCoS), show.legend = FALSE) +
  theme(legend.position = "top",
        plot.margin = unit(c(5.5, 44, 5.5, 44), "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Predicate") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"), 
                      breaks = c("activity", "change-of-state predicate", "state", "other")) +
  facet_wrap(~ fct_relevel(stateActivityCoS, "activity", "change-of-state predicate",
                           "state", "other"), ncol = 4) 
ggsave("../graphs/projection-by-predicate-stateActivityCoS-faceted-NO.pdf", height = 4, width = 10)


##### linear models ----
lm(Mean.Proj ~ fct_relevel(stateActivityCoS, "other"), data = mean.proj) %>%  
  summary()
# Coefficients:
#                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                      0.19675    0.02416   8.142  2.9e-15 ***
# fct_relevel(stateActivityCoS, "other")activity                   0.04040    0.03240   1.247   0.2130    
# fct_relevel(stateActivityCoS, "other")change-of-state predicate  0.08991    0.04982   1.805   0.0717 .  
# fct_relevel(stateActivityCoS, "other")state                      0.43097    0.03136  13.743  < 2e-16 ***

lm(Mean.Proj ~ stateActivityCoS + predicateType, data = mean.proj) %>%  
  summary()
# Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.228949   0.061043   3.751 0.000196 ***
# stateActivityCoSchange-of-state predicate -0.001513   0.055188  -0.027 0.978146    
# stateActivityCoSother                     -0.051184   0.029788  -1.718 0.086351 .  
# stateActivityCoSstate                      0.002048   0.068641   0.030 0.976212    
# predicateTypecommunicative                 0.004236   0.063172   0.067 0.946568    
# predicateTypeemotive                       0.508043   0.043089  11.790  < 2e-16 ***
# predicateTypeevidential                    0.062348   0.066716   0.935 0.350471 

lm(Mean.Proj ~ stateActivityCoS * predicateType, data = mean.proj) %>%  
  summary()
# Coefficients: (6 not defined because of singularities)
#                                                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                           0.11852    0.06968   1.701  0.08958 .  
# stateActivityCoSchange-of-state predicate                             0.73148    0.18436   3.968 8.29e-05 ***
# stateActivityCoSother                                                -0.13653    0.08137  -1.678  0.09398 .  
# stateActivityCoSstate                                                 0.11453    0.07968   1.437  0.15125    
# predicateTypecommunicative                                            0.11625    0.07263   1.601  0.11007    
# predicateTypeemotive                                                  0.50599    0.04345  11.646  < 2e-16 ***
# predicateTypeevidential                                               0.26481    0.09855   2.687  0.00744 ** 
# stateActivityCoSchange-of-state predicate:predicateTypecommunicative       NA         NA      NA       NA    
# stateActivityCoSother:predicateTypecommunicative                      0.08148    0.08741   0.932  0.35169    
# stateActivityCoSstate:predicateTypecommunicative                           NA         NA      NA       NA    
# stateActivityCoSchange-of-state predicate:predicateTypeemotive             NA         NA      NA       NA    
# stateActivityCoSother:predicateTypeemotive                                 NA         NA      NA       NA    
# stateActivityCoSstate:predicateTypeemotive                                 NA         NA      NA       NA    
# stateActivityCoSchange-of-state predicate:predicateTypeevidential    -0.85780    0.20094  -4.269 2.34e-05 ***
# stateActivityCoSother:predicateTypeevidential                              NA         NA      NA       NA    
# stateActivityCoSstate:predicateTypeevidential                        -0.23120    0.17501  -1.321  0.18707 

##### ordinal models ----
# remove "other" and "comPriv" predicates
d.proj = d.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")

clmm(as.factor(veridicality_num) ~ fct_relevel(stateActivityCoS, "activity") + (1 | participant), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(stateActivityCoS, "activity")change-of-state predicate  0.09156    0.12017   0.762   0.4461    
# fct_relevel(stateActivityCoS, "activity")other                     -0.18362    0.07993  -2.297   0.0216 *  
# fct_relevel(stateActivityCoS, "activity")state                      1.65531    0.07797  21.229   <2e-16 ***


# with predicate type
clmm(as.factor(veridicality_num) ~ stateActivityCoS * fct_relevel(predicateType2, "emoComm")
     + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                                                                                           Estimate Std. Error z value Pr(>|z|)    
# stateActivityCoSchange-of-state predicate                                                  -0.5827     0.2233  -2.610 0.009058 ** 
# stateActivityCoSother                                                                      -0.2375     0.4088  -0.581 0.561336    
# stateActivityCoSstate                                                                      -0.3731     0.4379  -0.852 0.394189    
# fct_relevel(predicateType2, "emoComm")cognitive                                            -0.8531     0.2238  -3.812 0.000138 ***
# fct_relevel(predicateType2, "emoComm")emotive                                               2.3152     0.4565   5.072 3.94e-07 ***
# fct_relevel(predicateType2, "emoComm")evidential                                            0.1886     0.2199   0.858 0.391140    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                           -0.5907     0.1252  -4.720 2.36e-06 ***
# stateActivityCoSchange-of-state predicate:fct_relevel(predicateType2, "emoComm")cognitive   4.2582     0.7426   5.734 9.82e-09 ***
# stateActivityCoSstate:fct_relevel(predicateType2, "emoComm")cognitive                       0.8412     0.4960   1.696 0.089916 .  
# stateActivityCoSother:fct_relevel(predicateType2, "emoComm")evidential                     -0.3140     0.4655  -0.674 0.500000    
# stateActivityCoSother:fct_relevel(predicateType2, "emoComm")nonEmoComm                      0.1683     0.4202   0.401 0.688713  


### H3.4.2 evidentials only ----
# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# create predicateType, emotiveComponent, activity columns
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         stateActivityCoS = case_when(stative_predicate == "yes" ~ "state",
                                      change_of_state_predicate == "yes" ~ 
                                        "change-of-state predicate",
                                      activity_predicate == "yes" ~ "activity",
                                      TRUE ~ "other"),
         evidenceType = case_when(perceptual_evidence == "yes" ~ "perceptual",
                                  reportative_evidence == "yes" ~	"reportative",
                                  inferential_evidence == "yes" ~ "inferential",
                                  TRUE ~ NA))

d.proj = droplevels(subset(d.proj, predicateType == "evidential"))
nrow(d.proj) # 856

# how many predicates in which evidence type?
d.proj %>%
  select(evidenceType, verb_renamed) %>% 
  unique() %>% 
  group_by(evidenceType) %>% 
  summarize(count=n())
# # A tibble: 3 × 2
# evidenceType count
#   <chr>        <int>
# 1 inferential     40
# 2 perceptual      10
# 3 reportative     36

# how many predicates in which type of predicate?
d.proj %>%
  select(stateActivityCoS, verb_renamed) %>% 
  unique() %>% 
  group_by(stateActivityCoS) %>% 
  summarize(count=n())
# # A tibble: 4 × 2
# stateActivityCoS          count
# <chr>                     <int>
# 1 activity                     12
# 2 change-of-state predicate    38
# 3 other                        33
# 4 state                         3

#### by evidence type ----
mean.proj = d.proj %>%
  group_by(evidenceType, stateActivityCoS) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         evidenceType = fct_reorder(as.factor(evidenceType), Mean.Proj))
mean.proj
nrow(mean.proj) # 9

##### plot ----
ggplot(mean.proj, aes(x = evidenceType, y = Mean.Proj, 
                      colour = fct_relevel(stateActivityCoS, 
                                           "activity", "change-of-state predicate",
                                           "state", "other"))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Evidence type",
       colour = "Type of predicate") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"))
ggsave("../graphs/projection-by-evidenceType-and-stateActivityCoS-NO.pdf", height = 4, width = 10)

# only evidence type
mean.proj = d.proj %>%
  group_by(evidenceType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         evidenceType = fct_reorder(as.factor(evidenceType), Mean.Proj))
mean.proj
nrow(mean.proj) # 3

ggplot(mean.proj, aes(x = evidenceType, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Evidence type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) 
ggsave("../graphs/projection-by-evidenceType-NO.pdf", height = 4, width = 10)


#### by predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
mean.proj
nrow(mean.proj) # 86

# add verb and evidence type to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, evidenceType, stateActivityCoS)) %>%
  distinct(verb, verb_renamed, evidenceType, stateActivityCoS)
nrow(tmp) # 86

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 86

mean.proj %>%
  select(evidenceType, verb_renamed, stateActivityCoS) %>%
  group_by(evidenceType, stateActivityCoS) %>%
  summarize(count=n()) %>% 
  pivot_wider(names_from = stateActivityCoS, values_from = count)
# evidenceType activity `change-of-state predicate` other state
#   <chr>           <int>                       <int> <int> <int>
# 1 inferential         3                          35     2    NA
# 2 perceptual          4                          NA     3     3
# 3 reportative         5                           3    28    NA

##### plots ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = stateActivityCoS)) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating", 
       x = "Evidential predicate",
       colour = "Type of predicate") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red3", "gold3", "aquamarine4", "grey"),
                      breaks = c("activity", "change-of-state predicate", "state", "other"))
ggsave("../graphs/projection-by-evidential-stateActivityCoS-NO.pdf", height = 4, width = 13)


##### linear model ----
lm(Mean.Proj ~ fct_relevel(stateActivityCoS, "activity"), data = mean.proj) %>%  
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.38333    0.08494   4.513 2.11e-05 ***
# fct_relevel(stateActivityCoS, "activity")change-of-state predicate -0.12632    0.09743  -1.296    0.198    
# fct_relevel(stateActivityCoS, "activity")other                     -0.13653    0.09919  -1.376    0.172    
# fct_relevel(stateActivityCoS, "activity")state                     -0.11667    0.18993  -0.614    0.541   

## activity, CoS and other *** significant, state not significant

##### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(stateActivityCoS, "activity") + (1 | participant), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error z value Pr(>|z|)   
# fct_relevel(stateActivityCoS, "activity")change-of-state predicate  -0.6621     0.2337  -2.833  0.00460 **
# fct_relevel(stateActivityCoS, "activity")other                      -0.7675     0.2497  -3.074  0.00211 **
# fct_relevel(stateActivityCoS, "activity")state                      -0.2430     0.4539  -0.535  0.59237   

# Interpretation?


# H4: veridicality and projection ----
## H4.1 general ----
# positive correlation of veridicality and projection ratings  

# create datasets for projection and veridicality inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411
d.verid = droplevels(subset(d, d$polarity == "positive" & d$conditional == "False"))
nrow(d.verid) # 5401

# create columns for predicate type, emotive component, projectivity, change-of-state
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
                                    predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"),
         canonicallyProjective = case_when(canonically_projective == "yes" ~ "yes", 
                                           TRUE ~ "no")) 

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
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
  summarize(Mean.Verid = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Verid = Mean.Verid - CILow, 
         YMax.Verid = Mean.Verid + CIHigh, 
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
nrow(mean.proj.verid) # 523

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
ggsave("../graphs/projection-by-veridicality-with-CIs-NO.pdf", height = 6, width = 6)

ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2,
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality2-NO.pdf", height = 4, width = 7)

# faceted
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")
ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(18, "pt")) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2) +
  coord_fixed(ratio = 1) + 
  facet_wrap( ~ predicateType2, labeller = as_labeller(predicateType2_names), ncol = 5)
ggsave("../graphs/projection-by-veridicality2-faceted-NO.pdf", height = 3, width = 10.5)


### linear models ----
lm(Mean.Proj ~ Mean.Verid, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.1439     0.0252   5.708 1.92e-08 ***
# Mean.Verid    0.3789     0.0354  10.702  < 2e-16 ***

lm(Mean.Proj ~ Mean.Verid + predicateType2, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.18032    0.03301   5.463 7.29e-08 ***
# Mean.Verid                0.19156    0.03020   6.343 4.92e-10 ***
# predicateType2emoComm     0.01637    0.04859   0.337  0.73634    
# predicateType2emotive     0.40240    0.04093   9.830  < 2e-16 ***
# predicateType2evidential -0.01026    0.04153  -0.247  0.80494    
# predicateType2nonEmoComm -0.10218    0.03727  -2.742  0.00632 ** 

lm(Mean.Proj ~ Mean.Verid * predicateType2, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          0.128167   0.037236   3.442 0.000625 ***
# Mean.Verid                           0.391054   0.076398   5.119 4.36e-07 ***
# predicateType2emoComm               -0.096547   0.103318  -0.934 0.350501    
# predicateType2emotive                0.414898   0.079936   5.190 3.03e-07 ***
# predicateType2evidential            -0.007444   0.055734  -0.134 0.893797    
# predicateType2nonEmoComm             0.033230   0.047337   0.702 0.483007    
# Mean.Verid:predicateType2emoComm     0.036395   0.150100   0.242 0.808511    
# Mean.Verid:predicateType2emotive    -0.150909   0.113218  -1.333 0.183153    
# Mean.Verid:predicateType2evidential -0.105926   0.099163  -1.068 0.285931    
# Mean.Verid:predicateType2nonEmoComm -0.352088   0.088181  -3.993 7.48e-05 ***


### ordinal models ----
d.proj.mean.verid = left_join(d.proj, mean.verid, by = c("verb_renamed")) %>%
  distinct()
nrow(d.proj.mean.verid) # 5411

# remove "other" and "comPriv" predicates
d.proj.mean.verid = d.proj.mean.verid %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(d.proj.mean.verid) # 5202

clmm(as.factor(veridicality_num) ~ Mean.Verid + (1 | participant), data = d.proj.mean.verid) %>% 
  summary()
# Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
# Mean.Verid  1.49341    0.08226   18.16   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ Mean.Verid * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj.mean.verid) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)    
# Mean.Verid                                                    1.5205     0.3884   3.915 9.04e-05 ***
# fct_relevel(predicateType2, "emoComm")cognitive               0.1835     0.3108   0.590  0.55493    
# fct_relevel(predicateType2, "emoComm")emotive                 1.9647     0.3737   5.258 1.46e-07 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.2272     0.3170   0.717  0.47351    
# fct_relevel(predicateType2, "emoComm")nonEmoComm              0.3349     0.3032   1.105  0.26930    
# Mean.Verid:fct_relevel(predicateType2, "emoComm")cognitive    0.3194     0.4539   0.704  0.48165    
# Mean.Verid:fct_relevel(predicateType2, "emoComm")emotive     -0.1816     0.4821  -0.377  0.70637    
# Mean.Verid:fct_relevel(predicateType2, "emoComm")evidential  -0.4825     0.4334  -1.113  0.26556    
# Mean.Verid:fct_relevel(predicateType2, "emoComm")nonEmoComm  -1.3262     0.4110  -3.227  0.00125 ** 



## H4.2 with "factives" ----
# mean projection by mean veridicality with canonically projective predicates highlighted
### plot ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(data = mean.proj.verid %>% 
               filter(canonicallyProjective == "yes"),
             aes(x = Mean.Verid, y = Mean.Proj, fill = "canonically projective predicate"), 
             size = 3, colour = "red", alpha = 0.7) +
  geom_point(data = mean.proj.verid, 
             aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2), alpha = 0.7) +
  geom_smooth(data = mean.proj.verid, 
              aes(x = Mean.Verid, y = Mean.Proj, colour = "grey10"), 
              method = "lm", se = FALSE, show.legend = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type",
       fill = element_blank()) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2,
                      labels = c("cognitive", "communicative with\nemotive component", 
                                 "emotive", "evidential", 
                                 "communicative without\nemotive component")) +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality-with-factives2-NO.pdf", height = 4, width = 7)

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
ggsave("../graphs/projection-by-veridicality2-NO.pdf", height = 6, width = 6)

