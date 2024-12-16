# >>> NEGATION ONLY
# Modified copy of analysis3.R, including only projection ratings based on items with negation as 
# embedding environment. 
# Includes predicates with MEAN ACCEPTABILITY RATINGS greater than 4 across embedding environments,
# i.e., those predicates that fall within this range of acceptability across all embedding environments 
# but not in the negation-only environment are INCLUDED here. The number of predicates considered 
# here is therefore the same as in the original code in analysis3.R.

# Testing hypotheses based on White & Rawlins' MegaVeridicality I dataset 

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
library(scales)
library(ggh4x)
theme_set(theme_bw())

# prepare data ----
## MV dataset (White & Rawlins 2018) ----
# load data
d <- read.csv("../data/d.csv")
nrow(d) # 21692
names(d)

# load predicate coding
y <- read.csv("../data/predicate-coding.csv")
nrow(y) # 544
names(y)

d <- left_join(d, y, by = c("verb", "voice"))
nrow(d) # 21692

# specify colours for predicate types
cols <- c(cognitive = "coral",
          communicative = "deepskyblue2",
          emotive = "darkgreen",
          evidential = "purple")

cols2 <- c(cognitive = "coral",
           emoComm = "green3",
           emotive = "darkgreen",
           evidential = "purple",
           nonEmoComm = "deepskyblue2")

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotion entailment", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotion entailment")

# create dataset for projection inferences
d.proj <- droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# create predicateType, emotiveComponent, change-of-state, environment columns
d.proj <- d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         dynamicity = case_when(stative_predicate == "yes" ~ "stative",
                                dynamic_predicate == "yes" ~ "dynamic"),
         volition = case_when(volitional == "yes" ~ "volitional",
                              TRUE ~ "non-volitional"),
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
                              state_changing_comm == "yes" ~ "state changing"),
         emoCommType = case_when(emo_comm_manner == "yes" ~ "manner",
                                 emo_comm_attitude == "yes" ~ "attitude"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
                              TRUE ~ "no"),
         evidenceType = case_when(perceptual_evidence == "yes" ~ "perceptual",
                                  reportative_evidence == "yes" ~	"reportative",
                                  inferential_evidence == "yes" ~ "inferential",
                                  TRUE ~ NA),
         sayVerb = case_when(say_verb == "yes" ~ "yes",
                             TRUE ~ "no"),
         sayVerbType = case_when(discourse_role_verb == "yes" ~ "discourse role verb",
                                 mode_verb == "yes" ~ "mode verb", 
                                 verb_renamed == "say" ~ "say"),
         modeVerbType = case_when(say_by_means == "yes" ~ "say-by-means",
                                  say_with_attitude == "yes" ~ "say-with-attitude"),
         sayByMeansVerbType = case_when(say_by_means_manner == "yes" ~ "manner",
                                        say_by_means_sound == "yes" ~ "sound",
                                        say_by_means_form == "yes" ~ "form"))

d.proj %>% 
  group_by(polarity, conditional) %>% 
  count()
#   polarity conditional     n
#   <chr>    <chr>       <int>
# 1 negative False        5411

# calculate by-predicate projection means 
mean.proj.all <- d.proj %>%
  group_by(verb_renamed) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

# add predicateType etc. to the means
tmp <- d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, commType, 
           emoCommType, changeOfState, dynamicity, activity, evidenceType, volition, sayVerb, 
           sayVerbType, modeVerbType, sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, commType, 
           emoCommType, changeOfState, dynamicity, activity, evidenceType, volition, sayVerb, 
           sayVerbType, modeVerbType, sayByMeansVerbType)
nrow(tmp) # 544

mean.proj.all <- left_join(mean.proj.all, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

mean.proj.all %>%
  group_by(predicateType2) %>%
  distinct(verb_renamed) %>%
  count()
# predicateType2     n
# <chr>          <int>
# 1 cognitive         53
# 2 comPriv           12
# 3 emoComm           33
# 4 emotive          148
# 5 evidential        85
# 6 nonEmoComm       184
# 7 other             29

# exclude "comPriv" and "other" predicates from analysis.
mean.proj <- mean.proj.all %>% 
  filter(! predicateType %in% c("comPriv", "other"))
nrow(mean.proj) # 503  

mean.proj %>%
  group_by(predicateType2) %>%
  distinct(verb_renamed) %>%
  count()
# predicateType2     n
# <chr>          <int>
# 1 cognitive         53
# 2 emoComm           33
# 3 emotive          148
# 4 evidential        85
# 5 nonEmoComm       184

# Subsets for all predicates / communicative predicates with mean acceptability ratings greater than 
# 4 ACROSS ALL EMBEDDING ENVIRONMENTS (= 'original' acceptable predicates in analysis3.R).
# See "Acceptability" in analysis3.R for the reasons for and the consequences of examining only these 
# subsets.

# acceptable predicates
mean.acc.overall <- d %>% 
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other")) %>% 
  filter((polarity == "negative" | conditional == "True") & ! predicateType %in% c("comPriv", "other")) %>% 
  group_by(verb_renamed) %>%
  summarise(Mean.Acc = mean(acceptability)) 
acc.preds <- mean.acc.overall %>% 
  filter(Mean.Acc > 4) %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
length(acc.preds) # 474

# acceptable communicatives
acc.comms <- d %>% 
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other")) %>% 
  filter((polarity == "negative" | conditional == "True") & predicateType == "communicative") %>% 
  group_by(verb_renamed) %>%
  summarise(Mean.Acc = mean(acceptability)) %>% 
  filter(Mean.Acc > 4) %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
length(acc.comms) # 201

# by-predicate projection means for "acceptable" predicates
mean.proj.acc <- mean.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(mean.proj.acc) # 474

# by-predicate projection means for "acceptable" communicatives
mean.proj.comm <- mean.proj.acc %>%
  filter(verb_renamed %in% acc.comms)
nrow(mean.proj.comm) # 201

d.proj.acc <- d.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(d.proj.acc) # 4717

d.proj.comm <- d.proj.acc %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 2002

# create dataset for veridicality inferences
d.verid <- d %>% 
  filter(polarity == "positive" & conditional == "False")
nrow(d.verid) # 5401

# only include acceptable predicates
d.verid <- d.verid %>% 
  filter(verb_renamed %in% acc.preds)
d.verid %>% distinct(verb_renamed) %>% nrow() # 474

# calculate by-predicate veridicality means 
mean.verid <- d.verid %>%
  group_by(verb_renamed) %>%
  summarise(Mean.Verid = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Verid = Mean.Verid - CILow, 
         YMax.Verid = Mean.Verid + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Verid))
nrow(mean.verid) # 474

# combine projection and veridicality means in one data frame
mean.proj.verid <- mean.proj.acc %>%
  inner_join(mean.verid, by = ("verb_renamed"))
nrow(mean.proj.verid) # 474

# only communicative predicates
mean.proj.verid.comm <- mean.proj.verid %>%
  filter(predicateType == "communicative")
nrow(mean.proj.verid.comm) # 201



## valence and arousal data (Warriner et al. 2013) ----
# load data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  rename(verb = Word) %>%
  inner_join(d.proj.acc, by = "verb")
nrow(w2) # 3862
n_distinct(w2$verb) # 375
n_distinct(w2$verb_renamed) # 388

# create predicate type, emotive component and environment columns
w2 <-  w2 %>%
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

w3 <- w2 %>% 
  distinct(verb_renamed, verb, voice, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum)

# combine projection and valence/arousal/dominance ratings in one data frame
mean.proj.vad <- left_join(w3, mean.proj, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.vad) # 388

# Warriner et al.' (2013) dataset contains valence, arousal and dominance ratings for 388 of the 
# predicates in the MV dataset which belong to (only) one of our 5 predicate types and have a mean 
# acceptability rating greater than 4 across embedding environments. 

mean.proj.vad %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()  
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         47
# 2 emoComm           25
# 3 emotive           97
# 4 evidential        67
# 5 nonEmoComm       152

# exclusion of lemmata that could be interpreted differently (see analysis3.R)
mean.proj.vad <- mean.proj.vad %>% 
  filter(!verb_renamed %in% c("address", "figure", "intercept", "log", "notice", "post", "share", 
                              "spot", "state", "stress", "type"))
nrow(mean.proj.vad) # 377

# Warriner et al.'s (2013) dataset contains valence, arousal and dominance ratings for 377 of the 
# predicates in the MV dataset which belong to (only) one of our 5 predicate types, have a mean 
# acceptability rating greater than 4 and could not have been interpreted as a noun whose meaning 
# is distinctly different from that of the corresponding verb.

mean.proj.vad %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         46
# 2 emoComm           25
# 3 emotive           97
# 4 evidential        64
# 5 nonEmoComm       145

# rescale V + A + D ratings
# Valence and dominance have extremes (unhappy - happy, controlled - controlling) and a neutral state 
# in between. The neutral state of arousal is not in the middle of the "calm - aroused" scale, but 
# at the lower end: calmness is the absence of arousal; there is no such thing as "negative" arousal. 

# The ratings are rescaled to range from 0 to 1. 
new.scale <- mean.proj.vad %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"),
         D.Mean.Sum2.cont = (D.Mean.Sum - 1)/8,
         V.Mean.Sum.sc = scale(V.Mean.Sum),
         A.Mean.Sum.sc = scale(A.Mean.Sum),
         D.Mean.Sum.sc = scale(D.Mean.Sum),
         V.Mean.Sum2.sc = scale(V.Mean.Sum2),
         A.Mean.Sum2.sc = scale(A.Mean.Sum2),
         D.Mean.Sum2.sc = scale(D.Mean.Sum2),
         D.Mean.Sum2.cont.sc = scale(D.Mean.Sum2.cont))

# data frame for ordinal models
d.proj.vad = w2 %>% 
  filter(!verb_renamed %in% c("address", "figure", "intercept", "log", "notice", "post", "share", 
                              "spot", "state", "stress", "type")) %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"),
         D.Mean.Sum2.cont = (D.Mean.Sum - 1)/8,
         V.Mean.Sum.sc = scale(V.Mean.Sum),
         A.Mean.Sum.sc = scale(A.Mean.Sum),
         D.Mean.Sum.sc = scale(D.Mean.Sum),
         V.Mean.Sum2.sc = scale(V.Mean.Sum2),
         A.Mean.Sum2.sc = scale(A.Mean.Sum2),
         D.Mean.Sum2.sc = scale(D.Mean.Sum2),
         D.Mean.Sum2.cont.sc = scale(D.Mean.Sum2.cont)) %>% 
  distinct(verb_renamed, verb, voice, participant, predicateType, predicateType2, 
           veridicality, veridicality_num, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum, 
           V.Mean.Sum2, V.Mean.Sum2.direction, A.Mean.Sum2, D.Mean.Sum2,
           D.Mean.Sum2.direction)

d.proj.vad %>% 
  distinct(verb_renamed) %>% 
  nrow() # 377

V_labels <- c("negative" = "negative valence",
              "positive" = "positive valence")

D_labels <- c("negative" = "negative dominance",
              "positive" = "positive dominance")

# A Acceptability ----
## A.1 correlation with projection ----
### overall ----
#### plot ----
ggplot(mean.proj, aes(x = Mean.Acc, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean acceptability rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 7), breaks = c(0:7)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-acceptability-line-NO.pdf", height = 6, width = 8)

#### linear model ----
lm(Mean.Proj ~ Mean.Acc, data = mean.proj) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.37705    0.08738  -4.315 1.92e-05 ***
# Mean.Acc     0.15264    0.01754   8.703  < 2e-16 ***

# Residual standard error: 0.3159 on 501 degrees of freedom
# Multiple R-squared:  0.1313,	Adjusted R-squared:  0.1296 
# F-statistic: 75.73 on 1 and 501 DF,  p-value: < 2.2e-16

#### ordinal model ----
clmm(as.factor(veridicality) ~ acceptability + (1 | participant), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.43314    0.02235   19.38   <2e-16 ***

### communicatives only ----
#### plot ----
ggplot(mean.proj.comm, aes(x = Mean.Acc, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean acceptability rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 7), breaks = c(0:7)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-acceptability-line-comm-NO.pdf", height = 6, width = 8)

#### linear model ----
lm(Mean.Proj ~ Mean.Acc, data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -0.11050    0.11102  -0.995  0.32081   
# Mean.Acc     0.06553    0.02270   2.886  0.00433 **

# Residual standard error: 0.2438 on 199 degrees of freedom
# Multiple R-squared:  0.04018,	Adjusted R-squared:  0.03536 
# F-statistic: 8.331 on 1 and 199 DF,  p-value: 0.004328

#### ordinal model ----
clmm(as.factor(veridicality) ~ acceptability + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.32069    0.04017   7.984 1.42e-15 ***

### A.2 distributions ----
# how many items with which acceptability rating?
d.proj %>% 
  group_by(acceptability) %>% 
  summarise(count = n())
#   acceptability count
#           <int> <int>
# 1             1   152
# 2             2   440
# 3             3   697
# 4             4   702
# 5             5  1139
# 6             6  1115
# 7             7  1166

# distribution of ratings within each acceptability subgroup
o <- d.proj %>% 
  group_by(acceptability, veridicality) %>% 
  count() %>% 
  group_by(acceptability) %>%
  mutate(percentage =  n * 100 / sum(n)) %>% 
  print(n = Inf)
#  acceptability veridicality     n percentage
#          <int> <chr>        <int>      <dbl>
# 1             1 maybe          101      66.4 
# 2             1 no              23      15.1 
# 3             1 yes             28      18.4 
# 4             2 maybe          276      62.7 
# 5             2 no              42       9.55
# 6             2 yes            122      27.7 
# 7             3 maybe          446      64.0 
# 8             3 no              75      10.8 
# 9             3 yes            176      25.3 
# 10             4 maybe          428      61.0 
# 11             4 no              46       6.55
# 12             4 yes            228      32.5 
# 13             5 maybe          552      48.5 
# 14             5 no              86       7.55
# 15             5 yes            501      44.0 
# 16             6 maybe          448      40.2 
# 17             6 no              48       4.30
# 18             6 yes            619      55.5 
# 19             7 maybe          413      35.4 
# 20             7 no              49       4.20
# 21             7 yes            704      60.4 

#### plots ----
ggplot(o, aes(x = acceptability, y = percentage, colour = veridicality)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Acceptability rating", 
       y = "Percentage",
       colour = "Projection rating") +
  scale_x_discrete(limits = c(1:7), breaks = c(1:7)) +
  scale_colour_manual(values = c("maybe" = "grey50", "no" = "red", "yes" = "green3"))
ggsave("../graphs/percentage-projection-by-acceptability-line-NO.pdf", height = 6, width = 8)

ggplot(o, aes(x = acceptability, y = percentage, fill = veridicality)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1) +
  labs(x = "Acceptability rating", 
       y = "Percentage",
       fill = "Projection rating") +
  scale_x_discrete(limits = c(1:7), breaks = c(1:7)) +
  scale_fill_manual(values = c("maybe" = "grey50", "no" = "red", "yes" = "green3"))
ggsave("../graphs/percentage-projection-by-acceptability-bar-NO.pdf", height = 6, width = 8)

ggplot(o, aes(x = acceptability, y = n, fill = veridicality)) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1) +
  labs(x = "Acceptability rating", 
       y = "Count",
       fill = "Projection rating") +
  scale_x_discrete(limits = c(1:7), breaks = c(1:7)) +
  scale_fill_manual(values = c("maybe" = "grey50", "no" = "red", "yes" = "green3"))
ggsave("../graphs/count-projection-by-acceptability-bar-NO.pdf", height = 6, width = 8)

## A.3 low acceptability predicates ----
# predicates with mean acceptability ratings of less than or equal to 4
mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  arrange(Mean.Acc) %>% 
  reframe(verb_renamed, Mean.Acc, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
#   verb_renamed   Mean.Acc Mean.Proj predicateType2
#   <fct>             <dbl>     <dbl> <chr>         
# 1 bark               2.33     0.111 nonEmoComm    
# 2 snap               2.7      0     emoComm       
# 3 vote               2.7      0.3   nonEmoComm    
# 4 be nonplussed      2.9      0.5   emotive       
# 5 negotiate          2.9      0.3   nonEmoComm    
# 6 stutter            2.9      0.2   nonEmoComm    
# 7 be bet             3        0.222 evidential    
# 8 be cheered         3.1      0.6   emotive       
# 9 be tricked         3.1     -0.1   evidential    
# 10 curse              3.1      0     emoComm       
# 11 gab                3.1      0.4   nonEmoComm    
# 12 okay               3.1      0.4   nonEmoComm    
# 13 quip               3.1      0     nonEmoComm    
# 14 growl              3.11     0.111 emoComm       
# 15 sing               3.11     0     nonEmoComm    
# 16 be tweeted         3.2      0.1   evidential    
# 17 scribble           3.2      0     nonEmoComm    
# 18 will               3.2      0     cognitive     
# 19 simulate           3.3      0.3   nonEmoComm    
# 20 muse               3.4      0.1   cognitive     
# 21 rediscover         3.4      0     evidential    
# 22 request            3.4      0.2   nonEmoComm    
# 23 underscore         3.4      0.1   nonEmoComm    
# 24 grunt              3.5      0.1   emoComm       
# 25 maintain           3.5      0     nonEmoComm    
# 26 underline          3.5      0.3   nonEmoComm    
# 27 be quoted          3.6      0.1   evidential    
# 28 prophesy           3.6      0.2   nonEmoComm    
# 29 spout              3.6      0.3   nonEmoComm    
# 30 whimper            3.6      0.3   emoComm       
# 31 yell               3.6      0     nonEmoComm    
# 32 add                3.7      0.2   nonEmoComm    
# 33 be invigorated     3.7      0.8   emotive       
# 34 be maddened        3.7      0.9   emotive       
# 35 cackle             3.7      0.5   emoComm       
# 36 chant              3.7      0     nonEmoComm    
# 37 hoot               3.7      0.1   emoComm       
# 38 jest               3.7      0.4   nonEmoComm    
# 39 lie                3.7      0.5   nonEmoComm    
# 40 measure            3.7      0.1   evidential    
# 41 rationalize        3.7      0.3   evidential    
# 42 underestimate      3.7      0.2   cognitive     
# 43 be befuddled       3.8      0.4   emotive       
# 44 be charmed         3.8      0.6   emotive       
# 45 be stupefied       3.8      0.4   emotive       
# 46 elect              3.8      0.1   evidential    
# 47 giggle             3.8     -0.1   emoComm       
# 48 lecture            3.8      0.3   nonEmoComm    
# 49 manufacture        3.8      0.2   nonEmoComm    
# 50 mutter             3.8      0     emoComm       
# 51 reason             3.8     -0.1   evidential    
# 52 rule               3.8     -0.2   nonEmoComm    
# 53 shriek             3.8      0.2   emoComm       
# 54 volunteer          3.8      0.1   nonEmoComm    
# 55 be signalled       3.9      0.4   evidential    
# 56 be tantalized      3.9      0.5   emotive       
# 57 categorize         3.9      0.2   evidential    
# 58 hope               3.9      0.2   cognitive     
# 59 howl               3.9      0.3   emoComm       
# 60 mumble             3.9      0     nonEmoComm    
# 61 presuppose         3.9     -0.1   cognitive     
# 62 require            3.9     -0.1   cognitive     
# 63 snitch             3.9      0.3   nonEmoComm    
# 64 summarize          3.9      0.3   nonEmoComm    
# 65 test               3.9     -0.1   nonEmoComm    
# 66 be galled          4        0.667 emotive       
# 67 chatter            4        0.3   nonEmoComm    
# 68 depict             4        0.1   nonEmoComm    
# 69 derive             4       -0.1   evidential    
# 70 rant               4        0.4   emoComm       
# 71 reply              4       -0.2   nonEmoComm    
# 72 sketch             4        0.3   nonEmoComm    
# 73 submit             4        0     nonEmoComm   

# What proportion of predicates within each type has a mean acceptability rating 
# of less than or equal to 4?
count <- mean.proj %>% 
  group_by(predicateType2) %>% 
  summarise(total.count = n())
count2 <- mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  group_by(predicateType2) %>% 
  summarise(low.acc.count = n())
left_join(count, count2, by = "predicateType2") %>% 
  mutate(low.acc.percentage = low.acc.count * 100 / total.count)
# # A tibble: 5 × 4
#   predicateType2 total.count low.acc.count low.acc.percentage
#   <chr>                <int>         <int>              <dbl>
# 1 cognitive               53             6              11.3 
# 2 emoComm                 33            12              36.4 
# 3 emotive                148             9               6.08
# 4 evidential              85            12              14.1 
# 5 nonEmoComm             184            34              18.5 

### neg-only vs all embedding environments ----
# MV-all low acceptability predicates
MV.all.low.acc <- mean.acc.overall %>% 
  filter(Mean.Acc <= 4) %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
MV.all.low.acc %>% length() # 29

# MV-neg-only low acceptability predicates
MV.neg.low.acc <- mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
MV.neg.low.acc %>% length() # 73

not.acc.neg.only <- setdiff(MV.neg.low.acc, MV.all.low.acc) %>% print()
# [1] "add"            "be befuddled"   "be charmed"     "be galled"      "be invigorated" "be maddened"    "be quoted"     
# [8] "be signalled"   "be stupefied"   "be tantalized"  "be tricked"     "chant"          "chatter"        "depict"        
# [15] "derive"         "gab"            "grunt"          "hope"           "howl"           "jest"           "lecture"       
# [22] "lie"            "maintain"       "mumble"         "muse"           "mutter"         "prophesy"       "quip"          
# [29] "rant"           "rationalize"    "reason"         "rediscover"     "reply"          "request"        "require"       
# [36] "rule"           "scribble"       "shriek"         "sing"           "sketch"         "snitch"         "spout"         
# [43] "submit"         "summarize"      "underestimate"  "underline"      "underscore"     "whimper"        "will"          
# [50] "yell"  

# There are 50 predicates in the MV dataset that have acceptability ratings > 4 across all embedding 
# environments but acceptability ratings <= 4 when only considering negation as embedding environment.

only.acc.neg.only <- setdiff(MV.all.low.acc, MV.neg.low.acc) %>% print()
# [1] "be educated"   "be faxed"      "be stimulated" "diagnose"      "pity"          "update"  

# There are 6 predicates in the MV dataset that have acceptability ratings <= 4 across all embedding 
# environments but acceptability ratings > 4 when only considering negation as embedding environment.

mean.proj %>% 
  filter(verb_renamed %in% not.acc.neg.only & predicateType == "communicative") %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
# [1] "add"        "chant"      "chatter"    "depict"     "gab"        "grunt"      "howl"       "jest"       "lecture"    "lie"       
# [11] "maintain"   "mumble"     "mutter"     "prophesy"   "quip"       "rant"       "reply"      "request"    "rule"       "scribble"  
# [21] "shriek"     "sing"       "sketch"     "snitch"     "spout"      "submit"     "summarize"  "underline"  "underscore" "whimper"   
# [31] "yell"  

# Of the 50 additional predicates with low mean acceptability ratings when only considering neg-only 
# items, 31 are communicatives. These would have had to be excluded from experiment 1 if the embedding 
# environment had been taken into account when defining the exclusion criterion. 

mean.proj %>% 
  filter(verb_renamed %in% only.acc.neg.only & predicateType == "communicative") %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
# [1] "update"

# Of the 6 predicates with mean acceptability > 4 based only on neg-only items and <= across all
# environments, one is a communicative. This was excluded from experiment 1, but would have had to 
# have been included had the embedding environment been considered in the exclusion.

mean.proj1 <- mean.proj %>% 
  mutate(acceptability2 = case_when(verb_renamed %in% not.acc.neg.only ~ "not acceptable neg-only",
                                    verb_renamed %in% only.acc.neg.only ~ "only acceptable neg-only",
                                    verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc) ~ "not acceptable",
         ! verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc) ~ "acceptable"))
mean.proj1 %>% 
    group_by(acceptability2) %>% 
  count()
#   acceptability2               n
#   <chr>                    <int>
# 1 acceptable                 424
# 2 not acceptable              23
# 3 not acceptable neg-only     50
# 4 only acceptable neg-only     6

### plots ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = "acceptable"), 
             alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc)), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "not acceptable")) +
  geom_point(data = mean.proj %>% filter(verb_renamed %in% not.acc.neg.only), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "not acceptable neg-only"), alpha = 0.8) +
  geom_point(data = mean.proj %>% filter(verb_renamed %in% only.acc.neg.only), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "only acceptable neg-only")) +
  
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating under negation",
       colour = "Acceptability") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("acceptable", "not acceptable", "not acceptable neg-only", "only acceptable neg-only"),
                      labels = c("acceptable overall\nand under negation only", 
                                 "neither acceptable overall\nnor under negation only",
                                 "acceptable overall,\nnot acceptable under negation only", 
                                 "not acceptable overall,\nacceptable under negation only"),
                      values = c("yellowgreen", "grey20", "deeppink3", "blue"))
ggsave("../graphs/projection-by-predicate-acceptability-NO.pdf", height = 4, width = 13)

mean.proj.excl <- d.proj %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  mutate(acceptability2 = case_when(verb_renamed %in% not.acc.neg.only ~ "not acceptable neg-only",
                                    verb_renamed %in% only.acc.neg.only ~ "only acceptable neg-only",
                                    verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc) ~ "not acceptable",
                                    ! verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc) ~ "acceptable")) %>%
  group_by(acceptability2) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         acceptability2 = fct_reorder(as.factor(acceptability2), Mean.Proj)) %>% 
  print()

#   acceptability2           Mean.Proj  CILow CIHigh Mean.Acc YMin.Proj YMax.Proj
#   <fct>                        <dbl>  <dbl>  <dbl>    <dbl>     <dbl>     <dbl>
# 1 acceptable                   0.401 0.0178 0.0180     5.16     0.383     0.419
# 2 not acceptable               0.181 0.0749 0.0705     3.33     0.106     0.251
# 3 not acceptable neg-only      0.205 0.0502 0.0482     3.68     0.155     0.253
# 4 only acceptable neg-only     0.508 0.136  0.136      4.41     0.373     0.644

ggplot(mean.proj.excl, aes(x = acceptability2, y = Mean.Proj, colour = acceptability2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(hjust = 1),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating under negation") +
  xlab("Acceptability") +
  scale_x_discrete(labels = c("acceptable" = "acceptable overall\nand under negation only", 
                              "not acceptable" = "neither acceptable overall\nnor under negation only",
                              "not acceptable neg-only" = "acceptable overall,\nnot acceptable under negation only", 
                              "only acceptable neg-only" = "not acceptable overall,\nacceptable under negation only")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(limits = c("acceptable", "not acceptable", "not acceptable neg-only", "only acceptable neg-only"),
                      values = c("yellowgreen", "grey20", "deeppink3", "blue"))
ggsave("../graphs/projection-by-acceptability-NO.pdf", height = 4, width = 12)

### linear model ----
lm(Mean.Proj ~ fct_relevel(acceptability2, "only acceptable neg-only"), data = mean.proj1) %>% 
  summary()
# Coefficients:
#                                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                        0.40176    0.01610  24.957  < 2e-16 ***
# fct_relevel(acceptability2, "acceptable")not acceptable           -0.22156    0.07097  -3.122   0.0019 ** 
# fct_relevel(acceptability2, "acceptable")not acceptable neg-only  -0.19642    0.04956  -3.963 8.48e-05 ***
# fct_relevel(acceptability2, "acceptable")only acceptable neg-only  0.11306    0.13628   0.830   0.4071  

#                                                                       Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                                            0.18019    0.06912   2.607  0.00941 **
# fct_relevel(acceptability2, "not acceptable")acceptable                0.22156    0.07097   3.122  0.00190 **
# fct_relevel(acceptability2, "not acceptable")not acceptable neg-only   0.02514    0.08351   0.301  0.76352   
# fct_relevel(acceptability2, "not acceptable")only acceptable neg-only  0.33462    0.15195   2.202  0.02811 * 

#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.20533    0.04688   4.380 1.45e-05 ***
# fct_relevel(acceptability2, "not acceptable neg-only")acceptable                0.19642    0.04956   3.963 8.48e-05 ***
# fct_relevel(acceptability2, "not acceptable neg-only")not acceptable           -0.02514    0.08351  -0.301   0.7635    
# fct_relevel(acceptability2, "not acceptable neg-only")only acceptable neg-only  0.30948    0.14321   2.161   0.0312 *  

#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                      0.5148     0.1353   3.804  0.00016 ***
# fct_relevel(acceptability2, "only acceptable neg-only")acceptable               -0.1131     0.1363  -0.830  0.40715    
# fct_relevel(acceptability2, "only acceptable neg-only")not acceptable           -0.3346     0.1520  -2.202  0.02811 *  
# fct_relevel(acceptability2, "only acceptable neg-only")not acceptable neg-only  -0.3095     0.1432  -2.161  0.03117 *  

### ordinal model ----
clmm(as.factor(veridicality) ~ fct_relevel(acceptability2, "acceptable") + (1 | participant),
     data = d.proj %>% 
       mutate(acceptability2 = case_when(verb_renamed %in% not.acc.neg.only ~ "not acceptable neg-only",
                                         verb_renamed %in% only.acc.neg.only ~ "only acceptable neg-only",
                                         verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc) ~ "not acceptable",
                                         ! verb_renamed %in% intersect(MV.all.low.acc, MV.neg.low.acc) ~ "acceptable"))) %>% 
  summary()
# Coefficients:
#                                                                   Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(acceptability2, "acceptable")not acceptable           -0.96861    0.15313  -6.325 2.53e-10 ***
# fct_relevel(acceptability2, "acceptable")not acceptable neg-only  -0.88084    0.10572  -8.332  < 2e-16 ***
# fct_relevel(acceptability2, "acceptable")only acceptable neg-only  0.08413    0.28358   0.297    0.767   

#                                                                       Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(acceptability2, "not acceptable")acceptable                0.96861    0.15322   6.322 2.59e-10 ***
# fct_relevel(acceptability2, "not acceptable")not acceptable neg-only   0.08778    0.18063   0.486 0.627007    
# fct_relevel(acceptability2, "not acceptable")only acceptable neg-only  1.05274    0.31927   3.297 0.000976 ***

#                                                                                Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(acceptability2, "not acceptable neg-only")acceptable                0.88084    0.10570   8.333  < 2e-16 ***
# fct_relevel(acceptability2, "not acceptable neg-only")not acceptable           -0.08778    0.18055  -0.486  0.62685    
# fct_relevel(acceptability2, "not acceptable neg-only")only acceptable neg-only  0.96497    0.30063   3.210  0.00133 ** 

#                                                                                Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(acceptability2, "only acceptable neg-only")acceptable              -0.08413    0.28299  -0.297 0.766247    
# fct_relevel(acceptability2, "only acceptable neg-only")not acceptable          -1.05274    0.31863  -3.304 0.000953 ***
# fct_relevel(acceptability2, "only acceptable neg-only")not acceptable neg-only -0.96496    0.30004  -3.216 0.001299 ** 

# B All predicateTypes ----
## B.1 by predicate type ----
### plots ----
# calculate by-predicateType means
mean.proj.bt <- d.proj.acc %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj))
nrow(mean.proj.bt) # 4

ggplot(mean.proj.bt, aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
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
mean.proj.bt2 <- d.proj.acc %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
nrow(mean.proj.bt2) # 5

ggplot(mean.proj.bt2, 
       aes(x = factor(predicateType2, c("cognitive", "evidential", "nonEmoComm", 
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
                              "communicative without\nemotion entailment", 
                              "communicative with\nemotion entailment", "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2-NO.pdf", height = 4, width = 10)

### linear models ----
lm(Mean.Proj ~ fct_relevel(predicateType, "communicative"), mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                            0.20608    0.01743  11.827   <2e-16 ***
# fct_relevel(predicateType, "communicative")cognitive   0.02490    0.03844   0.648   0.5174    
# fct_relevel(predicateType, "communicative")emotive     0.53204    0.02697  19.726   <2e-16 ***
# fct_relevel(predicateType, "communicative")evidential  0.06520    0.03311   1.969   0.0495 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.247 on 470 degrees of freedom
# Multiple R-squared:  0.4813,	Adjusted R-squared:  0.4779 
# F-statistic: 145.3 on 3 and 470 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04715   7.148 3.38e-12 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.10605    0.05811  -1.825  0.06865 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.40108    0.05138   7.806 3.87e-14 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.06575    0.05479  -1.200  0.23075    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15128    0.05067  -2.985  0.00298 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.245 on 469 degrees of freedom
# Multiple R-squared:  0.4909,	Adjusted R-squared:  0.4866 
# F-statistic: 113.1 on 4 and 469 DF,  p-value: < 2.2e-16

### distribution ----
mean.proj.acc %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
# 1 cognitive         52
# 2 emoComm           27
# 3 emotive          144
# 4 evidential        77
# 5 nonEmoComm       174

## B.2 by predicate ----
### with communicatives ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.acc, 
             aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "communicative"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "communicative", "know"),
                      values = c("grey50", "deepskyblue2", "red"))
ggsave("../graphs/projection-by-predicate-communicative-NO.pdf", height = 4, width = 13)

### with emoComms ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.acc, 
             aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "communicative with emotive component")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "communicative with emotive component", "know"),
                      values = c("grey50", "green3", "red"))
ggsave("../graphs/projection-by-predicate-emoComm-NO.pdf", height = 4, width = 13)

### with nonEmoComms ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.acc, 
             aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(predicateType2 == "nonEmoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "communicative without emotive component")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "communicative without emotive component", "know"),
                      values = c("grey50", "deepskyblue2", "red"))
ggsave("../graphs/projection-by-predicate-nonEmoComm-NO.pdf", height = 4, width = 13)

### with emotive cognitives ----
ggplot(mean.proj.acc, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "other"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed %in% c("desire", "hope", "wish")), 
             aes(colour = "cognitive")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed %in% c("dread", "fear", "worry", "be worried")), 
             aes(colour = "emotive")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed %in% c("desire", "hope", "wish")),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "coral") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed %in% c("dread", "fear", "worry", "be worried")),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "cognitive", "emotive"),
                      values = c("grey50", "coral", "darkgreen"))
ggsave("../graphs/projection-by-predicate-emoCog-NO.pdf", height = 4, width = 13)

mean.proj.emoCogs <- d.proj.acc %>%
  mutate(emoCogType = case_when(verb_renamed %in% c("desire", "hope", "wish") ~ "cognitive",
                                verb_renamed %in% c("dread", "fear", "worry", "be worried") ~ "emotive",
                                TRUE ~ "other")) %>%
  group_by(emoCogType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         emoCogType = fct_reorder(as.factor(emoCogType), Mean.Proj))
nrow(mean.proj.emoCogs) # 3

ggplot(mean.proj.emoCogs %>% filter(emoCogType != "other"), 
       aes(x = emoCogType, y = Mean.Proj, colour = emoCogType)) +
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

# C Communicatives ----
## C.1 overall ----
### C.1.1 by predicateType ----
#### plots ----
# calculate by-predicateType means
mean.proj.comm.bt <-  d.proj.comm %>%
  group_by(commType) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj.comm.bt) # 3

ggplot(mean.proj.comm.bt, aes(x = factor(commType, c("state changing", "discourse participation", "pure")), 
                              y = Mean.Proj, colour = commType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(vjust = -.2),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state changing" = "state\nchanging",
                              "discourse participation" = "discourse\nparticipation",
                              "pure" = "pure")) +
  scale_colour_manual(values = c("pure" = "orange", 
                                 "discourse participation" = "darkorange", 
                                 "state changing" = "orangered"))
ggsave("../graphs/projection-by-predicateType-commType-NO.pdf", height = 4, width = 5)

# faceted by predicate type
mean.proj.comm.bt2 <-  d.proj.comm %>%
  group_by(commType, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj.comm.bt2) # 5

ggplot(mean.proj.comm.bt2, aes(x = factor(commType, c("state changing", "discourse participation", "pure")),
                               y = Mean.Proj, colour = commType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        strip.text = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        panel.grid.major.x = element_blank()) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state changing" = "state\nchanging", 
                              "discourse participation" = "discourse\nparticipation", 
                              "pure" = "pure")) + 
  scale_colour_manual(
    values = c("pure" = "orange",
               "discourse participation" = "darkorange",
               "state changing" = "orangered")) +
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names))
ggsave("../graphs/projection-by-predicateType-commType-faceted-NO.pdf", height = 4, width = 8)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(commType, "pure"), 
   mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                     0.20621    0.02447   8.426 7.26e-15 ***
# fct_relevel(commType, "discourse participation")pure            0.02389    0.03704   0.645    0.520    
# fct_relevel(commType, "discourse participation")state changing -0.09565    0.06044  -1.583    0.115    

# Coefficients:
#                                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                           0.23010    0.02781   8.275 1.87e-14 ***
# fct_relevel(commType, "pure")discourse participation -0.02389    0.03704  -0.645   0.5197    
# fct_relevel(commType, "pure")state changing          -0.11954    0.06187  -1.932   0.0548 .  

# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2472 on 198 degrees of freedom
# Multiple R-squared:  0.01851,	Adjusted R-squared:  0.008593 
# F-statistic: 1.867 on 2 and 198 DF,  p-value: 0.1573

lm(Mean.Proj ~ fct_relevel(commType, "discourse participation") * fct_relevel(predicateType2, "nonEmoComm"), 
   mean.proj.comm) %>% 
  summary()
# no significant differences in either predicate type.

### C.1.2 by-predicate ----
# How many of which type of communicative predicate?
mean.proj.comm %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  summarize(count=n())
#   commType                count
#   <chr>                   <int>
# 1 discourse participation   102
# 2 pure                       79
# 3 state changing             20

#### plots ----
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj, colour = commType)) +
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


## C.3 say verbs (Grimshaw 2015) ----
# distribution of communication predicates
d.proj.acc %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(predicateType, sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType sayVerb     n
#   <chr>         <chr>   <int>
# 1 cognitive     yes         2
# 2 communicative no         91
# 3 communicative yes       110


### C.3.1 types of communicatives ----
#### distribution ----
d.proj.comm %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb     n
#   <chr>   <int>
# 1 no         91
# 2 yes       110

#### plot ----
mean.proj.commsay <- d.proj.comm %>%
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.commsay) # 2

ggplot(mean.proj.commsay, aes(x = sayVerb, y = Mean.Proj)) +
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
  labs(x = "Communicative type",
       y = "Mean projection rating") + 
  scale_x_discrete(labels = c("non-say verb", "say verb")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-communication-type-NO.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayVerb, "no"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.24750    0.02578   9.599   <2e-16 ***
# fct_relevel(sayVerb, "no")yes -0.07568    0.03485  -2.171   0.0311 *  

### C.3.2 types of say verbs ----
#### distribution ----
d.proj.comm %>% 
  filter(sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerbType             n
#   <chr>               <int>
# 1 discourse role verb    55
# 2 mode verb              54
# 3 say                     1

#### plots ----
mean.proj.saytype <- d.proj.comm %>%
  filter(sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.saytype) # 3
# As the predicate say merely denotes the light verb SAY, it does not belong to
# any sub-category of say-verbs.

ggplot(mean.proj.saytype, aes(x = sayVerbType, y = Mean.Proj)) +
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
  labs(x = "Say verb type",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-sayverb-type-NO.pdf", height = 4, width = 10)

mean.proj.saytype2 <- d.proj.comm %>%
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.saytype2) # 4

ggplot(mean.proj.saytype2, aes(x = sayVerb, y = Mean.Proj, colour = sayVerbType)) +
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
  labs(x = "Communicative type",
       y = "Mean projection rating",
       colour = "Say verb type") + 
  scale_x_discrete(labels = c("non-say verb",
                              "say verb")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("purple","deeppink", "orange3", "grey50"))
ggsave("../graphs/projection-by-sayverb-type2-NO.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayVerbType, "discourse role verb"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                               0.10909    0.03264   3.343  0.00114 **
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.12609    0.04637   2.719  0.00763 **
# fct_relevel(sayVerbType, "discourse role verb")say        0.09091    0.24423   0.372  0.71046  

#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.23519    0.03294   7.140 1.18e-10 ***
# fct_relevel(sayVerbType, "mode verb")discourse role verb -0.12609    0.04637  -2.719  0.00763 ** 
# fct_relevel(sayVerbType, "mode verb")say                 -0.03519    0.24427  -0.144  0.88574 

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "discourse role verb") +  (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(sayVerbType, "discourse role verb")mode verb   0.5404     0.1481   3.649 0.000263 ***
# fct_relevel(sayVerbType, "discourse role verb")say        -0.0392     0.7636  -0.051 0.959065    

# fct_relevel(sayVerbType, "mode verb")discourse role verb  -0.5404     0.1481  -3.649 0.000263 ***
# fct_relevel(sayVerbType, "mode verb")say                  -0.5796     0.7625  -0.760 0.447145  

# fct_relevel(sayVerbType, "say")discourse role verb  0.03919    0.76377   0.051    0.959
# fct_relevel(sayVerbType, "say")mode verb            0.57963    0.76261   0.760    0.447

### C.3.3 types of mode verbs ----
#### distribution ----
d.proj.comm %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means         41
# 2 say-with-attitude    13

#### plot ----
mean.proj.modetype <- d.proj.comm %>%
  filter(sayVerb == "yes" & verb_renamed %in% acc.comms) %>%  
  group_by(sayVerbType, modeVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.modetype) # 4

ggplot(mean.proj.modetype, aes(x = sayVerbType, y = Mean.Proj, colour = modeVerbType)) +
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
  labs(x = "Say verb type",
       y = "Mean projection rating",
       colour = "Mode verb type") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-NO.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(modeVerbType, "say-with-attitude"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                 0.20976    0.03593   5.837 3.48e-07 ***
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude  0.10563    0.07324   1.442    0.155 

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-by-means") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude   0.5531     0.2529   2.187   0.0287 *

# fct_relevel(modeVerbType, "say-with-attitude")say-by-means  -0.5531     0.2529  -2.187   0.0287 *

### C.3.4 types of say-by-means verbs ----
#### distribution ----
d.proj.comm %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                  14
# 2 manner                22
# 3 sound                  5

#### plot ----
mean.proj.bymeanstype <- d.proj.comm %>%
  filter((sayVerbType == "mode verb" | verb_renamed == "say") & verb_renamed %in% acc.comms) %>%  
  group_by(modeVerbType, sayByMeansVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.bymeanstype) # 5

ggplot(mean.proj.bymeanstype, aes(x = modeVerbType, y = Mean.Proj, colour = sayByMeansVerbType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(hjust = 0.31, vjust = -0.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Mode verb type",
       y = "Mean projection rating",
       colour = "Say-by-means verb type") + 
  scale_x_discrete(labels = c("say-by-means", "say-with-attitude", "say")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2", "grey50"))
ggsave("../graphs/projection-by-saybymeansverb-type-NO.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayByMeansVerbType, "manner"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                    0.18571    0.06519   2.849  0.00705 **
# fct_relevel(sayByMeansVerbType, "form")manner  0.04156    0.08339   0.498  0.62110   
# fct_relevel(sayByMeansVerbType, "form")sound   0.01429    0.12708   0.112  0.91108 

#                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                     0.22727    0.05200   4.370 9.27e-05 ***
# fct_relevel(sayByMeansVerbType, "manner")form  -0.04156    0.08339  -0.498    0.621    
# fct_relevel(sayByMeansVerbType, "manner")sound -0.02727    0.12084  -0.226    0.823  

#                                                Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                     0.20000    0.10908   1.833   0.0746 .
# fct_relevel(sayByMeansVerbType, "sound")form   -0.01429    0.12708  -0.112   0.9111  
# fct_relevel(sayByMeansVerbType, "sound")manner  0.02727    0.12084   0.226   0.8227 

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayByMeansVerbType, "sound") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(sayByMeansVerbType, "form")manner   0.1309     0.2521   0.519    0.604
# fct_relevel(sayByMeansVerbType, "form")sound    0.0270     0.3790   0.071    0.943

# fct_relevel(sayByMeansVerbType, "manner")form   -0.1309     0.2521  -0.519    0.604
# fct_relevel(sayByMeansVerbType, "manner")sound  -0.1039     0.3652  -0.284    0.776

# fct_relevel(sayByMeansVerbType, "sound")form    -0.0270     0.3790  -0.071    0.943
# fct_relevel(sayByMeansVerbType, "sound")manner   0.1039     0.3652   0.284    0.776

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### C.3.5 overall ----
#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, commType, sayVerbType, modeVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType         modeVerbType          n
#   <chr>          <chr>                   <chr>               <chr>             <int>
# 1 emoComm        discourse participation mode verb           say-with-attitude     4
# 2 emoComm        discourse participation NA                  NA                    2
# 3 emoComm        pure                    mode verb           say-by-means         16
# 4 emoComm        pure                    mode verb           say-with-attitude     3
# 5 emoComm        pure                    NA                  NA                    2
# 6 nonEmoComm     discourse participation discourse role verb NA                   45
# 7 nonEmoComm     discourse participation mode verb           say-with-attitude     1
# 8 nonEmoComm     discourse participation NA                  NA                   50
# 9 nonEmoComm     pure                    discourse role verb NA                    5
# 10 nonEmoComm     pure                    mode verb           say-by-means         25
# 11 nonEmoComm     pure                    mode verb           say-with-attitude     5
# 12 nonEmoComm     pure                    say                 NA                    1
# 13 nonEmoComm     pure                    NA                  NA                   22
# 14 nonEmoComm     state changing          discourse role verb NA                    5
# 15 nonEmoComm     state changing          NA                  NA                   15

#### plot ----
mean.proj.overall <- d.proj.comm %>%
  filter(sayVerb == "yes") %>%  
  group_by(sayVerbType, modeVerbType, sayByMeansVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.overall) # 6

ggplot(mean.proj.overall, aes(x = sayVerbType, y = Mean.Proj, colour = modeVerbType, 
                              shape = ifelse(is.na(sayByMeansVerbType), "NA", sayByMeansVerbType))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.3), size = 2) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.3)) +
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.spacing.y = unit(-0.2, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Say verb type",
       y = "Mean projection rating",
       colour = "Mode verb type",
       shape = "Say-by-means verb type") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3", "blue", "grey50")) + 
  scale_shape_manual(breaks = c("form", "manner", "sound", "NA"),
                     values = c(15, 19, 17, 18))  
ggsave("../graphs/projection-by-saybymeansverb-type2-NO.pdf", height = 4, width = 10)



# H Dynamicity/Activity/CoS predicates and projection----
## H.1 dynamicity ----
### H.1.1 overall ----
#### plot ----
mean.proj.dyn <- d.proj.acc %>%
  group_by(dynamicity) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.dyn) # 2

ggplot(mean.proj.dyn, aes(x = dynamicity, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.3),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Dynamicity",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-dynamicity-NO.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ dynamicity, mean.proj.acc) %>% 
  summary()
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.22287    0.01635   13.63   <2e-16 ***
# dynamicitystative  0.40744    0.02625   15.52   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2785 on 472 degrees of freedom
# Multiple R-squared:  0.338,	Adjusted R-squared:  0.3366 
# F-statistic:   241 on 1 and 472 DF,  p-value: < 2.2e-16

### H.1.2 by predicate type ----
# how many predicates in which predicateType and with which dynamicity?
d.proj.acc %>%
  select(predicateType2, verb_renamed, dynamicity) %>%
  unique() %>%
  group_by(predicateType2, dynamicity) %>%
  summarise(count=n())
#   predicateType2 dynamicity count
#   <chr>          <chr>      <int>
# 1 cognitive      dynamic       14
# 2 cognitive      stative       38
# 3 emoComm        dynamic       27
# 4 emotive        dynamic        1
# 5 emotive        stative      143
# 6 evidential     dynamic       74
# 7 evidential     stative        3
# 8 nonEmoComm     dynamic      174

d.proj.acc %>% filter(predicateType2 == "emotive" & dynamicity == "dynamic") %>% distinct(verb_renamed)
#   verb_renamed
# 1      agonize

#### plot ----
mean.proj.dyn2 <- d.proj.acc %>%
  group_by(predicateType2, dynamicity) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
nrow(mean.proj.dyn2) # 8

ggplot(mean.proj.dyn2, aes(x = predicateType2, y = Mean.Proj, colour = dynamicity)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.3),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating", 
       colour = "Dynamicity") +
  scale_x_discrete(labels = predicateType2_names) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("dynamic" = "violetred3", "stative" = "aquamarine4"))
ggsave("../graphs/projection-by-predicateType-and-dynamicity-NO.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ dynamicity * fct_relevel(predicateType2, "cognitive"), 
   data = mean.proj.acc) %>%  
  summary()
# predicateType2  significance of dynamic-stative difference
# cognitive       n.s.
# emoComm         n/a
# emotive         . (only 1 dynamic predicate)
# evidential      n.s.
# nonEmoComm      n/a
 
# Residual standard error: 0.2449 on 466 degrees of freedom
# Multiple R-squared:  0.4947,	Adjusted R-squared:  0.4871 
# F-statistic: 65.18 on 7 and 466 DF,  p-value: < 2.2e-16

## H.2 change of state ----
### H.2.1 overall ----
mean.proj.CoS <- d.proj.acc %>%
  group_by(changeOfState) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.CoS) # 2

ggplot(mean.proj.CoS, aes(x = changeOfState, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.3),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Change of state",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-CoS-NO.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ changeOfState, mean.proj.acc) %>% 
  summary()
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.38803    0.01629  23.816   <2e-16 ***
# changeOfStateyes -0.09470    0.05996  -1.579    0.115    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3414 on 472 degrees of freedom
# Multiple R-squared:  0.005257,	Adjusted R-squared:  0.003149 
# F-statistic: 2.494 on 1 and 472 DF,  p-value: 0.1149

### H.2.2 by predicate type ----
# how many predicates in which predicateType and CoS / no CoS?
d.proj.acc %>%
  select(predicateType2, verb_renamed, changeOfState) %>%
  unique() %>%
  group_by(predicateType2, changeOfState) %>%
  summarise(count=n())
#   predicateType2 changeOfState count
#   <chr>          <chr>         <int>
# 1 cognitive      no               50
# 2 cognitive      yes               2
# 3 emoComm        no               27
# 4 emotive        no              144
# 5 evidential     no               44
# 6 evidential     yes              33
# 7 nonEmoComm     no              174

##### plot ----
mean.proj.CoS2 <- d.proj.acc %>%
  group_by(predicateType2, changeOfState) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
nrow(mean.proj.CoS2) # 7

ggplot(mean.proj.CoS2, aes(x = predicateType2, y = Mean.Proj, colour = changeOfState)) +
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
  scale_x_discrete(labels = predicateType2_names) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("no" = "darkblue", "yes" = "gold3"))
ggsave("../graphs/projection-by-predicateType-and-CoS-NO.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ changeOfState * fct_relevel(predicateType2, "cognitive"), mean.proj.acc) %>% 
  summary()
# predicateType2  significance of no vs yes
# cognitive       *** (note: only 2 of 52 cognitives are CoS predicates)
# emoComm         n/a
# emotive         n/a
# evidential      n.s.
# nonEmoComm      n/a

# Residual standard error: 0.242 on 467 degrees of freedom
# Multiple R-squared:  0.5055,	Adjusted R-squared:  0.4991 
# F-statistic: 79.56 on 6 and 467 DF,  p-value: < 2.2e-16

## H.3 activity ----
### H.3.1 overall ----
#### plot ----
mean.proj.act <- d.proj.acc %>%
  group_by(activity) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.act) # 2

ggplot(mean.proj.act, aes(x = activity, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.3),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Activity",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-activity-NO.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ activity, mean.proj.acc) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.44606    0.01804  24.723  < 2e-16 ***
# activityyes -0.21404    0.03273  -6.539 1.61e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3278 on 472 degrees of freedom
# Multiple R-squared:  0.08306,	Adjusted R-squared:  0.08112 
# F-statistic: 42.76 on 1 and 472 DF,  p-value: 1.614e-10

### H.3.2 by predicate type ----
# how many predicates in which predicateType and activity / no activity?
d.proj.acc %>%
  select(predicateType2, verb_renamed, activity) %>%
  unique() %>%
  group_by(predicateType2, activity) %>%
  summarise(count=n())
#   predicateType2 activity count
#   <chr>          <chr>    <int>
# 1 cognitive      no          40
# 2 cognitive      yes         12
# 3 emoComm        no           2
# 4 emoComm        yes         25
# 5 emotive        no         143
# 6 emotive        yes          1
# 7 evidential     no          67
# 8 evidential     yes         10
# 9 nonEmoComm     no          78
# 10 nonEmoComm     yes         96

#### plot ----
mean.proj.act2 <- d.proj.acc %>%
  group_by(predicateType2, activity) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
nrow(mean.proj.act2) # 10

ggplot(mean.proj.act2, aes(x = predicateType2, y = Mean.Proj, colour = activity)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.3),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type",
       colour = "Activity") +
  scale_x_discrete(labels = predicateType2_names) + 
  scale_y_continuous(limits = c(-.25, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("no" = "slateblue", "yes" = "red3"))
ggsave("../graphs/projection-by-predicateType-and-activity-NO.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ activity * fct_relevel(predicateType2, "cognitive"), mean.proj.acc) %>% 
  summary()
# predicateType2  significance of no vs yes
# cognitive       *
# emoComm         *
# emotive         .
# evidential      n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.2423 on 464 degrees of freedom
# Multiple R-squared:  0.5073,	Adjusted R-squared:  0.4978 
# F-statistic: 53.09 on 9 and 464 DF,  p-value: < 2.2e-16

## H.4 evidence type ----
# how many predicates in which evidence type?
d.proj.acc %>%
  filter(predicateType2 == "evidential") %>% 
  select(evidenceType, verb_renamed) %>% 
  unique() %>% 
  group_by(evidenceType) %>% 
  summarise(count=n())
#   evidenceType count
#   <chr>        <int>
# 1 inferential     37
# 2 perceptual      10
# 3 reportative     30

#### plot ----
mean.proj.evT <- d.proj.acc %>%
  filter(predicateType2 == "evidential") %>% 
  group_by(evidenceType) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         evidenceType = fct_reorder(as.factor(evidenceType), Mean.Proj))
nrow(mean.proj.evT) # 3

ggplot(mean.proj.evT, aes(x = evidenceType, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.3),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Evidence type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) 
ggsave("../graphs/projection-by-evidenceType-NO.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ evidenceType, 
   mean.proj.acc %>% filter(predicateType2 == "evidential")) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.263964   0.050671   5.209 1.65e-06 ***
# evidenceTypeperceptual  0.036036   0.109851   0.328    0.744    
# evidenceTypereportative 0.006777   0.075724   0.089    0.929    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3082 on 74 degrees of freedom
# Multiple R-squared:  0.001454,	Adjusted R-squared:  -0.02553 
# F-statistic: 0.05388 on 2 and 74 DF,  p-value: 0.9476



## X VAD ratings ----

### X.5 projection: VAD against projection ratings ----
#### X.5.1 valence ----
##### X.5.1.1 overall ---- 
###### plots ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-NO.pdf", height = 3.5, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
ggsave("../graphs/projection-by-valence2-NO.pdf", height = 4, width = 6)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-faceted-NO.pdf", height = 4, width = 6)

# projection by valence faceted with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.15220    0.02882   5.281 2.18e-07 ***
# V.Mean.Sum2  0.69363    0.08208   8.451 6.49e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3121 on 375 degrees of freedom
# Multiple R-squared:   0.16,	Adjusted R-squared:  0.1577 
# F-statistic: 71.41 on 1 and 375 DF,  p-value: 6.489e-16

# by predicate type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType   significance of V.Mean.Sum2
# communicative   **
# predicateType2  significance of V.Mean.Sum2
# cognitive       **
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.2425 on 367 degrees of freedom
# Multiple R-squared:  0.5036,	Adjusted R-squared:  0.4915 
# F-statistic: 41.37 on 9 and 367 DF,  p-value: < 2.2e-16

# Valence is significant overall. Within predicate types, it is only significant for cognitives and
# emoComms.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   2.7649     0.1849   14.95   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2
# cognitive         **
# emoComm           ***
# emotive           n.s.
# evidential        n.s.
# nonEmoComm        .

##### X.5.1.2 with direction ----
# projection by valence with direction
###### plots ----
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
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-valence-with-direction-NO.pdf", height = 3.5, width = 6)

# projection by valence faceted with emotive component
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
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) +
  geom_text(data = new.scale %>% count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

# by direction of valence
mean.proj.Vdir <- d.proj.vad %>%
  group_by(V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir) # 2

ggplot(mean.proj.Vdir, aes(x = V.Mean.Sum2.direction, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Direction of valence") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-Vdir-NO.pdf", height = 4, width = 4)

# by direction of valence and predicate type
mean.proj.Vdir2 <- d.proj.vad %>%
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir2) # 10

ggplot(mean.proj.Vdir2, aes(x = predicateType2, y = Mean.Proj, colour = V.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.3)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 22, 5.5, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Direction of valence") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names)
ggsave("../graphs/projection-by-Vdir-predType2-NO.pdf", height = 4, width = 7.5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.44268    0.02538  17.443  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.16088    0.03425  -4.697 3.71e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3309 on 375 degrees of freedom
# Multiple R-squared:  0.05557,	Adjusted R-squared:  0.05305 
# F-statistic: 22.06 on 1 and 375 DF,  p-value: 3.708e-06

# by direction of valence and predicate type
lm(Mean.Proj ~ V.Mean.Sum2.direction * fct_relevel(predicateType, "communicative"), data = new.scale) %>% 
  summary()
# predicateType   significance of V.Mean.Sum2.direction
# communicative   **
# predicateType2  significance of V.Mean.Sum2.direction
# cognitive       .
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      *

# valence and direction of valence
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), data = new.scale) %>% 
  summary()
# V.Mean.Sum2.direction significance of V.Mean.Sum2
# negative              ***
# positive              ***

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(predicateType2, "cognitive") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale) %>% 
  summary()
# predicateType	  V.Mean.Sum2.direction   significance of V.Mean.Sum2
# communicative   negative                .
# communicative   positive                .
# predicateType2	V.Mean.Sum2.direction   significance of V.Mean.Sum2
# cognitive       negative                .
# emoComm         negative                *
# emotive         negative                n.s.
# evidential      negative                n.s.
# nonEmoComm      negative                n.s.
# cognitive       positive                *
# emoComm         positive                n.s.
# emotive         positive                n.s.
# evidential      positive                n.s.
# nonEmoComm      positive                .

# Residual standard error: 0.2417 on 357 degrees of freedom
# Multiple R-squared:  0.5205,	Adjusted R-squared:  0.4949 
# F-statistic: 20.39 on 19 and 357 DF,  p-value: < 2.2e-16

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.59411    0.06888  -8.625   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "evidential") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2.direction
# cognitive         n.s.
# emoComm           n.s.
# emotive           n.s.
# evidential        n.s.
# nonEmoComm        **


clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                 2.8647     0.2579  11.109  < 2e-16 ***
# V.Mean.Sum2.directionpositive              -0.3398     0.1255  -2.708  0.00678 ** 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.4922     0.3646  -1.350  0.17697   

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "cognitive") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction significance of V.Mean.Sum2
# cognitive       negative  *
# emoComm         negative  *
# emotive         negative  n.s.
# evidential      negative  n.s.
# nonEmoComm      negative  n.s.
# cognitive       positive  **
# emoComm         positive  *
# emotive         positive  n.s.
# evidential      positive  n.s.
# nonEmoComm      positive  *

##### X.5.1.3 communicative type ----
###### plot ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ commType) 
ggsave("../graphs/projection-by-valence-communicative-faceted-NO.pdf", height = 4, width = 6)

##### X.5.1.4 say verbs ----
###### say vs non-say verbs ----
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayVerb, 
             labeller = as_labeller(c("no" = "non-say-verb", 
                                      "yes" = "say-verb"))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-valence-say-vs-non-say-NO.pdf", height = 4, width = 5)

new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayVerb, 
             labeller = labeller(.cols = as_labeller(c("no" = "non-say-verb", 
                                                       "yes" = "say-verb")),
                                 .rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# overall
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerb, "no"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of V.Mean.Sum2
# no      *         
# yes     *

# Residual standard error: 0.2376 on 166 degrees of freedom
# Multiple R-squared:  0.06414,	Adjusted R-squared:  0.04723 
# F-statistic: 3.793 on 3 and 166 DF,  p-value: 0.01152

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of V.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              .
# no		              positive                *
# yes		              positive                n.s.

# Residual standard error: 0.2325 on 162 degrees of freedom
# Multiple R-squared:  0.1253,	Adjusted R-squared:  0.0875 
# F-statistic: 3.315 on 7 and 162 DF,  p-value: 0.002525

###### X.5.1.4.1 say-verb type ----
####### plots ----
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !verb_renamed == "say") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayVerbType) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-valence-sayverb-faceted-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !verb_renamed == "say") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of V.Mean.Sum2
# discourse role verb n.s.
# mode verb           .

# Residual standard error: 0.2355 on 88 degrees of freedom
# (77 observations deleted due to missingness)
# Multiple R-squared:  0.1029,	Adjusted R-squared:  0.06214 
# F-statistic: 2.524 on 4 and 88 DF,  p-value: 0.04647

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of V.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              *
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2257 on 84 degrees of freedom
# (77 observations deleted due to missingness)
# Multiple R-squared:  0.2135,	Adjusted R-squared:  0.1386 
# F-statistic:  2.85 on 8 and 84 DF,  p-value: 0.00749

###### X.5.1.4.2 mode-verb type ---- 
####### plots ----
new.scale %>% 
  filter(sayVerbType == "mode verb") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ modeVerbType) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerbType == "mode verb") %>% 
              count(modeVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-valence-modeverb-faceted-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(sayVerbType == "mode verb") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ modeVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) + 
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerbType == "mode verb") %>% 
              count(modeVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of V.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of V.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              *
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

###### X.5.1.4.3 say-by-means-verb type ----
####### plots ----
new.scale %>% 
  filter(modeVerbType == "say-by-means") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayByMeansVerbType) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-valence-saybymeans-faceted-NO.pdf", height = 4, width = 6)

# faceted by direction of valence
new.scale %>% 
  filter(modeVerbType == "say-by-means") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayByMeansVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "manner"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of V.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of V.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

#### X.5.2 arousal ----
##### X 5.2.1 overall ----
###### plots ----
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
                      labels = predicateType2_names) 
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
ggsave("../graphs/projection-by-arousal-faceted-NO.pdf", height = 4, width = 6)

# projection by arousal faceted with emotive component
ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-faceted2-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.05769    0.06063  -0.951    0.342    
# A.Mean.Sum2  0.99114    0.14036   7.062 8.04e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3199 on 375 degrees of freedom
# Multiple R-squared:  0.1174,	Adjusted R-squared:  0.115 
# F-statistic: 49.87 on 1 and 375 DF,  p-value: 8.036e-12

# by predicate type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType	  significance of A.Mean.Sum2
# communicative   *
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.2459 on 367 degrees of freedom
# Multiple R-squared:  0.4895,	Adjusted R-squared:  0.477 
# F-statistic:  39.1 on 9 and 367 DF,  p-value: < 2.2e-16

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.7472     0.3048   12.29   <2e-16 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s. 
# emotive         n.s.  
# evidential      n.s.
# nonEmoComm      n.s.


##### X.5.2.2 with direction of valence ----
###### plots ----
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
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-arousal-with-direction-NO.pdf", height = 3.5, width = 6)

# projection by arousal faceted with emotive component
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
                                 .cols = predicateType2_names)) +
  geom_text(data = new.scale %>% 
              count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

###### linear model ----
# arousal and direction of valence
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), data = new.scale) %>% 
  summary()
# V.Mean.Sum2.direction significance of A.Mean.Sum2
# negative              ***
# positive              ***

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale) %>% 
  summary()
# predicateType	  V.Mean.Sum2.direction   significance of A.Mean.Sum2
# communicative   negative                n.s.
# communicative   positive                n.s.
# predicateType2	V.Mean.Sum2.direction   significance of A.Mean.Sum2
# cognitive       negative                n.s.
# emoComm         negative                n.s.
# emotive         negative                n.s.
# evidential      negative                n.s.
# nonEmoComm      negative                n.s.
# cognitive       positive                n.s.
# emoComm         positive                n.s.
# emotive         positive                n.s.
# evidential      positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2469 on 357 degrees of freedom
# Multiple R-squared:  0.4994,	Adjusted R-squared:  0.4727 
# F-statistic: 18.74 on 19 and 357 DF,  p-value: < 2.2e-16

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                 3.3553     0.4567   7.347 2.02e-13 ***
# V.Mean.Sum2.directionpositive              -0.2774     0.2721  -1.019    0.308    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.1897     0.6313  -0.300    0.764    

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction	significance of A.Mean.Sum2
# cognitive	      negative  
# emoComm		      negative	n.s.
# emotive		      negative	
# evidential      negative  
# nonEmoComm	    negative  n.s.
# cognitive	      positive  
# emoComm		      positive  n.s.
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  n.s.

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2  n
# 1               negative      cognitive 14
# 2               negative        emoComm 21
# 3               negative        emotive 63
# 4               negative     evidential 15
# 5               negative     nonEmoComm 57
# 6               positive      cognitive 32
# 7               positive        emoComm  4
# 8               positive        emotive 34
# 9               positive     evidential 49
# 10              positive     nonEmoComm 88

###### negative / positive valence only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients: 
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.4233     0.4652   7.359 1.85e-13 *** [negative]

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.1694     0.4558   6.953 3.57e-12 *** [positive]

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2
# cognitive	      negative  
# emoComm	      	negative	n.s.
# emotive		      negative	
# evidential	    negative  
# nonEmoComm	    negative  n.s.
# cognitive	      positive  
# emoComm		      positive  n.s.
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  n.s

##### X.5.2.3 communicative type ----
###### plot ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ commType) 
ggsave("../graphs/projection-by-arousal-communicative-faceted-NO.pdf", height = 4, width = 6)

##### X.5.2.4 say verbs ----
###### say vs non-say verbs ----
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayVerb, 
             labeller = as_labeller(c("no" = "non-say-verb", 
                                      "yes" = "say-verb"))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-arousal-say-vs-non-say-NO.pdf", height = 4, width = 5)

new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayVerb, 
             labeller = labeller(.cols = as_labeller(c("no" = "non-say-verb", 
                                                       "yes" = "say-verb")),
                                 .rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))


###### linear models ----
# overall
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerb, "yes"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of A.Mean.Sum2
# no      n.s.         
# yes     n.s.

# Residual standard error: 0.2413 on 166 degrees of freedom
# Multiple R-squared:  0.03438,	Adjusted R-squared:  0.01693 
# F-statistic:  1.97 on 3 and 166 DF,  p-value: 0.1205

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerb, "no") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of A.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                .

# Residual standard error: 0.2357 on 162 degrees of freedom
# Multiple R-squared:  0.1015,	Adjusted R-squared:  0.06267 
# F-statistic: 2.614 on 7 and 162 DF,  p-value: 0.0139

###### X.5.2.4.1 say-verb type ----
####### plots ----
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !verb_renamed == "say") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayVerbType) + 
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-arousal-sayverb-faceted-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !verb_renamed == "say") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))


###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of A.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of V.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2309 on 84 degrees of freedom
# (77 observations deleted due to missingness)
# Multiple R-squared:  0.1766,	Adjusted R-squared:  0.09817 
# F-statistic: 2.252 on 8 and 84 DF,  p-value: 0.03129


###### X.5.2.4.2 mode-verb type ---- 
####### plots ----
new.scale %>% 
  filter(sayVerbType == "mode verb") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ modeVerbType) +
  geom_text(data = new.scale %>% 
              filter(sayVerbType == "mode verb") %>% 
              count(modeVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-arousal-modeverb-faceted-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(sayVerbType == "mode verb") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ modeVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(sayVerbType == "mode verb") %>% 
              count(modeVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of A.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.2168 on 43 degrees of freedom
# (123 observations deleted due to missingness)
# Multiple R-squared:  0.09586,	Adjusted R-squared:  0.03278 
# F-statistic:  1.52 on 3 and 43 DF,  p-value: 0.223

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of A.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

# Residual standard error: 0.2265 on 39 degrees of freedom
# (123 observations deleted due to missingness)
# Multiple R-squared:  0.1047,	Adjusted R-squared:  -0.05603 
# F-statistic: 0.6513 on 7 and 39 DF,  p-value: 0.711


###### X.5.2.4.3 say-by-means-verb type ----
####### plots ----
new.scale %>% 
  filter(modeVerbType == "say-by-means") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayByMeansVerbType, ncol = 4) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-NO.pdf", height = 4, width = 6)

# faceted by direction of valence
new.scale %>% 
  filter(modeVerbType == "say-by-means") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayByMeansVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of A.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# Residual standard error: 0.2345 on 29 degrees of freedom
# (135 observations deleted due to missingness)
# Multiple R-squared:  0.0664,	Adjusted R-squared:  -0.09456 
# F-statistic: 0.4125 on 5 and 29 DF,  p-value: 0.8361

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of A.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n/a

# Residual standard error: 0.244 on 24 degrees of freedom
# (135 observations deleted due to missingness)
# Multiple R-squared:  0.1628,	Adjusted R-squared:  -0.186 
# F-statistic: 0.4669 on 10 and 24 DF,  p-value: 0.8951

#### X.5.3 dominance ---- 
##### X.5.3.1 overall ----
###### plots ----
# projection by dominance with emotive component 
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = D.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-dominance2-NO.pdf", height = 4, width = 6)

ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-faceted2-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.3375     0.0335  10.075   <2e-16 ***
# D.Mean.Sum2   0.0721     0.1219   0.591    0.555    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3403 on 375 degrees of freedom
# Multiple R-squared:  0.0009321,	Adjusted R-squared:  -0.001732 
# F-statistic: 0.3498 on 1 and 375 DF,  p-value: 0.5546

# by predicate type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType   significance of D.Mean.Sum2
# communicative   n.s.
# predicateType2  significance of D.Mean.Sum2
# cognitive:      n.s.
# emoComm:        n.s.
# emotive:        n.s.
# evidential:     n.s.
# nonEmoComm:     n.s.

# Residual standard error: 0.2455 on 367 degrees of freedom
# Multiple R-squared:  0.491,	Adjusted R-squared:  0.4786 
# F-statistic: 39.34 on 9 and 367 DF,  p-value: < 2.2e-16

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# D.Mean.Sum2   0.6346     0.2462   2.578  0.00995 **

clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.   
# evidential      n.s.
# nonEmoComm      n.s.


##### X5.3.2 with direction of valence ----
# happy vs unhappy
###### plots ----
# projection by dominance with direction
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-domiance-with-direction-of-valence-NO.pdf", height = 3.5, width = 6)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) +
  geom_text(data = new.scale %>% 
              count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

###### linear model ----
# dominance and direction of valence
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), data = new.scale) %>% 
  summary()
# V.Mean.Sum2.direction significance of D.Mean.Sum2
# negative              n.s.
# positive              n.s.

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale) %>% 
  summary()
# predicateType	  V.Mean.Sum2.direction   significance of D.Mean.Sum2
# communicative   negative                n.s.
# communicative   positive                n.s.
# predicateType2  V.Mean.Sum2.direction   significance of D.Mean.Sum2
# cognitive       negative                n.s.
# emoComm         negative                n.s.
# emotive         negative                n.s.
# evidential      negative                *
# nonEmoComm      negative                n.s.
# cognitive       positive                n.s.
# emoComm         positive                n.s.
# emotive         positive                n.s.
# evidential      positive                .
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2429 on 357 degrees of freedom
# Multiple R-squared:  0.5155,	Adjusted R-squared:  0.4897 
# F-statistic: 19.99 on 19 and 357 DF,  p-value: < 2.2e-16

###### ordinal models ----
# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.0960     0.3631   3.018  0.00254 ** 
# V.Mean.Sum2.directionpositive              -0.5719     0.1304  -4.386 1.15e-05 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.3617     0.4880  -0.741  0.45854  

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         *
# negative  evidential      
# negative  nonEmoComm      n.s.
# positive  cognitive       
# positive  emotive         
# positive  emoComm         *
# positive  evidential      
# positive  nonEmoComm      n.s.
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##### X.5.3.3 with "direction" of dominance ----
# controlled/submissive/guided/... vs in control/ autonomous/controlling/... 
###### plots ----
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = D.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-domiance-with-direction-of-dominance-NO.pdf", height = 3.5, width = 6)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(D.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")), 
                                 .cols = predicateType2_names)) +
  geom_text(data = new.scale %>% count(predicateType2, D.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2-NO.pdf", 
       height = 6, width = 10)

# by direction of dominance
mean.proj.Ddir <- d.proj.vad %>%
  group_by(D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir) # 2

ggplot(mean.proj.Ddir, aes(x = D.Mean.Sum2.direction, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Direction of dominance") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-Ddir-NO.pdf", height = 4, width = 4)

# by direction of dominance and predicate type
mean.proj.Ddir2 <- d.proj.vad %>%
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir2) # 10

ggplot(mean.proj.Ddir2, aes(x = predicateType2, y = Mean.Proj, colour = D.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.3)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 22, 5.5, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Direction of dominance") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names)
ggsave("../graphs/projection-by-Ddir-predType2-NO.pdf", height = 4, width = 7.5)


###### linear models ----
# by direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.45908    0.02960  15.509  < 2e-16 ***
# D.Mean.Sum2.directionpositive -0.15731    0.03628  -4.336 1.86e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3323 on 375 degrees of freedom
# Multiple R-squared:  0.04775,	Adjusted R-squared:  0.04521 
# F-statistic:  18.8 on 1 and 375 DF,  p-value: 1.864e-05

# by direction of dominance and predicate type
lm(Mean.Proj ~ D.Mean.Sum2.direction * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType   significance of D.Mean.Sum2.direction
# communicative   n.s.
# predicateType2  significance of D.Mean.Sum2.direction
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# dominance and direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive"), data = new.scale) %>% 
  summary()
# D.Mean.Sum2.direction significance of D.Mean.Sum2
# negative              n.s.
# positive              n.s.

# by predicate type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale) %>% 
  summary()
# predicateType	  D.Mean.Sum2.direction   significance of D.Mean.Sum2
# communicative   negative                n.s.
# communicative   positive                n.s.
# predicateType2	D.Mean.Sum2.direction   significance of D.Mean.Sum2
# cognitive       negative                n.s.
# emoComm         negative                n.s.
# emotive         negative                n.s.
# evidential      negative                *
# nonEmoComm      negative                n.s.
# cognitive       positive                n.s.
# emoComm         positive                n.s.
# emotive         positive                n.s.
# evidential      positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2437 on 357 degrees of freedom
# Multiple R-squared:  0.5125,	Adjusted R-squared:  0.4865 
# F-statistic: 19.75 on 19 and 357 DF,  p-value: < 2.2e-16

##### X.5.3.4 communicative type ----
###### plot ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ commType) 
ggsave("../graphs/projection-by-dominance-communicative-faceted-NO.pdf", height = 4, width = 6)

##### X.5.3.5 say verbs ----
###### say vs non-say verbs ----
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayVerb, 
             labeller = as_labeller(c("no" = "non-say-verb", 
                                      "yes" = "say-verb"))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-say-vs-non-say-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayVerb, 
             labeller = labeller(.cols = as_labeller(c("no" = "non-say-verb", 
                                                       "yes" = "say-verb")),
                                 .rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

# faceted by direction of dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(D.Mean.Sum2.direction ~ sayVerb, 
             labeller = labeller(.cols = as_labeller(c("no" = "non-say-verb", 
                                                       "yes" = "say-verb")),
                                 .rows = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(sayVerb, D.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# overall
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayVerb, "yes"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of D.Mean.Sum2
# no      n.s.         
# yes     n.s.

# Residual standard error: 0.2441 on 166 degrees of freedom
# Multiple R-squared:  0.01226,	Adjusted R-squared:  -0.005593 
# F-statistic: 0.6867 on 3 and 166 DF,  p-value: 0.5614

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of D.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                n.s.

# Residual standard error: 0.2378 on 162 degrees of freedom
# Multiple R-squared:  0.08482,	Adjusted R-squared:  0.04527 
# F-statistic: 2.145 on 7 and 162 DF,  p-value: 0.04181

# with direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(D.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            D.Mean.Sum2.direction   significance of D.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                n.s.

###### X.5.3.5.1 say-verb type ----
####### plots ----
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !verb_renamed == "say") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayVerbType) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-sayverb-faceted-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !verb_renamed == "say") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of D.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# Residual standard error: 0.2381 on 88 degrees of freedom
# (77 observations deleted due to missingness)
# Multiple R-squared:  0.08297,	Adjusted R-squared:  0.04129 
# F-statistic: 1.991 on 4 and 88 DF,  p-value: 0.1029

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of D.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2301 on 84 degrees of freedom
# (77 observations deleted due to missingness)
# Multiple R-squared:  0.182,	Adjusted R-squared:  0.1041 
# F-statistic: 2.336 on 8 and 84 DF,  p-value: 0.02563

###### X.5.3.5.2 mode-verb type ---- 
####### plots ----
new.scale %>% 
  filter(sayVerbType == "mode verb") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ modeVerbType) +
  geom_text(data = new.scale %>% 
              filter(sayVerbType == "mode verb") %>% 
              count(modeVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-modeverb-faceted-NO.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(sayVerbType == "mode verb") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ modeVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(sayVerbType == "mode verb") %>% 
              count(modeVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of D.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.216 on 43 degrees of freedom
# (123 observations deleted due to missingness)
# Multiple R-squared:  0.1024,	Adjusted R-squared:  0.03983 
# F-statistic: 1.636 on 3 and 43 DF,  p-value: 0.1951

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of D.Mean.Sum2
# say-by-means	      negative                .
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

# Residual standard error: 0.2187 on 39 degrees of freedom
# (123 observations deleted due to missingness)
# Multiple R-squared:  0.1653,	Adjusted R-squared:  0.01552 
# F-statistic: 1.104 on 7 and 39 DF,  p-value: 0.3802

###### X.5.3.5.3 say-by-means-verb type ----
####### plots ----
new.scale %>% 
  filter(modeVerbType == "say-by-means") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayByMeansVerbType, ncol = 4) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-NO.pdf", height = 4, width = 6)

# faceted by direction of valence
new.scale %>% 
  filter(modeVerbType == "say-by-means") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ sayByMeansVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")))) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of D.Mean.Sum2
# form                n.s.
# manner              .
# sound               n.s.

# Residual standard error: 0.2237 on 29 degrees of freedom
# (135 observations deleted due to missingness)
# Multiple R-squared:   0.15,	Adjusted R-squared:  0.003401 
# F-statistic: 1.023 on 5 and 29 DF,  p-value: 0.4223

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "form") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of D.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              .
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

# Residual standard error: 0.2401 on 24 degrees of freedom
# (135 observations deleted due to missingness)
# Multiple R-squared:  0.1894,	Adjusted R-squared:  -0.1483 
# F-statistic: 0.5608 on 10 and 24 DF,  p-value: 0.8289

#### X.5.4 valence + arousal ----
##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.06600    0.05782  -1.142    0.254    
# V.Mean.Sum2  0.54317    0.08744   6.212 1.40e-09 ***
# A.Mean.Sum2  0.63034    0.14587   4.321 1.99e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.305 on 374 degrees of freedom
# Multiple R-squared:  0.1999,	Adjusted R-squared:  0.1956 
# F-statistic: 46.73 on 2 and 374 DF,  p-value: < 2.2e-16

library(car)
vif(lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale))
# V.Mean.Sum2 A.Mean.Sum2 
# 1.188422    1.188422 

lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)             -0.07229    0.10893  -0.664   0.5073  
# V.Mean.Sum2              0.56330    0.30793   1.829   0.0681 .
# A.Mean.Sum2              0.64567    0.26812   2.408   0.0165 *
# V.Mean.Sum2:A.Mean.Sum2 -0.04565    0.66946  -0.068   0.9457  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3054 on 373 degrees of freedom
# Multiple R-squared:  0.1999,	Adjusted R-squared:  0.1935 
# F-statistic: 31.07 on 3 and 373 DF,  p-value: < 2.2e-16

vif(lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, data = new.scale))
# V.Mean.Sum2             A.Mean.Sum2 V.Mean.Sum2:A.Mean.Sum2 
#   14.697613                4.004393               22.236947 

lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, data = new.scale) %>% 
  summary()
# Coefficients:
#                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.354765   0.016879  21.018  < 2e-16 ***
# V.Mean.Sum2.sc                 0.106728   0.017485   6.104 2.60e-09 ***
# A.Mean.Sum2.sc                 0.074327   0.017520   4.242 2.79e-05 ***
# V.Mean.Sum2.sc:A.Mean.Sum2.sc -0.001052   0.015428  -0.068    0.946    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3054 on 373 degrees of freedom
# Multiple R-squared:  0.1999,	Adjusted R-squared:  0.1935 
# F-statistic: 31.07 on 3 and 373 DF,  p-value: < 2.2e-16

vif(lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, data = new.scale))
# V.Mean.Sum2.sc                A.Mean.Sum2.sc V.Mean.Sum2.sc:A.Mean.Sum2.sc 
#       1.232741                      1.237680                      1.130786 

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2               2.1330     0.5963   3.577 0.000348 ***
# V.Mean.Sum2               1.9614     0.6988   2.807 0.005006 ** 
# A.Mean.Sum2:V.Mean.Sum2   0.6482     1.5431   0.420 0.674420 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive           
# emoComm         n.s.   *  
# emotive          
# evidential      
# nonEmoComm      n.s.  n.s.

##### negative / positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [negative]
# A.Mean.Sum2               2.5037     0.9017   2.777 0.005492 ** 
# V.Mean.Sum2               3.8308     1.1238   3.409 0.000652 ***
# A.Mean.Sum2:V.Mean.Sum2  -3.2042     2.3393  -1.370 0.170773   


# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [positive]
# A.Mean.Sum2               1.3366     0.8416   1.588    0.112
# V.Mean.Sum2               1.1989     0.9344   1.283    0.199
# A.Mean.Sum2:V.Mean.Sum2   2.4832     2.1828   1.138    0.255

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive	      negative       
# emoComm	      	negative	n.s.  **
# emotive		      negative	
# evidential	    negative  
# nonEmoComm	    negative  n.s.  n.s.
# cognitive	      positive  
# emoComm		      positive  Variance-covariance matrix of the parameters is not defined
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  Variance-covariance matrix of the parameters is not defined


#### X.5.5 valence + arousal + dominance ----
##### combined plots ----
###### two lines ----
# combined plot with two fitted lines
# valence
vplot <- ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
                      labels = predicateType2_names) 

# arousal
aplot <- ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)

# dominance
dplot <- ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 

vplot + aplot + dplot + plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "top", 
        legend.box = "vertical", 
        legend.box.just = "left", 
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.spacing.y = unit(-6, "pt"))
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-NO.pdf", height = 4.5, width = 9.5)

###### valence facets ----
# combined plot with valence facets
# valence
vplotf <- 
  ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) + 
  facet_wrap(~ V.Mean.Sum2.direction, ncol = 1, strip.position = "right", 
             labeller = as_labeller(c("negative" = "negative valence", 
                                      "positive" = "positive valence"))) +
  geom_text(data = new.scale %>% count(V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

# arousal
aplotf <- ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_wrap(~ V.Mean.Sum2.direction, ncol = 1, strip.position = "right", 
             labeller = as_labeller(c("negative" = "negative valence", 
                                      "positive" = "positive valence"))) +
  geom_text(data = new.scale %>% count(V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

# dominance
dplotf <- 
  ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% count(D.Mean.Sum2.direction, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

vplotf + aplotf + dplotf + plot_layout(guides = "collect", axis_titles = "collect", widths = c(1, 1, 2.03)) &
  theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-facets-NO.pdf", height = 5.5, width = 10)

###### valence and dominance facets ----
# valence faceted by valence and dominance directions
vplotf2 <-  
  ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% count(D.Mean.Sum2.direction, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

vplotf2 + dplotf + plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-NO.pdf", height = 5.5, width = 9)

# arousal faceted by valence and dominance directions
aplotf2 <-  
  ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% count(D.Mean.Sum2.direction, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

vplotf2 + aplotf2 + dplotf + plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-2-NO.pdf", height = 5.5, width = 14)

###### valence, dominance, predicate type facets ----
# valence faceted by predicate type and valence and dominance directions
vplotf3.1 <-     
  new.scale %>% 
  filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence rating",
  #      y = "Mean projection rating",
  #      colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction,
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

vplotf3.2 <-      
  new.scale %>% 
  filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence rating",
  #      y = "Mean projection rating",
  #      colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction,
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

layout <- "
AAAAAAAAAA
BBBBBBB###
"

vplotf3.1 + vplotf3.2 + plot_layout(design = layout) + theme(axis.title.y = element_text(size = 16, hjust = 2.8),
                                                             axis.title.x = element_text(size = 16, hjust = .85)) +
  labs(x = "Mean valence rating",
       y = "Mean projection rating")
ggsave("../graphs/projection-by-valence-Vdir-Ddir-predType2-NO.pdf", height = 10, width = 11)

# arousal faceted by predicate type and valence and dominance directions
aplotf3.1 <-   
  new.scale %>% 
  filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence rating",
  #      y = "Mean projection rating",
  #      colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction,
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

aplotf3.2 <-       
  new.scale %>% 
  filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence rating",
  #      y = "Mean projection rating",
  #      colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction,
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

layout <- "
AAAAAAAAAA
BBBBBBB###
"

aplotf3.1 + aplotf3.2 + plot_layout(design = layout) + theme(axis.title.y = element_text(size = 16, hjust = 2.8),
                                                             axis.title.x = element_text(size = 16, hjust = .85)) +
  labs(x = "Mean arousal rating",
       y = "Mean projection rating")
ggsave("../graphs/projection-by-arousal-Vdir-Ddir-predType2-NO.pdf", height = 10, width = 11)


# dominance faceted by predicate type, direction of valence and dominance
dplotf3.1 <-   
  new.scale %>% 
  filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence rating",
  #      y = "Mean projection rating",
  #      colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction,
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

dplotf3.2 <-       
  new.scale %>% 
  filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence rating",
  #      y = "Mean projection rating",
  #      colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction,
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04) +

layout <- "
AAAAAAAAAA
BBBBBBB###
"

dplotf3.1 + dplotf3.2 + plot_layout(design = layout) + theme(axis.title.y = element_text(size = 16, hjust = 2.8),
                                                             axis.title.x = element_text(size = 16, hjust = .85)) +
  labs(x = "Mean dominance rating",
       y = "Mean projection rating")
ggsave("../graphs/projection-by-dominance-Vdir-Ddir-predType2-NO.pdf", height = 10, width = 11)

# VAD fitted lines only
new.scale %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04, size = 3)
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-NO.pdf", height = 4.5, width = 13.5)

# wrapped
VAD_lines1 <- 
  new.scale %>% 
  filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(panel.grid.minor.y = element_blank(), 
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence / arousal / dominance rating", 
  #      y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("cognitive", "emoComm", "emotive")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

VAD_lines2 <- 
  new.scale %>% 
  filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(panel.grid.minor.y = element_blank(), 
        strip.text = element_text(size = 12), 
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  # labs(x = "Mean valence / arousal / dominance rating", 
  #      y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)

# # labels for significances  
#   geom_text(data = new.scale %>%
#               filter(predicateType2 %in% c("evidential", "nonEmoComm")) %>%
#               count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2) %>%
#               mutate(significanceV = c("***", "n.s.", "n.s.", "**", "n.s.", "n.s.","n.s.", "n.s."),
#                      significanceA = c("**", ".", "n.s.", "***", "*", "n.s.","n.s.", "***"),
#                      significanceD = c("n.s.", "n.s.", "n.s.", "n.s", ".", "n.s.","*", "n.s.")), # dummy values
#             aes(x = Inf, y = .8, 
#                 label = paste0("significances:\n", significanceV, " / ", significanceA, " / ", significanceD)), 
#             hjust = 1.04)

layout <- "
AAAAAAAAAA
BBBBBBB###
"
VAD_lines1 + VAD_lines2 + plot_layout(design = layout) + 
  theme(axis.title.y = element_text(size = 16, hjust = 2.8),
        axis.title.x = element_text(size = 16, hjust = 1.2)) +
  labs(x = "Mean valence / arousal / dominance rating",
       y = "Mean projection rating")
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-wrapped-NO.pdf", height = 10, width = 11)

# VAD fitted lines only, with scaled ratings
new.scale %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(aes(x = V.Mean.Sum2.sc, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2.sc, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2.sc, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  #scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04, size = 3)
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-scaled-NO.pdf", height = 4.5, width = 13.5)

#### X.5.6 directions of valence and dominance ----
##### tables ----
d.proj.vad %>%
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            count = length(unique(verb_renamed))) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  group_by(predicateType2) %>% 
  arrange(predicateType2, desc(Mean.Proj)) %>% 
  relocate(count, .after = last_col())
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction Mean.Proj  CILow CIHigh YMin.Proj YMax.Proj count
#   <chr>          <chr>                 <chr>                     <dbl>  <dbl>  <dbl>     <dbl>     <dbl> <int>
# 1 cognitive      negative              negative                  0.329 0.139  0.127     0.190      0.456     8
# 2 cognitive      negative              positive                  0.267 0.133  0.133     0.133      0.4       6
# 3 cognitive      positive              positive                  0.211 0.0602 0.0536    0.151      0.264    30
# 4 cognitive      positive              negative                  0.15  0.3    0.3      -0.15       0.45      2
# 5 emoComm        negative              negative                  0.392 0.108  0.1       0.283      0.492    12
# 6 emoComm        positive              positive                  0.333 0.267  0.234     0.0667     0.567     3
# 7 emoComm        negative              positive                  0.311 0.144  0.144     0.167      0.456     9
# 8 emoComm        positive              negative                  0.2   0.5    0.5      -0.3        0.7       1
# 9 emotive        positive              positive                  0.765 0.0564 0.0565    0.708      0.821    32
# 10 emotive        negative              positive                  0.75  0.111  0.0926    0.639      0.843    11
# 11 emotive        negative              negative                  0.742 0.0426 0.0426    0.700      0.785    52
# 12 emotive        positive              negative                  0.65  0.3    0.25      0.35       0.9       2
# 13 evidential     negative              positive                  0.304 0.159  0.159     0.145      0.464     7
# 14 evidential     positive              positive                  0.261 0.0523 0.0545    0.209      0.316    46
# 15 evidential     positive              negative                  0.167 0.233  0.233    -0.0667     0.4       3
# 16 evidential     negative              negative                  0.125 0.162  0.15     -0.0375     0.275     8
# 17 nonEmoComm     negative              positive                  0.242 0.0669 0.0669    0.175      0.309    27
# 18 nonEmoComm     negative              negative                  0.228 0.0638 0.0571    0.164      0.285    30
# 19 nonEmoComm     positive              negative                  0.212 0.125  0.113     0.0875     0.325     8
# 20 nonEmoComm     positive              positive                  0.132 0.0415 0.0377    0.0905     0.170    80

# V Volition ----
## V.1 all predicates ----
### V.1.1 by predicate ----
#### plots ----
ggplot(mean.proj.acc, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red","green3"))
ggsave("../graphs/projection-by-volition-NO.pdf", height = 4, width = 13)

# faceted by predicate type
ggplot(mean.proj.acc, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red","green3")) +
  facet_wrap( ~ predicateType2, 
              labeller = as_labeller(predicateType2_names), ncol = 5)
ggsave("../graphs/projection-by-volition-and-predicateType2-NO.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.19140    0.02246   8.522   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.31211    0.02881  10.832   <2e-16 ***

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol <- d.proj.acc %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj.vol
nrow(mean.proj.vol) # 9

ggplot(mean.proj.vol, aes(x = predicateType2, y = Mean.Proj, colour = volition)) +
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
       colour = element_blank()) +
  scale_x_discrete(labels = predicateType2_names) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("volitional" = "green3", "non-volitional" = "red"))
ggsave("../graphs/projection-by-predicateType-and-volition-NO.pdf", height = 4, width = 10)

#### distribution ----
# how many predicates of which predicateType are non-/volitional?
d.proj.acc %>%
  select(predicateType2, verb_renamed, volition) %>%
  unique() %>%
  group_by(predicateType2, volition) %>%
  summarize(count=n())

#   predicateType2 volition       count
#   <chr>          <chr>          <int>
# 1 cognitive      non-volitional    42
# 2 cognitive      volitional        10
# 3 emoComm        non-volitional    16
# 4 emoComm        volitional        11
# 5 emotive        non-volitional   144
# 6 evidential     non-volitional    73
# 7 evidential     volitional         4
# 8 nonEmoComm     non-volitional    13
# 9 nonEmoComm     volitional       161

# All emotives are non-volitional. Almost all evidentials are non-volitional.

## V.2 only communicatives ----
### by predicate ----
#### plot ----
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("red","green3"))
ggsave("../graphs/projection-by-volition-comm-NO.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.18320    0.01849   9.909  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.15856    0.04867   3.258  0.00132 ** 


#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, volition) %>% 
  count()
#   predicateType2 volition           n
#   <chr>          <chr>          <int>
# 1 emoComm        non-volitional    16
# 2 emoComm        volitional        11
# 3 nonEmoComm     non-volitional    13
# 4 nonEmoComm     volitional       161


# Y Veridicality ----
## plots ----
mean.proj.verid %>% 
  ggplot(aes(x = Mean.Verid, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, linewidth = 0.5) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean veridicality rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_colour_manual(values = cols2,
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-veridicality2-NO.pdf", height = 4, width = 6)

# faceted
mean.proj.verid %>% 
  ggplot(aes(x = Mean.Verid, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, show.legend = FALSE, linewidth = 0.5, colour = "grey20") +
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-veridicality2-faceted-NO.pdf", height = 3, width = 11.5)

## linear models ----
lm(Mean.Proj ~ Mean.Verid, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.14878    0.02672   5.568 4.34e-08 ***
# Mean.Verid   0.38348    0.03736  10.264  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3095 on 472 degrees of freedom
# Multiple R-squared:  0.1825,	Adjusted R-squared:  0.1807 
# F-statistic: 105.4 on 1 and 472 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ Mean.Verid * fct_relevel(predicateType, "communicative"), 
   mean.proj.verid) %>% 
  summary()
# predicate type  significance of Mean.Verid
# communicative   .
# predicate type2 significance of Mean.Verid
# cognitive       ***
# emoComm         *
# emotive         **
# evidential      ***
# nonEmoComm      n.s.

# Residual standard error: 0.232 on 464 degrees of freedom
# Multiple R-squared:  0.5482,	Adjusted R-squared:  0.5395 
# F-statistic: 62.56 on 9 and 464 DF,  p-value: < 2.2e-16


# Z projection by communicatives ----
## with think and know ----
### plot ----
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know", "believe")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "believe"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "believe")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "believe"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.2,
                   colour = "purple") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "believe", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "purple", "orangered3"))
ggsave("../graphs/projection-by-communicative-NO.pdf", height = 4, width = 13)

## with highly projective predicates ---- 
# communicative predicates that project as much or more strongly than 'know'
mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType)
#   verb_renamed Mean.Proj commType                sayVerbType        
#   <fct>            <dbl> <chr>                   <chr>              
# 1 apologize        1     discourse participation discourse role verb
# 2 fess up          0.9   discourse participation NA                 
# 3 disclose         0.8   discourse participation NA                 
# 4 warn             0.778 state changing          NA                 
# 5 cry              0.7   pure                    mode verb          
# 6 flaunt           0.7   discourse participation NA                 
# 7 log              0.7   pure                    mode verb          
# 8 point out        0.7   discourse participation discourse role verb
# 9 weep             0.7   pure                    mode verb          
# 10 whine            0.7   pure                    mode verb    

### plot ----
# with labels for highly projective predicates
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-highest-NO.pdf", height = 4, width = 13)


## with extreme predicates ----
# calculate difference in mean projection rating between the predicate with the 
# highest rating and 'know':
slice_max(mean.proj.comm, Mean.Proj, with_ties = FALSE)$Mean.Proj - 
  subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj
# 0.3

### lists ----
# communicative predicates whose projection rating ranges between that of 'think'
# and think + 0.3:
aaa <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                                  Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                  Mean.Proj >= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj) %>% 
  arrange(Mean.Proj) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

# communicative predicates with similar projection ratings as 'know': 
# (know - 0.2) to max [= know + 0.3] 
bbb <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                                  Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

### plots ----
# with labels for extreme predicates
#### 41-9 ----
# [name based on original plot] 
# 0 to (think + 0.3) and know to (know + 0.3)
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                               Mean.Proj >= 0), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                                     Mean.Proj >= 0),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-41-9-NO.pdf", height = 4, width = 13)

#### 12-9 ----
# [name based on original plot]

ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 12), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                               Mean.Proj >= 0), Mean.Proj, n = 12),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 2, nudge_y = -0.5,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-extremes-12-9-NO.pdf", height = 4, width = 13)

#### 28-26 ----
# [name based on original plot]

ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 28), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.1), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                               Mean.Proj >= 0), Mean.Proj, n = 28),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 2, nudge_y = -0.7,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.1),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-extremes-28-26-NO.pdf", height = 4, width = 13)


#### 41-44 ----
# [name based on original plot]
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 41), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                               Mean.Proj >= 0), Mean.Proj, n = 41),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 2, nudge_y = -0.7,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-extremes-41-44-NO.pdf", height = 4, width = 13)

## all emoComms ----
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComms", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType2 == "emoComm"),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "emoComms", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "green4", "orangered3"))
ggsave("../graphs/projection-by-communicative-emoComms-NO.pdf", height = 4, width = 13)

### all predicates ----
ggplot(mean.proj.acc, 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComms", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType2 == "emoComm"),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("predicate", "say", "think", "emoComms", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "green4", "orangered3"))
ggsave("../graphs/projection-by-predicate-emoComms-NO.pdf", height = 4, width = 13)

### manner - attitude ----
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(emoCommType == "manner"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoCommsManner", alpha = 0.9)) +
  geom_point(data = mean.proj.acc %>% filter(emoCommType == "attitude"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoCommsAttitude", alpha = 0.9)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(emoCommType == "attitude"),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "darkolivegreen") +
  geom_label_repel(data = mean.proj.acc %>% filter(emoCommType == "manner"),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "springgreen4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", 
                                 "emoCommsAttitude", "emoCommsManner", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", 
                                 "darkolivegreen", "springgreen4", "orangered3"))
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude-NO.pdf", height = 4, width = 13)

# ZZ combined models ----
## predicate type ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.all) %>% 
  summary()
 nrow(mean.proj.all) # 544
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.29428    0.04296   6.850 2.02e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.06954    0.05472  -1.271   0.2043    
# fct_relevel(predicateType2, "emoComm")comPriv    -0.16742    0.08319  -2.013   0.0446 *  
# fct_relevel(predicateType2, "emoComm")emotive     0.44274    0.04751   9.320  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.02826    0.05061  -0.558   0.5768    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.10659    0.04665  -2.285   0.0227 *  
# fct_relevel(predicateType2, "emoComm")other       0.14557    0.06281   2.318   0.0208 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2468 on 537 degrees of freedom
# Multiple R-squared:  0.4711,	Adjusted R-squared:  0.4652 
# F-statistic: 79.72 on 6 and 537 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj) %>% 
  summary()
nrow(mean.proj) # 503
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.29428    0.04222   6.971    1e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.06954    0.05377  -1.293   0.1966    
# fct_relevel(predicateType2, "emoComm")emotive     0.44274    0.04668   9.483   <2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.02826    0.04974  -0.568   0.5701    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.10659    0.04584  -2.325   0.0205 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2425 on 498 degrees of freedom
# Multiple R-squared:  0.4912,	Adjusted R-squared:  0.4871 
# F-statistic: 120.2 on 4 and 498 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.acc) %>% 
  summary()
nrow(mean.proj.acc) # 474
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04715   7.148 3.38e-12 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.10605    0.05811  -1.825  0.06865 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.40108    0.05138   7.806 3.87e-14 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.06575    0.05479  -1.200  0.23075    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15128    0.05067  -2.985  0.00298 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.245 on 469 degrees of freedom
# Multiple R-squared:  0.4909,	Adjusted R-squared:  0.4866 
# F-statistic: 113.1 on 4 and 469 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.comm) %>% 
  summary()
nrow(mean.proj.comm) # 201
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04684   7.195 1.24e-11 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15128    0.05034  -3.005    0.003 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2434 on 199 degrees of freedom
# Multiple R-squared:  0.0434,	Adjusted R-squared:  0.0386 
# F-statistic: 9.029 on 1 and 199 DF,  p-value: 0.002998

# Overall, predicate type is a very important predictor for projection ratings. Considering only
# communicative predicates, the two subtypes are highly significant, but this distinction per se 
# explains very little of the variance we observe.

## VAD ----
# predicate type and VAD ratings
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
nrow(new.scale) # 377
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.34800    0.04912   7.085 7.00e-12 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.11129    0.06102  -1.824  0.06899 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.40068    0.05508   7.274 2.08e-12 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.10234    0.05792  -1.767  0.07806 .  
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.17107    0.05318  -3.217  0.00141 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2456 on 372 degrees of freedom
# Multiple R-squared:  0.484,	Adjusted R-squared:  0.4784 
# F-statistic: 87.22 on 4 and 372 DF,  p-value: < 2.2e-16

### Valence ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.29578    0.05571   5.309 1.90e-07 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.10624    0.06084  -1.746  0.08161 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.38752    0.05528   7.010 1.13e-11 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.08294    0.05854  -1.417  0.15741    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14840    0.05423  -2.737  0.00651 ** 
# V.Mean.Sum2                                       0.14749    0.07522   1.961  0.05067 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2446 on 371 degrees of freedom
# Multiple R-squared:  0.4893,	Adjusted R-squared:  0.4824 
# F-statistic: 71.08 on 5 and 371 DF,  p-value: < 2.2e-16

# Valence adds nothing to the model (slightly less nothing than across all embedding environments).

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.11746    0.11616   1.011   0.3126    
# fct_relevel(predicateType2, "emoComm")cognitive              -0.04490    0.13645  -0.329   0.7423    
# fct_relevel(predicateType2, "emoComm")emotive                 0.62513    0.13085   4.777 2.57e-06 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.17913    0.12937   1.385   0.1670    
# fct_relevel(predicateType2, "emoComm")nonEmoComm              0.02516    0.12092   0.208   0.8353    
# V.Mean.Sum2                                                   0.65106    0.29809   2.184   0.0296 *  
# fct_relevel(predicateType2, "emoComm")cognitive:V.Mean.Sum2  -0.13790    0.35558  -0.388   0.6984    
# fct_relevel(predicateType2, "emoComm")emotive:V.Mean.Sum2    -0.63731    0.32285  -1.974   0.0491 *  
# fct_relevel(predicateType2, "emoComm")evidential:V.Mean.Sum2 -0.87990    0.36849  -2.388   0.0175 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:V.Mean.Sum2 -0.47983    0.32684  -1.468   0.1429    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2425 on 367 degrees of freedom
# Multiple R-squared:  0.5036,	Adjusted R-squared:  0.4915 
# F-statistic: 41.37 on 9 and 367 DF,  p-value: < 2.2e-16

# V.Mean.Sum2 is significant for emoComms but not for nonEmoComms.
# For emoComms, the effect of V.Mean.Sum2 on Mean.Proj is significantly lower for emotives and 
# evidentials (and vice versa, obviously).

### Arousal ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + A.Mean.Sum2, data = new.scale) %>%
  summary()
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.25063    0.07407   3.384 0.000791 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.09167    0.06187  -1.482 0.139277    
# fct_relevel(predicateType2, "emoComm")emotive     0.39432    0.05505   7.163 4.27e-12 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.08226    0.05889  -1.397 0.163276    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15529    0.05379  -2.887 0.004121 ** 
# A.Mean.Sum2                                       0.20959    0.11961   1.752 0.080550 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2449 on 371 degrees of freedom
# Multiple R-squared:  0.4882,	Adjusted R-squared:  0.4813 
# F-statistic: 70.78 on 5 and 371 DF,  p-value: < 2.2e-16

# Arousal adds nothing to the model (slightly less nothing than across all embedding environments).

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * A.Mean.Sum2, data = new.scale) %>%
  summary()
# A.Mean.Sum2 is not significant for emoComm and nonEmoComm.
# No significant interactions for emoComm and nonEmoComm.

### Dominance ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + D.Mean.Sum2, data = new.scale) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35222    0.05211   6.759 5.39e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.10981    0.06139  -1.789  0.07448 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.40188    0.05537   7.258 2.32e-12 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.10039    0.05853  -1.715  0.08716 .  
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.17074    0.05327  -3.206  0.00147 ** 
# D.Mean.Sum2                                      -0.02204    0.09002  -0.245  0.80675    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2459 on 371 degrees of freedom
# Multiple R-squared:  0.4841,	Adjusted R-squared:  0.4771 
# F-statistic: 69.61 on 5 and 371 DF,  p-value: < 2.2e-16

# Dominance adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * D.Mean.Sum2, data = new.scale) %>%
  summary()
# Coefficients:
#                                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                      0.226654   0.039648   5.717 2.25e-08 ***
# fct_relevel(predicateType2, "nonEmoComm")cognitive              -0.006756   0.079695  -0.085   0.9325    
# fct_relevel(predicateType2, "nonEmoComm")emoComm                 0.021081   0.089981   0.234   0.8149    
# fct_relevel(predicateType2, "nonEmoComm")emotive                 0.537062   0.061659   8.710  < 2e-16 ***
# fct_relevel(predicateType2, "nonEmoComm")evidential             -0.013896   0.078594  -0.177   0.8598    
# D.Mean.Sum2                                                     -0.241315   0.165032  -1.462   0.1445    
# fct_relevel(predicateType2, "nonEmoComm")cognitive:D.Mean.Sum2   0.306430   0.281496   1.089   0.2771    
# fct_relevel(predicateType2, "nonEmoComm")emoComm:D.Mean.Sum2     0.765163   0.373506   2.049   0.0412 *  
# fct_relevel(predicateType2, "nonEmoComm")emotive:D.Mean.Sum2     0.180140   0.232091   0.776   0.4382    
# fct_relevel(predicateType2, "nonEmoComm")evidential:D.Mean.Sum2  0.358951   0.272142   1.319   0.1880    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2455 on 367 degrees of freedom
# Multiple R-squared:  0.491,	Adjusted R-squared:  0.4786 
# F-statistic: 39.34 on 9 and 367 DF,  p-value: < 2.2e-16

# For emoComm and nonEmoComm, D.Mean.Sum2 is not significant. The effect of D.Mean.Sum2 on Mean.Proj 
# is significantly lower for the nonEmoComms than for the emoComms.

### Collinearity ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35410    0.03372  10.500  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.03421    0.04190  -0.816  0.41475    
# fct_relevel(predicateType2, "emoComm")emotive     0.08922    0.03782   2.359  0.01883 *  
# fct_relevel(predicateType2, "emoComm")evidential -0.13156    0.03977  -3.308  0.00103 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15369    0.03652  -4.209 3.22e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1686 on 372 degrees of freedom
# Multiple R-squared:  0.2683,	Adjusted R-squared:  0.2604 
# F-statistic:  34.1 on 4 and 372 DF,  p-value: < 2.2e-16

# The fact that the model with valence as the dependent variable has the highest R-squared and 
# F-statistic indicates that the (possible) correlation between VAD and predicateType2 is highest 
# for valence.

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.46455    0.02123  21.881  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.09357    0.02638  -3.548 0.000439 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.03033    0.02381   1.274 0.203464    
# fct_relevel(predicateType2, "emoComm")evidential -0.09582    0.02504  -3.827 0.000152 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.07528    0.02299  -3.275 0.001157 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1062 on 372 degrees of freedom
# Multiple R-squared:  0.193,	Adjusted R-squared:  0.1843 
# F-statistic: 22.24 on 4 and 372 DF,  p-value: < 2.2e-16

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.19140    0.02833   6.757 5.45e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.06686    0.03519   1.900  0.05821 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.05435    0.03177   1.711  0.08795 .  
# fct_relevel(predicateType2, "emoComm")evidential  0.08829    0.03340   2.643  0.00856 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.01463    0.03067   0.477  0.63353    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1416 on 372 degrees of freedom
# Multiple R-squared:  0.0427,	Adjusted R-squared:  0.0324 
# F-statistic: 4.148 on 4 and 372 DF,  p-value: 0.00267

## SAY ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04684   7.195 1.24e-11 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15128    0.05034  -3.005    0.003 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2434 on 199 degrees of freedom
# Multiple R-squared:  0.0434,	Adjusted R-squared:  0.0386 
# F-statistic: 9.029 on 1 and 199 DF,  p-value: 0.002998

### say verb ----
lm(Mean.Proj ~ sayVerb , data = mean.proj.comm) %>%
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.24750    0.02578   9.599   <2e-16 ***
# sayVerbyes  -0.07568    0.03485  -2.171   0.0311 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.246 on 199 degrees of freedom
# Multiple R-squared:  0.02314,	Adjusted R-squared:  0.01824 
# F-statistic: 4.715 on 1 and 199 DF,  p-value: 0.03108

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerb , data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.42807    0.05463   7.836 2.78e-13 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.18888    0.05081  -3.717 0.000262 ***
# sayVerbyes                                       -0.10687    0.03481  -3.070 0.002439 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2384 on 198 degrees of freedom
# Multiple R-squared:  0.08688,	Adjusted R-squared:  0.07765 
# F-statistic: 9.419 on 2 and 198 DF,  p-value: 0.0001237

# sayVerb is significant! (Unlike across embedding environments.) Even more so within the emoComms.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerb , data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                  0.425000   0.119500   3.556 0.000471 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm            -0.185664   0.122216  -1.519 0.130328    
# sayVerbyes                                                  -0.103261   0.129475  -0.798 0.426099    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbyes -0.003891   0.134450  -0.029 0.976941    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.239 on 197 degrees of freedom
# Multiple R-squared:  0.08688,	Adjusted R-squared:  0.07298 
# F-statistic: 6.248 on 3 and 197 DF,  p-value: 0.0004504

### say verb type ----
lm(Mean.Proj ~ sayVerbType, data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)   
# (Intercept)           0.10909    0.03264   3.343  0.00114 **
# sayVerbTypemode verb  0.12609    0.04637   2.719  0.00763 **
# sayVerbTypesay        0.09091    0.24423   0.372  0.71046   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.242 on 107 degrees of freedom
# (91 observations deleted due to missingness)
# Multiple R-squared:  0.06476,	Adjusted R-squared:  0.04727 
# F-statistic: 3.704 on 2 and 107 DF,  p-value: 0.02783

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(sayVerb == "yes")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.32174    0.04935   6.520 2.32e-09 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.18956    0.05549  -3.416 0.000896 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2367 on 108 degrees of freedom
# Multiple R-squared:  0.09752,	Adjusted R-squared:  0.08916 
# F-statistic: 11.67 on 1 and 108 DF,  p-value: 0.0008962

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerbType, data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.25986    0.07272   3.573 0.000532 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15077    0.06530  -2.309 0.022894 *  
# sayVerbTypemode verb                              0.06188    0.05329   1.161 0.248217    
# sayVerbTypesay                                    0.09091    0.23944   0.380 0.704943    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2373 on 106 degrees of freedom
# (91 observations deleted due to missingness)
# Multiple R-squared:  0.1095,	Adjusted R-squared:  0.08433 
# F-statistic: 4.346 on 3 and 106 DF,  p-value: 0.006262

# say verb type adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients: (2 not defined because of singularities)
#                                                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                            0.25986    0.07272   3.573 0.000532 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                      -0.15077    0.06530  -2.309 0.022894 *  
# sayVerbTypemode verb                                                   0.06188    0.05329   1.161 0.248217    
# sayVerbTypesay                                                         0.09091    0.23944   0.380 0.704943    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypemode verb       NA         NA      NA       NA    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypesay             NA         NA      NA       NA    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2373 on 106 degrees of freedom
# (91 observations deleted due to missingness)
# Multiple R-squared:  0.1095,	Adjusted R-squared:  0.08433 
# F-statistic: 4.346 on 3 and 106 DF,  p-value: 0.006262

### mode verb type ----
lm(Mean.Proj ~ modeVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.20976    0.03593   5.837 3.48e-07 ***
# modeVerbTypesay-with-attitude  0.10563    0.07324   1.442    0.155    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2301 on 52 degrees of freedom
# (147 observations deleted due to missingness)
# Multiple R-squared:  0.03846,	Adjusted R-squared:  0.01997 
# F-statistic:  2.08 on 1 and 52 DF,  p-value: 0.1552 

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(sayVerbType == "mode verb")) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.32174    0.04629   6.950 5.94e-09 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15077    0.06110  -2.468   0.0169 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.222 on 52 degrees of freedom
# Multiple R-squared:  0.1048,	Adjusted R-squared:  0.08762 
# F-statistic:  6.09 on 1 and 52 DF,  p-value: 0.01693

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + modeVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.29597    0.05091   5.813 4.02e-07 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14139    0.06136  -2.304   0.0253 *  
# modeVerbTypesay-with-attitude                     0.08467    0.07096   1.193   0.2383    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2211 on 51 degrees of freedom
# (147 observations deleted due to missingness)
# Multiple R-squared:  0.1291,	Adjusted R-squared:  0.09499 
# F-statistic: 3.781 on 2 and 51 DF,  p-value: 0.02942

# mode verb type adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * modeVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.30000    0.05581   5.376 2.01e-06 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.14800    0.07147  -2.071   0.0436 *  
# modeVerbTypesay-with-attitude                                                   0.07143    0.10116   0.706   0.4834    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:modeVerbTypesay-with-attitude  0.02657    0.14329   0.185   0.8536    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2232 on 50 degrees of freedom
# (147 observations deleted due to missingness)
# Multiple R-squared:  0.1297,	Adjusted R-squared:  0.07752 
# F-statistic: 2.485 on 3 and 50 DF,  p-value: 0.07138

### say-by-means verb type ----
lm(Mean.Proj ~ sayByMeansVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)   
# (Intercept)               0.18571    0.06519   2.849  0.00705 **
# sayByMeansVerbTypemanner  0.04156    0.08339   0.498  0.62110   
# sayByMeansVerbTypesound   0.01429    0.12708   0.112  0.91108   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2439 on 38 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.00673,	Adjusted R-squared:  -0.04555 
# F-statistic: 0.1287 on 2 and 38 DF,  p-value: 0.8796

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(modeVerbType == "say-by-means")) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.30000    0.05749   5.218 6.28e-06 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14800    0.07362  -2.010   0.0514 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.23 on 39 degrees of freedom
# Multiple R-squared:  0.09389,	Adjusted R-squared:  0.07065 
# F-statistic: 4.041 on 1 and 39 DF,  p-value: 0.05136

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayByMeansVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                       0.38920    0.11187   3.479  0.00131 **
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.20349    0.09301  -2.188  0.03507 * 
# sayByMeansVerbTypemanner                         -0.06944    0.09433  -0.736  0.46630   
# sayByMeansVerbTypesound                          -0.14850    0.14220  -1.044  0.30311   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2326 on 37 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.1205,	Adjusted R-squared:  0.04921 
# F-statistic:  1.69 on 3 and 37 DF,  p-value: 0.1859

# sayByMeansVerbType adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayByMeansVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients: (1 not defined because of singularities)
#                                                                             Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                                1.857e-01  2.685e-01   0.692    0.494
# fct_relevel(predicateType2, "emoComm")nonEmoComm                          -3.732e-16  2.611e-01   0.000    1.000
# sayByMeansVerbTypemanner                                                   1.476e-01  2.768e-01   0.533    0.597
# sayByMeansVerbTypesound                                                    1.429e-02  2.418e-01   0.059    0.953
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayByMeansVerbTypemanner -2.333e-01  2.796e-01  -0.834    0.410
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayByMeansVerbTypesound          NA         NA      NA       NA
# 
# Residual standard error: 0.2336 on 36 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.1372,	Adjusted R-squared:  0.04134 
# F-statistic: 1.431 on 4 and 36 DF,  p-value: 0.2436