# >>> NEGATION ONLY - EXPERIMENT 1 PREDICATES ONLY
# Modified copy of analysis3.R, including only projection ratings based on items with negation as 
# embedding environment for only those predicates that are included in experiment 1.
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
library(lme4)
library(lmerTest)
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

# load new data
e1 <- read.csv("../../Experiment1/data/e1.csv")
nrow(e1) # 3454

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

# only include predicates also included in experiment 1
d.proj <- d.proj %>% 
  filter(verb_renamed %in% e1$predicate)
d.proj %>% distinct(verb_renamed) %>% nrow() # 192

# create predicateType, emotive component, change-of-state, environment columns
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
# 1 negative False        1913

# calculate by-predicate projection means 
mean.proj <- d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 192

# add predicateType etc. to the means
tmp <- d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, dynamicity, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, dynamicity, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)
nrow(tmp) # 192

mean.proj <- left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 192

mean.proj %>%
  group_by(predicateType2) %>%
  distinct(verb_renamed) %>%
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           27
# 3 nonEmoComm       163

# by-predicate projection means for communicatives
mean.proj.comm <- mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj.comm) # 190

d.proj.comm <- d.proj %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 1893

## valence and arousal data (Warriner et al. 2013) ----
# load data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  rename(verb = Word) %>%
  inner_join(d.proj, by = "verb")
nrow(w2) # 1704
n_distinct(w2$verb) # 171
n_distinct(w2$verb_renamed) # 171

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
nrow(mean.proj.vad) # 171

# Warriner et al.' (2013) dataset contains valence, arousal and dominance ratings for 171 of the 
# 192 predicates included in expreriment 1.

mean.proj.vad %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()  
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           25
# 3 nonEmoComm       144

# exclusion of lemmata that could be interpreted differently (see analysis3.R)
mean.proj.vad <- mean.proj.vad %>% 
  filter(!verb_renamed %in% c("address", "figure", "intercept", "log", "notice", "post", "share", 
                              "spot", "state", "stress", "type"))
nrow(mean.proj.vad) # 164

# For 164 of the 192 predicates included in experiment 1, Warriner et al.'s (2013) dataset contains 
# valence, arousal and dominance ratings that could not have been based on an interpretation of the 
# lemma as a noun whose meaning is clearly different from that of the corresponding verb.

mean.proj.vad %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           25
# 3 nonEmoComm       137

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
  nrow() # 164

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
ggsave("../graphs/projection-by-acceptability-line-NO-my.pdf", height = 6, width = 8)

#### linear model ----
lm(Mean.Proj ~ Mean.Acc, data = mean.proj) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -0.11406    0.11543  -0.988  0.32437   
# Mean.Acc     0.06694    0.02341   2.859  0.00472 **


#### ordinal model ----
clmm(as.factor(veridicality) ~ acceptability + (1 | participant), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.30955    0.04075   7.597 3.04e-14 ***


### A.2 distributions ----
# how many items with which acceptability rating?
d.proj %>% 
  group_by(acceptability) %>% 
  summarise(count = n())
#   acceptability count
#           <int> <int>
# 1             1    45
# 2             2   159
# 3             3   269
# 4             4   254
# 5             5   399
# 6             6   372
# 7             7   415

# distribution of ratings within each acceptability subgroup
o <- d.proj %>% 
  group_by(acceptability, veridicality) %>% 
  count() %>% 
  group_by(acceptability) %>%
  mutate(percentage =  n * 100 / sum(n)) %>% 
  print(n = Inf)
#   acceptability veridicality     n percentage
#           <int> <chr>        <int>      <dbl>
# 1             1 maybe           32      71.1 
# 2             1 no               9      20   
# 3             1 yes              4       8.89
# 4             2 maybe          106      66.7 
# 5             2 no              17      10.7 
# 6             2 yes             36      22.6 
# 7             3 maybe          186      69.1 
# 8             3 no              31      11.5 
# 9             3 yes             52      19.3 
# 10             4 maybe          184      72.4 
# 11             4 no              18       7.09
# 12             4 yes             52      20.5 
# 13             5 maybe          234      58.6 
# 14             5 no              44      11.0 
# 15             5 yes            121      30.3 
# 16             6 maybe          226      60.8 
# 17             6 no              11       2.96
# 18             6 yes            135      36.3 
# 19             7 maybe          226      54.5 
# 20             7 no              27       6.51
# 21             7 yes            162      39.0 

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
ggsave("../graphs/percentage-projection-by-acceptability-line-NO-my.pdf", height = 6, width = 8)

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
ggsave("../graphs/percentage-projection-by-acceptability-bar-NO-my.pdf", height = 6, width = 8)

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
ggsave("../graphs/count-projection-by-acceptability-bar-NO-my.pdf", height = 6, width = 8)

## A.3 low acceptability predicates ----
# predicates with mean acceptability ratings of less than or equal to 4
mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  arrange(Mean.Acc) %>% 
  reframe(verb_renamed, Mean.Acc, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
#   verb_renamed Mean.Acc Mean.Proj predicateType2
#   <fct>           <dbl>     <dbl> <chr>         
# 1 gab              3.1        0.4 nonEmoComm    
# 2 quip             3.1        0   nonEmoComm    
# 3 sing             3.11       0   nonEmoComm    
# 4 scribble         3.2        0   nonEmoComm    
# 5 underscore       3.4        0.1 nonEmoComm    
# 6 grunt            3.5        0.1 emoComm       
# 7 maintain         3.5        0   nonEmoComm    
# 8 underline        3.5        0.3 nonEmoComm    
# 9 spout            3.6        0.3 nonEmoComm    
# 10 whimper          3.6        0.3 emoComm       
# 11 yell             3.6        0   nonEmoComm    
# 12 add              3.7        0.2 nonEmoComm    
# 13 chant            3.7        0   nonEmoComm    
# 14 jest             3.7        0.4 nonEmoComm    
# 15 lie              3.7        0.5 nonEmoComm    
# 16 lecture          3.8        0.3 nonEmoComm    
# 17 mutter           3.8        0   emoComm       
# 18 shriek           3.8        0.2 emoComm       
# 19 howl             3.9        0.3 emoComm       
# 20 mumble           3.9        0   nonEmoComm    
# 21 snitch           3.9        0.3 nonEmoComm    
# 22 summarize        3.9        0.3 nonEmoComm    
# 23 chatter          4          0.3 nonEmoComm    
# 24 depict           4          0.1 nonEmoComm    
# 25 rant             4          0.4 emoComm       
# 26 reply            4         -0.2 nonEmoComm    
# 27 sketch           4          0.3 nonEmoComm    
# 28 submit           4          0   nonEmoComm  

# Of the 190 communicatives in the experiment, 28 should have been excluded had the embedding environment
# been considered part of the exclusion criterion of Mean.Acc <= 4.

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
#   predicateType2 total.count low.acc.count low.acc.percentage
#   <chr>                <int>         <int>              <dbl>
# 1 cognitive                2            NA               NA  
# 2 emoComm                 27             6               22.2
# 3 nonEmoComm             163            22               13.5

### neg-only vs all embedding environments ----
not.acc.neg.only <- mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  select(verb_renamed) %>% 
  unlist() %>% 
  as.vector()
not.acc.neg.only %>% length() # 28

# There are 28 predicates in the MV dataset that have acceptability ratings > 4 across all embedding 
# environments but acceptability ratings <= 4 when only considering negation as embedding environment.

mean.proj1 <- mean.proj %>% 
  mutate(acceptability2 = case_when(Mean.Acc > 4 ~ "acceptable",
                                    Mean.Acc <= 4 ~ "not acceptable neg-only"))
mean.proj1 %>% 
  group_by(acceptability2) %>% 
  count()
# acceptability2     n
# <chr>          <int>
# 1 acceptable                164
# 2 not acceptable neg-only    28

### plots ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = "acceptable"), 
             alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed %in% not.acc.neg.only), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "not acceptable neg-only"), alpha = 0.8) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating under negation",
       colour = "Acceptability") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(labels = c("acceptable" = "acceptable overall\nand under negation only", 
                                 "not acceptable neg-only" = "acceptable overall,\nnot acceptable under negation only"),
                      values = c("yellowgreen", "deeppink3"))
ggsave("../graphs/projection-by-predicate-acceptability-NO.pdf", height = 4, width = 13)

mean.proj.excl <- d.proj %>% 
  mutate(acceptability2 = case_when(verb_renamed %in% not.acc.neg.only ~ "not acceptable neg-only",
                                    TRUE ~ "acceptable")) %>%
  group_by(acceptability2) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         acceptability2 = fct_reorder(as.factor(acceptability2), Mean.Proj)) %>% 
  print()

#   acceptability2          Mean.Proj  CILow CIHigh Mean.Acc YMin.Proj YMax.Proj
#   <fct>                       <dbl>  <dbl>  <dbl>    <dbl>     <dbl>     <dbl>
# 1 acceptable                  0.218 0.0288 0.0276     5.07     0.189     0.245
# 2 not acceptable neg-only     0.176 0.0645 0.0573     3.68     0.111     0.233

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
ggsave("../graphs/projection-by-acceptability-NO-my.pdf", height = 4, width = 12)

### linear models ----
lm(Mean.Proj ~ fct_relevel(acceptability2, "not acceptable neg-only"), data = mean.proj1) %>% 
  summary()
# Coefficients:
#                                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                       0.21836    0.01968  11.095   <2e-16 ***
# fct_relevel(acceptability2, "acceptable")not acceptable neg-only -0.04336    0.05154  -0.841    0.401 

# Coefficients:
#                                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                       0.17500    0.04763   3.674  0.00031 ***
# fct_relevel(acceptability2, "not acceptable neg-only")acceptable  0.04336    0.05154   0.841  0.40122 

### ordinal model ----
clmm(as.factor(veridicality) ~ fct_relevel(acceptability2, "not acceptable neg-only") + (1 | participant),
     data = d.proj %>% 
       mutate(acceptability2 = 
                case_when(verb_renamed %in% not.acc.neg.only ~ "not acceptable neg-only",
                                                   TRUE ~ "acceptable"))) %>% 
  summary()
# Coefficients:
#                                                                  Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(acceptability2, "acceptable")not acceptable neg-only  -0.3852     0.1603  -2.403   0.0163 *
#   
# Coefficients:
#                                                                  Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(acceptability2, "not acceptable neg-only")acceptable   0.3852     0.1603   2.403   0.0163 *


# B All predicateTypes ----
## B.1 by predicate type ----
### plot ----
# calculate by-predicateType2 means
mean.proj.bt2 <- d.proj.comm %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
nrow(mean.proj.bt2) # 3

mean.proj.bt2 %>% 
ggplot(aes(x = predicateType2, 
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
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2-NO-my.pdf", height = 4, width = 5)

### linear model ----
lm(Mean.Proj ~ predicateType2, mean.proj.comm) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.33704    0.04714   7.149 1.88e-11 ***
# predicateType2nonEmoComm -0.14678    0.05090  -2.884  0.00438 ** 

### distribution ----
mean.proj %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           27
# 3 nonEmoComm       163

## B.2 by predicate ----
### with emoComms ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, 
             aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "emoComm", "know"),
                      labels = c("other", "communicative with emotion entailment", "know"),
                      values = c("grey50", "green3", "red"))
ggsave("../graphs/projection-by-predicate-emoComm-NO-my.pdf", height = 4, width = 13)


# C Communicatives ----
## C.1 overall ----
### C.1.1 by-predicateType ----
#### plot ----
# calculate by-predicateType means
mean.proj.comm.bt <-  d.proj.comm %>%
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj.comm.bt) # 3

ggplot(mean.proj.comm.bt, aes(x = commType, y = Mean.Proj, colour = commType)) +
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
ggsave("../graphs/projection-by-predicateType-commType-NO-my.pdf", height = 4, width = 10)

### C.1.2 by-predicate ----
# How many of which type of communicative predicate?
mean.proj.comm %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  count()
#   commType                    n
#   <chr>                   <int>
# 1 discourse participation    95
# 2 pure                       76
# 3 state changing             19

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
ggsave("../graphs/projection-by-communicative-predicate-NO-my.pdf", height = 4, width = 10)


## C.3 say verbs (Grimshaw 2015) ----
# distribution of communication predicates
d.proj %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(predicateType, sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType sayVerb     n
#   <chr>         <chr>   <int>
# 1 cognitive     yes         1
# 2 communicative no         84
# 3 communicative yes       106

d.proj %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(predicateType2, sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2 sayVerb     n
#   <chr>          <chr>   <int>
# 1 cognitive      yes         1
# 2 emoComm        no          4
# 3 emoComm        yes        23
# 4 nonEmoComm     no         80
# 5 nonEmoComm     yes        83


### C.3.1 types of communicatives ----
#### distributions ----
d.proj.comm %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb     n
#   <chr>   <int>
# 1 no         84
# 2 yes       106

#### plots ----
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
ggsave("../graphs/projection-by-communication-type-NO-my.pdf", height = 4, width = 4)

# by predicate type by say/non-say
d.proj.comm %>%
  group_by(predicateType2, sayVerb) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = sayVerb)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Type of communicative") + 
  scale_colour_manual(values = c("firebrick2", "dodgerblue3"),
                      labels = c("no" = "non-say verb", "yes" = "say verb")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names)
ggsave("../graphs/projection-by-communication-type-predType2-NO-my.pdf", height = 4, width = 5)

# by say/non-say by predicate type
d.proj.comm %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, sayVerb) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = sayVerb, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Type of communicative",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_x_discrete(labels = c("non-say verb", "say verb")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-communication-type-predType2-alt-NO-my.pdf", height = 4, width = 5.5)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(sayVerb, "yes"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.18208    0.02410   7.554 1.79e-12 ***
# fct_relevel(sayVerb, "yes")no  0.06568    0.03625   1.812   0.0716 .  

lm(Mean.Proj ~ fct_relevel(sayVerb, "yes") * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.321739   0.050413   6.382 1.35e-09 ***
# fct_relevel(sayVerb, "yes")no                                                   0.103261   0.130977   0.788  0.43147    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.178366   0.056971  -3.131  0.00202 ** 
# fct_relevel(sayVerb, "yes")no:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.007745   0.136345  -0.057  0.95476    

#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                    0.143373   0.026538   5.403 1.99e-07 ***
# fct_relevel(sayVerb, "yes")no                                                  0.095515   0.037881   2.521  0.01253 *  
# fct_relevel(predicateType2, "nonEmoComm")emoComm                               0.178366   0.056971   3.131  0.00202 ** 
# fct_relevel(sayVerb, "yes")no:fct_relevel(predicateType2, "nonEmoComm")emoComm 0.007745   0.136345   0.057  0.95476   

# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2418 on 186 degrees of freedom
# Multiple R-squared:  0.077,	Adjusted R-squared:  0.06211 
# F-statistic: 5.172 on 3 and 186 DF,  p-value: 0.001866

### C.3.2 types of say verbs ----
#### distributions ----
d.proj.comm %>% 
  filter(sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerbType             n
#   <chr>               <int>
# 1 discourse role verb    51
# 2 mode verb              54
# 3 say                     1

d.proj.comm %>% 
  filter(sayVerb == "yes") %>%  
  group_by(predicateType2, sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2 sayVerbType             n
#   <chr>          <chr>               <int>
# 1 emoComm        mode verb              23
# 2 nonEmoComm     discourse role verb    51
# 3 nonEmoComm     mode verb              31
# 4 nonEmoComm     say                     1

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
  scale_y_continuous(limits = c(-0.2, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-sayverb-type-NO-my.pdf", height = 4, width = 6)

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
  scale_y_continuous(limits = c(-0.2, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("purple","deeppink", "orange3", "grey50"))
ggsave("../graphs/projection-by-sayverb-type2-NO-my.pdf", height = 4, width = 7)

# by predicate type by say verb type
d.proj.comm %>%
  filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>%  
  group_by(predicateType2, sayVerbType) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = sayVerbType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Say verb type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = predicateType2_names) + 
  scale_colour_manual(values = c("purple","deeppink"))
ggsave("../graphs/projection-by-sayverb-type-predType2-NO-my.pdf", height = 4, width = 5)

# by say verb type by predicate type
d.proj.comm %>%
  filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>%  
  group_by(predicateType2, sayVerbType) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = sayVerbType, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Say verb type",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-sayverb-type-predType2-alt-NO-my.pdf", height = 4, width = 5.5)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(sayVerbType, "mode verb"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.12549    0.03374   3.719 0.000325 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.10969    0.04705   2.332 0.021672 *  
# fct_relevel(sayVerbType, "discourse role verb")say        0.07451    0.24330   0.306 0.760039  

#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.23519    0.03279   7.173 1.16e-10 ***
# fct_relevel(sayVerbType, "mode verb")discourse role verb -0.10969    0.04705  -2.332   0.0217 *  
# fct_relevel(sayVerbType, "mode verb")say                 -0.03519    0.24317  -0.145   0.8852  

# Residual standard error: 0.241 on 103 degrees of freedom
# (84 observations deleted due to missingness)
# Multiple R-squared:  0.05018,	Adjusted R-squared:  0.03174 
# F-statistic: 2.721 on 2 and 103 DF,  p-value: 0.07055

lm(Mean.Proj ~ fct_relevel(sayVerbType, "discourse role verb") * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.comm) %>%  
  summary()
# emoComm
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                                                0.27626    0.07286   3.791 0.000254 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb                                                   0.04548    0.05374   0.846 0.399407    
# fct_relevel(sayVerbType, "discourse role verb")say                                                         0.07451    0.23828   0.313 0.755147    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                          -0.15077    0.06494  -2.322 0.022240 *  
# fct_relevel(sayVerbType, "discourse role verb")mode verb:fct_relevel(predicateType2, "emoComm")nonEmoComm       NA         NA      NA       NA    
# fct_relevel(sayVerbType, "discourse role verb")say:fct_relevel(predicateType2, "emoComm")nonEmoComm             NA         NA      NA       NA  

# nonEmoComm
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                                                0.12549    0.03304   3.798 0.000248 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb                                                   0.04548    0.05374   0.846 0.399407    
# fct_relevel(sayVerbType, "discourse role verb")say                                                         0.07451    0.23828   0.313 0.755147    
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                                           0.15077    0.06494   2.322 0.022240 *  
# fct_relevel(sayVerbType, "discourse role verb")mode verb:fct_relevel(predicateType2, "nonEmoComm")emoComm       NA         NA      NA       NA    
# fct_relevel(sayVerbType, "discourse role verb")say:fct_relevel(predicateType2, "nonEmoComm")emoComm             NA         NA      NA       NA   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.236 on 102 degrees of freedom
# (84 observations deleted due to missingness)
# Multiple R-squared:  0.09785,	Adjusted R-squared:  0.07132 
# F-statistic: 3.688 on 3 and 102 DF,  p-value: 0.01439

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "mode verb") +  (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)   
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.46675    0.15095   3.092  0.00199 **
# fct_relevel(sayVerbType, "discourse role verb")say       -0.09926    0.76640  -0.130  0.89695     

#                                                          Estimate Std. Error z value Pr(>|z|)   
# fct_relevel(sayVerbType, "mode verb")discourse role verb  -0.4667     0.1510  -3.092  0.00199 **
# fct_relevel(sayVerbType, "mode verb")say                  -0.5660     0.7650  -0.740  0.45939   

### C.3.3 types of mode verbs ----
#### distributions ----
d.proj.comm %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means         41
# 2 say-with-attitude    13

d.proj.comm %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(predicateType2, modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2 modeVerbType          n
#   <chr>          <chr>             <int>
# 1 emoComm        say-by-means         16
# 2 emoComm        say-with-attitude     7
# 3 nonEmoComm     say-by-means         25
# 4 nonEmoComm     say-with-attitude     6

#### plot ----
mean.proj.modetype <- d.proj.comm %>%
  filter(sayVerb == "yes") %>%  
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
  scale_y_continuous(limits = c(-0.2, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-NO-my.pdf", height = 4, width = 6)

# by predicate type by mode verb type
d.proj.comm %>%
  filter(predicateType == "communicative" & sayVerbType == "mode verb") %>%  
  group_by(modeVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = modeVerbType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Mode verb type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names) + 
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-predType-NO-my.pdf", height = 4, width = 5)

# by mode verb type by predicate type
d.proj.comm %>%
  filter(predicateType == "communicative" & sayVerbType == "mode verb") %>%  
  group_by(modeVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = modeVerbType, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Mode verb type",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-modeverb-type-predType-alt-NO-my.pdf", height = 4, width = 5.5)


#### linear models ----
lm(Mean.Proj ~ fct_relevel(modeVerbType, "say-by-means"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                 0.20976    0.03593   5.837 3.48e-07 ***
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude  0.10563    0.07324   1.442    0.155   

# Coefficients:
#                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                 0.31538    0.06382   4.942 8.43e-06 ***
# fct_relevel(modeVerbType, "say-with-attitude")say-by-means -0.10563    0.07324  -1.442    0.155 

lm(Mean.Proj ~ fct_relevel(modeVerbType, "say-by-means") * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.comm) %>% 
  summary()
# always n.s. 


#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-with-attitude") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                            Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude   0.5531     0.2529   2.187   0.0287 *

# fct_relevel(modeVerbType, "say-with-attitude")say-by-means  -0.5531     0.2529  -2.187   0.0287 *

### C.3.4 types of say-by-means verbs ----
#### distributions ----
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

d.proj.comm %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(predicateType2, sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2 sayByMeansVerbType     n
#   <chr>          <chr>              <int>
# 1 emoComm        manner                12
# 2 emoComm        sound                  4
# 3 nonEmoComm     form                  14
# 4 nonEmoComm     manner                10
# 5 nonEmoComm     sound                  1

#### plot ----
mean.proj.bymeanstype <- d.proj.comm %>%
  filter(sayVerbType == "mode verb" | verb_renamed == "say") %>%  
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
  scale_y_continuous(limits = c(-0.2, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2", "grey50"))
ggsave("../graphs/projection-by-saybymeansverb-type-NO-my.pdf", height = 4, width = 6)

# by predicate type by say-by-means verb type
d.proj.comm %>%
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = sayByMeansVerbType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -0.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Say-by-means verb type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2"))
ggsave("../graphs/projection-by-saybymeansverb-type-predType2-NO-my.pdf", height = 4, width = 5)

# by say-by-means verb type by predicate type 
d.proj.comm %>%
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = sayByMeansVerbType, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -0.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Say-by-means verb type",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  #scale_x_discrete(labels = predicateType2_names) + 
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-saybymeansverb-type-predType2-alt-NO-my.pdf", height = 4, width = 5.5)

#### linear models ----
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

# Residual standard error: 0.2439 on 38 degrees of freedom
# (149 observations deleted due to missingness)
# Multiple R-squared:  0.00673,	Adjusted R-squared:  -0.04555 
# F-statistic: 0.1287 on 2 and 38 DF,  p-value: 0.8796

lm(Mean.Proj ~ fct_relevel(sayByMeansVerbType, "form") *  fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm) %>% 
  summary()
# n.s.

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
# 6 nonEmoComm     discourse participation discourse role verb NA                   41
# 7 nonEmoComm     discourse participation mode verb           say-with-attitude     1
# 8 nonEmoComm     discourse participation NA                  NA                   47
# 9 nonEmoComm     pure                    discourse role verb NA                    5
# 10 nonEmoComm     pure                    mode verb           say-by-means         25
# 11 nonEmoComm     pure                    mode verb           say-with-attitude     5
# 12 nonEmoComm     pure                    say                 NA                    1
# 13 nonEmoComm     pure                    NA                  NA                   19
# 14 nonEmoComm     state changing          discourse role verb NA                    5
# 15 nonEmoComm     state changing          NA                  NA                   14

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
  scale_y_continuous(limits = c(-0.2, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3", "blue", "grey50")) + 
  scale_shape_manual(breaks = c("form", "manner", "sound", "NA"),
                     values = c(15, 19, 17, 18))  
ggsave("../graphs/projection-by-saybymeansverb-type2-NO-my.pdf", height = 4, width = 7)


## X VAD ratings ----
### X.1 valence ----
#### X.1.1 by predicate ----
##### plots ----
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
# ggsave("../graphs/valence-by-predicate-NO-my.pdf", height = 4, width = 13)

# valence by predicate with emotion entailment distinction for communicatives
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
                      labels = predicateType2_names)
# ggsave("../graphs/valence-by-predicate2-NO-my.pdf", height = 4, width = 13)

#### X.1.2 by predicate type ----
##### X.1.2.1 overall ----
###### plots ----
# original ratings
# calculate valence by predicateType means
mean.valence <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum), 
            CILow = ci.low(V.Mean.Sum), 
            CIHigh = ci.high(V.Mean.Sum)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Valence))
mean.valence

ggplot(mean.valence, aes(x = predicateType, y = Mean.Valence, colour = predicateType)) +
  geom_hline(yintercept = 5, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_colour_manual(values = cols)
ggsave("../graphs/valence-by-predicateType-orig-NO-my.pdf", height = 4, width = 4)

# calculate valence by predicateType2 means (communicatives with/without emotion entailment)
mean.valence2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum), 
            CILow = ci.low(V.Mean.Sum), 
            CIHigh = ci.high(V.Mean.Sum)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  geom_hline(yintercept = 5, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2-orig-NO-my.pdf", height = 4, width = 6)

# split valence scale
# calculate valence by predicateType2 means (communicatives with/without emotion entailment)
mean.valence2.sp <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), 
            CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2.sp, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  #geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2-NO-my.pdf", height = 4, width = 6)

###### linear models ----
lm(V.Mean.Sum ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        3.8404     0.2015  19.056  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive    2.9096     0.7405   3.929 0.000126 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm   1.3500     0.2192   6.160 5.59e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.008 on 161 degrees of freedom
# Multiple R-squared:  0.2147,	Adjusted R-squared:  0.205 
# F-statistic: 22.01 on 2 and 161 DF,  p-value: 3.534e-09

lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35410    0.03098  11.431  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.08340    0.11382   0.733    0.465    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15134    0.03369  -4.493 1.34e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1549 on 161 degrees of freedom
# Multiple R-squared:  0.1291,	Adjusted R-squared:  0.1183 
# F-statistic: 11.93 on 2 and 161 DF,  p-value: 1.471e-05

##### X.1.2.2 with direction of valence ----
###### plot ----
# calculate valence by predicateType2 means and direction of valence
mean.valence3 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), 
            CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, 
         YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 5

# valence by predicate type and direction of valence
ggplot(mean.valence3, aes(x = predicateType2, y = Mean.Valence, 
                          colour = predicateType2, shape = V.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean valence rating",
       shape = "Direction of valence") +
  guides(colour = "none") + 
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType-and-direction-NO-my.pdf", height = 4, width = 6)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2.direction difference within predicate type
# cognitive       n/a
# emoComm         *
# nonEmoComm      n.s.

### X.2 arousal ----
#### X.2.1 by predicate ----
##### plots ----
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
#ggsave("../graphs/arousal-by-predicate-NO-my.pdf", height = 4, width = 13)

# arousal by predicate with emotion entailment
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
                      labels = predicateType2_names)
#ggsave("../graphs/arousal-by-predicate2-NO-my.pdf", height = 4, width = 13)


#### X.2.2 by predicate type ----
##### X.2.2.1 overall ----
###### plots ----
# original ratings
# calculate arousal by predicateType means
mean.arousal <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum), 
            CILow = ci.low(A.Mean.Sum), 
            CIHigh = ci.high(A.Mean.Sum)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
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
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_colour_manual(values = cols)
ggsave("../graphs/arousal-by-predicateType-orig-NO-my.pdf", height = 4, width = 4)

# calculate arousal by predicateType2 means (communicatives with/without emotion entailment)
mean.arousal2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum), 
            CILow = ci.low(A.Mean.Sum), 
            CIHigh = ci.high(A.Mean.Sum)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal2

ggplot(mean.arousal2, aes(x = predicateType2, y = Mean.Arousal, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = predicateType2_names) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2-orig-NO-my.pdf", height = 4, width = 6)

# with adjusted scales
# calculate arousal by predicateType2 means (communicatives with/without emotion entailment)
mean.arousal2.ad <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), 
            CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal2

ggplot(mean.arousal2.ad, aes(x = predicateType2, y = Mean.Arousal, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = predicateType2_names) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2-NO-my.pdf", height = 4, width = 6)

###### linear model ----
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        4.7164     0.1649  28.601  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive   -1.2214     0.6059  -2.016 0.045474 *  (only 2 predicates)
# fct_relevel(predicateType2, "emoComm")nonEmoComm  -0.6044     0.1793  -3.371 0.000939 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8245 on 161 degrees of freedom
# Multiple R-squared:  0.0738,	Adjusted R-squared:  0.06229 
# F-statistic: 6.414 on 2 and 161 DF,  p-value: 0.002089

##### X.2.2.2 with direction of valence ----
###### plot ----
# calculate arousal by predicateType2 means and direction of valence
mean.arousal3 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), 
            CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal3
nrow(mean.arousal3) # 5

# arousal by predicate type and direction of valence
ggplot(mean.arousal3, aes(x = predicateType2, y = Mean.Arousal, 
                          colour = predicateType2, shape = V.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean arousal rating",
       shape = "Direction of valence") +
  guides(colour = "none") + 
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType-and-direction-of-valence-NO-my.pdf", height = 4, width = 6)

###### linear model ----
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Residual standard error: 0.09838 on 159 degrees of freedom
# Multiple R-squared:  0.1665,	Adjusted R-squared:  0.1455 
# F-statistic: 7.941 on 4 and 159 DF,  p-value: 7.327e-06

# predicateType2  significance of V.Mean.Sum2.direction difference within predicate type
# cognitive       n/a
# emoComm         n.s.
# nonEmoComm      ***

### X.3 dominance ----
#### X.3.1 by predicate ----
##### plot ----
# with emotion entailment distinction for communicatives
ggplot(new.scale, aes(x = reorder(verb_renamed, D.Mean.Sum2), y = D.Mean.Sum2, 
                      colour = predicateType2)) +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate", 
       y = "Mean dominance rating", 
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
#ggsave("../graphs/valence-by-predicate2-NO-my.pdf", height = 4, width = 13)

#### X.3.2 by predicate type ----
##### X 3.2.1 overall ----
###### plots ----
# original ratings
# calculate dominance by predicateType2 means (communicatives with/without emotion entailment)
mean.dominance <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum), 
            CILow = ci.low(D.Mean.Sum), 
            CIHigh = ci.high(D.Mean.Sum)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, 
         YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance

ggplot(mean.dominance, aes(x = predicateType2, y = Mean.Dominance, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/dominance-by-predicateType2-orig-NO-my.pdf", height = 4, width = 6)

# split dominance ratings
mean.dominance.sp <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), 
            CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, 
         YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance

ggplot(mean.dominance.sp, aes(x = predicateType2, y = Mean.Dominance, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/dominance-by-predicateType2-NO-my.pdf", height = 4, width = 6)

###### linear models ----
lm(D.Mean.Sum ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        4.8712     0.1653  29.464  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive    1.2688     0.6075   2.089 0.038306 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm   0.6970     0.1798   3.877 0.000154 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8266 on 161 degrees of freedom
# Multiple R-squared:  0.09226,	Adjusted R-squared:  0.08099 
# F-statistic: 8.182 on 2 and 161 DF,  p-value: 0.0004129  

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.19140    0.02535   7.551 3.04e-12 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.09360    0.09313   1.005    0.316    
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.02072    0.02756   0.752    0.453    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1267 on 161 degrees of freedom
# Multiple R-squared:  0.007868,	Adjusted R-squared:  -0.004457 
# F-statistic: 0.6384 on 2 and 161 DF,  p-value: 0.5295

##### X.3.2.2 with direction of valence ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance2 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), 
            CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, 
         YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance2
nrow(mean.dominance2) # 5

ggplot(mean.dominance2, aes(x = predicateType2, y = Mean.Dominance, 
                            colour = predicateType2, shape = V.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       shape =  "Direction of valence") +
  guides(colour = "none") + 
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-valence-NO-my.pdf", height = 4, width = 6)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2.direction difference within predicate type
# cognitive       n/a
# emoComm         n.s.
# nonEmoComm      ***

##### X.3.2.3 with 'direction' of dominance ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance3 <- new.scale %>%
  group_by(predicateType2, D.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), 
            CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, 
         YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance3
nrow(mean.dominance3) # 5

ggplot(mean.dominance3, aes(x = predicateType2, y = Mean.Dominance, 
                            colour = predicateType2, shape = D.Mean.Sum2.direction)) +
  #geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       shape =  "Direction of dominance") +
  guides(colour = "none") + 
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-dominance-NO-my.pdf", height = 4, width = 6)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm") * D.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2.direction difference within predicate type
# cognitive       n/a
# emoComm         n.s.
# nonEmoComm      ***

###### tables: what is "direction of dominance"? ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#    verb_renamed D.Mean.Sum2 predicateType2
# 1        detail      0.5350     nonEmoComm
# 2        signal      0.5125     nonEmoComm
# 3    articulate      0.4725     nonEmoComm
# 4           say      0.4700     nonEmoComm
# 5   communicate      0.4500     nonEmoComm
# 6           add      0.4475     nonEmoComm
# 7         reply      0.4475     nonEmoComm
# 8          sing      0.4175     nonEmoComm
# 9       express      0.3925     nonEmoComm
# 10       praise      0.3925     nonEmoComm

new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           12
# 3 nonEmoComm       107

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#    verb_renamed D.Mean.Sum2 predicateType2
# 1           cry      0.6100        emoComm
# 2           sob      0.4275        emoComm
# 3        reject      0.3850     nonEmoComm
# 4          fuss      0.3750        emoComm
# 5         bitch      0.3375        emoComm
# 6        mumble      0.3300     nonEmoComm
# 7        stress      0.2875     nonEmoComm
# 8        gossip      0.2650     nonEmoComm
# 9      denounce      0.2500     nonEmoComm
# 10       shriek      0.2500        emoComm

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 emoComm           13
# 2 nonEmoComm        37

##### X.3.2.4 with direction of valence and 'direction' of dominance ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance4 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), 
            CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, 
         YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance4
nrow(mean.dominance4) # 9

ggplot(mean.dominance4, aes(x = predicateType2, y = Mean.Dominance, 
                            colour = predicateType2, linetype = V.Mean.Sum2.direction, 
                            shape = D.Mean.Sum2.direction)) +
  #geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.4)) +
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.spacing = unit(0, "cm"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       linetype = "Direction of valence",
       shape =  "Direction of dominance") +
  guides(colour = "none") + 
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-valence-and-dominance-NO-my.pdf", height = 4, width = 6)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * D.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2.direction difference within predicate type
# cognitive       n/a
# emoComm         n.s.
# nonEmoComm      ***

### X.4 valence + arousal + dominance distributions and correlations ----
#### X.4.1 by predicate ----
##### plots ----
###### X.4.1.1 valence + arousal ----
# no patterns (clusters) emerge.
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = V.Mean.Sum2, colour = predicateType2, 
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
                      labels = predicateType2_names) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-by-arousal-NO-my.pdf", height = 8, width = 12)

###### X.4.1.2 valence + dominance ----
# no patterns (clusters) emerge.
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = V.Mean.Sum2, colour = predicateType2, 
                      fill = predicateType2)) +
  geom_point(size = 2) +
  geom_mark_hull(alpha = 0.1) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 24)) +
  labs(x = "Mean dominance rating", 
       y = "Mean valence rating",
       colour = "Predicate type", 
       fill = "Predicate type") +
  coord_equal() + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-NO-my.pdf", height = 8, width = 12)

###### X.4.1.3 arousal + dominance ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = A.Mean.Sum2, colour = predicateType2, 
                      fill = predicateType2)) +
  geom_point(size = 2) +
  geom_mark_hull(alpha = 0.1) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 24)) +
  labs(x = "Mean dominance rating", 
       y = "Mean arousal rating",
       colour = "Predicate type", 
       fill = "Predicate type") +
  coord_equal() + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/arousal-by-dominance-NO-my.pdf", height = 8, width = 12)


#### X.4.2 by predicate type ----
##### plots ----
###### X.4.2.1 valence + arousal ----
# distribution of valence and arousal ratings by predicate type including emotion entailment
new.scale %>% 
  filter(predicateType == "communicative") %>% 
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
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-arousal-by-predicateType2-NO-my.pdf", height = 6, width = 10)

###### X.4.2.2 valence + dominance ----
# distribution of valence and dominance ratings by predicate type including emotion entailment
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  select(verb_renamed, predicateType2, Valence = V.Mean.Sum2, Dominance = D.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Dominance), names_to = "variable", values_to = "value") %>%
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
  labs(y = "Mean valence / dominance rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(limits = c("Valence", "Dominance")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-dominance-by-predicateType2-NO-my.pdf", height = 6, width = 10)

###### X.4.2.3 arousal + dominance ----
# distribution of valence and dominance ratings by predicate type including emotion entailment
new.scale %>% 
  filter(predicateType == "communicative") %>%  
  select(verb_renamed, predicateType2, Arousal = A.Mean.Sum2, Dominance = D.Mean.Sum2) %>%
  pivot_longer(., cols = c(Arousal, Dominance), names_to = "variable", values_to = "value") %>%
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
  labs(y = "Mean arousal / dominance rating", 
       fill = "Predicate type",
       caption = "arousal rating: completely calm (0) to completely excited (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(limits = c("Arousal", "Dominance")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/arousal-dominance-by-predicateType2-NO-my.pdf", height = 6, width = 10)

###### X.4.2.4 valence + arousal + dominance ----
# distribution of valence, arousal and dominance ratings by 5 predicate types
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  select(verb_renamed, predicateType2, Valence = V.Mean.Sum2, Arousal = A.Mean.Sum2, Dominance = D.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Arousal, Dominance), names_to = "variable", values_to = "value") %>% 
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
  labs(y = "Mean valence / arousal / dominance rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal", "Dominance")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-arousal-dominance-by-predicateType2-NO-my.pdf", height = 6, width = 10)

ggplot() +
  geom_point(data = mean.valence2, 
             aes(x = factor(predicateType2, levels = c("cognitive", "emoComm", "emotive", "evidential", "nonEmoComm")), 
                 y = Mean.Valence, colour = "valence"), 
             position = position_nudge(x = - 0.1)) +
  geom_errorbar(data = mean.valence2, 
                aes(x = predicateType2, y = Mean.Valence, ymin = YMin.Valence, 
                    ymax = YMax.Valence,colour = "valence"), 
                width = 0, position = position_nudge(x = -0.1)) +
  geom_point(data = mean.arousal2, 
             aes(x = predicateType2, y = Mean.Arousal,colour = "arousal")) +
  geom_errorbar(data = mean.arousal2, 
                aes(x = predicateType2, y = Mean.Arousal, ymin = YMin.Arousal, 
                    ymax = YMax.Arousal, colour = "arousal"), 
                width = 0) +
  geom_point(data = mean.dominance, 
             aes(x = predicateType2, y = Mean.Dominance, colour = "dominance"),
             position = position_nudge(x = 0.1)) +
  geom_errorbar(data = mean.dominance, 
                aes(x = predicateType2, y = Mean.Dominance, ymin = YMin.Dominance, 
                    ymax = YMax.Dominance, colour = "dominance"), 
                width = 0, position = position_nudge(x = 0.1)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = cols2, face = "bold"),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence / arousal / dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(limits = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))
ggsave("../graphs/valence-arousal-dominance-by-predicateType2-2-NO-my.pdf", height = 6, width = 6)

ggplot() +
  geom_point(data = mean.valence3 %>% filter(V.Mean.Sum2.direction == "negative"), 
             aes(x = factor(predicateType2, levels = c("cognitive", "emoComm", "emotive", "evidential", "nonEmoComm")), 
                 y = Mean.Valence, colour = "negative valence"), 
             position = position_nudge(x = - 0.15)) +
  geom_errorbar(data = mean.valence3 %>% filter(V.Mean.Sum2.direction == "negative"), 
                aes(x = predicateType2, y = Mean.Valence, ymin = YMin.Valence, 
                    ymax = YMax.Valence,colour = "negative valence"), 
                width = 0, position = position_nudge(x = -0.15)) +
  geom_point(data = mean.valence3 %>% filter(V.Mean.Sum2.direction == "positive"), 
             aes(x = predicateType2, y = Mean.Valence, colour = "positive valence"), 
             position = position_nudge(x = - 0.1)) +
  geom_errorbar(data = mean.valence3 %>% filter(V.Mean.Sum2.direction == "positive"), 
                aes(x = predicateType2, y = Mean.Valence, ymin = YMin.Valence, 
                    ymax = YMax.Valence,colour = "positive valence"), 
                width = 0, position = position_nudge(x = -0.1)) +
  geom_point(data = mean.arousal2.ad, 
             aes(x = predicateType2, y = Mean.Arousal,colour = "arousal")) +
  geom_errorbar(data = mean.arousal2.ad, 
                aes(x = predicateType2, y = Mean.Arousal, ymin = YMin.Arousal, 
                    ymax = YMax.Arousal, colour = "arousal"), 
                width = 0) +
  geom_point(data = mean.dominance3 %>% filter(D.Mean.Sum2.direction == "negative"), 
             aes(x = predicateType2, y = Mean.Dominance, colour = "negative dominance"),
             position = position_nudge(x = 0.1)) +
  geom_errorbar(data = mean.dominance3 %>% filter(D.Mean.Sum2.direction == "negative"), 
                aes(x = predicateType2, y = Mean.Dominance, ymin = YMin.Dominance, 
                    ymax = YMax.Dominance, colour = "negative dominance"), 
                width = 0, position = position_nudge(x = 0.1)) +
  geom_point(data = mean.dominance3 %>% filter(D.Mean.Sum2.direction == "positive"), 
             aes(x = predicateType2, y = Mean.Dominance, colour = "positive dominance"),
             position = position_nudge(x = 0.15)) +
  geom_errorbar(data = mean.dominance3 %>% filter(D.Mean.Sum2.direction == "positive"), 
                aes(x = predicateType2, y = Mean.Dominance, ymin = YMin.Dominance, 
                    ymax = YMax.Dominance, colour = "positive dominance"), 
                width = 0, position = position_nudge(x = 0.15)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = cols2, face = "bold"),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence / arousal / dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(limits = c("negative valence", "positive valence", "arousal", "negative dominance", 
                                 "positive dominance"),
                      values = c("blue4", "steelblue3", "red3", "grey30", "grey60"))
ggsave("../graphs/valence-arousal-dominance-by-predicateType2-2-neg-pos-NO-my.pdf", height = 6, width = 10)

#### X.4.3 rating correlations ----
##### X.4.3.1 valence + arousal ----
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-valence-NO-my.pdf", height = 4, width = 6)

# original scale
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum, y = A.Mean.Sum)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-valence-orig-NO-my.pdf", height = 4, width = 6)

# faceted by predicate type
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean arousal rating (calm - excited)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/arousal-by-valence-faceted-NO-my.pdf", height = 3.5, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean arousal rating (calm - excited)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ V.Mean.Sum2.direction, labeller = as_labeller(c("negative" = "negative valence",
                                                               "positive" = "positive valence"))) 
ggsave("../graphs/arousal-by-valence-faceted-Vdir-NO-my.pdf", height = 3, width = 4)

# by direction of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) + 
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction), 
            aes(x = Inf, y = .01, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/arousal-by-valence-faceted-Vdir-Ddir-NO-my.pdf", height = 5, width = 5)

# by predicate type and direction of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) + 
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = .01, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/arousal-by-valence-faceted-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 8.5)

new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean arousal rating (calm - excited)",
       y = "Mean valence rating (neutral - unhappy/happy)", 
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-arousal-NO-my.pdf", height = 4, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)",
       y = "Mean valence rating (unhappy - happy)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/valence-by-arousal-faceted-NO-my.pdf", height = 3.5, width = 5)

###### linear models -----
lm(V.Mean.Sum2 ~ A.Mean.Sum2, new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.02117    0.04765   0.444    0.657    
# A.Mean.Sum2  0.51152    0.11497   4.449 1.61e-05 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1555 on 160 degrees of freedom
# Multiple R-squared:  0.1101,	Adjusted R-squared:  0.1045 
# F-statistic:  19.8 on 1 and 160 DF,  p-value: 1.607e-05

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      **

lm(A.Mean.Sum2 ~ V.Mean.Sum2, 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.35199    0.01351  26.059  < 2e-16 ***
# V.Mean.Sum2  0.21524    0.04838   4.449 1.61e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1009 on 160 degrees of freedom
# Multiple R-squared:  0.1101,	Adjusted R-squared:  0.1045 
# F-statistic:  19.8 on 1 and 160 DF,  p-value: 1.607e-05

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      **

lm(A.Mean.Sum2 ~ 
     V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction significance of V.Mean.Sum2
# emoComm         negative              **
# nonEmoComm      negative              **
# emoComm         positive              n.s.
# nonEmoComm      positive              .

# Residual standard error: 0.0936 on 154 degrees of freedom
# Multiple R-squared:  0.2623,	Adjusted R-squared:  0.2288 
# F-statistic: 7.823 on 7 and 154 DF,  p-value: 4.277e-08

##### X.4.3.2 valence + dominance ----
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-valence-NO-my.pdf", height = 4.5, width = 6)

# original scale
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum, y = D.Mean.Sum)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean dominance rating (controlled - in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-valence-orig-NO-my.pdf", height = 4, width = 6)

# faceted
# by predicate type
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/dominance-by-valence-faceted-NO-my.pdf", height = 4.5, width = 5)

# by direction of valence
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ V.Mean.Sum2.direction, labeller = as_labeller(c("negative" = "negative valence",
                                                               "positive" = "positive valence"))) 
ggsave("../graphs/dominance-by-valence-faceted-Vdir-NO-my.pdf", height = 4, width = 6)

# by direction of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) + 
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction), 
            aes(x = Inf, y = .01, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/dominance-by-valence-faceted-Vdir-Ddir-NO-my.pdf", height = 5, width = 5)

# by predicate type and direction of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) + 
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = .01, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/dominance-by-valence-faceted-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 8.5)

new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2
# 1      complain      0.4750      0.0250
# 2          pout      0.4175      0.1300
# 3         scoff      0.3825      0.1750
# 4         gloat      0.3300      0.2750
# 5        holler      0.3300      0.1550
# 6         groan      0.2750      0.1250
# 7      disclose      0.2500      0.2225
# 8          brag      0.2375      0.0800
# 9          moan      0.2375      0.2600
# 10    insinuate      0.2275      0.2100

new.scale %>% 
  filter(V.Mean.Sum2.direction == "positive" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2)
#   verb_renamed V.Mean.Sum2 D.Mean.Sum2
# 1     showcase      0.3225      0.1500
# 2        phone      0.2725      0.1400
# 3    challenge      0.2375      0.0800
# 4      contest      0.2125      0.0175
# 5        admit      0.1400      0.1100
# 6      retract      0.0950      0.0350
# 7          gab      0.0500      0.2400
# 8       squeal      0.0350      0.1075

new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean dominance rating (neutral - controlled / in control)",
       y = "Mean valence rating (neutral - unhappy/happy)", 
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-NO-my.pdf", height = 4, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (neutral - controlled / in control)",
       y = "Mean valence rating (enutral - unhappy/happy)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/valence-by-dominance-faceted-NO-my.pdf", height = 4, width = 5)

###### linear models -----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.1809     0.0247   7.324 1.11e-11 ***
# D.Mean.Sum2   0.2165     0.1012   2.140   0.0339 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1625 on 160 degrees of freedom
# Multiple R-squared:  0.02783,	Adjusted R-squared:  0.02175 
# F-statistic:  4.58 on 1 and 160 DF,  p-value: 0.03386

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm         .
# nonEmoComm      *

lm(D.Mean.Sum2 ~ V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17986    0.01677   10.73   <2e-16 ***
# V.Mean.Sum2  0.12851    0.06005    2.14   0.0339 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1252 on 160 degrees of freedom
# Multiple R-squared:  0.02783,	Adjusted R-squared:  0.02175 
# F-statistic:  4.58 on 1 and 160 DF,  p-value: 0.03386

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         .
# nonEmoComm      .

lm(V.Mean.Sum2 ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction significance of D.Mean.Sum2
# emoComm         negative              n.s
# nonEmoComm      negative              n.s.
# emoComm         positive              *
# nonEmoComm      positive              *

# Residual standard error: 0.1501 on 154 degrees of freedom
# Multiple R-squared:  0.2019,	Adjusted R-squared:  0.1657 
# F-statistic: 5.567 on 7 and 154 DF,  p-value: 9.874e-06

##### X.4.3.3 arousal + dominance ----
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-arousal-NO-my.pdf", height = 4.5, width = 6)

# original scale
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum, y = D.Mean.Sum)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (controlled - in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-arousal-orig-NO-my.pdf", height = 4.5, width = 6)

# faceted
# by predicate type
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (controlled - in control)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/dominance-by-arousal-faceted-NO-my.pdf", height = 4, width = 6)

# by direction of valence
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ V.Mean.Sum2.direction, labeller = as_labeller(c("negative" = "negative valence",
                                                               "positive" = "positive valence"))) 
ggsave("../graphs/dominance-by-arousal-faceted-Vdir-NO-my.pdf", height = 4.5, width = 6)

# by direction of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) + 
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction), 
            aes(x = Inf, y = .01, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/dominance-by-arousal-faceted-Vdir-Ddir-NO-my.pdf", height = 5, width = 5)

# by predicate type and by direction of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) + 
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
             labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                 D.Mean.Sum2.direction = as_labeller(D_labels),
                                 predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = .01, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/dominance-by-arousal-faceted-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 8.5)

new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean dominance rating (neutral - controlled / in control)",
       y = "Mean arousal rating (calm - excited)", 
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-dominance-NO-my.pdf", height = 4, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (neutral - controlled / in control)",
       y = "Mean arousal rating (calm - excited)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/arousal-by-dominance-faceted-NO-my.pdf", height = 5, width = 6)

###### linear models -----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.40696    0.01624  25.063   <2e-16 ***
# D.Mean.Sum2 -0.03016    0.06653  -0.453    0.651    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1068 on 160 degrees of freedom
# Multiple R-squared:  0.001283,	Adjusted R-squared:  -0.004959 
# F-statistic: 0.2056 on 1 and 160 DF,  p-value: 0.6509

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm         .    
# nonEmoComm      n.s.

lm(D.Mean.Sum2 ~ A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.22596    0.03889   5.810 3.27e-08 ***
# A.Mean.Sum2 -0.04254    0.09382  -0.453    0.651    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1269 on 160 degrees of freedom
# Multiple R-squared:  0.001283,	Adjusted R-squared:  -0.004959 
# F-statistic: 0.2056 on 1 and 160 DF,  p-value: 0.6509

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# emoComm         .
# nonEmoComm      n.s.

##### X.4.3.4 direction of valence + direction of dominance ----
###### tables ----
contingency_table <- table(new.scale$V.Mean.Sum2.direction, new.scale$D.Mean.Sum2.direction) %>% 
  print()
#          negative positive
# negative       39       34
# positive        8       83

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 37.315, df = 1, p-value = 1.005e-09

# The directions of dominance and valence seem highly correlated.

# distribution of direction within predicate types
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  count(predicateType2, V.Mean.Sum2.direction) %>% 
  group_by(predicateType2) %>% 
  mutate(percentage =  n * 100 / sum(n),
         ratio =  round(n / min(n)))
#   predicateType2 V.Mean.Sum2.direction     n percentage ratio
#   <chr>          <chr>                 <int>      <dbl> <dbl>
# 1 emoComm        negative                 21       84       5
# 2 emoComm        positive                  4       16       1
# 3 nonEmoComm     negative                 52       38.0     1
# 4 nonEmoComm     positive                 85       62.0     2

new.scale %>% 
  filter(predicateType == "communicative") %>% 
  count(predicateType2, D.Mean.Sum2.direction) %>% 
  group_by(predicateType2) %>% 
  mutate(percentage =  n * 100 / sum(n),
         ratio =  round(n / min(n)))
#   predicateType2 D.Mean.Sum2.direction     n percentage ratio
#   <chr>          <chr>                 <int>      <dbl> <dbl>
# 1 emoComm        negative                 13       52       1
# 2 emoComm        positive                 12       48       1
# 3 nonEmoComm     negative                 34       24.8     1
# 4 nonEmoComm     positive                103       75.2     3


# these ratios are the same as in analysis3.

# distribution of directions by predicate types
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  count(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  mutate(percentage =  n * 100 / sum(n))
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction     n percentage
#   <chr>          <chr>                 <chr>                 <int>      <dbl>
# 1 emoComm        negative              negative                 12      57.1 
# 2 emoComm        negative              positive                  9      42.9 
# 3 emoComm        positive              negative                  1      25   
# 4 emoComm        positive              positive                  3      75   
# 5 nonEmoComm     negative              negative                 27      51.9 
# 6 nonEmoComm     negative              positive                 25      48.1 
# 7 nonEmoComm     positive              negative                  7       8.24
# 8 nonEmoComm     positive              positive                 78      91.8 

# communicatives overall
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  count(predicateType, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  group_by(predicateType, V.Mean.Sum2.direction) %>%
  mutate(percentage =  n * 100 / sum(n))
#   predicateType V.Mean.Sum2.direction D.Mean.Sum2.direction     n percentage
#   <chr>         <chr>                 <chr>                 <int>      <dbl>
# 1 communicative negative              negative                 39      53.4 
# 2 communicative negative              positive                 34      46.6 
# 3 communicative positive              negative                  8       8.99
# 4 communicative positive              positive                 81      91.0 


### X.5 projection: VAD against projection ratings ----
#### X.5.1 valence ----
##### X.5.1.1 overall ---- 
###### plots ----
# projection by valence
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
ggsave("../graphs/projection-by-valence2-NO-my.pdf", height = 4, width = 6)

# projection by valence faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2-NO-my.pdf", height = 4, width = 5)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13587    0.03215   4.225 3.97e-05 ***
# V.Mean.Sum2  0.32428    0.11415   2.841  0.00508 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2404 on 162 degrees of freedom
# Multiple R-squared:  0.04745,	Adjusted R-squared:  0.04157 
# F-statistic:  8.07 on 1 and 162 DF,  p-value: 0.005079

# by predicate type (the faceted plot with one combined fitted line)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         *
# nonEmoComm      n.s.

# Valence is significant overall and within the emoComms.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   1.4132     0.3268   4.324 1.53e-05 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2
# emoComm           ***
# nonEmoComm        n.s.


##### X.5.1.2 with direction ----
# projection by valence with direction
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -.5),
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
                                 .cols = predicateType2_names)) +
  geom_text(data = new.scale %>% filter(predicateType == "communicative") %>% 
              count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-NO-my.pdf", 
       height = 5, width = 5)

# by direction of valence
mean.proj.Vdir <- d.proj.vad %>%
  filter(predicateType == "communicative") %>% 
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
        #plot.margin = margin(5.5, 5.5, 22, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Direction of valence") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-Vdir-NO-my.pdf", height = 4, width = 4)

# by direction of valence and predicate type
mean.proj.Vdir2 <- d.proj.vad %>%
  filter(predicateType == "communicative") %>% 
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir2) # 4

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
ggsave("../graphs/projection-by-Vdir-predType2-NO-my.pdf", height = 4, width = 4.5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ V.Mean.Sum2.direction, 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.28463    0.02734  10.411  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.13781    0.03689  -3.736  0.00026 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2336 on 160 degrees of freedom
# Multiple R-squared:  0.08024,	Adjusted R-squared:  0.07449 
# F-statistic: 13.96 on 1 and 160 DF,  p-value: 0.0002596

# by direction of valence and predicate type
lm(Mean.Proj ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2  significance of V.Mean.Sum2.direction
# emoComm         n.s.
# nonEmoComm      **

# valence and direction of valence
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                                         0.08859    0.04250   2.085   0.0387 *
# V.Mean.Sum2                                                         0.28864    0.17210   1.677   0.0955 .
# fct_relevel(V.Mean.Sum2.direction, "positive")negative              0.13629    0.06311   2.160   0.0323 *
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative -0.05514    0.22734  -0.243   0.8087  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2312 on 158 degrees of freedom
# Multiple R-squared:   0.11,	Adjusted R-squared:  0.09311 
# F-statistic:  6.51 on 3 and 158 DF,  p-value: 0.0003517

# difference in V.Mean.Sum2.direction is significant.

# V.Mean.Sum2.direction   significance of V.Mean.Sum2
# negative                n.s.
# positive                .

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * fct_relevel(predicateType, "communicative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2	V.Mean.Sum2.direction   significance of V.Mean.Sum2
# emoComm         negative                *
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# by predicate type and direction of valence and dominance (= the nested faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
     fct_relevel(D.Mean.Sum2.direction, "positive") * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction  significance of V.Mean.Sum2
# 1        emoComm              negative              negative  n.s.
# 2        emoComm              negative              positive  *
# 3        emoComm              positive              negative  n.s.
# 4        emoComm              positive              positive  n.s.
# 5     nonEmoComm              negative              negative  n.s.
# 6     nonEmoComm              negative              positive  n.s.
# 7     nonEmoComm              positive              negative  n.s. 
# 8     nonEmoComm              positive              positive  .

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive  -0.5643     0.1083  -5.208 1.91e-07 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2.direction
# emoComm           n.s.
# nonEmoComm        ***


clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                 0.9817     0.4277   2.295 0.021711 *  
# V.Mean.Sum2.directionpositive              -0.6217     0.1876  -3.315 0.000916 ***
# V.Mean.Sum2:V.Mean.Sum2.directionpositive   0.4934     0.6552   0.753 0.451403   

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction significance of V.Mean.Sum2
# emoComm         negative  **
# nonEmoComm      negative  n.s.
# emoComm         positive  *
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ commType) 
ggsave("../graphs/projection-by-valence-communicative-faceted-NO-my.pdf", height = 4, width = 6)

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-say-vs-non-say-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-say-vs-non-say-Vdir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# overall
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerb, "yes"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of V.Mean.Sum2
# no      *         
# yes     *

# Residual standard error: 0.2386 on 158 degrees of freedom
# Multiple R-squared:  0.05243,	Adjusted R-squared:  0.03444 
# F-statistic: 2.914 on 3 and 158 DF,  p-value: 0.03614

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of V.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                .
# yes		              positive                n.s.

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverb-faceted-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverb-faceted-Vdir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of V.Mean.Sum2
# discourse role verb n.s.
# mode verb           .

# Residual standard error: 0.2334 on 84 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.0861,	Adjusted R-squared:  0.04259 
# F-statistic: 1.979 on 4 and 84 DF,  p-value: 0.1052

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb") * 
     fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of V.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              *
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2197 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.2291,	Adjusted R-squared:  0.152 
# F-statistic: 2.972 on 8 and 80 DF,  p-value: 0.005774

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-modeverb-faceted-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-modeverb-faceted-Vdir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of V.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of V.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              *
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

# Residual standard error: 0.2098 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.2316,	Adjusted R-squared:  0.09369 
# F-statistic: 1.679 on 7 and 39 DF,  p-value: 0.1427

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-saybymeans-faceted-NO-my.pdf", height = 4, width = 6)

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
        #axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-saybymeans-faceted-Vdir-NO-my.pdf", height = 5, width = 6)

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of V.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "form") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
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
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(6, "pt"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-arousal2-NO-my.pdf", height = 3.5, width = 6)

# projection by arousal faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-faceted2-NO-my.pdf", height = 4, width = 5)

###### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.07146    0.07381   0.968   0.3344  
# A.Mean.Sum2  0.34309    0.17806   1.927   0.0558 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2408 on 160 degrees of freedom
# Multiple R-squared:  0.02268,	Adjusted R-squared:  0.01657 
# F-statistic: 3.713 on 1 and 160 DF,  p-value: 0.05577

# by predicate type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2	significance of A.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.2365 on 158 degrees of freedom
# Multiple R-squared:  0.0693,	Adjusted R-squared:  0.05163 
# F-statistic: 3.922 on 3 and 158 DF,  p-value: 0.009825

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)  
# A.Mean.Sum2   1.0070     0.5105   1.973   0.0485 *

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# emoComm         n.s. 
# nonEmoComm      n.s.


##### X.5.2.2 with direction of valence ----
###### plots ----
# projection by arousal faceted with emotive component
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative") %>% 
              count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-NO-my.pdf", 
       height = 5, width = 5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()

# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                          0.0292     0.1032   0.283    0.777
# A.Mean.Sum2                                                          0.3169     0.2698   1.175    0.242
# fct_relevel(V.Mean.Sum2.direction, "positive")negative               0.2424     0.1515   1.601    0.111
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative  -0.2872     0.3652  -0.786    0.433
# 
# Residual standard error: 0.234 on 158 degrees of freedom
# Multiple R-squared:  0.08829,	Adjusted R-squared:  0.07098 
# F-statistic:   5.1 on 3 and 158 DF,  p-value: 0.002142

# difference in V.Mean.Sum2.direction is not significant.

# V.Mean.Sum2.direction   significance of A.Mean.Sum2
# negative                n.s.
# positive                n.s.

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2	V.Mean.Sum2.direction   significance of A.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2341 on 154 degrees of freedom
# Multiple R-squared:  0.1107,	Adjusted R-squared:  0.07033 
# F-statistic:  2.74 on 7 and 154 DF,  p-value: 0.01043

# by predicate type and direction of valence and dominance (= the nested faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
     fct_relevel(D.Mean.Sum2.direction, "positive") * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction  significance of A.Mean.Sum2
# 1        emoComm              negative              negative  n.s.
# 2        emoComm              negative              positive  n.s.
# 3        emoComm              positive              negative  n.s.
# 4        emoComm              positive              positive  n.s.
# 5     nonEmoComm              negative              negative  n.s.
# 6     nonEmoComm              negative              positive  n.s.
# 7     nonEmoComm              positive              negative  n.s. 
# 8     nonEmoComm              positive              positive  n.s.


###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)  
# A.Mean.Sum2                                -0.3661     0.7528  -0.486   0.6267  
# V.Mean.Sum2.directionpositive              -0.9789     0.4523  -2.164   0.0305 *
# A.Mean.Sum2:V.Mean.Sum2.directionpositive   1.0655     1.0983   0.970   0.3320   

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction	significance of A.Mean.Sum2
# emoComm		      negative	n.s.
# nonEmoComm	    negative  n.s.
# emoComm		      positive  n.s.
# nonEmoComm	    positive  n.s.

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#   V.Mean.Sum2.direction predicateType2  n
# 1              negative        emoComm 21
# 2              negative     nonEmoComm 52
# 3              positive        emoComm  4
# 4              positive     nonEmoComm 85

###### negative / positive valence only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients: 
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2 -0.14853    0.00297  -50.01   <2e-16 *** [negative]

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   1.1146     0.8187   1.361    0.173 [positive]

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2
# emoComm	      	negative	n.s.
# nonEmoComm	    negative  n.s.
# emoComm		      positive  n.s.
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ commType) 
ggsave("../graphs/projection-by-arousal-communicative-faceted-NO-my.pdf", height = 4, width = 6)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-say-vs-non-say-NO-my.pdf", height = 4, width = 5)

# with direction of valence
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-say-vs-non-say-Vdir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# overall
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerb, "no"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of A.Mean.Sum2
# no      n.s.         
# yes     n.s. 

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerb, "no") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of A.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                .

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverb-faceted-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverb-faceted-Vdir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of A.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# Residual standard error: 0.2366 on 84 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.06075,	Adjusted R-squared:  0.01602 
# F-statistic: 1.358 on 4 and 84 DF,  p-value: 0.2554

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of V.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2239 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.1994,	Adjusted R-squared:  0.1193 
# F-statistic:  2.49 on 8 and 80 DF,  p-value: 0.01819


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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-modeverb-faceted-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-modeverb-faceted-Vdir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of A.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.2168 on 43 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.09586,	Adjusted R-squared:  0.03278 
# F-statistic:  1.52 on 3 and 43 DF,  p-value: 0.223

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of A.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.


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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayByMeansVerbType) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-NO-my.pdf", height = 4, width = 6)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-Vdir-NO-my.pdf", height = 5, width = 6)

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "form"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of A.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# Residual standard error: 0.2345 on 29 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.0664,	Adjusted R-squared:  -0.09456 
# F-statistic: 0.4125 on 5 and 29 DF,  p-value: 0.8361

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of A.Mean.Sum2
# form                negative                n/a
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

# Residual standard error: 0.244 on 24 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.1628,	Adjusted R-squared:  -0.186 
# F-statistic: 0.4669 on 10 and 24 DF,  p-value: 0.8951

#### X.5.3 dominance ---- 
##### X.5.3.1 overall ----
###### plots ----
# projection by dominance with emotion entailment
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = D.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        legend.spacing.y = unit(6, "pt"),
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-dominance2-NO-my.pdf", height = 4, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-faceted2-NO-my.pdf", height = 4, width = 5)

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.23628    0.03693   6.398 1.66e-09 ***
# D.Mean.Sum2 -0.13098    0.15132  -0.866    0.388    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.243 on 160 degrees of freedom
# Multiple R-squared:  0.004661,	Adjusted R-squared:  -0.00156 
# F-statistic: 0.7493 on 1 and 160 DF,  p-value: 0.388

# by predicate type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm:        n.s.
# nonEmoComm:     n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# D.Mean.Sum2  -0.5606     0.4236  -1.324    0.186

clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of D.Mean.Sum2
# emoComm         **
# nonEmoComm      *


##### X5.3.2 with direction of valence ----
# happy vs unhappy
###### plots ----
# projection by dominance with direction
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(6, "pt"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
ggsave("../graphs/projection-by-domiance-with-direction-of-valence-NO-my.pdf", height = 3.5, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
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
              filter(predicateType == "communicative") %>% 
              count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-NO-my.pdf", 
       height = 5, width = 5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()

# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.26753    0.04803   5.570 1.07e-07 ***
# D.Mean.Sum2                                                         0.10013    0.23063   0.434    0.665    
# fct_relevel(V.Mean.Sum2.direction, "negative")positive             -0.11043    0.07272  -1.519    0.131    
# D.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive -0.14294    0.30679  -0.466    0.642    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2349 on 158 degrees of freedom
# Multiple R-squared:  0.0816,	Adjusted R-squared:  0.06416 
# F-statistic: 4.679 on 3 and 158 DF,  p-value: 0.003687

# difference in V.Mean.Sum2.direction is not significant.

# V.Mean.Sum2.direction   significance of D.Mean.Sum2
# negative                n.s.
# positive                n.s.

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction   significance of D.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# by predicate type and direction of valence and dominance (= the nested faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
     fct_relevel(D.Mean.Sum2.direction, "negative") * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction  significance of D.Mean.Sum2
# 1        emoComm              negative              negative  n.s.
# 2        emoComm              negative              positive  n.s.
# 3        emoComm              positive              negative  n.s.
# 4        emoComm              positive              positive  n.s.
# 5     nonEmoComm              negative              negative  n.s.
# 6     nonEmoComm              negative              positive  n.s.
# 7     nonEmoComm              positive              negative  n.s. 
# 8     nonEmoComm              positive              positive  n.s.


###### ordinal models ----
# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)  
# D.Mean.Sum2                                 0.6661     0.6752   0.986   0.3239  
# V.Mean.Sum2.directionpositive              -0.3609     0.2097  -1.721   0.0852 .
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -1.0394     0.8843  -1.175   0.2398

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  emoComm         *
# negative  nonEmoComm      n.s.
# positive  emoComm         .
# positive  nonEmoComm      n.s.
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



##### X.5.3.3 with "direction" of dominance ----
# controlled/submissive/guided/... vs in control/ autonomous/controlling/... 
# i.e., somebody else's vs one's own dominance
###### plots ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = D.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(6, "pt"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 
ggsave("../graphs/projection-by-domiance-with-direction-of-dominance-NO-my.pdf", height = 3.5, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
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
  geom_text(data = new.scale %>% filter(predicateType == "communicative") 
            %>% count(predicateType2, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2-NO-my.pdf", 
       height = 5, width = 5)

# by direction of dominance
mean.proj.Ddir <- d.proj.vad %>%
  filter(predicateType == "communicative") %>% 
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
ggsave("../graphs/projection-by-Ddir-NO-my.pdf", height = 4, width = 4)

# by direction of dominance and predicate type
mean.proj.Ddir2 <- d.proj.vad %>%
  filter(predicateType == "communicative") %>% 
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir2) # 4

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
ggsave("../graphs/projection-by-Ddir-predType2-NO-my.pdf", height = 4, width = 4.5)

###### linear models ----
# by direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2.direction, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.29574    0.03503   8.442 1.67e-14 ***
# D.Mean.Sum2.directionpositive -0.12015    0.04148  -2.897  0.00429 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2402 on 162 degrees of freedom
# Multiple R-squared:  0.04925,	Adjusted R-squared:  0.04338 
# F-statistic: 8.392 on 1 and 162 DF,  p-value: 0.004291

# by direction of dominance and predicate type
lm(Mean.Proj ~ D.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2.direction
# emoComm         n.s.
# nonEmoComm      *

# dominance and direction of dominance
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# D.Mean.Sum2.direction significance of D.Mean.Sum2
# negative              n.s.
# positive              n.s.
# 
# Residual standard error: 0.2371 on 158 degrees of freedom
# Multiple R-squared:  0.06406,	Adjusted R-squared:  0.04629 
# F-statistic: 3.605 on 3 and 158 DF,  p-value: 0.01481

# difference in D.Mean.Sum2.direction is not significant.

# by predicate type and direction of dominance (= the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(D.Mean.Sum2.direction, "positive"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2	D.Mean.Sum2.direction   significance of D.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2336 on 154 degrees of freedom
# Multiple R-squared:  0.1145,	Adjusted R-squared:  0.07424 
# F-statistic: 2.844 on 7 and 154 DF,  p-value: 0.00811

###### ordinal models ----
# with 'direction' of dominance
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * D.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)   
# D.Mean.Sum2                                1.61404    0.81632   1.977  0.04802 * 
# D.Mean.Sum2.directionpositive              0.07276    0.21261   0.342  0.73218   
# D.Mean.Sum2:D.Mean.Sum2.directionpositive -2.56464    0.97490  -2.631  0.00852 **

# with 'direction' of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  emoComm         *
# negative  nonEmoComm      n.s.
# positive  emoComm         n.s.
# positive  nonEmoComm      n.s.

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ commType) 
ggsave("../graphs/projection-by-dominance-communicative-faceted-NO-my.pdf", height = 4, width = 6)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-say-vs-non-say-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-say-vs-non-say-Vdir-NO-my.pdf", height = 5, width = 5)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-say-vs-non-say-Ddir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# overall
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayVerb, "no"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of D.Mean.Sum2
# no      n.s.         
# yes     n.s.

# Residual standard error: 0.2439 on 158 degrees of freedom
# Multiple R-squared:  0.009669,	Adjusted R-squared:  -0.009135 
# F-statistic: 0.5142 on 3 and 158 DF,  p-value: 0.6731

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of D.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                n.s.

# Residual standard error: 0.2354 on 154 degrees of freedom
# Multiple R-squared:  0.1014,	Adjusted R-squared:  0.06052 
# F-statistic: 2.482 on 7 and 154 DF,  p-value: 0.01928

# with direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            D.Mean.Sum2.direction   significance of D.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                n.s.

# Residual standard error: 0.2372 on 154 degrees of freedom
# Multiple R-squared:  0.08732,	Adjusted R-squared:  0.04583 
# F-statistic: 2.105 on 7 and 154 DF,  p-value: 0.04616

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverb-faceted-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverb-faceted-Vdir-NO-my.pdf", height = 5, width = 5)

# faceted by direction of dominance
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(D.Mean.Sum2.direction ~ sayVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & sayVerb == "yes" & verb_renamed != "say") %>% 
              count(sayVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverb-faceted-Ddir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of D.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# Residual standard error: 0.2348 on 84 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.07477,	Adjusted R-squared:  0.03071 
# F-statistic: 1.697 on 4 and 84 DF,  p-value: 0.1583

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb") * 
     fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of D.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2241 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.1978,	Adjusted R-squared:  0.1176 
# F-statistic: 2.465 on 8 and 80 DF,  p-value: 0.01928

# by sayVerbType type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb") * 
     fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        D.Mean.Sum2.direction   significance of D.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2243 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.1963,	Adjusted R-squared:  0.1159 
# F-statistic: 2.442 on 8 and 80 DF,  p-value: 0.02037

###### X.5.3.5.2 mode-verb type ---- 
###### plots ----
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-modeverb-faceted-NO-my.pdf", height = 4, width = 5)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-modeverb-faceted-Vdir-NO-my.pdf", height = 5, width = 5)

# faceted by direction of dominance
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(D.Mean.Sum2.direction ~ modeVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(sayVerbType == "mode verb") %>% 
              count(modeVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-modeverb-faceted-Ddir-NO-my.pdf", height = 5, width = 5)

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of D.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.216 on 43 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.1024,	Adjusted R-squared:  0.03983 
# F-statistic: 1.636 on 3 and 43 DF,  p-value: 0.1951

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means") * 
     fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of D.Mean.Sum2
# say-by-means	      negative                .
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

# Residual standard error: 0.2187 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.1653,	Adjusted R-squared:  0.01552 
# F-statistic: 1.104 on 7 and 39 DF,  p-value: 0.3802

# by modeVerbType type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means") * 
     fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      D.Mean.Sum2.direction   significance of D.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

# Residual standard error: 0.2122 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.2143,	Adjusted R-squared:  0.07323 
# F-statistic: 1.519 on 7 and 39 DF,  p-value: 0.1895

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_wrap(~ sayByMeansVerbType) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-NO-my.pdf", height = 4, width = 6)

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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-Vdir-NO-my.pdf", height = 5, width = 6)

# faceted by direction of dominance
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
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_grid(D.Mean.Sum2.direction ~ sayByMeansVerbType,
             labeller = labeller(.rows = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) +
  geom_text(data = new.scale %>% 
              filter(modeVerbType == "say-by-means") %>% 
              count(sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-Ddir-NO-my.pdf", height = 5, width = 6)

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "manner"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of D.Mean.Sum2
# form                n.s.
# manner              .
# sound               n.s.

# Residual standard error: 0.2237 on 29 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:   0.15,	Adjusted R-squared:  0.003401 
# F-statistic: 1.023 on 5 and 29 DF,  p-value: 0.4223

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "form") * 
     fct_relevel(V.Mean.Sum2.direction, "negative"), 
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
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.1894,	Adjusted R-squared:  -0.1483 
# F-statistic: 0.5608 on 10 and 24 DF,  p-value: 0.8289

# by sayByMeansVerbType type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound") * 
     fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	D.Mean.Sum2.direction   significance of D.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

# Residual standard error: 0.2415 on 24 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.1801,	Adjusted R-squared:  -0.1615 
# F-statistic: 0.5272 on 10 and 24 DF,  p-value: 0.854

#### X.5.4 valence + arousal ----
##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.06563    0.07290   0.900    0.369  
# V.Mean.Sum2  0.27542    0.12087   2.279    0.024 *
# A.Mean.Sum2  0.20221    0.18633   1.085    0.279  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2377 on 159 degrees of freedom
# Multiple R-squared:  0.05358,	Adjusted R-squared:  0.04168 
# F-statistic: 4.501 on 2 and 159 DF,  p-value: 0.01255

library(car)
vif(lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, 
       new.scale %>% filter(predicateType == "communicative"))) 
# V.Mean.Sum2 A.Mean.Sum2 
# 1.123722    1.123722 

lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)
# (Intercept)              0.01437    0.12873   0.112    0.911
# V.Mean.Sum2              0.50627    0.49244   1.028    0.305
# A.Mean.Sum2              0.32301    0.31189   1.036    0.302
# V.Mean.Sum2:A.Mean.Sum2 -0.51201    1.05862  -0.484    0.629
# 
# Residual standard error: 0.2383 on 158 degrees of freedom
# Multiple R-squared:  0.05498,	Adjusted R-squared:  0.03704 
# F-statistic: 3.064 on 3 and 158 DF,  p-value: 0.02978

vif(lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, 
       new.scale %>% filter(predicateType == "communicative")))
# V.Mean.Sum2             A.Mean.Sum2 V.Mean.Sum2:A.Mean.Sum2 
#   18.563009                3.133313               24.377519 

lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.212426   0.019738  10.762   <2e-16 ***
# V.Mean.Sum2.sc                 0.049763   0.021901   2.272   0.0244 *  
# A.Mean.Sum2.sc                 0.021917   0.019896   1.102   0.2723    
# V.Mean.Sum2.sc:A.Mean.Sum2.sc -0.008989   0.018585  -0.484   0.6293    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2383 on 158 degrees of freedom
# Multiple R-squared:  0.05498,	Adjusted R-squared:  0.03704 
# F-statistic: 3.064 on 3 and 158 DF,  p-value: 0.02978

vif(lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, 
       new.scale %>% filter(predicateType == "communicative")))
# V.Mean.Sum2.sc                A.Mean.Sum2.sc V.Mean.Sum2.sc:A.Mean.Sum2.sc 
#       1.349461                      1.125621                      1.241379

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)  
# A.Mean.Sum2               0.9723     0.9425   1.032   0.3022  
# V.Mean.Sum2               2.3635     1.4248   1.659   0.0972 .
# A.Mean.Sum2:V.Mean.Sum2  -2.2975     3.1018  -0.741   0.4589  

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "nonEmoComm") + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2 / V.Mean.Sum2
# emoComm         .     ***     
# nonEmoComm      n.s.  n.s.

##### negative / positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    [negative]
# A.Mean.Sum2               0.5822     1.5031   0.387   0.6985  
# V.Mean.Sum2               3.9940     2.2098   1.807   0.0707 .
# A.Mean.Sum2:V.Mean.Sum2  -5.9562     4.7146  -1.263   0.2065 


# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    [positive]
# A.Mean.Sum2                1.215      1.275   0.952    0.341
# V.Mean.Sum2                2.623      1.963   1.336    0.182
# A.Mean.Sum2:V.Mean.Sum2   -2.101      4.453  -0.472    0.637

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2 / V.Mean.Sum2
# emoComm	      	negative	n.s.  **
# nonEmoComm	    negative  n.s.  n.s.
# emoComm		      positive  Model is nearly unidentifiable: large eigenvalue ratio
# nonEmoComm	    positive  Model is nearly unidentifiable: large eigenvalue ratio


#### X.5.5 valence + arousal + dominance ----
##### combined plots ----
###### two lines ----
# combined plot with two fitted lines
# valence
vplot <- new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(6, "pt"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 

# arousal
aplot <- new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
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
        legend.spacing.y = unit(6, "pt"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
  labs(x = "Mean arousal rating", 
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)

# dominance
dplot <- new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
        legend.spacing.y = unit(6, "pt"),
        aspect.ratio = 1) +
  guides(colour = guide_legend(byrow = TRUE), 
         linetype = guide_legend(byrow = TRUE)) +
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
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-NO-my.pdf", height = 4.5, width = 9.5)

###### valence facets ----
# combined plot with valence facets
# valence
vplotf <- 
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
aplotf <- 
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
dplotf  <- 
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-facets-NO-my.pdf", height = 5.5, width = 10)

###### valence and dominance facets ----
# valence faceted by valence and dominance directions
vplotf2 <-   
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-NO-my.pdf", height = 5.5, width = 10)

# arousal faceted by valence and dominance directions
aplotf2 <- 
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-2-NO-my.pdf", height = 5.5, width = 14)

###### valence, dominance, predicate type facets ----
# valence faceted by predicate type and valence and dominance directions
vplotf3 <-     
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 8)

# arousal faceted by predicate type and valence and dominance directions
aplotf3 <-   
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        #axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 8)

# dominance faceted by predicate type, direction of valence and dominance
dplotf3 <-   
new.scale %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        #axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 8)

vplotf3 + 
  (aplotf3 & theme(axis.title.y = element_blank())) + 
  (dplotf3 & theme(axis.title.y = element_blank())) 
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-predType2-NO-my.pdf", height = 5, width = 24)

# VAD fitted lines only
new.scale %>% 
  filter(predicateType == "communicative") %>% 
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
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 12),
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
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-NO-my.pdf", height = 5.5, width = 8)

# VAD fitted lines only, with scaled ratings
new.scale %>% 
  filter(predicateType == "communicative") %>% 
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
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 12),
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
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-scaled-NO-my.pdf", height = 5.5, width = 8)

#### X.5.6 directions of valence and dominance ----
mean.projection <- d.proj.vad %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  arrange(desc(Mean.Proj))
# predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction Mean.Proj  CILow CIHigh YMin.Proj YMax.Proj
# <chr>          <chr>                 <chr>                     <dbl>  <dbl>  <dbl>     <dbl>     <dbl>
# 1 emoComm        negative              negative                  0.392 0.108  0.108     0.283      0.5  
# 2 emoComm        positive              positive                  0.333 0.267  0.267     0.0667     0.6  
# 3 emoComm        negative              positive                  0.311 0.144  0.133     0.167      0.444
# 4 nonEmoComm     positive              negative                  0.286 0.114  0.114     0.171      0.4  
# 5 nonEmoComm     negative              negative                  0.257 0.0669 0.0632    0.190      0.320
# 6 nonEmoComm     negative              positive                  0.249 0.0724 0.0723    0.177      0.321
# 7 emoComm        positive              negative                  0.2   0.5    0.4      -0.3        0.6  
# 8 nonEmoComm     positive              positive                  0.128 0.0425 0.0387    0.0851     0.166

ggplot(mean.projection, aes(x = predicateType2, y = Mean.Proj, colour = predicateType2, 
                            shape = D.Mean.Sum2.direction, linetype = V.Mean.Sum2.direction)) +
  #geom_hline(yintercept = 5, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, position = position_dodge(0.4)) +
  theme(legend.position = "right",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       shape = "Direction of dominance",
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, guide = "none")
ggsave("../graphs/projection-by-Vdir-Ddir-NO-my.pdf", height = 4, width = 6)

##### tables ----
# negative/positive valence vs negative/positive dominance predicates
# negative valence, negative dominance - higher projection
new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1           lie      0.6525      0.0700     nonEmoComm
# 2       dispute      0.6200      0.2150     nonEmoComm
# 3         bitch      0.6125      0.3375        emoComm
# 4          fake      0.5950      0.1325     nonEmoComm
# 5           sob      0.5875      0.4275        emoComm
# 6        bicker      0.5475      0.1150     nonEmoComm
# 7         whine      0.5450      0.0675        emoComm
# 8       quarrel      0.5400      0.0600        emoComm
# 9          weep      0.5300      0.2075        emoComm
# 10       reject      0.5125      0.3850     nonEmoComm

# negative valence, positive dominance - lower projection
new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1      complain      0.4750      0.0250        emoComm
# 2          pout      0.4175      0.1300        emoComm
# 3         scoff      0.3825      0.1750     nonEmoComm
# 4         gloat      0.3300      0.2750        emoComm
# 5        holler      0.3300      0.1550     nonEmoComm
# 6         groan      0.2750      0.1250        emoComm
# 7      disclose      0.2500      0.2225     nonEmoComm
# 8          brag      0.2375      0.0800        emoComm
# 9          moan      0.2375      0.2600        emoComm
# 10    insinuate      0.2275      0.2100     nonEmoComm

# positive valence, negative dominance
new.scale %>% 
  filter(V.Mean.Sum2.direction == "positive" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#   verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1     showcase      0.3225      0.1500     nonEmoComm
# 2        phone      0.2725      0.1400     nonEmoComm
# 3    challenge      0.2375      0.0800     nonEmoComm
# 4      contest      0.2125      0.0175     nonEmoComm
# 5        admit      0.1400      0.1100     nonEmoComm
# 6      retract      0.0950      0.0350     nonEmoComm
# 7          gab      0.0500      0.2400     nonEmoComm
# 8       squeal      0.0350      0.1075        emoComm

# positive valence, positive dominance
new.scale %>% 
  filter(V.Mean.Sum2.direction == "positive" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1          joke      0.7200      0.1975     nonEmoComm
# 2        praise      0.6625      0.3925     nonEmoComm
# 3          sing      0.6250      0.4175     nonEmoComm
# 4         cheer      0.5250      0.3425        emoComm
# 5          know      0.4550      0.1950      cognitive
# 6         think      0.4200      0.3750      cognitive
# 7       promise      0.4100      0.3675     nonEmoComm
# 8           vow      0.4000      0.2500     nonEmoComm
# 9    articulate      0.3875      0.4725     nonEmoComm
# 10        voice      0.3750      0.3050     nonEmoComm

# frequency of predicate types for valence and dominance direction groups
new.scale %>% 
  group_by(V.Mean.Sum2.direction, D.Mean.Sum2.direction, predicateType2) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(group_size = sum(n), .by = predicateType2) %>% 
  mutate(proportion = n / group_size) %>% 
  arrange(V.Mean.Sum2.direction, D.Mean.Sum2.direction, desc(n))
#   V.Mean.Sum2.direction D.Mean.Sum2.direction predicateType2     n group_size proportion
#   <chr>                 <chr>                 <chr>          <int>      <int>      <dbl>
# 1 negative              negative              nonEmoComm        27        137     0.197 
# 2 negative              negative              emoComm           12         25     0.48  

# 3 negative              positive              nonEmoComm        25        137     0.182 
# 4 negative              positive              emoComm            9         25     0.36  

# 5 positive              negative              nonEmoComm         7        137     0.0511
# 6 positive              negative              emoComm            1         25     0.04  

# 7 positive              positive              nonEmoComm        78        137     0.569 
# 8 positive              positive              emoComm            3         25     0.12  
# 9 positive              positive              cognitive          2          2     1  

# frequency of valence and dominance direction groups for predicate types
new.scale %>% 
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(group_size = sum(n), .by = predicateType2) %>% 
  mutate(proportion = n / group_size) %>% 
  arrange(predicateType2, desc(n))
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction     n group_size proportion
#   <chr>          <chr>                 <chr>                 <int>      <int>      <dbl>
# 1 cognitive      positive              positive                  2          2     1     

# 2 emoComm        negative              negative                 12         25     0.48  
# 3 emoComm        negative              positive                  9         25     0.36  
# 4 emoComm        positive              positive                  3         25     0.12  
# 5 emoComm        positive              negative                  1         25     0.04  

# 6 nonEmoComm     positive              positive                 78        137     0.569 
# 7 nonEmoComm     negative              negative                 27        137     0.197 
# 8 nonEmoComm     negative              positive                 25        137     0.182 
# 9 nonEmoComm     positive              negative                  7        137     0.0511

# V Volition ----
## V.1 all predicates ----
### V.1.1 by predicate ----
#### plots ----
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
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
ggsave("../graphs/projection-by-volition-NO-my.pdf", height = 4, width = 13)

# faceted by predicate type
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
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
ggsave("../graphs/projection-by-volition-and-predicateType2-NO-my.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.18916    0.01898   9.966  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.16684    0.05233   3.189  0.00168 ** 

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol <- d.proj.comm %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj.vol
nrow(mean.proj.vol) # 4

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
ggsave("../graphs/projection-by-predicateType-and-volition-NO-my.pdf", height = 4, width = 10)

#### distribution ----
# how many predicates of which predicateType are non-/volitional?
d.proj %>%
  select(predicateType2, verb_renamed, volition) %>%
  unique() %>%
  group_by(predicateType2, volition) %>%
  summarize(count=n())

#   predicateType2 volition       count
#   <chr>          <chr>          <int>
# 1 cognitive      non-volitional     2
# 2 emoComm        non-volitional    16
# 3 emoComm        volitional        11
# 4 nonEmoComm     non-volitional     9
# 5 nonEmoComm     volitional       154


# Z projection by communicatives ----
## with think and know ----
### plot ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "believe"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "believe")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "believe"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.2,
                   colour = "purple") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
# ggsave("../graphs/projection-by-communicative-NO-my.pdf", height = 4, width = 13)

## with highly projective predicates ---- 
# communicative predicates that project as much or more strongly than 'know'
mean.proj %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj) %>% 
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
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-communicative-highest-NO-my.pdf", height = 4, width = 13)


## with extreme predicates ----
# calculate difference in mean projection rating between the predicate with the 
# highest rating and 'know':
slice_max(mean.proj.comm, Mean.Proj, with_ties = FALSE)$Mean.Proj - 
  subset(mean.proj, verb_renamed == "know")$Mean.Proj
# 0.3

### lists ----
# communicative predicates whose projection rating ranges between that of 'think'
# and think + 0.3:
aaa <- mean.proj.comm %>% filter(Mean.Proj <= subset(mean.proj, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                  Mean.Proj >= subset(mean.proj, verb_renamed == "think")$Mean.Proj) %>% 
  arrange(Mean.Proj) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

# communicative predicates with similar projection ratings as 'know': 
# (know - 0.2) to max [= know + 0.3] 
bbb <- mean.proj.comm %>% filter(Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

### plots ----
# with labels for extreme predicates
#### 41-9 ----
# [name based on original plot] 
# 0 to (think + 0.3) and know to (know + 0.3)
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj %>% filter(predicateType == "communicative" & 
                                               Mean.Proj <= subset(mean.proj, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                               Mean.Proj >= 0), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj <= subset(mean.proj, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                                     Mean.Proj >= 0),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-communicative-41-9-NO-my.pdf", height = 4, width = 13)

#### 12-9 ----
# [name based on original plot]

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 12), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj %>% filter(predicateType == "communicative" & 
                                                               Mean.Proj >= 0), Mean.Proj, n = 12),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 2, nudge_y = -0.5,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-communicative-extremes-12-9-NO-my.pdf", height = 4, width = 13)

#### 28-26 ----
# [name based on original plot]

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 28), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj - 0.1), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj %>% filter(predicateType == "communicative" & 
                                                               Mean.Proj >= 0), Mean.Proj, n = 28),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 2, nudge_y = -0.7,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj - 0.1),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-communicative-extremes-28-26-NO-my.pdf", height = 4, width = 13)


#### 41-44 ----
# [name based on original plot]
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 41), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj - 0.2), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj %>% filter(predicateType == "communicative" & 
                                                               Mean.Proj >= 0), Mean.Proj, n = 41),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 2, nudge_y = -0.7,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType == "communicative" & 
                                                     Mean.Proj >= subset(mean.proj, verb_renamed == "know")$Mean.Proj - 0.2),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-communicative-extremes-41-44-NO-my.pdf", height = 4, width = 13)

## all emoComms ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComms", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType2 == "emoComm"),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-communicative-emoComms-NO-my.pdf", height = 4, width = 13)

### all predicates ----
ggplot(mean.proj, 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "predicate"), alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComms", alpha = 0.5)) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(predicateType2 == "emoComm"),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
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
#ggsave("../graphs/projection-by-predicate-emoComms-NO-my.pdf", height = 4, width = 13)

### manner - attitude ----
ggplot(mean.proj,aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(colour = "deepskyblue2", alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj), colour = "blue") +
  geom_point(data = mean.proj %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj), colour = "deeppink") +
  geom_point(data = mean.proj %>% filter(emoCommType == "manner"), 
             aes(x = verb_renamed, y = Mean.Proj), colour = "green4", alpha = 0.9) +
  geom_point(data = mean.proj %>% filter(emoCommType == "attitude"), 
             aes(x = verb_renamed, y = Mean.Proj), colour = "green4", alpha = 0.9) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj), colour = "orangered3") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj %>% filter(emoCommType == "manner"),
                   aes(label = verb_renamed, fill = "manner"), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "green4") +
  geom_label_repel(data = mean.proj %>% filter(emoCommType == "attitude"),
                   aes(label = verb_renamed, fill = "attitude"), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       fill = "Type of communicative with emotion entailment") + 
  guides(colour = "none") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_manual(limits = c("attitude", "manner"),
                    labels = c("with attitude entailment", "with manner entailment"),
                    values = c("yellow2", "palegreen"))
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude-NO-my.pdf", height = 4, width = 13)


# ZZ combined models ----
## predicate type ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj) %>% 
  summary()
nrow(mean.proj) # 192
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04768   7.069 2.93e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.03704    0.18156  -0.204  0.83857    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14678    0.05148  -2.852  0.00484 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2477 on 189 degrees of freedom
# Multiple R-squared:  0.04248,	Adjusted R-squared:  0.03235 
# F-statistic: 4.193 on 2 and 189 DF,  p-value: 0.01653

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.comm) %>% 
  summary()
nrow(mean.proj.comm) # 190

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04714   7.149 1.88e-11 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14678    0.05090  -2.884  0.00438 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.245 on 188 degrees of freedom
# Multiple R-squared:  0.04237,	Adjusted R-squared:  0.03727 
# F-statistic: 8.317 on 1 and 188 DF,  p-value: 0.004

# The effects of predicate type are significantly different between the two types of communicatives.
# However, predicateType2 explains very little of the variation found in the data.

## VAD ----
# predicate type and VAD ratings
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
nrow(new.scale) # 164
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.34800    0.04722   7.369 8.65e-12 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16446    0.05135  -3.203  0.00164 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2361 on 160 degrees of freedom
# Multiple R-squared:  0.06024,	Adjusted R-squared:  0.05437 
# F-statistic: 10.26 on 1 and 160 DF,  p-value: 0.001643

### Valence ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.26915    0.06308   4.267 3.39e-05 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.13076    0.05406  -2.419   0.0167 *  
# V.Mean.Sum2                                       0.22267    0.11923   1.868   0.0637 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2343 on 159 degrees of freedom
# Multiple R-squared:  0.08042,	Adjusted R-squared:  0.06885 
# F-statistic: 6.952 on 2 and 159 DF,  p-value: 0.001275

# Valence adds nothing to the model (slightly less nothing than across all embedding environments).

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                                   0.11746    0.11165   1.052   0.2944  
# fct_relevel(predicateType2, "emoComm")nonEmoComm              0.03889    0.11645   0.334   0.7388  
# V.Mean.Sum2                                                   0.65106    0.28651   2.272   0.0244 *
# fct_relevel(predicateType2, "emoComm")nonEmoComm:V.Mean.Sum2 -0.51698    0.31474  -1.643   0.1025  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2331 on 158 degrees of freedom
# Multiple R-squared:  0.09585,	Adjusted R-squared:  0.07869 
# F-statistic: 5.584 on 3 and 158 DF,  p-value: 0.00115

# V.Mean.Sum2 is significant for emoComms but not for nonEmoComms.

### Arousal ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                       0.24880    0.09618   2.587  0.01058 * 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14833    0.05307  -2.795  0.00583 **
# A.Mean.Sum2                                       0.21353    0.18044   1.183  0.23842   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2358 on 159 degrees of freedom
# Multiple R-squared:  0.06845,	Adjusted R-squared:  0.05673 
# F-statistic: 5.842 on 2 and 159 DF,  p-value: 0.003564

# Arousal adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# A.Mean.Sum2 is not significant for emoComm and nonEmoComm.
# No significant interactions for emoComm and nonEmoComm.

# Residual standard error: 0.2365 on 158 degrees of freedom
# Multiple R-squared:  0.0693,	Adjusted R-squared:  0.05163 
# F-statistic: 3.922 on 3 and 158 DF,  p-value: 0.009825

### Dominance ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.36779    0.05509   6.677 3.87e-10 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16232    0.05152  -3.150  0.00195 ** 
# D.Mean.Sum2                                      -0.10341    0.14752  -0.701  0.48433    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2365 on 159 degrees of freedom
# Multiple R-squared:  0.06314,	Adjusted R-squared:  0.05136 
# F-statistic: 5.358 on 2 and 159 DF,  p-value: 0.0056

# Dominance adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                                   0.247735   0.076863   3.223  0.00154 **
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.007116   0.086688  -0.082  0.93468   
# D.Mean.Sum2                                                   0.523848   0.318842   1.643  0.10238   
# fct_relevel(predicateType2, "emoComm")nonEmoComm:D.Mean.Sum2 -0.792960   0.358489  -2.212  0.02841 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2337 on 158 degrees of freedom
# Multiple R-squared:  0.09128,	Adjusted R-squared:  0.07403 
# F-statistic:  5.29 on 3 and 158 DF,  p-value: 0.001676

# For emoComm and nonEmoComm, D.Mean.Sum2 is not significant. The effect of D.Mean.Sum2 on Mean.Proj 
# is significantly lower for the nonEmoComms than for the emoComms.

### Collinearity ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35410    0.03107  11.396  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15134    0.03379  -4.479 1.42e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1554 on 160 degrees of freedom
# Multiple R-squared:  0.1114,	Adjusted R-squared:  0.1059 
# F-statistic: 20.06 on 1 and 160 DF,  p-value: 1.419e-05

# The fact that the model with valence as the dependent variable has the highest R-squared and 
# F-statistic indicates that the (possible) correlation between VAD and predicateType2 is highest 
# for valence.

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.46455    0.02066  22.480  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.07555    0.02247  -3.362 0.000967 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1033 on 160 degrees of freedom
# Multiple R-squared:  0.06599,	Adjusted R-squared:  0.06015 
# F-statistic:  11.3 on 1 and 160 DF,  p-value: 0.0009672

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.19140    0.02535   7.551  3.1e-12 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.02072    0.02756   0.752    0.453    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1267 on 160 degrees of freedom
# Multiple R-squared:  0.003518,	Adjusted R-squared:  -0.00271 
# F-statistic: 0.5649 on 1 and 160 DF,  p-value: 0.4534

## SAY ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04714   7.149 1.88e-11 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14678    0.05090  -2.884  0.00438 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.245 on 188 degrees of freedom
# Multiple R-squared:  0.04237,	Adjusted R-squared:  0.03727 
# F-statistic: 8.317 on 1 and 188 DF,  p-value: 0.004385

### say verb ----
lm(Mean.Proj ~ sayVerb , data = mean.proj.comm) %>%
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.24775    0.02708   9.150   <2e-16 ***
# sayVerbyes  -0.06568    0.03625  -1.812   0.0716 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2482 on 188 degrees of freedom
# Multiple R-squared:  0.01716,	Adjusted R-squared:  0.01193 
# F-statistic: 3.282 on 1 and 188 DF,  p-value: 0.07162

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerb , data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.41891    0.05576   7.513 2.32e-12 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.17972    0.05162  -3.481 0.000621 ***
# sayVerbyes                                       -0.09611    0.03629  -2.648 0.008780 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2411 on 187 degrees of freedom
# Multiple R-squared:  0.07699,	Adjusted R-squared:  0.06711 
# F-statistic: 7.799 on 2 and 187 DF,  p-value: 0.0005584

# sayVerb is significant! (Unlike across predicate types.)

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
# (Intercept)           0.12549    0.03374   3.719 0.000325 ***
# sayVerbTypemode verb  0.10969    0.04705   2.332 0.021672 *  
# sayVerbTypesay        0.07451    0.24330   0.306 0.760039    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.241 on 103 degrees of freedom
# (84 observations deleted due to missingness)
# Multiple R-squared:  0.05018,	Adjusted R-squared:  0.03174 
# F-statistic: 2.721 on 2 and 103 DF,  p-value: 0.07055

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(sayVerb == "yes")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.32174    0.04891   6.578 1.97e-09 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.17837    0.05528  -3.227  0.00167 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2346 on 104 degrees of freedom
# Multiple R-squared:  0.091,	Adjusted R-squared:  0.08226 
# F-statistic: 10.41 on 1 and 104 DF,  p-value: 0.001675

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerbType, data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.27626    0.07286   3.791 0.000254 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15077    0.06494  -2.322 0.022240 *  
# sayVerbTypemode verb                              0.04548    0.05374   0.846 0.399407    
# sayVerbTypesay                                    0.07451    0.23828   0.313 0.755147    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.236 on 102 degrees of freedom
# (84 observations deleted due to missingness)
# Multiple R-squared:  0.09785,	Adjusted R-squared:  0.07132 
# F-statistic: 3.688 on 3 and 102 DF,  p-value: 0.01439

# say verb type adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients: (2 not defined because of singularities)
#                                                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                            0.27626    0.07286   3.791 0.000254 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                      -0.15077    0.06494  -2.322 0.022240 *  
# sayVerbTypemode verb                                                   0.04548    0.05374   0.846 0.399407    
# sayVerbTypesay                                                         0.07451    0.23828   0.313 0.755147    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypemode verb       NA         NA      NA       NA    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypesay             NA         NA      NA       NA    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.236 on 102 degrees of freedom
# (84 observations deleted due to missingness)
# Multiple R-squared:  0.09785,	Adjusted R-squared:  0.07132 
# F-statistic: 3.688 on 3 and 102 DF,  p-value: 0.01439

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
# (136 observations deleted due to missingness)
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

# mode verb type adds barely anything to the model.

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

# sayByMeansVerbType adds barely anything to the model.

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

# M Monsterplots ----
## M.1 Valence ----
### valence + Vdir ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(V.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverbs-monsterplot-VDir-NO-my.pdf", height = 6, width = 12)

### valence + Vdir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(predicateType2 + V.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(c("negative" = "negative\nvalence", 
                                                                         "positive" = "positive\nvalence")), 
                                   predicateType2 = as_labeller(predicateType2_names),
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverbs-monsterplot-VDir-predType2-NO-my.pdf", height = 8.5, width = 12)

### valence + Vdir + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(predicateType2 + V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   predicateType2 = as_labeller(c("emoComm" = "communicative with emotion entailment", 
                                                                  "nonEmoComm" = "communicative without emotion entailment")),
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverbs-monsterplot-VDir-Ddir-predType2-NO-my.pdf", height = 15, width = 12)

## M.2 Arousal ----
### arousal ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.title.y = element_text(hjust = -.05),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(. ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverbs-monsterplot-NO-my.pdf", height = 4.3, width = 12)

### arousal + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(predicateType2 ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(
                 predicateType2 = as_labeller(predicateType2_names),
                 sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverbs-monsterplot-predType2-NO-my.pdf", height = 5.5, width = 13)


### arousal + Vdir + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(predicateType2 + V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   predicateType2 = as_labeller(c("emoComm" = "communicative with emotion entailment", 
                                                                  "nonEmoComm" = "communicative without emotion entailment")),
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverbs-monsterplot-Vdir-Ddir-predType2-NO-my.pdf", height = 15, width = 12)


## M.3 Dominance ----
### dominance + Ddir ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(D.Mean.Sum2.direction = as_labeller(D_labels), 
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverbs-monsterplot-Ddir-NO-my.pdf", height = 6, width = 12)


### dominance + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(predicateType2 + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   predicateType2 = as_labeller(predicateType2_names),
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverbs-monsterplot-Ddir-predType2-NO-my.pdf", height = 8.5, width = 12)


### dominance + Vdir + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) +
  facet_nested(predicateType2 + V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   predicateType2 = as_labeller(c("emoComm" = "communicative with emotion entailment", 
                                                                  "nonEmoComm" = "communicative without emotion entailment")),
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverbs-monsterplot-VDir-Ddir-predType2-NO-my.pdf", height = 15, width = 12)


## M.4 VAD + Vdir + Ddir ----
# fitted lines for all three VAD ratings
### say vs non-say ----
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-say-non-say-monsterplot-VDir-Ddir-NO-my.pdf", height = 9, width = 6)

### sayVerbType ----
new.scale %>% 
  filter(predicateType == "communicative"& verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb, sayVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-sayVerbType-monsterplot-VDir-Ddir-NO-my.pdf", height = 9, width = 7)

### modeVerbType ----
new.scale %>% 
  filter(predicateType == "communicative"& verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-modeVerbType-monsterplot-VDir-Ddir-NO-my.pdf", height = 8.5, width = 8.5)

### sayByMeansVerbType (all sayVerb categories) ----
new.scale %>% 
  filter(predicateType == "communicative"& verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + 
                 sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType,
                    sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-sayByMeansVerbType-monsterplot-VDir-Ddir-NO-my.pdf", height = 9, width = 12)

## VAD + Vdir + Ddir +predicateType2 ----
# fitted lines for all three VAD ratings faceted by Vdir, Ddir and predicateType2
### all sayVerb categories -----
new.scale %>% 
  filter(predicateType == "communicative" & verb_renamed != "say") %>% 
  mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
         modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
         sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_line(aes(x = V.Mean.Sum2, y = Mean.Proj, colour = "valence"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = A.Mean.Sum2, y = Mean.Proj, colour = "arousal"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  geom_line(aes(x = D.Mean.Sum2, y = Mean.Proj, colour = "dominance"), 
            stat = "smooth", method = "lm", alpha = .8, linewidth = 1) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1,
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence / arousal / dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30")) +
  facet_nested(predicateType2 + V.Mean.Sum2.direction  + D.Mean.Sum2.direction ~ sayVerb + sayVerbType + modeVerbType + sayByMeansVerbType, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(c("negative" = "negative\ndominance", 
                                                                         "positive" = "positive\ndominance")), 
                                   predicateType2 = as_labeller(c("emoComm" = "communicative with emotion entailment", 
                                                                  "nonEmoComm" = "communicative without emotion entailment")),
                                   sayVerb = as_labeller(c("yes" = "say verb", "no" = "non-say verb")))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative" & verb_renamed != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-sayverbs-monsterplot-VDir-Ddir-predType2-NO-my.pdf", height = 15, width = 12)
