# Testing hypotheses based on results of experiment 1

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
library(ggtext)
library(lme4)
library(lmerTest)
library(ggh4x)
library(cowplot)
theme_set(theme_bw())

# prepare data ----
## new data ----
e1 <- read.csv("../data/e1.csv")
nrow(e1) # 3454
names(e1)
length(unique(e1$participant_id)) # 314

# load predicate coding
y <- read.csv("../../MegaVeridicality/data/predicate-coding.csv")
nrow(y) # 544
names(y)

y1 <- y %>% filter(verb_renamed %in% e1$predicate)
nrow(y) # 192

e1 <- left_join(e1, y1, by = c("predicate" = "verb_renamed"))
nrow(e1) # 3454

# specify colours and names for predicate types
cols2 <- c(cognitive = "coral",
           emoComm = "green3",
           nonEmoComm = "deepskyblue2")

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotion entailment", 
                           "nonEmoComm" = "communicative without\nemotion entailment")

e1 <- e1 %>% 
  mutate(predicateType = case_when(communicative == "yes" ~ "communicative",   
                                   cognitive == "yes" ~ "cognitive"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         volition = case_when(volitional == "yes" ~ "volitional",
                              TRUE ~ "non-volitional"),
         predicateType2 = case_when(predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "cognitive" ~ "cognitive"),
         commType = case_when(pure_comm == "yes" ~ "pure",
                              discourse_participation_comm == "yes" ~ "discourse participation",
                              state_changing_comm == "yes" ~ "state changing"),
         emoCommType = case_when(emo_comm_manner == "yes" ~ "manner",
                                 emo_comm_attitude == "yes" ~ "attitude"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
                              TRUE ~ "no"),
         sayVerb = case_when(say_verb == "yes" ~ "yes",
                             TRUE ~ "no"),
         sayVerbType = case_when(discourse_role_verb == "yes" ~ "discourse role verb",
                                 mode_verb == "yes" ~ "mode verb", 
                                 predicate == "say" ~ "say"),
         modeVerbType = case_when(say_by_means == "yes" ~ "say-by-means",
                                  say_with_attitude == "yes" ~ "say-with-attitude"),
         sayByMeansVerbType = case_when(say_by_means_manner == "yes" ~ "manner",
                                        say_by_means_sound == "yes" ~ "sound",
                                        say_by_means_form == "yes" ~ "form"))

# add the number of ratings each predicate has received
e1 <- e1 %>%
  group_by(predicate) %>%
  mutate(ratingsCount = n()) %>%
  ungroup()

# how many of which type of predicate?
e1 %>%
  group_by(predicateType2) %>%
  distinct(predicate) %>%
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           27
# 3 nonEmoComm       163

# communicatives only
e1.comm <- e1 %>% filter(predicateType == "communicative")

e1.comm %>%
  distinct(predicate) %>%
  count() # 190

# calculate by-predicate projection means for new data
mean.proj.e1 <- e1 %>%
  group_by(predicate) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.e1) # 192
# cat(as.character(mean.proj.e1$predicate), sep = "\n")
# cat(paste0('"', as.character(mean.proj.e1$predicate), '",'))

tmp <- e1 %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, 
           ratingsCount)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, 
           ratingsCount)
nrow(tmp) # 192

mean.proj.e1 <- left_join(mean.proj.e1, tmp, by = c("predicate")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.e1) # 192

# communicatives only
mean.proj.e1.comm <- mean.proj.e1 %>% filter(predicateType == "communicative")
mean.proj.e1.comm %>% nrow() # 190


## MV dataset (White & Rawlins 2018)----
d <- read.csv("../../MegaVeridicality/data/d.csv")
nrow(d) # 21692

d <- left_join(d, y, by = c("verb", "voice"))
nrow(d) # 21692

### overall ----
# create dataset for projection inferences
d.proj <- droplevels(subset(d, d$polarity == "negative" | d$conditional == "True"))
nrow(d.proj) # 16291

# only the predicates investigated here
d.proj <- d.proj %>% filter(verb_renamed %in% mean.proj.e1$predicate)
d.proj %>% nrow() # 5753
length(unique(d.proj$verb_renamed)) # 192

d.proj <- d.proj %>% 
  mutate(predicate = verb_renamed,
    predicateType = case_when(communicative == "yes" ~ "communicative",   
                                   cognitive == "yes" ~ "cognitive"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         volition = case_when(volitional == "yes" ~ "volitional",
                              TRUE ~ "non-volitional"),
         predicateType2 = case_when(predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "cognitive" ~ "cognitive"),
         commType = case_when(pure_comm == "yes" ~ "pure",
                              discourse_participation_comm == "yes" ~ "discourse participation",
                              state_changing_comm == "yes" ~ "state changing"),
         emoCommType = case_when(emo_comm_manner == "yes" ~ "manner",
                                 emo_comm_attitude == "yes" ~ "attitude"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
                              TRUE ~ "no"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "qcond",
                                 polarity == "negative" & conditional == "True" ~ "qcondneg"),
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

# calculate by-predicate projection means for MV data
mean.proj.MV <- d.proj %>%
  group_by(predicate) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV) # 192

tmp <- d.proj %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType)
nrow(tmp) # 192

mean.proj.MV <- left_join(mean.proj.MV, tmp, by = c("predicate")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.MV) # 192

#### by embedding environment ----
# calculate by-predicate projection means for MV data
mean.proj.MV.env <- d.proj %>%
  group_by(predicate, environment) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV.env) # 576

tmp <- d.proj %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, 
           environment)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, 
           environment)
nrow(tmp) # 576

mean.proj.MV.env <- left_join(mean.proj.MV.env, tmp, by = c("predicate", "environment")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.MV.env) # 576

# how many predicates in which environment?
mean.proj.MV.env %>%
  select(environment, predicate) %>% 
  unique() %>% 
  group_by(environment) %>% 
  summarise(count = n())
#   environment count
#   <chr>       <int>
# 1 neg           192
# 2 qcond         192
# 3 qcondneg      192


### negation only ----
# create dataset for projection from under negation only
d.proj.neg <- droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj.neg) # 5411

# only the predicates investigated here
d.proj.neg <- d.proj.neg %>% filter(verb_renamed %in% mean.proj.e1$predicate)
d.proj.neg %>% nrow() # 1913
length(unique(d.proj.neg$verb_renamed)) # 192

d.proj.neg <- d.proj.neg %>% 
  mutate(predicate = verb_renamed,
         predicateType = case_when(communicative == "yes" ~ "communicative",   
                                   cognitive == "yes" ~ "cognitive"),         
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         volition = case_when(volitional == "yes" ~ "volitional",
                              TRUE ~ "non-volitional"),
         predicateType2 = case_when(predicateType == "communicative" & 
                                      emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & 
                                      emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "cognitive" ~ "cognitive"),
         commType = case_when(pure_comm == "yes" ~ "pure",
                              discourse_participation_comm == "yes" ~ "discourse participation",
                              state_changing_comm == "yes" ~ "state changing"),
         emoCommType = case_when(emo_comm_manner == "yes" ~ "manner",
                                 emo_comm_attitude == "yes" ~ "attitude"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
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


# calculate by-predicate projection means
mean.proj.MV.neg <- d.proj.neg %>%
  group_by(predicate) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV.neg) # 192

tmp <- d.proj %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           changeOfState, activity, volition, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType)
nrow(tmp) # 192

mean.proj.MV.neg <- left_join(mean.proj.MV.neg, tmp, by = c("predicate")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.MV.neg) # 192

# low acceptability under negation only
# mean.proj.MV.neg %>% filter(Mean.Acc <= 4) %>% distinct(predicate, Mean.Acc) %>% print(n = Inf)
# predicate  Mean.Acc
#   <fct>         <dbl>
# 1 add            3.7 
# 2 chant          3.7 
# 3 chatter        4   
# 4 depict         4   
# 5 gab            3.1 
# 6 grunt          3.5 
# 7 howl           3.9 
# 8 jest           3.7 
# 9 lecture        3.8 
# 10 lie            3.7 
# 11 maintain       3.5 
# 12 mumble         3.9 
# 13 mutter         3.8 
# 14 quip           3.1 
# 15 rant           4   
# 16 reply          4   
# 17 scribble       3.2 
# 18 shriek         3.8 
# 19 sing           3.11
# 20 sketch         4   
# 21 snitch         3.9 
# 22 spout          3.6 
# 23 submit         4   
# 24 summarize      3.9 
# 25 underline      3.5 
# 26 underscore     3.4 
# 27 whimper        3.6 
# 28 yell           3.6 


## combine data ----
# add mean acceptability ratings from MV dataset to new data
e1 <- e1 %>%
  left_join(
    mean.proj.MV %>%
      select(predicate, Mean.Acc) %>%
      rename_with(~ paste0(., ".MV"), "Mean.Acc"),
    by = "predicate"
  ) %>%
  left_join(
    mean.proj.MV.neg %>%
      select(predicate, Mean.Acc) %>%
      rename_with(~ paste0(., ".MV.neg"), "Mean.Acc"),
    by = "predicate"
  )

# dataframe with mean projection and acceptability ratings
mean.proj.e1.plus <- mean.proj.e1 %>%
  left_join(
    mean.proj.MV %>%
      select(predicate, Mean.Proj, Mean.Acc, YMin.Proj, YMax.Proj) %>%
      rename_with(~ paste0(., ".MV"), starts_with(c("Mean", "Y"))),
    by = "predicate") %>%
  left_join(
    mean.proj.MV.neg %>%
      select(predicate, Mean.Proj, Mean.Acc, YMin.Proj, YMax.Proj) %>%
      rename_with(~ paste0(., ".MV.neg"), starts_with(c("Mean", "Y"))),
    by = "predicate") %>% 
  mutate(Mean.Proj.diff = Mean.Proj - Mean.Proj.MV,
         Mean.Proj.diff.neg = Mean.Proj - Mean.Proj.MV.neg)

# add rankings of projection ratings for both datasets
mean.proj.e1.plus <- mean.proj.e1.plus %>% 
  mutate(rankingNew = rank(Mean.Proj),
         rankingMVneg = rank(Mean.Proj.MV.neg),
         ranking_difference = abs(rankingNew - rankingMVneg))

mean.proj.e1.plus.comm <- mean.proj.e1.plus %>% filter(predicateType == "communicative")
mean.proj.e1.plus.comm %>% nrow() # 190


## valence and arousal data (Warriner et al. 2013) ----
# load data
w <-  read.csv("../../MegaVeridicality/data/BRM-emot-submit.csv")
nrow(w) # 13915

# combine projection and valence/arousal/dominance ratings in one data frame
e1.VAD <-  w %>%
  filter(Word %in% e1$predicate) %>%
  rename(predicate = Word) %>%
  select(predicate, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum) %>% 
  left_join(e1, by = "predicate")
nrow(e1.VAD) # 3083
n_distinct(e1.VAD$predicate) # 171
# of the 192 predicates in the new dataset, 171 are also in Warriner et al.'s (2013) dataset.

e1.VAD %>% 
  group_by(predicateType2) %>% 
  distinct(predicate) %>% 
  count() 
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           25
# 3 nonEmoComm       144

# combine mean projection and valence/arousal/dominance ratings in one data frame
mean.proj.e1.plus.VAD <- w %>% 
  filter(Word %in% e1$predicate) %>%
  rename(predicate = Word) %>%
  select(predicate, V.Mean.Sum, V.SD.Sum, A.Mean.Sum, D.Mean.Sum) %>% 
  left_join(mean.proj.e1.plus, by = "predicate")
nrow(mean.proj.e1.plus.VAD) # 171

mean.proj.e1.plus.VAD %>% 
  group_by(predicateType2) %>% 
  distinct(predicate) %>% 
  count() 
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           25
# 3 nonEmoComm       144

# exclusion of lemmata that could be interpreted differently (see analysis3.R)
e1.VAD <- e1.VAD %>% 
  filter(!predicate %in% c("address", "figure", "intercept", "log", "notice", "post", "share", 
                           "spot", "state", "stress", "type"))
nrow(e1.VAD) # 2969
n_distinct(e1.VAD$predicate) # 164

e1.VAD %>% 
  group_by(predicateType2) %>% 
  distinct(predicate) %>% 
  count() 
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           25
# 3 nonEmoComm       137

# communicatives only
e1.VAD.comm <- e1.VAD %>% filter(predicateType == "communicative")

e1.VAD.comm %>% 
  group_by(predicateType2) %>% 
  distinct(predicate) %>% 
  count() 
#   predicateType2     n
#   <chr>          <int>
# 1 emoComm           25
# 2 nonEmoComm       137

mean.proj.e1.plus.VAD <- mean.proj.e1.plus.VAD %>% 
  filter(!predicate %in% c("address", "figure", "intercept", "log", "notice", "post", "share", 
                              "spot", "state", "stress", "type"))
nrow(mean.proj.e1.plus.VAD) # 164

mean.proj.e1.plus.VAD %>% 
  group_by(predicateType2) %>% 
  distinct(predicate) %>% 
  count() 
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           25
# 3 nonEmoComm       137

# communicatives only
mean.proj.e1.plus.VAD.comm <- mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")
nrow(mean.proj.e1.plus.VAD.comm) # 162

# For 164 of the 192 predicates included in experiment 1, Warriner et al.'s (2013) dataset contains 
# valence, arousal and dominance ratings that could not have been based on an interpretation of the 
# lemma as a noun whose meaning is clearly different from that of the corresponding verb. 162 of 
# these are communicatives.


# rescale V + A + D ratings
# Valence and dominance have extremes (unhappy - happy, controlled - controlling) and a neutral state 
# in between. The neutral state of arousal is not in the middle of the "calm - aroused" scale, but 
# at the lower end: calmness is the absence of arousal; there is no such thing as "negative" arousal. 

# The ratings are rescaled to range from 0 to 1. 
e1.VAD <- e1.VAD %>% 
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

new.scale <- mean.proj.e1.plus.VAD %>% 
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


V_labels <- c("negative" = "negative valence",
              "positive" = "positive valence")

D_labels <- c("negative" = "negative dominance",
              "positive" = "positive dominance")

### VAD ratings of CC ----
# columns for subject and predicate
e1.VAD.CC <- e1.VAD %>% mutate(CC.subject = str_extract(CC, "(?<=the )[^ ]+"),
                               CC.predicate = case_when(
                                 grepl("popped", CC) ~ "pop",
                                 grepl("deployed", CC) ~ "deploy",
                                 grepl("loosened", CC) ~ "loosen",
                                 grepl("darkened", CC) ~ "darken",
                                 grepl("restarted", CC) ~ "restart",
                                 grepl("thickened", CC) ~ "thicken",
                                 grepl("opened", CC) ~ "open",
                                 grepl("closed", CC) ~ "close",
                                 grepl("burned", CC) ~ "burn",
                                 grepl("cracked", CC) ~ "crack",
                                 grepl("tightened", CC) ~ "tighten")) 

CC.subjects <- unique(e1.VAD.CC$CC.subject)
CC.predicates <- unique(e1.VAD.CC$CC.predicate)

# Are there any subjects or predicates in the stimuli that do not have VAD ratings?
e1.VAD.CC %>%
  filter(!(CC.subject %in% w$Word)) %>%
  distinct(CC.subject) # n/a
e1.VAD.CC %>%
  filter(!(CC.predicate %in% w$Word)) %>%
  distinct(CC.predicate) 
#   CC.predicate
# 1      thicken
# As there are no ratings for 'thicken' in Warriner et al.'s (2013) data, the ratings for 'thick'
# are used instead.

# add valence and dominance ratings as well as 'directions' to data frame
w.CC.subject <- w %>% filter(Word %in% CC.subjects) %>% 
  mutate(CC.sub.V = V.Mean.Sum,
         CC.sub.D = D.Mean.Sum,
         CC.sub.V2 = abs(V.Mean.Sum - 5)/4,
         CC.sub.D2 = abs(D.Mean.Sum - 5)/4,
         CC.sub.Vdir = case_when(V.Mean.Sum >= 5 ~ "positive",
                                 V.Mean.Sum < 5 ~ "negative"),
         CC.sub.Ddir = case_when(D.Mean.Sum >= 5 ~ "positive",
                                 D.Mean.Sum < 5 ~ "negative")) %>% 
  select(Word, CC.sub.V, CC.sub.D, CC.sub.V2, CC.sub.D2, CC.sub.Vdir, CC.sub.Ddir)

w.CC.predicate <- w %>% 
  mutate(Word = ifelse(Word == "thick", "thicken", Word)) %>% 
  filter(Word %in% CC.predicates) %>% 
  mutate(CC.pred.V = V.Mean.Sum,
         CC.pred.D = D.Mean.Sum,
         CC.pred.V2 = abs(V.Mean.Sum - 5)/4,
         CC.pred.D2 = abs(D.Mean.Sum - 5)/4,
         CC.pred.Vdir = case_when(V.Mean.Sum >= 5 ~ "positive",
                                  V.Mean.Sum < 5 ~ "negative"),
         CC.pred.Ddir = case_when(D.Mean.Sum >= 5 ~ "positive",
                                  D.Mean.Sum < 5 ~ "negative")) %>% 
  select(Word, CC.pred.V, CC.pred.D, CC.pred.V2, CC.pred.D2, CC.pred.Vdir, CC.pred.Ddir)

e1.VAD.CC <- e1.VAD.CC %>% 
  left_join(w.CC.subject, by = c("CC.subject" = "Word")) %>% 
  left_join(., w.CC.predicate, by = c("CC.predicate" = "Word")) %>% 
  relocate(starts_with("CC."), .after = CC) 


# A Acceptability ----
## A.1 correlation with projection ----
### overall ----
#### plots ----
# new data
mean.proj.e1.plus %>% 
ggplot(aes(x = Mean.Acc.MV.neg, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-acceptability-line-new.pdf", height = 6, width = 6)

# MV ratings under negation only
mean.proj.e1.plus %>% 
ggplot(aes(x = Mean.Acc.MV.neg, y = Mean.Proj.MV.neg)) +
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
ggsave("../graphs/projection-by-acceptability-line-NO-my.pdf", height = 6, width = 6)

#### linear models ----
# new
lm(Mean.Proj ~ Mean.Acc.MV.neg, mean.proj.e1.plus) %>% 
  summary()
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)  
# (Intercept)      0.25946    0.14088   1.842   0.0671 .
# Mean.Acc.MV.neg  0.02666    0.02857   0.933   0.3520  

# MV neg only
lm(Mean.Proj.MV.neg ~ Mean.Acc.MV.neg, mean.proj.e1.plus) %>% 
  summary()
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     -0.11406    0.11543  -0.988  0.32437   
# Mean.Acc.MV.neg  0.06694    0.02341   2.859  0.00472 **


### A.2 distribution ----
# how many items (in new data) with which mean acceptability rating (from MV data, negation only)?
e1 %>% 
  mutate(acc_bin = cut(e1$Mean.Acc.MV.neg, seq(1, 7, 1))) %>% 
  group_by(acc_bin) %>% 
  summarise(count = n(),
            proportion = count / nrow(e1))
#   acc_bin count proportion
#   <fct>   <int>      <dbl>
# 1 (3,4]     496     0.144 
# 2 (4,5]    1507     0.436 
# 3 (5,6]    1168     0.338 
# 4 (6,7]     283     0.0819

e1 %>% 
  mutate(acc_bin = cut(Mean.Acc.MV.neg, seq(1, 7, 1))) %>% 
  group_by(acc_bin) %>% 
  summarise(Mean.Rating = mean(rating))
#   acc_bin Mean.Rating
#   <fct>         <dbl>
# 1 (3,4]         0.372
# 2 (4,5]         0.364
# 3 (5,6]         0.402
# 4 (6,7]         0.474


## A.3 low acceptability predicates ----
# predicates with mean acceptability ratings of less than or equal to 4
mean.proj.e1.plus %>% 
  filter(Mean.Acc.MV.neg <= 4) %>% 
  arrange(Mean.Acc.MV.neg) %>% 
  reframe(predicate, Mean.Acc.MV.neg, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
#   predicate  Mean.Acc.MV.neg Mean.Proj predicateType2
#   <fct>                <dbl>     <dbl> <chr>         
# 1 gab                   3.1     0.664  nonEmoComm    
# 2 quip                  3.1     0.682  nonEmoComm    
# 3 sing                  3.11    0.179  nonEmoComm    
# 4 scribble              3.2     0.23   nonEmoComm    
# 5 underscore            3.4     0.793  nonEmoComm    
# 6 grunt                 3.5     0.496  emoComm       
# 7 maintain              3.5     0.0286 nonEmoComm    
# 8 underline             3.5     0.379  nonEmoComm    
# 9 spout                 3.6     0.352  nonEmoComm    
# 10 whimper               3.6     0.456  emoComm       
# 11 yell                  3.6     0.109  nonEmoComm    
# 12 add                   3.7     0.829  nonEmoComm    
# 13 chant                 3.7     0.136  nonEmoComm    
# 14 jest                  3.7     0.616  nonEmoComm    
# 15 lie                   3.7     0.388  nonEmoComm    
# 16 lecture               3.8     0.284  nonEmoComm    
# 17 mutter                3.8     0.409  emoComm       
# 18 shriek                3.8     0.417  emoComm       
# 19 howl                  3.9     0.463  emoComm       
# 20 mumble                3.9     0.478  nonEmoComm    
# 21 snitch                3.9     0.782  nonEmoComm    
# 22 summarize             3.9     0.0407 nonEmoComm    
# 23 chatter               4       0.448  nonEmoComm    
# 24 depict                4       0.251  nonEmoComm    
# 25 rant                  4       0.549  emoComm       
# 26 reply                 4      -0.0467 nonEmoComm    
# 27 sketch                4       0.146  nonEmoComm    
# 28 submit                4       0.138  nonEmoComm  

# What proportion of predicates within each type has a mean acceptability rating 
# of less than or equal to 4?
count <- mean.proj.e1.plus %>% 
  group_by(predicateType2) %>% 
  summarise(total.count = n())
count2 <- mean.proj.e1.plus %>% 
  filter(Mean.Acc.MV.neg <= 4) %>% 
  group_by(predicateType2) %>% 
  summarise(low.acc.count = n())
left_join(count, count2, by = "predicateType2") %>% 
  mutate(low.acc.percentage = low.acc.count * 100 / total.count)
#   predicateType2 total.count low.acc.count low.acc.percentage
#   <chr>                <int>         <int>              <dbl>
# 1 cognitive                2            NA               NA  
# 2 emoComm                 27             6               22.2
# 3 nonEmoComm             163            22               13.5

### plots ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1.plus, 
             aes(x = predicate, y = Mean.Proj, colour = "acceptable"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.e1.plus %>% filter(Mean.Acc.MV.neg <= 4), 
             aes(x = predicate, y = Mean.Proj, colour = "not acceptable neg-only"), alpha = 0.8) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection",
       colour = "Acceptability") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("acceptable", "not acceptable neg-only"),
                      labels = c("acceptable overall\nand under negation only", 
                                 "acceptable overall,\nnot acceptable under negation only"),
                      values = c("yellowgreen", "deeppink3"))
ggsave("../graphs/projection-by-predicate-acceptability-new.pdf", height = 4, width = 13)

mean.proj.excl <- e1 %>% 
  mutate(acceptability2 = case_when(Mean.Acc.MV.neg <= 4 ~ "not acceptable neg-only",
                                    TRUE ~ "acceptable")) %>%
  group_by(acceptability2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Acc = mean(Mean.Acc.MV.neg)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         acceptability2 = fct_reorder(as.factor(acceptability2), Mean.Proj)) %>% 
  print()

#   acceptability2          Mean.Proj  CILow CIHigh Mean.Acc YMin.Proj YMax.Proj
#   <fct>                       <dbl>  <dbl>  <dbl>    <dbl>     <dbl>     <dbl>
# 1 acceptable                  0.390 0.0247 0.0252     5.10     0.365     0.415
# 2 not acceptable neg-only     0.372 0.0671 0.0610     3.68     0.305     0.434

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
                              "not acceptable neg-only" = "acceptable overall,\nnot acceptable under negation only")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(limits = c("acceptable", "not acceptable neg-only"),
                      values = c("yellowgreen", "deeppink3"))
ggsave("../graphs/projection-by-acceptability-new.pdf", height = 4, width = 12)

### linear models ----
lm(Mean.Proj ~ fct_relevel(acceptability2, "not acceptable neg-only"), 
   data = mean.proj.e1.plus %>% 
     mutate(acceptability2 = case_when(Mean.Acc.MV.neg <= 4 ~ "not acceptable neg-only",
                                       TRUE ~ "acceptable"))) %>% 
  summary()
# Coefficients:
#                                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                       0.390588   0.023615   16.54   <2e-16 ***
# fct_relevel(acceptability2, "acceptable")not acceptable neg-only -0.008651   0.061840   -0.14    0.889 

#                                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                      0.381938   0.057153   6.683 2.53e-10 ***
# fct_relevel(acceptability2, "not acceptable neg-only")acceptable 0.008651   0.061840   0.140    0.889   

lmer(rating ~ fct_relevel(acceptability2, "acceptable") + (1 | participant_id), 
    data = e1 %>% mutate(acceptability2 = case_when(Mean.Acc.MV.neg <= 4 ~ "not acceptable neg-only",
                                                    TRUE ~ "acceptable"))) %>% 
   summary()
# Fixed effects:
#                                                                    Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                                         0.38972    0.02476  331.27150  15.739   <2e-16 ***
# fct_relevel(acceptability2, "acceptable")not acceptable neg-only   -0.01795    0.02908 3227.21554  -0.617    0.537   

#                                                                   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                                                      3.718e-01  3.487e-02 1.148e+03  10.662   <2e-16 ***
# fct_relevel(acceptability2, "not acceptable neg-only")acceptable 1.795e-02  2.908e-02 3.227e+03   0.617    0.537  



# B General ----
## B.0 Rating counts ----
# how many ratings per predicate?
e1 %>% group_by(predicate) %>% summarise(count = n()) %>% filter(count >= 10 & count < 20) %>% nrow()
# 0 - 9 ratings: 3 predicates
# 10 - 19 ratings: 127 predicates
# 20 - 29 ratings: 61 predicates
# 30 + ratings: 1 predicate

# which are the predicates with the lowest/highest numbers of ratings?
e1 %>% group_by(predicate) %>% summarise(count = n()) %>% arrange(count) %>% print(n = Inf)
# fewer than 10 ratings:
#   predicate   count
# 1 question        8
# 2 chronicle       9
# 3 leak            9

# more than 25 ratings:
#     predicate   count
# 184 sob            26
# 185 think          26
# 186 uncover        26
# 187 certify        27
# 188 know           27
# 189 reaffirm       27
# 190 pretend        28
# 191 grunt          29
# 192 summarize      30

## B.1 by predicate type ----
### plot ----
# calculate by-predicateType2 means
mean.proj.bt2 <- e1 %>%
  group_by(predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-predicateType2-new.pdf", height = 4, width = 5)

### linear models ----
lm(Mean.Proj ~ predicateType2, 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.58484    0.05538  10.561  < 2e-16 ***
# predicateType2nonEmoComm -0.22707    0.05979  -3.798 0.000197 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2877 on 188 degrees of freedom
# Multiple R-squared:  0.07126,	Adjusted R-squared:  0.06632 
# F-statistic: 14.42 on 1 and 188 DF,  p-value: 0.0001969

lm(rating ~ fct_relevel(predicateType2, "emoComm"), 
   e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58782    0.03096  18.985  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.23435    0.03353  -6.989 3.31e-12 ***


### distribution ----
mean.proj.e1 %>% 
  group_by(predicateType2) %>% 
  distinct(predicate) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          2
# 2 emoComm           27
# 3 nonEmoComm       163


## B.2 by predicate ----
### B.2.1 New data ----
##### think / know labels ----
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
   geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
              aes(x = predicate, y = Mean.Proj, colour = "think")) +
   geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
              aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
   geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                    aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                    min.segment.length = 0,
                    nudge_x = -0.2, nudge_y = -0.2,
                    colour = "deeppink") +
   geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                    aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                    min.segment.length = 0,
                    nudge_x = 0.2, nudge_y = -0.2,
                    colour = "orangered3") +
   theme(legend.position = "none",
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         panel.grid.major.x = element_blank()) +
   labs(x = "Predicate",
        y = "Mean projection rating") +
   scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                       values = c("deepskyblue2", "blue", "deeppink", "orangered3"))
ggsave("../graphs/projection-by-communicative-new.pdf", height = 4, width = 13)

##### extreme predicate labels ----
# how many highlighted predicates on either side?
mean.proj.e1 %>% 
  filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1) %>% 
  nrow() # 18 - without 'know', 17 predicates have projection ratings at least as high as 'know' minus 0.1.
mean.proj.e1 %>% 
  filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2) %>% 
  nrow() # 46 - without 'know', 45 predicates have projection ratings at least as high as 'know' minus 0.1.

###### 17-17 ----
# labels for the 17 most and least projective communicatives
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1, Mean.Proj, n = 18),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1), 
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.e1, Mean.Proj, n = 18) %>% filter(predicate != "think"),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% 
                     filter(predicate != "know") %>% 
                     filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-labels-17-17.pdf", height = 4, width = 13)

###### 17pos-17 ----
# labels for the 17 most projective communicatives and 17 least projective ones with positive projection rating
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0), Mean.Proj, n = 17),
            aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1), 
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0), Mean.Proj, n = 17),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% 
                     filter(predicate != "know") %>% 
                     filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-labels-17pos-17.pdf", height = 4, width = 13)

###### 45-45 ----
# labels for the 45 most and least projective communicatives
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1, Mean.Proj, n = 46),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2), 
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.e1, Mean.Proj, n = 46) %>% filter(predicate != "think"),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% 
                     filter(predicate != "know") %>% 
                     filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-labels-45-45.pdf", height = 4, width = 13)

###### 45pos-45 ----
# 45 most projective communicatives and 45 least projective ones with positive projection rating
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0 & predicate != "say"), Mean.Proj, n = 45),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2), 
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0 & predicate != "say"), Mean.Proj, n = 45),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.5,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% 
                     filter(predicate != "know") %>% 
                     filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))
ggsave("../graphs/projection-by-communicative-labels-45pos-45.pdf", height = 4, width = 13)

##### emoComm labels ----
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "green4", "blue", "deeppink", "orangered3"))
ggsave("../graphs/projection-by-communicative-emo-labels.pdf", height = 4, width = 13)

### B.2.2 New data - MV comparison ----
#### think / know labels ----
##### MV overall ----
projNew <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered3"))

commProjMV <- 
  ggplot(mean.proj.MV, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV %>% filter(predicate == "say"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV %>% filter(predicate == "think"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV %>% filter(predicate == "know"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MV data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered3"))

# combine new data and MV plots
projNew / commProjMV + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-comparison.pdf", height = 6, width = 13)

##### MV negation only ----
commProjMVneg <- 
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MV data",
       x = "Predicate",
       y = "Mean projection rating (under negation)") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered3"))

# combine new data and MV plots 
(projNew + labs(y = "Mean projection rating (under negation)")) / commProjMVneg + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-comparison-neg.pdf", height = 6, width = 13)

#### 1SD labels ----
mean(mean.proj.e1$Mean.Proj) # 0.3893268
sd(mean.proj.e1$Mean.Proj) # 0.3016475

# predicates with a mean projection rating at least 1SD from the mean
# - 1SD
minusSD <- mean.proj.e1.plus %>% 
  filter(Mean.Proj <= mean(mean.proj.e1.plus$Mean.Proj) - sd(mean.proj.e1.plus$Mean.Proj)) %>% 
  nrow() # 34 (includes "think" - 33 communicatives)
# + 1SD
plusSD <- mean.proj.e1.plus %>% 
  filter(Mean.Proj >= mean(mean.proj.e1.plus$Mean.Proj) + sd(mean.proj.e1.plus$Mean.Proj)) %>% 
  nrow() # 35 (includes "know" - 34 communicatives)
# MV (under negation only):
# - 1SD
minusSDMVneg <- mean.proj.e1.plus %>% 
  filter(Mean.Proj.MV.neg <= mean(mean.proj.e1.plus$Mean.Proj.MV.neg) - sd(mean.proj.e1.plus$Mean.Proj.MV.neg)) %>% 
  nrow() # 24 (includes "think" - 23 communicatives)
plusSDMVneg <- mean.proj.e1.plus %>% 
  filter(Mean.Proj.MV.neg >= mean(mean.proj.e1.plus$Mean.Proj.MV.neg) + sd(mean.proj.e1.plus$Mean.Proj.MV.neg)) %>% 
  nrow() # 32 (includes "know" - 31 communicatives)

# labels for extremes: at least +/- standard deviation from the mean of mean projection ratings
projNewSDLabels <-  
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1, Mean.Proj, n = minusSD),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = slice_max(mean.proj.e1, Mean.Proj, n = plusSD),
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.e1, Mean.Proj, n = minusSD) %>% 
                     filter(predicate != "think"),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = slice_max(mean.proj.e1, Mean.Proj, n = plusSD) %>% 
                     filter(predicate != "know"),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))

commProjMVnegSDLabels <- 
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.MV.neg, Mean.Proj, n = minusSDMVneg),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = slice_max(mean.proj.MV.neg, Mean.Proj, n = plusSDMVneg),
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.MV.neg, Mean.Proj, n = minusSDMVneg) %>% 
                     filter(predicate != "think"),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = slice_max(mean.proj.MV.neg, Mean.Proj, n = plusSDMVneg) %>% 
                     filter(predicate != "know"),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MV data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))

projNewSDLabels / commProjMVnegSDLabels + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-comparison-labels-SD.pdf", height = 7, width = 13)


#### 52-49 labels -----
# labels for the most and least projective communicatives based on the distribution in the MV dataset
slice_min(mean.proj.MV.neg, Mean.Proj, n = 34) %>% nrow() # 53 (52 without "think")
slice_max(mean.proj.MV.neg, Mean.Proj, n = 35) %>% nrow() # 50 (49 without "know")
# Because there are 10 ratings per predicate per embedding environment in the MV dataset, many of the 
# mean projection ratings considered here are multiples of 0.1. Selecting the lowest and highest rating 
# predicates (= 1SD from mean in new data), results in 52 and 49 communicatives, respectively. The
# plots below therefore have labels for these numbers of predicates with "extreme" ratings.
projNew52 <-   
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1, Mean.Proj, n = 53),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = slice_max(mean.proj.e1, Mean.Proj, n = 50),
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.e1, Mean.Proj, n = 53) %>% 
                     filter(predicate != "think"),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = slice_max(mean.proj.e1, Mean.Proj, n = 50) %>% 
                     filter(predicate != "know"),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))

projNewMVneg52 <- 
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.MV.neg, Mean.Proj, n = 53),
             aes(x = predicate, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = slice_max(mean.proj.MV.neg, Mean.Proj, n = 50),
             aes(x = predicate, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.MV.neg, Mean.Proj, n = 53) %>% 
                     filter(predicate != "think"),
                   aes(label = predicate),
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "hotpink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = slice_max(mean.proj.MV.neg, Mean.Proj, n = 50) %>% 
                     filter(predicate != "know"),
                   aes(label = predicate), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "orangered") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MV data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "blue", "hotpink", "deeppink", "orangered", "orangered3"))

projNew52 / projNewMVneg52 + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-comparison-labels-52.pdf", height = 8, width = 13)

#### emoComm labels ----
##### general ----
###### plot ----
projNewEmo <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "green4", "blue", "deeppink", "orangered3"))

projMVnegEmo <-   
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MV data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "green4", "blue", "deeppink", "orangered3"))

projNewEmo / projMVnegEmo + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-emo-labels-comparison.pdf", height = 7, width = 13)

###### linear models ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.e1) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58484    0.05630  10.388  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.26280    0.21439  -1.226 0.221799    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.22707    0.06079  -3.736 0.000248 ***

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.MV.neg) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04768   7.069 2.93e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.03704    0.18156  -0.204  0.83857    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14678    0.05148  -2.852  0.00484 ** 

# The emoComm/nonEmoComm distinction is highly significant in both datasets.

##### type of emoComm: manner - attitude labels ----
###### plot ----
projNewEmoManAtt <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(colour = "deepskyblue2", alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj), colour = "blue") +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj), colour = "deeppink") +
  geom_point(data = mean.proj.e1 %>% filter(emoCommType == "manner"), 
             aes(x = predicate, y = Mean.Proj), colour = "green4", alpha = 0.9) +
  geom_point(data = mean.proj.e1 %>% filter(emoCommType == "attitude"), 
             aes(x = predicate, y = Mean.Proj), colour = "green4", alpha = 0.9) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj), colour = "orangered3") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(emoCommType == "manner"),
                   aes(label = predicate, fill = "manner"), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.e1 %>% filter(emoCommType == "attitude"),
                   aes(label = predicate, fill = "attitude"), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating",
       fill = "Type of communicative with emotion entailment") + 
  scale_y_continuous(limits = c(-1, 1.1), breaks = c(-1, 0, 1)) +
  scale_fill_manual(limits = c("attitude", "manner"),
                    labels = c("with attitude entailment", "with manner entailment"),
                    values = c("yellow2", "palegreen"))

projMVnegEmoManAtt <- 
  ggplot(mean.proj.MV.neg, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(colour = "deepskyblue2", alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj), colour = "blue") +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj), colour = "deeppink") +
  geom_point(data = mean.proj.MV.neg %>% filter(emoCommType == "manner"), 
             aes(x = predicate, y = Mean.Proj), colour = "green4", alpha = 0.9) +
  geom_point(data = mean.proj.MV.neg %>% filter(emoCommType == "attitude"), 
             aes(x = predicate, y = Mean.Proj), colour = "green4", alpha = 0.9) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj), colour = "orangered3") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(emoCommType == "manner"),
                   aes(label = predicate, fill = "manner"), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.7,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(emoCommType == "attitude"),
                   aes(label = predicate, fill = "attitude"), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MV data",
       x = "Predicate",
       y = "Mean projection rating",
       fill = "Type of communicative with emotion entailment") + 
  guides(colour = "none") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_fill_manual(limits = c("attitude", "manner"),
                    labels = c("with attitude entailment", "with manner entailment"),
                    values = c("yellow2", "palegreen"))

legend <- get_legend(projNewEmoManAtt)
projNewEmoManAtt <- projNewEmoManAtt + theme(legend.position = "none")
wrap_elements(legend) / projNewEmoManAtt / projMVnegEmoManAtt + 
  plot_layout(axis_titles = "collect", heights = c(0.1, 1, 1))
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude-comparison.pdf", height = 7, width = 13)

###### linear models ----
lm(Mean.Proj ~ fct_relevel(emoCommType, "attitude"), 
   mean.proj.e1 %>% filter(predicateType2 == "emoComm")) %>% 
  summary()
# Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                 0.64003    0.05252  12.187 5.16e-12 ***
# fct_relevel(emoCommType, "attitude")manner -0.09313    0.06822  -1.365    0.184 

lm(Mean.Proj ~ fct_relevel(emoCommType, "attitude"), 
   mean.proj.MV.neg %>% filter(predicateType2 == "emoComm")) %>% 
  summary()
# Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                 0.37273    0.06821   5.465 1.13e-05 ***
# fct_relevel(emoCommType, "attitude")manner -0.06023    0.08860  -0.680    0.503  

# The type of emoComm distinction is not significant in either of the datasets.


# C Communicatives ----
## C.1 overall ----
### C.1.1 by-predicateType ----
#### plot ----
# calculate by-predicateType means
mean.proj.comm.bt <-  e1.comm %>%
  group_by(commType) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-predicateType-commType-new.pdf", height = 4, width = 5)

# faceted by predicate type
mean.proj.comm.bt2 <-  e1.comm %>%
  group_by(commType, predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-predicateType-commType-faceted-new.pdf", height = 4, width = 8)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(commType, "discourse participation"), 
   mean.proj.e1.comm) %>% 
  summary()
# Coefficients:
#                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                     0.407833   0.030285  13.466   <2e-16 ***
# fct_relevel(commType, "discourse participation")pure           -0.002883   0.045428  -0.063    0.949    
# fct_relevel(commType, "discourse participation")state changing -0.166449   0.074183  -2.244    0.026 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2952 on 187 degrees of freedom
# Multiple R-squared:  0.02785,	Adjusted R-squared:  0.01746 
# F-statistic: 2.679 on 2 and 187 DF,  p-value: 0.07127

lm(Mean.Proj ~ fct_relevel(commType, "pure") * fct_relevel(predicateType2, "nonEmoComm"), 
   mean.proj.e1.comm) %>% 
  summary()
# emoComm     significance of difference between commTypes
# dp - pure   n.s. (this looks significant in the plot)
# nonEmoComm
# sc - dp     *
# dp - pure   n.s.
# sc - pure   n.s. ?! (this looks significant in the plot)

### C.1.2 distribution ----
# How many of which type of communicative predicate?
mean.proj.e1.comm %>%
  select(commType, predicate) %>% 
  unique() %>% 
  group_by(commType) %>% 
  count()
#   commType                    n
#   <chr>                   <int>
# 1 discourse participation    95
# 2 pure                       76
# 3 state changing             19

## C.3 say verbs (Grimshaw 2015) ----
### C.3.1 types of communicatives ----
#### distributions ----
e1 %>% 
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  distinct(predicate) %>% 
  count()
#   sayVerb     n
#   <chr>   <int>
# 1 no         84
# 2 yes       106
# Of the 190 communicatives included in this investigation, 106 are say-predicates.

e1 %>% 
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, sayVerb) %>% 
  distinct(predicate) %>% 
  count()
#   predicateType2 sayVerb     n
#   <chr>          <chr>   <int>
# 1 emoComm        no          4
# 2 emoComm        yes        23
# 3 nonEmoComm     no         80
# 4 nonEmoComm     yes        83

#### plots ----
e1 %>%
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
ggplot(aes(x = sayVerb, y = Mean.Proj)) +
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
       y = "Mean projection rating") + 
  scale_x_discrete(labels = c("non-say verb", "say verb")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-communication-type-new.pdf", height = 4, width = 4)

# by predicate type by say/non-say
e1 %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, sayVerb) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-communication-type-predType2-new.pdf", height = 4, width = 5)

# by say/non-say by predicate type
e1 %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, sayVerb) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-communication-type-predType2-alt-new.pdf", height = 4, width = 5.5)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(sayVerb, "yes"), 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.34454    0.02857  12.060   <2e-16 ***
# fct_relevel(sayVerb, "yes")no  0.10290    0.04297   2.395   0.0176 *  

lm(rating ~ sayVerb, data = e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.43247    0.01797  24.059  < 2e-16 ***
# sayVerbyes  -0.07969    0.02406  -3.312 0.000937 ***

# by predicate type
lm(Mean.Proj ~ fct_relevel(sayVerb, "no") * fct_relevel(predicateType2, "emoComm"), 
  mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.562781   0.058364   9.643  < 2e-16 ***
# fct_relevel(sayVerb, "yes")no                                                   0.148875   0.151633   0.982    0.327    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.278717   0.065956  -4.226 3.73e-05 ***
# fct_relevel(sayVerb, "yes")no:fct_relevel(predicateType2, "emoComm")nonEmoComm  0.001295   0.157848   0.008    0.993    

# Coefficients:
#                                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.284065   0.030723   9.246  < 2e-16 ***
# fct_relevel(sayVerb, "yes")no                                                   0.150170   0.043855   3.424 0.000758 ***
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                0.278717   0.065956   4.226 3.73e-05 ***
# fct_relevel(sayVerb, "yes")no:fct_relevel(predicateType2, "nonEmoComm")emoComm -0.001295   0.157848  -0.008 0.993464   

# Coefficients:
#                                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.711656   0.139951   5.085 8.93e-07 ***
# fct_relevel(sayVerb, "no")yes                                                  -0.148875   0.151633  -0.982   0.3275    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.277422   0.143407  -1.935   0.0546 .  
# fct_relevel(sayVerb, "no")yes:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.001295   0.157848  -0.008   0.9935 

# The say-verb / non-say-verb distinction is highly significant for the nonEmoComms.
# The emoComm/nonEmoComm distinction seems only significant for say verbs. The model based on the raw
# data (below) and the plot (above) indicate that it is highly significant for non-say-verbs as well.

mean.proj.comm %>% 
  filter(predicateType == "communicative") %>% 
  count(sayVerb, predicateType2)
#   sayVerb predicateType2     n
#   <chr>   <chr>          <int>
# 1 no      emoComm            4
# 2 no      nonEmoComm        80
# 3 yes     emoComm           23
# 4 yes     nonEmoComm        83

# That there are only 4 emoComms vs 80 nonEmoComms in the non-say-verb group explains why the difference
# observed in the plot is not significant. In the MV dataset, this is also not significant.

lm(rating ~ fct_relevel(sayVerb, "yes") * fct_relevel(predicateType2, "nonEmoComm"), 
   data = e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.28747    0.01807  15.908  < 2e-16 ***
# fct_relevel(sayVerb, "yes")no                                                   0.13283    0.02564   5.182 2.33e-07 ***
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                0.28237    0.03758   7.515 7.25e-14 ***
# fct_relevel(sayVerb, "yes")no:fct_relevel(predicateType2, "nonEmoComm")emoComm  0.01249    0.09709   0.129    0.898    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6903 on 3397 degrees of freedom
# Multiple R-squared:  0.02259,	Adjusted R-squared:  0.02172 
# F-statistic: 26.17 on 3 and 3397 DF,  p-value: < 2.2e-16


### C.3.2 types of say verbs ----
#### distributions ----
e1 %>% 
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   sayVerbType             n
#   <chr>               <int>
# 1 discourse role verb    51
# 2 mode verb              54
# 3 say                     1

e1 %>% 
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(predicateType2, sayVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   predicateType2 sayVerbType             n
#   <chr>          <chr>               <int>
# 1 emoComm        mode verb              23
# 2 nonEmoComm     discourse role verb    51
# 3 nonEmoComm     mode verb              31
# 4 nonEmoComm     say                     1

#### plots ----
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
ggplot(aes(x = sayVerbType, y = Mean.Proj)) +
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
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-sayverb-type-new.pdf", height = 4, width = 5)

e1 %>%
  filter(predicateType == "communicative") %>%  
  group_by(sayVerb, sayVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
ggplot(aes(x = sayVerb, y = Mean.Proj, colour = sayVerbType)) +
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
  labs(x = "Communicative type",
       y = "Mean projection rating",
       colour = "Say verb type") + 
  scale_x_discrete(labels = c("non-say verb", "say verb")) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("purple","deeppink", "orange3", "grey50"))
ggsave("../graphs/projection-by-sayverb-type2-new.pdf", height = 4, width = 6)

# by predicate type by say verb type
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>%  
  group_by(predicateType2, sayVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-sayverb-type-predType2-new.pdf", height = 4, width = 5)

# by say verb type by predicate type
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>%  
  group_by(predicateType2, sayVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-sayverb-type-predType2-alt-new.pdf", height = 4, width = 5.5)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(sayVerbType, "mode verb"), data = mean.proj.e1) %>% 
  summary()
# Coefficients:
#                                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.234007   0.036615   6.391 4.87e-09 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.217156   0.051057   4.253 4.65e-05 ***
# fct_relevel(sayVerbType, "discourse role verb")say       -0.009841   0.264033  -0.037     0.97  

# Coefficients:
#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.45116    0.03558  12.679  < 2e-16 ***
# fct_relevel(sayVerbType, "mode verb")discourse role verb -0.21716    0.05106  -4.253 4.65e-05 ***
# fct_relevel(sayVerbType, "mode verb")say                 -0.22700    0.26389  -0.860    0.392  

# The sayVerbType distinction is highly significant.

lm(Mean.Proj ~ fct_relevel(sayVerbType, "mode verb") * fct_relevel(predicateType2, "nonEmoComm"),
   data = mean.proj.e1 %>% filter(predicate != "say")) %>% 
  summary()
# Coefficients: (1 not defined because of singularities)
#                                                                                                           Estimate Std. Error t value
# (Intercept)                                                                                                0.23401    0.03547   6.598
# fct_relevel(sayVerbType, "discourse role verb")mode verb                                                   0.13434    0.05768   2.329
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                                           0.19443    0.06970   2.789
# fct_relevel(sayVerbType, "discourse role verb")mode verb:fct_relevel(predicateType2, "nonEmoComm")emoComm       NA         NA      NA
# Pr(>|t|)    
# (Intercept)                                                                                               1.89e-09 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb                                                    0.0218 *  
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                                            0.0063 ** 
# fct_relevel(sayVerbType, "discourse role verb")mode verb:fct_relevel(predicateType2, "nonEmoComm")emoComm       NA  

# the mode / discourse role verb distinction within the emoComms is significant, but all emoComms 
# are mode verbs!
# the emoComm / nonEmoComm distinction within mode verbs is significant. In the discourse role verbs
# group there are no emoComms.

lm(rating ~ fct_relevel(sayVerbType, "discourse role verb"), 
   data = e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.229945   0.022684  10.137  < 2e-16 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.243256   0.031710   7.671  2.7e-14 ***
# fct_relevel(sayVerbType, "discourse role verb")say       -0.005779   0.141887  -0.041    0.968   

### C.3.3 types of mode verbs ----
#### distributions ----
e1 %>% 
  filter(mode_verb == "yes") %>%   
  group_by(modeVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   modeVerbType          n
# #   <chr>             <int>
# 1 say-by-means         41
# 2 say-with-attitude    13

e1 %>% 
  filter(mode_verb == "yes") %>%   
  group_by(predicateType2, modeVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   predicateType2 modeVerbType          n
#   <chr>          <chr>             <int>
# 1 emoComm        say-by-means         16
# 2 emoComm        say-with-attitude     7
# 3 nonEmoComm     say-by-means         25
# 4 nonEmoComm     say-with-attitude     6

#### plots ----
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(sayVerbType, modeVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = sayVerbType, y = Mean.Proj, colour = modeVerbType)) +
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
       colour = "Mode verb type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-new.pdf", height = 4, width = 6)

# by predicate type by mode verb type
e1 %>%
  filter(predicateType == "communicative" & sayVerbType == "mode verb") %>%  
  group_by(modeVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-modeverb-type-predType-new.pdf", height = 4, width = 5)

# by mode verb type by predicate type
e1 %>%
  filter(predicateType == "communicative" & sayVerbType == "mode verb") %>%  
  group_by(modeVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-modeverb-type-predType-alt-new.pdf", height = 4, width = 5.5)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(modeVerbType, "say-with-attitude"), data = mean.proj.e1) %>% 
  summary()
# Coefficients:
#                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                 0.40738    0.03150  12.934  < 2e-16 ***
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude  0.18187    0.06419   2.833  0.00654 ** 

#                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                 0.58925    0.05593  10.535 1.67e-14 ***
# fct_relevel(modeVerbType, "say-with-attitude")say-by-means -0.18187    0.06419  -2.833  0.00654 ** 

lm(Mean.Proj ~ fct_relevel(modeVerbType, "say-by-means") *  fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1) %>% 
  summary()
# Pr(>|t|)    
# (Intercept)                                                                                                 1.21e-15 ***
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude                                                   0.17936    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                             0.00146 ** 
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude:fct_relevel(predicateType2, "emoComm")nonEmoComm  0.47690  

# For emoComms, the s-b-m / s-w-a distinction is not significant.
# For nonEmoComms, the s-b-m / s-w-a distinction is significant.
# For s-b-m predicates the emoComm/nonEmoComm distinction is significant.
# For s-w-a predicates the emoComm/nonEmoComm distinction is not significant.


lm(rating ~ modeVerbType, data = e1) %>% 
  summary()
# (Intercept)                    0.42715    0.02442  17.494  < 2e-16 ***
# modeVerbTypesay-with-attitude  0.17734    0.04792   3.701 0.000227 ***

### C.3.4 types of say-by-means verbs ----
#### distributions ----
e1 %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                  14
# 2 manner                22
# 3 sound                  5

e1 %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(predicateType2, sayByMeansVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   predicateType2 sayByMeansVerbType     n
#   <chr>          <chr>              <int>
# 1 emoComm        manner                12
# 2 emoComm        sound                  4
# 3 nonEmoComm     form                  14
# 4 nonEmoComm     manner                10
# 5 nonEmoComm     sound                  1

#### plots ----
e1 %>%
  filter(sayVerbType == "mode verb" | predicate == "say") %>%  
  group_by(modeVerbType, sayByMeansVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = modeVerbType, y = Mean.Proj, colour = sayByMeansVerbType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(hjust = 0.31, vjust = -0.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Mode verb type",
       y = "Mean projection rating",
       colour = "Say-by-means verb type") + 
  scale_x_discrete(labels = c("say-by-means", "say-with-attitude", "say")) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2", "grey50"))
ggsave("../graphs/projection-by-saybymeansverb-type-new.pdf", height = 4, width = 6)

# by predicate type by say-by-means verb type
e1 %>%
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-saybymeansverb-type-predType2-new.pdf", height = 4, width = 5)

# by say-by-means verb type by predicate type 
e1 %>%
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType, predicateType2) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-saybymeansverb-type-predType2-alt-new.pdf", height = 4, width = 5.5)

#### linear models ----
lm(Mean.Proj ~ fct_relevel(sayByMeansVerbType, "sound"), data = mean.proj.e1) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                    0.33791    0.05182   6.521  1.1e-07 ***
# fct_relevel(sayByMeansVerbType, "form")manner  0.10737    0.06629   1.620    0.114    
# fct_relevel(sayByMeansVerbType, "form")sound   0.09720    0.10102   0.962    0.342  

#                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                     0.44528    0.04134  10.771 4.16e-13 ***
# fct_relevel(sayByMeansVerbType, "manner")form  -0.10737    0.06629  -1.620    0.114    
# fct_relevel(sayByMeansVerbType, "manner")sound -0.01017    0.09606  -0.106    0.916   

#                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                     0.43511    0.08671   5.018 1.26e-05 ***
# fct_relevel(sayByMeansVerbType, "sound")form   -0.09720    0.10102  -0.962    0.342    
# fct_relevel(sayByMeansVerbType, "sound")manner  0.01017    0.09606   0.106    0.916      

# Multiple R-squared:  0.06725,	Adjusted R-squared:  0.01816 

lm(Mean.Proj ~ fct_relevel(sayByMeansVerbType, "manner") * fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1) %>% 
  summary()
# Coefficients: (1 not defined because of singularities)
#                                                                                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                                      0.54871    0.05106  10.747 8.75e-13 ***
# fct_relevel(sayByMeansVerbType, "manner")form                                                    0.01674    0.07323   0.229  0.82047    
# fct_relevel(sayByMeansVerbType, "manner")sound                                                  -0.08182    0.10211  -0.801  0.42825    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                -0.22753    0.07573  -3.005  0.00482 ** 
# fct_relevel(sayByMeansVerbType, "manner")form:fct_relevel(predicateType2, "emoComm")nonEmoComm        NA         NA      NA       NA    
# fct_relevel(sayByMeansVerbType, "manner")sound:fct_relevel(predicateType2, "emoComm")nonEmoComm  0.06864    0.21175   0.324  0.74768   

# Multiple R-squared:  0.2648,	Adjusted R-squared:  0.1831 

# For both emoComms and nonEmoComms, the manner/sound distinction is not significant.
# For manner, the emoComm/nonEmoComm distinction is significant.
# For sound, the emoComm/nonEmoComm distinction is not significant. There are no form emoComms.

lm(rating ~ fct_relevel(sayByMeansVerbType, "form"), data = e1) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                                    0.35449    0.04447   7.972 6.26e-15 ***
# fct_relevel(sayByMeansVerbType, "form")manner  0.10984    0.05613   1.957   0.0508 .  
# fct_relevel(sayByMeansVerbType, "form")sound   0.09400    0.07858   1.196   0.2320 

# (Intercept)                                     0.46433    0.03426  13.553   <2e-16 ***
# fct_relevel(sayByMeansVerbType, "manner")form  -0.10984    0.05613  -1.957   0.0508 .  
# fct_relevel(sayByMeansVerbType, "manner")sound -0.01584    0.07328  -0.216   0.8290  

# (Intercept)                                     0.44849    0.06478   6.923 9.94e-12 ***
# fct_relevel(sayByMeansVerbType, "sound")form   -0.09400    0.07858  -1.196    0.232    
# fct_relevel(sayByMeansVerbType, "sound")manner  0.01584    0.07328   0.216    0.829    

### C.3.5 overall ----
#### distribution ----
mean.proj.e1 %>% 
  group_by(predicateType2, commType, sayVerbType, modeVerbType) %>% 
  count()
# predicateType2 commType                sayVerbType         modeVerbType          n
# <chr>          <chr>                   <chr>               <chr>             <int>
# 1 cognitive      NA                      NA                  NA                    2
# 2 emoComm        discourse participation mode verb           say-with-attitude     4
# 3 emoComm        discourse participation NA                  NA                    2
# 4 emoComm        pure                    mode verb           say-by-means         16
# 5 emoComm        pure                    mode verb           say-with-attitude     3
# 6 emoComm        pure                    NA                  NA                    2
# 7 nonEmoComm     discourse participation discourse role verb NA                   41
# 8 nonEmoComm     discourse participation mode verb           say-with-attitude     1
# 9 nonEmoComm     discourse participation NA                  NA                   47
# 10 nonEmoComm     pure                    discourse role verb NA                    5
# 11 nonEmoComm     pure                    mode verb           say-by-means         25
# 12 nonEmoComm     pure                    mode verb           say-with-attitude     5
# 13 nonEmoComm     pure                    say                 NA                    1
# 14 nonEmoComm     pure                    NA                  NA                   19
# 15 nonEmoComm     state changing          discourse role verb NA                    5
# 16 nonEmoComm     state changing          NA                  NA                   14

#### plot ----
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(sayVerbType, modeVerbType, sayByMeansVerbType) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  ggplot(aes(x = sayVerbType, y = Mean.Proj, colour = modeVerbType, 
             shape = ifelse(is.na(sayByMeansVerbType), "NA", sayByMeansVerbType))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.3), size = 2) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.3)) +
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.spacing.y = unit(-0.2, "cm"),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 11, 5.5), "pt")) +
  labs(x = "Say verb type",
       y = "Mean projection rating",
       colour = "Mode verb type",
       shape = "Say-by-means verb type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3", "blue", "grey50")) + 
  scale_shape_manual(breaks = c("form", "manner", "sound", "NA"),
                     values = c(15, 19, 17, 18))  
ggsave("../graphs/projection-by-saybymeansverb-type2-new.pdf", height = 4, width = 6)


# D Differences ----
## D.1 by predicate ----
### D.1.1 overall ----
# new data vs MV data with all embedding environments

# mean difference in projection ratings:
mean(mean.proj.e1.plus$Mean.Proj.diff) # 0.07843124

mean.proj.e1.plus %>% count(Mean.Proj.diff > 0)
#   `Mean.Proj.diff > 0`     n
#   <lgl>                <int>
# 1 FALSE                   74
# 2 TRUE                   118
# 118 (61%) of the predicates investigated here have a higher projection rating in the new dataset
# than in the MV dataset across embedding environments.

#### MV vs new data ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.MV, alpha = 0.5,
             aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.MV, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  geom_point(data = mean.proj.e1, alpha = 0.5,
             aes(x = predicate, y = Mean.Proj, 
                 colour = "new data"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = mean.proj.e1, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "new data"), 
                width = 0, position = position_nudge(x = -0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Dataset") +
  scale_x_discrete(labels = c("think", "say", "know"), breaks = c("think", "say", "know")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("chocolate2", "deepskyblue"))
ggsave("../graphs/projection-by-communicative-comparison-MV-new.pdf", height = 7, width = 13)

#### new data vs MV ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, alpha = 0.5,
             aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj, 
                 colour = "new data"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = mean.proj.e1, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "new data"), 
                width = 0, position = position_nudge(x = -0.07)) +
  geom_point(data = mean.proj.MV, alpha = 0.5,
             aes(x = predicate, y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.MV, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Dataset") +
  scale_x_discrete(labels = c("think", "say", "know"), breaks = c("think", "say", "know")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("chocolate2", "deepskyblue"))
ggsave("../graphs/projection-by-communicative-comparison-new-MV.pdf", height = 7, width = 13)

#### alphabetical order ----
mean.proj.e1.plus %>% 
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5,
             aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj.MV, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(alpha = 0.5,
                aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj.MV, 
                    ymin = YMin.Proj.MV, ymax = YMax.Proj.MV, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  geom_point(alpha = 0.5,
             aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj, 
                 colour = "new data"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(alpha = 0.5,
                aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "new data"), 
                width = 0, position = position_nudge(x = -0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Dataset") +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("chocolate2", "deepskyblue")) +
  facet_wrap(~ group, scales = "free_x", ncol = 1)
ggsave("../graphs/projection-by-communicative-comparison-alphabetical.pdf", height = 8, width = 13)

#### with ratings count ----
mean.proj.e1.plus %>%  
  arrange(Mean.Proj.diff) %>%
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>%
  ggplot(aes(x = reorder(predicate, Mean.Proj.diff), y = Mean.Proj.diff, fill = ratingsCount)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate", 
       y = "Difference between mean projection ratings (new - MV)", 
       fill = "Ratings Count") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  facet_wrap(~ group, scales = "free_x", ncol = 1)
ggsave("../graphs/projection-by-communicative-differences.pdf", height = 7, width = 13)

# with emoComms highlighted
mean.proj.e1.plus %>%  
  arrange(Mean.Proj.diff) %>%
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>%
  ggplot(aes(x = reorder(predicate, Mean.Proj.diff), y = Mean.Proj.diff, fill = predicateType2)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate", 
       y = "Difference between mean projection ratings (new - MV)", 
       fill = "Predicate Type") +
  scale_fill_manual(values = c("grey60", "green3", "deepskyblue2"),
                    labels = c("cogntive", "communicative with\nemotion entailment", 
                               "communicative without\nemotion entailment")) +
  facet_wrap(~ group, scales = "free_x", ncol = 1)
ggsave("../graphs/projection-by-communicative-differences-emo.pdf", height = 7, width = 13)

### D.1.2 under negation only ----
# new data vs MV data with negation as embedding environment only

# mean difference in projection ratings:
mean(mean.proj.e1.plus$Mean.Proj.diff.neg) # 0.1772898

mean.proj.e1.plus %>% count(Mean.Proj.diff.neg > 0)
#   `Mean.Proj.diff.neg > 0`     n
#   <lgl>                    <int>
# 1 FALSE                       41
# 2 TRUE                       151
# 151 (79%) of the predicates investigated here have a higher projection rating in the new dataset
# than in the MV dataset within the negation-only embedding environments.

#### MV vs new data ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.MV.neg, alpha = 0.5,
             aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.MV.neg, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  geom_point(data = mean.proj.e1, alpha = 0.5,
             aes(x = predicate, y = Mean.Proj, 
                 colour = "new data"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = mean.proj.e1, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "new data"), 
                width = 0, position = position_nudge(x = -0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating (under negation)",
       colour = "Dataset") +
  scale_x_discrete(labels = c("think", "say", "know"), breaks = c("think", "say", "know")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("chocolate2", "deepskyblue"))
ggsave("../graphs/projection-by-communicative-comparison-MV-new-neg.pdf", height = 7, width = 13)

#### new data vs MV ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, alpha = 0.5,
             aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj, 
                 colour = "new data"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = mean.proj.e1, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "new data"), 
                width = 0, position = position_nudge(x = -0.07)) +
  geom_point(data = mean.proj.MV.neg, alpha = 0.5,
             aes(x = predicate, y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.MV.neg, alpha = 0.5,
                aes(x = predicate, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating (under negation)",
       colour = "Dataset") +
  scale_x_discrete(labels = c("think", "say", "know"), breaks = c("think", "say", "know")) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("chocolate2", "deepskyblue"))
ggsave("../graphs/projection-by-communicative-comparison-new-MV-neg.pdf", height = 7, width = 13)

#### alphabetical order ----
mean.proj.e1.plus %>% 
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5,
             aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj.MV.neg, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(alpha = 0.5,
                aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj.MV.neg, 
                    ymin = YMin.Proj.MV.neg, ymax = YMax.Proj.MV.neg, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  geom_point(alpha = 0.5,
             aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj, 
                 colour = "new data"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(alpha = 0.5,
                aes(x = factor(predicate, levels = unique(predicate)), y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "new data"), 
                width = 0, position = position_nudge(x = -0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate",
       y = "Mean projection rating (under negation)",
       colour = "Dataset") +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("chocolate2", "deepskyblue")) +
  facet_wrap(~ group, scales = "free_x", ncol = 1)
ggsave("../graphs/projection-by-communicative-comparison-alphabetical-neg.pdf", height = 8, width = 13)

#### with ratings count ----
mean.proj.e1.plus %>%  
  arrange(Mean.Proj.diff.neg) %>%
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>%
  ggplot(aes(x = reorder(predicate, Mean.Proj.diff.neg), y = Mean.Proj.diff.neg, fill = ratingsCount)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate", 
       y = "Difference between mean projection ratings (new - MV [neg only])", 
       fill = "Ratings Count") +
  scale_y_continuous(limits = c(-0.5, 0.75), breaks = c(-0.5, 0, 0.5)) +
  facet_wrap(~ group, scales = "free_x", ncol = 1)
ggsave("../graphs/projection-by-communicative-differences-neg.pdf", height = 7, width = 13)

# with emoComms highlighted
mean.proj.e1.plus %>%  
  arrange(Mean.Proj.diff.neg) %>%
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>%
  ggplot(aes(x = reorder(predicate, Mean.Proj.diff.neg), y = Mean.Proj.diff.neg, fill = predicateType2)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Predicate", 
       y = "Difference between mean projection ratings (new - MV [neg only])", 
       fill = "Predicate Type") +
  scale_fill_manual(values = c("grey60", "green3", "deepskyblue2"),
                    labels = c("cogntive", "communicative with\nemotion entailment", 
                               "communicative without\nemotion entailment")) +
  facet_wrap(~ group, scales = "free_x", ncol = 1)
ggsave("../graphs/projection-by-communicative-differences-emo-neg.pdf", height = 7, width = 13)

## D.2 Ranking differences ----
### D.2.1 overall ----
# table of lowest to highest difference in ranking in the two datasets. 
# Mind the large number of ties in the MV dataset!
rankingDist <- 
  mean.proj.e1.plus %>% 
  mutate(rankingNew = rank(Mean.Proj),
         rankingMVneg = rank(Mean.Proj.MV.neg),
         ranking_difference = abs(rankingNew - rankingMVneg)) %>%
  arrange(ranking_difference) %>%
  select(predicate, rankingNew, rankingMVneg, ranking_difference, predicateType2) %>% 
  print(n = Inf)

#### density plot ----
# densitiy plot of the differences in ranking show that low differences are much more common than
# high ones.
mean.proj.e1.plus %>% 
  ggplot(aes(ranking_difference)) + 
  geom_density() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Ranking difference",
       y = "Density") 
ggsave("../graphs/ranking-difference.pdf", height = 7, width = 7)

mean(mean.proj.e1.plus$ranking_difference) # 34.30729

### D.2.2 emoComms only ----
# table of lowest to highest difference in ranking in the two datasets. 
# Mind the large number of ties in the MV dataset!
rankingDistEmo <- 
  mean.proj.e1.plus %>% 
  filter(predicateType2 == "emoComm") %>% 
  mutate(rankingNew = rank(Mean.Proj),
         rankingMVneg = rank(Mean.Proj.MV.neg),
         ranking_difference = abs(rankingNew - rankingMVneg)) %>%
  arrange(ranking_difference) %>%
  select(predicate, rankingNew, rankingMVneg, ranking_difference, predicateType2) %>% 
  print(n = Inf)

#### density plot ----
rankingDistEmo %>% 
  ggplot(aes(ranking_difference)) + 
  geom_density() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Ranking difference",
       y = "Density") 
ggsave("../graphs/ranking-difference-emo.pdf", height = 7, width = 7)

### D.2.2 smallest ranking differences ----

# Identify a particular number of predicates that occur in the smallest possible subset of highest 
# ranking predicates both in the MV dataset (negation only) and the new data.

similar_ranking <- function(number){
  for (x in 1:nrow(mean.proj.e1.plus %>% filter(predicate != "know"))) {
    
    top_ranking_new <- mean.proj.e1.plus %>%
      filter(predicate != "know") %>%   
      slice_max(order_by = rankingNew, n = x) %>% 
      pull(predicate)
    top_ranking_MV_neg <- mean.proj.e1.plus %>%
      filter(predicate != "know") %>%    
      slice_max(order_by = rankingMVneg, n = x) %>% 
      pull(predicate)
    
    common_predicates <- intersect(top_ranking_new, top_ranking_MV_neg)
    
    if (length(common_predicates) >= number) {
      break  
    }
  }
  a <- mean.proj.e1.plus %>% 
    filter(predicate %in% common_predicates) %>% 
    arrange(ranking_difference) %>% 
    select(predicate, Mean.Proj, Mean.Proj.MV.neg, rankingNew, rankingMVneg, ranking_difference)
  
  print(a, n = Inf)

  if (nrow(a) > number) {
    cat("\nselected number:", number, "; number of predicates:", nrow(a), "; size of subset:", x,"\n\nThe number of predicates in the output is larger than the selected number because of the\nties in the MV dataset (negation-only) and the fact that the for-loop can add multiple\npredicates to the list in one iteration.")
  } else {
    cat("\nselected number:", number, "; number of predicates:", nrow(a), "; size of subset:", x)
  }
}

similar_ranking(16)

#   predicate Mean.Proj Mean.Proj.MV.neg rankingNew rankingMVneg ranking_difference
#   <fct>         <dbl>            <dbl>      <dbl>        <dbl>              <dbl>
# 1 divulge       0.789            0.6          177         176.                0.5
# 2 flaunt        0.846            0.7          186         185                 1  
# 3 dispute       0.748            0.5          170         166.                3.5
# 4 apologize     0.885            1            188         192                 4  
# 5 fuss          0.753            0.5          171         166.                4.5
# 6 weep          0.791            0.7          178         185                 7  
# 7 disclose      0.822            0.8          181         190                 9  
# 8 whine         0.78             0.7          175         185                10  
# 9 publicize     0.747            0.667        169         181                12  
# 10 question      0.905            0.6          191         176.               14.5
# 11 bitch         0.823            0.5          182         166.               15.5
# 12 complain      0.833            0.5          185         166.               18.5
# 13 point out     0.73             0.7          166         185                19  
# 14 stress        0.891            0.5          190         166.               23.5
# 15 grumble       0.908            0.5          192         166.               25.5
# 
# selected number: 15 ; number of predicates: 15 ; size of subset: 26


## D.3 Spearman rank correlations ----
# correlations of projection ratings

### new data vs MV ----
# overall
# new data - MV all ECOs
mean.proj.e1.plus %>%
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV, method = "spearman")) # 0.497
# new data - MV negation only
mean.proj.e1.plus %>%
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV.neg, method = "spearman")) # 0.674

# communicatives with emotion entailment
mean.proj.e1.plus %>%
  filter(predicateType2 == "emoComm") %>% 
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV, method = "spearman")) # 0.397
mean.proj.e1.plus %>%
  filter(predicateType2 == "emoComm") %>% 
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV.neg, method = "spearman")) # 0.592

# communicatives with emotion entailment
mean.proj.e1.plus %>%
  filter(predicateType2 == "nonEmoComm") %>% 
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV, method = "spearman")) # 0.441
mean.proj.e1.plus %>%
  filter(predicateType2 == "nonEmoComm") %>% 
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV.neg, method = "spearman")) # 0.648

### within MV ----
# between the different embedding environments in the MV dataset
# negation vs question + conditional
mean.proj.MV.env %>% 
  pivot_wider(id_cols = predicate, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcond, method = "spearman")) # 0.163

# negation vs question + conditional + negation
mean.proj.MV.env %>% 
  pivot_wider(id_cols = predicate, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcondneg, method = "spearman")) # 0.508

# question + conditional vs question + conditional + negation
mean.proj.MV.env %>% 
  pivot_wider(id_cols = predicate, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(qcond, qcondneg, method = "spearman")) # -0.0290


# E Rating patterns ----
## E.1 new data ----
e1 %>%
  filter(rating == -1) %>% 
  group_by(participant_id) %>% 
  summarise(rating_count = n()) %>% 
  group_by(rating_count) %>% 
  summarise(participant_count = n()) %>% 
  mutate(count_sum = cumsum(participant_count))

# rating 1 ("yes")
#   rating_count participant_count count_sum
#          <int>             <int>     <int>
# 1            1                34        34
# 2            2                30        64
# 3            3                26        90
# 4            4                19       109
# 5            5                19       128
# 6            6                22       150
# 7            7                16       166
# 8            8                13       179
# 9            9                18       197
# 10           10                17       214
# 11           11                32       246
# 246 of 314 participants (78%) rated (precisely) "yes" at least once; 68 (22%) never rated "yes".
# 32 (10%) participants answered "yes" for every item.

# rating 0
#   rating_count participant_count count_sum
#           <int>             <int>     <int>
# 1            1                37        37
# 2            2                 9        46
# 3            3                 9        55
# 4            4                 5        60
# 5            5                 7        67
# 6            6                 3        70
# 7            7                 1        71
# 8            8                 2        73
# 9            9                 1        74
# 10           10                 1        75
# 11           11                 1        76
# 76 participants (24%) rated (precisely) 0 at least once, 238 (76%) never rated 0. 
# One participant rated every item at 0.

# rating -1 ("no")
#   rating_count participant_count count_sum
#           <int>             <int>     <int>
# 1            1                32        32
# 2            2                23        55
# 3            3                11        66
# 4            4                 4        70
# 5            5                 7        77
# 6            6                 5        82
# 7            7                 4        86
# 8            8                 4        90
# 9            9                 2        92
# 10           10                 3        95
# 11           11                 1        96
# 96 (31%) participants rated (precisely) "no" at least once, 218 (69%) never rated -1. 
# One participant rated "no" for every item.

e1 %>%
  filter(rating == 1) %>% 
  summarise(frequency = n(),
            proportion = n()/nrow(e1))
#     frequency proportion
#         <int>      <dbl>
#   1      1375      0.398
# 39.8% of the ratings were 'yes' (slider all the way to the right.)

e1 %>%
  filter(rating == -1) %>% 
  summarise(frequency = n(),
            proportion = n()/nrow(e1))
#     frequency proportion
#         <int>      <dbl>
#   1       311     0.0900
# 9% of the ratings were 'no' (slider all the way to the left.)

e1 %>% 
  mutate(rating_bin = cut(e1$rating, breaks = 3)) %>% 
  group_by(rating_bin) %>% 
  summarise(frequency = n(),
            proportion = n()/nrow(e1)) %>% 
  mutate(count_sum = cumsum(frequency)) %>% print(n = Inf)
# 3 bins
#   rating_bin     frequency proportion count_sum
#   <fct>              <int>      <dbl>     <int>
# 1 (-1,-0.333]          652      0.189       652
# 2 (-0.333,0.333]       723      0.209      1375
# 3 (0.333,1]           2079      0.602      3454
# roughly 20-20-60 

# 10 bins
#   rating_bin  frequency proportion count_sum
#   <fct>           <int>      <dbl>     <int>
# 1 (-1,-0.8]         391     0.113        391
# 2 (-0.8,-0.6]       103     0.0298       494
# 3 (-0.6,-0.4]       128     0.0371       622
# 4 (-0.4,-0.2]       120     0.0347       742
# 5 (-0.2,0]          373     0.108       1115
# 6 (0,0.2]           160     0.0463      1275
# 7 (0.2,0.4]         167     0.0483      1442
# 8 (0.4,0.6]         196     0.0567      1638
# 9 (0.6,0.8]         215     0.0622      1853
# 10 (0.8,1]          1601     0.464       3454

# 46.4% of the ratings were in the top 10 per cent range of the slider (very far or all the way to 
# the right). 11.3% of the ratings were in the bottom 10 per cent range of the slider. 10.8% were 
# between -.2 and 0.

# 20 bins
#    rating_bin  frequency proportion count_sum
# 20 (0.9,1]          1492     0.432       3454 - 43.2% in the top 5

# 100 bins
#    rating_bin  frequency proportion count_sum
# 90 (0.98,1]         1375    0.398        3454 - 39.8% in the top 100

### plots ----
# density plot
e1 %>% ggplot() +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_density(aes(rating), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(x = "Rating",
       y = "Density") 
ggsave("../graphs/ratings-density.pdf", height = 4, width = 4)

# plot of the 11 ratings each participant provided. 
# Participants are ordered by the sum of their ratings, i.e. the more high ratings a participant 
# gave, the further to the right of the x-axis they are in the plot. Ratings are ordered by their 
# numerical value, i.e. the closer a rating is to -1 ("no"), the lower it is on the y-axis, and 
# vice versa.
e1 %>%
  group_by(participant_id) %>%
  mutate(sum_ratings = sum(rating)) %>% 
  ungroup() %>%
  mutate(participant_id = fct_reorder(participant_id, sum_ratings)) %>%
  group_by(participant_id) %>%
  arrange(rating) %>% 
  mutate(order_within_participant = row_number()) %>%
  ungroup() %>% 
ggplot(aes(x = participant_id, y = order_within_participant, fill = rating)) +
  geom_tile(colour = "white") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 18, vjust = 0.9),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(5.5, 5.5, 11, 11, "pt")) +
  labs(x = "Participant", 
       y = "Ratings",
       fill = "Slider position") +
  scale_y_continuous(breaks = seq(1, 11, 1)) +  
  coord_cartesian(ylim = c(1, 11)) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       limits = c(-1, 1), breaks = c(-1, 1), labels = c("no", "yes"),
                       guide = guide_colourbar(barwidth = unit(200, "pt"))) 
ggsave("../graphs/ratings-distribution.pdf", height = 4, width = 13)

## E.1 MV ----
### E.1.1 overall ----
d %>% 
  filter(polarity == "negative" | conditional == "True") %>% 
  group_by(veridicality) %>% 
  summarise(frequency = n(),
            proportion = n()/nrow(d %>% filter(polarity == "negative" | conditional == "True")))
#   veridicality frequency proportion
#   <chr>            <int>      <dbl>
# 1 maybe             7725     0.474 
# 2 no                 936     0.0575
# 3 yes               7630     0.468 

# For projection ratings in the overall MV dataset, the most frequent choice was 'maybe' with 47.4%,
# closely followed by 'yes' with 46.8%. This is quite different from the 20-20-60 pattern in the new 
# data.

d %>% 
  filter(polarity == "negative" | conditional == "True") %>% 
  group_by(participant) %>%
  mutate(total_ratings = n(),
         count = sum(veridicality == "yes"),
         proportion = count / total_ratings) %>% 
  distinct(participant, total_ratings, count, proportion) %>% 
  arrange(desc(proportion))
#   participant total_ratings count proportion
#         <int>         <int> <int>      <dbl>
# 1          57            36    35      0.972
# 2         281            33    31      0.939
# 3         173            32    28      0.875
# 4           7            68    58      0.853
# No-one always rated 'yes'. One person always said 'yes' except once.

d %>% 
  filter(polarity == "negative" | conditional == "True") %>% 
  group_by(participant) %>%
  mutate(total_ratings = n(),
         count = sum(veridicality == "maybe"),
         proportion = count / total_ratings) %>% 
  distinct(participant, total_ratings, count, proportion) %>% 
  arrange(desc(proportion))
#   participant total_ratings count proportion
#         <int>         <int> <int>      <dbl>
# 1         245            42    42      1    
# 2          24            32    30      0.938
# 3         221            68    61      0.897
# 4          87            34    30      0.882
# One participant always selected "maybe".

d %>% 
  filter(polarity == "negative" | conditional == "True") %>% 
  group_by(participant) %>%
  mutate(total_ratings = n(),
         count = sum(veridicality == "no"),
         proportion = count / total_ratings) %>% 
  distinct(participant, total_ratings, count, proportion) %>% 
  arrange(desc(proportion))
#   participant total_ratings count proportion
#         <int>         <int> <int>      <dbl>
# 1          92            38    35      0.921
# 2         176            28    16      0.571
# 3          76            34    19      0.559
# 4         134            29    13      0.448
# No-one always chose "no".

d %>%
  group_by(participant) %>%
  filter(n() == 68) %>%
  mutate(yes_count = sum(veridicality == "yes")) %>% 
  ungroup() %>% 
  slice_max(yes_count, n = 1) %>% 
  filter(veridicality == "maybe") %>% 
  select(participant, verb_renamed, polarity, conditional)
# # A tibble: 1 × 4
# participant verb_renamed polarity conditional
#       <int> <chr>        <chr>    <chr>      
# 1        57 establish    negative False

# Of the 290 participants contributing to the MV dataset, no-one always rated 'yes'. One participant
# rated 'yes' every time except once. They provided a single 'maybe' rating for 'establish' under 
# negation.

# How many projection ratings per participant?
d %>% 
  filter(polarity == "negative" | conditional == "True") %>% 
  count(participant) %>%  
  summarise(min_count = min(n), max_count = max(n)) 
#   min_count max_count
# 1        26       110

# Participants provided between 26 and 110 ratings for items with an entailment-cancelling operator.
# Numbers of ratings were therefore normalised in the plot below.

#### plot ----
d %>%
  filter(polarity == "negative" | conditional == "True") %>% 
  group_by(participant) %>%
  arrange(factor(veridicality, levels = c("no", "maybe", "yes")), .by_group = TRUE) %>%
  mutate(total_ratings = n(),
         yes_count = sum(veridicality == "yes")) %>%
  ungroup() %>%
  mutate(participant = fct_reorder(as.factor(participant), yes_count / total_ratings)) %>%
  group_by(participant) %>%
  mutate(order_within_participant = row_number() / total_ratings,
         ymin = (row_number() - 1) / total_ratings,
         ymax = row_number() / total_ratings) %>%
  ungroup() %>%
  ggplot(aes(fill = veridicality)) +
  geom_rect(aes(xmin = as.numeric(participant) - 0.5,
                xmax = as.numeric(participant) + 0.5,
                ymin = ymin,
                ymax = ymax), 
            colour = "grey90") +
  theme(legend.position = "top",
        legend.title = element_text(size = 16, vjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = "Participant",
       y = "Normalised ratings",
       fill = "Response option") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(limits = c("yes", "maybe", "no"),
                    labels = c("yes", "maybe or maybe not", "no"),
                    values = c("blue", "white", "red")) +
  guides(fill = guide_legend(override.aes = list(colour = c("black", "black", "black"))))
ggsave("../graphs/ratings-distribution-MV.pdf", height = 4, width = 13)

### E.1.2 negation only ----
d %>% 
  filter(polarity == "negative" & conditional == "False") %>% 
  group_by(veridicality) %>% 
  summarise(frequency = n(),
            proportion = n()/nrow(d %>% filter(polarity == "negative" & conditional == "False")))
#   veridicality frequency proportion
#   <chr>            <int>      <dbl>
# 1 maybe             2664     0.492 
# 2 no                 369     0.0682
# 3 yes               2378     0.439 

# For projection ratings based on items with negation, the most frequent choice was 'maybe' with 49.2%,
# followed by 'yes' with 43.9%. This distribution is very similar to that for projection ratings based
# on all three embedding environments.

d %>% 
  filter(polarity == "negative" & conditional == "False") %>% 
  distinct(participant) %>% nrow() # 159
d %>% 
  filter(polarity == "negative" & conditional == "False") %>% 
  count(participant) %>% 
  summarise(min_count = min(n), max_count = max(n))
#   min_count max_count
# 1        26        42 
# 159 participants provided between 26 and 42 ratings for items with negation only. 

#### plot ----
d %>%
  filter(polarity == "negative" & conditional == "False") %>% 
  group_by(participant) %>%
  arrange(factor(veridicality, levels = c("no", "maybe", "yes")), .by_group = TRUE) %>%
  mutate(total_ratings = n(),
         yes_count = sum(veridicality == "yes")) %>%
  ungroup() %>%
  mutate(participant = fct_reorder(as.factor(participant), yes_count / total_ratings)) %>%
  group_by(participant) %>%
  mutate(order_within_participant = row_number() / total_ratings,
         ymin = (row_number() - 1) / total_ratings,
         ymax = row_number() / total_ratings) %>%
  ungroup() %>%
  ggplot(aes(fill = veridicality)) +
  geom_rect(aes(xmin = as.numeric(participant) - 0.5,
                xmax = as.numeric(participant) + 0.5,
                ymin = ymin,
                ymax = ymax), 
            colour = "grey90") +
  theme(legend.position = "top",
        legend.title = element_text(size = 16, vjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = "Participant",
       y = "Normalised ratings",
       fill = "Response option") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(limits = c("yes", "maybe", "no"),
                    labels = c("yes", "maybe or maybe not", "no"),
                    values = c("blue", "white", "red")) +
  guides(fill = guide_legend(override.aes = list(colour = c("black", "black", "black"))))
ggsave("../graphs/ratings-distribution-MV-NO.pdf", height = 4, width = 13)

### E.1.3 negation only, experiment 1 predicates ----
d %>% 
  filter(polarity == "negative" & conditional == "False" & verb_renamed %in% e1$predicate) %>% 
  group_by(veridicality) %>% 
  summarise(frequency = n(),
            proportion = n()/nrow(d %>% filter(polarity == "negative" & conditional == "False" & 
                                                 verb_renamed %in% e1$predicate)))
#   veridicality frequency proportion
#   <chr>            <int>      <dbl>
# 1 maybe             1194     0.624 
# 2 no                 157     0.0821
# 3 yes                562     0.294 

# For projection ratings based on MV items under negation for those predicates investigated here, 
# the most frequent choice was 'maybe' with 62.4% (vs 49.2% negation-only, all predicates). At 29.4%,
# 'yes' ratings are much less frequent than compared to all predicates under negation only (43.9%). 
# This distribution is therefore very different from that based on all predicates in the MV dataset 
# with negation only. This difference is somewhat expected, as the predicates in experiment 1 are all
# 'non-factive' (except for 'know'). However, the rating distribution found here is also extremely 
# different from that of the new data (roughly 20-20-60 with three bins).

d %>% 
  filter(polarity == "negative" & conditional == "False" & verb_renamed %in% e1$predicate) %>% 
  distinct(participant) %>% nrow() # 159
d %>% 
  filter(polarity == "negative" & conditional == "False" & verb_renamed %in% e1$predicate) %>% 
  count(participant) %>% 
  summarise(min_count = min(n), max_count = max(n))
#   min_count max_count
# 1         7        19
# 159 participants provided between 7 and 19 ratings for items with negation only that contained the
# 192 predicates of experiment 1. 

#### plot ----
d %>%
  filter(polarity == "negative" & conditional == "False" & verb_renamed %in% e1$predicate) %>% 
  group_by(participant) %>%
  arrange(factor(veridicality, levels = c("no", "maybe", "yes")), .by_group = TRUE) %>%
  mutate(total_ratings = n(),
         yes_count = sum(veridicality == "yes")) %>%
  ungroup() %>%
  mutate(participant = fct_reorder(as.factor(participant), yes_count / total_ratings)) %>%
  group_by(participant) %>%
  mutate(order_within_participant = row_number() / total_ratings,
         ymin = (row_number() - 1) / total_ratings,
         ymax = row_number() / total_ratings) %>%
  ungroup() %>%
  ggplot(aes(fill = veridicality)) +
  geom_rect(aes(xmin = as.numeric(participant) - 0.5,
                xmax = as.numeric(participant) + 0.5,
                ymin = ymin,
                ymax = ymax), 
            colour = "grey90") +
  theme(legend.position = "top",
        legend.title = element_text(size = 16, vjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = "Participant",
       y = "Normalised ratings",
       fill = "Response option") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(limits = c("yes", "maybe", "no"),
                    labels = c("yes", "maybe or maybe not", "no"),
                    values = c("blue", "white", "red")) +
  guides(fill = guide_legend(override.aes = list(colour = c("black", "black", "black"))))
ggsave("../graphs/ratings-distribution-MV-NO-my.pdf", height = 4, width = 13)



# H Dynamicity/Activity/CoS predicates and projection----
## H.3 activity ----
### H.3.1 overall ----
#### plot ----
mean.proj.act <- e1.comm %>%
  group_by(activity) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-activity-new.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ activity, mean.proj.e1.comm) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.33642    0.03509   9.588   <2e-16 ***
# activityyes  0.08560    0.04434   1.931    0.055 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2957 on 188 degrees of freedom
# Multiple R-squared:  0.01944,	Adjusted R-squared:  0.01423 
# F-statistic: 3.728 on 1 and 188 DF,  p-value: 0.05502

### H.3.2 by predicate type ----
# how many predicates in which predicateType and activity / no activity?
e1.comm %>%
  select(predicateType2, predicate, activity) %>%
  unique() %>%
  group_by(predicateType2, activity) %>%
  summarise(count = n())
#   predicateType2 activity count
#   <chr>          <chr>    <int>
# 1 emoComm        no           2
# 2 emoComm        yes         25
# 3 nonEmoComm     no          69
# 4 nonEmoComm     yes         94

#### plot ----
mean.proj.act2 <- e1.comm %>%
  group_by(predicateType2, activity) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2),Mean.Proj))
nrow(mean.proj.act2) # 4

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
ggsave("../graphs/projection-by-predicateType-and-activity-new.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ activity * fct_relevel(predicateType2, "emoComm"), mean.proj.e1.comm) %>% 
  summary()
# predicateType2  significance of no vs yes
# emoComm         n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.2883 on 186 degrees of freedom
# Multiple R-squared:  0.07762,	Adjusted R-squared:  0.06275 
# F-statistic: 5.218 on 3 and 186 DF,  p-value: 0.001758




# X VAD ratings ----
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
ggsave("../graphs/projection-by-valence2-new.pdf", height = 4, width = 6)

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
ggsave("../graphs/projection-by-valence-faceted2-new.pdf", height = 4, width = 5)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.30106    0.03933   7.654 1.73e-12 ***
# V.Mean.Sum2  0.36199    0.14088   2.569   0.0111 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2937 on 160 degrees of freedom
# Multiple R-squared:  0.03963,	Adjusted R-squared:  0.03363 
# F-statistic: 6.602 on 1 and 160 DF,  p-value: 0.0111

# by predicate type (the faceted plot with one combined fitted line)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      n.s.

# Valence is significant overall but not within either predicate type.

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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-new.pdf", 
       height = 6, width = 6)

# faceted by predicate type and direcions of valence and dominance
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) + 
  # plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_nested(V.Mean.Sum2.direction ~ predicateType2 + D.Mean.Sum2.direction, 
               labeller = labeller(V.Mean.Sum2.direction = as_labeller(V_labels), 
                                   D.Mean.Sum2.direction = as_labeller(D_labels),
                                   predicateType2 = as_labeller(predicateType2_names))) +
  geom_text(data = new.scale %>% 
              filter(predicateType == "communicative") %>% 
              count(D.Mean.Sum2.direction, V.Mean.Sum2.direction, predicateType2), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-Vdir-Ddir-predType2-new.pdf", 
       height = 6, width = 8.5)

# by direction of valence
mean.proj.Vdir <- e1.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-Vdir-new.pdf", height = 4, width = 4)

# by direction of valence and predicate type
mean.proj.Vdir2 <- e1.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-Vdir-predType2-new.pdf", height = 4, width = 4.5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ V.Mean.Sum2.direction, 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.47428    0.03370  14.075  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.16630    0.04546  -3.658 0.000345 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2879 on 160 degrees of freedom
# Multiple R-squared:  0.07718,	Adjusted R-squared:  0.07141 
# F-statistic: 13.38 on 1 and 160 DF,  p-value: 0.0003445

# by direction of valence and predicate type
lm(Mean.Proj ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2  significance of V.Mean.Sum2.direction
# emoComm         n.s.
# nonEmoComm      *

# valence and direction of valence
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.34476    0.05707   6.041 1.06e-08 ***
# V.Mean.Sum2                                                         0.50622    0.18170   2.786  0.00599 ** 
# fct_relevel(V.Mean.Sum2.direction, "negative")positive             -0.03516    0.07720  -0.455  0.64943    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive -0.51427    0.27809  -1.849  0.06628 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2829 on 158 degrees of freedom
# Multiple R-squared:  0.1204,	Adjusted R-squared:  0.1037 
# F-statistic: 7.209 on 3 and 158 DF,  p-value: 0.0001449

# difference in V.Mean.Sum2.direction is significant.

# V.Mean.Sum2.direction   significance of V.Mean.Sum2
# negative                **
# positive                n.s.

# V.Mean.Sum2 is significant for emoComms. This is DIFFERENT from the MV data, were it is not 
# significant in either predicate type.

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2	V.Mean.Sum2.direction   significance of V.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# V.Mean.Sum2 is not significant for any predicate type - V.Mean.Sum2.direction combination. This is 
# DIFFERENT from the MV data, were it is significant for emoComm - negative valence.

# by predicate type and direction of valence and dominance (= the nested faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
     fct_relevel(D.Mean.Sum2.direction, "positive") * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction  significance of V.Mean.Sum2
# 1        emoComm              negative              negative  n.s.
# 2        emoComm              negative              positive  n.s.
# 3        emoComm              positive              negative  n.s.
# 4        emoComm              positive              positive  n.s.
# 5     nonEmoComm              negative              negative  n.s.
# 6     nonEmoComm              negative              positive  n.s.
# 7     nonEmoComm              positive              negative  n.s. 
# 8     nonEmoComm              positive              positive  n.s.


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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-valence-communicative-faceted-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-say-vs-non-say-new.pdf", height = 4, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-say-vs-non-say-Vdir-new.pdf", height = 5, width = 5)

###### linear models ----
# overall
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerb, "no"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of V.Mean.Sum2
# no      *         
# yes     n.s.

# Residual standard error: 0.2925 on 158 degrees of freedom
# Multiple R-squared:  0.05955,	Adjusted R-squared:  0.04169 
# F-statistic: 3.335 on 3 and 158 DF,  p-value: 0.02099

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of V.Mean.Sum2
# no		              negative                *
# yes                 negative	              .
# no		              positive                n.s.
# yes		              positive                n.s.

###### X.5.1.4.1 say-verb type ----
####### plots ----
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverb-faceted-new.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverb-faceted-Vdir-new.pdf", height = 5, width = 5)

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of V.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# Residual standard error: 0.2641 on 84 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.1496,	Adjusted R-squared:  0.1091 
# F-statistic: 3.694 on 4 and 84 DF,  p-value: 0.008067

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb") * 
     fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of V.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              .
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2548 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.2462,	Adjusted R-squared:  0.1708 
# F-statistic: 3.266 on 8 and 80 DF,  p-value: 0.002856

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-modeverb-faceted-new.pdf", height = 4, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-modeverb-faceted-Vdir-new.pdf", height = 5, width = 5)

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of V.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.2039 on 43 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.1421,	Adjusted R-squared:  0.08225 
# F-statistic: 2.374 on 3 and 43 DF,  p-value: 0.08336

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

# Residual standard error: 0.181 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.3868,	Adjusted R-squared:  0.2767 
# F-statistic: 3.515 on 7 and 39 DF,  p-value: 0.005104

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-saybymeans-faceted-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-saybymeans-faceted-Vdir-new.pdf", height = 5, width = 6)

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "form"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of V.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "manner") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of V.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

# Residual standard error: 0.1999 on 24 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.2191,	Adjusted R-squared:  -0.1063 
# F-statistic: 0.6733 on 10 and 24 DF,  p-value: 0.7379

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
ggsave("../graphs/projection-by-arousal2-new.pdf", height = 3.5, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-faceted2-new.pdf", height = 4, width = 5)

###### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.21886    0.09088   2.408   0.0172 *
# A.Mean.Sum2  0.40947    0.21924   1.868   0.0636 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2965 on 160 degrees of freedom
# Multiple R-squared:  0.02134,	Adjusted R-squared:  0.01522 
# F-statistic: 3.488 on 1 and 160 DF,  p-value: 0.06364

# by predicate type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2	significance of A.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.2862 on 158 degrees of freedom
# Multiple R-squared:  0.09949,	Adjusted R-squared:  0.08239 
# F-statistic: 5.818 on 3 and 158 DF,  p-value: 0.0008505


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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-new.pdf", 
       height = 5, width = 5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                                         0.29352    0.12726   2.307   0.0224 *
# A.Mean.Sum2                                                         0.03894    0.33282   0.117   0.9070  
# fct_relevel(V.Mean.Sum2.direction, "positive")negative              0.04425    0.18687   0.237   0.8131  
# A.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative  0.27368    0.45054   0.607   0.5444  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2887 on 158 degrees of freedom
# Multiple R-squared:  0.08341,	Adjusted R-squared:  0.066 
# F-statistic: 4.792 on 3 and 158 DF,  p-value: 0.003185

# difference in V.Mean.Sum2.direction is not significant.

# V.Mean.Sum2.direction   significance of A.Mean.Sum2
# negative                n.s.
# positive                n.s.

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2	V.Mean.Sum2.direction   significance of A.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2839 on 154 degrees of freedom
# Multiple R-squared:  0.1362,	Adjusted R-squared:  0.09697 
# F-statistic:  3.47 on 7 and 154 DF,  p-value: 0.001764


# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#   V.Mean.Sum2.direction predicateType2  n
# 1              negative        emoComm 21
# 2              negative     nonEmoComm 52
# 3              positive        emoComm  4
# 4              positive     nonEmoComm 85

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
# 5     nonEmoComm              negative              negative  *
# 6     nonEmoComm              negative              positive  n.s.
# 7     nonEmoComm              positive              negative  n.s. 
# 8     nonEmoComm              positive              positive  n.s.


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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-arousal-communicative-faceted-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-say-vs-non-say-new.pdf", height = 4, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-say-vs-non-say-Vdir-new.pdf", height = 5, width = 5)

###### linear models ----
# overall
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerb, "yes"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of A.Mean.Sum2
# no      n.s.         
# yes     n.s. 

# with direction of valence (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerb, "no") * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            V.Mean.Sum2.direction   significance of A.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                n.s.

###### X.5.2.4.1 say-verb type ----
####### plots ----
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverb-faceted-new.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverb-faceted-Vdir-new.pdf", height = 5, width = 5)

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of A.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# Residual standard error: 0.2649 on 84 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.1444,	Adjusted R-squared:  0.1037 
# F-statistic: 3.545 on 4 and 84 DF,  p-value: 0.0101

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

# Residual standard error: 0.2584 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.2249,	Adjusted R-squared:  0.1474 
# F-statistic: 2.901 on 8 and 80 DF,  p-value: 0.00684


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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-modeverb-faceted-new.pdf", height = 4, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-modeverb-faceted-Vdir-new.pdf", height = 5, width = 5)

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of A.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.2008 on 43 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.1678,	Adjusted R-squared:  0.1097 
# F-statistic:  2.89 on 3 and 43 DF,  p-value: 0.04629

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

# Residual standard error: 0.1972 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.2726,	Adjusted R-squared:  0.142 
# F-statistic: 2.087 on 7 and 39 DF,  p-value: 0.06806

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
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
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
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-Vdir-new.pdf", height = 5, width = 6)

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of A.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# Residual standard error: 0.1994 on 29 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.06163,	Adjusted R-squared:  -0.1002 
# F-statistic: 0.3809 on 5 and 29 DF,  p-value: 0.8577

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     A.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "manner") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of A.Mean.Sum2
# form                negative                n/a
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              .
# sound 		          positive	              n.s.

# Residual standard error: 0.184 on 24 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.3388,	Adjusted R-squared:  0.06333 
# F-statistic:  1.23 on 10 and 24 DF,  p-value: 0.3222

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
ggsave("../graphs/projection-by-dominance2-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-faceted2-new.pdf", height = 4, width = 5)

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.4439     0.0452   9.822   <2e-16 ***
# D.Mean.Sum2  -0.2921     0.1852  -1.577    0.117    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2974 on 160 degrees of freedom
# Multiple R-squared:  0.01531,	Adjusted R-squared:  0.009156 
# F-statistic: 2.488 on 1 and 160 DF,  p-value: 0.1167

# by predicate type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>%  
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm:        n.s.
# nonEmoComm:     n.s.


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
ggsave("../graphs/projection-by-domiance-with-direction-of-valence-new.pdf", height = 3.5, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-new.pdf", 
       height = 5, width = 5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.47881    0.05912   8.099 1.41e-13 ***
# D.Mean.Sum2                                                        -0.02656    0.28390  -0.094    0.926    
# fct_relevel(V.Mean.Sum2.direction, "negative")positive             -0.12408    0.08952  -1.386    0.168    
# D.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive -0.16808    0.37765  -0.445    0.657    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2892 on 158 degrees of freedom
# Multiple R-squared:  0.08078,	Adjusted R-squared:  0.06333 
# F-statistic: 4.629 on 3 and 158 DF,  p-value: 0.003936

# difference in V.Mean.Sum2.direction is not significant.

# V.Mean.Sum2.direction   significance of D.Mean.Sum2
# negative                n.s.
# positive                n.s.

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction   significance of D.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# by predicate type and direction of valence and dominance (= the nested faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
     fct_relevel(D.Mean.Sum2.direction, "negative") * fct_relevel(predicateType2, "nonEmoComm"), 
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


##### X.5.3.3 with "direction" of dominance ----
# controlled/submissive/guided/... vs in control/autonomous/controlling/... 
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
ggsave("../graphs/projection-by-domiance-with-direction-of-dominance-new.pdf", height = 3.5, width = 6)

# faceted
new.scale %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2-new.pdf", 
       height = 5, width = 5)

# by direction of dominance
mean.proj.Ddir <- e1.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-Ddir-new.pdf", height = 4, width = 4)

# by direction of dominance and predicate type
mean.proj.Ddir2 <- e1.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
ggsave("../graphs/projection-by-Ddir-predType2-new.pdf", height = 4, width = 4.5)


###### linear models ----
# by direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2.direction, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.50605    0.04216  12.003  < 2e-16 ***
# D.Mean.Sum2.directionpositive -0.17346    0.05004  -3.466 0.000678 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.289 on 160 degrees of freedom
# Multiple R-squared:  0.06985,	Adjusted R-squared:  0.06404 
# F-statistic: 12.02 on 1 and 160 DF,  p-value: 0.0006777

# by direction of dominance and predicate type
lm(Mean.Proj ~ D.Mean.Sum2.direction * fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2.direction
# emoComm         n.s.
# nonEmoComm      **

# dominance and direction of dominance
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()

# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.41305    0.05654   7.305 1.28e-11 ***
# D.Mean.Sum2                                                        -0.35248    0.21807  -1.616   0.1080    
# fct_relevel(D.Mean.Sum2.direction, "positive")negative              0.03570    0.08987   0.397   0.6917    
# D.Mean.Sum2:fct_relevel(D.Mean.Sum2.direction, "positive")negative  0.70707    0.40870   1.730   0.0856 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2876 on 158 degrees of freedom
# Multiple R-squared:  0.09094,	Adjusted R-squared:  0.07368 
# F-statistic: 5.268 on 3 and 158 DF,  p-value: 0.001724

# difference in D.Mean.Sum2.direction is not significant.

# D.Mean.Sum2.direction   significance of D.Mean.Sum2
# negative                n.s.
# positive                n.s.

# by predicate type and direction of dominance (= the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(D.Mean.Sum2.direction, "negative"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2	D.Mean.Sum2.direction   significance of D.Mean.Sum2
# emoComm         negative                n.s.
# nonEmoComm      negative                n.s.
# emoComm         positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.2823 on 154 degrees of freedom
# Multiple R-squared:  0.1458,	Adjusted R-squared:  0.1069 
# F-statistic: 3.754 on 7 and 154 DF,  p-value: 0.000876


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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-dominance-communicative-faceted-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-say-vs-non-say-new.pdf", height = 4, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-say-vs-non-say-Vdir-new.pdf", height = 5, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-say-vs-non-say-Ddir-new.pdf", height = 5, width = 5)

###### linear models ----
# overall
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayVerb, "yes"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb significance of D.Mean.Sum2
# no      n.s.         
# yes     .

# Residual standard error: 0.2958 on 158 degrees of freedom
# Multiple R-squared:  0.03816,	Adjusted R-squared:  0.0199 
# F-statistic: 2.089 on 3 and 158 DF,  p-value: 0.1038

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

# Residual standard error: 0.2875 on 154 degrees of freedom
# Multiple R-squared:  0.1141,	Adjusted R-squared:  0.07386 
# F-statistic: 2.834 on 7 and 154 DF,  p-value: 0.00831

# with direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerb, "yes") * fct_relevel(D.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerb	            D.Mean.Sum2.direction   significance of D.Mean.Sum2
# no		              negative                n.s.
# yes                 negative	              n.s.
# no		              positive                n.s.
# yes		              positive                *

# Residual standard error: 0.2865 on 154 degrees of freedom
# Multiple R-squared:  0.1207,	Adjusted R-squared:  0.0807 
# F-statistic: 3.019 on 7 and 154 DF,  p-value: 0.005315

###### X.5.3.5.1 say-verb type ----
####### plots ----
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverb-faceted-new.pdf", height = 4, width = 5)

# faceted by direction of valence
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType, V.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverb-faceted-Vdir-new.pdf", height = 5, width = 5)

# faceted by direction of dominance
new.scale %>% 
  filter(predicateType == "communicative" & sayVerb == "yes" & !predicate == "say") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(), 
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
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
              filter(predicateType == "communicative" & sayVerb == "yes" & predicate != "say") %>% 
              count(sayVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverb-faceted-Ddir-new.pdf", height = 5, width = 5)

###### linear models ----
# by sayVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType         significance of D.Mean.Sum2
# discourse role verb n.s.
# mode verb           n.s.

# Residual standard error: 0.2617 on 84 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.1647,	Adjusted R-squared:  0.1249 
# F-statistic:  4.14 on 4 and 84 DF,  p-value: 0.004133

# by sayVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerbType, "mode verb") * 
     fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        V.Mean.Sum2.direction   significance of D.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.2563 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.2371,	Adjusted R-squared:  0.1608 
# F-statistic: 3.108 on 8 and 80 DF,  p-value: 0.00417

# by sayVerbType type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayVerbType, "discourse role verb") * 
     fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayVerbType	        D.Mean.Sum2.direction   significance of D.Mean.Sum2
# discourse role verb	negative                n.s.
# mode verb		        negative	              n.s.
# discourse role verb	positive                n.s.
# mode verb		        positive                n.s.

# Residual standard error: 0.259 on 80 degrees of freedom
# (73 observations deleted due to missingness)
# Multiple R-squared:  0.2213,	Adjusted R-squared:  0.1435 
# F-statistic: 2.843 on 8 and 80 DF,  p-value: 0.007863

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-modeverb-faceted-new.pdf", height = 4, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-modeverb-faceted-Vdir-new.pdf", height = 5, width = 5)

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
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
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
ggsave("../graphs/projection-by-dominance-modeverb-faceted-Ddir-new.pdf", height = 5, width = 5)

###### linear models ----
# by modeVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType        significance of D.Mean.Sum2
# say-by-means        n.s.
# say-with-attitude   n.s.

# Residual standard error: 0.2035 on 43 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.1459,	Adjusted R-squared:  0.08627 
# F-statistic: 2.448 on 3 and 43 DF,  p-value: 0.07662

# by modeVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(modeVerbType, "say-by-means") * 
     fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      V.Mean.Sum2.direction   significance of D.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                .

# Residual standard error: 0.2187 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.1653,	Adjusted R-squared:  0.01552 
# F-statistic: 1.104 on 7 and 39 DF,  p-value: 0.3802

# by modeVerbType type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(modeVerbType, "say-with-attitude") * 
     fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# modeVerbType	      D.Mean.Sum2.direction   significance of D.Mean.Sum2
# say-by-means	      negative                n.s.
# say-with-attitude		negative	              n.s.
# say-by-means	      positive                n.s.
# say-with-attitude		positive                n.s.

# Residual standard error: 0.1956 on 39 degrees of freedom
# (115 observations deleted due to missingness)
# Multiple R-squared:  0.2844,	Adjusted R-squared:  0.1559 
# F-statistic: 2.214 on 7 and 39 DF,  p-value: 0.05396

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
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
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
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-new.pdf", height = 4, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
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
            aes(x = Inf, y = -.98, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-Vdir-new.pdf", height = 5, width = 6)

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
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
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
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-Ddir-new.pdf", height = 5, width = 6)

###### linear models ----
# by sayByMeansVerbType type (the faceted plot)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "form"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType  significance of D.Mean.Sum2
# form                n.s.
# manner              n.s.
# sound               n.s.

# Residual standard error: 0.1924 on 29 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.1265,	Adjusted R-squared:  -0.02405 
# F-statistic: 0.8403 on 5 and 29 DF,  p-value: 0.5322

# by sayByMeansVerbType type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "manner") * 
     fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	V.Mean.Sum2.direction   significance of D.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

# Residual standard error: 0.2003 on 24 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.2159,	Adjusted R-squared:  -0.1108 
# F-statistic: 0.6608 on 10 and 24 DF,  p-value: 0.7483

# by sayByMeansVerbType type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(sayByMeansVerbType, "sound") * 
     fct_relevel(D.Mean.Sum2.direction, "negative"), 
   data = new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# sayByMeansVerbType	D.Mean.Sum2.direction   significance of D.Mean.Sum2
# form                negative                n.s.
# manner		          negative	              n.s.
# sound 		          negative	              n.s.
# form                positive                n.s.
# manner		          positive	              n.s.
# sound 		          positive	              n.s.

# Residual standard error: 0.204 on 24 degrees of freedom
# (127 observations deleted due to missingness)
# Multiple R-squared:  0.1866,	Adjusted R-squared:  -0.1523 
# F-statistic: 0.5507 on 10 and 24 DF,  p-value: 0.8366

#### X.5.4 valence + arousal ----
##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.21235    0.09002   2.359   0.0195 *
# V.Mean.Sum2  0.30774    0.14925   2.062   0.0408 *
# A.Mean.Sum2  0.25205    0.23008   1.095   0.2750  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2935 on 159 degrees of freedom
# Multiple R-squared:  0.04682,	Adjusted R-squared:  0.03483 
# F-statistic: 3.905 on 2 and 159 DF,  p-value: 0.0221

lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)
# (Intercept)               0.2495     0.1590   1.569    0.119
# V.Mean.Sum2               0.1405     0.6084   0.231    0.818
# A.Mean.Sum2               0.1646     0.3853   0.427    0.670
# V.Mean.Sum2:A.Mean.Sum2   0.3709     1.3079   0.284    0.777
# 
# Residual standard error: 0.2944 on 158 degrees of freedom
# Multiple R-squared:  0.04731,	Adjusted R-squared:  0.02922 
# F-statistic: 2.615 on 3 and 158 DF,  p-value: 0.05308

lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   0.381249   0.024385  15.634   <2e-16 ***
# V.Mean.Sum2.sc                0.047624   0.027057   1.760   0.0803 .  
# A.Mean.Sum2.sc                0.026540   0.024580   1.080   0.2819    
# V.Mean.Sum2.sc:A.Mean.Sum2.sc 0.006511   0.022960   0.284   0.7771    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2944 on 158 degrees of freedom
# Multiple R-squared:  0.04731,	Adjusted R-squared:  0.02922 
# F-statistic: 2.615 on 3 and 158 DF,  p-value: 0.05308


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
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-new.pdf", height = 4.5, width = 9.5)

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
        strip.text = element_text(size = 12),
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
        strip.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
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
        strip.text = element_text(size = 12),
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
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-facets-new.pdf", height = 5.5, width = 10)

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
        strip.text = element_text(size = 12),
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
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-new.pdf", height = 5.5, width = 10)

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
        strip.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
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
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-2-new.pdf", height = 5.5, width = 14)

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
ggsave("../graphs/projection-by-valence-Vdir-Ddir-predType2-new.pdf", height = 5, width = 8)

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
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
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
ggsave("../graphs/projection-by-arousal-Vdir-Ddir-predType2-new.pdf", height = 5, width = 8)

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
ggsave("../graphs/projection-by-dominance-Vdir-Ddir-predType2-new.pdf", height = 5, width = 8)

vplotf3 + 
  (aplotf3 & theme(axis.title.y = element_blank())) + 
  (dplotf3 & theme(axis.title.y = element_blank())) 
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-predType2-new.pdf", height = 5, width = 24)

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
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-new.pdf", height = 5.5, width = 8)

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
  theme(panel.grid.minor = element_blank(), 
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
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-scaled-new.pdf", height = 5.5, width = 8)

#### X.5.6 directions of valence and dominance ----
mean.projection <- e1.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>% 
  arrange(desc(Mean.Proj)) %>% print()
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction Mean.Proj  CILow CIHigh YMin.Proj YMax.Proj
#   <chr>          <chr>                 <chr>                     <dbl>  <dbl>  <dbl>     <dbl>     <dbl>
# 1 emoComm        negative              positive                  0.631 0.0872 0.0894     0.544     0.720
# 2 emoComm        negative              negative                  0.604 0.0717 0.0729     0.533     0.677
# 3 nonEmoComm     positive              negative                  0.554 0.108  0.106      0.446     0.660
# 4 emoComm        positive              negative                  0.492 0.270  0.240      0.223     0.732
# 5 nonEmoComm     negative              negative                  0.440 0.0636 0.0609     0.377     0.501
# 6 emoComm        positive              positive                  0.415 0.163  0.168      0.253     0.583
# 7 nonEmoComm     negative              positive                  0.385 0.0687 0.0604     0.317     0.446
# 8 nonEmoComm     positive              positive                  0.273 0.0394 0.0396     0.234     0.313

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
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2, guide = "none")
ggsave("../graphs/projection-by-Vdir-Ddir-new.pdf", height = 4, width = 6)

##### tables ----
# negative/positive valence vs negative/positive dominance predicates
# negative valence, negative dominance - higher projection
new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(predicate, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    predicate V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1        lie      0.6525      0.0700     nonEmoComm
# 2    dispute      0.6200      0.2150     nonEmoComm
# 3      bitch      0.6125      0.3375        emoComm
# 4       fake      0.5950      0.1325     nonEmoComm
# 5        sob      0.5875      0.4275        emoComm
# 6     bicker      0.5475      0.1150     nonEmoComm
# 7      whine      0.5450      0.0675        emoComm
# 8    quarrel      0.5400      0.0600        emoComm
# 9       weep      0.5300      0.2075        emoComm
# 10    reject      0.5125      0.3850     nonEmoComm

# negative valence, positive dominance - lower projection
new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(predicate, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#       predicate V.Mean.Sum2 D.Mean.Sum2 predicateType2
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
  select(predicate, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#      predicate V.Mean.Sum2 D.Mean.Sum2 predicateType2
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
  select(predicate, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#       predicate V.Mean.Sum2 D.Mean.Sum2 predicateType2
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
# 1 negative              negative              nonEmoComm        27        137     0.197 - in the neg-neg group there are 27 nonEmoComms, which is 19.7% of the predicates of this type.
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

# 2 emoComm        negative              negative                 12         25     0.48  - amongst the emoComms there are 12 neg-neg predicates, which is 48% of all emoComms.
# 3 emoComm        negative              positive                  9         25     0.36  
# 4 emoComm        positive              positive                  3         25     0.12  
# 5 emoComm        positive              negative                  1         25     0.04  

# 6 nonEmoComm     positive              positive                 78        137     0.569 
# 7 nonEmoComm     negative              negative                 27        137     0.197 
# 8 nonEmoComm     negative              positive                 25        137     0.182 
# 9 nonEmoComm     positive              negative                  7        137     0.0511


# F say that not ----
## plots ----
e1 %>% 
  mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                               "dispute", "question") ~ "say that not", 
                              TRUE ~ "other")) %>% 
  filter(predicateType == "communicative") %>% 
  group_by(that_not) %>% 
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) %>%
  ggplot(aes(x = that_not, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") + 
  #  scale_x_discrete(labels = c("other", "say that not")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-say-that-not.pdf", height = 4, width = 4)

ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                             "dispute", "question")), 
             aes(x = predicate, y = Mean.Proj, colour = "say that not")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                                   "dispute", "question")),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.4,
                   colour = "black") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say that not", "say", "think", "know"),
                      values = c("deepskyblue2", "black", "blue", "deeppink", "orangered3"))
ggsave("../graphs/projection-by-communicative-say-that-not-labels.pdf", height = 4, width = 13)

## linear models ----
# new data
lm(Mean.Proj ~ that_not, data = mean.proj.e1 %>% 
     mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                  "dispute", "question") ~ "say that not", 
                                 TRUE ~ "other")) %>% 
     filter(predicateType == "communicative") ) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.37716    0.02152  17.524  < 2e-16 ***
# that_notsay that not  0.34956    0.11213   3.117  0.00211 ** 

lm(rating ~ that_not, data = e1 %>% 
     mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                  "dispute", "question") ~ "say that not", 
                                 TRUE ~ "other")) %>% 
     filter(predicateType == "communicative") ) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.37517    0.01215  30.879  < 2e-16 ***
# that_notsay that not  0.33816    0.06238   5.421 6.35e-08 ***

# MV overall
lm(Mean.Proj.MV ~ that_not, data = mean.proj.e1.plus %>% 
     mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                  "dispute", "question") ~ "say that not", 
                                 TRUE ~ "other")) %>% 
     filter(predicateType == "communicative") ) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.31289    0.01314  23.816   <2e-16 ***
# that_notsay that not -0.05098    0.06845  -0.745    0.457  

lm(veridicality_num ~ that_not, data = d.proj %>% 
     mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                  "dispute", "question") ~ "say that not", 
                                 TRUE ~ "other")) %>% 
     filter(predicateType == "communicative") ) %>% 
  summary()
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.312785   0.007928  39.454   <2e-16 ***
# that_notsay that not -0.050880   0.041278  -1.233    0.218  

# MV neg-only
lm(Mean.Proj.MV.neg ~ that_not, data = mean.proj.e1.plus %>% 
     mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                  "dispute", "question") ~ "say that not", 
                                 TRUE ~ "other")) %>% 
     filter(predicateType == "communicative") ) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.20389    0.01830  11.142   <2e-16 ***
# that_notsay that not  0.19611    0.09534   2.057   0.0411 *  

lm(veridicality_num ~ that_not, data = d.proj.neg %>% 
     mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                                  "dispute", "question") ~ "say that not", 
                                 TRUE ~ "other")) %>% 
     filter(predicateType == "communicative") ) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.20351    0.01346  15.123  < 2e-16 ***
# that_notsay that not  0.19649    0.06998   2.808  0.00504 ** 

# "say that not" predicates project significantly more than others in the new data as well as MV-neg-only
# data. In the overall MV data, no such pattern is observed. 


# V Volition ----
## V.1 all predicates ----
### V.1.1 by predicate ----
#### plots ----
mean.proj.e1 %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = predicate, y = Mean.Proj, colour = volition)) +
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
ggsave("../graphs/projection-by-volition-new.pdf", height = 4, width = 13)

# faceted by predicate type
mean.proj.e1 %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = predicate, y = Mean.Proj, colour = volition)) +
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
ggsave("../graphs/projection-by-volition-and-predicateType2-new.pdf", height = 4, width = 5)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.35887    0.02238  16.032  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.23688    0.06171   3.839 0.000169 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2875 on 188 degrees of freedom
# Multiple R-squared:  0.07268,	Adjusted R-squared:  0.06775 
# F-statistic: 14.74 on 1 and 188 DF,  p-value: 0.0001689

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol <- e1 %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-predicateType-and-volition-new.pdf", height = 4, width = 5)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional") * fct_relevel(predicateType2, "nonEmoComm"), 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                                         0.34755    0.02303  15.092   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional                                                   0.18496    0.09800   1.887   0.0607 .  
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                                    0.16968    0.08919   1.902   0.0587 .  
# fct_relevel(volition, "volitional")non-volitional:fct_relevel(predicateType2, "nonEmoComm")emoComm -0.07089    0.14877  -0.476   0.6343    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2858 on 186 degrees of freedom
# Multiple R-squared:  0.09367,	Adjusted R-squared:  0.07906 
# F-statistic: 6.408 on 3 and 186 DF,  p-value: 0.0003737

# volition is not significant for either predicate type.

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


# ZZ combined models ----
## predicate type ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   mean.proj.e1) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58484    0.05630  10.388  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.26280    0.21439  -1.226 0.221799    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.22707    0.06079  -3.736 0.000248 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2926 on 189 degrees of freedom
# Multiple R-squared:  0.06925,	Adjusted R-squared:  0.0594 
# F-statistic: 7.031 on 2 and 189 DF,  p-value: 0.001135

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58484    0.05538  10.561  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.22707    0.05979  -3.798 0.000197 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2877 on 188 degrees of freedom
# Multiple R-squared:  0.07126,	Adjusted R-squared:  0.06632 
# F-statistic: 14.42 on 1 and 188 DF,  p-value: 0.0001969

# The effects of predicate type are significantly different between the two types of communicatives.
# However, predicateType2 explains very little of the variation found in the data.

## ZZ.1 VAD ----
# predicate type and VAD ratings
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58441    0.05738  10.184  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.23826    0.06240  -3.818 0.000192 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2869 on 160 degrees of freedom
# Multiple R-squared:  0.08352,	Adjusted R-squared:  0.07779 
# F-statistic: 14.58 on 1 and 160 DF,  p-value: 0.0001917

### ZZ.1.1 Valence ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.51006    0.07698   6.626 5.07e-10 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.20649    0.06597  -3.130  0.00208 ** 
# V.Mean.Sum2                                       0.20997    0.14551   1.443  0.15098    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2859 on 159 degrees of freedom
# Multiple R-squared:  0.09536,	Adjusted R-squared:  0.08398 
# F-statistic: 8.381 on 2 and 159 DF,  p-value: 0.0003465

# Valence adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                                    0.4218     0.1372   3.076  0.00247 **
# fct_relevel(predicateType2, "emoComm")nonEmoComm              -0.1078     0.1430  -0.754  0.45209   
# V.Mean.Sum2                                                    0.4591     0.3520   1.304  0.19399   
# fct_relevel(predicateType2, "emoComm")nonEmoComm:V.Mean.Sum2  -0.3006     0.3866  -0.778  0.43798   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2863 on 158 degrees of freedom
# Multiple R-squared:  0.09881,	Adjusted R-squared:  0.0817 
# F-statistic: 5.775 on 3 and 158 DF,  p-value: 0.0008997

# V.Mean.Sum2 is neither significant for emoComms nor for nonEmoComms.

### ZZ.1.2 Arousal ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.48426    0.11703   4.138 5.67e-05 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.22198    0.06457  -3.438 0.000749 ***
# A.Mean.Sum2                                       0.21558    0.21955   0.982 0.327631    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2869 on 159 degrees of freedom
# Multiple R-squared:  0.08904,	Adjusted R-squared:  0.07758 
# F-statistic: 7.771 on 2 and 159 DF,  p-value: 0.0006028

# Arousal adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * A.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# A.Mean.Sum2 is not significant for emoComm and nonEmoComm.
# No significant interactions for emoComm and nonEmoComm.

# Residual standard error: 0.2862 on 158 degrees of freedom
# Multiple R-squared:  0.09949,	Adjusted R-squared:  0.08239 
# F-statistic: 5.818 on 3 and 158 DF,  p-value: 0.0008505

### ZZ.1.3 Dominance ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.63274    0.06662   9.498  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.23303    0.06231  -3.740 0.000257 ***
# D.Mean.Sum2                                      -0.25251    0.17841  -1.415 0.158940    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.286 on 159 degrees of freedom
# Multiple R-squared:  0.09492,	Adjusted R-squared:  0.08353 
# F-statistic: 8.337 on 2 and 159 DF,  p-value: 0.0003603

# Dominance adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * D.Mean.Sum2, 
   new.scale %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.60049    0.09432   6.367    2e-09 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.19135    0.10637  -1.799   0.0739 .  
# D.Mean.Sum2                                                  -0.08403    0.39124  -0.215   0.8302    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:D.Mean.Sum2 -0.21298    0.43989  -0.484   0.6289    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2867 on 158 degrees of freedom
# Multiple R-squared:  0.09626,	Adjusted R-squared:  0.0791 
# F-statistic:  5.61 on 3 and 158 DF,  p-value: 0.001112

# For emoComm and nonEmoComm, D.Mean.Sum2 is not significant. 

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

## ZZ.2 SAY ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>%  
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58484    0.05538  10.561  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.22707    0.05979  -3.798 0.000197 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2877 on 188 degrees of freedom
# Multiple R-squared:  0.07126,	Adjusted R-squared:  0.06632 
# F-statistic: 14.42 on 1 and 188 DF,  p-value: 0.0001969

### ZZ.2.1 say verb ----
lm(Mean.Proj ~ sayVerb , 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.44744    0.03209  13.942   <2e-16 ***
# sayVerbyes  -0.10290    0.04297  -2.395   0.0176 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2941 on 188 degrees of freedom
# Multiple R-squared:  0.02961,	Adjusted R-squared:  0.02445 
# F-statistic: 5.736 on 1 and 188 DF,  p-value: 0.0176

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerb , 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.71267    0.06455  11.040  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.27849    0.05976  -4.660 5.99e-06 ***
# sayVerbyes                                       -0.15007    0.04202  -3.572 0.000451 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2792 on 187 degrees of freedom
# Multiple R-squared:  0.1306,	Adjusted R-squared:  0.1213 
# F-statistic: 14.04 on 2 and 187 DF,  p-value: 2.082e-06

# sayVerb is significant! (Unlike across predicate types.)

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerb , 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                  0.711656   0.139951   5.085 8.93e-07 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm            -0.277422   0.143407  -1.935   0.0546 .  
# sayVerbyes                                                  -0.148875   0.151633  -0.982   0.3275    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbyes -0.001295   0.157848  -0.008   0.9935    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2799 on 186 degrees of freedom
# Multiple R-squared:  0.1306,	Adjusted R-squared:  0.1165 
# F-statistic: 9.311 on 3 and 186 DF,  p-value: 9.115e-06

### ZZ.2.2 say verb type ----
lm(Mean.Proj ~ sayVerbType, 
   mean.proj.e1 %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.234007   0.036615   6.391 4.87e-09 ***
# sayVerbTypemode verb  0.217156   0.051057   4.253 4.65e-05 ***
# sayVerbTypesay       -0.009841   0.264033  -0.037     0.97    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2615 on 103 degrees of freedom
# (84 observations deleted due to missingness)
# Multiple R-squared:  0.1509,	Adjusted R-squared:  0.1344 
# F-statistic: 9.152 on 2 and 103 DF,  p-value: 0.0002196

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1 %>% filter(sayVerb == "yes")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.56278    0.05369  10.482  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.79278    0.26302  -3.014  0.00324 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.27872    0.06067  -4.594 1.23e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2575 on 104 degrees of freedom
# Multiple R-squared:  0.2002,	Adjusted R-squared:  0.1848 
# F-statistic: 13.02 on 2 and 104 DF,  p-value: 9.007e-06

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerbType, data = mean.proj.e1) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.428439   0.078207   5.478 3.11e-07 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.194432   0.069703  -2.789   0.0063 ** 
# sayVerbTypemode verb                              0.134342   0.057682   2.329   0.0218 *  
# sayVerbTypesay                                   -0.009841   0.255748  -0.038   0.9694    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2533 on 102 degrees of freedom
# (86 observations deleted due to missingness)
# Multiple R-squared:  0.2111,	Adjusted R-squared:  0.1879 
# F-statistic: 9.097 on 3 and 102 DF,  p-value: 2.164e-05

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerbType, data = mean.proj.e1) %>%
  summary()
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                            0.428439   0.078207   5.478 3.11e-07 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                      -0.194432   0.069703  -2.789   0.0063 ** 
# sayVerbTypemode verb                                                   0.134342   0.057682   2.329   0.0218 *  
# sayVerbTypesay                                                        -0.009841   0.255748  -0.038   0.9694    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypemode verb        NA         NA      NA       NA    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypesay              NA         NA      NA       NA    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2533 on 102 degrees of freedom
# (86 observations deleted due to missingness)
# Multiple R-squared:  0.2111,	Adjusted R-squared:  0.1879 
# F-statistic: 9.097 on 3 and 102 DF,  p-value: 2.164e-05

# sayVerbType is significant for both predicate types.

### ZZ.2.3 mode verb type ----
lm(Mean.Proj ~ modeVerbType, data = mean.proj.e1) %>%
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.40738    0.03150  12.934  < 2e-16 ***
# modeVerbTypesay-with-attitude  0.18187    0.06419   2.833  0.00654 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2017 on 52 degrees of freedom
# (138 observations deleted due to missingness)
# Multiple R-squared:  0.1337,	Adjusted R-squared:  0.1171 
# F-statistic: 8.027 on 1 and 52 DF,  p-value: 0.006544

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1 %>% filter(sayVerbType == "mode verb")) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.56278    0.04030  13.965  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.19443    0.05319  -3.656 0.000598 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1933 on 52 degrees of freedom
# Multiple R-squared:  0.2045,	Adjusted R-squared:  0.1892 
# F-statistic: 13.36 on 1 and 52 DF,  p-value: 0.000598

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + modeVerbType, data = mean.proj.e1) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.51542    0.04213   12.23   <2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.17719    0.05077   -3.49   0.0010 ** 
# modeVerbTypesay-with-attitude                     0.15560    0.05872    2.65   0.0107 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.183 on 51 degrees of freedom
# (138 observations deleted due to missingness)
# Multiple R-squared:  0.3007,	Adjusted R-squared:  0.2733 
# F-statistic: 10.97 on 2 and 51 DF,  p-value: 0.0001092

# adding mode verb type significantly improves the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * modeVerbType, data = mean.proj.e1) %>%
  summary()
# Coefficients:
#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.52825    0.04596  11.494 1.21e-15 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.19823    0.05886  -3.368  0.00146 ** 
# modeVerbTypesay-with-attitude                                                   0.11345    0.08331   1.362  0.17936    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:modeVerbTypesay-with-attitude  0.08457    0.11800   0.717  0.47690    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1838 on 50 degrees of freedom
# (138 observations deleted due to missingness)
# Multiple R-squared:  0.3078,	Adjusted R-squared:  0.2663 
# F-statistic: 7.413 on 3 and 50 DF,  p-value: 0.0003348

# modeVerbType is significant for nonEmoComm, but not for emoComm.

### ZZ.2.4 say-by-means verb type ----
lm(Mean.Proj ~ sayByMeansVerbType, data = mean.proj.e1) %>%
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.33791    0.05182   6.521  1.1e-07 ***
# sayByMeansVerbTypemanner  0.10737    0.06629   1.620    0.114    
# sayByMeansVerbTypesound   0.09720    0.10102   0.962    0.342    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1939 on 38 degrees of freedom
# (151 observations deleted due to missingness)
# Multiple R-squared:  0.06725,	Adjusted R-squared:  0.01816 
# F-statistic:  1.37 on 2 and 38 DF,  p-value: 0.2664

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1 %>% filter(modeVerbType == "say-by-means")) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.52825    0.04290  12.314 5.16e-15 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.19823    0.05494  -3.608 0.000866 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1716 on 39 degrees of freedom
# Multiple R-squared:  0.2503,	Adjusted R-squared:  0.2311 
# F-statistic: 13.02 on 1 and 39 DF,  p-value: 0.0008656

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayByMeansVerbType, data = mean.proj.e1) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.55667    0.08403   6.625 9.01e-08 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.21875    0.06986  -3.131  0.00339 ** 
# sayByMeansVerbTypemanner                         -0.01195    0.07085  -0.169  0.86696    
# sayByMeansVerbTypesound                          -0.07780    0.10681  -0.728  0.47093    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1747 on 37 degrees of freedom
# (151 observations deleted due to missingness)
# Multiple R-squared:  0.2627,	Adjusted R-squared:  0.2029 
# F-statistic: 4.393 on 3 and 37 DF,  p-value: 0.009657

# sayByMeansVerbType adds barely anything to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * 
     fct_relevel(sayByMeansVerbType, "form"), data = mean.proj.e1) %>%
  summary()
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                                                                     0.49680    0.20331   2.444   0.0196 *
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                               -0.15889    0.19774  -0.804   0.4269  
# fct_relevel(sayByMeansVerbType, "form")manner                                                   0.05190    0.20962   0.248   0.8059  
# fct_relevel(sayByMeansVerbType, "form")sound                                                   -0.02991    0.18307  -0.163   0.8711  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:fct_relevel(sayByMeansVerbType, "form")manner -0.06864    0.21175  -0.324   0.7477  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:fct_relevel(sayByMeansVerbType, "form")sound        NA         NA      NA       NA  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1769 on 36 degrees of freedom
# (151 observations deleted due to missingness)
# Multiple R-squared:  0.2648,	Adjusted R-squared:  0.1831 
# F-statistic: 3.242 on 4 and 36 DF,  p-value: 0.02271


# M Monsterplots ----
## M.1 Valence ----
### valence + Vdir ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverbs-monsterplot-VDir-new.pdf", height = 6, width = 12)

### valence + Vdir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverbs-monsterplot-VDir-predType2-new.pdf", height = 8.5, width = 12)

### valence + Vdir + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-valence-sayverbs-monsterplot-VDir-Ddir-predType2-new.pdf", height = 15, width = 12)

## M.2 Arousal ----
### arousal ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverbs-monsterplot-new.pdf", height = 4.3, width = 12)

### arousal + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverbs-monsterplot-predType2-new.pdf", height = 5.5, width = 13)


### arousal + Vdir + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-arousal-sayverbs-monsterplot-Vdir-Ddir-predType2-new.pdf", height = 15, width = 12)


## M.3 Dominance ----
### dominance + Ddir ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverbs-monsterplot-Ddir-new.pdf", height = 6, width = 12)


### dominance + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverbs-monsterplot-Ddir-predType2-new.pdf", height = 8.5, width = 12)


### dominance + Vdir + Ddir + predicateType2 ----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-dominance-sayverbs-monsterplot-VDir-Ddir-predType2-new.pdf", height = 15, width = 12)


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
ggsave("../graphs/projection-by-VAD-lines-say-non-say-monsterplot-VDir-Ddir-new.pdf", height = 9, width = 6)

### sayVerbType ----
new.scale %>% 
  filter(predicateType == "communicative"& predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb, sayVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-sayVerbType-monsterplot-VDir-Ddir-new.pdf", height = 9, width = 7)

### modeVerbType ----
new.scale %>% 
  filter(predicateType == "communicative"& predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-modeVerbType-monsterplot-VDir-Ddir-new.pdf", height = 8.5, width = 8.5)

### sayByMeansVerbType (all sayVerb categories) ----
new.scale %>% 
  filter(predicateType == "communicative"& predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(V.Mean.Sum2.direction, D.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType,
                    sayByMeansVerbType), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-sayByMeansVerbType-monsterplot-VDir-Ddir-new.pdf", height = 9, width = 12)

## VAD + Vdir + Ddir +predicateType2 ----
# fitted lines for all three VAD ratings faceted by Vdir, Ddir and predicateType2
### all sayVerb categories -----
new.scale %>% 
  filter(predicateType == "communicative" & predicate != "say") %>% 
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
              filter(predicateType == "communicative" & predicate != "say") %>% 
              mutate(sayVerbType = ifelse(is.na(sayVerbType), "", sayVerbType),
                     modeVerbType = ifelse(is.na(modeVerbType), "", modeVerbType),
                     sayByMeansVerbType = ifelse(is.na(sayByMeansVerbType), "", sayByMeansVerbType)) %>% 
              count(predicateType2, V.Mean.Sum2.direction, sayVerb, sayVerbType, modeVerbType, sayByMeansVerbType, D.Mean.Sum2.direction), 
            aes(x = Inf, y = -.97, label = paste0("n = ", n)), hjust = 1.04)
ggsave("../graphs/projection-by-VAD-lines-sayverbs-monsterplot-VDir-Ddir-predType2-new.pdf", height = 15, width = 12)


# G CCs ----
## G.1 overall ----
### by CC ----
#### plot ----
mean.proj.CC <- e1 %>%
  group_by(CC) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         CC = fct_reorder(as.factor(CC), Mean.Proj))
nrow(mean.proj.CC) # 11

mean.proj.CC %>% 
  ggplot(aes(x = CC, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "CC",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) 
ggsave("../graphs/projection-by-CC.pdf", height = 5, width = 6)

#### linear model ----
lm(rating ~ fct_relevel(CC, "the airbag deployed"), e1.VAD.CC) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.30290    0.04344   6.972 3.83e-12 ***
# fct_relevel(CC, "the gate opened")the airbag deployed     0.06070    0.06055   1.002  0.31622    
# fct_relevel(CC, "the gate opened")the balloon popped      0.16528    0.06104   2.708  0.00681 ** 
# fct_relevel(CC, "the gate opened")the bolt loosened       0.03268    0.06168   0.530  0.59625    
# fct_relevel(CC, "the gate opened")the computer restarted  0.06526    0.06115   1.067  0.28594    
# fct_relevel(CC, "the gate opened")the egg cracked         0.07371    0.06060   1.216  0.22400    
# fct_relevel(CC, "the gate opened")the factory closed      0.13164    0.06132   2.147  0.03190 *  
# fct_relevel(CC, "the gate opened")the knot tightened      0.06199    0.06076   1.020  0.30772    
# fct_relevel(CC, "the gate opened")the paper burned        0.11569    0.06098   1.897  0.05791 .  
# fct_relevel(CC, "the gate opened")the room darkened       0.04500    0.06093   0.739  0.46026    
# fct_relevel(CC, "the gate opened")the sauce thickened     0.08283    0.06050   1.369  0.17103 

# The mean projection rating of "the gate opened" (lowest mean projection rating) is significantly
# lower than that of "the balloon popped" (highest) and "the factory closed" (second highest). For
# the CC with the second lowest mean projection rating, "the airbag deployed", no differences are
# significant.

e1.VAD.CC %>% 
  filter(CC.subject %in% c("gate", "factory", "balloon")) %>% 
  distinct(CC.subject, CC.sub.Vdir, CC.sub.Ddir)
#   CC.subject CC.sub.Vdir CC.sub.Ddir
# 1       gate    positive    positive
# 2    balloon    positive    positive
# 3    factory    negative    positive

### by CC and predicate type ----
#### plot ----
mean.proj.CC2 <- e1 %>%
  filter(predicateType == "communicative") %>% 
  group_by(CC, predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         CC = fct_reorder(as.factor(CC), Mean.Proj))
nrow(mean.proj.CC) # 22

mean.proj.CC2 %>% 
  ggplot(aes(x = CC, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "CC",
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-CC-predType2.pdf", height = 5.5, width = 8.7)

#### linear model ----
lm(rating ~ fct_relevel(CC, "the balloon popped") * fct_relevel(predicateType2, "nonEmoComm"), 
   e1.VAD.CC %>% filter(predicateType == "communicative")) %>% 
  summary()
# CC                      predicateType2  significantly different from
# the airbag deployed     emoComm         n/a                               
# the balloon popped      emoComm         n/a                                                       
# the bolt loosened       emoComm         n/a                                                           
# the computer restarted  emoComm         n/a                                                            
# the egg cracked         emoComm         n/a                                                           
# the factory closed      emoComm         n/a                                                           
# the gate opened         emoComm         n/a                                                             
# the knot tightened      emoComm         n/a                                                            
# the paper burned        emoComm         n/a                                                            
# the room darkened       emoComm         n/a                                                            
# the sauce thickened     emoComm         n/a                                                            
# the airbag deployed     nonEmoComm      balloon *                                                            
# the balloon popped      nonEmoComm      airbag *, bolt ** , computer ** , gate ** , room **                                 
# the bolt loosened       nonEmoComm      balloon **                                   
# the computer restarted  nonEmoComm      balloon **                                      
# the egg cracked         nonEmoComm      n/a                                   
# the factory closed      nonEmoComm      gate *                                   
# the gate opened         nonEmoComm      balloon **, factory *                                  
# the knot tightened      nonEmoComm      n/a                                   
# the paper burned        nonEmoComm      n/a                                   
# the room darkened       nonEmoComm      balloon **                                   
# the sauce thickened     nonEmoComm      n/a                                   

## G.2 distribution ----
### How many predicate was each CC combined with?
e1.VAD.CC %>% 
  distinct(CC, predicate) %>%
  group_by(CC) %>%
  count() %>% 
  ungroup() %>% 
  mutate(outlier = n < (quantile(n, 0.25) - 1.5 * (quantile(n, 0.75) - quantile(n, 0.25))) | 
      n > (quantile(n, 0.75) + 1.5 * (quantile(n, 0.75) - quantile(n, 0.25))))
# CC                         n outlier
#   <chr>                  <int> <lgl>  
# 1 the airbag deployed      128 FALSE  
# 2 the balloon popped       130 FALSE  
# 3 the bolt loosened        132 FALSE  
# 4 the computer restarted   130 FALSE  
# 5 the egg cracked          142 TRUE 
# 6 the factory closed       124 FALSE  
# 7 the gate opened          126 FALSE  
# 8 the knot tightened       132 FALSE  
# 9 the paper burned         137 FALSE  
# 10 the room darkened        134 FALSE  
# 11 the sauce thickened      134 FALSE  

# With uneven number of ratings per predicate (8-31) the numbers of combinations with each CC also
# vary. A particularly high number of predicates was combined with the CC "the egg cracked" (more
# than 3 SDs above the interquartile range. 

## G.3 VAD rating of CC ----
### tables ----
e1.VAD.CC %>% 
  distinct(CC, CC.sub.V, CC.sub.D, CC.sub.Vdir, CC.sub.Ddir, CC.pred.V, CC.pred.D, CC.pred.Vdir, CC.pred.Ddir)
#                        CC CC.sub.V CC.sub.D CC.sub.Vdir CC.sub.Ddir CC.pred.V CC.pred.D CC.pred.Vdir CC.pred.Ddir
# 1       the bolt loosened     3.90     4.64    negative    negative      5.00      6.00     positive     positive
# 2         the gate opened     5.32     5.95    positive    positive      6.14      6.17     positive     positive
# 3  the computer restarted     6.84     6.58    positive    positive      4.40      6.63     negative     positive
# 4     the sauce thickened     6.58     5.85    positive    positive      4.63      5.72     negative     positive
# 5       the room darkened     5.55     5.71    positive    positive      4.45      5.76     negative     positive
# 6      the knot tightened     4.57     6.04    negative    positive      5.00      6.08     positive     positive
# 7         the egg cracked     5.95     5.74    positive    positive      2.68      5.42     negative     positive
# 8     the airbag deployed     5.40     4.81    positive    negative      4.25      4.90     negative     negative
# 9      the balloon popped     6.84     5.45    positive    positive      6.05      5.68     positive     positive
# 10     the factory closed     4.95     5.09    negative    positive      5.23      5.94     positive     positive
# 11       the paper burned     5.42     5.64    positive    positive      3.73      3.90     negative     negative

# frequencies of VAD combination in the CCs
# valence
e1.VAD.CC %>% 
  group_by(CC) %>%
  slice_head() %>%
  ungroup() %>%
  count(CC.sub.Vdir, CC.pred.Vdir)
#   CC.sub.Vdir CC.pred.Vdir     n
#   <chr>       <chr>        <int>
# 1 negative    positive         3
# 2 positive    negative         6
# 3 positive    positive         2

# dominance
e1.VAD.CC %>% 
  group_by(CC) %>%
  slice_head() %>%
  ungroup() %>%
  count(CC.sub.Ddir, CC.pred.Ddir)
#   CC.sub.Ddir CC.pred.Ddir     n
#   <chr>       <chr>        <int>
# 1 negative    negative         1
# 2 negative    positive         1
# 3 positive    negative         1
# 4 positive    positive         8

# frequency of VAD of matrix clause predicate and VAD of of CC
# valence
e1.VAD.CC %>% 
  group_by(CC.sub.Vdir, CC.pred.Vdir, predicate) %>%
  slice_head() %>%
  ungroup() %>%
  count(CC.sub.Vdir, CC.pred.Vdir, V.Mean.Sum2.direction)
#   CC.sub.Vdir CC.pred.Vdir V.Mean.Sum2.direction     n
#   <chr>       <chr>        <chr>                 <int>
# 1 negative    positive     negative                 72
# 2 negative    positive     positive                 89
# 3 positive    negative     negative                 73
# 4 positive    negative     positive                 91
# 5 positive    positive     negative                 71
# 6 positive    positive     positive                 85

# valence
e1.VAD.CC %>% 
  group_by(CC.sub.Ddir, CC.pred.Ddir, predicate) %>%
  slice_head() %>%
  ungroup() %>%
  count(CC.sub.Ddir, CC.pred.Ddir, D.Mean.Sum2.direction)
#   CC.sub.Ddir CC.pred.Ddir D.Mean.Sum2.direction     n
#   <chr>       <chr>        <chr>                 <int>
# 1 negative    negative     negative                 33
# 2 negative    negative     positive                 95
# 3 negative    positive     negative                 39
# 4 negative    positive     positive                 93
# 5 positive    negative     negative                 43
# 6 positive    negative     positive                 94
# 7 positive    positive     negative                 47
# 8 positive    positive     positive                117

### plots ----
# Vdir
mean.proj.Vdir <- e1.VAD.CC %>%
  filter(predicateType == "communicative") %>% 
  group_by(CC.sub.Vdir, CC.pred.Vdir) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) 
nrow(mean.proj.Vdir) # 3

mean.proj.Vdir %>% 
  ggplot(aes(x = interaction(CC.sub.Vdir, CC.pred.Vdir), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = 1),
        axis.text = element_text(size = 12),
       # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Direction of valence",
       y = "Mean projection rating",
       colour = "predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = c("positive.negative" = "subject: positive\npredicate: negative", 
                              "negative.positive" = "subject: negative\npredicate: positive", 
                              "positive.positive" = "subject: positive\npredicate: positive")) 
ggsave("../graphs/projection-by-CC-Vdir.pdf", height = 5, width = 6)

# with valence direction of matrix clause predicate
mean.proj.Vdir.both <- e1.VAD.CC %>%
  filter(predicateType == "communicative") %>% 
  group_by(CC.sub.Vdir, CC.pred.Vdir, V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) 
nrow(mean.proj.Vdir.both) # 6

mean.proj.Vdir.both %>% 
  ggplot(aes(x = interaction(CC.sub.Vdir, CC.pred.Vdir), y = Mean.Proj, colour = V.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = 1),
        axis.text = element_text(size = 12),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Direction of valence",
       y = "Mean projection rating",
       colour = "Matrix clause predicate direction of valence") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = c("positive.negative" = "subject: positive\npredicate: negative", 
                              "negative.positive" = "subject: negative\npredicate: positive", 
                              "positive.positive" = "subject: positive\npredicate: positive")) 
ggsave("../graphs/projection-by-CC-Vdir-both.pdf", height = 5, width = 6)


# Ddir
mean.proj.Ddir <- e1.VAD.CC %>%
  filter(predicateType == "communicative") %>% 
  group_by(CC.sub.Ddir, CC.pred.Ddir) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) 
nrow(mean.proj.Ddir) # 4

mean.proj.Ddir %>% 
  ggplot(aes(x = interaction(CC.sub.Ddir, CC.pred.Ddir), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = 1),
        axis.text = element_text(size = 12),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Direction of dominance",
       y = "Mean projection rating",
       colour = "predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = c("negative.negative" = "subject: negative\npredicate: negative", 
                              "positive.negative" = "subject: positive\npredicate: negative", 
                              "negative.positive" = "subject: negative\npredicate: positive", 
                              "positive.positive" = "subject: positive\npredicate: positive")) 
ggsave("../graphs/projection-by-CC-Ddir.pdf", height = 5, width = 7.5)

# with dominance direction of matrix clause predicate
mean.proj.Ddir.both <- e1.VAD.CC %>%
  filter(predicateType == "communicative") %>% 
  group_by(CC.sub.Ddir, CC.pred.Ddir, D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh) 
nrow(mean.proj.Ddir.both) # 8

mean.proj.Ddir.both %>% 
  ggplot(aes(x = interaction(CC.sub.Ddir, CC.pred.Ddir), y = Mean.Proj, colour = D.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0,
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = 1),
        axis.text = element_text(size = 12),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank()) +
  labs(x = "Direction of dominance",
       y = "Mean projection rating",
       colour = "Matrix clause predicate direction of dominance") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = c("negative.negative" = "subject: negative\npredicate: negative", 
                              "positive.negative" = "subject: positive\npredicate: negative", 
                              "negative.positive" = "subject: negative\npredicate: positive", 
                              "positive.positive" = "subject: positive\npredicate: positive")) 
ggsave("../graphs/projection-by-CC-Ddir-both.pdf", height = 5, width = 7.5)

# presentation plots ----
## by predicate type MV neg only vs new data ----
comms.MV.neg <- d.proj.neg %>%
  filter(predicateType == "communicative") %>%
  group_by(predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>%
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

comms.new.blank <- 
  e1 %>%
  filter(predicateType == "communicative") %>%
  group_by(predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>%
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
  scale_colour_manual(values = c("white", "white"))

comms.new <- e1 %>%
  filter(predicateType == "communicative") %>%
  group_by(predicateType2) %>%
  summarise(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>%
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

# MV
(comms.MV.neg + labs(title = "MegaVeridicality data") + plot_spacer()) + 
  plot_layout(axis_titles = "collect") &
  theme(plot.title = element_text(hjust = 0.01),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12))
ggsave("../graphs/projection-by-predicateType2-MV.jpeg", height = 4, width = 9)

# combine MV and new data blank
(comms.MV.neg + labs(title = "MegaVeridicality data") + comms.new.blank + labs(title = "New data")) + 
  plot_layout(axis_titles = "collect") &
  theme(plot.title = element_text(hjust = 0.01),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12))
ggsave("../graphs/projection-by-predicateType2-comparison-blank.jpeg", height = 4, width = 9)

# combine MV and new data
(comms.MV.neg + labs(title = "MegaVeridicality data") + comms.new + labs(title = "New data")) + 
  plot_layout(axis_titles = "collect") &
  theme(plot.title = element_text(hjust = 0.01),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12))
ggsave("../graphs/projection-by-predicateType2-comparison.jpeg", height = 4, width = 9)

## by predicate ----
### MV neg only vs new ----
commProjMVneg <- 
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate),  
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered3"))


projNewBlank <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1))

projNew <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered3"))

# combine MV and new data blank plots 
(commProjMVneg / projNewBlank) + 
  plot_layout(axis_titles = "collect") &
  theme(plot.title = element_text(hjust = 0.01),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12))
ggsave("../graphs/projection-by-communicative-comparison-neg-blank.jpeg", height = 6, width = 10)

# combine MV and new data plots 
(commProjMVneg / projNew) + 
  plot_layout(axis_titles = "collect") &
  theme(plot.title = element_text(hjust = 0.01),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12))
ggsave("../graphs/projection-by-communicative-comparison-neg.jpeg", height = 6, width = 10)

### with emoComms ---- 
#### legend plot ----
legend_plot <- 
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating",
       colour = "Type of communicative") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("#009A00", "deepskyblue2"), limits = c("emoComm", "nonEmoComm"), 
                      labels = c("with emotion entailment", "without emotion entailment"))

#### with points MV neg only vs new ----  
emoPointsMVneg <- 
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "nonEmoComm"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"),
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.3,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate),  
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("nonEmoComm", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "#009A00", "blue", "deeppink", "orangered3"))


emoPointsBlank <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1))

emoPointsNew <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "nonEmoComm"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"),
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.3,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.1, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("nonEmoComm", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "#009A00", "blue", "deeppink", "orangered3"))

# combine MV and new data blank plots 
wrap_elements(get_legend(legend_plot)) / ((emoPointsMVneg / emoPointsBlank) + 
                           plot_layout(axis_titles = "collect") &
                           theme(plot.title = element_text(hjust = 0.01),
                                 axis.title = element_text(size = 18),
                                 axis.title.x = element_text(vjust = -.5),
                                 axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-point-comparison-blank.jpeg", height = 6, width = 10)

# combine MV and new data plots 
wrap_elements(get_legend(legend_plot)) / ((emoPointsMVneg / emoPointsNew) + 
                                            plot_layout(axis_titles = "collect") &
                                            theme(plot.title = element_text(hjust = 0.01),
                                                  axis.title = element_text(size = 18),
                                                  axis.title.x = element_text(vjust = -.5),
                                                  axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-point-comparison.jpeg", height = 6, width = 10)


#### with labels MV neg only vs new ----
projMVnegEmo <-   
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "#009A00") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.3,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "#009A00", "blue", "deeppink", "orangered3"))

projNewEmo <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.6,
                   colour = "#009A00") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "say"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.3,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "think"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = 0.3,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate == "know"),
                   aes(label = predicate), 
                   min.segment.length = 0,
                   nudge_x = -7, nudge_y = 0.2,
                   colour = "orangered3") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "#009A00", "blue", "deeppink", "orangered3"))

# combine MV and new data plots 
wrap_elements(get_legend(legend_plot)) / ((projMVnegEmo / projNewEmo) + 
                                            plot_layout(axis_titles = "collect") &
                                            theme(plot.title = element_text(hjust = 0.01),
                                                  axis.title = element_text(size = 18),
                                                  axis.title.x = element_text(vjust = -.5),
                                                  axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-labels-comparison.jpeg", height = 6, width = 10)


### emoComm ranking differences ----
mean.proj.emoComm.rankings <- 
  mean.proj.e1.plus %>%
  filter(predicateType2 == "emoComm") %>%  
    mutate(rankingNew = rank(Mean.Proj),
         rankingMVneg = rank(Mean.Proj.MV.neg),
         ranking_difference = abs(rankingNew - rankingMVneg))

# highest rankings
similar_high_ranking_emo <- function(number){
  for (x in 1:nrow(mean.proj.emoComm.rankings %>% filter(predicateType2 == "emoComm"))) {
    
    top_ranking_new <- mean.proj.emoComm.rankings %>%
      filter(predicateType2 == "emoComm") %>%   
      slice_max(order_by = rankingNew, n = x) %>% 
      pull(predicate)
    top_ranking_MV_neg <- mean.proj.emoComm.rankings %>%
      filter(predicateType2 == "emoComm") %>%    
      slice_max(order_by = rankingMVneg, n = x) %>% 
      pull(predicate)
    
    common_predicates <- intersect(top_ranking_new, top_ranking_MV_neg)
    
    if (length(common_predicates) >= number) {
      break  
    }
  }
  a <- mean.proj.emoComm.rankings %>% 
    filter(predicate %in% common_predicates) %>% 
    arrange(ranking_difference) %>% 
    select(predicate, Mean.Proj, Mean.Proj.MV.neg, rankingNew, rankingMVneg, ranking_difference)
  
  print(a, n = Inf)
  
  cat("\n", paste0('"', as.character(a$predicate), '",'))

  if (nrow(a) > number) {
    cat("\n\nselected number:", number, "; number of predicates:", nrow(a), "; size of subset:", x,"\n\nThe number of predicates in the output is larger than the selected number because of the\nties in the MV dataset (negation-only) and the fact that the for-loop can add multiple\npredicates to the list in one iteration.")
  } else {
    cat("\n\nselected number:", number, "; number of predicates:", nrow(a), "; size of subset:", x)
  }
}

similar_high_ranking_emo(9)

top_similar_nine <- c("fuss", "weep", "whine", "gloat", "bitch", "complain", "pout", "groan", "grumble")

# lowest rankings
similar_low_ranking_emo <- function(number){
  for (x in 1:nrow(mean.proj.emoComm.rankings %>% filter(predicateType2 == "emoComm"))) {
    
    bottom_ranking_new <- mean.proj.emoComm.rankings %>%
      filter(predicateType2 == "emoComm") %>%   
      slice_min(order_by = rankingNew, n = x) %>% 
      pull(predicate)
    bottom_ranking_MV_neg <- mean.proj.emoComm.rankings %>%
      filter(predicateType2 == "emoComm") %>%    
      slice_min(order_by = rankingMVneg, n = x) %>% 
      pull(predicate)
    
    common_predicates <- intersect(bottom_ranking_new, bottom_ranking_MV_neg)
    
    if (length(common_predicates) >= number) {
      break  
    }
  }
  a <- mean.proj.emoComm.rankings %>% 
    filter(predicate %in% common_predicates) %>% 
    arrange(ranking_difference) %>% 
    select(predicate, Mean.Proj, Mean.Proj.MV.neg, rankingNew, rankingMVneg, ranking_difference)
  
  print(a, n = Inf)
  
  cat("\n", paste0('"', as.character(a$predicate), '",'))
  
  if (nrow(a) > number) {
    cat("\n\nselected number:", number, "; number of predicates:", nrow(a), "; size of subset:", x,"\n\nThe number of predicates in the output is larger than the selected number because of the\nties in the MV dataset (negation-only) and the fact that the for-loop can add multiple\npredicates to the list in one iteration.")
  } else {
    cat("\n\nselected number:", number, "; number of predicates:", nrow(a), "; size of subset:", x)
  }
}

similar_low_ranking_emo(9)

bottom_similar_nine <- c("exclaim", "scream", "boast", "mutter", "shriek", "squeal", "howl", "rave", "whimper")

least_similar <- 
  mean.proj.emoComm.rankings %>% 
    filter(predicateType2 == "emoComm" & ranking_difference >= 9) %>% 
    arrange(ranking_difference) %>% 
    select(predicate) %>%
    unlist() %>% 
    as.vector() %>% print()

least_similar10 <- 
  mean.proj.emoComm.rankings %>% 
  filter(predicateType2 == "emoComm" & ranking_difference >= 10) %>% 
  arrange(ranking_difference) %>% 
  select(predicate) %>%
  unlist() %>% 
  as.vector() %>% print()

most_similar2 <- 
  mean.proj.emoComm.rankings %>% 
  filter(predicateType2 == "emoComm" & ranking_difference <= 2) %>% 
  arrange(ranking_difference) %>% 
  select(predicate) %>%
  unlist() %>% 
  as.vector() 

most_similar4 <- 
  mean.proj.emoComm.rankings %>% 
  filter(predicateType2 == "emoComm" & ranking_difference <= 4) %>% 
  arrange(ranking_difference) %>% 
  select(predicate) %>%
  unlist() %>% 
  as.vector() 

#### with labels most similar 2 only ----
projMVnegEmo_mostSim2 <-   
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate %in% most_similar2),
                   aes(label = predicate), 
                   fill = "khaki2",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.3,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.MV.neg %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

projNewEmo_mostSim2 <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate %in% most_similar2),
                   aes(label = predicate), 
                   fill = "khaki2",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.3,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.e1 %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

# combine MV and new data plots 
wrap_elements(get_legend(legend_plot)) / ((projMVnegEmo_mostSim2 / projNewEmo_mostSim2) + 
                                            plot_layout(axis_titles = "collect") &
                                            theme(plot.title = element_text(hjust = 0.01),
                                                  axis.title = element_text(size = 18),
                                                  axis.title.x = element_text(vjust = -.5),
                                                  axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-labels-comparison-most-similar2.jpeg", height = 6, width = 10)


#### with labels most similar 4 only ----
projMVnegEmo_mostSim4 <-   
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate %in% most_similar4),
                   aes(label = predicate), 
                   fill = "khaki2",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.4,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.MV.neg %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

projNewEmo_mostSim4 <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate %in% most_similar4),
                   aes(label = predicate), 
                   fill = "khaki2",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.4,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.e1 %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

# combine MV and new data plots 
wrap_elements(get_legend(legend_plot)) / ((projMVnegEmo_mostSim4 / projNewEmo_mostSim4) + 
                                            plot_layout(axis_titles = "collect") &
                                            theme(plot.title = element_text(hjust = 0.01),
                                                  axis.title = element_text(size = 18),
                                                  axis.title.x = element_text(vjust = -.5),
                                                  axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-labels-comparison-most-similar4.jpeg", height = 6, width = 10)


#### with labels least similar 9 only----
projMVnegEmo_leastSim <-   
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate %in% least_similar),
                   aes(label = predicate), 
                   fill = "pink",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.3,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.MV.neg %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

projNewEmo_leastSim <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate %in% least_similar),
                   aes(label = predicate), 
                   fill = "pink",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.3,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.e1 %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

# combine MV and new data plots 
wrap_elements(get_legend(legend_plot)) / ((projMVnegEmo_leastSim / projNewEmo_leastSim) + 
                                            plot_layout(axis_titles = "collect") &
                                            theme(plot.title = element_text(hjust = 0.01),
                                                  axis.title = element_text(size = 18),
                                                  axis.title.x = element_text(vjust = -.5),
                                                  axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-labels-comparison-least-similar.jpeg", height = 6, width = 10)


#### with labels least similar 10 only----
projMVnegEmo_leastSim10 <-   
  ggplot(mean.proj.MV.neg, aes(x = fct_reorder(as.factor(predicate), Mean.Proj), y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(predicate %in% least_similar10),
                   aes(label = predicate), 
                   fill = "pink",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.3,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.MV.neg %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "MegaVeridicality data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

projNewEmo_leastSim10 <- 
  ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicateType2 == "emoComm"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComm")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate %in% least_similar10),
                   aes(label = predicate), 
                   fill = "pink",
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -.3,
                   colour = "#009A00") +
  # geom_label_repel(data = mean.proj.e1 %>% 
  #                    filter(predicateType2 == "emoComm" & ! predicate %in% least_similar),
  #                  aes(label = predicate), 
  #                  min.segment.length = 0,
  #                  nudge_x = 0.2, nudge_y = -0.6,
  #                  colour = "#009A00") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "New data",
       x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "emoComm"),
                      values = c("deepskyblue2", "#009A00"))

# combine MV and new data plots 
wrap_elements(get_legend(legend_plot)) / ((projMVnegEmo_leastSim10 / projNewEmo_leastSim10) + 
                                            plot_layout(axis_titles = "collect") &
                                            theme(plot.title = element_text(hjust = 0.01),
                                                  axis.title = element_text(size = 18),
                                                  axis.title.x = element_text(vjust = -.5),
                                                  axis.text = element_text(size = 12))) + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emo-labels-comparison-least-similar10.jpeg", height = 6, width = 10)
