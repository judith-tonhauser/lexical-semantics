# Testing hypotheses based on White & Rawlins' MegaVeridicality I dataset 
# analysis3.R

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
library(cowplot)
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
d.proj <- droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) # 16291

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
         dynamicity = case_when(stative_predicate == "yes" ~ "stative",
                                dynamic_predicate == "yes" ~ "dynamic",
                                TRUE ~ "unclear"),
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         activity = case_when(activity_predicate == "yes" ~ "yes",
                              TRUE ~ "no"),
         evidenceType = case_when(perceptual_evidence == "yes" ~ "perceptual",
                                  reportative_evidence == "yes" ~	"reportative",
                                  inferential_evidence == "yes" ~ "inferential",
                                  TRUE ~ NA),
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

d.proj %>% 
  group_by(polarity, conditional2) %>% 
  count()
#   polarity conditional2     n
#   <chr>    <chr>        <int>
# 1 negative conditional   5440
# 2 negative matrix        5411
# 3 positive conditional   5440

# calculate by-predicate projection means 
mean.proj.all <- d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
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
           emoCommType, dynamicity, changeOfState, activity, evidenceType, volition, sayVerb, 
           sayVerbType, modeVerbType, sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, commType, 
           emoCommType, dynamicity, changeOfState, activity, evidenceType, volition, sayVerb, 
           sayVerbType, modeVerbType, sayByMeansVerbType)
nrow(tmp) # 544

mean.proj.all <- left_join(mean.proj.all, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

mean.proj.all %>%
  group_by(predicateType) %>%
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

# Subsets for all predicates / communicative predicates with mean acceptability
# ratings greater than 4. See "Acceptability" below for the reasons for and the
# consequences of examining only these subsets.

# acceptable predicates
acc.preds <- mean.proj %>%
  filter(Mean.Acc > 4) %>% 
  distinct(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
length(acc.preds) # 474

# acceptable communicatives
acc.comms <- mean.proj %>%
  filter(predicateType == "communicative" & Mean.Acc > 4) %>% 
  distinct(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
length(acc.comms) # 201

# by-predicate projection means for "acceptable" predicates
mean.proj.acc <- mean.proj %>% 
  filter(Mean.Acc > 4)
nrow(mean.proj.acc) # 474

# by-predicate projection means for "acceptable" communicatives
mean.proj.comm <- mean.proj.acc %>%
  filter(predicateType == "communicative")
nrow(mean.proj.comm) # 201

d.proj.acc <- d.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(d.proj.acc) # 14197

d.proj.comm <- d.proj.acc %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 6022

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

# by embedding environment
# calculate by-predicate projection means for MV data
mean.proj.all.env <- d.proj %>%
  group_by(verb_renamed, environment) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(verb_renamed), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.all.env) # 1632

tmp <- d.proj %>%
  select(c(verb_renamed, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           dynamicity, changeOfState, activity, evidenceType, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType, environment)) %>%
  distinct(verb_renamed, predicateType, predicateType2, emotiveComponent, commType, emoCommType, 
           dynamicity, changeOfState, activity, evidenceType, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType, environment)
nrow(tmp) # 1632

mean.proj.all.env <- left_join(mean.proj.all.env, tmp, by = c("verb_renamed", "environment")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all.env) # 1632

# how many predicates in which environment?
mean.proj.all.env %>%
  select(environment, verb_renamed) %>% 
  unique() %>% 
  group_by(environment) %>% 
  summarise(count = n())
#   environment count
#   <chr>       <int>
# 1 neg           544
# 2 qcond         544
# 3 qcondneg      544

# exclude "comPriv" and "other" predicates from analysis.
mean.proj.env <- mean.proj.all.env %>% 
  filter(! predicateType %in% c("comPriv", "other"))
nrow(mean.proj.env) # 1509  

# only 'acceptable' predicates
mean.proj.acc.env <- mean.proj.env %>% 
  filter(verb_renamed %in% as.vector(acc.preds))
nrow(mean.proj.acc.env) # 1422


## valence and arousal data (Warriner et al. 2013) ----
# load data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  rename(verb = Word) %>%
  inner_join(d.proj.acc, by = "verb")
nrow(w2) # 11622
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

# Warriner et al.'s (2013) dataset contains valence, arousal and dominance ratings for 388 of the 
# predicates in the MV dataset which belong to (only) one of our 5 predicate types and have a mean 
# acceptability rating greater than 4. 

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

# Warriner et al.'s (2013) participants only saw lemmata. In many cases, these could have been 
# interpreted as either verbs or nouns.  Because according to Warriner et al. (2013: 1192) "it 
# would be expected that the emotional values would generalize to inflected forms (e.g., sings, 
# sang, sung, and singing for the verb lemma sing)", I also assume that as long as a verb and noun
# are largely congruent in meaning, they would receive very similar VAD ratings. If, however, the
# meaning of a noun (or one of its possible meanings) is clearly distinct from that of the verb,
# differences in VAD ratings depending on how individual participants interpreted the lemma would 
# have to be expected. The ratings for these lemmata would therefore not be sufficiently informative
# wrt to the emotivity of the verb specifically. These lemmata therefore have to be excluded from 
# the present analysis.

# mean.proj.vad %>% 
#   select(verb_renamed)

# nouns whose meaning is (or can be) clearly different from that of the corresponding verb: 
# address*, figure*, intercept, log*, notice*, post*, share*, spot*, state*, stress*, type*.
# * These nouns have at least two meanings, at least one of which is clearly different from the 
# meaning of the verb.
# >>> these predicates are excluded below.

# nouns whose meaning is clearly closely related to that of the corresponding verb: 
# answer, attest, bitch, blog, broadcast, care, challenge, chant, charge, chatter, check, cheer, 
# claim, comment, conjecture, contest, cry, daydream, debate, decree, demand, desire, despair, 
# detail, dislike, display, dispute, disregard, document, doubt, dread, dream, email, envy, 
# estimate, exhibit, fake, fax, fear, find, forecast, fuss, gossip, grunt, guarantee, hate, hint, 
# hope, howl, jest, joke, judge, leak, lecture, lie, like, love, marvel, mention, murmur, panic, 
# phone, picture, praise, promise,  quarrel, question, radio, rant, reason, recap, record, regret, 
# remark, reply, report, request, research, resolve, rule, scream, shout, show, showcase, shriek, 
# sigh, signal, sketch, snitch, sob, spout, squeal, suspect, tease, trust, tweet, videotape, voice, 
# vow, wager, whisper, will, wish, witness, worry.
# >>> these are not excluded.

mean.proj.vad %>% 
  filter(verb_renamed %in% c("address", "figure", "intercept", "log", "notice", "post", "share", 
                             "spot", "state", "stress", "type")) %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count() 
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          1
# 2 evidential         3
# 3 nonEmoComm         7

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

# X.01 rescale V + A + D ratings
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

# library(languageR)
# pairscor.fnc(new.scale[,c("V.Mean.Sum", "V.Mean.Sum2", "A.Mean.Sum", "A.Mean.Sum2", "D.Mean.Sum", "D.Mean.Sum2")])
# pairscor.fnc(new.scale[,c("V.Mean.Sum.sc", "V.Mean.Sum2.sc", "A.Mean.Sum.sc", "A.Mean.Sum2.sc", "D.Mean.Sum.sc", "D.Mean.Sum2.sc")])

VAD_orig <- ggplot(new.scale) +
  geom_vline(xintercept = 5, linetype = 2, colour = "grey50") +
  geom_density(aes(V.Mean.Sum, colour = "valence"), size = 1, trim = TRUE) +
  geom_density(aes(A.Mean.Sum, colour = "arousal"), size = 1, trim = TRUE) +
  geom_density(aes(D.Mean.Sum, colour = "dominance"), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Original ratings",
       x = "Valence/arousal/dominance rating",
       y = "Density") +
  scale_x_continuous(limits = c(1, 9), breaks = seq(1, 9, 1)) + 
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      #labels = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))
#ggsave("../graphs/VAD-distribution.pdf", height = 6, width = 6)

VAD_sc <- 
ggplot(new.scale) +
  geom_density(aes(V.Mean.Sum.sc, colour = "valence"), size = 1, trim = TRUE) +
  geom_density(aes(A.Mean.Sum.sc, colour = "arousal"), size = 1, trim = TRUE) +
  geom_density(aes(D.Mean.Sum.sc, colour = "dominance"), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Original ratings\ncentred and scaled",
       x = "Valence/arousal/dominance rating",
       y = "Density") +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))

VAD_2 <- 
ggplot(new.scale) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_density(aes(V.Mean.Sum2, colour = "valence"), size = 1, trim = TRUE) +
  geom_density(aes(A.Mean.Sum2, colour = "arousal"), size = 1, trim = TRUE) +
  geom_density(aes(D.Mean.Sum2, colour = "dominance"), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Adjusted scales\nwith split valence and dominance",
       x = "Valence/arousal/dominance rating",
       y = "Density") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))

VAD_2_sc <- 
  ggplot(new.scale) +
  geom_density(aes(V.Mean.Sum2.sc, colour = "valence"), size = 1, trim = TRUE) +
  geom_density(aes(A.Mean.Sum2.sc, colour = "arousal"), size = 1, trim = TRUE) +
  geom_density(aes(D.Mean.Sum2.sc, colour = "dominance"), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Adjusted scales\nwith split valence and dominance\ncentred and scaled",
       x = "Valence/arousal/dominance rating",
       y = "Density") +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))

VAD_orig + VAD_sc + VAD_2 + VAD_2_sc + 
  plot_layout(ncol = 2, axis_titles = "collect", guides = "collect") &
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 18))
#ggsave("../graphs/VAD-scale-comparison.pdf", height = 6, width = 8)

VAD_2_Dcont <- 
  ggplot(new.scale) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_density(aes(V.Mean.Sum2, colour = "valence"), size = 1, trim = TRUE) +
  geom_density(aes(A.Mean.Sum2, colour = "arousal"), size = 1, trim = TRUE) +
  geom_density(aes(D.Mean.Sum2.cont, colour = "dominance"), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Adjusted scales\nwith split valence",
       x = "Valence/arousal/dominance rating",
       y = "Density") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))

VAD_2_Dcont_sc <- 
  ggplot(new.scale) +
  geom_density(aes(V.Mean.Sum2.sc, colour = "valence"), size = 1, trim = TRUE) +
  geom_density(aes(A.Mean.Sum2.sc, colour = "arousal"), size = 1, trim = TRUE) +
  geom_density(aes(D.Mean.Sum2.cont.sc, colour = "dominance"), size = 1, trim = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Adjusted scales\nwith split valence\ncentred and scaled",
       x = "Valence/arousal/dominance rating",
       y = "Density") +
  scale_colour_manual(breaks = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))

VAD_orig + VAD_sc + VAD_2_Dcont + VAD_2_Dcont_sc + VAD_2 + VAD_2_sc + 
  plot_layout(ncol = 2, axis_titles = "collect", guides = "collect") &
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 18))
#ggsave("../graphs/VAD-scale-comparison2.pdf", height = 9, width = 8)

# ranges of original values
range(new.scale$V.Mean.Sum)                             # 1.40 8.21
vdiff <- diff(range(new.scale$V.Mean.Sum)) %>% print()  # 6.81
range(new.scale$A.Mean.Sum)                             # 2.57 7.19
adiff <- diff(range(new.scale$A.Mean.Sum)) %>% print()  # 4.62
range(new.scale$D.Mean.Sum)                             # 2.56 7.29
ddiff <- diff(range(new.scale$D.Mean.Sum)) %>% print()  # 4.73


# Valence: everything in order? - yes.
(max(mean.proj.vad$V.Mean.Sum)-min(mean.proj.vad$V.Mean.Sum))/4
# [1] 1.7025
new.scale %>% 
  group_by(V.Mean.Sum2.direction) %>% 
  slice_max(V.Mean.Sum2) %>% 
  ungroup() %>% 
  pull(V.Mean.Sum2) %>% 
  sum()
# [1] 1.7025

# arousal: everything in order? - yes.
(max(mean.proj.vad$A.Mean.Sum)-min(mean.proj.vad$A.Mean.Sum))/8
# [1] 0.5775
max(new.scale$A.Mean.Sum2)-min(new.scale$A.Mean.Sum2)
# [1] 0.5775

# Dominance: everything in order? - yes.
(max(mean.proj.vad$D.Mean.Sum)-min(mean.proj.vad$D.Mean.Sum))/4
# [1] 1.1825
new.scale %>% 
  group_by(D.Mean.Sum2.direction) %>% 
  slice_max(D.Mean.Sum2) %>% 
  ungroup() %>% 
  pull(D.Mean.Sum2) %>% 
  sum()
# [1] 1.1825

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
         D.Mean.Sum2.cont.sc = scale(D.Mean.Sum2.cont),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "qcond",
                                 polarity == "negative" & conditional == "True" ~ "qcondneg")) %>% 
  distinct(verb_renamed, verb, voice, participant, predicateType, predicateType2, 
           veridicality, veridicality_num, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum, 
           V.Mean.Sum2, V.Mean.Sum2.direction, A.Mean.Sum2, D.Mean.Sum2,
           D.Mean.Sum2.direction, environment)

d.proj.vad %>% 
  distinct(verb_renamed) %>% 
  nrow() # 377

V_labels <- c("negative" = "negative valence",
              "positive" = "positive valence")

D_labels <- c("negative" = "negative dominance",
              "positive" = "positive dominance")


# A Acceptability ----
## A.1 correlation with projection ----
### plot ----
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
ggsave("../graphs/projection-by-acceptability-line.pdf", height = 6, width = 8)

### ordinal model ----
clmm(as.factor(veridicality) ~ acceptability + (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.37328    0.01209   30.88   <2e-16 ***

## A.2 distributions ----
# how many items with which acceptability rating?
d.proj %>% 
  group_by(acceptability) %>% 
  summarise(count = n())
#   acceptability count
#           <int> <int>
# 1             1   518
# 2             2  1005
# 3             3  1542
# 4             4  1652
# 5             5  2931
# 6             6  3216
# 7             7  5427

# distribution of ratings within each acceptability subgroup
o <- d.proj %>% 
  group_by(acceptability, veridicality) %>% 
  count() %>% 
  group_by(acceptability) %>%
  mutate(percentage =  n * 100 / sum(n)) %>% 
  print(n = Inf)
#   acceptability veridicality     n percentage
# 1             1 maybe          363      70.1 
# 2             1 no              63      12.2 
# 3             1 yes             92      17.8 
# 4             2 maybe          692      68.9 
# 5             2 no              83       8.26
# 6             2 yes            230      22.9 
# 7             3 maybe         1016      65.9 
# 8             3 no             133       8.63
# 9             3 yes            393      25.5 
# 10             4 maybe         1026      62.1 
# 11             4 no             106       6.42
# 12             4 yes            520      31.5 
# 13             5 maybe         1505      51.3 
# 14             5 no             184       6.28
# 15             5 yes           1242      42.4 
# 16             6 maybe         1327      41.3 
# 17             6 no             153       4.76
# 18             6 yes           1736      54.0 
# 19             7 maybe         1796      33.1 
# 20             7 no             214       3.94
# 21             7 yes           3417      63.0 

### plots ----
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
ggsave("../graphs/percentage-projection-by-acceptability-line.pdf", height = 6, width = 8)

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
ggsave("../graphs/percentage-projection-by-acceptability-bar.pdf", height = 6, width = 8)

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
ggsave("../graphs/count-projection-by-acceptability-bar.pdf", height = 6, width = 8)

## A.3 excluded predicates ----
# predicates with mean acceptability ratings of less than or equal to 4
mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  arrange(Mean.Acc) %>% 
  reframe(verb_renamed, Mean.Acc, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
# verb_renamed  Mean.Acc Mean.Proj predicateType2
# <fct>            <dbl>     <dbl> <chr>         
# 1 be bet            2.55    0.0345 evidential    
# 2 okay              3.03    0.333  nonEmoComm    
# 3 negotiate         3.1     0.2    nonEmoComm    
# 4 elect             3.33    0.167  evidential    
# 5 curse             3.37    0.267  emoComm       
# 6 bark              3.38    0.276  nonEmoComm    
# 7 be tweeted        3.4     0.167  evidential    
# 8 measure           3.5     0.167  evidential    
# 9 snap              3.57    0.333  emoComm       
# 10 stutter           3.57    0.3    nonEmoComm    
# 11 be cheered        3.6     0.733  emotive       
# 12 growl             3.62    0.483  emoComm       
# 13 manufacture       3.67    0.0333 nonEmoComm    
# 14 simulate          3.67    0.167  nonEmoComm    
# 15 giggle            3.7     0.3    emoComm       
# 16 hoot              3.7     0.4    emoComm       
# 17 volunteer         3.7     0.267  nonEmoComm    
# 18 be faxed          3.77    0.267  evidential    
# 19 diagnose          3.8     0.233  evidential    
# 20 be stimulated     3.83    0.724  emotive       
# 21 be nonplussed     3.83    0.433  emotive       
# 22 cackle            3.83    0.433  emoComm       
# 23 categorize        3.83    0.367  evidential    
# 24 pity              3.83    0.667  emotive       
# 25 test              3.87    0.0667 nonEmoComm    
# 26 vote              3.9     0.2    nonEmoComm    
# 27 be educated       3.93    0.433  evidential    
# 28 presuppose        4      -0.1    cognitive     
# 29 update            4       0.367  nonEmoComm  

excl <- mean.proj %>% 
  filter(Mean.Acc <= 4) %>% 
  select(verb_renamed)
cat(paste0('"', as.character(excl$verb_renamed), '",'))
# "bark", "be bet", "be cheered", "be educated", "be faxed", "be nonplussed", "be stimulated", 
# "be tweeted", "cackle", "categorize", "curse", "diagnose", "elect", "giggle", "growl", "hoot", 
# "manufacture", "measure", "negotiate", "okay", "pity", "presuppose", "simulate", "snap", "stutter", 
# "test", "update", "volunteer", "vote"

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
# 1 cognitive               53             1               1.89
# 2 emoComm                 33             6              18.2 
# 3 emotive                148             4               2.70
# 4 evidential              85             8               9.41
# 5 nonEmoComm             184            10               5.43

# Due to their low acceptability ratings, the 29 predicates (incl. 16 communicatives)
# listed above are excluded from analysis.


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

Tall <- 
  ggplot(mean.proj.bt, aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType.pdf", height = 4, width = 6)

# calculate by-predicateType2 means
mean.proj.bt2 <- d.proj.acc %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
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
        axis.title.x = element_text(vjust = -.5),
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
ggsave("../graphs/projection-by-predicateType2.pdf", height = 4, width = 9.5)

### linear models ----
lm(Mean.Proj ~ fct_relevel(predicateType, "communicative"), mean.proj.acc) %>% 
   summary()
# Coefficients:
#                                                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                            0.301298   0.013885  21.700   <2e-16 ***
# fct_relevel(predicateType, "communicative")cognitive  -0.085980   0.030626  -2.807   0.0052 ** 
# fct_relevel(predicateType, "communicative")emotive     0.422034   0.021491  19.637   <2e-16 ***
# fct_relevel(predicateType, "communicative")evidential -0.002313   0.026382  -0.088   0.9302    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1968 on 470 degrees of freedom
# Multiple R-squared:  0.5156,	Adjusted R-squared:  0.5125 
# F-statistic: 166.8 on 3 and 470 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43457    0.03734  11.637  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.21925    0.04603  -4.763 2.54e-06 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.28876    0.04069   7.096 4.77e-12 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.13558    0.04340  -3.124 0.001894 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15395    0.04014  -3.836 0.000142 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.194 on 469 degrees of freedom
# Multiple R-squared:  0.5304,	Adjusted R-squared:  0.5264 
# F-statistic: 132.4 on 4 and 469 DF,  p-value: < 2.2e-16

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

### by embedding environment ----
#### by predicate type ----
# negation
Tneg <- 
  d.proj.acc %>%
  filter(environment == "neg") %>% 
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj)) %>% 
ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(y = "Mean projection rating",
       x = "Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)

# question + conditional
Tqcond <- 
  d.proj.acc %>%
  filter(environment == "qcond") %>% 
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj)) %>% 
  ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType-qcond.pdf", height = 4, width = 6)

# question + conditional + negation
Tqcondneg <- 
  d.proj.acc %>%
  filter(environment == "qcondneg") %>% 
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj)) %>% 
  ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType-qcondneg.pdf", height = 4, width = 6)

# comparison
Tall + labs(title = "all embedding environments") + 
  Tneg + labs(title = "negation") + 
  Tqcond + labs(title = "question + conditional") +
  Tqcondneg + labs(title = "question + conditional + negation") +
  plot_layout(axis_titles = "collect", nrow = 2) &
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 16))
ggsave("../graphs/projection-by-predicateType-comparison.pdf", height = 6, width = 10)

#### by predicate type 2 ----
# overall
T2all <- 
  d.proj.acc %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>% 
ggplot(aes(x = predicateType2, y = Mean.Proj, colour = predicateType2)) +
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
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)

# negation
T2neg <- 
  d.proj.acc %>%
  filter(environment == "neg") %>% 
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = predicateType2)) +
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
ggsave("../graphs/projection-by-predicateType2.pdf", height = 4, width = 9.5)

# question + conditional
T2qcond <- 
  d.proj.acc %>%
  filter(environment == "qcond") %>% 
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = predicateType2)) +
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
ggsave("../graphs/projection-by-predicateType2-qcond.pdf", height = 4, width = 9.5)

# question + conditional + negation
T2qcondneg <- 
  d.proj.acc %>%
  filter(environment == "qcondneg") %>% 
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj)) %>% 
  ggplot(aes(x = predicateType2, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  # geom_label_repel(aes(label = predicateType2_names), 
  #                  segment.colour = "transparent",
  #                  nudge_x = 0.2, nudge_y = 0.2,
  #                  colour = cols2) +
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
ggsave("../graphs/projection-by-predicateType2-qcondneg.pdf", height = 4, width = 9.5)

# comparison
wrap_elements(get_legend(T2all + 
                           theme(legend.title = element_blank(),
                                 legend.text = element_text(size = 12)))) /
(T2all + labs(title = "all embedding environments") + 
  T2neg + labs(title = "negation") + 
  T2qcond + labs(title = "question + conditional") +
  T2qcondneg + labs(title = "question + conditional + negation") +
  plot_layout(axis_titles = "collect", nrow = 2) &
  theme(plot.title = element_text(size = 13),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 8),
        axis.text.x = element_blank(),
        legend.position = "none")) +
  plot_layout(heights = c(.1, 1))
ggsave("../graphs/projection-by-predicateType2-comparison.pdf", height = 6, width = 8)

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
ggsave("../graphs/projection-by-predicate-communicative.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoComm.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-nonEmoComm.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoCog.pdf", height = 4, width = 13)

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
#### plot ----
mean.proj.comm.bt <-  d.proj.comm %>%
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
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
ggsave("../graphs/projection-by-predicateType-commType.pdf", height = 4, width = 5)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(commType, "discourse participation"), 
   mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                           0.33285    0.01975  16.854  < 2e-16 ***
# fct_relevel(commType, "pure")discourse participation -0.03381    0.02631  -1.285  0.20022    
# fct_relevel(commType, "pure")state changing          -0.14469    0.04394  -3.293  0.00117 ** 

# Coefficients:
#                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                     0.29904    0.01738  17.206   <2e-16 ***
# fct_relevel(commType, "discourse participation")pure            0.03381    0.02631   1.285   0.2002    
# fct_relevel(commType, "discourse participation")state changing -0.11088    0.04293  -2.583   0.0105 *  

 
# Residual standard error: 0.1755 on 198 degrees of freedom
# Multiple R-squared:  0.05208,	Adjusted R-squared:  0.0425 
# F-statistic: 5.439 on 2 and 198 DF,  p-value: 0.005017

# faceted by predicate type
mean.proj.comm.bt2 <-  d.proj.comm %>%
  group_by(commType, predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
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
ggsave("../graphs/projection-by-predicateType-commType-faceted.pdf", height = 4, width = 8)

lm(Mean.Proj ~ fct_relevel(commType, "discourse participation") * fct_relevel(predicateType2, "nonEmoComm"), 
   mean.proj.comm) %>% 
  summary()                                                                                
#                                                                                                                 t value Pr(>|t|)    
# (Intercept)                                                                                                      16.717   <2e-16 ***
# fct_relevel(commType, "discourse participation")pure                                                              0.185   0.8532    
# fct_relevel(commType, "discourse participation")state changing                                                   -2.448   0.0153 *  
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                                                  1.990   0.0479 *  
# fct_relevel(commType, "discourse participation")pure:fct_relevel(predicateType2, "nonEmoComm")emoComm            -0.044   0.9652    
# fct_relevel(commType, "discourse participation")state changing:fct_relevel(predicateType2, "nonEmoComm")emoComm      NA       NA    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1704 on 196 degrees of freedom
# Multiple R-squared:  0.1162,	Adjusted R-squared:  0.09821 
# F-statistic: 6.445 on 4 and 196 DF,  p-value: 6.821e-05


### C.1.2 by predicate ----
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
ggsave("../graphs/projection-by-communicative-predicate.pdf", height = 4, width = 10)


# All communicatives with labels for the most and least projective communicatives.
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  geom_label_repel(data = slice_max(mean.proj.comm, Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   colour = "blue3") +
  geom_label_repel(data = slice_min(mean.proj.comm, Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100,
                   colour = "deeppink") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
# ggsave("../graphs/projection-by-communicative.pdf", height = 4, width = 13)

slice_max(mean.proj.comm, Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
# 1 cry              0.833
# 2 fess up          0.733
# 3 apologize        0.724
# 4 whine            0.7  
# 5 disclose         0.667
# 6 pout             0.667
# 7 bitch            0.633
# 8 document         0.633
# 9 weep             0.633
# 10 complain         0.6  
# 11 explain          0.6  
# 12 stress           0.6 

slice_min(mean.proj.comm, Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 pretend        -0.367 
# 2 fabricate      -0.2   
# 3 suggest        -0.167 
# 4 feign          -0.133 
# 5 claim          -0.1   
# 6 charge         -0.0333
# 7 allege          0     
# 8 demand          0.0333
# 9 fake            0.0333
# 10 retract         0.0333

### C.1.3 distribution ----
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

## C.2 our subcategories ----
### C.2.1 "pure" communicatives ----
#### plot ----
# All communicatives with labels for the most and least projective pure communicatives.
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  geom_label_repel(data = slice_max(subset(mean.proj.comm, commType == "pure"), 
                                    Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   colour = "blue3") +
  geom_label_repel(data = slice_min(subset(mean.proj.comm, commType == "pure"), 
                                    Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100,
                   colour = "deeppink") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
# ggsave("../graphs/projection-by-communicative-minmax-pure.pdf", height = 4, width = 13)

#### highest/lowest projection ratings ---- 
slice_max(subset(mean.proj.comm, commType == "pure"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
# 1 cry              0.833
# 2 whine            0.7  
# 3 pout             0.667
# 4 document         0.633
# 5 weep             0.633
# 6 publicize        0.586
# 7 cheer            0.533
# 8 log              0.533
# 9 emphasize        0.5  
# 10 gab              0.5  
# 11 grumble          0.5  

slice_min(subset(mean.proj.comm, commType == "pure"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
# 1 fax             0.0667
# 2 jest            0.0667
# 3 forecast        0.0690
# 4 holler          0.1   
# 5 joke            0.1   
# 6 type            0.1   
# 7 depict          0.133 
# 8 narrate         0.133 
# 9 prophesy        0.133 
# 10 express         0.167 
# 11 mark            0.167 
# 12 post            0.167 

### C.2.2 "discourse participation" communicatives ----
#### plot ----
# All communicatives with labels for the most and least projective dp communicatives.
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  geom_label_repel(data = slice_max(subset(mean.proj.comm, commType == "discourse participation"), 
                                    Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   colour = "blue3") +
  geom_label_repel(data = slice_min(subset(mean.proj.comm, commType == "discourse participation"), 
                                    Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100,
                   colour = "deeppink") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
# ggsave("../graphs/projection-by-communicative-minmax-dp.pdf", height = 4, width = 13)

#### highest/lowest projection ratings ---- 
slice_max(subset(mean.proj.comm, commType == "discourse participation"), 
          Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 fess up          0.733
# 2 apologize        0.724
# 3 disclose         0.667
# 4 bitch            0.633
# 5 complain         0.6  
# 6 explain          0.6  
# 7 stress           0.6  
# 8 flaunt           0.567
# 9 fuss             0.567
# 10 leak             0.567
# 11 point out        0.567
# 12 reveal           0.567
# 13 share            0.567

slice_min(subset(mean.proj.comm, commType == "discourse participation"), 
          Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 suggest        -0.167 
# 2 claim          -0.1   
# 3 charge         -0.0333
# 4 allege          0     
# 5 demand          0.0333
# 6 retract         0.0333
# 7 contend         0.0667
# 8 decree          0.0667
# 9 hint            0.0667
# 10 imply          0.0667
# 11 propose        0.0667
# 12 reject         0.0667

#### vs "discourse role verbs" (Grimshaw 2015) ----
mean.proj.comm %>% 
  filter(commType == "discourse participation" | sayVerbType == "discourse role verb") %>% 
  group_by(predicateType2, commType, sayVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType             n
#   <chr>          <chr>                   <chr>               <int>
# 1 emoComm        discourse participation mode verb               4 - A
# 2 emoComm        discourse participation NA                      2
# 3 nonEmoComm     discourse participation discourse role verb    45
# 4 nonEmoComm     discourse participation mode verb               1 - B
# 5 nonEmoComm     discourse participation NA                     50
# 6 nonEmoComm     pure                    discourse role verb     5 - C
# 7 nonEmoComm     state changing          discourse role verb     5 - D

# A
mean.proj.comm %>% 
  filter(predicateType2 == "emoComm" & commType == "discourse participation" &
           sayVerbType == "mode verb") %>% 
  select(verb_renamed, modeVerbType)
#   verb_renamed modeVerbType     
#   <fct>        <chr>            
# 1 bitch        say-with-attitude
# 2 boast        say-with-attitude
# 3 brag         say-with-attitude
# 4 complain     say-with-attitude

# B
mean.proj.comm %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "discourse participation" &
           sayVerbType == "mode verb") %>% 
  select(verb_renamed, modeVerbType)
#   verb_renamed modeVerbType     
#   <fct>        <chr>            
# 1 tease        say-with-attitude

# C
mean.proj.comm %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "pure" &
           sayVerbType == "discourse role verb") %>% 
  select(verb_renamed)
#   verb_renamed
#   <fct>       
# 1 mark        
# 2 remark      
# 3 repeat      
# 4 restate     
# 5 state   

# D
mean.proj.comm %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "state changing" &
           sayVerbType == "discourse role verb") %>% 
  select(verb_renamed)
# 1 insist      
# 2 lie         
# 3 promise     
# 4 swear       
# 5 vow 


### C.2.3 "state changing" communicatives ----
#### plot ----
# All communicatives with labels for all 22 state changing communicatives.
# Most state changing communicatives have relatively low projection ratings.
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  geom_label_repel(data = subset(mean.proj.comm, commType == "state changing"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
# ggsave("../graphs/projection-by-communicative-sc.pdf", height = 4, width = 13)

#### highest/lowest projection ratings ---- 
# All communicatives with labels for the most and least projective sc communicatives.
ggplot(mean.proj.comm, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  geom_label_repel(data = slice_max(subset(mean.proj.comm, commType == "state changing"), 
                                    Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   colour = "blue3") +
  geom_label_repel(data = slice_min(subset(mean.proj.comm, commType == "state changing"), 
                                    Mean.Proj, n = 10),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   max.overlaps = 100,
                   colour = "deeppink") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
# ggsave("../graphs/projection-by-communicative-minmax-sc.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 warn             0.552
# 2 expose           0.533
# 3 advise           0.367
# 4 certify          0.367
# 5 showcase         0.333
# 6 advertise        0.3  
# 7 guarantee        0.267
# 8 swear            0.233
# 9 vow              0.233
# 10 prove            0.207

slice_min(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 pretend        -0.367 
# 2 fabricate      -0.2   
# 3 feign          -0.133 
# 4 fake            0.0333
# 5 promise         0.1   
# 6 demonstrate     0.138 
# 7 insist          0.167 
# 8 lie             0.167 
# 9 warrant         0.2   
# 10 prove          0.207 

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
# 2 communicative no         92
# 3 communicative yes       110

d.proj.acc %>% 
  filter(predicateType == "cognitive" & sayVerb == "yes") %>% 
  distinct(verb_renamed)
# verb_renamed
# 1        think
# 2         pray
# These predicates "report internal linguistic formulation only" (Grimshaw 2015: 84).

# The MV dataset contains 202 communication predicates with mean acceptability 
# ratings greater than 4. It further contains two say-verbs that are not communicatives.
# These are not included in the present analysis.

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
# Of the 201 communicatives included in this investigation, 110 are say-predicates.

#### plot ----
mean.proj.commsay <- d.proj.comm %>%
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-communication-type.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayVerb, "no"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.29520    0.01884  15.666   <2e-16 ***
# fct_relevel(sayVerb, "no")yes  0.01114    0.02547   0.437    0.662 

### C.3.2 types of say verbs ----
#### distribution ----
d.proj.comm %>% 
  filter((sayVerb == "yes")) %>%  
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
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-sayverb-type.pdf", height = 4, width = 10)

mean.proj.saytype2 <- d.proj.comm %>%
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-sayverb-type2.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayVerbType, "discourse role verb"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                              0.261045   0.022397  11.655  < 2e-16 ***
# fct_relevel(sayVerbType, "discourse role verb")mode verb 0.092169   0.031821   2.896  0.00458 ** 
# fct_relevel(sayVerbType, "discourse role verb")say       0.005622   0.167607   0.034  0.97331 

#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                               0.35321    0.02260  15.626  < 2e-16 ***
# fct_relevel(sayVerbType, "mode verb")discourse role verb -0.09217    0.03182  -2.896  0.00458 ** 
# fct_relevel(sayVerbType, "mode verb")say                 -0.08655    0.16763  -0.516  0.60672 

# Residual standard error: 0.1661 on 107 degrees of freedom
# (91 observations deleted due to missingness)
# Multiple R-squared:  0.07317,	Adjusted R-squared:  0.05585 
# F-statistic: 4.224 on 2 and 107 DF,  p-value: 0.01716

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "mode verb") + 
       (1 | participant) + (1 | environment), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.41928    0.08448   4.963 6.93e-07 ***
# fct_relevel(sayVerbType, "discourse role verb")say        0.01411    0.43096   0.033    0.974 

# fct_relevel(sayVerbType, "mode verb")discourse role verb -0.41928    0.08448  -4.963 6.93e-07 ***
# fct_relevel(sayVerbType, "mode verb")say                 -0.40517    0.43066  -0.941    0.347  

# fct_relevel(sayVerbType, "say")discourse role verb -0.01411    0.43097  -0.033    0.974
# fct_relevel(sayVerbType, "say")mode verb            0.40517    0.43067   0.941    0.347

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
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-modeverb-type.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(modeVerbType, "say-with-attitude"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                0.351388   0.026255  13.384   <2e-16 ***
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude 0.007587   0.053510   0.142    0.888 

#                                                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                 0.358974   0.046626   7.699 3.82e-10 ***
# fct_relevel(modeVerbType, "say-with-attitude")say-by-means -0.007587   0.053510  -0.142    0.888 

# Residual standard error: 0.1681 on 52 degrees of freedom
# (147 observations deleted due to missingness)
# Multiple R-squared:  0.0003864,	Adjusted R-squared:  -0.01884 
# F-statistic: 0.0201 on 1 and 52 DF,  p-value: 0.8878

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-by-means") + 
       (1 | participant) + (1 | environment), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude  0.09049    0.14558   0.622    0.534

# fct_relevel(modeVerbType, "say-with-attitude")say-by-means -0.09049    0.14558  -0.622    0.534

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
ggsave("../graphs/projection-by-saybymeansverb-type.pdf", height = 4, width = 10)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayByMeansVerbType, "sound"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                    0.30714    0.04220   7.279 1.04e-08 ***
# fct_relevel(sayByMeansVerbType, "form")manner  0.06893    0.05398   1.277    0.209    
# fct_relevel(sayByMeansVerbType, "form")sound   0.05952    0.08226   0.724    0.474  

# (Intercept)                                     0.376071   0.033662  11.172 1.44e-13 ***
# fct_relevel(sayByMeansVerbType, "manner")form  -0.068928   0.053980  -1.277    0.209    
# fct_relevel(sayByMeansVerbType, "manner")sound -0.009404   0.078224  -0.120    0.905   

# (Intercept)                                     0.366667   0.070611   5.193 7.26e-06 ***
# fct_relevel(sayByMeansVerbType, "sound")form   -0.059524   0.082259  -0.724    0.474    
# fct_relevel(sayByMeansVerbType, "sound")manner  0.009404   0.078224   0.120    0.905    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1579 on 38 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.04243,	Adjusted R-squared:  -0.007966 
# F-statistic: 0.8419 on 2 and 38 DF,  p-value: 0.4388

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayByMeansVerbType, "sound") + 
       (1 | participant) + (1 | environment), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(sayByMeansVerbType, "form")manner   0.2999     0.1585   1.892   0.0585 .
# fct_relevel(sayByMeansVerbType, "form")sound    0.2731     0.2399   1.138   0.2550  

# fct_relevel(sayByMeansVerbType, "manner")form  -0.29993    0.15851  -1.892   0.0585 .
# fct_relevel(sayByMeansVerbType, "manner")sound -0.02683    0.23335  -0.115   0.9085 

# fct_relevel(sayByMeansVerbType, "sound")form   -0.27310    0.23994  -1.138    0.255
# fct_relevel(sayByMeansVerbType, "sound")manner  0.02683    0.23335   0.115    0.908

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### C.3.5 overall ----
#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, commType, sayVerbType, modeVerbType) %>% 
  count()
# predicateType2 commType                sayVerbType         modeVerbType          n
# <chr>          <chr>                   <chr>               <chr>             <int>
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
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-saybymeansverb-type2.pdf", height = 4, width = 10)


# D Spearman rank correlations ----
# between the different embedding environments in the MV dataset

# all 544 predicates
# negation vs question + conditional
mean.proj.all.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcond, method = "spearman")) # 0.408

# negation vs question + conditional + negation
mean.proj.all.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcondneg, method = "spearman")) # 0.705

# question + conditional vs question + conditional + negation
mean.proj.all.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(qcond, qcondneg, method = "spearman")) # 0.333

# 503 predicates (those for which we have defined a predicate type)
# negation vs question + conditional
mean.proj.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcond, method = "spearman")) # 0.425

# negation vs question + conditional + negation
mean.proj.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcondneg, method = "spearman")) # 0.714

# question + conditional vs question + conditional + negation
mean.proj.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(qcond, qcondneg, method = "spearman")) # 0.343

# 474 predicates (defined predicate type + acceptability rating > 4 across embedding environments)
# negation vs question + conditional
mean.proj.acc.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcond, method = "spearman")) # 0.429

# negation vs question + conditional + negation
mean.proj.acc.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(neg, qcondneg, method = "spearman")) # 0.725

# question + conditional vs question + conditional + negation
mean.proj.acc.env %>% 
  pivot_wider(id_cols = verb_renamed, names_from = environment, values_from = Mean.Proj) %>% 
  summarise(correlation = cor(qcond, qcondneg, method = "spearman")) # 0.342

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
ggsave("../graphs/projection-by-dynamicity.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ dynamicity, mean.proj.acc) %>% 
  summary()
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.29214    0.01363   21.43   <2e-16 ***
# dynamicitystative  0.32862    0.02188   15.02   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2322 on 472 degrees of freedom
# Multiple R-squared:  0.3233,	Adjusted R-squared:  0.3219 
# F-statistic: 225.5 on 1 and 472 DF,  p-value: < 2.2e-16

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
ggsave("../graphs/projection-by-predicateType-and-dynamicity.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ dynamicity * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.acc) %>%  
  summary()
# predicateType2  significance of dynamic-stative difference
# cognitive       *
# emoComm         n/a
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n/a

# Residual standard error: 0.1933 on 466 degrees of freedom
# Multiple R-squared:  0.5367,	Adjusted R-squared:  0.5298 
# F-statistic: 77.13 on 7 and 466 DF,  p-value: < 2.2e-16

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
ggsave("../graphs/projection-by-CoS.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ changeOfState, mean.proj.acc) %>% 
  summary()
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.42632    0.01342  31.758   <2e-16 ***
# changeOfStateyes -0.08960    0.04940  -1.814   0.0703 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2813 on 472 degrees of freedom
# Multiple R-squared:  0.006922,	Adjusted R-squared:  0.004818 
# F-statistic:  3.29 on 1 and 472 DF,  p-value: 0.07035

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
ggsave("../graphs/projection-by-predicateType-and-CoS.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ changeOfState * fct_relevel(predicateType2, "evidential"), mean.proj.acc) %>% 
  summary()
# predicateType2  significance of no vs yes
# cognitive       *** (note: only 2 of 52 cognitives are CoS predicates)
# emoComm         n/a
# emotive         n/a
# evidential      n.s.
# nonEmoComm      n/a

# Residual standard error: 0.1898 on 467 degrees of freedom
# Multiple R-squared:  0.5527,	Adjusted R-squared:  0.547 
# F-statistic: 96.18 on 6 and 467 DF,  p-value: < 2.2e-16

## H.3 activity ----
### H.3.1 overall ----
#### plot ----
mean.proj.act <- d.proj.acc %>%
  group_by(activity) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
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
ggsave("../graphs/projection-by-activity.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ activity, mean.proj.acc) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.47188    0.01491  31.642  < 2e-16 ***
# activityyes -0.17176    0.02706  -6.348 5.12e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2709 on 472 degrees of freedom
# Multiple R-squared:  0.07866,	Adjusted R-squared:  0.07671 
# F-statistic:  40.3 on 1 and 472 DF,  p-value: 5.12e-10

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
  summarize(Mean.Proj = mean(veridicality_num), 
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
ggsave("../graphs/projection-by-predicateType-and-activity.pdf", height = 4, width = 8)

#### linear model ----
lm(Mean.Proj ~ activity * fct_relevel(predicateType2, "cognitive"), mean.proj.acc) %>% 
  summary()
# predicateType2  significance of no vs yes
# cognitive       ***
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.1897 on 464 degrees of freedom
# Multiple R-squared:  0.556,	Adjusted R-squared:  0.5474 
# F-statistic: 64.56 on 9 and 464 DF,  p-value: < 2.2e-16

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
ggsave("../graphs/projection-by-evidenceType.pdf", height = 4, width = 4)

#### linear model ----
lm(Mean.Proj ~ evidenceType, 
   mean.proj.acc %>% filter(predicateType2 == "evidential")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.291488   0.040896   7.128 5.68e-10 ***
# evidenceTypeperceptual   0.078512   0.088660   0.886    0.379    
# evidenceTypereportative -0.006929   0.061116  -0.113    0.910   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2488 on 74 degrees of freedom
# Multiple R-squared:  0.01267,	Adjusted R-squared:  -0.01402 
# F-statistic: 0.4747 on 2 and 74 DF,  p-value: 0.6239



## X VAD ratings ----
### X.0 duplicates ----
# predicates in both active and passive sentence frames with same predicate type

# new.scale[duplicated(new.scale[,cbind(2,14)]),] %>%
#   select(verb.x, predicateType2)
#     verb.x predicateType2
# 185 grieve        emotive
# 237 marvel        emotive
# 260  panic        emotive
# 400  worry        emotive

# new.scale %>%
#   filter(predicateType == "emotive") %>%
#   nrow() # 97

# Of the 97 emotives with valence/arousal/dominance ratings, four predicates occur 
# in both sentence frames in the MV data set. Beloew, the "active" and "passive 
# voice" versions of these predicates are assigned the same valence/arousal/dominance 
# ratings. However, due to the small number of these cases, the effect of this on
# the investigation below should be relatively small. 

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
# ggsave("../graphs/valence-by-predicate.pdf", height = 4, width = 13)

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
                      labels = predicateType2_names)
# ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### X.1.2 by predicate type ----
##### X.1.2.1 overall ----
###### plots ----
# original ratings
# calculate valence by predicateType means
mean.valence <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum), CILow = ci.low(V.Mean.Sum), 
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
ggsave("../graphs/valence-by-predicateType-orig.pdf", height = 4, width = 6)

# calculate valence by predicateType2 means (communicatives with/without emotive component)
mean.valence2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum), CILow = ci.low(V.Mean.Sum), 
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
ggsave("../graphs/valence-by-predicateType2-orig.pdf", height = 4, width = 6)

# split valence scale
# calculate valence by predicateType2 means (communicatives with/without emotive component)
mean.valence2.sp <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2.sp, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2.pdf", height = 4, width = 6)

###### linear models ----
lm(V.Mean.Sum ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        3.8404     0.2608  14.727  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive    1.8413     0.3240   5.684 2.67e-08 ***
# fct_relevel(predicateType2, "emoComm")emotive      0.5708     0.2925   1.952   0.0517 .  
# fct_relevel(predicateType2, "emoComm")evidential   1.6891     0.3075   5.493 7.35e-08 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm   1.3382     0.2824   4.739 3.06e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.304 on 372 degrees of freedom
# Multiple R-squared:  0.1495,	Adjusted R-squared:  0.1403 
# F-statistic: 16.34 on 4 and 372 DF,  p-value: 2.418e-12

lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of difference in mean valence from
#                 cognitive emoComm emotive evidential  nonEmoComm    ranking (low-high)
# cognitive       n/a       n.s.    ***     **          ***           3
# emoComm         n.s.      n/a     *       **          ***           4
# emotive         ***       *       n/a     ***         ***           5
# evidential      **        **      ***     n/a         n.s.          2
# nonEmoComm      ***       ***     ***     n.s.        n/a           1

##### X.1.2.2 with direction of valence ----
###### plot ----
# calculate valence by predicateType2 means and direction of valence
mean.valence3 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 10

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
ggsave("../graphs/valence-by-predicateType-and-direction.pdf", height = 4, width = 6)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Coefficients:
#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.38333    0.03658  10.480  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive                                -0.13780    0.05784  -2.383  0.01770 *  
# fct_relevel(predicateType2, "emoComm")emotive                                   0.07127    0.04224   1.687  0.09239 .  
# fct_relevel(predicateType2, "emoComm")evidential                               -0.19100    0.05667  -3.370  0.00083 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.18522    0.04279  -4.329 1.94e-05 ***
# V.Mean.Sum2.directionpositive                                                  -0.18271    0.09145  -1.998  0.04646 *  
# fct_relevel(predicateType2, "emoComm")cognitive:V.Mean.Sum2.directionpositive   0.28959    0.10606   2.731  0.00663 ** 
# fct_relevel(predicateType2, "emoComm")emotive:V.Mean.Sum2.directionpositive     0.15053    0.09816   1.534  0.12600    
# fct_relevel(predicateType2, "emoComm")evidential:V.Mean.Sum2.directionpositive  0.22216    0.10397   2.137  0.03327 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:V.Mean.Sum2.directionpositive  0.18650    0.09579   1.947  0.05229 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1676 on 367 degrees of freedom
# Multiple R-squared:  0.2866,	Adjusted R-squared:  0.2691 
# F-statistic: 16.38 on 9 and 367 DF,  p-value: < 2.2e-16

# Significant at the 0.001 level for all predicate types.

# predicateType2  significance of V.Mean.Sum2.direction difference within predicate type
# cognitive       *
# emoComm         *
# emotive         n.s.
# evidential      n.s.
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
# ggsave("../graphs/arousal-by-predicate.pdf", height = 4, width = 13)

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
                      labels = predicateType2_names)
# ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)


#### X.2.2 by predicate type ----
##### X.2.2.1 overall ----
###### plots ----
# calculate arousal by predicateType means
# original ratings
mean.arousal <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum), CILow = ci.low(A.Mean.Sum), 
            CIHigh = ci.high(A.Mean.Sum)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Arousal))
mean.arousal

ggplot(mean.arousal, aes(x = predicateType, y = Mean.Arousal, colour = predicateType)) +
  geom_hline(yintercept = 5, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_colour_manual(values = cols)
ggsave("../graphs/arousal-by-predicateType-orig.pdf", height = 4, width = 6)

# calculate arousal by predicateType2 means (communicatives with/without emotive component)
mean.arousal2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum), CILow = ci.low(A.Mean.Sum), 
            CIHigh = ci.high(A.Mean.Sum)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
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
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2-orig.pdf", height = 4, width = 6)

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
ggsave("../graphs/arousal-by-predicateType2.pdf", height = 4, width = 6)

###### linear models ----
lm(A.Mean.Sum ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
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

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of difference in mean arousal from
#                 cognitive emoComm emotive evidential  nonEmoComm    ranking (low-high)
# cognitive       n/a       ***     ***     n.s.        n.s.          2
# emoComm         ***       n/a     n.s.    ***         **            4
# emotive         ***       n.s.    n/a     ***         ***           5
# evidential      n.s.      ***     ***     n/a         n.s.          1
# nonEmoComm      n.s.      **      ***     n.s.        n/a           3

##### X.2.2.2 with direction of valence ----
###### plot ----
# calculate arousal by predicateType2 means and direction of valence
mean.arousal3 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal3
nrow(mean.arousal3) # 10

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
ggsave("../graphs/arousal-by-predicateType-and-direction-of-valence.pdf", height = 4, width = 6)

###### linear model ----
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Coefficients:
#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.45095    0.02265  19.907   <2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive                                -0.06890    0.03582  -1.924   0.0552 .  
# fct_relevel(predicateType2, "emoComm")emotive                                   0.06157    0.02616   2.354   0.0191 *  
# fct_relevel(predicateType2, "emoComm")evidential                               -0.04629    0.03509  -1.319   0.1880    
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.02459    0.02650  -0.928   0.3540    
# V.Mean.Sum2.directionpositive                                                   0.08499    0.05663   1.501   0.1343    
# fct_relevel(predicateType2, "emoComm")cognitive:V.Mean.Sum2.directionpositive  -0.10091    0.06568  -1.536   0.1253    
# fct_relevel(predicateType2, "emoComm")emotive:V.Mean.Sum2.directionpositive    -0.13530    0.06079  -2.226   0.0266 *  
# fct_relevel(predicateType2, "emoComm")evidential:V.Mean.Sum2.directionpositive -0.13192    0.06439  -2.049   0.0412 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:V.Mean.Sum2.directionpositive -0.14610    0.05932  -2.463   0.0142 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1038 on 367 degrees of freedom
# Multiple R-squared:  0.2386,	Adjusted R-squared:   0.22 
# F-statistic: 12.78 on 9 and 367 DF,  p-value: < 2.2e-16

# predicateType2  significance of V.Mean.Sum2.direction difference within predicate type
# cognitive       n.s.
# emoComm         n.s.
# emotive         *
# evidential      n.s.
# nonEmoComm      ***

### X.3 dominance ----
#### X.3.1 by predicate ----
##### plot ----
# with emotive component distinction for communicatives
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
# ggsave("../graphs/dominance-by-predicate2.pdf", height = 4, width = 13)

#### X.3.2 by predicate type ----
##### X 3.2.1 overall ----
###### plots ----
# original ratings
# calculate dominance by predicateType2 means (communicatives with/without emotive component)
mean.dominance <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum), CILow = ci.low(D.Mean.Sum), 
            CIHigh = ci.high(D.Mean.Sum)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance

ggplot(mean.dominance, aes(x = predicateType2, y = Mean.Dominance, colour = predicateType2)) +
  #geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean dominance rating") +
  # scale_y_continuous(limits = c(1, 9), breaks = c(1, 5, 9), minor_breaks = seq(0, 9, 1),
  #                    labels = c("controlled", "neutral", "in control")) +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/dominance-by-predicateType2-orig.pdf", height = 4, width = 6)

# split dominance ratings
# calculate dominance by predicateType2 means (communicatives with/without emotive component)
mean.dominance.sp <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
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
ggsave("../graphs/dominance-by-predicateType2.pdf", height = 4, width = 6)

###### linear models ----
lm(D.Mean.Sum ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       4.87120    0.19259  25.293  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.86054    0.23926   3.597 0.000366 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.01736    0.21598   0.080 0.935993    
# fct_relevel(predicateType2, "emoComm")evidential  0.95443    0.22711   4.203 3.31e-05 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.65818    0.20853   3.156 0.001728 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9629 on 372 degrees of freedom
# Multiple R-squared:  0.1298,	Adjusted R-squared:  0.1204 
# F-statistic: 13.87 on 4 and 372 DF,  p-value: 1.485e-10

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of difference in mean dominance from
#                 cognitive emoComm emotive evidential  nonEmoComm    ranking (low-high)
# cognitive       n/a       .       n.s.    n.s.        *             4
# emoComm         .         n/a     .       **          n.s.          1
# emotive         n.s.      .       n/a     n.s.        *             3
# evidential      n.s.      **      n.s.    n/a         ***           5
# nonEmoComm      *         n.s.    *       ***         n/a           2

##### X.3.2.2 with direction of valence ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance2 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance2
nrow(mean.dominance2) # 10

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
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-valence.pdf", height = 4, width = 6)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2.direction difference within predicate type
# cognitive       **
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      ***

##### X.3.2.3 with direction of dominance ----
###### plot ----
# calculate dominance by predicateType2 means and direction of dominance
mean.dominance3 <- new.scale %>%
  group_by(predicateType2, D.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance3
nrow(mean.dominance3) # 10

ggplot(mean.dominance3, aes(x = predicateType2, y = Mean.Dominance, 
                            colour = predicateType2, shape = D.Mean.Sum2.direction)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
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
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-dominance.pdf", height = 4, width = 6)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm") * D.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2.direction difference within predicate type
# cognitive       *
# emoComm         n.s.
# emotive         n.s.
# evidential      .
# nonEmoComm      ***

###### tables: what is "direction of dominance"? ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#        verb_renamed D.Mean.Sum2 predicateType2
# 1      be delighted      0.5725        emotive
# 2             enjoy      0.5700        emotive
# 3      be comforted      0.5450        emotive
# 4          research      0.5400     evidential
# 5            detail      0.5350     nonEmoComm
# 6         recognize      0.5275     evidential
# 7         determine      0.5225     evidential
# 8            signal      0.5125     nonEmoComm
# 9      be signalled      0.5125     evidential
# 10       appreciate      0.5000      cognitive
# 11 be congratulated      0.5000     evidential
# 12            learn      0.5000     evidential

new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         36
# 2 emoComm           12
# 3 emotive           43
# 4 evidential        53
# 5 nonEmoComm       107

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#      verb_renamed D.Mean.Sum2 predicateType2
# 1            cry      0.6100        emoComm
# 2          panic      0.5875        emotive
# 3    be panicked      0.5875        emotive
# 4  be frightened      0.5725        emotive
# 5    be tortured      0.5600        emotive
# 6   be terrified      0.5525        emotive
# 7      be fooled      0.5150     evidential
# 8          doubt      0.4850      cognitive
# 9           envy      0.4600        emotive
# 10    be worried      0.4575        emotive
# 11         worry      0.4575        emotive

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         10
# 2 emoComm           13
# 3 emotive           54
# 4 evidential        11
# 5 nonEmoComm        38

### X.4 valence + arousal + dominance distributions and correlations ----
#### X.4.1 by predicate ----
##### plots ----
###### X.4.1.1 valence + arousal ----
# no patterns (clusters) emerge.
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
                      labels = predicateType2_names) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-by-arousal.pdf", height = 8, width = 12)

###### X.4.1.2 valence + dominance ----
# no patterns (clusters) emerge.
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2, colour = predicateType2, 
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
ggsave("../graphs/valence-by-dominance.pdf", height = 8, width = 12)

###### X.4.1.3 arousal + dominance ----
ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2, colour = predicateType2, 
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
ggsave("../graphs/arousal-by-dominance.pdf", height = 8, width = 12)

#### X.4.2 by predicate type ----
##### plots ----
###### X.4.2.1 valence + arousal ----
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
                    labels = predicateType2_names)
ggsave("../graphs/valence-arousal-by-predicateType2.pdf", height = 6, width = 10)

###### X.4.2.2 valence + dominance ----
# distribution of valence and dominance ratings by predicate type
new.scale %>% 
  select(verb_renamed, predicateType, Valence = V.Mean.Sum2, Dominance = D.Mean.Sum2) %>%
  pivot_longer(., cols = c(Valence, Dominance), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType)) +
  geom_violin() +
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 10)) + 
  labs(y = "Mean valence / dominance rating", 
       fill = "Predicate type",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(limits = c("Valence", "Dominance")) +
  scale_fill_manual(values = cols)
ggsave("../graphs/valence-dominance-by-predicateType.pdf", height = 6, width = 10)

# distribution of valence and dominance ratings by predicate type including emotive component
new.scale %>% 
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
ggsave("../graphs/valence-dominance-by-predicateType2.pdf", height = 6, width = 10)

###### X.4.2.3 arousal + dominance ----
# distribution of valence and dominance ratings by predicate type including emotive component
new.scale %>% 
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
ggsave("../graphs/arousal-dominance-by-predicateType2.pdf", height = 6, width = 10)

###### X.4.2.4 valence + arousal + dominance ----
# distribution of valence, arousal and dominance ratings by 5 predicate types
new.scale %>% 
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
ggsave("../graphs/valence-arousal-dominance-by-predicateType2.pdf", height = 6, width = 10)

ggplot() +
  geom_point(data = mean.valence2, 
             aes(x = factor(predicateType2, levels = c("cognitive", "emoComm", "emotive", "evidential", "nonEmoComm")), y = Mean.Valence, colour = "valence"), 
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
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = cols2, face = "bold"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 12)) +
  labs(x = "Predicate type",
       y = "Mean valence / arousal / dominance rating",
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(limits = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))
ggsave("../graphs/valence-arousal-dominance-by-predicateType2-2.pdf", height = 6, width = 10)

# distribution of valence, arousal and dominance ratings by communicative predicates
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
ggsave("../graphs/valence-arousal-dominance-by-communicative.pdf", height = 6, width = 10)

# distribution of valence, arousal and dominance ratings by communicative predicates and emotives
new.scale %>% 
  filter(predicateType %in% c("communicative", "emotive")) %>% 
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
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)\ndominance rating: neutral (0) to completely in control / controllled (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal", "Dominance")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-arousal-dominance-by-communicative-emotive.pdf", height = 6, width = 10)

#### X.4.3 rating correlations ----
##### X.4.3.1 valence + arousal ----
###### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-valence.pdf", height = 4, width = 6)

# original scale
ggplot(new.scale, aes(x = V.Mean.Sum, y = A.Mean.Sum)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-valence-orig.pdf", height = 4, width = 6)

# faceted by predicate type
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-valence-faceted.pdf", height = 4, width = 6)

# faceted by direction of valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-valence-faceted-Vdir.pdf", height = 4, width = 8)

# by direction of valence and dominance
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) 
ggsave("../graphs/arousal-by-valence-faceted-Vdir-Ddir.pdf", height = 5, width = 5)

ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)",
       y = "Mean valence rating (neutral - unhappy/happy)", 
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-arousal.pdf", height = 4, width = 6)

# faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-arousal-faceted.pdf", height = 5, width = 6)

###### linear models -----
lm(V.Mean.Sum2.sc ~ A.Mean.Sum2.sc, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.01530    0.03413   0.448    0.654    
# A.Mean.Sum2  0.66424    0.07902   8.406 8.95e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1801 on 375 degrees of freedom
# Multiple R-squared:  0.1585,	Adjusted R-squared:  0.1563 
# F-statistic: 70.66 on 1 and 375 DF,  p-value: 8.946e-16

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         **
# evidential      *
# nonEmoComm      **
# 
# Residual standard error: 0.164 on 367 degrees of freedom
# Multiple R-squared:  0.3171,	Adjusted R-squared:  0.3003 
# F-statistic: 18.93 on 9 and 367 DF,  p-value: < 2.2e-16

lm(V.Mean.Sum2 ~ 
     A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), new.scale) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction significance of A.Mean.Sum2
# cognitive       negative              n.s.
# emoComm         negative              *
# emotive         negative              *
# evidential      negative              **
# nonEmoComm      negative              **
# cognitive       positive              n.s.
# emoComm         positive              n.s.
# emotive         positive              .
# evidential      positive              n.s.
# nonEmoComm      positive              .

# Residual standard error: 0.1624 on 357 degrees of freedom
# Multiple R-squared:  0.349,	Adjusted R-squared:  0.3144 
# F-statistic: 10.07 on 19 and 357 DF,  p-value: < 2.2e-16

lm(A.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.34615    0.00997  34.719  < 2e-16 ***
# V.Mean.Sum2  0.23869    0.02840   8.406 8.95e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.108 on 375 degrees of freedom
# Multiple R-squared:  0.1585,	Adjusted R-squared:  0.1563 
# F-statistic: 70.66 on 1 and 375 DF,  p-value: 8.946e-16

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         **
# evidential      *
# nonEmoComm      **

# Residual standard error: 0.1031 on 367 degrees of freedom
# Multiple R-squared:  0.2489,	Adjusted R-squared:  0.2305 
# F-statistic: 13.52 on 9 and 367 DF,  p-value: < 2.2e-16

lm(A.Mean.Sum2 ~ 
     V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), new.scale) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction significance of V.Mean.Sum2
# cognitive       negative              n.s.
# emoComm         negative              **
# emotive         negative              .
# evidential      negative              **
# nonEmoComm      negative              **
# cognitive       positive              n.s.
# emoComm         positive              n.s.
# emotive         positive              .
# evidential      positive              n.s.
# nonEmoComm      positive              .

# Residual standard error: 0.09978 on 357 degrees of freedom
# Multiple R-squared:  0.3157,	Adjusted R-squared:  0.2793 
# F-statistic: 8.667 on 19 and 357 DF,  p-value: < 2.2e-16

##### X.4.3.2 valence + dominance ----
###### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (neutral - unhappy/happy)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-valence.pdf", height = 4, width = 6)

# original scale
ggplot(new.scale, aes(x = V.Mean.Sum, y = D.Mean.Sum)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean dominance rating (controlled - in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-valence-orig.pdf", height = 4, width = 6)

# faceted
# by predicate type
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-valence-faceted.pdf", height = 5, width = 6)

# by direction of valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-valence-faceted-Vdir.pdf", height = 4, width = 6)

# by direction of valence and dominance
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) 
ggsave("../graphs/dominance-by-valence-faceted-Vdir-Ddir.pdf", height = 5, width = 5)

new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2)
#      verb_renamed V.Mean.Sum2 D.Mean.Sum2
# 1      be angered      0.6250      0.0350
# 2  be intimidated      0.5400      0.1875
# 3        complain      0.4750      0.0250
# 4         dislike      0.4375      0.2025
# 5            pout      0.4175      0.1300
# 6          regret      0.3975      0.1575
# 7           scoff      0.3825      0.1750
# 8      disapprove      0.3625      0.0525
# 9           gloat      0.3300      0.2750
# 10         holler      0.3300      0.1550

new.scale %>% 
  filter(V.Mean.Sum2.direction == "positive" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2
# 1         check      0.3875      0.0825
# 2      showcase      0.3225      0.1500
# 3         phone      0.2725      0.1400
# 4     challenge      0.2375      0.0800
# 5        expect      0.2375      0.0275
# 6       contest      0.2125      0.0175
# 7   be incensed      0.1975      0.1375
# 8       witness      0.1525      0.1400
# 9         admit      0.1400      0.1100
# 10      retract      0.0950      0.0350

ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (neutral - controlled / in control)",
       y = "Mean valence rating (neutral - unhappy/happy)", 
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance.pdf", height = 4, width = 6)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-dominance-faceted.pdf", height = 6, width = 7)

###### linear models -----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18773    0.01827   10.27  < 2e-16 ***
# D.Mean.Sum2  0.44289    0.06650    6.66 9.77e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1857 on 375 degrees of freedom
# Multiple R-squared:  0.1058,	Adjusted R-squared:  0.1034 
# F-statistic: 44.36 on 1 and 375 DF,  p-value: 9.771e-11

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         .
# emotive         ***
# evidential      n.s.
# nonEmoComm      *

# Residual standard error: 0.1558 on 367 degrees of freedom
# Multiple R-squared:  0.3834,	Adjusted R-squared:  0.3683 
# F-statistic: 25.35 on 9 and 367 DF,  p-value: < 2.2e-16

lm(V.Mean.Sum2 ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   new.scale) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction significance of D.Mean.Sum2
# cognitive       negative              *
# emoComm         negative              n.s
# emotive         negative              ***
# evidential      negative              n.s.
# nonEmoComm      negative              n.s.
# cognitive       positive              ***
# emoComm         positive              *
# emotive         positive              ***
# evidential      positive              .
# nonEmoComm      positive              *

# Residual standard error: 0.1549 on 357 degrees of freedom
# Multiple R-squared:  0.4077,	Adjusted R-squared:  0.3762 
# F-statistic: 12.93 on 19 and 357 DF,  p-value: < 2.2e-16

lm(D.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()


##### X.4.3.3 arousal + dominance ----
###### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-arousal.pdf", height = 4.5, width = 6)

# original scale
ggplot(new.scale, aes(x = A.Mean.Sum, y = D.Mean.Sum)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (controlled - in control)",
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-arousal-orig.pdf", height = 4.5, width = 6)

# faceted
# by predicate type
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-arousal-faceted.pdf", height = 5, width = 6)

# by direction of valence
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-arousal-faceted-Vdir.pdf", height = 4.5, width = 6)

# by direction of valence and dominance
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (neutral - controlled / in control)",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_grid(V.Mean.Sum2.direction ~ D.Mean.Sum2.direction, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")))) 
ggsave("../graphs/dominance-by-arousal-faceted-Vdir-Ddir.pdf", height = 5, width = 5)


ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = V.Mean.Sum2.direction)) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (neutral - controlled / in control)",
       y = "Mean arousal rating (calm - excited)", 
       colour = "Predicate type",
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-dominance.pdf", height = 4, width = 6)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-dominance-faceted.pdf", height = 5, width = 6)

###### linear models -----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.41291    0.01158  35.650   <2e-16 ***
# D.Mean.Sum2  0.01197    0.04215   0.284    0.777    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1177 on 375 degrees of freedom
# Multiple R-squared:  0.0002151,	Adjusted R-squared:  -0.002451  <<<
# F-statistic: 0.08069 on 1 and 375 DF,  p-value: 0.7765          <<<

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.1061 on 367 degrees of freedom
# Multiple R-squared:  0.205,	Adjusted R-squared:  0.1856 
# F-statistic: 10.52 on 9 and 367 DF,  p-value: 1.63e-14

lm(A.Mean.Sum2 ~ 
     D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive") , 
   new.scale) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction significance of D.Mean.Sum2
# cognitive       negative              n.s.
# emoComm         negative              .
# emotive         negative              n.s.
# evidential      negative              n.s.
# nonEmoComm      negative              n.s.
# cognitive       positive              n.s.
# emoComm         positive              n.s.
# emotive         positive              n.s.
# evidential      positive              n.s.
# nonEmoComm      positive              n.s.

# Residual standard error: 0.1036 on 357 degrees of freedom
# Multiple R-squared:  0.2622,	Adjusted R-squared:  0.2229 
# F-statistic: 6.677 on 19 and 357 DF,  p-value: 5.096e-15

##### X.4.3.4 direction of valence + direction of dominance ----
###### tables ----
contingency_table <- table(new.scale$V.Mean.Sum2.direction, new.scale$D.Mean.Sum2.direction) %>% 
  print()
#          negative positive
# negative      110       60
# positive       16      191

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 133.63, df = 1, p-value < 2.2e-16

# The directions of dominance and valence seem highly correlated.

# ranges of original values
range(new.scale$V.Mean.Sum)                             # 1.40 8.21
vdiff <- diff(range(new.scale$V.Mean.Sum)) %>% print()  # 6.81
range(new.scale$A.Mean.Sum)                             # 2.57 7.19
adiff <- diff(range(new.scale$A.Mean.Sum)) %>% print()  # 4.62
range(new.scale$D.Mean.Sum)                             # 2.56 7.29
ddiff <- diff(range(new.scale$D.Mean.Sum)) %>% print()  # 4.73

# VAD ratio
round(c(vdiff, adiff, ddiff) / min(c(vdiff, adiff, ddiff)), 2)
# 1.47 1.00 1.02

# ranges of rescaled values
range(new.scale$V.Mean.Sum2)                              # 0.0 0.9
vdiff2 <- diff(range(new.scale$V.Mean.Sum2)) %>% print()  # 0.9
range(new.scale$A.Mean.Sum2)                              # 0.19625 0.77375
adiff2 <- diff(range(new.scale$A.Mean.Sum2)) %>% print()  # 0.5775
range(new.scale$D.Mean.Sum2)                              # 0.00 0.61
ddiff2 <- diff(range(new.scale$D.Mean.Sum2)) %>% print()  # 0.61

# VAD ratio
round(c(vdiff2, adiff2, ddiff2) / min(c(vdiff2, adiff2, ddiff2)), 2)
# 1.56 1.00 1.06

new.scale %>% 
  count(V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  group_by(V.Mean.Sum2.direction) %>%
  mutate(percentage =  n * 100 / sum(n))
#   V.Mean.Sum2.direction D.Mean.Sum2.direction     n percentage
#   <chr>                 <chr>                 <int>      <dbl>
# 1 negative              negative                110      64.7 
# 2 negative              positive                 60      35.3 
# 3 positive              negative                 16       7.73
# 4 positive              positive                191      92.3 

# distribution of directions within predicate types
new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction) %>% 
  group_by(predicateType2) %>% 
  mutate(percentage =  n * 100 / sum(n),
         ratio =  round(n / min(n)))
#   predicateType2 V.Mean.Sum2.direction     n percentage ratio
#   <chr>          <chr>                 <int>      <dbl> <dbl>
# 1 cognitive      negative                 14       30.4     1
# 2 cognitive      positive                 32       69.6     2
# 3 emoComm        negative                 21       84       5
# 4 emoComm        positive                  4       16       1
# 5 emotive        negative                 63       64.9     2
# 6 emotive        positive                 34       35.1     1
# 7 evidential     negative                 15       23.4     1
# 8 evidential     positive                 49       76.6     3
# 9 nonEmoComm     negative                 57       39.3     1
# 10 nonEmoComm     positive                 88       60.7     2

new.scale %>% 
  count(predicateType2, D.Mean.Sum2.direction) %>% 
  group_by(predicateType2) %>% 
  mutate(percentage =  n * 100 / sum(n),
         ratio =  round(n / min(n)))
#   predicateType2 D.Mean.Sum2.direction     n percentage ratio
#   <chr>          <chr>                 <int>      <dbl> <dbl>
# 1 cognitive      negative                 10       21.7     1
# 2 cognitive      positive                 36       78.3     4
# 3 emoComm        negative                 13       52       1
# 4 emoComm        positive                 12       48       1
# 5 emotive        negative                 54       55.7     1
# 6 emotive        positive                 43       44.3     1
# 7 evidential     negative                 11       17.2     1
# 8 evidential     positive                 53       82.8     5
# 9 nonEmoComm     negative                 38       26.2     1
# 10 nonEmoComm     positive                107       73.8     3

# For both 'direction' categories, there are more 'positive' than 'negative' predicates in the 
# cognitives, evidentials and nonEmoComms, with negative-positive ratios of about 1:2 - 1:3 for the 
# direction of valence distinction and about 1:3 - 1:5 ratios wrt to the direction of dominance. For
# the emoComms and the emoties, there are more 'negative' than 'positive' predicates in both categories.
# Wrt the direction of valence, the differences for these predicate types are very pronounced with 
# negative-positive ratios of 5:1 and 2:1, respectively. For the direcion of dominance the ratios of
# both of these predicate types are about 1:1. Overall, the proportion of negative valence predicates
# is larger than the proportion of negative dominance predicates for all predicate types. 

# distribution of directions by predicate types
new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  mutate(percentage =  n * 100 / sum(n))
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction     n percentage
#   <chr>          <chr>                 <chr>                 <int>      <dbl>
# 1 cognitive      negative              negative                  8      57.1 
# 2 cognitive      negative              positive                  6      42.9 
# 3 cognitive      positive              negative                  2       6.25
# 4 cognitive      positive              positive                 30      93.8 
# 5 emoComm        negative              negative                 12      57.1 
# 6 emoComm        negative              positive                  9      42.9 
# 7 emoComm        positive              negative                  1      25   
# 8 emoComm        positive              positive                  3      75   
# 9 emotive        negative              negative                 52      82.5 
# 10 emotive        negative              positive                 11      17.5 
# 11 emotive        positive              negative                  2       5.88
# 12 emotive        positive              positive                 32      94.1 
# 13 evidential     negative              negative                  8      53.3 
# 14 evidential     negative              positive                  7      46.7 
# 15 evidential     positive              negative                  3       6.12
# 16 evidential     positive              positive                 46      93.9 
# 17 nonEmoComm     negative              negative                 30      52.6 
# 18 nonEmoComm     negative              positive                 27      47.4 
# 19 nonEmoComm     positive              negative                  8       9.09
# 20 nonEmoComm     positive              positive                 80      90.9 

# communicatives overall
new.scale %>% 
  count(predicateType, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  group_by(predicateType, V.Mean.Sum2.direction) %>%
  mutate(percentage =  n * 100 / sum(n))
#   predicateType V.Mean.Sum2.direction D.Mean.Sum2.direction     n percentage
#   <chr>         <chr>                 <chr>                 <int>
# 5 communicative negative              negative                 42      53.8 
# 6 communicative negative              positive                 36      46.2 
# 7 communicative positive              negative                  9       9.78
# 8 communicative positive              positive                 83      90.2 

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
ggsave("../graphs/projection-by-valence.pdf", height = 3.5, width = 6)

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
ggsave("../graphs/projection-by-valence2.pdf", height = 4, width = 6)

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
ggsave("../graphs/projection-by-valence-faceted.pdf", height = 4, width = 6)

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
ggsave("../graphs/projection-by-valence-faceted2.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.26562    0.02453  10.829  < 2e-16 ***
# V.Mean.Sum2  0.45860    0.06986   6.564 1.74e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2656 on 375 degrees of freedom
# Multiple R-squared:  0.1031,	Adjusted R-squared:  0.1007 
# F-statistic: 43.09 on 1 and 375 DF,  p-value: 1.741e-10

# by predicate type (the faceted plot with one combined fitted line)
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# predicateType   significance of V.Mean.Sum2
# communicative   n.s.
# predicateType2  significance of V.Mean.Sum2
# cognitive       n.s.
# emoComm         .
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# valence is significant, but not within any predicate type. The difference of the effect of
# valence on Mean.Proj between predicate types is sometimes significant. See section ZZ.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant) + (1 | environment),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   1.9157     0.1032   18.56   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2
# cognitive         n.s.
# emoComm           ***
# emotive           n.s.
# evidential        n.s.
# nonEmoComm        n.s.

###### largest residuals ----
# For which predicates does the linear model make the least accurate predictions?
# I don't remember why we were wondering about this. :)
residual <- resid(lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale))
cbind(new.scale, residual) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual)
#      verb_renamed predicateType2   residual
# 126         dream      cognitive -0.7442240
# 158     be fooled     evidential -0.6973864
# 81       daydream      cognitive -0.6787885
# 255       pretend     nonEmoComm -0.6758565
# 223     be misled     evidential -0.6719940
# 349    be tricked     evidential -0.6449008
# 38    be bothered        emotive  0.6368397
# 28  be astonished        emotive  0.6046528
# 148     fantasize      cognitive -0.5949242
# 190       imagine      cognitive -0.5868199

# predicates grouped by predicate type
cbind(new.scale, residual) %>% 
  group_by(predicateType2) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual) %>% 
  print(n = 50)


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
ggsave("../graphs/projection-by-valence-with-direction.pdf", height = 3.5, width = 6)

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
  geom_text(data = new.scale %>% count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

# by direction of valence
# all embedding environments
mean.proj.Vdir <- d.proj.vad %>%
  group_by(V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir) # 2

Vall <- ggplot(mean.proj.Vdir, aes(x = V.Mean.Sum2.direction, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-Vdir.pdf", height = 4, width = 4)

# neg
mean.proj.Vdir.neg <- d.proj.vad %>%
  filter(environment == "neg") %>% 
  group_by(V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir.neg) # 2

Vneg <- ggplot(mean.proj.Vdir.neg, aes(x = V.Mean.Sum2.direction, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)

# qcond
mean.proj.Vdir.qcond <- d.proj.vad %>%
  filter(environment == "qcond") %>% 
  group_by(V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir.qcond) # 2

Vqcond <- ggplot(mean.proj.Vdir.qcond, aes(x = V.Mean.Sum2.direction, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-Vdir-qcond.pdf", height = 4, width = 4)

# qcondneg
mean.proj.Vdir.qcondneg <- d.proj.vad %>%
  filter(environment == "qcondneg") %>% 
  group_by(V.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir.qcondneg) # 2

Vqcondneg <- ggplot(mean.proj.Vdir.qcondneg, aes(x = V.Mean.Sum2.direction, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-Vdir-qcondneg.pdf", height = 4, width = 4)

# comparison
Vall + labs(title = "all embedding environments") + 
  Vneg + labs(title = "negation") + 
  Vqcond + labs(title = "question + conditional") +
  Vqcondneg + labs(title = "question + conditional + negation") +
  plot_layout(axis_titles = "collect", nrow = 2) &
  theme(plot.title = element_text(size = 12))
ggsave("../graphs/projection-by-Vdir-comparison.pdf", height = 6, width = 6.5)

# by direction of valence and predicate type
# all embedding environments
mean.proj.Vdir2 <- d.proj.vad %>%
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir2) # 10

Vall2 <- ggplot(mean.proj.Vdir2, aes(x = predicateType2, y = Mean.Proj, colour = V.Mean.Sum2.direction)) +
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
ggsave("../graphs/projection-by-Vdir-predType2.pdf", height = 4, width = 7.5)

# neg
mean.proj.Vdir2.neg <- d.proj.vad %>%
  filter(environment == "neg") %>% 
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir2.neg) # 10

Vneg2 <- ggplot(mean.proj.Vdir2.neg, aes(x = predicateType2, y = Mean.Proj, colour = V.Mean.Sum2.direction)) +
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

# qcond
mean.proj.Vdir2.qcond <- d.proj.vad %>%
  filter(environment == "qcond") %>% 
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir2.qcond) # 10

Vqcond2 <- ggplot(mean.proj.Vdir2.qcond, aes(x = predicateType2, y = Mean.Proj, colour = V.Mean.Sum2.direction)) +
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
ggsave("../graphs/projection-by-Vdir-predType2-qcond.pdf", height = 4, width = 7.5)

# qcondneg
mean.proj.Vdir2.qcondneg <- d.proj.vad %>%
  filter(environment == "qcondneg") %>% 
  group_by(V.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Vdir2.qcondneg) # 10

Vqcondneg2 <- 
  ggplot(mean.proj.Vdir2.qcondneg, aes(x = predicateType2, y = Mean.Proj, colour = V.Mean.Sum2.direction)) +
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
  scale_y_continuous(limits = c(-.2, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names)
ggsave("../graphs/projection-by-Vdir-predType2-qcondneg.pdf", height = 4, width = 7.5)

# comparison
wrap_elements(get_legend(Vall2 + 
                           theme(legend.title = element_text(size = 18),
                                 legend.text = element_text(size = 16)))) /
(Vall2 + labs(title = "all embedding environments") + 
  Vneg2 + labs(title = "negation") + 
  Vqcond2 + labs(title = "question + conditional") +
  Vqcondneg2 + labs(title = "question + conditional + negation") +
  plot_layout(axis_titles = "collect", ncol = 2) &
   scale_y_continuous(limits = c(-.2, 1), breaks = c(0, 0.5, 1)) & 
  theme(legend.position = "none", 
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 20))) +
  plot_layout(heights = c(.1, 1))
ggsave("../graphs/projection-by-Vdir-comparison-predType2.pdf", height = 10, width = 15.5)

###### linear models ----
# by direction of valence
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.45170    0.02120  21.309  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.09547    0.02861  -3.337 0.000931 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2764 on 375 degrees of freedom
# Multiple R-squared:  0.02884,	Adjusted R-squared:  0.02625 
# F-statistic: 11.14 on 1 and 375 DF,  p-value: 0.0009308

# by direction of valence and predicate type
lm(Mean.Proj ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType   significance of V.Mean.Sum2.direction
# communicative   n.s.
# predicateType2  significance of V.Mean.Sum2.direction
# cognitive       .
# emoComm         n.s.
# emotive         n.s.
# evidential      **
# nonEmoComm      n.s.

# by valence and direction of valence
lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.28554    0.03666   7.789 6.75e-14 ***
# V.Mean.Sum2                                0.52013    0.09579   5.430 1.02e-07 ***
# V.Mean.Sum2.directionpositive             -0.01987    0.04928  -0.403    0.687    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.18280    0.13992  -1.306    0.192    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2632 on 373 degrees of freedom
# Multiple R-squared:  0.1238,	Adjusted R-squared:  0.1168 
# F-statistic: 17.57 on 3 and 373 DF,  p-value: 1.093e-10

# V.Mean.Sum2.direction significance of V.Mean.Sum2
# negative              ***
# positive              **

# by predicate type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * fct_relevel(predicateType2, "emoComm"), 
   data = new.scale) %>% 
  summary()
# predicateType	  V.Mean.Sum2.direction   significance of V.Mean.Sum2
# communicative   negative                n.s.
# communicative   positive                n.s.
# predicateType2	V.Mean.Sum2.direction   significance of V.Mean.Sum2
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

# Residual standard error: 0.1944 on 357 degrees of freedom
# Multiple R-squared:  0.5425,	Adjusted R-squared:  0.5181 
# F-statistic: 22.28 on 19 and 357 DF,  p-value: < 2.2e-16

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant) + 
       (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.41237    0.03941  -10.46   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                                                                Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive                                                   -0.3351     0.1952  -1.716  0.08608 .  
# fct_relevel(predicateType2, "emoComm")cognitive                                 -0.7112     0.1360  -5.231 1.69e-07 ***
# fct_relevel(predicateType2, "emoComm")emotive                                    1.2228     0.1045  11.699  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")evidential                                -1.1234     0.1339  -8.388  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                -0.7058     0.1011  -6.979 2.97e-12 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")cognitive   -0.1074     0.2316  -0.464  0.64275    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")emotive      0.5942     0.2198   2.704  0.00685 ** 
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")evidential   0.8984     0.2269   3.960 7.50e-05 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")nonEmoComm   0.3391     0.2058   1.648  0.09933 . 

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                2.21967    0.14589  15.214  < 2e-16 ***
# V.Mean.Sum2.directionpositive             -0.08036    0.07090  -1.133    0.257    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.83993    0.20657  -4.066 4.78e-05 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2  direction significance of V.Mean.Sum2
# cognitive       negative  n.s.
# emoComm         negative  ***
# emotive         negative  
# evidential      negative  
# nonEmoComm      negative  n.s.
# cognitive       positive  
# emoComm         positive  *
# emotive         positive  
# evidential      positive  
# nonEmoComm      positive  n.s.

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
ggsave("../graphs/projection-by-valence-communicative-faceted.pdf", height = 4, width = 6)

##### X.5.1.4 say verbs ----
###### X.5.1.4.1 say-verb type ----
####### plot ----
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
  facet_wrap(~ sayVerbType) 
ggsave("../graphs/projection-by-valence-sayverb-faceted.pdf", height = 4, width = 5)

###### X.5.1.4.2 mode-verb type ---- 
####### plot ----
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
  facet_wrap(~ modeVerbType) 
ggsave("../graphs/projection-by-valence-modeverb-faceted.pdf", height = 4, width = 5)

###### X.5.1.4.3 say-by-means-verb type ----
####### plot ----
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
  facet_wrap(~ sayByMeansVerbType, ncol = 4) 
ggsave("../graphs/projection-by-valence-saybymeans-faceted.pdf", height = 4, width = 6)

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
ggsave("../graphs/projection-by-arousal-faceted.pdf", height = 4, width = 6)

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
ggsave("../graphs/projection-by-arousal-faceted2.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.12359    0.05106   2.421    0.016 *  
# A.Mean.Sum2  0.66315    0.11820   5.611 3.92e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2694 on 375 degrees of freedom
# Multiple R-squared:  0.07744,	Adjusted R-squared:  0.07498 
# F-statistic: 31.48 on 1 and 375 DF,  p-value: 3.922e-08

lm(Mean.Proj ~ A.Mean.Sum2 + V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.11813    0.04966   2.379 0.017872 *  
# A.Mean.Sum2  0.42608    0.12530   3.401 0.000745 ***
# V.Mean.Sum2  0.35690    0.07511   4.752 2.88e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2619 on 374 degrees of freedom
# Multiple R-squared:   0.13,	Adjusted R-squared:  0.1253 
# F-statistic: 27.94 on 2 and 374 DF,  p-value: 4.93e-12

lm(Mean.Proj ~ predicateType2 + A.Mean.Sum2 + V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.25064    0.04705   5.327 1.73e-07 ***
# predicateType2emoComm     0.22985    0.04959   4.635 4.96e-06 *** <<<
# predicateType2emotive     0.52272    0.03737  13.987  < 2e-16 ***
# predicateType2evidential  0.05934    0.03842   1.545    0.123    
# predicateType2nonEmoComm  0.06169    0.03419   1.804    0.072 .   <<<
# A.Mean.Sum2              -0.08008    0.09920  -0.807    0.420    
# V.Mean.Sum2              -0.00552    0.06245  -0.088    0.930    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1963 on 370 degrees of freedom
# Multiple R-squared:  0.5167,	Adjusted R-squared:  0.5089 
# F-statistic: 65.93 on 6 and 370 DF,  p-value: < 2.2e-16

# With predicateType2 as the only predictor, the model has an adjusted R-squared of 0.5157. Adding
# A.Mean.Sum2 and V.Mean.Sum2 results in a R-squared of 0.5167, i.e., these two factors add nothing
# to the model.

# by predicate type (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType	  significance of A.Mean.Sum2
# communicative   n.s.
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

# Residual standard error: 0.1967 on 367 degrees of freedom
# Multiple R-squared:  0.5184,	Adjusted R-squared:  0.5066 
# F-statistic:  43.9 on 9 and 367 DF,  p-value: < 2.2e-16

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.5594     0.1707      15   <2e-16 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       .
# emoComm         n.s. 
# emotive         n.s.   
# evidential      n.s.
# nonEmoComm      .


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
ggsave("../graphs/projection-by-arousal-with-direction.pdf", height = 3.5, width = 6)

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
  geom_text(data = new.scale %>% count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

###### linear models ----
# arousal and direction of valence
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), data = new.scale) %>% 
  summary()
# V.Mean.Sum2.direction significance of A.Mean.Sum2
# negative              ***
# positive              ***

# by predicate type and direction of valence (the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * fct_relevel(predicateType2, "emotive"), 
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

# Residual standard error: 0.1949 on 357 degrees of freedom
# Multiple R-squared:  0.5404,	Adjusted R-squared:  0.516 
# F-statistic: 22.09 on 19 and 357 DF,  p-value: < 2.2e-16

####### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                 2.3365     0.2580   9.054   <2e-16 ***
# V.Mean.Sum2.directionpositive              -0.1501     0.1546  -0.971    0.331    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.2205     0.3586  -0.615    0.539     

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
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
# nonEmoComm	    positive  *

# number of predicates of each predicate type in the two valence categories
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
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients: 
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.1784     0.2553   8.532   <2e-16 *** [negative]

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.2573     0.2609   8.652   <2e-16 *** [positive]

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant) + (1 | environment), 
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
# nonEmoComm	    positive  *

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
ggsave("../graphs/projection-by-arousal-communicative-faceted.pdf", height = 4, width = 6)

##### X.5.2.4 say verbs ----
###### X.5.2.4.1 say-verb type ----
####### plot ----
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
  facet_wrap(~ sayVerbType) 
ggsave("../graphs/projection-by-arousal-sayverb-faceted.pdf", height = 4, width = 5)

###### X.5.2.4.2 mode-verb type ---- 
####### plot ----
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
  facet_wrap(~ modeVerbType) 
ggsave("../graphs/projection-by-arousal-modeverb-faceted.pdf", height = 4, width = 5)

###### X.5.2.4.3 say-by-means-verb type ----
####### plot ----
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
  facet_wrap(~ sayByMeansVerbType, ncol = 4) 
ggsave("../graphs/projection-by-arousal-saybymeans-faceted.pdf", height = 4, width = 6)


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
       colour = "Predicate type",
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-dominance2.pdf", height = 4, width = 6)

ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = D.Mean.Sum2.direction)) +
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
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-faceted2.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.38377    0.02759  13.911   <2e-16 ***
# D.Mean.Sum2  0.06621    0.10040   0.659     0.51    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2803 on 375 degrees of freedom
# Multiple R-squared:  0.001158,	Adjusted R-squared:  -0.001505 
# F-statistic: 0.4349 on 1 and 375 DF,  p-value: 0.51

# by predicate type (the faceted plot with one combined fitted line)
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# communicative   n.s.
# predicateType2  significance of D.Mean.Sum2
# cognitive:      n.s.
# emoComm:        *
# emotive:        n.s.
# evidential:     n.s.
# nonEmoComm:     n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# D.Mean.Sum2   0.3342     0.1352   2.472   0.0134 *

clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         ***
# emotive         **   
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
ggsave("../graphs/projection-by-domiance-with-direction-of-valence.pdf", height = 3.5, width = 6)

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
  geom_text(data = new.scale %>% count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y= -1, label=paste0("n = ",n)))
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

# faceted by valence, two fitted lines for dominance
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5, 
              aes(linetype = D.Mean.Sum2.direction)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.grid.minor.y = element_blank()) + 
  # plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating", 
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) +
  geom_text(data = new.scale %>% count(predicateType2, V.Mean.Sum2.direction), 
            aes(x = .9, y = -1, label = paste0("n = ", n)))
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-Ddir.pdf", 
       height = 6, width = 10)

###### linear models ----
# dominance and direction of valence
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), data = new.scale) %>% 
  summary()
# V.Mean.Sum2.direction significance of D.Mean.Sum2
# negative              n.s.
# positive              n.s.

# by predicate type and direction of valence (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * fct_relevel(predicateType2, "nonEmoComm"), 
   data = new.scale) %>% 
  summary()
# predicateType	  V.Mean.Sum2.direction   significance of D.Mean.Sum2
# communicative   negative                *
# communicative   positive                n.s.
# predicateType2	V.Mean.Sum2.direction   significance of D.Mean.Sum2
# cognitive       negative                n.s.
# emoComm         negative                *
# emotive         negative                .
# evidential      negative                *
# nonEmoComm      negative                n.s.
# cognitive       positive                n.s.
# emoComm         positive                n.s.
# emotive         positive                n.s.
# evidential      positive                .
# nonEmoComm      positive                n.s.

# Residual standard error: 0.1916 on 357 degrees of freedom
# Multiple R-squared:  0.5559,	Adjusted R-squared:  0.5323 
# F-statistic: 23.52 on 19 and 357 DF,  p-value: < 2.2e-16

###### ordinal models ----
# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                0.77050    0.21181   3.638 0.000275 ***
# V.Mean.Sum2.directionpositive             -0.42223    0.07436  -5.678 1.36e-08 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive -0.15880    0.28156  -0.564 0.572751 

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         ***
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
# i.e., somebody else's vs one's own dominance
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
ggsave("../graphs/projection-by-domiance-with-direction-of-dominance.pdf", height = 3.5, width = 6)

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
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2.pdf", 
       height = 6, width = 10)

# by direction of dominance
# all embedding environments
mean.proj.Ddir <- d.proj.vad %>%
  group_by(D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir) # 2

Dall <- ggplot(mean.proj.Ddir, aes(x = D.Mean.Sum2.direction, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-Ddir.pdf", height = 4, width = 4)

# neg
mean.proj.Ddir.neg <- d.proj.vad %>%
  filter(environment == "neg") %>% 
  group_by(D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir.neg) # 2

Dneg <- ggplot(mean.proj.Ddir.neg, aes(x = D.Mean.Sum2.direction, y = Mean.Proj)) +
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

# qcond
mean.proj.Ddir.qcond <- d.proj.vad %>%
  filter(environment == "qcond") %>% 
  group_by(D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir.qcond) # 2

Dqcond <- ggplot(mean.proj.Ddir.qcond, aes(x = D.Mean.Sum2.direction, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-Ddir-qcond.pdf", height = 4, width = 4)

# qcondneg
mean.proj.Ddir.qcondneg <- d.proj.vad %>%
  filter(environment == "qcondneg") %>% 
  group_by(D.Mean.Sum2.direction) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir.qcondneg) # 2

Dqcondneg <- ggplot(mean.proj.Ddir.qcondneg, aes(x = D.Mean.Sum2.direction, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-Ddir-qcondneg.pdf", height = 4, width = 4)

# comparison
Dall + labs(title = "all embedding environments") + 
  Dneg + labs(title = "negation") + 
  Dqcond + labs(title = "question + conditional") +
  Dqcondneg + labs(title = "question + conditional + negation") +
  plot_layout(axis_titles = "collect", nrow = 2) &
  theme(plot.title = element_text(size = 12))
ggsave("../graphs/projection-by-Ddir-comparison.pdf", height = 6, width = 6.5)

# by direction of dominance and predicate type
# all embedding environments
mean.proj.Ddir2 <- d.proj.vad %>%
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir2) # 10

Dall2 <- ggplot(mean.proj.Ddir2, aes(x = predicateType2, y = Mean.Proj, colour = D.Mean.Sum2.direction)) +
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
ggsave("../graphs/projection-by-Ddir-predType2.pdf", height = 4, width = 7.5)

# neg
mean.proj.Ddir2.neg <- d.proj.vad %>%
  filter(environment == "neg") %>% 
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir2.neg) # 10

Dneg2 <- ggplot(mean.proj.Ddir2.neg, aes(x = predicateType2, y = Mean.Proj, colour = D.Mean.Sum2.direction)) +
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
ggsave("../graphs/projection-by-Ddir-predType2.pdf", height = 4, width = 7.5)

# qcond
mean.proj.Ddir2.qcond <- d.proj.vad %>%
  filter(environment == "qcond") %>% 
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir2.qcond) # 10

Dqcond2 <- ggplot(mean.proj.Ddir2.qcond, aes(x = predicateType2, y = Mean.Proj, colour = D.Mean.Sum2.direction)) +
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
  scale_y_continuous(limits = c(-.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names)
ggsave("../graphs/projection-by-Ddir-predType2-qcond.pdf", height = 4, width = 7.5)

# qcondneg
mean.proj.Ddir2.qcondneg <- d.proj.vad %>%
  filter(environment == "qcondneg") %>% 
  group_by(D.Mean.Sum2.direction, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.Ddir2.qcondneg) # 10

Dqcondneg2 <- ggplot(mean.proj.Ddir2.qcondneg, aes(x = predicateType2, y = Mean.Proj, colour = D.Mean.Sum2.direction)) +
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
  scale_y_continuous(limits = c(-.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_x_discrete(labels = predicateType2_names)
ggsave("../graphs/projection-by-Ddir-predType2-qcondneg.pdf", height = 4, width = 7.5)

# comparison
wrap_elements(get_legend(Dall2 + 
                           theme(legend.title = element_text(size = 18),
                                 legend.text = element_text(size = 16)))) /
  (Dall2 + labs(title = "all embedding environments") + 
     Dneg2 + labs(title = "negation") + 
     Dqcond2 + labs(title = "question + conditional") +
     Dqcondneg2 + labs(title = "question + conditional + negation") +
     plot_layout(axis_titles = "collect", ncol = 2) &
     scale_y_continuous(limits = c(-.2, 1), breaks = c(0, 0.5, 1)) & 
     theme(legend.position = "none", 
           plot.title = element_text(size = 14),
           axis.title = element_text(size = 20))) +
  plot_layout(heights = c(.1, 1))
ggsave("../graphs/projection-by-Ddir-comparison-predType2.pdf", height = 10, width = 15.5)


###### linear models ----
# by direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.46905    0.02459  19.073  < 2e-16 ***
# D.Mean.Sum2.directionpositive -0.10479    0.03014  -3.477 0.000566 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.276 on 375 degrees of freedom
# Multiple R-squared:  0.03123,	Adjusted R-squared:  0.02865 
# F-statistic: 12.09 on 1 and 375 DF,  p-value: 0.0005665

# by direction of dominance and predicate type
lm(Mean.Proj ~ D.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType   significance of D.Mean.Sum2.direction
# communicative   n.s.
# predicateType2  significance of D.Mean.Sum2.direction
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      ***
# nonEmoComm      n.s.

# dominance and direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "negative"), data = new.scale) %>% 
  summary()
# D.Mean.Sum2.direction significance of D.Mean.Sum2
# negative              n.s.
# positive              n.s.

# by predicate type and direction of dominance (the faceted plot)
lm(Mean.Proj ~ 
     D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive") * fct_relevel(predicateType2, "evidential"), 
   data = new.scale) %>% 
  summary()
# predicateType	  D.Mean.Sum2.direction   significance of D.Mean.Sum2
# communicative   negative                *
# communicative   positive                n.s.
# predicateType2	D.Mean.Sum2.direction   significance of D.Mean.Sum2
# cognitive       negative                n.s.
# emoComm         negative                *
# emotive         negative                *
# evidential      negative                *
# nonEmoComm      negative                n.s.
# cognitive       positive                n.s.
# emoComm         positive                n.s.
# emotive         positive                n.s.
# evidential      positive                n.s.
# nonEmoComm      positive                n.s.

# Residual standard error: 0.1905 on 357 degrees of freedom
# Multiple R-squared:  0.561,	Adjusted R-squared:  0.5376 
# F-statistic: 24.01 on 19 and 357 DF,  p-value: < 2.2e-16

###### ordinal models ----
# with 'direction' of dominance
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * D.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                1.24004    0.23222   5.340 9.29e-08 ***
# D.Mean.Sum2.directionpositive             -0.28257    0.07499  -3.768 0.000165 ***
# D.Mean.Sum2:D.Mean.Sum2.directionpositive -0.98643    0.28982  -3.404 0.000665 ***

# with 'direction' of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         ***
# negative  evidential      
# negative  nonEmoComm      n.s.
# positive  cognitive       
# positive  emotive         
# positive  emoComm         n.s.
# positive  evidential      
# positive  nonEmoComm      *

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
ggsave("../graphs/projection-by-dominance-communicative-faceted.pdf", height = 4, width = 6)

##### X.5.3.5 say verbs ----
###### X.5.3.5.1 say-verb type ----
####### plot ----
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
  facet_wrap(~ sayVerbType) 
ggsave("../graphs/projection-by-dominance-sayverb-faceted.pdf", height = 4, width = 5)

###### X.5.3.5.2 mode-verb type ---- 
####### plot ----
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
  facet_wrap(~ modeVerbType) 
ggsave("../graphs/projection-by-dominance-modeverb-faceted.pdf", height = 4, width = 5)

###### X.5.3.5.3 say-by-means-verb type ----
####### plot ----
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
  facet_wrap(~ sayByMeansVerbType, ncol = 4) 
ggsave("../graphs/projection-by-dominance-saybymeans-faceted.pdf", height = 4, width = 6)


#### X.5.4 valence + arousal ----
##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.11813    0.04966   2.379 0.017872 *  
# V.Mean.Sum2  0.35690    0.07511   4.752 2.88e-06 ***
# A.Mean.Sum2  0.42608    0.12530   3.401 0.000745 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2619 on 374 degrees of freedom
# Multiple R-squared:   0.13,	Adjusted R-squared:  0.1253 
# F-statistic: 27.94 on 2 and 374 DF,  p-value: 4.93e-12

library(car)
vif(lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale))
# V.Mean.Sum2 A.Mean.Sum2 
# 1.188422    1.188422 

lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)              0.13166    0.09356   1.407   0.1602  
# V.Mean.Sum2              0.31364    0.26448   1.186   0.2364  
# A.Mean.Sum2              0.39313    0.23029   1.707   0.0886 .
# V.Mean.Sum2:A.Mean.Sum2  0.09811    0.57500   0.171   0.8646  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2623 on 373 degrees of freedom
# Multiple R-squared:   0.13,	Adjusted R-squared:  0.123 
# F-statistic: 18.58 on 3 and 373 DF,  p-value: 2.951e-11

vif(lm(Mean.Proj ~ V.Mean.Sum2 * A.Mean.Sum2, data = new.scale))
# V.Mean.Sum2             A.Mean.Sum2 V.Mean.Sum2:A.Mean.Sum2 
#   14.697613                4.004393               22.236947 

lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   0.398380   0.014498  27.479  < 2e-16 ***
# V.Mean.Sum2.sc                0.069493   0.015018   4.627 5.12e-06 ***
# A.Mean.Sum2.sc                0.049568   0.015048   3.294  0.00108 ** 
# V.Mean.Sum2.sc:A.Mean.Sum2.sc 0.002261   0.013251   0.171  0.86461    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2623 on 373 degrees of freedom
# Multiple R-squared:   0.13,	Adjusted R-squared:  0.123 
# F-statistic: 18.58 on 3 and 373 DF,  p-value: 2.951e-11

vif(lm(Mean.Proj ~ V.Mean.Sum2.sc * A.Mean.Sum2.sc, data = new.scale))
# V.Mean.Sum2.sc                A.Mean.Sum2.sc V.Mean.Sum2.sc:A.Mean.Sum2.sc 
#       1.232741                      1.237680                      1.130786 

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
       (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2               1.1062     0.3360   3.292 0.000996 ***
# V.Mean.Sum2               0.9359     0.3936   2.377 0.017431 *  
# A.Mean.Sum2:V.Mean.Sum2   1.4276     0.8666   1.647 0.099483 . 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive           
# emoComm         *     ***  
# emotive          
# evidential      
# nonEmoComm      n.s.  n.s.

##### negative / positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
       (1 | environment), data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [negative]
# A.Mean.Sum2               2.0501     0.5002   4.098 4.16e-05 ***
# V.Mean.Sum2               3.7999     0.6292   6.039 1.55e-09 ***
# A.Mean.Sum2:V.Mean.Sum2  -4.2317     1.3057  -3.241  0.00119 ** 


# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [positive]
# A.Mean.Sum2              0.005039   0.482831   0.010   0.9917    
# V.Mean.Sum2             -0.903124   0.535123  -1.688   0.0915 .  
# A.Mean.Sum2:V.Mean.Sum2  5.197391   1.250262   4.157 3.22e-05 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant) + (1 | environment), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive	      negative       
# emoComm	      	negative	*     ***
# emotive		      negative	
# evidential	    negative  
# nonEmoComm	    negative  n.s.  n.s.
# cognitive	      positive  
# emoComm		      positive  n.s.  n.s.
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  n.s.  n.s.


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
ggsave("../graphs/projection-by-VAD-with-direction-of-valence.pdf", height = 4.5, width = 9.5)

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
aplotf <- ggplot(new.scale, aes(x = A.Mean.Sum2, y = Mean.Proj)) +
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
dplotf <- 
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-facets.pdf", height = 5.5, width = 10)

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
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir.pdf", height = 5.5, width = 9)

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
ggsave("../graphs/projection-by-VAD-facets-Vdir-Ddir-2.pdf", height = 5.5, width = 14)

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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-valence-Vdir-Ddir-predType2.pdf", height = 10, width = 11)

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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-arousal-Vdir-Ddir-predType2.pdf", height = 10, width = 11)


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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
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

dplotf3.1 + dplotf3.2 + plot_layout(design = layout) + theme(axis.title.y = element_text(size = 16, hjust = 2.8),
                                                             axis.title.x = element_text(size = 16, hjust = .85)) +
  labs(x = "Mean dominance rating",
       y = "Mean projection rating")
ggsave("../graphs/projection-by-dominance-Vdir-Ddir-predType2.pdf", height = 10, width = 11)

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
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2.pdf", height = 4.5, width = 13.5)

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
        legend.text = element_text(size = 11),
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
        legend.text = element_text(size = 11),
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

layout <- "
AAAAAAAAAA
BBBBBBB###
"
VAD_lines1 + VAD_lines2 + plot_layout(design = layout) + 
  theme(axis.title.y = element_text(size = 16, hjust = 2.8),
        axis.title.x = element_text(size = 16, hjust = 1.2)) +
  labs(x = "Mean valence / arousal / dominance rating",
       y = "Mean projection rating")
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-wrapped.pdf", height = 10, width = 11)

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
  theme(panel.grid.minor = element_blank(), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 9),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
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
ggsave("../graphs/projection-by-VAD-lines-Vdir-Ddir-predType2-scaled.pdf", height = 4.5, width = 13.5)

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2 * 
     fct_relevel(V.Mean.Sum2.direction, "positive") * 
     fct_relevel(D.Mean.Sum2.direction, "positive") * 
     fct_relevel(predicateType2, "nonEmoComm"), 
   data = new.scale) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction D.Mean.Sum2.direction V.Mean.Sum2 A.Mean.Sum2 D.Mean.Sum2
# cognitive       negative              negative              n.s.        n.s.        n.s.
# cognitive       negative              positive              n.s.        n.s.        *
# cognitive       positive              negative              n.s.        n.s.        n.s.
# cognitive       positive              positive              n.s.        n.s.        n.s.
# emoComm         negative              negative              n.s.        n.s.        *
# emoComm         negative              positive              n.s.        n.s.        n.s.
# emoComm         positive              negative              n.s.        n.s.        n/a
# emoComm         positive              positive              n.s.        n.s.        n.s.
# emotive         negative              negative              n.s.        n.s.        *
# emotive         negative              positive              n.s.        n.s.        n.s.
# emotive         positive              negative              n.s.        n.s.        n.s.
# emotive         positive              positive              n.s.        n.s.        n.s.
# evidential      negative              negative              n.s.        n.s.        *
# evidential      negative              positive              n.s.        *           n.s.
# evidential      positive              negative              n.s.        n.s.        n.s.
# evidential      positive              positive              n.s.        n.s.        .
# nonEmoComm      negative              negative              n.s.        n.s.        n.s.
# nonEmoComm      negative              positive              n.s.        n.s.        n.s.
# nonEmoComm      positive              negative              n.s.        n.s.        n.s.
# nonEmoComm      positive              positive              n.s.        n.s.        n.s.

# Residual standard error: 0.1951 on 338 degrees of freedom
# Multiple R-squared:  0.564,	Adjusted R-squared:  0.5149 
# F-statistic:  11.5 on 38 and 338 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ D.Mean.Sum2 * 
     fct_relevel(V.Mean.Sum2.direction, "positive") * 
     fct_relevel(D.Mean.Sum2.direction, "positive"), 
   data = new.scale %>% filter(predicateType2 == "cognitive")) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction D.Mean.Sum2.direction V.Mean.Sum2 A.Mean.Sum2 D.Mean.Sum2
# cognitive       negative              negative              n.s.        n.s.        n.s.
# cognitive       negative              positive              n.s.        n.s.        n.s.
# cognitive       positive              negative              n.s.        n.s.        n.s.
# cognitive       positive              positive              n.s.        n.s.        n.s.
# emoComm         negative              negative              n.s.        n.s.        *
# emoComm         negative              positive              .           n.s.        n.s.  
# emoComm         positive              negative              n.s.        n.s.        n.s.
# emoComm         positive              positive              n.s.        n.s.        n.s.
# emotive         negative              negative              n.s.        n.s.        **
# emotive         negative              positive              n.s.        n.s.        n.s.
# emotive         positive              negative              n.s.        n.s.        n.s.
# emotive         positive              positive              n.s.        n.s.        n.s.
# evidential      negative              negative              n.s.        n.s.        .
# evidential      negative              positive              n.s.        *           n.s.
# evidential      positive              negative              n.s.        n.s.        n.s.
# evidential      positive              positive              n.s.        n.s.        n.s.
# nonEmoComm      negative              negative              .           n.s.        n.s.
# nonEmoComm      negative              positive              n.s.        n.s.        n.s.
# nonEmoComm      positive              negative              n.s.        n.s.        n.s.
# nonEmoComm      positive              positive              n.s.        n.s.        n.s.


lm(Mean.Proj ~ D.Mean.Sum2, 
   new.scale %>% 
     filter(V.Mean.Sum2.direction == "positive" & 
              D.Mean.Sum2.direction == "positive" &
              predicateType2 == "cognitive")) %>% 
  summary()
# predicateType2  V.Mean.Sum2.direction D.Mean.Sum2.direction V.Mean.Sum2 A.Mean.Sum2 D.Mean.Sum2
# cognitive       negative              negative              n.s.        n.s.        n.s.
# cognitive       negative              positive              n.s.        n.s.        *
# cognitive       positive              negative              n.s.        n.s.        n.s.
# cognitive       positive              positive              n.s.        n.s.        n.s.
# emoComm         negative              negative              n.s.        n.s.        *
# emoComm         negative              positive              **          .           n.s.
# emoComm         positive              negative              n.s.        n.s.        n.s.
# emoComm         positive              positive              n.s.        n.s.        n.s.
# emotive         negative              negative              n.s.        n.s.        * 
# emotive         negative              positive              n.s.        n.s.        n.s.
# emotive         positive              negative              n.s.        n.s.        n.s.
# emotive         positive              positive              n.s.        .           n.s.
# evidential      negative              negative              n.s.        n.s.        n.s.
# evidential      negative              positive              n.s.        .           n.s.  
# evidential      positive              negative              n.s.        n.s.        n.s.  
# evidential      positive              positive              n.s.        n.s.        .
# nonEmoComm      negative              negative              .           n.s.        n.s.
# nonEmoComm      negative              positive              n.s.        n.s.        n.s.
# nonEmoComm      positive              negative              n.s.        n.s.        n.s.
# nonEmoComm      positive              positive              n.s.        n.s.        n.s.
  
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
# 1 cognitive      negative              positive                 0.306  0.0889 0.0778    0.217      0.383     6
# 2 cognitive      negative              negative                 0.289  0.0795 0.0753    0.209      0.364     8
# 3 cognitive      positive              positive                 0.186  0.0345 0.0367    0.151      0.222    30
# 4 cognitive      positive              negative                 0.183  0.15   0.15      0.0333     0.333     2
# 5 emoComm        negative              negative                 0.472  0.0583 0.0556    0.414      0.528    12
# 6 emoComm        negative              positive                 0.422  0.0779 0.0704    0.344      0.493     9
# 7 emoComm        positive              positive                 0.4    0.133  0.133     0.267      0.533     3
# 8 emoComm        positive              negative                 0.367  0.2    0.2       0.167      0.567     1
# 9 emotive        positive              positive                 0.770  0.0282 0.0292    0.741      0.799    32
# 10 emotive        negative              negative                 0.715  0.0238 0.0244    0.691      0.739    52
# 11 emotive        negative              positive                 0.713  0.0579 0.0579    0.655      0.771    11
# 12 emotive        positive              negative                 0.65   0.167  0.15      0.483      0.8       2
# 13 evidential     positive              positive                 0.323  0.0312 0.0327    0.291      0.355    46
# 14 evidential     negative              positive                 0.287  0.0861 0.0958    0.201      0.383     7
# 15 evidential     positive              negative                 0.244  0.111  0.111     0.133      0.356     3
# 16 evidential     negative              negative                 0.0333 0.0958 0.0918   -0.0625     0.125     8
# 17 nonEmoComm     negative              negative                 0.292  0.0356 0.0368    0.256      0.329    30
# 18 nonEmoComm     positive              positive                 0.282  0.0230 0.0234    0.259      0.306    80
# 19 nonEmoComm     positive              negative                 0.271  0.0708 0.0792    0.2        0.35      8
# 20 nonEmoComm     negative              positive                 0.263  0.0408 0.0421    0.222      0.305    27

# negative/positive valence vs negative/positive dominance predicates
# negative valence, negative dominance
new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1   be tortured      0.9000      0.5600        emotive
# 2   be stressed      0.8025      0.2875        emotive
# 3          hate      0.7600      0.1325        emotive
# 4     be pained      0.7500      0.3825        emotive
# 5    be worried      0.7250      0.4575        emotive
# 6         worry      0.7250      0.4575        emotive
# 7           lie      0.6525      0.0700     nonEmoComm
# 8       suspect      0.6525      0.4450      cognitive
# 9         mourn      0.6500      0.2300        emotive
# 10 be disgraced      0.6375      0.4200        emotive
# 11      be hurt      0.6375      0.3175        emotive
# 12     be upset      0.6375      0.1750        emotive

# negative valence, positive dominance
new.scale %>% 
  filter(V.Mean.Sum2.direction == "negative" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#      verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1      be angered      0.6250      0.0350        emotive
# 2  be intimidated      0.5400      0.1875        emotive
# 3        complain      0.4750      0.0250        emoComm
# 4         dislike      0.4375      0.2025        emotive
# 5            pout      0.4175      0.1300        emoComm
# 6          regret      0.3975      0.1575        emotive
# 7           scoff      0.3825      0.1750     nonEmoComm
# 8      disapprove      0.3625      0.0525        emotive
# 9           gloat      0.3300      0.2750        emoComm
# 10         holler      0.3300      0.1550     nonEmoComm

# positive valence, negative dominance
new.scale %>% 
  filter(V.Mean.Sum2.direction == "positive" & D.Mean.Sum2.direction == "negative") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1         check      0.3875      0.0825     evidential
# 2      showcase      0.3225      0.1500     nonEmoComm
# 3         phone      0.2725      0.1400     nonEmoComm
# 4     challenge      0.2375      0.0800     nonEmoComm
# 5        expect      0.2375      0.0275      cognitive
# 6       contest      0.2125      0.0175     nonEmoComm
# 7   be incensed      0.1975      0.1375        emotive
# 8       witness      0.1525      0.1400     evidential
# 9         admit      0.1400      0.1100     nonEmoComm
# 10      retract      0.0950      0.0350     nonEmoComm

# positive valence, positive dominance
new.scale %>% 
  filter(V.Mean.Sum2.direction == "positive" & D.Mean.Sum2.direction == "positive") %>% 
  slice_max(V.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, V.Mean.Sum2, D.Mean.Sum2, predicateType2)
#    verb_renamed V.Mean.Sum2 D.Mean.Sum2 predicateType2
# 1  be delighted      0.8025      0.5725        emotive
# 2          love      0.7500      0.2300        emotive
# 3          joke      0.7200      0.1975     nonEmoComm
# 4    be excited      0.6975      0.1975        emotive
# 5         enjoy      0.6675      0.5700        emotive
# 6        praise      0.6625      0.3925     nonEmoComm
# 7          care      0.6600      0.3900        emotive
# 8          sing      0.6250      0.4175     nonEmoComm
# 9          hope      0.6200      0.4450      cognitive
# 10         like      0.6100      0.3200        emotive
# 11 be surprised      0.6100      0.0425        emotive

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
# 1 negative              negative              emotive           52         97     0.536 - in the neg-neg group, there are 52 emotives, which is 53.6% of this predicate type.
# 2 negative              negative              nonEmoComm        30        145     0.207 
# 3 negative              negative              emoComm           12         25     0.48  
# 4 negative              negative              cognitive          8         46     0.174 
# 5 negative              negative              evidential         8         64     0.125 

# 6 negative              positive              nonEmoComm        27        145     0.186 
# 7 negative              positive              emotive           11         97     0.113 
# 8 negative              positive              emoComm            9         25     0.36  
# 9 negative              positive              evidential         7         64     0.109 
# 10 negative              positive              cognitive          6         46     0.130 

# 11 positive              negative              nonEmoComm         8        145     0.0552
# 12 positive              negative              evidential         3         64     0.0469
# 13 positive              negative              cognitive          2         46     0.0435
# 14 positive              negative              emotive            2         97     0.0206
# 15 positive              negative              emoComm            1         25     0.04  

# 16 positive              positive              nonEmoComm        80        145     0.552 
# 17 positive              positive              evidential        46         64     0.719 
# 18 positive              positive              emotive           32         97     0.330 
# 19 positive              positive              cognitive         30         46     0.652 
# 20 positive              positive              emoComm            3         25     0.12  

# frequency of valence and dominance direction groups for predicate types
new.scale %>% 
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(VDgroup_size = sum(n), .by = predicateType2) %>% 
  mutate(proportion = n / VDgroup_size) %>% 
  arrange(predicateType2, desc(n))
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction     n VDgroup_size proportion
#   <chr>          <chr>                 <chr>                 <int>        <int>      <dbl>
# 1 cognitive      positive              positive                 30           46     0.652 - amongst the cognitives, there are 30 pos-pos predicates, which is 65.2% of this predicate type.
# 2 cognitive      negative              negative                  8           46     0.174 
# 3 cognitive      negative              positive                  6           46     0.130 
# 4 cognitive      positive              negative                  2           46     0.0435

# 5 emoComm        negative              negative                 12           25     0.48  
# 6 emoComm        negative              positive                  9           25     0.36  
# 7 emoComm        positive              positive                  3           25     0.12  
# 8 emoComm        positive              negative                  1           25     0.04  

# 9 emotive        negative              negative                 52           97     0.536 
# 10 emotive        positive              positive                 32           97     0.330 
# 11 emotive        negative              positive                 11           97     0.113 
# 12 emotive        positive              negative                  2           97     0.0206

# 13 evidential     positive              positive                 46           64     0.719 
# 14 evidential     negative              negative                  8           64     0.125 
# 15 evidential     negative              positive                  7           64     0.109 
# 16 evidential     positive              negative                  3           64     0.0469

# 17 nonEmoComm     positive              positive                 80          145     0.552 
# 18 nonEmoComm     negative              negative                 30          145     0.207 
# 19 nonEmoComm     negative              positive                 27          145     0.186 
# 20 nonEmoComm     positive              negative                  8          145     0.0552 

# V Volition ----
## V.1 all predicates ----
### VY.1.1 by predicate ----
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
ggsave("../graphs/projection-by-volition.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-volition-and-predicateType2.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.28114    0.01890  14.875   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.22678    0.02421   9.368   <2e-16 ***

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol <- d.proj.acc %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
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
ggsave("../graphs/projection-by-predicateType-and-volition.pdf", height = 4, width = 10)

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
ggsave("../graphs/projection-by-volition-comm.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.27953    0.01300  21.496  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.14101    0.03383   4.169 4.55e-05 ***


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
ggsave("../graphs/projection-by-veridicality2.pdf", height = 4, width = 6)

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
ggsave("../graphs/projection-by-veridicality2-faceted.pdf", height = 3, width = 11.5)

## linear models ----
lm(Mean.Proj ~ Mean.Verid, data = mean.proj.verid) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.10160    0.01717   5.916 6.34e-09 ***
# Mean.Verid   0.52522    0.02401  21.874  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1989 on 472 degrees of freedom
# Multiple R-squared:  0.5034,	Adjusted R-squared:  0.5023 
# F-statistic: 478.5 on 1 and 472 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ Mean.Verid * fct_relevel(predicateType2, "nonEmoComm"), 
   mean.proj.verid) %>% 
  summary()
# predicate type  significance of Mean.Verid
# communicative   ***
# predicate type2 significance of Mean.Verid
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

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
ggsave("../graphs/projection-by-communicative.pdf", height = 4, width = 13)

## with highly projective predicates ---- 
# communicative predicates that project as much or more strongly than 'know'
mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType)
# verb_renamed Mean.Proj commType                sayVerbType        
#   <fct>            <dbl> <chr>                   <chr>              
# 1 cry              0.833 pure                    mode verb          
# 2 fess up          0.733 discourse participation NA                 
# 3 apologize        0.724 discourse participation discourse role verb
# 4 whine            0.7   pure                    mode verb          
# 5 disclose         0.667 discourse participation NA                 
# 6 pout             0.667 pure                    NA                 
# 7 bitch            0.633 discourse participation mode verb          
# 8 document         0.633 pure                    mode verb          
# 9 weep             0.633 pure                    mode verb   

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
ggsave("../graphs/projection-by-communicative-highest.pdf", height = 4, width = 13)


## with extreme predicates ----
# calculate difference in mean projection rating between the predicate with the 
# highest rating and 'know':
slice_max(mean.proj.comm, Mean.Proj, with_ties = FALSE)$Mean.Proj - 
  subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj
# 0.2

### lists ----
# communicative predicates whose projection rating ranges between that of 'think'
# and think + 0.2:
aaa <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.2 & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj) %>% 
  arrange(Mean.Proj) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)
# aaa %>% write.csv("../data/barely-projective-communicatives.csv")
# 1 charge         -0.0333 discourse participation NA                 
# 2 allege          0      discourse participation discourse role verb
# 3 demand          0.0333 discourse participation discourse role verb
# 4 fake            0.0333 state changing          NA                 
# 5 retract         0.0333 discourse participation NA                 
# 6 contend         0.0667 discourse participation discourse role verb
# 7 decree          0.0667 discourse participation discourse role verb
# 8 fax             0.0667 pure                    mode verb          
# 9 hint            0.0667 discourse participation NA                 
# 10 imply           0.0667 discourse participation NA                 
# 11 jest            0.0667 pure                    mode verb          
# 12 propose         0.0667 discourse participation discourse role verb
# 13 reject          0.0667 discourse participation NA                 
# 14 forecast        0.0690 pure                    NA                 
# 15 dispel          0.1    discourse participation NA                 
# 16 holler          0.1    pure                    mode verb          
# 17 joke            0.1    pure                    mode verb          
# 18 posit           0.1    discourse participation discourse role verb
# 19 promise         0.1    state changing          discourse role verb
# 20 type            0.1    pure                    mode verb          
# 21 wager           0.1    discourse participation NA                 
# 22 depict          0.133  pure                    NA                 
# 23 narrate         0.133  pure                    NA                 
# 24 proclaim        0.133  discourse participation discourse role verb
# 25 prophesy        0.133  pure                    NA                 
# 26 quarrel         0.133  discourse participation NA                 
# 27 recap           0.133  discourse participation discourse role verb
# 28 request         0.133  discourse participation NA                 
# 29 rule            0.133  discourse participation discourse role verb
# 30 demonstrate     0.138  state changing          NA                 
# 31 challenge       0.167  discourse participation NA                 
# 32 corroborate     0.167  discourse participation NA                 
# 33 debate          0.167  discourse participation NA                 
# 34 establish       0.167  discourse participation discourse role verb
# 35 express         0.167  pure                    NA                 
# 36 insinuate       0.167  discourse participation NA                 
# 37 insist          0.167  state changing          discourse role verb
# 38 lie             0.167  state changing          discourse role verb
# 39 mark            0.167  pure                    discourse role verb
# 40 post            0.167  pure                    mode verb          
# 41 reassert        0.167  discourse participation discourse role verb
# 42 submit          0.167  discourse participation NA                 
# 43 tease           0.167  discourse participation mode verb   

# communicative predicates with similar projection ratings as 'know': 
# (know - 0.2) to max [= know + 0.2] 
bbb <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)
# bbb %>% write.csv("../data/highly-projective-communicatives.csv")
# 1 cry              0.833 pure                    mode verb          
# 2 fess up          0.733 discourse participation NA                 
# 3 apologize        0.724 discourse participation discourse role verb
# 4 whine            0.7   pure                    mode verb          
# 5 disclose         0.667 discourse participation NA                 
# 6 pout             0.667 pure                    NA                 
# 7 bitch            0.633 discourse participation mode verb          
# 8 document         0.633 pure                    mode verb          
# 9 weep             0.633 pure                    mode verb          
# 10 complain         0.6   discourse participation mode verb          
# 11 explain          0.6   discourse participation discourse role verb
# 12 stress           0.6   discourse participation NA                 
# 13 publicize        0.586 pure                    NA                 
# 14 flaunt           0.567 discourse participation NA                 
# 15 fuss             0.567 discourse participation NA                 
# 16 leak             0.567 discourse participation NA                 
# 17 point out        0.567 discourse participation discourse role verb
# 18 reveal           0.567 discourse participation NA                 
# 19 share            0.567 discourse participation NA                 
# 20 warn             0.552 state changing          NA                 
# 21 announce         0.533 discourse participation discourse role verb
# 22 cheer            0.533 pure                    mode verb          
# 23 expose           0.533 state changing          NA                 
# 24 log              0.533 pure                    mode verb          
# 25 reiterate        0.533 discourse participation discourse role verb
# 26 snitch           0.533 discourse participation NA                 
# 27 divulge          0.5   discourse participation NA                 
# 28 emphasize        0.5   pure                    NA                 
# 29 gab              0.5   pure                    NA                 
# 30 grumble          0.5   pure                    mode verb          
# 31 address          0.467 discourse participation NA                 
# 32 gasp             0.467 pure                    mode verb          
# 33 gush             0.467 pure                    mode verb          
# 34 moan             0.467 pure                    mode verb          
# 35 shriek           0.467 pure                    mode verb          
# 36 sob              0.467 pure                    mode verb          
# 37 whimper          0.467 pure                    mode verb          
# 38 admit            0.433 discourse participation discourse role verb
# 39 broadcast        0.433 pure                    mode verb          
# 40 detail           0.433 discourse participation NA                 
# 41 murmur           0.433 pure                    mode verb          
# 42 spout            0.433 pure                    NA                 
# 43 stammer          0.433 pure                    mode verb          
# 44 voice            0.433 pure                    NA 

### plots ----
# with labels for extreme predicates
#### 41-9 ----
# 0 to (think + 0.2) and know to (know + 0.2)
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.2 & 
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
                                                     Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.2 & 
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
ggsave("../graphs/projection-by-communicative-41-9.pdf", height = 4, width = 13)

#### 12-9 ----
# with labels for 12/9 extreme predicates on both ends
# There are 9 predicates that project as much or more strongly than 'know'
# Of the 12 least projective predicates from 0, predicates 5-12 have a mean 
# projection rating of 0.6770. Predicate 13 has a rating 0.690. Therefore, on 
# the lower end of the projection scale 12 predicates are included.

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
ggsave("../graphs/projection-by-communicative-extremes-12-9.pdf", height = 4, width = 13)

#### 28-26 ----
# some more predicates - how many?
mean.proj.acc %>% 
  filter(predicateType == "communicative" & 
           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.1) %>% 
  nrow() # 26
# Of the predicates with the lowest projection ratings from 0, no. 28 has a 
# mean projection rating of 0.133. The 29th predicate has a rating of 0.167. 
# Therefore, the number of predicates considered on the lower end is 28.

# with labels for 28/26 extreme predicates on the ends
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
ggsave("../graphs/projection-by-communicative-extremes-28-26.pdf", height = 4, width = 13)


#### 41-44 ----
# even more predicates - how many?
mean.proj.acc %>% 
  filter(predicateType == "communicative" & 
           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  nrow() # 44
# Of the predicates with the lowest projection ratings from 0, the 41st has a 
# mean projection rating of 0.167. The 42nd predicate has a rating of 0.2. 
# Therefore, the number of predicates considered on the lower end is 41.

# with labels for 41/44 extreme predicates on the ends
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
ggsave("../graphs/projection-by-communicative-extremes-41-44.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-emoComms.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoComms.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude.pdf", height = 4, width = 13)


## acceptability > 5 ----
# the following 15 emoComms are considered:
zzz <- mean.proj.acc %>% filter(predicateType2 == "emoComm" & Mean.Acc > 5) %>% 
  select(verb_renamed) %>% droplevels() %>% unlist() %>% as.vector() %>% print()
# [1] "bitch"    "boast"    "brag"     "cheer"    "complain" "cry"      "exclaim"  
# "fuss"     "gloat"    "mutter"   "pout"    
# [12] "rant"     "rave"     "scream"   "whine"

# the following 12 predicates with ratings of up to 5 are excluded:
www <- mean.proj.acc %>% filter(predicateType2 == "emoComm") %>% select(verb_renamed) %>% 
  droplevels() %>% unlist() %>% as.vector() 
setdiff(www, zzz)
# [1] "groan"   "grumble" "grunt"   "howl"    "moan"    "quarrel" "shriek"  "sigh"    
# "sob"     "squeal"  "weep"    "whimper"

mean.proj.comm %>% filter(verb_renamed %in% www) %>% group_by(emoCommType) %>% count()
#   emoCommType     n
#   <chr>       <int>
# 1 attitude       11
# 2 manner         16

mean.proj.comm %>% filter(verb_renamed %in% zzz) %>% group_by(emoCommType) %>% count()
# emoCommType     n
# <chr>       <int>
# 1 attitude       10
# 2 manner          5

### 34-32 ----
ggplot(mean.proj.acc %>% 
         filter((predicateType == "communicative" | verb_renamed %in% c("think", "know")) & Mean.Acc > 5), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0 & Mean.Acc > 5), 
                              Mean.Proj, n = 32), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "barely projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2 &
                                               Mean.Acc > 5), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "highly projective", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed),
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" &
                                                               Mean.Proj >= 0  & Mean.Acc > 5),
                                    Mean.Proj, n = 32),
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
                                                     Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2 & 
                                                     Mean.Acc > 5),
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
ggsave("../graphs/projection-by-communicative-extremes-34-32-acc5.pdf", height = 4, width = 13)


### all emoComms ----
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know") & Mean.Acc > 5), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say" & Mean.Acc > 5), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think" & Mean.Acc > 5), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType2 == "emoComm" & Mean.Acc > 5), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComms", alpha = 0.5)) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know" & Mean.Acc > 5), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say" & Mean.Acc > 5),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think" & Mean.Acc > 5),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(predicateType2 == "emoComm" & Mean.Acc > 5),
                   aes(label = verb_renamed), 
                   alpha = 0.7,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know" & Mean.Acc > 5),
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
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "emoComms", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "green4", "orangered3"))
ggsave("../graphs/projection-by-communicative-emoComms-acc5.pdf", height = 4, width = 13)


# ZZ combined models ----
## predicate type ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.all) %>% 
  summary()
nrow(mean.proj.all) # 544
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.42271    0.03375  12.526  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.21334    0.04299  -4.963 9.34e-07 ***
# fct_relevel(predicateType2, "emoComm")comPriv    -0.18823    0.06535  -2.880  0.00413 ** 
# fct_relevel(predicateType2, "emoComm")emotive     0.29835    0.03732   7.994 8.03e-15 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.13028    0.03976  -3.277  0.00112 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14534    0.03665  -3.966 8.31e-05 ***
# fct_relevel(predicateType2, "emoComm")other       0.02909    0.04934   0.590  0.55570    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1939 on 537 degrees of freedom
# Multiple R-squared:  0.5134,	Adjusted R-squared:  0.5079 
# F-statistic: 94.42 on 6 and 537 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj) %>% 
  summary()
nrow(mean.proj) # 503
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.42271    0.03327  12.707  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.21334    0.04238  -5.035 6.70e-07 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.29835    0.03679   8.110 3.97e-15 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.13028    0.03920  -3.324 0.000953 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14534    0.03613  -4.023 6.64e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1911 on 498 degrees of freedom
# Multiple R-squared:  0.5343,	Adjusted R-squared:  0.5306 
# F-statistic: 142.8 on 4 and 498 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.acc) %>% 
  summary()
nrow(mean.proj.acc) # 474
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43457    0.03734  11.637  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.21925    0.04603  -4.763 2.54e-06 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.28876    0.04069   7.096 4.77e-12 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.13558    0.04340  -3.124 0.001894 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15395    0.04014  -3.836 0.000142 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.194 on 469 degrees of freedom
# Multiple R-squared:  0.5304,	Adjusted R-squared:  0.5264 
# F-statistic: 132.4 on 4 and 469 DF,  p-value: < 2.2e-16

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.comm) %>% 
  summary()
nrow(mean.proj.comm) # 201
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43457    0.03309  13.134  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15395    0.03556  -4.329 2.37e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1719 on 199 degrees of freedom
# Multiple R-squared:  0.08607,	Adjusted R-squared:  0.08148 <<<
# F-statistic: 18.74 on 1 and 199 DF,  p-value: 2.37e-05

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
# (Intercept)                                       0.44133    0.03919  11.261  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.22217    0.04869  -4.563 6.86e-06 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.28995    0.04395   6.597 1.45e-10 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.16211    0.04622  -3.508 0.000507 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16128    0.04244  -3.800 0.000169 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.196 on 372 degrees of freedom
# Multiple R-squared:  0.5157,	Adjusted R-squared:  0.5105 
# F-statistic: 99.04 on 4 and 372 DF,  p-value: < 2.2e-16

### Valence ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.44787    0.04468  10.025  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.22280    0.04879  -4.566 6.77e-06 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.29160    0.04433   6.577 1.63e-10 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.16454    0.04695  -3.505 0.000513 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16412    0.04349  -3.774 0.000187 ***
# V.Mean.Sum2                                      -0.01846    0.06033  -0.306 0.759719    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1962 on 371 degrees of freedom
# Multiple R-squared:  0.5158,	Adjusted R-squared:  0.5093 
# F-statistic: 79.06 on 5 and 371 DF,  p-value: < 2.2e-16

# Valence adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.28833    0.09387   3.072  0.00229 ** 
# fct_relevel(predicateType2, "emoComm")cognitive              -0.07268    0.11026  -0.659  0.51020    
# fct_relevel(predicateType2, "emoComm")emotive                 0.43669    0.10574   4.130  4.5e-05 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.02257    0.10454   0.216  0.82918    
# fct_relevel(predicateType2, "emoComm")nonEmoComm              0.01463    0.09771   0.150  0.88103    
# V.Mean.Sum2                                                   0.43209    0.24089   1.794  0.07368 .  
# fct_relevel(predicateType2, "emoComm")cognitive:V.Mean.Sum2  -0.42110    0.28734  -1.465  0.14364    
# fct_relevel(predicateType2, "emoComm")emotive:V.Mean.Sum2    -0.41796    0.26089  -1.602  0.11000    
# fct_relevel(predicateType2, "emoComm")evidential:V.Mean.Sum2 -0.57445    0.29777  -1.929  0.05448 .  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:V.Mean.Sum2 -0.54640    0.26412  -2.069  0.03927 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.196 on 367 degrees of freedom
# Multiple R-squared:  0.5223,	Adjusted R-squared:  0.5105 
# F-statistic: 44.58 on 9 and 367 DF,  p-value: < 2.2e-16

# V.Mean.Sum2 is not significant for emoComm and nonEmoComm.
# The effect of V.Mean.Sum2 on Mean.Proj is significantly lower for nonEmoComm than for emoComm 
# (and vice versa, obviously).

lm(Mean.Proj ~ 
     fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# no significant interactions for emoComm and nonEmoComm.

### Arousal ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + A.Mean.Sum2, data = new.scale) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.47958    0.05929   8.089 8.66e-15 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.22987    0.04952  -4.642 4.80e-06 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.29245    0.04406   6.637 1.14e-10 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.17000    0.04713  -3.607 0.000352 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16748    0.04306  -3.890 0.000119 ***
# A.Mean.Sum2                                      -0.08233    0.09574  -0.860 0.390383    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.196 on 371 degrees of freedom
# Multiple R-squared:  0.5167,	Adjusted R-squared:  0.5102 
# F-statistic: 79.32 on 5 and 371 DF,  p-value: < 2.2e-16

# Arousal adds nothing to the model.

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * A.Mean.Sum2, data = new.scale) %>%
  summary()
# A.Mean.Sum2 is not significant for emoComm and nonEmoComm.
# No significant interactions for emoComm and nonEmoComm.

### Dominance ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + D.Mean.Sum2, data = new.scale) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43489    0.04157  10.461  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.22442    0.04898  -4.582 6.30e-06 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.28812    0.04417   6.523 2.26e-10 ***
# fct_relevel(predicateType2, "emoComm")evidential -0.16508    0.04670  -3.535 0.000459 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16177    0.04249  -3.807 0.000165 ***
# D.Mean.Sum2                                       0.03366    0.07181   0.469 0.639549    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1962 on 371 degrees of freedom
# Multiple R-squared:  0.516,	Adjusted R-squared:  0.5095 
# F-statistic: 79.11 on 5 and 371 DF,  p-value: < 2.2e-16

#### Interactions? ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "nonEmoComm") * D.Mean.Sum2, data = new.scale) %>%
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.32729    0.06423   5.096 5.57e-07 ***
# fct_relevel(predicateType2, "emoComm")cognitive              -0.08627    0.08454  -1.021   0.3082    
# fct_relevel(predicateType2, "emoComm")emotive                 0.44033    0.07440   5.919 7.45e-09 ***
# fct_relevel(predicateType2, "emoComm")evidential             -0.09511    0.08388  -1.134   0.2576    
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.06098    0.07155  -0.852   0.3946    
# D.Mean.Sum2                                                   0.59582    0.26643   2.236   0.0259 *  
# fct_relevel(predicateType2, "emoComm")cognitive:D.Mean.Sum2  -0.68044    0.32228  -2.111   0.0354 *  
# fct_relevel(predicateType2, "emoComm")emotive:D.Mean.Sum2    -0.74371    0.29634  -2.510   0.0125 *  
# fct_relevel(predicateType2, "emoComm")evidential:D.Mean.Sum2 -0.42766    0.31716  -1.348   0.1784    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:D.Mean.Sum2 -0.52914    0.29699  -1.782   0.0756 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1952 on 367 degrees of freedom
# Multiple R-squared:  0.5257,	Adjusted R-squared:  0.5141 
# F-statistic:  45.2 on 9 and 367 DF,  p-value: < 2.2e-16

# For emoComm, D.Mean.Sum2 is significant. Its effect on Mean.Proj is significantly lower for the 
# cognitives and emotives.
# For nonEmoComms, D.Mean.Sum2 is not significant and there are no significant interactions.

# Dominance is the only VAD factor that affects Mean.Proj irrespective of predicate type. It therefore
# seems to be the factor that is least correlated with predicateType2. 

### Collinearity ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35410    0.03415  10.368  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.04054    0.04227  -0.959 0.338198    
# fct_relevel(predicateType2, "emoComm")emotive     0.08922    0.03830   2.329 0.020355 *  
# fct_relevel(predicateType2, "emoComm")evidential -0.13623    0.04002  -3.404 0.000735 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15068    0.03686  -4.088  5.3e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1708 on 383 degrees of freedom
# Multiple R-squared:  0.2586,	Adjusted R-squared:  0.2509 <<< 
# F-statistic: 33.41 on 4 and 383 DF,  p-value: < 2.2e-16

# The fact that the model with valence as the dependent variable has the highest R-squared and 
# F-statistic indicates that the (possible) correlation between VAD and predicateType2 is highest 
# for valence.

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.46455    0.02118  21.933  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.09436    0.02622  -3.600 0.000361 ***
# fct_relevel(predicateType2, "emoComm")emotive     0.03033    0.02375   1.277 0.202364    
# fct_relevel(predicateType2, "emoComm")evidential -0.09755    0.02482  -3.931 0.000101 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.07842    0.02286  -3.431 0.000667 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1059 on 383 degrees of freedom
# Multiple R-squared:  0.1971,	Adjusted R-squared:  0.1887 
# F-statistic: 23.51 on 4 and 383 DF,  p-value: < 2.2e-16

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.19140    0.02850   6.716 6.75e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.06344    0.03527   1.799   0.0729 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.05435    0.03196   1.701   0.0898 .  
# fct_relevel(predicateType2, "emoComm")evidential  0.07718    0.03339   2.311   0.0213 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.01416    0.03075   0.460   0.6455    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1425 on 383 degrees of freedom
# Multiple R-squared:  0.03475,	Adjusted R-squared:  0.02467 
# F-statistic: 3.447 on 4 and 383 DF,  p-value: 0.008764

## SAY ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43457    0.03309  13.134  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15395    0.03556  -4.329 2.37e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1719 on 199 degrees of freedom
# Multiple R-squared:  0.08607,	Adjusted R-squared:  0.08148 
# F-statistic: 18.74 on 1 and 199 DF,  p-value: 2.37e-05

### say verb ----
lm(Mean.Proj ~ sayVerb , data = mean.proj.comm) %>%
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.29520    0.01884  15.666   <2e-16 ***
# sayVerbyes   0.01114    0.02547   0.437    0.662

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerb , data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.44748    0.03946  11.341  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15928    0.03670  -4.340 2.27e-05 ***
# sayVerbyes                                       -0.01516    0.02514  -0.603    0.547    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1722 on 198 degrees of freedom
# Multiple R-squared:  0.08775,	Adjusted R-squared:  0.07853 
# F-statistic: 9.522 on 2 and 198 DF,  p-value: 0.0001126

# sayVerb adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerb , data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                  0.441667   0.086317   5.117 7.35e-07 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm            -0.153201   0.088279  -1.735   0.0842 .  
# sayVerbyes                                                  -0.008333   0.093522  -0.089   0.9291    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbyes -0.007362   0.097116  -0.076   0.9396    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1726 on 197 degrees of freedom
# Multiple R-squared:  0.08777,	Adjusted R-squared:  0.07388 
# F-statistic: 6.318 on 3 and 197 DF,  p-value: 0.000411

### say verb type ----
lm(Mean.Proj ~ sayVerbType, data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.261045   0.022397  11.655  < 2e-16 ***
# sayVerbTypemode verb 0.092169   0.031821   2.896  0.00458 ** 
# sayVerbTypesay       0.005622   0.167607   0.034  0.97331    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1661 on 107 degrees of freedom
#   (91 observations deleted due to missingness)
# Multiple R-squared:  0.07317,	Adjusted R-squared:  0.05585 
# F-statistic: 4.224 on 2 and 107 DF,  p-value: 0.01716

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(sayVerb == "yes")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43333    0.03307  13.104  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.16056    0.03718  -4.318  3.5e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1586 on 108 degrees of freedom
# Multiple R-squared:  0.1472,	Adjusted R-squared:  0.1393 
# F-statistic: 18.65 on 1 and 108 DF,  p-value: 3.504e-05

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayVerbType, data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.400607   0.048865   8.198 6.09e-13 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.139562   0.043881  -3.180  0.00193 ** 
# sayVerbTypemode verb                              0.032726   0.035811   0.914  0.36287    
# sayVerbTypesay                                    0.005622   0.160894   0.035  0.97219    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1595 on 106 degrees of freedom
# (91 observations deleted due to missingness)
# Multiple R-squared:  0.1539,	Adjusted R-squared:   0.13 
# F-statistic: 6.427 on 3 and 106 DF,  p-value: 0.0004842

# say verb type adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients: (2 not defined because of singularities)
#                                                                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                            0.400607   0.048865   8.198 6.09e-13 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                      -0.139562   0.043881  -3.180  0.00193 ** 
# sayVerbTypemode verb                                                   0.032726   0.035811   0.914  0.36287    
# sayVerbTypesay                                                         0.005622   0.160894   0.035  0.97219    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypemode verb        NA         NA      NA       NA    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayVerbTypesay              NA         NA      NA       NA    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1595 on 106 degrees of freedom
# (91 observations deleted due to missingness)
# Multiple R-squared:  0.1539,	Adjusted R-squared:   0.13 
# F-statistic: 6.427 on 3 and 106 DF,  p-value: 0.0004842


### mode verb type ----
lm(Mean.Proj ~ modeVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   0.351388   0.026255  13.384   <2e-16 ***
# modeVerbTypesay-with-attitude 0.007587   0.053510   0.142    0.888    

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(sayVerbType == "mode verb")) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43333    0.03185   13.61  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.13956    0.04203   -3.32  0.00165 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1527 on 52 degrees of freedom
# Multiple R-squared:  0.1749,	Adjusted R-squared:  0.1591 
# F-statistic: 11.02 on 1 and 52 DF,  p-value: 0.001649

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + modeVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.43739    0.03549  12.326  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14104    0.04276  -3.298  0.00178 ** 
# modeVerbTypesay-with-attitude                    -0.01332    0.04946  -0.269  0.78882    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1541 on 51 degrees of freedom
# (147 observations deleted due to missingness)
# Multiple R-squared:  0.1761,	Adjusted R-squared:  0.1438 
# F-statistic:  5.45 on 2 and 51 DF,  p-value: 0.007159

# mode verb type adds nothing to the model.
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * modeVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                     0.42292    0.03856  10.969 6.53e-15 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.11731    0.04937  -2.376   0.0214 *  
# modeVerbTypesay-with-attitude                                                   0.03423    0.06989   0.490   0.6265    
# fct_relevel(predicateType2, "emoComm")nonEmoComm:modeVerbTypesay-with-attitude -0.09539    0.09899  -0.964   0.3399    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1542 on 50 degrees of freedom
# (147 observations deleted due to missingness)
# Multiple R-squared:  0.1911,	Adjusted R-squared:  0.1426 
# F-statistic: 3.938 on 3 and 50 DF,  p-value: 0.01341

### say-by-means verb type ----
lm(Mean.Proj ~ sayByMeansVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.30714    0.04220   7.279 1.04e-08 ***
# sayByMeansVerbTypemanner  0.06893    0.05398   1.277    0.209    
# sayByMeansVerbTypesound   0.05952    0.08226   0.724    0.474    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1579 on 38 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.04243,	Adjusted R-squared:  -0.007966 
# F-statistic: 0.8419 on 2 and 38 DF,  p-value: 0.4388 

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.comm %>% filter(modeVerbType == "say-by-means")) %>%
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.42292    0.03702  11.425 5.16e-14 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.11731    0.04740  -2.475   0.0178 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1481 on 39 degrees of freedom
# Multiple R-squared:  0.1357,	Adjusted R-squared:  0.1135 
# F-statistic: 6.124 on 1 and 39 DF,  p-value: 0.01779

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") + sayByMeansVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients:
#                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.4329949  0.0728215   5.946 7.43e-07 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.1258520  0.0605428  -2.079   0.0446 *  
# sayByMeansVerbTypemanner                          0.0002816  0.0614016   0.005   0.9964    
# sayByMeansVerbTypesound                          -0.0411578  0.0925664  -0.445   0.6592    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1514 on 37 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.1426,	Adjusted R-squared:  0.07305 
# F-statistic: 2.051 on 3 and 37 DF,  p-value: 0.1236

# sayByMeansVerbType adds nothing to the model.

lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm") * sayByMeansVerbType, data = mean.proj.comm) %>%
  summary()
# Coefficients: (1 not defined because of singularities)
#                                                                             2Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                                                3.071e-01  1.749e-01   1.756   0.0876 .
# fct_relevel(predicateType2, "emoComm")nonEmoComm                           1.511e-16  1.701e-01   0.000   1.0000  
# sayByMeansVerbTypemanner                                                   1.345e-01  1.804e-01   0.746   0.4606  
# sayByMeansVerbTypesound                                                    5.952e-02  1.575e-01   0.378   0.7077  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayByMeansVerbTypemanner -1.443e-01  1.822e-01  -0.792   0.4335  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:sayByMeansVerbTypesound          NA         NA      NA       NA  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1522 on 36 degrees of freedom
# (160 observations deleted due to missingness)
# Multiple R-squared:  0.1573,	Adjusted R-squared:  0.06362 
# F-statistic: 1.679 on 4 and 36 DF,  p-value: 0.1761

# presentation plots ----
## by predicate ----
### legend plots ----
legend_plot <- ggplot(mean.proj.acc %>% 
                        filter(predicateType == "communicative"), 
                      aes(x = verb_renamed, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)

legend_plot2 <- 
  ggplot(mean.proj.acc %>% 
                        filter(predicateType == "communicative"), 
                      aes(x = verb_renamed, y = Mean.Proj, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Type of communicative") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("#009A00", "deepskyblue2"), limits = c("emoComm", "nonEmoComm"), 
                      labels = c("with emotion entailment", "without emotion entailment"))

### grey ----
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(44, 11, 11, 11), "pt"),
        aspect.ratio = 1/4.2) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("grey80", "blue", "deeppink", "orangered3"))
ggsave("../graphs/projection-by-communicative-grey.jpeg", height = 4, width = 13)


### grey all ----
ggplot(mean.proj.acc,  
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "predicate")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(44, 11, 11, 11), "pt"),
        aspect.ratio = 1/4.2) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("predicate", "say", "think", "know"),
                      values = c("grey80", "blue", "deeppink", "orangered3"))
ggsave("../graphs/projection-by-communicative-grey-all.jpeg", height = 4, width = 13)


### with highlights ----



# with random highlights
sample_predicates <- function(data) {

  data <- data %>% arrange(Mean.Proj)

  n <- nrow(data)

  bottom_indices <- seq_len(floor(n * 0.5))
  middle_indices <- (floor(n * 0.5) + 1):floor(n * 0.7)
  top_indices <- (floor(n * 0.7) + 1):n

  total_to_sample <- floor(n * 0.42)
  bottom_sample_size <- floor(total_to_sample * 0.57)
  middle_sample_size <- floor(total_to_sample * 0.28)
  top_sample_size <- total_to_sample - bottom_sample_size - middle_sample_size

  bottom_sample <- data %>%
    filter(row_number() %in% bottom_indices) %>%
    sample_n(bottom_sample_size)

  middle_sample <- data %>%
    filter(row_number() %in% middle_indices) %>%
    sample_n(middle_sample_size)

  top_sample <- data %>%
    filter(row_number() %in% top_indices) %>%
    sample_n(top_sample_size)

  selected_predicates <- bind_rows(bottom_sample, middle_sample, top_sample)

  return(selected_predicates$verb_renamed)
}

selected_predicates <- sample_predicates(mean.proj.comm)

highlights <- 
  ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed %in% sample_predicates(mean.proj.comm)),
             aes(x = verb_renamed, y = Mean.Proj, colour = "highlight")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"),
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed),
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   size = 5,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1/4.2) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "highlight", "say", "think", "know"),
                      values = c("grey80", "deepskyblue2", "blue", "deeppink", "orangered3"))

legend <- get_legend(legend_plot)
wrap_elements(legend) / highlights + 
  plot_layout( heights = c(0.1, 1))
#ggsave("../graphs/projection-by-communicative-grey-with-say.jpeg", height = 4, width = 13)

### all blue ----
allcomms <- ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"),
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed),
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   size = 5,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        aspect.ratio = 1/4.2) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", "orangered3"))

legend <- get_legend(legend_plot)
wrap_elements(legend) / allcomms + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative.jpeg", height = 4, width = 13)

### with emoComm points ----
emoCommPoints <- 
  ggplot(mean.proj.acc %>% 
                     filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
                   aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "nonEmoComm"), alpha = 0.8) +
  geom_point(data = mean.proj.acc %>% filter(predicateType2 == "emoComm"),
             aes(x = verb_renamed, y = Mean.Proj, colour = "emoComm")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"),
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "think"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "say"),
                   aes(label = verb_renamed),
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   size = 5,
                   colour = "blue") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.25,
                   size = 5,
                   colour = "orangered3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        aspect.ratio = 1/4.2) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("nonEmoComm", "emoComm", "say", "think", "know"),
                      values = c("deepskyblue2", "#009A00", "blue", "deeppink", "orangered3"))

legend2 <- get_legend(legend_plot2)
wrap_elements(legend2) / emoCommPoints + 
  plot_layout( heights = c(0.1, 1))
ggsave("../graphs/projection-by-communicative-emoComm-points.jpeg", height = 4, width = 13)

## by predicate type ----
d.proj.acc %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj)) %>% 
ggplot(aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        aspect.ratio = 1/2.6,
        plot.margin = unit(c(44, 5.5, 5.5, 5.5), "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType.jpeg", height = 4, width = 8)

# predicate type2
abc <- d.proj.acc %>%
  group_by(predicateType, predicateType2) %>%
  summarise(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = factor(predicateType, 
                                levels = c("cognitive", "evidential", "communicative", "emotive")),
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
abc %>% 
ggplot(aes(x = predicateType, 
                      y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = abc %>% filter(predicateType == "communicative"),
             aes(colour = predicateType2), position = position_dodge(.3)) +
  geom_point(data = abc %>% filter(predicateType != "communicative"),
             aes(colour = predicateType), show.legend = FALSE) +
  geom_errorbar(data = abc %>% filter(predicateType == "communicative"),
                aes(
                    ymin = YMin.Proj, ymax = YMax.Proj, colour = predicateType2), 
                width = 0, position = position_dodge(.3)) +
  geom_errorbar(data = abc %>% filter(predicateType != "communicative"),
                aes(
                    ymin = YMin.Proj, ymax = YMax.Proj, colour = predicateType), 
                width = 0, show.legend = FALSE) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(vjust = -.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        aspect.ratio = 1/2.6,
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean projection rating",
       colour = "Type of communicative") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_color_manual(values = cols2,
                     breaks = c("emoComm", "nonEmoComm"),
                     labels = c("with emotion entailment", 
                                "without emotion entailment")) +
  scale_x_discrete(drop = FALSE)
ggsave("../graphs/projection-by-predicateType2.jpeg", height = 4, width = 8)
