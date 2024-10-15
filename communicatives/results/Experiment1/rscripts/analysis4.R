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

# calculate by-predicate projection means for new data
mean.proj.e1 <- e1 %>%
  group_by(predicate) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.e1) # 192
# cat(as.character(mean.proj.e1$predicate), sep = "\n")

tmp <- e1 %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType, ratingsCount)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType, ratingsCount)
nrow(tmp) # 192

mean.proj.e1 <- left_join(mean.proj.e1, tmp, by = c("predicate")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.e1) # 192

## MV dataset ----
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
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV) # 192

tmp <- d.proj %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)
nrow(tmp) # 192

mean.proj.MV <- left_join(mean.proj.MV, tmp, by = c("predicate")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.MV) # 192

#### by embedding environment ----
# calculate by-predicate projection means for MV data
mean.proj.MV.env <- d.proj %>%
  group_by(predicate, environment) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV.env) # 576

tmp <- d.proj %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType, environment)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType, environment)
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
  summarize(count = n())
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
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(predicate), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV.neg) # 192

tmp <- d.proj %>%
  select(c(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)) %>%
  distinct(predicate, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)
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

table(e1.VAD$predicateType2)
#  cognitive    emoComm nonEmoComm 
#         53        469       2561 

# combine mean projection and valence/arousal/dominance ratings in one data frame
mean.proj.e1.plus.VAD <- w %>% 
  filter(Word %in% e1$predicate) %>%
  rename(predicate = Word) %>%
  select(predicate, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum) %>% 
  left_join(mean.proj.e1.plus, by = "predicate")
nrow(mean.proj.e1.plus.VAD) # 171

table(mean.proj.e1.plus.VAD$predicateType2)
# cognitive    emoComm nonEmoComm 
#         2         25        144 

### rescale V + A + D ratings
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings for valence and arousal are rescaled to range from 0 to 1. 
e1.VAD <- e1.VAD %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))

mean.proj.e1.plus.VAD <- mean.proj.e1.plus.VAD %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))



# new data ----
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

## plots ----
### basic ----
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

### labels for extreme predicates ----
# how many highlighted predicates on either side?
mean.proj.e1 %>% 
  filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1) %>% 
  nrow() # 18 - without 'know', 17 predicates have projection ratings at least as high as 'know' minus 0.1.
mean.proj.e1 %>% 
  filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2) %>% 
  nrow() # 46 - without 'know', 45 predicates have projection ratings at least as high as 'know' minus 0.1.

#### 17-17 ----
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

#### 17pos-17 ----
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

#### 45-45 ----
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

#### 45pos-45 ----
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

### emotion entailment ----
#### plot ----
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

#### models ----
lm(rating ~ fct_relevel(predicateType2, "emoComm"), data = e1) %>% summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.58782    0.03105  18.930  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.25537    0.10040  -2.544    0.011 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.23435    0.03363  -6.969 3.81e-12 ***

lm(rating ~ predicateType2, data = e1 %>% filter(! predicateType2 == "cognitive")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.58782    0.03096  18.985  < 2e-16 ***
# predicateType2nonEmoComm -0.23435    0.03353  -6.989 3.31e-12 ***

lm(Mean.Proj ~ predicateType2, data = mean.proj.e1 %>% filter(! predicateType2 == "cognitive")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.58484    0.05538  10.561  < 2e-16 ***
# predicateType2nonEmoComm -0.22707    0.05979  -3.798 0.000197 ***


# comparison with MV dataset ----
## plots ----
### overall ----
#### basic ----
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


### under negation only ----
#### basic ----
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
# projNew is created above under comparison ... - plots - overall - basic
(projNew + labs(y = "Mean projection rating (under negation)")) / commProjMVneg + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-comparison-neg.pdf", height = 6, width = 13)

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


### differences ----
#### overall ----
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

##### plots ----
# with ratings count
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


#### negation only ----
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

##### plots ----
# with ratings count
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


## Spearman rank correlations ----
# new data - MV overall
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


# rating patterns ----
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

## plot ----
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
  

# high and low ratings ----
## 1SD comparison ----
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


## 52-49 comparison -----
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

## emo comparison ----
### plots ----
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


### type of communicative with emotion entailment manner - attitude ----
projNewEmoManAtt <- 
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(emoCommType == "manner"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoCommsManner")) +
  geom_point(data = mean.proj.e1 %>% filter(emoCommType == "attitude"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoCommsAttitude")) +
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
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.e1 %>% filter(emoCommType == "attitude"),
                   aes(label = predicate), 
                   alpha = 1,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.8,
                   colour = "darkolivegreen") +
  geom_label_repel(data = mean.proj.e1 %>% filter(emoCommType == "manner"),
                   aes(label = predicate), 
                   alpha = 1,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "springgreen4") +
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
  scale_colour_manual(limits = c("communicative predicate", "say", "think", 
                                 "emoCommsAttitude", "emoCommsManner", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", 
                                 "darkolivegreen", "springgreen4", "orangered3"))

projMVnegEmoManAtt <- 
ggplot(mean.proj.MV.neg, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.MV.neg %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.MV.neg %>% filter(emoCommType == "manner"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoCommsManner")) +
  geom_point(data = mean.proj.MV.neg %>% filter(emoCommType == "attitude"), 
             aes(x = predicate, y = Mean.Proj, colour = "emoCommsAttitude")) +
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
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(emoCommType == "attitude"),
                   aes(label = predicate), 
                   alpha = 1,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.8,
                   colour = "darkolivegreen") +
  geom_label_repel(data = mean.proj.MV.neg %>% filter(emoCommType == "manner"),
                   aes(label = predicate), 
                   alpha = 1,
                   min.segment.length = 0,
                   max.overlaps = 100, 
                   nudge_x = 0.2, nudge_y = -0.3,
                   colour = "springgreen4") +
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
  scale_colour_manual(limits = c("communicative predicate", "say", "think", 
                                 "emoCommsAttitude", "emoCommsManner", "know"),
                      values = c("deepskyblue2", "blue", "deeppink", 
                                 "darkolivegreen", "springgreen4", "orangered3"))

projNewEmoManAtt / projMVnegEmoManAtt + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude-comparison.pdf", height = 7, width = 13)


### models for MV negation only ----
lm(Mean.Proj ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.MV.neg) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.33704    0.04768   7.069 2.93e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.03704    0.18156  -0.204  0.83857    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14678    0.05148  -2.852  0.00484 ** 

clmm(as.factor(veridicality_num) ~ fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.neg) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(predicateType2, "emoComm")cognitive  -0.08697    0.50853  -0.171    0.864    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.66692    0.14746  -4.523  6.1e-06 ***


# ranking differences ----
## overall ----
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


### plot ----
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

## emoComms only ----
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

### plot ----
rankingDistEmo %>% 
  ggplot(aes(ranking_difference)) + 
  geom_density() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Ranking difference",
       y = "Density") 
ggsave("../graphs/ranking-difference-emo.pdf", height = 7, width = 7)


# closer inspection ----

# Select a set number of predicates such that they occur in the smallest possible subset of highest 
# ranking predicates both in the MV dataset (negation only) and the new data.

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

  if (length(common_predicates) >= 15) {
    break  
  }
}
mean.proj.e1.plus %>% 
  filter(predicate %in% common_predicates) %>% 
  arrange(ranking_difference) %>% 
  select(predicate, Mean.Proj, Mean.Proj.MV.neg, rankingNew, rankingMVneg, ranking_difference) %>% 
  print()
print(x)

# The 15 predicates that occur in the top 26 of both datasets (excluding 'know'):
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
# > print(x)
# [1] 26
# NOTE: In some cases, the number of predicates in the output is larger than the selected value.
# This is due to the large number of ties in the MV dataset (neg-only).



# acceptability ----
# overall 
ggplot(mean.proj.e1.plus, aes(x = Mean.Acc.MV, y = Mean.Proj)) +
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

lm(Mean.Proj ~ Mean.Acc.MV, data = mean.proj.e1.plus) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.49507    0.18945   2.613  0.00969 **
# Mean.Acc.MV -0.01994    0.03549  -0.562  0.57486   

# MV ratings under negation only
ggplot(mean.proj.e1.plus, aes(x = Mean.Acc.MV.neg, y = Mean.Proj)) +
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

lm(Mean.Proj ~ Mean.Acc.MV.neg, data = mean.proj.e1.plus) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept)      0.25946    0.14088   1.842   0.0671 .
# Mean.Acc.MV.neg  0.02666    0.02857   0.933   0.3520  


## C.3 say verbs (Grimshaw 2015) ----
### C.3.1 types of communicatives ----
#### distribution ----
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

#### plot ----
e1 %>%
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(rating), 
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
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") + 
  scale_x_discrete(labels = c("non-say verb", "say verb")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-communication-type-new.pdf", height = 4, width = 10)

#### linear model ----
lm(rating ~ sayVerb, data = e1) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.44026    0.01785  24.663  < 2e-16 ***
# sayVerbyes  -0.09535    0.02392  -3.987 6.84e-05 ***

lm(Mean.Proj ~ sayVerb, data = mean.proj.e1) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.45246    0.03223  14.041  < 2e-16 ***
# sayVerbyes  -0.11329    0.04317  -2.624  0.00938 ** 

### C.3.2 types of say verbs ----
#### distribution ----
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

#### plots ----
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  summarize(Mean.Proj = mean(rating), 
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
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Say verb type",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-sayverb-type-new.pdf", height = 4, width = 10)

e1 %>%
  filter(predicateType == "communicative") %>%  
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(rating), 
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
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Communicative type",
       y = "Mean projection rating",
       colour = "Say verb type") + 
  scale_x_discrete(labels = c("non-say verb", "say verb")) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("purple","deeppink", "orange3", "grey50"))
ggsave("../graphs/projection-by-sayverb-type2-new.pdf", height = 4, width = 10)


#### linear model ----
lm(rating ~ fct_relevel(sayVerbType, "discourse role verb"), data = e1) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   ***
# mode verb             ***
# say                   n.s.

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


### C.3.3 types of mode verbs ----
#### distribution ----
e1 %>% 
  filter(mode_verb == "yes") %>%   
  group_by(modeVerbType) %>% 
  distinct(predicate) %>% 
  count()
#   modeVerbType          n
# #   <chr>             <int>
# 1 say-by-means         41
# 2 say-with-attitude    13

#### plot ----
e1 %>%
  filter(predicateType == "communicative" & sayVerb == "yes") %>%  
  group_by(sayVerbType, modeVerbType) %>% 
  summarize(Mean.Proj = mean(rating), 
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
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Say verb type",
       y = "Mean projection rating",
       colour = "Mode verb type") + 
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-new.pdf", height = 4, width = 10)

#### linear model ----
lm(rating ~ modeVerbType, data = e1) %>% 
  summary()
# (Intercept)                    0.42715    0.02442  17.494  < 2e-16 ***
# modeVerbTypesay-with-attitude  0.17734    0.04792   3.701 0.000227 ***


### C.3.4 types of say-by-means verbs ----
#### distribution ----
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

#### plot ----
e1 %>%
  filter(sayVerbType == "mode verb" | predicate == "say") %>%  
  group_by(modeVerbType, sayByMeansVerbType) %>% 
  summarize(Mean.Proj = mean(rating), 
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
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Mode verb type",
       y = "Mean projection rating",
       colour = "Say-by-means verb type") + 
  scale_x_discrete(labels = c("say-by-means", "say-with-attitude", "say")) +
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2", "grey50"))
ggsave("../graphs/projection-by-saybymeansverb-type-new.pdf", height = 4, width = 10)

#### linear model ----
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

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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
  summarize(Mean.Proj = mean(rating), 
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
  scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("green3", "blue", "grey50")) + 
  scale_shape_manual(breaks = c("form", "manner", "sound", "NA"),
                     values = c(15, 19, 17, 18))  
ggsave("../graphs/projection-by-saybymeansverb-type2-new.pdf", height = 4, width = 10)

# say that not ----
## plots ----
e1 %>% 
  mutate(that_not = case_when(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                                               "dispute", "question") ~ "say that not", 
                              TRUE ~ "other")) %>% 
  filter(predicateType == "communicative") %>% 
  group_by(that_not) %>% 
  summarize(Mean.Proj = mean(rating), 
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
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank()) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") + 
#  scale_x_discrete(labels = c("other", "say that not")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-say-that-not.pdf", height = 4, width = 10)

# "say that not" predicates project significantly more than others in the new data as well as MV-neg-only
# data. In the overall MV data, no such pattern is observed. 

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

## correlations ----
mean.proj.e1.plus %>%
  filter(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                          "dispute", "question")) %>% 
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV, method = "spearman")) # -0.414
mean.proj.e1.plus %>%
  filter(predicate %in% c("challenge", "contest", "debate", "deny", "dismiss", 
                          "dispute", "question")) %>% 
  summarise(correlation = cor(Mean.Proj, Mean.Proj.MV.neg, method = "spearman")) # 0.655


# VAD ratings ----
## H2.1 valence ----
### by predicate ----
#### plot ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = reorder(predicate, V.Mean.Sum2), y = V.Mean.Sum2, colour = predicateType2)) +
  geom_point() +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate", 
       y = "Mean valence rating", 
       colour = "Predicate type") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-predicate2-new.pdf", height = 4, width = 13)

### by predicate type ----
#### plot ----
# calculate valence by predicateType means
mean.valence = mean.proj.e1.plus.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), 
            CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, 
         YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence

ggplot(mean.valence, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2-new.pdf", height = 8, width = 10)

#### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35410    0.03214  11.018  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.14828    0.03482  -4.259 3.42e-05 ***

# Both predicate types predict arousal ratings.

### with direction ----
# calculate valence by predicateType2 means and direction of valence
mean.valence2 = mean.proj.e1.plus.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), 
            CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, 
         YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

# valence by predicate type and direction of valence
#### plot ----
ggplot(mean.valence2, aes(x = predicateType2, y = Mean.Valence, 
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
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "orangered", "positive" = "seagreen3"))
ggsave("../graphs/valence-by-predicateType-and-direction-new.pdf", height = 8, width = 10)

#### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm") * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                                                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                                              0.38333    0.03480  11.015  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                                                        -0.17139    0.04101  -4.179 4.74e-05 ***
# fct_relevel(V.Mean.Sum2.direction, "negative")positive                                                  -0.18271    0.08700  -2.100   0.0372 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm:fct_relevel(V.Mean.Sum2.direction, "negative")positive  0.17290    0.09123   1.895   0.0598 . 

# Considering an interaction with negative valence, both predicate types predict valence ratings.
# With positive valence, only the nonEmoComm predicate type predicts valence ratings. Note that only
# 4 of 25 emoComms have positive valence.

#### distribution ----
mean.proj.e1.plus.VAD %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#   predicateType2 V.Mean.Sum2.direction  n
# 1      cognitive              positive  2
# 2        emoComm              negative 21 (84% of emoComms)
# 3        emoComm              positive  4
# 4     nonEmoComm              negative 54 (38% of nonEmoComms)
# 5     nonEmoComm              positive 90

## H2.2 arousal ----
### by predicate ----
#### plot ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = reorder(predicate, A.Mean.Sum2), y = A.Mean.Sum2, 
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
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-predicate2-new.pdf", height = 4, width = 13)

### by predicate type ----
#### plot ----
# calculate arousal by predicateType means
mean.arousal = mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), 
            CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Arousal))
mean.arousal

mean.arousal %>% 
  filter(predicateType2 != "cognitive") %>% 
ggplot(aes(x = predicateType2, y = Mean.Arousal, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2-new.pdf", height = 8, width = 10)

#### linear model ----
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.46455    0.02067  22.480  < 2e-16 ***
# predicateType2nonEmoComm -0.07885    0.02239  -3.522 0.000552 ***

# Both predicate types predict arousal ratings.

## H2.3 dominance ---- 
### by predicate ----
#### plot ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = reorder(predicate, D.Mean.Sum2), y = D.Mean.Sum2, colour = predicateType2)) +
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
ggsave("../graphs/valence-by-predicate2-new.pdf", height = 4, width = 13)

### by predicate type ----
#### plot ----
# calculate dominance by predicateType2 means
mean.dominance = mean.proj.e1.plus.VAD %>%
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), 
            CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
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
ggsave("../graphs/dominance-by-predicateType2-new.pdf", height = 8, width = 10)

#### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.19140    0.02535   7.550  2.7e-12 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.01992    0.02746   0.725    0.469  

# Neither predicate type predicts dominance ratings.

## H2.4 ratings correlated? ----
### valence - dominance ----
#### models ----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18264    0.02479   7.368 7.59e-12 ***
# D.Mean.Sum2  0.21651    0.10175   2.128   0.0348 * 

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      .

lm(D.Mean.Sum2 ~ V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18061    0.01622  11.134   <2e-16 ***
# V.Mean.Sum2  0.12192    0.05730   2.128   0.0348 *

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         .
# nonEmoComm      .

#### plots ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean dominance rating (controlled - in control)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-valence-new.pdf", height = 6, width = 8)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean dominance rating (controlled - in control)") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/dominance-by-valence-faceted-new.pdf", height = 6, width = 8)

mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (controlled - in control)",
       y = "Mean valence rating (unhappy - happy)", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-new.pdf", height = 6, width = 8)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (controlled - in control)",
       y = "Mean valence rating (unhappy - happy)") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/valence-by-dominance-faceted-new.pdf", height = 6, width = 8)


### arousal - dominance ----
##### models ----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.40045    0.01590  25.187   <2e-16 ***
# D.Mean.Sum2 -0.01480    0.06526  -0.227    0.821   

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm         .
# nonEmoComm      n.s.

lm(D.Mean.Sum2 ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.21664    0.03773   5.741 4.33e-08 ***
# A.Mean.Sum2 -0.02080    0.09172  -0.227    0.821 

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# emoComm         .
# nonEmoComm      n.s.

##### plots ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (controlled - in control)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/dominance-by-arousal-new.pdf", height = 6, width = 8)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)", 
       y = "Mean dominance rating (controlled - in control)") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/dominance-by-arousal-faceted-new.pdf", height = 6, width = 8)

mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (controlled - in control)",
       y = "Mean arousal rating (calm - excited)", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-dominance-new.pdf", height = 6, width = 8)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating (controlled - in control)",
       y = "Mean arousal rating (calm - excited)") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/arousal-by-dominance-faceted-new.pdf", height = 6, width = 8)

### valence - arousal ----
#### models ----
lm(V.Mean.Sum2 ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.02671    0.04764   0.561    0.576    
# A.Mean.Sum2  0.50593    0.11581   4.369 2.19e-05 ***

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      **

lm(A.Mean.Sum2 ~ V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.35120    0.01314  26.733  < 2e-16 ***
# V.Mean.Sum2  0.20271    0.04640   4.369 2.19e-05 ***

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      **

#### plots ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean arousal rating (calm - excited)",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/arousal-by-valence-new.pdf", height = 6, width = 8)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating (unhappy - happy)", 
       y = "Mean arousal rating (calm - excited)") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/arousal-by-valence-faceted-new.pdf", height = 6, width = 8)

mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)",
       y = "Mean valence rating (unhappy - happy)", 
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-arousal-new.pdf", height = 6, width = 8)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating (calm - excited)",
       y = "Mean valence rating (unhappy - happy)") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2) + 
  facet_wrap(~ predicateType2, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/valence-by-arousal-faceted-new.pdf", height = 6, width = 8)

## H2.5 directions ----
#### correlation ----
contingency_table <- table(mean.proj.e1.plus.VAD$V.Mean.Sum2.direction, mean.proj.e1.plus.VAD$D.Mean.Sum2.direction)
print(contingency_table)
#          negative positive
# negative       40       35
# positive       10       86

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 35.437, df = 1, p-value = 2.635e-09

# The direction of dominance and valence seem highly correlated.

### distributions ----
# distributions of direction within predicate types
mean.proj.e1.plus.VAD %>% 
  count(predicateType2, D.Mean.Sum2.direction)
#   predicateType2 D.Mean.Sum2.direction   n
# 1      cognitive              positive   2
# 2        emoComm              negative  13 (52%)
# 3        emoComm              positive  12
# 4     nonEmoComm              negative  37 (26%)
# 5     nonEmoComm              positive 107

mean.proj.e1.plus.VAD %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#    predicateType2 V.Mean.Sum2.direction n
# 1      cognitive              positive  2
# 2        emoComm              negative 21 (84%)
# 3        emoComm              positive  4
# 4     nonEmoComm              negative 54 (38%)
# 5     nonEmoComm              positive 90

# distribution of directions
mean.proj.e1.plus.VAD %>% group_by(V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% count()
#   V.Mean.Sum2.direction D.Mean.Sum2.direction     n
#   <chr>                 <chr>                 <int>
# 1 negative              negative                 40
# 2 negative              positive                 35
# 3 positive              negative                 10
# 4 positive              positive                 86

# distribution of directions by predicate types
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  group_by(predicateType2, V.Mean.Sum2.direction, D.Mean.Sum2.direction) %>% 
  distinct(predicate) %>% 
  count()
#   predicateType2 V.Mean.Sum2.direction D.Mean.Sum2.direction     n
#   <chr>          <chr>                 <chr>                 <int>
# 1 emoComm        negative              negative                 12 (57% of emoComms with neg. valence)
# 2 emoComm        negative              positive                  9
# 3 emoComm        positive              negative                  1 (25% of emoComms with pos. valence)
# 4 emoComm        positive              positive                  3
# 5 nonEmoComm     negative              negative                 28 (52% of nonEmoComms with neg. valence)
# 6 nonEmoComm     negative              positive                 26
# 7 nonEmoComm     positive              negative                  9 (10% of nonEmoComms with pos. valence)
# 8 nonEmoComm     positive              positive                 81 

# For both emoComms and nonEmoComms with negative valence, the numbers of predicates with negative 
# and positive dominance are roughly equal. Amongst emoComms and nonEmoComms with positive valence, 
# significantly more predicates have positive dominance.

## H2.6 projection: valence and arousal against projection ratings ----
### H2.6.1 valence ----
#### H2.6.1.1 overall ----
##### plots ----
# projection by valence
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-valence2-new.pdf", height = 3.5, width = 6)

# projection by valence faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-valence-faceted2-new.pdf", height = 4, width = 9)

##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.29544    0.03761   7.854 4.65e-13 ***
# V.Mean.Sum2  0.39919    0.13286   3.005  0.00307 ** 

lm(rating ~ V.Mean.Sum2, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.29590    0.02111  14.016  < 2e-16 ***
# V.Mean.Sum2  0.38663    0.07534   5.132 3.05e-07 ***

lm(Mean.Proj ~ V.Mean.Sum2 + fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.48799    0.07463   6.539 7.31e-10 ***
# V.Mean.Sum2                                       0.27228    0.13673   1.991  0.04809 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.19206    0.06477  -2.965  0.00347 ** 

lm(rating ~ V.Mean.Sum2 + fct_relevel(predicateType2, "emoComm"), 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.49387    0.04276  11.551  < 2e-16 ***
# V.Mean.Sum2                                       0.24432    0.07964   3.068  0.00217 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.19644    0.03695  -5.317 1.13e-07 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      n.s.

lm(rating ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         *
# nonEmoComm      *

# by predicate type (= the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.42185    0.06877   6.134 2.94e-06 ***
# V.Mean.Sum2  0.45909    0.17647   2.602    0.016 *  
lm(Mean.Proj ~ V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.30290    0.04102   7.384 1.19e-11 ***
# V.Mean.Sum2  0.23842    0.15764   1.512    0.133  

#### H2.6.1.2 with direction ----
# projection by valence with direction
##### plots ----
mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) 
ggsave("../graphs/projection-by-valence-with-direction-new.pdf", height = 3.5, width = 6)

# projection by valence faceted with emotive component
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-new.pdf", 
       height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.47849    0.03308  14.463  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.16564    0.04436  -3.734 0.000258 ***

lm(rating ~ V.Mean.Sum2.direction, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.47339    0.01886   25.10  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.16411    0.02536   -6.47 1.14e-10 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.34025    0.05430   6.266  3.1e-09 ***
# V.Mean.Sum2                                                         0.53182    0.16788   3.168  0.00183 ** 
# fct_relevel(V.Mean.Sum2.direction, "negative")positive             -0.03073    0.07416  -0.414  0.67909    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive -0.51535    0.26504  -1.944  0.05354 .   

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.30952    0.05050   6.129  6.3e-09 ***
# V.Mean.Sum2                                                         0.01646    0.20509   0.080   0.9361    
# fct_relevel(V.Mean.Sum2.direction, "positive")negative              0.03073    0.07416   0.414   0.6791    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative  0.51535    0.26504   1.944   0.0535 .  

lm(rating ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive"), 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.33720    0.03090  10.912  < 2e-16 ***
# V.Mean.Sum2                                                         0.53940    0.09724   5.547 3.15e-08 ***
# fct_relevel(V.Mean.Sum2.direction, "negative")positive             -0.02494    0.04278  -0.583 0.560012    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "negative")positive -0.55420    0.15480  -3.580 0.000349 ***

#                                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                         0.31226    0.02958  10.555  < 2e-16 ***
# V.Mean.Sum2                                                        -0.01480    0.12045  -0.123 0.902217    
# fct_relevel(V.Mean.Sum2.direction, "positive")negative              0.02494    0.04278   0.583 0.560012    
# V.Mean.Sum2:fct_relevel(V.Mean.Sum2.direction, "positive")negative  0.55420    0.15480   3.580 0.000349 ***

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ V.Mean.Sum2,
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.43266    0.09277   4.664 0.000169 ***
# V.Mean.Sum2  0.47039    0.22772   2.066 0.052778 .  

lm(Mean.Proj ~ V.Mean.Sum2,
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.33793    0.06482   5.213 3.25e-06 ***
# V.Mean.Sum2  0.41642    0.22809   1.826   0.0736 .  

lm(Mean.Proj ~ V.Mean.Sum2,
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.42609    0.07878   5.408   0.0325 *
# V.Mean.Sum2  0.04152    0.28254   0.147   0.8966  

lm(Mean.Proj ~ V.Mean.Sum2,
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.30447    0.05291   5.755 1.24e-07 ***
# V.Mean.Sum2  0.01470    0.21620   0.068    0.946 

### H2.6.2 arousal ----
# projection by arousal
#### H2.6.2.1 overall ----
##### plots ----
mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) 
ggsave("../graphs/projection-by-arousal2-new.pdf", height = 3.5, width = 6)

# projection by arousal faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-arousal-faceted2-new.pdf", height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.22591    0.08771   2.576   0.0109 *
# A.Mean.Sum2  0.40378    0.21321   1.894   0.0600 .

lm(rating ~ A.Mean.Sum2, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.21115    0.04822   4.379 1.23e-05 ***
# A.Mean.Sum2  0.43129    0.11699   3.687 0.000231 ***

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                                    0.7749     0.2439   3.176  0.00178 **
# A.Mean.Sum2                                                   -0.4100     0.5105  -0.803  0.42309   
# fct_relevel(predicateType2, "emoComm")nonEmoComm              -0.5571     0.2614  -2.131  0.03456 * 
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm   0.7580     0.5623   1.348  0.17950 
# 
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                                   0.21775    0.09398   2.317   0.0217 *
# A.Mean.Sum2                                                   0.34800    0.23570   1.476   0.1417  
# fct_relevel(predicateType2, "nonEmoComm")emoComm              0.55710    0.26141   2.131   0.0346 *
# A.Mean.Sum2:fct_relevel(predicateType2, "nonEmoComm")emoComm -0.75795    0.56227  -1.348   0.1795  

lm(rating ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                    0.7485     0.1367   5.476  4.7e-08 ***
# A.Mean.Sum2                                                   -0.3591     0.2847  -1.261 0.207290    
# fct_relevel(predicateType2, "emoComm")nonEmoComm              -0.5429     0.1467  -3.701 0.000218 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm   0.7245     0.3144   2.304 0.021269 *  

# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.20566    0.05318   3.867 0.000112 ***
# A.Mean.Sum2                                                   0.36544    0.13346   2.738 0.006213 ** 
# fct_relevel(predicateType2, "nonEmoComm")emoComm              0.54285    0.14667   3.701 0.000218 ***
# A.Mean.Sum2:fct_relevel(predicateType2, "nonEmoComm")emoComm -0.72451    0.31440  -2.304 0.021269 * 

# by predicate type (= the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.7749     0.1333   5.813 6.37e-06 ***
# A.Mean.Sum2  -0.4100     0.2790  -1.470    0.155 
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.21775    0.09917   2.196   0.0297 *
# A.Mean.Sum2  0.34800    0.24873   1.399   0.1640  

#### H2.6.2.2 with direction of valence ----
##### plots ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
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
  scale_colour_manual(values = cols2, labels = predicateType2_names) 
ggsave("../graphs/projection-by-arousal-with-direction-new.pdf", height = 3.5, width = 6)

# projection by arousal faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-new.pdf", 
       height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2 * V.Mean.Sum2.direction, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                0.33525    0.13591   2.467   0.0147 *
# A.Mean.Sum2                                0.32801    0.30183   1.087   0.2787  
# V.Mean.Sum2.directionpositive             -0.01689    0.18204  -0.093   0.9262  
# A.Mean.Sum2:V.Mean.Sum2.directionpositive -0.34308    0.44049  -0.779   0.4372 

lm(rating ~ A.Mean.Sum2 * V.Mean.Sum2.direction, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.32560    0.07431   4.382 1.22e-05 ***
# A.Mean.Sum2                                0.34240    0.16653   2.056   0.0399 *  
# V.Mean.Sum2.directionpositive             -0.06057    0.10054  -0.602   0.5469    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive -0.22288    0.24309  -0.917   0.3593  
  
lm(Mean.Proj ~ A.Mean.Sum2 * V.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()  
# n.s.

lm(rating ~ A.Mean.Sum * V.Mean.Sum2.direction * fct_relevel(predicateType2, "nonEmoComm"), 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()  
# Coefficients:
#                                                                                            Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                                                                 0.21170    0.08782   2.411  0.01598 * 
# A.Mean.Sum2                                                                                 0.48675    0.20033   2.430  0.01517 * 
# V.Mean.Sum2.directionpositive                                                               0.10113    0.11374   0.889  0.37401   
# fct_relevel(predicateType2, "nonEmoComm")emoComm                                            0.51195    0.16547   3.094  0.00199 **
# A.Mean.Sum2:V.Mean.Sum2.directionpositive                                                  -0.51617    0.27913  -1.849  0.06452 . 
# A.Mean.Sum2:fct_relevel(predicateType2, "nonEmoComm")emoComm                               -0.72786    0.36165  -2.013  0.04424 * 
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "nonEmoComm")emoComm             -1.37178    1.02925  -1.333  0.18270   
# A.Mean.Sum2:V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "nonEmoComm")emoComm  2.59063    1.93043   1.342  0.17970  

# by predicate type and direction of valence (= the faceted plot)
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.7543     0.1354   5.571 2.26e-05 ***
# A.Mean.Sum2  -0.3133     0.2907  -1.078    0.295   
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.1957     0.1822   1.074    0.288
# A.Mean.Sum2   0.5346     0.4102   1.303    0.198
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  -0.5390     0.3033  -1.777   0.2175  
# A.Mean.Sum2   1.8162     0.5643   3.218   0.0845 .
lm(Mean.Proj ~ A.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   0.3657     0.1268   2.883  0.00495 **
# A.Mean.Sum2  -0.1624     0.3440  -0.472  0.63801   

### H2.6.3 valence + arousal ----
#### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2 + V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()  
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.21646    0.08642   2.505   0.0132 *
# A.Mean.Sum2  0.22488    0.22154   1.015   0.3115  
# V.Mean.Sum2  0.35361    0.14023   2.522   0.0126 *

lm(rating ~ A.Mean.Sum2 + V.Mean.Sum2, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()  
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.20180    0.04814   4.192 2.85e-05 ***
# A.Mean.Sum2  0.26767    0.12308   2.175   0.0297 *  
# V.Mean.Sum2  0.33162    0.07943   4.175 3.06e-05 ***

lm(Mean.Proj ~ A.Mean.Sum2 * V.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()  
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)
# (Intercept)               0.2436     0.1557   1.565    0.120
# A.Mean.Sum2               0.1607     0.3784   0.425    0.672
# V.Mean.Sum2               0.2333     0.5910   0.395    0.694
# A.Mean.Sum2:V.Mean.Sum2   0.2682     1.2793   0.210    0.834

lm(rating ~ A.Mean.Sum2 * V.Mean.Sum2, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()  
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)              0.20908    0.08503   2.459    0.014 *
# A.Mean.Sum2              0.25059    0.20537   1.220    0.222  
# V.Mean.Sum2              0.29895    0.32443   0.921    0.357  
# A.Mean.Sum2:V.Mean.Sum2  0.07200    0.69327   0.104    0.917  


### H2.6.4 dominance ----
# projection by dominance
#### H2.6.4.1 overall ----
##### plots ----
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-dominance2-new.pdf", height = 3.5, width = 6)

# faceted
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
ggsave("../graphs/projection-by-dominance-faceted2-new.pdf", height = 4, width = 9)

##### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.43687    0.04405   9.918   <2e-16 ***
# D.Mean.Sum2 -0.24239    0.18080  -1.341    0.182 

lm(rating ~ D.Mean.Sum2, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.44015    0.02452  17.951  < 2e-16 ***
# D.Mean.Sum2 -0.27384    0.09997  -2.739  0.00619 ** 

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      n.s.

lm(rating ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         n.s.
# nonEmoComm      *

#### H2.6.4.2 with direction ----
# projection by dominance with direction
##### tables ----
# What does the "direction" of dominance mean?
mean.proj.e1.plus.VAD %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(predicate, D.Mean.Sum2, predicateType2)
#      predicate D.Mean.Sum2 predicateType2
# 1       detail      0.5350     nonEmoComm
# 2       signal      0.5125     nonEmoComm
# 3   articulate      0.4725     nonEmoComm
# 4          say      0.4700     nonEmoComm
# 5  communicate      0.4500     nonEmoComm
# 6          add      0.4475     nonEmoComm
# 7        reply      0.4475     nonEmoComm
# 8         sing      0.4175     nonEmoComm
# 9      express      0.3925     nonEmoComm
# 10      praise      0.3925     nonEmoComm

mean.proj.e1.plus.VAD %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(predicate, D.Mean.Sum2)
#    predicate D.Mean.Sum2
# 1        cry      0.6100
# 2        sob      0.4275
# 3     reject      0.3850
# 4       fuss      0.3750
# 5      bitch      0.3375
# 6     mumble      0.3300
# 7     stress      0.2875
# 8     gossip      0.2650
# 9   denounce      0.2500
# 10    shriek      0.2500

##### plots ----
###### with direction of valence: happy vs unhappy ----
mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
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
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-new.pdf", 
       height = 6, width = 10)

###### with "direction" of dominance ----
# controlled/submissive/guided/... vs in control/ autonomous/controlling/... 
# i.e., somebody else's vs one's own dominance
mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
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
mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2-new.pdf", 
       height = 6, width = 10)

##### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2.direction, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.50896    0.04063  12.526  < 2e-16 ***
# D.Mean.Sum2.directionpositive -0.17412    0.04842  -3.596 0.000425 ***

lm(rating ~ D.Mean.Sum2.direction, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.50528    0.02343  21.569  < 2e-16 ***
# D.Mean.Sum2.directionpositive -0.17272    0.02780  -6.212 5.94e-10 ***

lm(Mean.Proj ~ D.Mean.Sum2 * D.Mean.Sum2.direction, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.43925    0.06634   6.621 4.79e-10 ***
# D.Mean.Sum2                                0.43911    0.33142   1.325   0.1870    
# D.Mean.Sum2.directionpositive             -0.03373    0.08664  -0.389   0.6976    
# D.Mean.Sum2:D.Mean.Sum2.directionpositive -0.74748    0.39482  -1.893   0.0601 .   

lm(rating ~ D.Mean.Sum2 * D.Mean.Sum2.direction, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.44059    0.03827  11.513  < 2e-16 ***
# D.Mean.Sum2                                0.40623    0.19023   2.136 0.032797 *  
# D.Mean.Sum2.directionpositive             -0.02908    0.04980  -0.584 0.559275    
# D.Mean.Sum2:D.Mean.Sum2.directionpositive -0.74857    0.22599  -3.312 0.000936 ***

lm(Mean.Proj ~ D.Mean.Sum2 * V.Mean.Sum2.direction, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.47517    0.05872   8.092 1.21e-13 ***
# D.Mean.Sum2                                0.01924    0.28083   0.068    0.945    
# V.Mean.Sum2.directionpositive             -0.12588    0.08664  -1.453    0.148    
# D.Mean.Sum2:V.Mean.Sum2.directionpositive -0.17294    0.36795  -0.470    0.639   

lm(rating ~ D.Mean.Sum2 * V.Mean.Sum2.direction, 
   data = e1.VAD %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.48351    0.03321  14.558  < 2e-16 ***
# D.Mean.Sum2                               -0.05774    0.15591  -0.370  0.71113    
# V.Mean.Sum2.directionpositive             -0.13285    0.04939  -2.690  0.00718 ** 
# D.Mean.Sum2:V.Mean.Sum2.directionpositive -0.11623    0.20696  -0.562  0.57443  

# by predicate type and direction of valence (= the faceted plots)
# with direction of valence
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.63208    0.05570  11.349 6.61e-10 ***
# D.Mean.Sum2 -0.09883    0.22594  -0.437    0.667     
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.43008    0.08531   5.042 5.95e-06 ***
# D.Mean.Sum2 -0.02368    0.44293  -0.053    0.958  
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.44790    0.10909   4.106   0.0545 .
# D.Mean.Sum2 -0.07409    0.51888  -0.143   0.8995 
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.34043    0.06429   5.295 8.65e-07 ***
# D.Mean.Sum2 -0.13770    0.23787  -0.579    0.564 

# with direction of dominance
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & D.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.57840    0.07791   7.424 1.32e-05 ***
# D.Mean.Sum2  0.08516    0.28464   0.299     0.77   
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & D.Mean.Sum2.direction == "negative")) %>% 
  summary()
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.38631    0.08676   4.453 8.27e-05 ***
# D.Mean.Sum2  0.66067    0.52010   1.270    0.212  
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "emoComm" & D.Mean.Sum2.direction == "positive")) %>% 
  summary()
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.66549    0.07782   8.552 6.53e-06 ***
# D.Mean.Sum2 -0.56924    0.38953  -1.461    0.175 
lm(Mean.Proj ~ D.Mean.Sum2, 
   data = mean.proj.e1.plus.VAD %>% filter(predicateType2 == "nonEmoComm" & D.Mean.Sum2.direction == "positive")) %>% 
  summary()
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.3478     0.0625   5.565 2.02e-07 ***
# D.Mean.Sum2  -0.1669     0.2353  -0.709     0.48   

### H2.6.5 combined plots ----
#### new data ----
##### two lines ----
# combined plot of projection by VAD with two fitted lines
# valence
vplot <- mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names) 

# arousal
aplot <- mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)

# dominance
dplot <- mean.proj.e1.plus.VAD %>% 
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
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names) 

vplot + aplot + dplot + plot_layout(guides = "collect", axis_titles = "collect") # & theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-new.pdf", height = 3.5, width = 12)

##### faceted ----
vfac <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"), 
        aspect.ratio = 1) + 
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 

afac <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        aspect.ratio = 1/2) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 

dfac <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(D.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative dominance", 
                                                       "positive" = "positive dominance")), 
                                 .cols = predicateType2_names)) 
layout <- "
AABB
AABB
CCCC
"
vfac + dfac + afac + plot_layout(guides = "collect", axis_titles = "collect", design = layout) 
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-faceted-new.pdf", height = 8, width = 10)

###### by predicate type ----
vfac_emo <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType2 == "emoComm") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(),
        #plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"), 
        aspect.ratio = 1) + 
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ V.Mean.Sum2.direction, ncol = 2, labeller = as_labeller(c("negative" = "negative valence",
                                                                         "positive" = "positive valence")))


afac_emo <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType2 == "emoComm") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(), 
        #plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        aspect.ratio = 1/2) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2)

dfac_emo <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType2 == "emoComm") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(),
        #plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ D.Mean.Sum2.direction, ncol = 2, labeller = as_labeller(c("negative" = "negative dominance",
                                                                       "positive" = "positive dominance")))
layout <- "
AABBCC
"
VAD_emo <- vfac_emo + dfac_emo + afac_emo + 
  plot_layout(axis_titles = "collect", design = layout, widths = c(4.5, 4.5, 4.5)) + 
  plot_annotation(title = "Communicatives with emotion entailment")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-faceted-test.pdf", height = 4.5, width = 12.5)

vfac_nonEmo <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType2 == "nonEmoComm") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(),
        #plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"), 
        aspect.ratio = 1) + 
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ V.Mean.Sum2.direction, ncol = 2, labeller = as_labeller(c("negative" = "negative valence",
                                                                         "positive" = "positive valence")))


afac_nonEmo <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType2 == "nonEmoComm") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(), 
        #plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        aspect.ratio = 1/2) +
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2)

dfac_nonEmo <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType2 == "nonEmoComm") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        #axis.title.x = element_text(vjust = -1),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank(),
        #plot.margin = margin(0.5, 0.5, 1, 0.5, "cm"),
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ D.Mean.Sum2.direction, ncol = 2, labeller = as_labeller(c("negative" = "negative dominance",
                                                                         "positive" = "positive dominance")))
layout <- "
AABBCC
"
VAD_nonEmo <- vfac_nonEmo + dfac_nonEmo + afac_nonEmo + 
  plot_layout(axis_titles = "collect", design = layout, widths = c(4.5, 4.5, 4.5)) + 
  plot_annotation(title = "Communicatives without emotion entailment")

VAD_emo / VAD_nonEmo + plot_layout(axis_titles = "collect")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-faceted-test.pdf", height = 6, width = 12.5)

#### MV ----
##### neg only ----
###### two lines ----
# combined plot of projection by VAD with two fitted lines
# valence
vplotMVneg <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>%   
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj.MV.neg)) +
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
  scale_colour_manual(values = cols2, labels = predicateType2_names) 

# arousal
aplotMVneg <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>%   
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj.MV.neg)) +
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
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)

# dominance
dplotMVneg <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>%   
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj.MV.neg)) +
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

vplotMVneg + aplotMVneg + dplotMVneg + plot_layout(guides = "collect", axis_titles = "collect") # & theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-NO.pdf", height = 3.5, width = 12)

###### faceted ----
vfacMVneg <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj.MV.neg)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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

afacMVneg <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj.MV.neg)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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

dfacMVneg <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj.MV.neg)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
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
                                 .cols = predicateType2_names)) 

vfacMVneg + afacMVneg + dfacMVneg + plot_layout(guides = "collect", axis_titles = "collect") # & theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-faceted-NO.pdf", height = 4.5, width = 12.5)


##### all embedding environments ----
###### two lines ----
# combined plot of projection by VAD with two fitted lines
# valence
vplotMV <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>%   
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj.MV)) +
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
  scale_colour_manual(values = cols2, labels = predicateType2_names) 

# arousal
aplotMV <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>%   
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj.MV)) +
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
       y = "Mean projection rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)

# dominance
dplotMV <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>%   
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj.MV)) +
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

vplotMV + aplotMV + dplotMV + plot_layout(guides = "collect", axis_titles = "collect") # & theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-MV.pdf", height = 3.5, width = 12)

###### faceted ----
vfacMV <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum2, y = Mean.Proj.MV)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank()) + 
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 

afacMV <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum2, y = Mean.Proj.MV)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank()) + 
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names))

dfacMV <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum2, y = Mean.Proj.MV)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank()) + 
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 

vfacMV + afacMV + dfacMV + plot_layout(guides = "collect", axis_titles = "collect") # & theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-faceted-MV.pdf", height = 4.5, width = 12.5)

###### original VAD ratings ----
vfacMVorig <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = V.Mean.Sum, y = Mean.Proj.MV)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank()) + 
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(1, 9), breaks = c(1:9)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 

afacMVorig <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = A.Mean.Sum, y = Mean.Proj.MV)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank()) + 
  labs(x = "Mean arousal rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(1, 9), breaks = c(1:9)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names))

dfacMVorig <- mean.proj.e1.plus.VAD %>% 
  filter(predicateType == "communicative") %>% 
  ggplot(aes(x = D.Mean.Sum, y = Mean.Proj.MV)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        panel.grid.minor.y = element_blank()) + 
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(1, 9), breaks = c(1:9)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_grid(V.Mean.Sum2.direction ~ predicateType2, 
             labeller = labeller(.rows = as_labeller(c("negative" = "negative valence", 
                                                       "positive" = "positive valence")), 
                                 .cols = predicateType2_names)) 

vfacMVorig + afacMVorig + dfacMVorig + plot_layout(guides = "collect", axis_titles = "collect") # & theme(legend.position = "top")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-faceted-MV-orig.pdf", height = 4.5, width = 12.5)
