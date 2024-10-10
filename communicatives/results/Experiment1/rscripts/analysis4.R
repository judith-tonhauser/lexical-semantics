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

# # add predicate type (communicative with/without emotion entailment distinction) and type of emoComm
# emoComms <- c("bitch", "boast", "brag", "cheer", "complain", "cry", "exclaim", "fuss", "gloat", 
#                  "groan", "grumble", "grunt", "howl", "moan", "mutter", "pout", "quarrel", "rant", 
#                  "rave", "scream", "shriek", "sigh", "sob", "squeal", "weep", "whimper", "whine")
# emo_comm_attitude <- c("bitch", "boast", "brag", "cheer", "complain", "fuss", "gloat", "pout", "quarrel",
#                        "rant", "rave")   
# emo_comm_manner <- c("cry", "exclaim", "groan", "grumble", "grunt", "howl", "moan", "mutter", "scream",
#                      "shriek", "sigh", "sob", "squeal", "weep", "whimper", "whine")  

e1 <- e1 %>% 
  # mutate(predicateType2 = case_when(predicate %in% c("know", "think") ~ "cognitive",
  #                                   predicate %in% emoComms ~ "emoComm",
  #                                   TRUE ~ "nonEmoComm"),
  #        emoCommType = case_when(predicate %in% emo_comm_attitude ~ "attitude",
  #                                predicate %in% emo_comm_manner ~ "manner"))
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
# 
# # replace underscores in multi-word predicates in the MV with spaces (for comparison with new data)
# d.proj$verb <- gsub("_", " ", d.proj$verb)
# 
# # only include active voice predicates
# # when a predicate also occurs in the passive voice in the MV dataset (e.g., 'radio'), we consider
# # it a different predicate type (e.g. 'was radioed' = reportative evidential) and it is therefore 
# # not relevant to this investigation of communicative predicates.
# d.proj <- d.proj %>% filter(voice == "active")
# nrow(d.proj) # 11231

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
     # filter(predicate %in% e1$predicate) %>%
      select(predicate, Mean.Acc) %>%
      rename_with(~ paste0(., ".MV"), "Mean.Acc"),
    by = "predicate"
  ) %>%
  left_join(
    mean.proj.MV.neg %>%
      #filter(predicate %in% e1$predicate) %>%
      select(predicate, Mean.Acc) %>%
      rename_with(~ paste0(., ".MV.neg"), "Mean.Acc"),
    by = "predicate"
  )

# dataframe with mean projection and acceptability ratings
mean.proj.e1.plus <- mean.proj.e1 %>%
  left_join(
    mean.proj.MV %>%
      #filter(predicate %in% mean.proj.e1$predicate) %>%
      select(predicate, Mean.Proj, Mean.Acc, YMin.Proj, YMax.Proj) %>%
      rename_with(~ paste0(., ".MV"), starts_with(c("Mean", "Y"))),
    by = "predicate") %>%
  left_join(
    mean.proj.MV.neg %>%
     # filter(predicate %in% mean.proj.e1$predicate) %>%
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
# Which predicates are in the top 20 in both datasets?
mean.proj.e1.plus %>% 
  filter(predicate != "know" & rankingNew > 172 & rankingMVneg > 172) %>% 
  arrange(ranking_difference) %>% 
  select(predicate, Mean.Proj, Mean.Proj.MV.neg, ranking_difference)
#   predicate Mean.Proj Mean.Proj.MV.neg ranking_difference
#   <fct>         <dbl>            <dbl>              <dbl>
# 1 divulge       0.789              0.6                0.5
# 2 flaunt        0.846              0.7                1  
# 3 apologize     0.885              1                  4  
# 4 weep          0.791              0.7                7  
# 5 disclose      0.822              0.8                9  
# 6 whine         0.78               0.7               10  
# 7 question      0.905              0.6               14.5

# Which predicates are in the top 30 in both datasets?
mean.proj.e1.plus %>% 
  filter(predicate != "know" & rankingNew > 162 & rankingMVneg > 162) %>% 
  arrange(ranking_difference) %>% 
  select(predicate, Mean.Proj, Mean.Proj.MV.neg, ranking_difference)
#   predicate Mean.Proj Mean.Proj.MV.neg ranking_difference
#   <fct>         <dbl>            <dbl>              <dbl>
# 1 divulge       0.789            0.6                  0.5
# 2 flaunt        0.846            0.7                  1  
# 3 dispute       0.748            0.5                  3.5
# 4 apologize     0.885            1                    4  
# 5 fuss          0.753            0.5                  4.5
# 6 weep          0.791            0.7                  7  
# 7 disclose      0.822            0.8                  9  
# 8 whine         0.78             0.7                 10  
# 9 publicize     0.747            0.667               12  
# 10 question      0.905            0.6                 14.5
# 11 bitch         0.823            0.5                 15.5
# 12 complain      0.833            0.5                 18.5
# 13 point out     0.73             0.7                 19  
# 14 stress        0.891            0.5                 23.5
# 15 grumble       0.908            0.5                 25.5

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
