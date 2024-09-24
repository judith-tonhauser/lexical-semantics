# Testing hypotheses based on experiment 1

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
nrow(e1) # 3223
names(e1)
length(unique(e1$participant_id)) # 293

# add predicate type (communicative with/without emotion entailment distinction)
emoComms <- c("bitch", "boast", "brag", "cheer", "complain", "cry", "exclaim", "fuss", "gloat", 
                 "groan", "grumble", "grunt", "howl", "moan", "mutter", "pout", "quarrel", "rant", 
                 "rave", "scream", "shriek", "sigh", "sob", "squeal", "weep", "whimper", "whine")
e1 <- e1 %>% 
  mutate(predicateType2 = case_when(predicate %in% c("know", "think") ~ "cognitive",
                                    predicate %in% emoComms ~ "emoComm",
                                    TRUE ~ "nonEmoComm"))

# add the number of ratings each predicate has received
e1 <- e1 %>%
  group_by(predicate) %>%
  mutate(ratings_count = n()) %>%
  ungroup()

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

tmp <- e1 %>%
  select(predicate, predicateType2, ratings_count) %>%
  distinct(predicate, predicateType2, ratings_count)
nrow(tmp) # 192

mean.proj.e1 <- left_join(mean.proj.e1, tmp, by = c("predicate")) %>%
  distinct() %>%
  mutate(predicate = fct_reorder(as.factor(predicate), Mean.Proj))
nrow(mean.proj.e1) # 192

## MV dataset ----
d <- read.csv("../../MegaVeridicality/data/d.csv")
nrow(d) # 21692

# create dataset for projection inferences
d.proj <- droplevels(subset(d, d$polarity == "negative" | d$conditional == "True"))
nrow(d.proj) # 16291

# replace underscores in multi-word predicates in the MV with spaces (for comparison with new data)
d.proj$verb <- gsub("_", " ", d.proj$verb)

# calculate by-predicate projection means for MV data
mean.proj.MV <- d.proj %>%
  group_by(verb, voice) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(verb), Mean.Proj)) %>% 
  ungroup()

nrow(mean.proj.MV) # 544

# only include active voice predicates
# when a predicate also occurs in the passive voice in the MV dataset (e.g., 'radio'), we consider
# it a different predicate type (e.g. 'was radioed' = reportative evidential) and it is therefore 
# not relevant to this investigation of communicative predicates.
mean.proj.MV <- mean.proj.MV %>% filter(voice == "active")
nrow(mean.proj.MV) # 375

# only the predicates investigated here
mean.proj.MV <- mean.proj.MV %>% filter(predicate %in% mean.proj.e1$predicate)
mean.proj.MV %>% nrow() # 192

# create dataset for projection from under negation only
d.proj.neg <- droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj.neg) # 5411

d.proj.neg$verb <- gsub("_", " ", d.proj.neg$verb)

# calculate by-predicate projection means
mean.proj.MV.neg <- d.proj.neg %>%
  group_by(verb, voice) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         predicate = fct_reorder(as.factor(verb), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj.MV.neg) # 544

# only include active voice predicates
mean.proj.MV.neg <- mean.proj.MV.neg %>% filter(voice == "active")
nrow(mean.proj.MV.neg) # 375

# only the predicates investigated here
mean.proj.MV.neg <- mean.proj.MV.neg %>% filter(predicate %in% mean.proj.e1$predicate)
mean.proj.MV.neg %>% nrow() # 192

## combine data ----
# add mean acceptability ratings from MV dataset to new data
e1 <- e1 %>%
  left_join(
    mean.proj.MV %>%
      filter(predicate %in% e1$predicate) %>%
      select(predicate, Mean.Acc) %>%
      rename_with(~ paste0(., ".MV"), "Mean.Acc"),
    by = "predicate"
  ) %>%
  left_join(
    mean.proj.MV.neg %>%
      filter(predicate %in% e1$predicate) %>%
      select(predicate, Mean.Acc) %>%
      rename_with(~ paste0(., ".MV.neg"), "Mean.Acc"),
    by = "predicate"
  )

# dataframe with mean projection and acceptability ratings
mean.proj.e1.plus <- mean.proj.e1 %>%
  left_join(
    mean.proj.MV %>%
      filter(predicate %in% mean.proj.e1$predicate) %>%
      select(predicate, Mean.Proj, Mean.Acc, YMin.Proj, YMax.Proj) %>%
      rename_with(~ paste0(., ".MV"), starts_with(c("Mean", "Y"))),
    by = "predicate") %>%
  left_join(
    mean.proj.MV.neg %>%
      filter(predicate %in% mean.proj.e1$predicate) %>%
      select(predicate, Mean.Proj, Mean.Acc, YMin.Proj, YMax.Proj) %>%
      rename_with(~ paste0(., ".MV.neg"), starts_with(c("Mean", "Y"))),
    by = "predicate") %>% 
  mutate(Mean.Proj.diff = Mean.Proj - Mean.Proj.MV,
         Mean.Proj.diff.neg = Mean.Proj - Mean.Proj.MV.neg)



# new data ----
# how many ratings per predicate?
e1 %>% group_by(predicate) %>% summarise(count = n()) %>% filter(count >= 10 & count < 20) %>% nrow()
# 0 - 9 ratings: 5 predicates
# 10 - 19 ratings: 138 predicates
# 20 - 29 ratings: 49 predicates

# which are the predicates with the lowest/highest numbers of ratings?
e1 %>% group_by(predicate) %>% summarise(count = n()) %>% arrange(count) %>% print(n = Inf)
# fewer than 10 ratings:
#   predicate   count
# 1 chronicle       8
# 2 leak            8
# 3 question        8
# 4 holler          9
# 5 restate         9

# more than 25 ratings:
#     predicate   count
# 187 know           26
# 188 pretend        26
# 189 reaffirm       26
# 190 think          26
# 191 certify        27
# 192 grunt          29

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
ggsave("../graphs/projection-by-communicative.pdf", height = 4, width = 13)

### labels for extreme predicates ----
# how many highlighted predicates on either side?
mean.proj.e1 %>% 
  filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.1) %>% 
  nrow() # 15 - without 'know', 14 predicates have projection ratings at least as high as 'know' minus 0.1.
mean.proj.e1 %>% 
  filter(Mean.Proj >= subset(mean.proj.e1, predicate == "know")$Mean.Proj - 0.2) %>% 
  nrow() # 40 - without 'know', 39 predicates have projection ratings at least as high as 'know' minus 0.1.

#### 14-14 ----
# labels for the 14 most and least projective predicates
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1, Mean.Proj, n = 14),
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
  geom_label_repel(data = slice_min(mean.proj.e1, Mean.Proj, n = 15) %>% filter(predicate != "think"),
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
ggsave("../graphs/projection-by-communicative-labels-14-14.pdf", height = 4, width = 13)

#### 14pos-14 ----
# labels for the 14 most projective predicates and 14 least projective ones with positive projection rating
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0), Mean.Proj, n = 14),
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
  geom_label_repel(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0), Mean.Proj, n = 14),
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
ggsave("../graphs/projection-by-communicative-labels-14pos-14.pdf", height = 4, width = 13)

#### 39-39 ----
# labels for the 39 most and least projective predicates
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1, Mean.Proj, n = 39),
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
  geom_label_repel(data = slice_min(mean.proj.e1, Mean.Proj, n = 40) %>% filter(predicate != "think"),
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
ggsave("../graphs/projection-by-communicative-labels-39-39.pdf", height = 4, width = 13)

#### 39pos-39 ----
# 39 most projective predicates and 39 least projective ones with positive projection rating
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.e1, 
             aes(x = predicate, y = Mean.Proj, colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0), Mean.Proj, n = 39),
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
  geom_label_repel(data = slice_min(mean.proj.e1 %>% filter(Mean.Proj >= 0), Mean.Proj, n = 40) %>% 
                     filter(predicate != "say"),
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
ggsave("../graphs/projection-by-communicative-labels-39pos-39.pdf", height = 4, width = 13)

### emotion entailment ----
#### plot ----
ggplot(mean.proj.e1, aes(x = predicate, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.e1 %>% filter(predicate %in% emoComms), 
             aes(x = predicate, y = Mean.Proj, colour = "emoComms")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "say"), 
             aes(x = predicate, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "think"), 
             aes(x = predicate, y = Mean.Proj, colour = "think")) +
  geom_point(data = mean.proj.e1 %>% filter(predicate == "know"), 
             aes(x = predicate, y = Mean.Proj, colour = "know")) +
  geom_label_repel(data = mean.proj.e1 %>% filter(predicate %in% emoComms),
                   aes(label = paste0(predicate, ": ", round(Mean.Proj, 2))), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.4,
                   colour = "green3") +
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
  scale_colour_manual(limits = c("communicative predicate", "emoComms", "say", "think", "know"),
                      values = c("deepskyblue2", "green3", "blue", "deeppink", "orangered3"))
ggsave("../graphs/projection-by-communicative-emo-labels.pdf", height = 4, width = 13)

#### models ----
lm(rating ~ fct_relevel(predicateType2, "emoComm"), data = e1) %>% summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.60111    0.03251  18.492  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.26765    0.10279  -2.604  0.00926 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.23066    0.03521  -6.552 6.61e-11 ***

lm(rating ~ predicateType2, data = e1 %>% filter(! predicateType2 == "cognitive")) %>% 
  summary()
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.60111    0.03241  18.549  < 2e-16 ***
# predicateType2nonEmoComm -0.23066    0.03510  -6.571 5.81e-11 ***

lm(Mean.Proj ~ predicateType2, data = mean.proj.e1 %>% filter(! predicateType2 == "cognitive")) %>% 
  summary()
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.59884    0.05671  10.560  < 2e-16 ***
# predicateType2nonEmoComm -0.22606    0.06122  -3.692 0.000291 ***

# run models with participant, name, CC, no. of ratings



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
#### plots ----
##### overall ----
# new data vs MV data with all embedding environments

# with ratings count
mean.proj.e1.plus %>%  
  arrange(Mean.Proj.diff) %>%
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>%
ggplot(aes(x = reorder(predicate, Mean.Proj.diff), y = Mean.Proj.diff, fill = ratings_count)) +
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


##### negation only ----
# new data vs MV data with negation as embedding environment only
# with ratings count
mean.proj.e1.plus %>%  
  arrange(Mean.Proj.diff.neg) %>%
  mutate(group = cut_number(row_number(), n = 3, labels = c("Group 1", "Group 2", "Group 3"))) %>%
  ggplot(aes(x = reorder(predicate, Mean.Proj.diff.neg), y = Mean.Proj.diff.neg, fill = ratings_count)) +
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


# rating patterns ----
e1 %>%
  filter(rating == -0.52) %>% 
  group_by(participant_id) %>% 
  summarise(rating_count = n()) %>% 
  group_by(rating_count) %>% 
  summarise(participant_count = n()) %>% 
  mutate(count_sum = cumsum(participant_count))

# rating 1 ("yes")
#   rating_count participant_count count_sum
#          <int>             <int>     <int>
# 1            1                34        34
# 2            2                29        63
# 3            3                26        89
# 4            4                18       107
# 5            5                19       126
# 6            6                22       148
# 7            7                16       164
# 8            8                13       177
# 9            9                18       195
# 10           10                17       212
# 11           11                32       244
# 244 of 293 participants (83%) rated (precisely) "yes" at least once; 49 (17%) never rated "yes".
# 32 (11%) participants answered "yes" for every item.

# rating 0
#   rating_count participant_count count_sum
#           <int>             <int>     <int>
# 1            1                35        35
# 2            2                 8        43
# 3            3                 8        51
# 4            4                 5        56
# 5            5                 7        63
# 6            6                 3        66
# 7            7                 1        67
# 8            8                 2        69
# 9            9                 1        70
# 10           10                 1        71
# 11           11                 1        72
# 72 participants (25%) rated (precisely) 0 at least once, 221 (75%) never rated 0. 
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
# 96 (33%) participants rated (precisely) "no" at least once, 197 (67%) never rated -1. 
# One participant rated "no" for every item.



