# Testing hypotheses based on White & Rawlins' MegaVeridicality I dataset and Ross & Pavlick's 
# VerbVeridicality dataset.
# analysis2.R

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

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
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
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"),
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
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# add predicateType etc. to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544


# Communicatives ----
# only communicatives
mean.proj.comm = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj.comm) # 236

## overall ----
# There is no way to make this a reasonably nice plot with labels for all 236 predicates.
# # All communicatives with labels for the most and least projective communicatives.
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
ggsave("../graphs/projection-by-communicative.pdf", height = 4, width = 13)

slice_max(mean.proj.comm, Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 cry              0.833
# 2 flip out         0.833
# 3 cringe           0.8  
# 4 celebrate        0.733
# 5 fess up          0.733
# 6 apologize        0.724
# 7 whine            0.7  
# 8 approve          0.667
# 9 disclose         0.667
# 10 grimace         0.667
# 11 pout            0.667

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
# 10 manufacture    0.0333
# 11 retract        0.0333

# How many of which type of communicative predicate?
mean.proj.comm %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  summarize(count=n())
#   commType                count
#   <chr>                   <int>
# 1 discourse participation   114
# 2 pure                       95
# 3 state changing             27

## "pure" communicatives ----
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
ggsave("../graphs/projection-by-communicative-minmax-pure.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "pure"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 cry              0.833
# 2 flip out         0.833
# 3 cringe           0.8  
# 4 celebrate        0.733
# 5 whine            0.7  
# 6 grimace          0.667
# 7 pout             0.667
# 8 document         0.633
# 9 weep             0.633
# 10 publicize       0.586

slice_min(subset(mean.proj.comm, commType == "pure"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 fax             0.0667
# 2 jest            0.0667
# 3 forecast        0.0690
# 4 holler          0.1   
# 5 joke            0.1   
# 6 type            0.1   
# 7 depict          0.133 
# 8 narrate         0.133 
# 9 prophesy        0.133 
# 10 will           0.133 

## "discourse participation" communicatives ----
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
ggsave("../graphs/projection-by-communicative-minmax-dp.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "discourse participation"), 
          Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 fess up          0.733
# 2 apologize        0.724
# 3 approve          0.667
# 4 disclose         0.667
# 5 bitch            0.633
# 6 complain         0.6  
# 7 explain          0.6  
# 8 stress           0.6  
# 9 flaunt           0.567
# 10 fuss            0.567
# 11 leak            0.567
# 12 point out       0.567
# 13 reveal          0.567
# 14 share           0.567

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

# "discourse participation communicatives" (our classification) - "discourse role verbs" (Grimshaw 2015)
mean.proj %>% 
  filter((! predicateType2 %in% c("comPriv", "other")) & 
           (commType == "discourse participation" | sayVerbType == "discourse role verb")) %>% 
  group_by(predicateType2, commType, sayVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType             n
#   <chr>          <chr>                   <chr>               <int>
# 1 emoComm        discourse participation mode verb               4 - A
# 2 emoComm        discourse participation NA                      3
# 3 nonEmoComm     discourse participation discourse role verb    47
# 4 nonEmoComm     discourse participation mode verb               1 - B
# 5 nonEmoComm     discourse participation NA                     59
# 6 nonEmoComm     pure                    discourse role verb     5 - C
# 7 nonEmoComm     state changing          discourse role verb     5 - D

# A
mean.proj %>% 
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
mean.proj %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "discourse participation" &
           sayVerbType == "mode verb") %>% 
  select(verb_renamed, modeVerbType)
#   verb_renamed modeVerbType     
#   <fct>        <chr>            
# 1 tease        say-with-attitude

# C
mean.proj %>% 
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
mean.proj %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "state changing" &
           sayVerbType == "discourse role verb") %>% 
  select(verb_renamed)
# 1 insist      
# 2 lie         
# 3 promise     
# 4 swear       
# 5 vow 


## "state changing" communicatives ----
# All communicatives with labels for all 27 state changing communicatives.
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
ggsave("../graphs/projection-by-communicative-sc.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-minmax-sc.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 conceal          0.6  
# 2 warn             0.552
# 3 expose           0.533
# 4 hush up          0.467
# 5 advise           0.367
# 6 certify          0.367
# 7 showcase         0.333
# 8 advertise        0.3  
# 9 guarantee        0.267
# 10 swear           0.233
# 11 vow             0.233

slice_min(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 pretend        -0.367 
# 2 fabricate      -0.2   
# 3 feign          -0.133 
# 4 fake            0.0333
# 5 manufacture     0.0333
# 6 test            0.0667
# 7 generalize      0.1   
# 8 promise         0.1   
# 9 implore         0.133 
# 10 demonstrate    0.138 

## say verbs ----
# distribution of communication predicates
d.proj %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(sayVerb, predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb predicateType     n
#   <chr>   <chr>         <int>
# 1 no      communicative   116
# 2 yes     cognitive         2
# 3 yes     comPriv           2
# 4 yes     communicative   120

d.proj %>% 
  filter((predicateType != "communicative" & predicateType != "comPriv") & sayVerb == "yes") %>% 
  distinct(verb_renamed)
# verb_renamed
# 1        think
# 2         pray
# These predicates "report internal linguistic formulation only" (Grimshaw 2015: 84).

# There are 240 communication predicates in the MV dataset. Two of them are say-
# verbs but not communicatives. Two are predicates can be interpreted both as 
# communicative and private predicates. These four predicates are excluded from
# analysis, i.e., only the 236 communicative predicates are included.

### types of communicatives ----
d.proj %>% 
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
# sayVerb     n
# <chr>   <int>
# 1 no        116
# 2 yes       120
# Of the 236 communicatives in the MV dataset, 120 are say-predicates.

mean.proj.commsay <- d.proj %>%
  filter(predicateType == "communicative") %>% 
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

### types of say verbs ----
d.proj %>% 
  filter((sayVerb == "yes" & predicateType == "communicative")) %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
# sayVerbType             n
# <chr>               <int>
# 1 discourse role verb    57
# 2 mode verb              62
# 3 say                     1

mean.proj.saytype <- d.proj %>%
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
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

mean.proj.saytype2 <- d.proj %>%
  filter(predicateType == "communicative") %>% 
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

lm(Mean.Proj ~ fct_relevel(sayVerbType, "say"), data = mean.proj) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   ***
# mode verb             ***
# say                   .

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "say") + 
       (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.37808    0.07984   4.735 2.19e-06 ***
# fct_relevel(sayVerbType, "discourse role verb")say       -0.01984    0.42958  -0.046    0.963  
# fct_relevel(sayVerbType, "mode verb")discourse role verb -0.37808    0.07984  -4.735 2.19e-06 ***
# fct_relevel(sayVerbType, "mode verb")say                 -0.39792    0.42905  -0.927    0.354  
# fct_relevel(sayVerbType, "say")discourse role verb        0.01984    0.42958   0.046    0.963
# fct_relevel(sayVerbType, "say")mode verb                  0.39792    0.42905   0.927    0.354

### types of mode verbs ----
d.proj %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means         48
# 2 say-with-attitude    14

mean.proj.modetype <- d.proj %>%
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
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

clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-by-means") + 
       (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude 0.007779   0.137480   0.057    0.955
# fct_relevel(modeVerbType, "say-with-attitude")say-by-means -0.007779   0.137480  -0.057    0.955

### types of say-by-means verbs ----
d.proj %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                  14
# 2 manner                26
# 3 sound                  8

mean.proj.bymeanstype <- d.proj %>%
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2", "grey50"))
ggsave("../graphs/projection-by-saybymeansverb-type.pdf", height = 4, width = 10)

clmm(as.factor(veridicality_num) ~ fct_relevel(sayByMeansVerbType, "sound") + 
       (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(sayByMeansVerbType, "form")manner   0.2583     0.1515   1.705   0.0882 .
# fct_relevel(sayByMeansVerbType, "form")sound    0.2020     0.2071   0.975   0.3294  
# fct_relevel(sayByMeansVerbType, "manner")form  -0.25833    0.15150  -1.705   0.0882 .
# fct_relevel(sayByMeansVerbType, "manner")sound -0.05634    0.19388  -0.291   0.7714
# fct_relevel(sayByMeansVerbType, "sound")form   -0.20199    0.20711  -0.975    0.329
# fct_relevel(sayByMeansVerbType, "sound")manner  0.05635    0.19388   0.291    0.771

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### overall ----
mean.proj.overall <- d.proj %>%
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
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


## VerbVeridicality dataset ----
z <-  read.csv("../data/verb-veridicality.csv")
nrow(z) # 1498

# separate projection ratings (negative environment ratings) into separate columns. 
z <- z %>% 
  mutate(turker_neg_ratings2 = turker_neg_ratings, .after = turker_pos_ratings) %>% 
  separate(turker_neg_ratings, c("rating1", "rating2", "rating3"), 
           sep = ",")

# select only items with that-complements
z <- z %>% 
  subset(z$task == "that") %>% 
  pivot_longer(
    cols = c("rating1", "rating2", "rating3"), 
    names_to = "rating_number", 
    values_to = "rating")
nrow(z) # 2577

# "rating.rescaled" for easier comparison with MV dataset whose ratings we have
# recoded to numerical values ranging from -1 to 1.
z <- z %>% mutate(rating = as.numeric(as.character(z$rating)),
                  rating.rescaled = rating/2)

# how many predicates?
z %>% distinct(verb) %>% count() # 78

z %>% 
  filter(verb %in% d.proj$verb) %>%  
  distinct(verb) %>% 
  count() # 67

z %>% 
  filter(!verb %in% d.proj$verb) %>%  
  distinct(verb) %>% 
  arrange(verb) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "appear"    "be"        "felt"      "give"      "hold"      "mean"      "provide"   "recommend" "saw"      
# [10] "seem"      "speculate" 

# excluded from analysis:
# 'appear': pseudo-copula
# 'be': copula
# 'give': = 'given that' (= 'since', 'considering that')
# 'mean': only with non-sentient subjects
# 'provide': non-sentient subjects or 'provided that' (= 'on the condition that')
# 'seem': pseudo-copula

z2 <- z %>% 
  filter(! verb %in% c("appear", "be", "give", "mean", "provide", "seem"))

z2 %>% distinct(verb) %>% count() # 72

vvonlypreds <- c("felt", "hold", "recommend", "saw", "speculate") 

# Creating .csv file for predicate coding for VV dataset, transferring the 
# classifications for the 67 predicates that occur in both datasets from 
# predicate-coding.csv (for MV predicates) to the new file. Classifications
# for the 5 predicates that occur only in the VV dataset are added manually
# in the .csv file. 

# view(left_join(z2, y, by = c("verb")))
# 
# predicateCodingVV <- left_join(z2, y, by = c("verb")) %>% 
#   select(!c(1:2, 4:17)) %>% 
#   group_by(verb) %>% 
#   slice_head()
# view(predicateCodingVV)

# DO NOT RUN THIS AGAIN!
# predicateCodingVV %>% [DO NOT RUN!!!] write.csv("../data/predicate-coding-vv.csv", row.names = FALSE)


# load predicate coding
z3 <- read.csv("../data/predicate-coding-vv.csv")
nrow(z3) # 72

dvv <- left_join(z2, z3, by = c("verb"))
nrow(dvv) # 2403

# create predicateType, emotiveComponent, change-of-state, environment columns
d.proj.vv <- dvv %>%
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

# calculate by-predicate projection means for original and rescaled ratings.
mean.proj.vv <- dvv %>%
  group_by(verb) %>% 
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         verb = fct_reorder(as.factor(verb), Mean.Proj))

# add predicateType etc. to the means
tmp3 <- d.proj.vv %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)
nrow(tmp3) # 72

mean.proj.vv <- left_join(mean.proj.vv, tmp3, by = c("verb")) %>%
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.vv) # 68

### say verbs ----
# distribution of communication predicates
d.proj.vv %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(sayVerb, predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb predicateType     n
#   <chr>   <chr>         <int>
# 1 no      communicative    12
# 2 yes     cognitive         1
# 3 yes     comPriv           2
# 4 yes     communicative    22
# There are 37 communication predicates in the VV dataset. One of them ('think') 
# is a say-verb but not a communicative. Two are predicates that can be interpreted
# both as communicative and private predicates. These three predicates are excluded
# from analysis.

vvonly.acc.comms <- d.proj.vv %>% 
  filter(verb_renamed %in% vvonlypreds & predicateType == "communicative") %>% 
  distinct(verb_renamed)
# verb_renamed
# <chr>       
# 1 recommend   

d.proj.vv %>% 
  filter(predicateType == "communicative" & ! verb_renamed %in% acc.comms) %>% 
  distinct(verb_renamed)
#   verb_renamed
#   <chr>       
# 1 recommend   

# All communicative predicates in the VV dataset that also occur in the MV dataset
# have a mean acceptability rating of greater than 4 in the MV dataset. Therefore,
# non of the 34 communicatives are excluded from analysis. 


#### types of communicatives ----
d.proj.vv %>% 
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb     n
#   <chr>   <int>
# 1 no         12
# 2 yes        22
# Of the 34 communicatives in the MV dataset, 22 are say-predicates.

mean.proj.vv.commsay <- d.proj.vv %>%
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.vv.commsay) # 2

ggplot(mean.proj.vv.commsay, aes(x = sayVerb, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1))
ggsave("../graphs/projection-by-communication-type-vv.pdf", height = 4, width = 10)

#### types of say verbs ----
d.proj.vv %>% 
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerbType             n
#   <chr>               <int>
# 1 discourse role verb    19
# 2 mode verb               2
# 3 say                     1

mean.proj.saytype.vv <- d.proj.vv %>%
  filter((sayVerb == "yes" & predicateType == "communicative")) %>%  
  group_by(sayVerbType) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.saytype.vv) # 3

ggplot(mean.proj.saytype.vv, aes(x = sayVerbType, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1))
ggsave("../graphs/projection-by-sayverb-type-vv.pdf", height = 4, width = 10)

mean.proj.saytype.vv2 <- d.proj.vv %>%
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.saytype.vv2) # 4

ggplot(mean.proj.saytype.vv2, aes(x = sayVerb, y = Mean.Proj, colour = sayVerbType)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_colour_manual(values = c("purple","deeppink", "orange3", "grey50"))
ggsave("../graphs/projection-by-sayverb-type-vv2.pdf", height = 4, width = 10)

#### types of mode verbs ----
d.proj.vv %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means          1
# 2 say-with-attitude     1

mean.proj.modetype.vv <- d.proj.vv %>%
  filter((sayVerb == "yes" & predicateType == "communicative")) %>%  
  group_by(sayVerbType, modeVerbType) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.modetype.vv) # 4

ggplot(mean.proj.modetype.vv, aes(x = sayVerbType, y = Mean.Proj, colour = modeVerbType)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-vv.pdf", height = 4, width = 10)

#### types of say-by-means verbs ----
d.proj.vv %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                   1

# No plot for only one predicate. :) - no "overall" plot, either.

### by-predicateType ----
d.proj.vv %>%
  filter(predicateType2 != "comPriv" & predicateType2 != "other") %>% 
  group_by(predicateType2) %>%
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         16
# 2 emoComm            1
# 3 emotive            3
# 4 evidential        15
# 5 nonEmoComm        33

d.proj.vv %>%
  filter(predicateType2 == "emoComm") %>% 
  distinct(verb_renamed)
#   verb_renamed
#   <chr>       
# 1 complain  

# With only one communicative with an emotive component, the VV dataset is of 
# limited use for investigating the 'emotive component' property.

# With only three projection ratings per item, the dataset is overall ill-suited 
# for a more fine-grained investigation of communication predicates, 

#### plots ----
# calculate by-predicateType means

mean.proj.type.vv = d.proj.vv %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj))
nrow(mean.proj.type.vv) # 6

mean.proj.type.vv %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
  ggplot(aes(x = predicateType, y = Mean.Proj.rescaled, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 5.5, 22, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType-vv.pdf", height = 4, width = 10)

# calculate by-predicateType2 means
mean.proj.type2.vv = d.proj.vv %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
nrow(mean.proj.type2.vv) # 7

mean.proj.type2.vv %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
  ggplot(aes(x = factor(predicateType2, c("cognitive", "evidential", "nonEmoComm", 
                                          "emoComm", "emotive")), 
             y = Mean.Proj.rescaled, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled), width = 0) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_x_discrete(labels = c("cognitive", "evidential",
                              "communicative without\nemotive component",
                              "communicative with\nemotive component", "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2-vv.pdf", height = 4, width = 10)

### by predicate ----
#### plots ----
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicate-vv.pdf", height = 4, width = 13)

# with labels
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_label_repel(data = subset(mean.proj.vv, predicateType == "emotive"),
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
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-labelled-vv.pdf", height = 4, width = 13)

# faceted
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType, ncol = 4)
ggsave("../graphs/projection-by-predicate-faceted-vv.pdf", height = 4, width = 9)

# with labels
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_label_repel(data = subset(mean.proj.vv, predicateType2 == "emotive"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  geom_label_repel(data = subset(mean.proj.vv, predicateType2 == "emoComm"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = -0.2,
                   colour = "green3") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(labels = predicateType2_names, values = cols2)
ggsave("../graphs/projection-by-predicate-labelled2-vv.pdf", height = 4, width = 13)

# faceted
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType2)) +
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
ggsave("../graphs/projection-by-predicate-faceted-vv2.pdf", height = 4, width = 10)

#### models ----
lm(Mean.Proj.rescaled ~ fct_relevel(predicateType, "communicative"), data = mean.proj.vv) %>% 
  summary()
# Coefficients:
#                                                       Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                           -0.08113    0.04271  -1.899    0.062 .
# fct_relevel(predicateType, "communicative")cognitive   0.03279    0.07550   0.434    0.666  
# fct_relevel(predicateType, "communicative")emotive     0.13609    0.15000   0.907    0.368  
# fct_relevel(predicateType, "communicative")evidential  0.09766    0.07720   1.265    0.210 

# The predicate type communicative is significant at the 0.1 level, i.e. not really
# significant. None of the other predicate types are significant.

lm(Mean.Proj.rescaled ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.vv) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                       0.09722    0.24998   0.389    0.699
# fct_relevel(predicateType2, "emoComm")cognitive  -0.14556    0.25767  -0.565    0.574
# fct_relevel(predicateType2, "emoComm")emotive    -0.04226    0.28865  -0.146    0.884
# fct_relevel(predicateType2, "emoComm")evidential -0.08069    0.25818  -0.313    0.756
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.18375    0.25374  -0.724    0.472

# None of the five predicate types with the 'emotive component' distinction are 
# correlated with projection ratings.

## VAD ratings ----

# load valence and arousal data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  filter(Word %in% d$verb) %>%
  rename(verb = Word) %>%
  left_join(d, by = "verb")
nrow(w2) # 17747
n_distinct(w2$verb) # 423

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

# remove "other" and "comPriv" predicates
w2 <-  w2 %>%
  filter(predicateType != "other" & predicateType != "comPriv") 
nrow(w2) # 17070

# how many predicates?
n_distinct(w2$verb_renamed) # 428
n_distinct(w2$verb) # 410

w3 <- w2 %>% 
  distinct(verb_renamed, verb, voice, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum)

# combine projection and valence/arousal/dominance ratings in one data frame
mean.proj2 <-  left_join(w3, mean.proj, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj2) # 428

table(mean.proj2$predicateType)
# cognitive communicative       emotive    evidential 
#        46           205           101            76 

table(mean.proj2$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        46         41        101         76        164 

### rescale V + A + D ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings for valence and arousal are rescaled to range from 0 to 1. 
new.scale <- mean.proj2 %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))

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

# Dominance: everything in order? - yes.
(max(mean.proj2$D.Mean.Sum)-min(mean.proj2$D.Mean.Sum))/4
# [1] 1.1825
new.scale %>% 
  group_by(D.Mean.Sum2.direction) %>% 
  slice_max(D.Mean.Sum2) %>% 
  ungroup() %>% 
  pull(D.Mean.Sum2) %>% 
  sum()
# [1] 1.1825

### Dominance ---- 
#### by predicate ----
# with emotive component distinction for communicatives
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

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
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                       0.17329    0.02241   7.734 7.71e-14 ***
#   fct_relevel(predicateType2, "emoComm")cognitive   0.07834    0.03082   2.542  0.01137 *  
#   fct_relevel(predicateType2, "emoComm")emotive     0.07351    0.02657   2.767  0.00591 ** 
#   fct_relevel(predicateType2, "emoComm")evidential  0.08345    0.02780   3.002  0.00284 ** 
#   fct_relevel(predicateType2, "emoComm")nonEmoComm  0.03851    0.02505   1.537  0.12503  

# For all five predicate types there is a correlation with dominance ratings at the
# 0.001 significance level.

#### by predicate type ----
# calculate dominance by predicateType2 means (communicatives with/without emotive component)
mean.dominance2 = new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance2

ggplot(mean.dominance2, aes(x = predicateType2, y = Mean.Dominance, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/dominance-by-predicateType2.pdf", height = 8, width = 10)

#### distribution ----
##### by predicate type ----
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
                    labels = c("cognitive", "communicative with\nemotive component", 
                               "emotive", "evidential", 
                               "communicative without\nemotive component"))
ggsave("../graphs/valence-dominance-by-predicateType2.pdf", height = 6, width = 10)

# valence by dominance with fitted line
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean valence rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-line.pdf", height = 6, width = 8)

# valence by dominance with fitted lines for positive/negative valence
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
       y = "Mean valence rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-line-direction-of-valence.pdf", height = 6, width = 8)

# valence by dominance with fitted lines for positive/negative dominance
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
       y = "Mean valence rating",
       colour = "Predicate type", 
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-line-direction-of-dominance.pdf", height = 6, width = 8)


#### ratings correlated? ----
##### valence - dominance ----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18418    0.01699  10.841  < 2e-16 ***
# D.Mean.Sum2  0.45549    0.06275   7.258 1.87e-12 ***

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         .
# emotive         ***
# evidential      **
# nonEmoComm      *

lm(D.Mean.Sum2 ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.15897    0.01166  13.629  < 2e-16 ***
# V.Mean.Sum2  0.24163    0.03329   7.258 1.87e-12 ***

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       ***
# emoComm         n.s.
# emotive         ***
# evidential      ***
# nonEmoComm      *

###### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-valence.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-valence-faceted.pdf", height = 6, width = 8)

ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-dominance.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-dominance-faceted.pdf", height = 6, width = 8)


##### arousal - dominance ----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.18418    0.01699  10.841  < 2e-16 ***
# D.Mean.Sum2  0.45549    0.06275   7.258 1.87e-12 ***

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s. 

lm(D.Mean.Sum2 ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.20216    0.02600   7.774 5.75e-14 ***
# A.Mean.Sum2  0.06394    0.06047   1.057    0.291 

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s. 

###### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-arousal.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-arousal-faceted.pdf", height = 6, width = 8)

ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-dominance.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-dominance-faceted.pdf", height = 6, width = 8)

##### valence - arousal ----
lm(V.Mean.Sum2 ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.0001786  0.0326765   0.005    0.996    
# A.Mean.Sum2 0.6958581  0.0759862   9.158   <2e-16 ***

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         ***
# emotive         **
# evidential      *
# nonEmoComm      **

lm(A.Mean.Sum2 ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.345934   0.009044  38.252   <2e-16 ***
# V.Mean.Sum2 0.236373   0.025811   9.158   <2e-16 ***

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "evidential"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       n.s.
# emoComm         ***
# emotive         **
# evidential      *
# nonEmoComm      **

###### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-valence.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-valence-faceted.pdf", height = 6, width = 8)

ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-arousal.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-arousal-faceted.pdf", height = 6, width = 8)


#### predicate types  ----
# do they predict VAD ratings?
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "evidential"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      *** 

# All predicate types seem to be highly correlated with VAD ratings.

##### tables ----
# direction of dominance and valence correlated?

contingency_table <- table(new.scale$D.Mean.Sum2.direction, new.scale$V.Mean.Sum2.direction)
print(contingency_table)
#          negative positive
# negative      119       21
# positive       66      222

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 145.44, df = 1, p-value < 2.2e-16

# The direction of dominance and valence seem highly correlated.

# distribution of direction within predicate types
new.scale %>% 
  count(predicateType2, D.Mean.Sum2.direction)
#    predicateType2 D.Mean.Sum2.direction   n
# 1       cognitive              negative  10 22% of cognitives
# 2       cognitive              positive  36
# 3         emoComm              negative  17 41% of emoComms
# 4         emoComm              positive  24
# 5         emotive              negative  55 54% of emotives
# 6         emotive              positive  46
# 7      evidential              negative  13 17% of evidentials
# 8      evidential              positive  63
# 9      nonEmoComm              negative  45 27% of nonEmoComms
# 10     nonEmoComm              positive 119

new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#    predicateType2 V.Mean.Sum2.direction   n
# 1       cognitive              negative  14 30% of cognitives
# 2       cognitive              positive  32
# 3         emoComm              negative  29 71% of emoComms <== by far largest diff from dominance direction
# 4         emoComm              positive  12
# 5         emotive              negative  64 63% of emotives
# 6         emotive              positive  37
# 7      evidential              negative  17 22% of evidentials
# 8      evidential              positive  59
# 9      nonEmoComm              negative  61 37% of nonEmoComms
# 10     nonEmoComm              positive 103


#### valence by dominance: no patterns (clusters) emerge. ----
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


#### projection by dominance with emotive component ----
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-dominance2.pdf", height = 6, width = 8)

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
ggsave("../graphs/projection-by-dominance-faceted2.pdf", height = 4, width = 9)

lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.36730    0.02465  14.902   <2e-16 ***
# D.Mean.Sum2  0.11977    0.09104   1.316    0.189 

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# emoComm: 0.05
# nonEmoComm: n.s.
# emotive: n.s.
# evidential: n.s.
# cognitive: n.s.

#### tables ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#     verb_renamed D.Mean.Sum2
# 1   be delighted      0.5725
# 2          enjoy      0.5700
# 3        approve      0.5650
# 4   be comforted      0.5450
# 5       research      0.5400
# 6         detail      0.5350
# 7  be stimulated      0.5350
# 8      recognize      0.5275
# 9      determine      0.5225
# 10        signal      0.5125
# 11  be signalled      0.5125

new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         36
# 2 emoComm           24
# 3 emotive           46
# 4 evidential        63
# 5 nonEmoComm       119

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2)
#     verb_renamed D.Mean.Sum2
# 1            cry      0.6100
# 2          panic      0.5875
# 3    be panicked      0.5875
# 4  be frightened      0.5725
# 5    be tortured      0.5600
# 6   be terrified      0.5525
# 7      be fooled      0.5150
# 8          doubt      0.4850
# 9           envy      0.4600
# 10         worry      0.4575
# 11    be worried      0.4575

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         10
# 2 emoComm           17
# 3 emotive           55
# 4 evidential        13
# 5 nonEmoComm        45

#### with direction ----
# projection by dominance with direction
##### plots ----
###### with direction of valence: happy vs unhappy ----
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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

###### with "direction" of dominance ----
# controlled/submissive/guided/... vs in control/ autonomous/controlling/... 
# i.e., somebody else's vs one's own dominance
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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2.pdf", 
       height = 6, width = 10)


##### ordinal models ----
###### data frame for ordinal models ----
d.proj2 = droplevels(subset(w2, w2$polarity == "negative" | 
                              w2$conditional2 == "conditional")) %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"),
         environment = case_when(polarity == "negative" & conditional == "False" ~ "neg",  
                                 polarity == "positive" & conditional == "True" ~ "q+cond",
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg")) %>% 
  distinct(verb_renamed, verb, voice, participant, predicateType, predicateType2, 
           veridicality, veridicality_num, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum, 
           V.Mean.Sum2, V.Mean.Sum2.direction, A.Mean.Sum2, D.Mean.Sum2,
           D.Mean.Sum2.direction, environment)

###### models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj2) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2   0.5727     0.1285   4.458 8.26e-06 ***

# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.0075     0.2059   4.894 9.87e-07 ***
# V.Mean.Sum2.directionpositive              -0.4242     0.0702  -6.042 1.52e-09 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.1213     0.2700  -0.449    0.653  

# with 'direction' of dominance
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * D.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.4720     0.2266   6.495 8.33e-11 ***
# D.Mean.Sum2.directionpositive              -0.2827     0.0708  -3.992 6.55e-05 ***
# D.Mean.Sum2:D.Mean.Sum2.directionpositive  -0.9371     0.2790  -3.359 0.000783 ***

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "cognitive") +  (1 | participant) + (1 | environment), 
     data = d.proj2) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       .
# negative  emotive         ***
# negative  emoComm         ***
# negative  evidential      ***
# negative  nonEmoComm      **
# positive  cognitive       **
# positive  emotive         n.s.
# positive  emoComm         *
# positive  evidential      ***
# positive  nonEmoComm      .
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# with direction of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "cognitive") +  (1 | participant) + (1 | environment), 
     data = d.proj2) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       n.s.
# negative  emotive         ***
# negative  emoComm         ***
# negative  evidential      ***
# negative  nonEmoComm      n.s.
# positive  cognitive       n.s.
# positive  emotive         n.s.
# positive  emoComm         n.s.
# positive  evidential      ***
# positive  nonEmoComm      **


# Acceptability ----
clmm(as.factor(veridicality_num) ~ acceptability + (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# acceptability   0.3414     0.0113   30.21   <2e-16 ***

mean.proj.acc <- d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability), 
            CILow.Acc = ci.low(acceptability), 
            CIHigh.Acc = ci.high(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.acc) # 544

# add predicateType etc. to the means
tmp3 <-  d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)
nrow(tmp3) # 544

mean.proj.acc <- left_join(mean.proj.acc, tmp3, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.acc) # 544

# remove "other" and "comPriv" predicates
mean.proj.acc <- mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other"))
nrow(mean.proj.acc) # 523

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(mean.proj.acc, aes(x = Mean.Acc, y = Mean.Proj)) +
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

clmm(as.factor(veridicality) ~ acceptability + (1 | participant) + (1 | environment), 
     data = d.proj) %>% 
  summary()
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.37328    0.01209   30.88   <2e-16 ***

# tables 
d.proj %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  group_by(acceptability) %>% 
  summarise(count = n())
#   acceptability count
#           <int> <int>
# 1             1   486
# 2             2   958
# 3             3  1468
# 4             4  1595
# 5             5  2819
# 6             6  3100
# 7             7  5236

o <- d.proj %>% 
  group_by(acceptability, veridicality) %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  count() %>% 
  group_by(acceptability) %>%
  mutate(percentage =  n * 100 / sum(n)) %>% 
  print(n = Inf)
#   acceptability veridicality     n percentage
#           <int> <chr>        <int>      <dbl>
# 1             1 maybe          339      69.8 
# 2             1 no              59      12.1 
# 3             1 yes             88      18.1 
# 4             2 maybe          663      69.2 
# 5             2 no              80       8.35
# 6             2 yes            215      22.4 
# 7             3 maybe          967      65.9 
# 8             3 no             129       8.79
# 9             3 yes            372      25.3 
# 10             4 maybe          992      62.2 
# 11             4 no             102       6.39
# 12             4 yes            501      31.4 
# 13             5 maybe         1440      51.1 
# 14             5 no             174       6.17
# 15             5 yes           1205      42.7 
# 16             6 maybe         1270      41.0 
# 17             6 no             145       4.68
# 18             6 yes           1685      54.4 
# 19             7 maybe         1717      32.8 
# 20             7 no             205       3.92
# 21             7 yes           3314      63.3 

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

# predicates with mean acceptability ratings of less than 4
mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other"), Mean.Acc < 4) %>% 
  arrange(Mean.Acc) %>% 
  reframe(verb_renamed, Mean.Acc, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
#   verb_renamed  Mean.Acc Mean.Proj predicateType2
#   <fct>            <dbl>     <dbl> <chr>         
# 1 be bet            2.55    0.0345 evidential    
# 2 grin              2.83    0.333  emoComm       
# 3 come out          2.97    0.367  nonEmoComm    
# 4 okay              3.03    0.333  nonEmoComm    
# 5 negotiate         3.1     0.2    nonEmoComm    
# 6 elect             3.33    0.167  evidential    
# 7 curse             3.37    0.267  emoComm       
# 8 bark              3.38    0.276  emoComm       
# 9 be tweeted        3.4     0.167  evidential    
# 10 measure           3.5     0.167  evidential    
# 11 snap              3.57    0.333  emoComm       
# 12 stutter           3.57    0.3    nonEmoComm    
# 13 be cheered        3.6     0.733  emotive       
# 14 growl             3.62    0.483  emoComm       
# 15 hush up           3.67    0.467  nonEmoComm    
# 16 manufacture       3.67    0.0333 nonEmoComm    
# 17 simulate          3.67    0.167  nonEmoComm    
# 18 giggle            3.7     0.3    emoComm       
# 19 hoot              3.7     0.4    emoComm       
# 20 volunteer         3.7     0.267  nonEmoComm    
# 21 be faxed          3.77    0.267  evidential    
# 22 diagnose          3.8     0.233  evidential    
# 23 be stimulated     3.83    0.724  emotive       
# 24 be nonplussed     3.83    0.433  emotive       
# 25 beam              3.83    0.3    emoComm       
# 26 cackle            3.83    0.433  emoComm       
# 27 categorize        3.83    0.367  evidential    
# 28 pity              3.83    0.667  emotive       
# 29 test              3.87    0.0667 nonEmoComm    
# 30 vote              3.9     0.2    nonEmoComm    
# 31 be educated       3.93    0.433  evidential    


# What proportion of predicates within each type has a mean acceptability rating 
# of less than 4?
count <- mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  group_by(predicateType2) %>% 
  summarise(total.count = n())
count2 <- mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other"), Mean.Acc < 4) %>% 
  group_by(predicateType2) %>% 
  summarise(low.acc.count = n())
left_join(count, count2, by = "predicateType2") %>% 
  mutate(low.acc.percentage = low.acc.count * 100 / total.count)
#   <chr>                <int>         <int>              <dbl>
# 1 cognitive               53            NA              NA   
# 2 emoComm                 47             9              19.1 
# 3 emotive                148             4               2.70
# 4 evidential              86             8               9.30
# 5 nonEmoComm             189            10               5.29



# CoS evidentials ----
# only CoS evidentials
## MegaVeridicality dataset ----
mean.proj.cosevi = mean.proj %>%
  filter(predicateType == "evidential" & changeOfState == "yes" | verb_renamed == "know")
nrow(mean.proj.cosevi) # 39

ggplot(mean.proj.cosevi, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1)) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-cos-evidential.pdf", height = 4, width = 13)

slice_max(filter(mean.proj.cosevi, verb_renamed != "know"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed   Mean.Proj
#   <fct>              <dbl>
# 1 realize            0.867
# 2 notice             0.733
# 3 figure out         0.633
# 4 recognize          0.633
# 5 find out           0.552
# 6 piece together     0.5  
# 7 rediscover         0.5  
# 8 be educated        0.433
# 9 deduce             0.4  
# 10 detect             0.4  
# 11 learn              0.4  

slice_min(filter(mean.proj.cosevi, verb_renamed != "know"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 derive         -0.0333
# 2 prejudge        0.0333
# 3 be persuaded    0.0667
# 4 conclude        0.0667
# 5 estimate        0.1   
# 6 infer           0.1   
# 7 decide          0.133 
# 8 find            0.133 
# 9 calculate       0.167 
# 10 elect           0.167 
# 11 measure         0.167 
# 12 resolve         0.167 
# 13 select          0.167 

### similar predicates + K&A (2024)  ----
# (only those discussed by Korotkova & Anand (2024) and similar CoS evidentials + know)
a <- mean.proj %>% 
  filter(verb_renamed %in% c("assess", "conclude", "deduce", "detect", "determine", 
                             "discover", "evaluate", "figure out", "find out", 
                             "gather", "identify", "infer", "learn", "notice", 
                             "piece together", "realize", "reason out", "reason", 
                             "recognize", "know")) %>% 
  droplevels()

# load predicate attribute data
b <-  read.csv("../data/predicates2.csv")
nrow(b) # 19

# add "verb_renamed" column
b <- b %>% mutate(verb_renamed = verb)

# combine dataframes
mean.proj.cosevi2 <-  left_join(a, b, by = c("verb_renamed")) %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.cosevi2) # 20
levels(mean.proj.cosevi2$verb_renamed)

# define x-axis label colours, highlighting the predicates in Korotkova & Anand (2024)
KA <- mean.proj.cosevi2 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "realize"

immi <- mean.proj.cosevi2 %>% 
  filter(imminency_reading == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "detect"         "discover"       "figure out"     "find out"       "identify"       "learn"         
# [7] "notice"         "piece together" "realize"        "recognize"     

cols <-  data.frame(predicate = levels(mean.proj.cosevi2$verb_renamed)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate == "know", "KNOW", 
                                            "X"))),
         imminency = as.factor(ifelse(predicate %in% immi, "immi", "non-immi")),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KNOW", "grey60",
                                  "black")),
         faces =  ifelse(imminency == "immi", "bold", "plain"))

label_colours <- cols %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

label_faces <- cols %>% 
  select(faces) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

# imminency reading
ggplot(mean.proj.cosevi2, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
 # geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_point() +
  geom_point(data = mean.proj.cosevi2 %>% filter(imminency_reading == "yes"), 
             shape = 1, size = 2, stroke = 1.5,
             aes(colour = "imminency reading")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Predicate property",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("green3","blue"))
ggsave("../graphs/projection-by-cos-evidential-imminency.pdf", height = 5, width = 13)

# volition
ggplot(mean.proj.cosevi2, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_point(data = mean.proj.cosevi2 %>% filter(volition_how == "yes"), 
             shape = 1, size = 2, stroke = 1.5,
             aes(colour = "volition with how-complement")) +
  geom_point(data = mean.proj.cosevi2 %>% filter(volition_that == "yes"), 
             shape = 1, size = 3.5, stroke = 1.5, 
             aes(colour = "volition with that-complement")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Predicate property",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).\nPredicates in bold are those with an imminency reading.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("green3","blue")) 
ggsave("../graphs/projection-by-cos-evidential-volition.pdf", height = 5, width = 13)

# compatibility with abduction with shorter and longer chain of evidence accumulation
ggplot(mean.proj.cosevi2, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_abduction_effect_to_cause == "yes"), 
             shape = 1, size = 2, stroke = 1.5,
             aes(colour = "compatible with abductive reasoning\nwith few pieces of evidence")) +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_abduction_effect_to_cause_longer_chain == "yes"), 
             shape = 1, size = 3.5, stroke = 1.5, 
             aes(colour = "compatible with abductive reasoning\nwith many pieces of evidence")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Predicate property",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).\nPredicates in bold are those with an imminency reading.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("gold3","purple")) 
ggsave("../graphs/projection-by-cos-evidential-abductive-evidence.pdf", height = 5, width = 13)

# compatibility with different types of evidence
ggplot(mean.proj.cosevi2, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_abduction_effect_to_cause == "yes"), 
             shape = 1, size = 2, stroke = 1.5,
             aes(colour = "abductive reasoning\nwith few pieces of evidence")) +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_abduction_effect_to_cause_longer_chain == "yes"), 
             shape = 1, size = 3.5, stroke = 1.5, 
             aes(colour = "abductive reasoning\nwith many pieces of evidence")) +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_perceptual == "yes"), 
             shape = 1, size = 5, stroke = 1.5, 
             aes(colour = "perceptual evidence")) +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_hearsay == "yes"), 
             shape = 1, size = 6.5, stroke = 1.5, 
             aes(colour = "hearsay evidence")) +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_anti_abduction_general_knowledge == "yes"), 
             shape = 1, size = 8, stroke = 1.5, 
             aes(colour = "anti-abductive reasoning\nbased on general knowledge")) +
  geom_point(data = mean.proj.cosevi2 %>% 
               filter(evidence_compatibility_anti_abduction_eliminative_reasoning == "yes"), 
             shape = 1, size = 9.5, stroke = 1.5, 
             aes(colour = "anti-abductive\neliminative reasoning")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 11, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Compatible evidence types",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).\nPredicates in bold are those with an imminency reading.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("gold","purple", "blue","red",  "darkorange2", "green2")) 
#  guides(colour = guide_legend(nrow = 2, byrow = TRUE, ncol = 4))
ggsave("../graphs/projection-by-cos-evidential-types-of-evidence.pdf", height = 5, width = 13)

### more K&A (2024) predicates ----
# "more coming-to-know predicates" (as presented by Dr Korotkova) + know
c <- mean.proj %>% 
  filter(verb_renamed %in% c("comprehend", "confirm", "discover", "establish", 
                             "figure out", "find out", "foresee", "foretell", 
                             "learn", "note", "notice", "observe", "perceive", 
                             "realize", "recognize", "reveal", "understand", 
                             "verify", "know")) %>% 
  droplevels()

# define x-axis label colours, highlighting the main predicates in Korotkova & Anand (2024)
KA <- mean.proj.cosevi2 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "realize"

cols3 <-  data.frame(predicate = levels(c$verb_renamed)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate == "know", "KNOW", 
                                            "X"))),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KNOW", "grey60",
                                  "black")))

label_colours3 <- cols3 %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

# plot
ggplot(c, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  # geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_point() +
  # geom_point(data = mean.proj.cosevi2 %>% filter(imminency_reading == "yes"), 
  #            shape = 1, size = 2, stroke = 1.5,
  #            aes(colour = "imminency reading")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours3),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = '"Coming-to-know predicate"',
       y = "Mean projection rating", 
       colour = "Predicate property",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-coming-to-know-predicates.pdf", height = 5, width = 13)


### all CoS evidentials + K&A (2024) ----
# all CoS evidentials + K&A predicates + know
g <- mean.proj %>% 
  filter(predicateType2 == "evidential" & changeOfState == "yes" | verb_renamed == "know") %>% 
  droplevels()
nrow(g) # 39

# load predicate attribute data
b <-  read.csv("../data/predicates2.csv")
nrow(b) # 19

# add "verb_renamed" column
b <- b %>% mutate(verb_renamed = verb)

# combine dataframes
mean.proj.evi <-  left_join(g, b, by = c("verb_renamed")) %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.evi) # 39
levels(mean.proj.evi$verb_renamed)

# define x-axis label colours, highlighting the main predicates in Korotkova & Anand (2024)
KAlist <- mean.proj.evi %>% 
  filter(verb_renamed %in% c("comprehend", "confirm", "discover", "establish", 
                             "figure out", "find out", "foresee", "foretell", 
                             "learn", "note", "notice", "observe", "perceive", 
                             "realize", "recognize", "reveal", "understand", 
                             "verify")) %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "realize"    "recognize" 

immi3 <- mean.proj.evi %>% 
  filter(imminency_reading == "yes" | verb_renamed %in% KAlist) %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "detect"         "discover"       "figure out"     "find out"       "identify"       "learn"         
# [7] "notice"         "piece together" "realize"        "recognize"   
immi3
cols <-  data.frame(predicate = levels(mean.proj.evi$verb_renamed)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate %in% KAlist, "KAlist",
                                     ifelse(predicate == "know", "KNOW", 
                                            "X")))),
         imminency = as.factor(ifelse(predicate %in% immi3, "immi", "non-immi")),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KAlist", "orange3", 
                                  ifelse(category == "KNOW", "grey60", "black"))),
         faces =  ifelse(imminency == "immi", "bold", "plain"))

label_colours <- cols %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

label_faces <- cols %>% 
  select(faces) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

# plot
ggplot(mean.proj.evi, aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  # geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_point() +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential change-of-state predicate",
       y = "Mean projection rating", 
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024), predicates in orange are those listed as 'more coming-to-know predicates'. Predicates in bold have an imminency reading.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-cos-evidential-KA.pdf", height = 5, width = 13)


#### means ----
d.proj2 = d.proj %>%
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
         changeOfState = case_when(change_of_state_predicate == "yes" ~ "yes",
                                   TRUE ~ "no"),
         imminency = case_when(verb_renamed %in% immi3 ~ "yes", 
                               TRUE ~ "no"))

# How many CoS evidentials with/without imminency reading? - everything in order.
d.proj2 %>% 
  filter(predicateType2 == "evidential" & changeOfState == "yes") %>%  
  group_by(imminency) %>% 
  distinct(verb_renamed) %>% 
  count()
#   imminency     n
#   <chr>     <int>
# 1 no           28
# 2 yes          10

mean.proj.cosevi5 <- d.proj2 %>%
  filter(predicateType2 == "evidential" & changeOfState == "yes") %>% 
  group_by(imminency) %>% 
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.cosevi5) # 2

ggplot(mean.proj.cosevi5, aes(x = imminency, y = Mean.Proj)) +
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
  labs(x = "Change-of-state evidential predicate",
       y = "Mean projection rating") + 
  scale_x_discrete(labels = c("imminency reading\nnot available\n(n = 28)",
                              "'coming-to-know predicate':\nimminency reading available\n(n = 10)")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))
ggsave("../graphs/projection-by-cos-evidential-means.pdf", height = 4, width = 10)



### all evidentials + K&A (2024) ----
# + know
h <- mean.proj %>% 
  filter(predicateType2 == "evidential" | verb_renamed == "know") %>% 
  droplevels()
nrow(h) # 87

# load predicate attribute data
b <-  read.csv("../data/predicates2.csv")
nrow(b) # 19

# add "verb_renamed" column
b <- b %>% mutate(verb_renamed = verb)

# combine dataframes
mean.proj.evi2 <-  left_join(h, b, by = c("verb_renamed")) %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.evi2) # 87
levels(mean.proj.evi2$verb_renamed)

# define x-axis label colours, highlighting the predicates in Korotkova & Anand (2024)
KA <- mean.proj.evi2 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "realize"

# define x-axis label colours, highlighting the main predicates in Korotkova & Anand (2024)
KAlist <- mean.proj.evi2 %>% 
  filter(verb_renamed %in% c("comprehend", "confirm", "discover", "establish", 
                             "figure out", "find out", "foresee", "foretell", 
                             "learn", "note", "notice", "observe", "perceive", 
                             "realize", "recognize", "reveal", "understand", 
                             "verify")) %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "observe"    "perceive"   "realize"   
# [9] "recognize" 

immi4 <- mean.proj.evi2 %>% 
  filter(imminency_reading == "yes" | verb_renamed %in% KAlist) %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "detect"         "discover"       "figure out"     "find out"       "identify"       "learn"         
# [7] "notice"         "observe"        "perceive"       "piece together" "realize"        "recognize"    

cols <-  data.frame(predicate = levels(mean.proj.evi2$verb_renamed)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate %in% KAlist, "KAlist",
                                            ifelse(predicate == "know", "KNOW", 
                                                   "X")))),
         imminency = as.factor(ifelse(predicate %in% immi3, "immi", "non-immi")),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KAlist", "orange3", 
                                  ifelse(category == "KNOW", "grey60", "black"))),
         faces =  ifelse(imminency == "immi", "bold", "plain"))

label_colours <- cols %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

label_faces <- cols %>% 
  select(faces) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

# plot
ggplot(mean.proj.evi2, aes(x = verb_renamed, y = Mean.Proj, colour = changeOfState)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  # geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_point() +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Change of state",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024), predicates in orange are those listed as 'more coming-to-know predicates'. Predicates in bold have an imminency reading.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("purple","green4"))
ggsave("../graphs/projection-by-evidential-KA.pdf", height = 5, width = 13)


## VerbVeridicality dataset ----
z <-  read.csv("../data/verb-veridicality.csv")
nrow(z) # 1498

# separate projection ratings (negative environment ratings) into separate columns. 
z <- z %>% separate(turker_neg_ratings, c("rating1", "rating2", "rating3"), 
                    sep = ",")

# select only items with that-complements
z <- z %>% 
  subset(z$task == "that") %>% 
  pivot_longer(
    cols = c("rating1", "rating2", "rating3"), 
    names_to = "rating_number", 
    values_to = "rating")
nrow(z) # 2577

# "rating.rescaled" for easier comparison with MV dataset whose ratings we have
# recoded to numerical values ranging from -1 to 1.
z <- z %>% mutate(rating = as.numeric(as.character(z$rating)),
                  rating.rescaled = rating/2)

# calculate by-predicate projection means for original and rescaled ratings.
mean.proj.vv = z %>%
  group_by(verb) %>% 
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         verb = fct_reorder(as.factor(verb), Mean.Proj))


### similar predicates + K&A (2024) ----
# only those discussed by Korotkova & Anand (2024) and similar CoS evidentials + know

# combine dataframes
mean.proj.vv2 <-  left_join(mean.proj.vv, b, by = c("verb")) %>% 
  mutate(verb = fct_reorder(as.factor(verb), Mean.Proj)) %>% 
  filter(verb %in% a$verb_renamed) %>% 
  droplevels()
nrow(mean.proj.vv2) # 8
levels(mean.proj.vv2$verb) 
# [1] "conclude"  "determine" "discover"  "recognize" "learn"     "know"      "realize"   "notice"   

# define x-axis label colours, highlighting the predicates in Korotkova & Anand (2024)
# and those predicates with an imminency reading.
KA2 <- mean.proj.vv2 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover" "learn"    "notice"   "realize" 

immi2 <- mean.proj.vv2 %>% 
  filter(imminency_reading == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"  "learn"     "notice"    "realize"   "recognize"    

cols2 <-  data.frame(predicate = levels(mean.proj.vv2$verb)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate == "know", "KNOW", 
                                            "X"))),
         imminency = as.factor(ifelse(predicate %in% immi, "immi", "non-immi")),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KNOW", "grey60",
                                  "black")),
         faces =  ifelse(imminency == "immi", "bold", "plain"))

label_colours2 <- cols2 %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

label_faces2 <- cols2 %>% 
  select(faces) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

# imminency reading
ggplot(mean.proj.vv2, aes(x = verb, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  # geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_point() +
  geom_point(data = mean.proj.vv2 %>% filter(imminency_reading == "yes"), 
             shape = 1, size = 2, stroke = 1.5,
             aes(colour = "imminency reading")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours2, face = label_faces2),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Predicate property",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).") + 
  scale_y_continuous(limits = c(-2, 2), breaks = c(-2, -1, 0, 1, 2), minor_breaks = FALSE) +
  scale_colour_manual(values = c("green3"))
ggsave("../graphs/projection-by-cos-evidential-imminency-vv.pdf", height = 5, width = 13)

### more K&A (2024) predicates ----
# "more coming-to-know predicates" (as presented by Dr Korotkova) + know

# select predicates
mean.proj.vv3 <- mean.proj.vv %>% 
  mutate(verb = fct_reorder(as.factor(verb), Mean.Proj)) %>%
  filter(verb %in% c$verb_renamed) %>%
  droplevels()
nrow(mean.proj.vv3) # 11
levels(mean.proj.vv3$verb)
# [1] "reveal"     "confirm"    "note"       "observe"    "discover"   "recognize"  "learn"      "know"      
# [9] "understand" "realize"    "notice"  

# define x-axis label colours, highlighting the predicates in Korotkova & Anand (2024)
# and those predicates with an imminency reading.
KA2 <- mean.proj.vv2 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover" "learn"    "notice"   "realize" 

cols4 <-  data.frame(predicate = levels(mean.proj.vv3$verb)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate == "know", "KNOW", 
                                            "X"))),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KNOW", "grey60",
                                  "black")))

label_colours4 <- cols4 %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

# plot
ggplot(mean.proj.vv3, aes(x = verb, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  # geom_errorbar(aes(ymin = YMin.Proj, ymax = YMax.Proj), width = 0) +
  geom_point() +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours4),
        plot.caption = element_text(hjust = 0, vjust = -6, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = '"Coming-to-know predicate"',
       y = "Mean projection rating", 
       colour = "Predicate property",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).") + 
  scale_y_continuous(limits = c(-2, 2), breaks = c(-2, -1, 0, 1, 2), minor_breaks = FALSE)
ggsave("../graphs/projection-by-coming-to-know-predicates-vv.pdf", height = 5, width = 13)


## MegaVeridicality + VerbVeridicality  ----
### similar predicates + K&A (2024) ----
# only those discussed by Korotkova & Anand (2024) and similar CoS evidentials + know
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.cosevi2, 
             aes(x = verb_renamed, y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = mean.proj.cosevi2, 
                aes(x = verb_renamed, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = -0.07)) +
  geom_point(data = mean.proj.vv2, 
             aes(x = verb, y = Mean.Proj.rescaled, 
                 colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.vv2, 
                aes(x = verb, y = Mean.Proj.rescaled, 
                    ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled, 
                    colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -8, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate",
       y = "Mean projection rating", 
       colour = "Dataset",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024); predicates in bold are those with an imminency reading. 
       \nRating scales:
       White and Rawlins's (2018) participants answered the question 'Did that thing happen?' (CC: 'a particular thing happened') by choosing 'no', 'maybe or maybe not' or 'yes'. 
       These ratings were recoded as -1, 0 and 1, respectively.
       Ross and Pavlick's (2019) participants provided ratings on a 5-point Likert scale ranging from -2 (the CC is 'definitely not true') to 2 ('definitely true'). 
       These ratings were rescaled to range from -1 to 1.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("deepskyblue","chocolate"))
ggsave("../graphs/projection-by-cos-evidential-mv-vv.pdf", height = 7, width = 13)

#### negative embedding environment only ----
# Since the VV dataset only contains projection ratings for predicates under negation, 
# the two datasets can be compared more easily when only considering this environment 
# in the MV dataset as well.

# calculate by-predicate projection means only for negative environment
mean.proj2 = d.proj %>%
  group_by(verb_renamed, environment) %>%
  filter(environment == "neg") %>% 
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj)) %>% 
  ungroup()
nrow(mean.proj2) # 544

e <- mean.proj2 %>% 
  filter(verb_renamed %in% c("assess", "conclude", "deduce", "detect", "determine", 
                             "discover", "evaluate", "figure out", "find out", 
                             "gather", "identify", "infer", "learn", "notice", 
                             "piece together", "realize", "reason out", "reason", 
                             "recognize", "know")) %>% 
  droplevels()

# load predicate attribute data
b <-  read.csv("../data/predicates2.csv")
nrow(b) # 19

# add "verb_renamed" column
b <- b %>% mutate(verb_renamed = verb)

# combine dataframes
mean.proj.cosevi3 <-  left_join(e, b, by = c("verb_renamed")) %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.cosevi3) # 20
levels(mean.proj.cosevi3$verb_renamed)

KA <- mean.proj.cosevi3 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "realize"

immi <- mean.proj.cosevi3 %>% 
  filter(imminency_reading == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "detect"         "discover"       "figure out"     "find out"       "identify"       "learn"         
# [7] "notice"         "piece together" "realize"        "recognize"     

cols <-  data.frame(predicate = levels(mean.proj.cosevi3$verb_renamed)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate == "know", "KNOW", 
                                            "X"))),
         imminency = as.factor(ifelse(predicate %in% immi, "immi", "non-immi")),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KNOW", "grey60",
                                  "black")),
         faces =  ifelse(imminency == "immi", "bold", "plain"))

label_colours <- cols %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

label_faces <- cols %>% 
  select(faces) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj.cosevi3, 
             aes(x = verb_renamed, y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = mean.proj.cosevi3, 
                aes(x = verb_renamed, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = -0.07)) +
  geom_point(data = mean.proj.vv2, 
             aes(x = verb, y = Mean.Proj.rescaled, 
                 colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.vv2, 
                aes(x = verb, y = Mean.Proj.rescaled, 
                    ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled, 
                    colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours, face = label_faces),
        plot.caption = element_text(hjust = 0, vjust = -8, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = "Evidential predicate embedded under negation",
       y = "Mean projection rating", 
       colour = "Dataset",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024); predicates in bold are those with an imminency reading. 
       \nRating scales:
       White and Rawlins's (2018) participants answered the question 'Did that thing happen?' (CC: 'a particular thing happened') by choosing 'no', 'maybe or maybe not' or 'yes'. 
       These ratings were recoded as -1, 0 and 1, respectively.
       Ross and Pavlick's (2019) participants provided ratings on a 5-point Likert scale ranging from -2 (the CC is 'definitely not true') to 2 ('definitely true'). 
       These ratings were rescaled to range from -1 to 1.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("deepskyblue","chocolate"))
ggsave("../graphs/projection-by-cos-evidential-neg-only-mv-vv.pdf", height = 7, width = 13)

### more K&A (2024) predicates ----
# "more coming-to-know predicates" (as presented by Dr Korotkova) + know
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = c, 
             aes(x = verb_renamed, y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = c, 
                aes(x = verb_renamed, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = -0.07)) +
  geom_point(data = mean.proj.vv3, 
             aes(x = verb, y = Mean.Proj.rescaled, 
                 colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.vv3, 
                aes(x = verb, y = Mean.Proj.rescaled, 
                    ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled, 
                    colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours3),
        plot.caption = element_text(hjust = 0, vjust = -8, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = '"Coming-to-know predicate"',
       y = "Mean projection rating", 
       colour = "Dataset",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024). 
       \nRating scales:
       White and Rawlins's (2018) participants answered the question 'Did that thing happen?' (CC: 'a particular thing happened') by choosing 'no', 'maybe or maybe not' or 'yes'. 
       These ratings were recoded as -1, 0 and 1, respectively.
       Ross and Pavlick's (2019) participants provided ratings on a 5-point Likert scale ranging from -2 (the CC is 'definitely not true') to 2 ('definitely true'). 
       These ratings were rescaled to range from -1 to 1.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("deepskyblue","chocolate"))
ggsave("../graphs/projection-by-coming-to-know-predicates-mv-vv.pdf", height = 7, width = 13)

#### negative embedding environment only ----
# negative embedding environment only
f <- mean.proj2 %>% 
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj)) %>% 
  filter(verb_renamed %in% c("comprehend", "confirm", "discover", "establish", 
                             "figure out", "find out", "foresee", "foretell", 
                             "learn", "note", "notice", "observe", "perceive", 
                             "realize", "recognize", "reveal", "understand", 
                             "verify", "know")) %>% 
  droplevels() 

# define x-axis label colours, highlighting the main predicates in Korotkova & Anand (2024)
KA <- mean.proj.cosevi2 %>% 
  filter(K_A_original == "yes") %>% 
  select(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "discover"   "figure out" "find out"   "learn"      "notice"     "realize"

cols5 <-  data.frame(predicate = levels(f$verb_renamed)) %>% 
  mutate(category = as.factor(ifelse(predicate %in% KA, "KA", 
                                     ifelse(predicate == "know", "KNOW", 
                                            "X"))),
         colours =  ifelse(category == "KA", "deeppink", 
                           ifelse(category == "KNOW", "grey60",
                                  "black")))

label_colours5 <- cols5 %>% 
  select(colours) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = f, 
             aes(x = verb_renamed, y = Mean.Proj, 
                 colour = "MegaVeridicality (White & Rawlins 2018)"), 
             position = position_nudge(x = -0.07)) +
  geom_errorbar(data = f, 
                aes(x = verb_renamed, y = Mean.Proj, 
                    ymin = YMin.Proj, ymax = YMax.Proj, 
                    colour = "MegaVeridicality (White & Rawlins 2018)"), 
                width = 0, position = position_nudge(x = -0.07)) +
  geom_point(data = mean.proj.vv3, 
             aes(x = verb, y = Mean.Proj.rescaled, 
                 colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data =  mean.proj.vv3, 
                aes(x = verb, y = Mean.Proj.rescaled, 
                    ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled, 
                    colour = "VerbVeridicality (Ross & Pavlick 2019)"), 
                width = 0, position = position_nudge(x = 0.07)) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1, 
                                   colour = label_colours5),
        plot.caption = element_text(hjust = 0, vjust = -8, size = 11),
        plot.margin = unit(c(5.5, 5.5, 22, 5.5), "pt")) +
  labs(x = '"Coming-to-know predicate" embedded under negation',
       y = "Mean projection rating", 
       colour = "Dataset",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024). 
       \nRating scales:
       White and Rawlins's (2018) participants answered the question 'Did that thing happen?' (CC: 'a particular thing happened') by choosing 'no', 'maybe or maybe not' or 'yes'. 
       These ratings were recoded as -1, 0 and 1, respectively.
       Ross and Pavlick's (2019) participants provided ratings on a 5-point Likert scale ranging from -2 (the CC is 'definitely not true') to 2 ('definitely true'). 
       These ratings were rescaled to range from -1 to 1.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("deepskyblue","chocolate"))
ggsave("../graphs/projection-by-coming-to-know-predicates-neg-only-mv-vv.pdf", height = 7, width = 13)


# Volition ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
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

d.proj.comm <- d.proj %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 7069

# all predicates
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.28590    0.01755  16.286   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.20682    0.02257   9.162   <2e-16 ***

# only communicatives
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                         0.2850     0.0126  22.616  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional   0.1385     0.0310   4.466 1.24e-05 ***

clmm(as.factor(veridicality_num) ~ volition + (1 | participant) +
       (1 | environment), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
# volitionvolitional -0.54377    0.07187  -7.567 3.83e-14 ***

clmm(as.factor(veridicality_num) ~ volition * fct_relevel(predicateType2, "emoComm")+ 
       (1 | participant) + (1 | environment), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                                     Estimate Std. Error z value Pr(>|z|)    
# volitionvolitional                                                   -0.5145     0.1198  -4.296 1.74e-05 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm                     -0.7017     0.1361  -5.156 2.52e-07 ***
# volitionvolitional:fct_relevel(predicateType2, "emoComm")nonEmoComm   0.3738     0.1636   2.284   0.0224 *  

# faceted by predicate type
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

mean.proj %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
ggplot(aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
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

mean.proj %>% 
  filter(predicateType2 %in% c("emoComm", "nonEmoComm")) %>% 
  group_by(predicateType2, volition) %>% 
  count()
#   predicateType2 volition           n
#   <chr>          <chr>          <int>
# 1 emoComm        non-volitional    25
# 2 emoComm        volitional        22
# 3 nonEmoComm     non-volitional    14
# 4 nonEmoComm     volitional       175


### by predicate type ----
#### distribution ----
# how many predicates of which predicateType are non-/volitional?
d.proj %>%
  select(predicateType2, verb_renamed, volition) %>%
  unique() %>%
  group_by(predicateType2, volition) %>%
  summarize(count=n())

# predicateType2 volition       count
# <chr>          <chr>          <int>
# 1 cognitive      non-volitional    44
# 2 cognitive      volitional         9
# 3 comPriv        non-volitional     5
# 4 comPriv        volitional         4
# 5 emoComm        non-volitional    25
# 6 emoComm        volitional        22
# 7 emotive        non-volitional   148 
# 8 evidential     non-volitional    83 
# 9 evidential     volitional         3 
# 10 nonEmoComm     non-volitional    14
# 11 nonEmoComm     volitional       175
# 12 other          non-volitional    10
# 13 other          volitional         2

# All emotives are non-volitional. Almost all evidentials are non-volitional.

mean.proj.vol = d.proj %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj.vol
nrow(mean.proj.vol) # 13

# remove "other" and "comPriv" predicates
mean.proj.vol = mean.proj.vol %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.vol) # 9

#### plot ----
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
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("volitional" = "green3", "non-volitional" = "red"))
ggsave("../graphs/projection-by-predicateType-and-volition.pdf", height = 4, width = 10)

# Cognitives ----
# Why do the cognitives behave different from other predicates in several respects?
mean.proj %>%
  filter(predicateType2 == "cognitive") %>%
  select(verb_renamed, Mean.Proj, CILow, CIHigh) %>% 
  slice_max(Mean.Proj, n = 10) 
# verb_renamed Mean.Proj CILow CIHigh
# <fct>            <dbl> <dbl>  <dbl>
# 1 forget           0.867 0.133  0.1  
# 2 forgive          0.833 0.133  0.133
# 3 appreciate       0.767 0.2    0.167
# 4 comprehend       0.733 0.167  0.167
# 5 ignore           0.733 0.167  0.167
# 6 miss             0.7   0.2    0.167
# 7 know             0.633 0.167  0.167 <== not as high as one might expect.
# 8 understand       0.567 0.2    0.167
# 9 disregard        0.517 0.172  0.172
# 10 agonize          0.5   0.201  0.2  
# 11 foresee          0.5   0.2    0.167
# 12 repress          0.5   0.233  0.2  




# Overall ----
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

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

### with imminency ----

# all predicates with imminency readings as listed by K&A (2024) and identified by us
# ("identify", "detect" and "piece together") 
mean.proj.immi <- mean.proj %>% 
  filter(verb_renamed %in% c("comprehend", "confirm", "detect", "discover", 
                             "establish", "figure out", "find out", "foresee", 
                             "foretell", "identify", "learn", "note", "notice", 
                             "observe", "perceive", "piece together", "realize", 
                             "recognize", "reveal", "understand", "verify")) 
nrow(mean.proj.immi) # 21

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.immi, aes(x = verb_renamed, y = Mean.Proj, colour = "coming-to-know predicate"), 
             size = 2.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) + 
  scale_colour_manual(limits = c("other", "coming-to-know predicate", "know"),
    values = c("grey20", "green3", "red"))
ggsave("../graphs/projection-by-predicate-imminency.pdf", height = 4, width = 13)

### with emoComms ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "communicative with emotive component")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "communicative with emotive component", "know"),
                      values = c("grey50", "blue", "red"))
ggsave("../graphs/projection-by-predicate-emoComm.pdf", height = 4, width = 13)



# >>> NEGATION ONLY ----------------------------------------------------------------------------
# copy of code above with only negation as embedding environment. 

# Testing hypotheses based on White & Rawlins' MegaVeridicality I dataset and Ross & Pavlick's 
# VerbVeridicality dataset.
# analysis2.R

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

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

# create dataset for projection inferences
d.proj = droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

# create predicateType, emotiveComponent, change-of-state etc. columns
d.proj = d.proj %>%
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
  group_by(polarity, conditional2) %>% 
  count()
#   polarity conditional2     n
#   <chr>    <chr>        <int>
# 1 negative matrix        5411

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

# add predicateType etc. to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544


# Communicatives ----
# only communicatives
mean.proj.comm = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj.comm) # 236

## overall ----
# There is no way to make this a reasonably nice plot with labels for all 236 predicates.
# # All communicatives with labels for the most and least projective communicatives.
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
ggsave("../graphs/projection-by-communicative-NO.pdf", height = 4, width = 13)

slice_max(mean.proj.comm, Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 apologize        1    
# 2 fess up          0.9  
# 3 disclose         0.8  
# 4 flip out         0.8  
# 5 warn             0.778
# 6 cry              0.7  
# 7 flaunt           0.7  
# 8 grimace          0.7  
# 9 log              0.7  
# 10 point out        0.7  
# 11 weep             0.7  
# 12 whine            0.7 

slice_min(mean.proj.comm, Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 demonstrate     -0.444
# 2 decree          -0.3  
# 3 recap           -0.3  
# 4 signify         -0.3  
# 5 suggest         -0.3  
# 6 prove           -0.222
# 7 confide         -0.2  
# 8 holler          -0.2  
# 9 imply           -0.2  
# 10 indict          -0.2  
# 11 reply           -0.2  
# 12 respond         -0.2  
# 13 retort          -0.2  
# 14 rule            -0.2  
# 15 verify          -0.2  

# How many of which type of communicative predicate?
mean.proj.comm %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  summarize(count = n())
#   commType                count
#   <chr>                   <int>
# 1 discourse participation   114
# 2 pure                       95
# 3 state changing             27

## "pure" communicatives ----
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
ggsave("../graphs/projection-by-communicative-minmax-pure-NO.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "pure"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 flip out         0.8  
# 2 cry              0.7  
# 3 grimace          0.7  
# 4 log              0.7  
# 5 weep             0.7  
# 6 whine            0.7  
# 7 publicize        0.667
# 8 cringe           0.6  
# 9 document         0.6  
# 10 grin             0.6  
# 11 groan            0.6  

slice_min(subset(mean.proj.comm, commType == "pure"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 holler            -0.2
# 2 exclaim           -0.1
# 3 express           -0.1
# 4 fax               -0.1
# 5 giggle            -0.1
# 6 post              -0.1
# 7 chant              0  
# 8 curse              0  
# 9 email              0  
# 10 mumble             0  
# 11 mutter             0  
# 12 quip               0  
# 13 scream             0  
# 14 scribble           0  
# 15 sing               0  
# 16 snap               0  
# 17 state              0  
# 18 tweet              0  
# 19 type               0  
# 20 will               0  
# 21 yell               0 

## "discourse participation" communicatives ----
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
ggsave("../graphs/projection-by-communicative-minmax-dp-NO.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "discourse participation"), 
          Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 apologize          1  
# 2 fess up            0.9
# 3 disclose           0.8
# 4 flaunt             0.7
# 5 point out          0.7
# 6 announce           0.6
# 7 detail             0.6
# 8 divulge            0.6
# 9 leak               0.6
# 10 question           0.6
# 11 reiterate          0.6

slice_min(subset(mean.proj.comm, commType == "discourse participation"), 
          Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 decree            -0.3
# 2 recap             -0.3
# 3 signify           -0.3
# 4 suggest           -0.3
# 5 confide           -0.2
# 6 imply             -0.2
# 7 indict            -0.2
# 8 reply             -0.2
# 9 respond           -0.2
# 10 retort            -0.2
# 11 rule              -0.2
# 12 verify            -0.2

# "discourse participation communicatives" (our classification) - "discourse role verbs" (Grimshaw 2015)
mean.proj %>% 
  filter((! predicateType2 %in% c("comPriv", "other")) & 
           (commType == "discourse participation" | sayVerbType == "discourse role verb")) %>% 
  group_by(predicateType2, commType, sayVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType             n
#   <chr>          <chr>                   <chr>               <int>
# 1 emoComm        discourse participation mode verb               4 - A
# 2 emoComm        discourse participation NA                      3
# 3 nonEmoComm     discourse participation discourse role verb    47
# 4 nonEmoComm     discourse participation mode verb               1 - B
# 5 nonEmoComm     discourse participation NA                     59
# 6 nonEmoComm     pure                    discourse role verb     5 - C
# 7 nonEmoComm     state changing          discourse role verb     5 - D

# A
mean.proj %>% 
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
mean.proj %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "discourse participation" &
           sayVerbType == "mode verb") %>% 
  select(verb_renamed, modeVerbType)
#   verb_renamed modeVerbType     
#   <fct>        <chr>            
# 1 tease        say-with-attitude

# C
mean.proj %>% 
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
mean.proj %>% 
  filter(predicateType2 == "nonEmoComm" & commType == "state changing" &
           sayVerbType == "discourse role verb") %>% 
  select(verb_renamed)
# 1 insist      
# 2 lie         
# 3 promise     
# 4 swear       
# 5 vow 


## "state changing" communicatives ----
# All communicatives with labels for all 27 state changing communicatives.
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
ggsave("../graphs/projection-by-communicative-sc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-minmax-sc-NO.pdf", height = 4, width = 13)

slice_max(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 warn             0.778
# 2 conceal          0.6  
# 3 hush up          0.5  
# 4 lie              0.5  
# 5 certify          0.3  
# 6 expose           0.3  
# 7 fake             0.3  
# 8 negotiate        0.3  
# 9 simulate         0.3  
# 10 fabricate        0.2  
# 11 feign            0.2  
# 12 implore          0.2  
# 13 manufacture      0.2  
# 14 showcase         0.2  

slice_min(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 demonstrate     -0.444
# 2 prove           -0.222
# 3 advertise       -0.1  
# 4 guarantee       -0.1  
# 5 test            -0.1  
# 6 warrant         -0.1  
# 7 insist           0    
# 8 pretend          0    
# 9 swear            0    
# 10 advise           0.1  
# 11 generalize       0.1  
# 12 promise          0.1  
# 13 vow              0.1  

## say verbs ----
# distribution of communication predicates
d.proj %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(sayVerb, predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb predicateType     n
#   <chr>   <chr>         <int>
# 1 no      communicative   116
# 2 yes     cognitive         2
# 3 yes     comPriv           2
# 4 yes     communicative   120

d.proj %>% 
  filter((predicateType != "communicative" & predicateType != "comPriv") & sayVerb == "yes") %>% 
  distinct(verb_renamed)
# verb_renamed
# 1        think
# 2         pray
# These predicates "report internal linguistic formulation only" (Grimshaw 2015: 84).

# There are 240 communication predicates in the MV dataset. Two of them are say-
# verbs but not communicatives. Two are predicates can be interpreted both as 
# communicative and private predicates. These four predicates are excluded from
# analysis, i.e., only the 236 communicative predicates are included.

### types of communicatives ----
d.proj %>% 
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
# sayVerb     n
# <chr>   <int>
# 1 no        116
# 2 yes       120
# Of the 236 communicatives in the MV dataset, 120 are say-predicates.

mean.proj.commsay <- d.proj %>%
  filter(predicateType == "communicative") %>% 
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
ggsave("../graphs/projection-by-communication-type-NO.pdf", height = 4, width = 10)

### types of say verbs ----
d.proj %>% 
  filter((sayVerb == "yes" & predicateType == "communicative")) %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
# sayVerbType             n
# <chr>               <int>
# 1 discourse role verb    57
# 2 mode verb              62
# 3 say                     1

mean.proj.saytype <- d.proj %>%
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
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
ggsave("../graphs/projection-by-sayverb-type-NO.pdf", height = 4, width = 10)

mean.proj.saytype2 <- d.proj %>%
  filter(predicateType == "communicative") %>% 
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
ggsave("../graphs/projection-by-sayverb-type2-NO.pdf", height = 4, width = 10)

lm(Mean.Proj ~ fct_relevel(sayVerbType, "mode verb"), data = mean.proj) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   ***
# mode verb             ***
# say                   -

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "say") + 
       (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.45698    0.14080   3.246  0.00117 **
# fct_relevel(sayVerbType, "discourse role verb")say       -0.08259    0.76272  -0.108  0.91377 

# fct_relevel(sayVerbType, "mode verb")discourse role verb  -0.4570     0.1408  -3.246  0.00117 **
# fct_relevel(sayVerbType, "mode verb")say                  -0.5396     0.7615  -0.709  0.47858  

# fct_relevel(sayVerbType, "say")discourse role verb  0.08259    0.76253   0.108    0.914
# fct_relevel(sayVerbType, "say")mode verb            0.53957    0.76131   0.709    0.478

### types of mode verbs ----
d.proj %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means         48
# 2 say-with-attitude    14

mean.proj.modetype <- d.proj %>%
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
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
ggsave("../graphs/projection-by-modeverb-type-NO.pdf", height = 4, width = 10)

clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-with-attitude") + 
       (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude   0.4025     0.2338   1.721   0.0852 .
# fct_relevel(modeVerbType, "say-with-attitude")say-by-means  -0.4025     0.2338  -1.721   0.0852 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### types of say-by-means verbs ----
d.proj %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                  14
# 2 manner                26
# 3 sound                  8

mean.proj.bymeanstype <- d.proj %>%
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
  scale_colour_manual(values = c("red", "cyan3", "darkgoldenrod2", "grey50"))
ggsave("../graphs/projection-by-saybymeansverb-type-NO.pdf", height = 4, width = 10)

clmm(as.factor(veridicality_num) ~ fct_relevel(sayByMeansVerbType, "form") + 
       (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(sayByMeansVerbType, "form")manner  0.07002    0.24403   0.287    0.774
# fct_relevel(sayByMeansVerbType, "form")sound  -0.05362    0.33352  -0.161    0.872 

# fct_relevel(sayByMeansVerbType, "manner")form  -0.07002    0.24403  -0.287    0.774
# fct_relevel(sayByMeansVerbType, "manner")sound -0.12364    0.31589  -0.391    0.695

# fct_relevel(sayByMeansVerbType, "sound")form    0.05362    0.33351   0.161    0.872
# fct_relevel(sayByMeansVerbType, "sound")manner  0.12364    0.31588   0.391    0.695

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### overall ----
mean.proj.overall <- d.proj %>%
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
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
ggsave("../graphs/projection-by-saybymeansverb-type2-NO.pdf", height = 4, width = 10)


## VerbVeridicality dataset ----
z <-  read.csv("../data/verb-veridicality.csv")
nrow(z) # 1498

# separate projection ratings (negative environment ratings) into separate columns. 
z <- z %>% 
  mutate(turker_neg_ratings2 = turker_neg_ratings, .after = turker_pos_ratings) %>% 
  separate(turker_neg_ratings, c("rating1", "rating2", "rating3"), 
           sep = ",")

# select only items with that-complements
z <- z %>% 
  subset(z$task == "that") %>% 
  pivot_longer(
    cols = c("rating1", "rating2", "rating3"), 
    names_to = "rating_number", 
    values_to = "rating")
nrow(z) # 2577

# "rating.rescaled" for easier comparison with MV dataset whose ratings we have
# recoded to numerical values ranging from -1 to 1.
z <- z %>% mutate(rating = as.numeric(as.character(z$rating)),
                  rating.rescaled = rating/2)

# how many predicates?
z %>% distinct(verb) %>% count() # 78

z %>% 
  filter(verb %in% d.proj$verb) %>%  
  distinct(verb) %>% 
  count() # 67

z %>% 
  filter(!verb %in% d.proj$verb) %>%  
  distinct(verb) %>% 
  arrange(verb) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
# [1] "appear"    "be"        "felt"      "give"      "hold"      "mean"      "provide"   "recommend" "saw"      
# [10] "seem"      "speculate" 

# excluded from analysis:
# 'appear': pseudo-copula
# 'be': copula
# 'give': = 'given that' (= 'since', 'considering that')
# 'mean': only with non-sentient subjects
# 'provide': non-sentient subjects or 'provided that' (= 'on the condition that')
# 'seem': pseudo-copula

z2 <- z %>% 
  filter(! verb %in% c("appear", "be", "give", "mean", "provide", "seem"))

z2 %>% distinct(verb) %>% count() # 72

vvonlypreds <- c("felt", "hold", "recommend", "saw", "speculate") 

# Creating .csv file for predicate coding for VV dataset, transferring the 
# classifications for the 67 predicates that occur in both datasets from 
# predicate-coding.csv (for MV predicates) to the new file. Classifications
# for the 5 predicates that occur only in the VV dataset are added manually
# in the .csv file. 

# view(left_join(z2, y, by = c("verb")))
# 
# predicateCodingVV <- left_join(z2, y, by = c("verb")) %>% 
#   select(!c(1:2, 4:17)) %>% 
#   group_by(verb) %>% 
#   slice_head()
# view(predicateCodingVV)

# DO NOT RUN THIS AGAIN!
# predicateCodingVV %>% [DO NOT RUN!!!] write.csv("../data/predicate-coding-vv.csv", row.names = FALSE)


# load predicate coding
z3 <- read.csv("../data/predicate-coding-vv.csv")
nrow(z3) # 72

dvv <- left_join(z2, z3, by = c("verb"))
nrow(dvv) # 2403

# create predicateType, emotiveComponent, change-of-state, environment columns
d.proj.vv <- dvv %>%
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

# calculate by-predicate projection means for original and rescaled ratings.
mean.proj.vv <- dvv %>%
  group_by(verb) %>% 
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         verb = fct_reorder(as.factor(verb), Mean.Proj))

# add predicateType etc. to the means
tmp3 <- d.proj.vv %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)
nrow(tmp3) # 72

mean.proj.vv <- left_join(mean.proj.vv, tmp3, by = c("verb")) %>%
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.vv) # 68

### say verbs ----
# distribution of communication predicates
d.proj.vv %>% 
  filter(predicateType == "communicative" | sayVerb == "yes") %>% 
  group_by(sayVerb, predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb predicateType     n
#   <chr>   <chr>         <int>
# 1 no      communicative    12
# 2 yes     cognitive         1
# 3 yes     comPriv           2
# 4 yes     communicative    22
# There are 37 communication predicates in the VV dataset. One of them ('think') 
# is a say-verb but not a communicative. Two are predicates that can be interpreted
# both as communicative and private predicates. These three predicates are excluded
# from analysis.

vvonly.acc.comms <- d.proj.vv %>% 
  filter(verb_renamed %in% vvonlypreds & predicateType == "communicative") %>% 
  distinct(verb_renamed)
# verb_renamed
# <chr>       
# 1 recommend   

d.proj.vv %>% 
  filter(predicateType == "communicative" & ! verb_renamed %in% acc.comms) %>% 
  distinct(verb_renamed)
#   verb_renamed
#   <chr>       
# 1 recommend   

# All communicative predicates in the VV dataset that also occur in the MV dataset
# have a mean acceptability rating of greater than 4 in the MV dataset. Therefore,
# non of the 34 communicatives are excluded from analysis. 


#### types of communicatives ----
d.proj.vv %>% 
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb     n
#   <chr>   <int>
# 1 no         12
# 2 yes        22
# Of the 34 communicatives in the MV dataset, 22 are say-predicates.

mean.proj.vv.commsay <- d.proj.vv %>%
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.vv.commsay) # 2

ggplot(mean.proj.vv.commsay, aes(x = sayVerb, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1))
ggsave("../graphs/projection-by-communication-type-vv.pdf", height = 4, width = 10)

#### types of say verbs ----
d.proj.vv %>% 
  filter(sayVerb == "yes" & predicateType == "communicative") %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerbType             n
#   <chr>               <int>
# 1 discourse role verb    19
# 2 mode verb               2
# 3 say                     1

mean.proj.saytype.vv <- d.proj.vv %>%
  filter((sayVerb == "yes" & predicateType == "communicative")) %>%  
  group_by(sayVerbType) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.saytype.vv) # 3

ggplot(mean.proj.saytype.vv, aes(x = sayVerbType, y = Mean.Proj)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1))
ggsave("../graphs/projection-by-sayverb-type-vv.pdf", height = 4, width = 10)

mean.proj.saytype.vv2 <- d.proj.vv %>%
  filter(predicateType == "communicative") %>% 
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.saytype.vv2) # 4

ggplot(mean.proj.saytype.vv2, aes(x = sayVerb, y = Mean.Proj, colour = sayVerbType)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_colour_manual(values = c("purple","deeppink", "orange3", "grey50"))
ggsave("../graphs/projection-by-sayverb-type-vv2.pdf", height = 4, width = 10)

#### types of mode verbs ----
d.proj.vv %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means          1
# 2 say-with-attitude     1

mean.proj.modetype.vv <- d.proj.vv %>%
  filter((sayVerb == "yes" & predicateType == "communicative")) %>%  
  group_by(sayVerbType, modeVerbType) %>% 
  summarize(Mean.Proj = mean(rating.rescaled), CILow = ci.low(rating.rescaled), 
            CIHigh = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh)
nrow(mean.proj.modetype.vv) # 4

ggplot(mean.proj.modetype.vv, aes(x = sayVerbType, y = Mean.Proj, colour = modeVerbType)) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_colour_manual(values = c("green3","blue", "grey50"))
ggsave("../graphs/projection-by-modeverb-type-vv.pdf", height = 4, width = 10)

#### types of say-by-means verbs ----
d.proj.vv %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                   1

# No plot for only one predicate. :) - no "overall" plot, either.

### by-predicateType ----
d.proj.vv %>%
  filter(predicateType2 != "comPriv" & predicateType2 != "other") %>% 
  group_by(predicateType2) %>%
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         16
# 2 emoComm            1
# 3 emotive            3
# 4 evidential        15
# 5 nonEmoComm        33

d.proj.vv %>%
  filter(predicateType2 == "emoComm") %>% 
  distinct(verb_renamed)
#   verb_renamed
#   <chr>       
# 1 complain  

# With only one communicative with an emotive component, the VV dataset is of 
# limited use for investigating the 'emotive component' property.

# With only three projection ratings per item, the dataset is overall ill-suited 
# for a more fine-grained investigation of communication predicates. 

#### plots ----
# calculate by-predicateType means

mean.proj.type.vv = d.proj.vv %>%
  group_by(predicateType) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         predicateType = fct_reorder(as.factor(predicateType), Mean.Proj))
nrow(mean.proj.type.vv) # 6

mean.proj.type.vv %>%
  filter(predicateType != "other" & predicateType != "comPriv") %>% 
  ggplot(aes(x = predicateType, y = Mean.Proj.rescaled, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(vjust = -1),
        axis.text = element_text(size = 12),
        plot.margin = margin(5.5, 5.5, 22, 5.5, "pt"),
        panel.grid.major.x = element_blank()) +
  ylab("Mean projection rating") +
  xlab("Predicate type") +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicateType-vv.pdf", height = 4, width = 10)

# calculate by-predicateType2 means
mean.proj.type2.vv = d.proj.vv %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(rating), 
            CILow = ci.low(rating), 
            CIHigh = ci.high(rating),
            Mean.Proj.rescaled = mean(rating.rescaled), 
            CILow.rescaled = ci.low(rating.rescaled), 
            CIHigh.rescaled = ci.high(rating.rescaled)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         YMin.Proj.rescaled = Mean.Proj.rescaled - CILow.rescaled, 
         YMax.Proj.rescaled = Mean.Proj.rescaled + CIHigh.rescaled,
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Proj))
nrow(mean.proj.type2.vv) # 7

mean.proj.type2.vv %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
  ggplot(aes(x = factor(predicateType2, c("cognitive", "evidential", "nonEmoComm", 
                                          "emoComm", "emotive")), 
             y = Mean.Proj.rescaled, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj.rescaled, ymax = YMax.Proj.rescaled), width = 0) +
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
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_x_discrete(labels = c("cognitive", "evidential",
                              "communicative without\nemotive component",
                              "communicative with\nemotive component", "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2-vv.pdf", height = 4, width = 10)

### by predicate ----
#### plots ----
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicate-vv.pdf", height = 4, width = 13)

# with labels
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_label_repel(data = subset(mean.proj.vv, predicateType == "emotive"),
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
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-predicate-labelled-vv.pdf", height = 4, width = 13)

# faceted
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ predicateType, ncol = 4)
ggsave("../graphs/projection-by-predicate-faceted-vv.pdf", height = 4, width = 9)

# with labels
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point() +
  geom_label_repel(data = subset(mean.proj.vv, predicateType2 == "emotive"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  geom_label_repel(data = subset(mean.proj.vv, predicateType2 == "emoComm"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = -0.2, nudge_y = -0.2,
                   colour = "green3") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       colour = "Predicate type") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(labels = predicateType2_names, values = cols2)
ggsave("../graphs/projection-by-predicate-labelled2-vv.pdf", height = 4, width = 13)

# faceted
mean.proj.vv %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj.rescaled, colour = predicateType2)) +
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
ggsave("../graphs/projection-by-predicate-faceted-vv2.pdf", height = 4, width = 10)

#### models ----
lm(Mean.Proj.rescaled ~ fct_relevel(predicateType, "communicative"), data = mean.proj.vv) %>% 
  summary()
# Coefficients:
#                                                       Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                           -0.08113    0.04271  -1.899    0.062 .
# fct_relevel(predicateType, "communicative")cognitive   0.03279    0.07550   0.434    0.666  
# fct_relevel(predicateType, "communicative")emotive     0.13609    0.15000   0.907    0.368  
# fct_relevel(predicateType, "communicative")evidential  0.09766    0.07720   1.265    0.210 

# The predicate type communicative is significant at the 0.1 level, i.e. not really
# significant. None of the other predicate types are significant.

lm(Mean.Proj.rescaled ~ fct_relevel(predicateType2, "emoComm"), data = mean.proj.vv) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                       0.09722    0.24998   0.389    0.699
# fct_relevel(predicateType2, "emoComm")cognitive  -0.14556    0.25767  -0.565    0.574
# fct_relevel(predicateType2, "emoComm")emotive    -0.04226    0.28865  -0.146    0.884
# fct_relevel(predicateType2, "emoComm")evidential -0.08069    0.25818  -0.313    0.756
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.18375    0.25374  -0.724    0.472

# None of the five predicate types with the 'emotive component' distinction are 
# correlated with projection ratings.

## VAD ratings ----

# load valence and arousal data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  filter(Word %in% d$verb) %>%
  rename(verb = Word) %>%
  left_join(d, by = "verb")
nrow(w2) # 17747
n_distinct(w2$verb) # 423

# create predicate type, emotive component columns
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

# remove "other" and "comPriv" predicates
w2 <-  w2 %>%
  filter(predicateType != "other" & predicateType != "comPriv") 
nrow(w2) # 17070

# how many predicates?
n_distinct(w2$verb_renamed) # 428
n_distinct(w2$verb) # 410

w3 <- w2 %>% 
  distinct(verb_renamed, verb, voice, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum)

# combine projection and valence/arousal/dominance ratings in one data frame
mean.proj2 <-  left_join(w3, mean.proj, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj2) # 428

table(mean.proj2$predicateType)
# cognitive communicative       emotive    evidential 
#        46           205           101            76 

table(mean.proj2$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        46         41        101         76        164 

### rescale V + A + D ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings for valence and arousal are rescaled to range from 0 to 1. 
new.scale <- mean.proj2 %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))

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

# Dominance: everything in order? - yes.
(max(mean.proj2$D.Mean.Sum)-min(mean.proj2$D.Mean.Sum))/4
# [1] 1.1825
new.scale %>% 
  group_by(D.Mean.Sum2.direction) %>% 
  slice_max(D.Mean.Sum2) %>% 
  ungroup() %>% 
  pull(D.Mean.Sum2) %>% 
  sum()
# [1] 1.1825

### Dominance ---- 
#### by predicate ----
# with emotive component distinction for communicatives
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

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
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                                       0.17329    0.02241   7.734 7.71e-14 ***
#   fct_relevel(predicateType2, "emoComm")cognitive   0.07834    0.03082   2.542  0.01137 *  
#   fct_relevel(predicateType2, "emoComm")emotive     0.07351    0.02657   2.767  0.00591 ** 
#   fct_relevel(predicateType2, "emoComm")evidential  0.08345    0.02780   3.002  0.00284 ** 
#   fct_relevel(predicateType2, "emoComm")nonEmoComm  0.03851    0.02505   1.537  0.12503  

# For all five predicate types there is a correlation with dominance ratings at the
# 0.001 significance level.

#### by predicate type ----
# calculate dominance by predicateType2 means (communicatives with/without emotive component)
mean.dominance2 = new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance2

ggplot(mean.dominance2, aes(x = predicateType2, y = Mean.Dominance, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/dominance-by-predicateType2.pdf", height = 8, width = 10)

#### distribution ----
##### by predicate type ----
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
                    labels = c("cognitive", "communicative with\nemotive component", 
                               "emotive", "evidential", 
                               "communicative without\nemotive component"))
ggsave("../graphs/valence-dominance-by-predicateType2.pdf", height = 6, width = 10)

# valence by dominance with fitted line
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean valence rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-line.pdf", height = 6, width = 8)

# valence by dominance with fitted lines for positive/negative valence
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
       y = "Mean valence rating",
       colour = "Predicate type", 
       linetype = "Direction of valence") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-line-direction-of-valence.pdf", height = 6, width = 8)

# valence by dominance with fitted lines for positive/negative dominance
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
       y = "Mean valence rating",
       colour = "Predicate type", 
       linetype = "Direction of dominance") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/valence-by-dominance-line-direction-of-dominance.pdf", height = 6, width = 8)


#### ratings correlated? ----
##### valence - dominance ----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18418    0.01699  10.841  < 2e-16 ***
# D.Mean.Sum2  0.45549    0.06275   7.258 1.87e-12 ***

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         .
# emotive         ***
# evidential      **
# nonEmoComm      *

lm(D.Mean.Sum2 ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.15897    0.01166  13.629  < 2e-16 ***
# V.Mean.Sum2  0.24163    0.03329   7.258 1.87e-12 ***

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       ***
# emoComm         n.s.
# emotive         ***
# evidential      ***
# nonEmoComm      *

###### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-valence.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-valence-faceted.pdf", height = 6, width = 8)

ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-dominance.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-dominance-faceted.pdf", height = 6, width = 8)


##### arousal - dominance ----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.18418    0.01699  10.841  < 2e-16 ***
# D.Mean.Sum2  0.45549    0.06275   7.258 1.87e-12 ***

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s. 

lm(D.Mean.Sum2 ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.20216    0.02600   7.774 5.75e-14 ***
# A.Mean.Sum2  0.06394    0.06047   1.057    0.291 

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s. 

###### plots ----
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-arousal.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = D.Mean.Sum2)) +
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
ggsave("../graphs/dominance-by-arousal-faceted.pdf", height = 6, width = 8)

ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-dominance.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = D.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-dominance-faceted.pdf", height = 6, width = 8)

##### valence - arousal ----
lm(V.Mean.Sum2 ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.0001786  0.0326765   0.005    0.996    
# A.Mean.Sum2 0.6958581  0.0759862   9.158   <2e-16 ***

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         ***
# emotive         **
# evidential      *
# nonEmoComm      **

lm(A.Mean.Sum2 ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.345934   0.009044  38.252   <2e-16 ***
# V.Mean.Sum2 0.236373   0.025811   9.158   <2e-16 ***

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "evidential"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       n.s.
# emoComm         ***
# emotive         **
# evidential      *
# nonEmoComm      **

###### plots ----
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-valence.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = A.Mean.Sum2)) +
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
ggsave("../graphs/arousal-by-valence-faceted.pdf", height = 6, width = 8)

ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-arousal.pdf", height = 6, width = 8)

# faceted
ggplot(new.scale, aes(x = A.Mean.Sum2, y = V.Mean.Sum2)) +
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
ggsave("../graphs/valence-by-arousal-faceted.pdf", height = 6, width = 8)


#### predicate types  ----
# do they predict VAD ratings?
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "evidential"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      *** 

# All predicate types seem to be highly correlated with VAD ratings.

##### tables ----
# direction of dominance and valence correlated?

contingency_table <- table(new.scale$D.Mean.Sum2.direction, new.scale$V.Mean.Sum2.direction)
print(contingency_table)
#          negative positive
# negative      119       21
# positive       66      222

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 145.44, df = 1, p-value < 2.2e-16

# The direction of dominance and valence seem highly correlated.

# distribution of direction within predicate types
new.scale %>% 
  count(predicateType2, D.Mean.Sum2.direction)
#    predicateType2 D.Mean.Sum2.direction   n
# 1       cognitive              negative  10 22% of cognitives
# 2       cognitive              positive  36
# 3         emoComm              negative  17 41% of emoComms
# 4         emoComm              positive  24
# 5         emotive              negative  55 54% of emotives
# 6         emotive              positive  46
# 7      evidential              negative  13 17% of evidentials
# 8      evidential              positive  63
# 9      nonEmoComm              negative  45 27% of nonEmoComms
# 10     nonEmoComm              positive 119

new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#    predicateType2 V.Mean.Sum2.direction   n
# 1       cognitive              negative  14 30% of cognitives
# 2       cognitive              positive  32
# 3         emoComm              negative  29 71% of emoComms <== by far largest diff from dominance direction
# 4         emoComm              positive  12
# 5         emotive              negative  64 63% of emotives
# 6         emotive              positive  37
# 7      evidential              negative  17 22% of evidentials
# 8      evidential              positive  59
# 9      nonEmoComm              negative  61 37% of nonEmoComms
# 10     nonEmoComm              positive 103


#### valence by dominance: no patterns (clusters) emerge. ----
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


#### projection by dominance with emotive component ----
##### plots ----
ggplot(new.scale, aes(x = D.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean dominance rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, labels = predicateType2_names)
ggsave("../graphs/projection-by-dominance2-NO.pdf", height = 6, width = 8)

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

lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.31492    0.03006  10.476   <2e-16 ***
# D.Mean.Sum2  0.13754    0.11104   1.239    0.216  

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# emoComm: 0.1
# nonEmoComm: n.s.
# emotive: n.s.
# evidential: n.s.
# cognitive: n.s.

##### tables ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#     verb_renamed D.Mean.Sum2
# 1   be delighted      0.5725
# 2          enjoy      0.5700
# 3        approve      0.5650
# 4   be comforted      0.5450
# 5       research      0.5400
# 6         detail      0.5350
# 7  be stimulated      0.5350
# 8      recognize      0.5275
# 9      determine      0.5225
# 10        signal      0.5125
# 11  be signalled      0.5125

new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         36
# 2 emoComm           24
# 3 emotive           46
# 4 evidential        63
# 5 nonEmoComm       119

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2)
#     verb_renamed D.Mean.Sum2
# 1            cry      0.6100
# 2          panic      0.5875
# 3    be panicked      0.5875
# 4  be frightened      0.5725
# 5    be tortured      0.5600
# 6   be terrified      0.5525
# 7      be fooled      0.5150
# 8          doubt      0.4850
# 9           envy      0.4600
# 10         worry      0.4575
# 11    be worried      0.4575

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         10
# 2 emoComm           17
# 3 emotive           55
# 4 evidential        13
# 5 nonEmoComm        45

##### with direction ----
# projection by dominance with direction
##### plots ----
###### with direction of valence: happy vs unhappy ----
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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

###### with "direction" of dominance ----
# controlled/submissive/guided/... vs in control/ autonomous/controlling/... 
# i.e., somebody else's vs one's own dominance
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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2-NO.pdf", 
       height = 6, width = 10)


##### ordinal models ----
###### data frame for ordinal models ----
d.proj2 = droplevels(subset(w2, w2$polarity == "negative" & w2$conditional == "False")) %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative")) %>% 
  distinct(verb_renamed, verb, voice, participant, predicateType, predicateType2, 
           veridicality, veridicality_num, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum, 
           V.Mean.Sum2, V.Mean.Sum2.direction, A.Mean.Sum2, D.Mean.Sum2,
           D.Mean.Sum2.direction)
d.proj2 %>% nrow() # 4257

###### models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant), data = d.proj2) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2   0.6736     0.2231   3.019  0.00253 **

# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.5780     0.3564   4.427 9.55e-06 ***
# V.Mean.Sum2.directionpositive              -0.4640     0.1222  -3.798 0.000146 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.6972     0.4677  -1.491 0.136063  

# with 'direction' of dominance
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * D.Mean.Sum2.direction + (1 | participant), 
     data = d.proj2) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 2.1764     0.3937   5.528 3.23e-08 ***
# D.Mean.Sum2.directionpositive              -0.2175     0.1224  -1.777 0.075642 .  
# D.Mean.Sum2:D.Mean.Sum2.directionpositive  -1.7687     0.4836  -3.657 0.000255 ***

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") +  (1 | participant), data = d.proj2) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       n.s.
# negative  emotive         n.s.
# negative  emoComm         **
# negative  evidential      *
# negative  nonEmoComm      n.s.
# positive  cognitive       n.s.
# positive  emotive         n.s.
# positive  emoComm         n.s.
# positive  evidential      n.s.
# positive  nonEmoComm      n.s.
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# with direction of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") +  (1 | participant), data = d.proj2) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       .
# negative  emotive         .
# negative  emoComm         ***
# negative  evidential      **
# negative  nonEmoComm      n.s.
# positive  cognitive       n.s.
# positive  emotive         n.s.
# positive  emoComm         n.s.
# positive  evidential      .
# positive  nonEmoComm      n.s.


# Acceptability ----
clmm(as.factor(veridicality_num) ~ acceptability + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.44698    0.02162   20.68   <2e-16 ***

mean.proj.acc <- d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num),
            Mean.Acc = mean(acceptability), 
            CILow.Acc = ci.low(acceptability), 
            CIHigh.Acc = ci.high(acceptability)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.acc) # 544

# add predicateType etc. to the means
tmp3 <-  d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent)
nrow(tmp3) # 544

mean.proj.acc <- left_join(mean.proj.acc, tmp3, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.acc) # 544

# remove "other" and "comPriv" predicates
mean.proj.acc <- mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other"))
nrow(mean.proj.acc) # 523

predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

ggplot(mean.proj.acc, aes(x = Mean.Acc, y = Mean.Proj)) +
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

clmm(as.factor(veridicality) ~ acceptability + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.43314    0.02235   19.38   <2e-16 ***

# tables 
d.proj %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  group_by(acceptability) %>% 
  summarise(count = n())
#   acceptability count
#           <int> <int>
# 1             1   141
# 2             2   424
# 3             3   665
# 4             4   678
# 5             5  1096
# 6             6  1074
# 7             7  1124

o <- d.proj %>% 
  group_by(acceptability, veridicality) %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  count() %>% 
  group_by(acceptability) %>%
  mutate(percentage =  n * 100 / sum(n)) %>% 
  print(n = Inf)
#   acceptability veridicality     n percentage
#           <int> <chr>        <int>      <dbl>
# 1             1 maybe           93      66.0 
# 2             1 no              22      15.6 
# 3             1 yes             26      18.4 
# 4             2 maybe          269      63.4 
# 5             2 no              41       9.67
# 6             2 yes            114      26.9 
# 7             3 maybe          427      64.2 
# 8             3 no              72      10.8 
# 9             3 yes            166      25.0 
# 10             4 maybe          414      61.1 
# 11             4 no              46       6.78
# 12             4 yes            218      32.2 
# 13             5 maybe          531      48.4 
# 14             5 no              80       7.30
# 15             5 yes            485      44.3 
# 16             6 maybe          428      39.9 
# 17             6 no              47       4.38
# 18             6 yes            599      55.8 
# 19             7 maybe          395      35.1 
# 20             7 no              46       4.09
# 21             7 yes            683      60.8

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

# predicates with mean acceptability ratings of less than 4
mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other"), Mean.Acc < 4) %>% 
  arrange(Mean.Acc) %>% 
  reframe(verb_renamed, Mean.Acc, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
# 1 grin               2.3      0.6   emoComm       
# 2 bark               2.33     0.111 emoComm       
# 3 snap               2.7      0     emoComm       
# 4 vote               2.7      0.3   nonEmoComm    
# 5 come out           2.8      0.2   nonEmoComm    
# 6 be nonplussed      2.9      0.5   emotive       
# 7 negotiate          2.9      0.3   nonEmoComm    
# 8 stutter            2.9      0.2   nonEmoComm    
# 9 be bet             3        0.222 evidential    
# 10 be cheered         3.1      0.6   emotive       
# 11 be tricked         3.1     -0.1   evidential    
# 12 curse              3.1      0     emoComm       
# 13 gab                3.1      0.4   nonEmoComm    
# 14 okay               3.1      0.4   nonEmoComm    
# 15 quip               3.1      0     emoComm       
# 16 growl              3.11     0.111 emoComm       
# 17 sing               3.11     0     nonEmoComm    
# 18 be tweeted         3.2      0.1   evidential    
# 19 scribble           3.2      0     nonEmoComm    
# 20 will               3.2      0     nonEmoComm    
# 21 simulate           3.3      0.3   nonEmoComm    
# 22 beam               3.4      0.1   emoComm       
# 23 muse               3.4      0.1   cognitive     
# 24 rediscover         3.4      0     evidential    
# 25 request            3.4      0.2   nonEmoComm    
# 26 underscore         3.4      0.1   nonEmoComm    
# 27 authorize          3.5      0.4   nonEmoComm    
# 28 grunt              3.5      0.1   emoComm       
# 29 maintain           3.5      0     nonEmoComm    
# 30 underline          3.5      0.3   nonEmoComm    
# 31 be quoted          3.6      0.1   evidential    
# 32 indict             3.6     -0.2   nonEmoComm    
# 33 prophesy           3.6      0.2   nonEmoComm    
# 34 spout              3.6      0.3   nonEmoComm    
# 35 whimper            3.6      0.3   emoComm       
# 36 yell               3.6      0     nonEmoComm    
# 37 add                3.7      0.2   nonEmoComm    
# 38 be invigorated     3.7      0.8   emotive       
# 39 be maddened        3.7      0.9   emotive       
# 40 cackle             3.7      0.5   emoComm       
# 41 chant              3.7      0     nonEmoComm    
# 42 hoot               3.7      0.1   emoComm       
# 43 jest               3.7      0.4   emoComm       
# 44 lie                3.7      0.5   nonEmoComm    
# 45 measure            3.7      0.1   evidential    
# 46 rationalize        3.7      0.3   evidential    
# 47 underestimate      3.7      0.2   cognitive     
# 48 be befuddled       3.8      0.4   emotive       
# 49 be charmed         3.8      0.6   emotive       
# 50 be stupefied       3.8      0.4   emotive       
# 51 elect              3.8      0.1   evidential    
# 52 giggle             3.8     -0.1   emoComm       
# 53 lecture            3.8      0.3   nonEmoComm    
# 54 manufacture        3.8      0.2   nonEmoComm    
# 55 mutter             3.8      0     emoComm       
# 56 reason             3.8     -0.1   evidential    
# 57 rule               3.8     -0.2   nonEmoComm    
# 58 shriek             3.8      0.2   emoComm       
# 59 volunteer          3.8      0.1   nonEmoComm    
# 60 be signalled       3.9      0.4   evidential    
# 61 be tantalized      3.9      0.5   emotive       
# 62 categorize         3.9      0.2   evidential    
# 63 hope               3.9      0.2   cognitive     
# 64 howl               3.9      0.3   emoComm       
# 65 mumble             3.9      0     nonEmoComm    
# 66 presuppose         3.9     -0.1   cognitive     
# 67 require            3.9     -0.1   cognitive     
# 68 snitch             3.9      0.3   nonEmoComm    
# 69 summarize          3.9      0.3   nonEmoComm    
# 70 test               3.9     -0.1   nonEmoComm 


# What proportion of predicates within each type has a mean acceptability rating 
# of less than 4?
count <- mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  group_by(predicateType2) %>% 
  summarise(total.count = n())
count2 <- mean.proj.acc %>% 
  filter(! predicateType %in% c("comPriv", "other"), Mean.Acc < 4) %>% 
  group_by(predicateType2) %>% 
  summarise(low.acc.count = n())
left_join(count, count2, by = "predicateType2") %>% 
  mutate(low.acc.percentage = low.acc.count * 100 / total.count)
#   predicateType2 total.count low.acc.count low.acc.percentage
#   <chr>                <int>         <int>              <dbl>
# 1 cognitive               53             5               9.43
# 2 emoComm                 47            16              34.0 
# 3 emotive                148             8               5.41
# 4 evidential              86            11              12.8 
# 5 nonEmoComm             189            30              15.9 


# Volition ----
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
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

d.proj.comm <- d.proj %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 2349

# all predicates
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.19571    0.02089    9.37   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.29075    0.02686   10.82   <2e-16 ***

# only communicatives
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), 
   data = mean.proj %>% filter(predicateType == "communicative")) %>% 
  summary()
# Coefficients:
#                                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.18900    0.01739  10.871  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.14006    0.04277   3.275  0.00122 ** 

lm(Mean.Proj ~ fct_relevel(volition, "volitional"), 
   data = mean.proj %>% filter(predicateType2 == "emoComm")) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.29192    0.04904   5.953 3.67e-07 ***
# fct_relevel(volition, "volitional")non-volitional  0.07297    0.06724   1.085    0.284   

lm(Mean.Proj ~ fct_relevel(volition, "volitional"), 
   data = mean.proj %>% filter(predicateType2 == "nonEmoComm")) %>% 
  summary()
# (Intercept)                                        0.17606    0.01850   9.516   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.08902    0.06798   1.310    0.192  

clmm(as.factor(veridicality_num) ~ volition + (1 | participant), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
# volitionvolitional  -0.6126     0.1232  -4.974 6.55e-07 ***

clmm(as.factor(veridicality_num) ~ volition * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                                     Estimate Std. Error z value Pr(>|z|)    
# volitionvolitional                                                  -0.38333    0.20453  -1.874   0.0609 .
# fct_relevel(predicateType2, "emoComm")nonEmoComm                    -0.51296    0.23229  -2.208   0.0272 *
# volitionvolitional:fct_relevel(predicateType2, "emoComm")nonEmoComm  0.03549    0.28388   0.125   0.9005   

# faceted by predicate type
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

mean.proj %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
  ggplot(aes(x = verb_renamed, y = Mean.Proj, colour = volition)) +
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

mean.proj %>% 
  filter(predicateType2 %in% c("emoComm", "nonEmoComm")) %>% 
  group_by(predicateType2, volition) %>% 
  count()
#   predicateType2 volition           n
#   <chr>          <chr>          <int>
# 1 emoComm        non-volitional    25
# 2 emoComm        volitional        22
# 3 nonEmoComm     non-volitional    14
# 4 nonEmoComm     volitional       175


### by predicate type ----
#### distribution ----
# how many predicates of which predicateType are non-/volitional?
d.proj %>%
  select(predicateType2, verb_renamed, volition) %>%
  unique() %>%
  group_by(predicateType2, volition) %>%
  summarize(count=n())

# predicateType2 volition       count
# <chr>          <chr>          <int>
# 1 cognitive      non-volitional    44
# 2 cognitive      volitional         9
# 3 comPriv        non-volitional     5
# 4 comPriv        volitional         4
# 5 emoComm        non-volitional    25
# 6 emoComm        volitional        22
# 7 emotive        non-volitional   148 
# 8 evidential     non-volitional    83 
# 9 evidential     volitional         3 
# 10 nonEmoComm     non-volitional    14
# 11 nonEmoComm     volitional       175
# 12 other          non-volitional    10
# 13 other          volitional         2

# All emotives are non-volitional. Almost all evidentials are non-volitional.

mean.proj.vol = d.proj %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType2), Mean.Proj))
mean.proj.vol
nrow(mean.proj.vol) # 13

# remove "other" and "comPriv" predicates
mean.proj.vol = mean.proj.vol %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.vol) # 9

#### plot ----
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
  scale_x_discrete(labels = c("cognitive", "communicative with\nemotive component", 
                              "emotive", "evidential", 
                              "communicative without\nemotive component")) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_colour_manual(values = c("volitional" = "green3", "non-volitional" = "red"))
ggsave("../graphs/projection-by-predicateType-and-volition-NO.pdf", height = 4, width = 10)

# Cognitives ----
# Why do the cognitives behave different from other predicates in several respects?
mean.proj %>%
  filter(predicateType2 == "cognitive") %>%
  select(verb_renamed, Mean.Proj, CILow, CIHigh) %>% 
  slice_max(Mean.Proj, n = 10) 
# verb_renamed Mean.Proj CILow CIHigh
# <fct>            <dbl> <dbl>  <dbl>
# 1 forget           0.9   0.2    0.1  
# 2 forgive          0.8   0.3    0.2  
# 3 understand       0.8   0.3    0.2  
# 4 appreciate       0.7   0.5    0.3  
# 5 ignore           0.7   0.3    0.3  
# 6 know             0.7   0.3    0.202
# 7 miss             0.7   0.3    0.3  
# 8 disregard        0.667 0.333  0.225
# 9 anticipate       0.6   0.3    0.3  
# 10 expect           0.6   0.3    0.3   


# Overall ----
### by-predicate ----
# calculate by-predicate projection means 
mean.proj <-  d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# add predicateType and verb to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, predicateType2)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2)
nrow(tmp) # 544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj) # 544

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) # 523

### with imminency ----

# all predicates with imminency readings as listed by K&A (2024) and identified by us
# ("identify", "detect" and "piece together") 
mean.proj.immi <- mean.proj %>% 
  filter(verb_renamed %in% c("comprehend", "confirm", "detect", "discover", 
                             "establish", "figure out", "find out", "foresee", 
                             "foretell", "identify", "learn", "note", "notice", 
                             "observe", "perceive", "piece together", "realize", 
                             "recognize", "reveal", "understand", "verify")) 
nrow(mean.proj.immi) # 21

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj.immi, aes(x = verb_renamed, y = Mean.Proj, colour = "coming-to-know predicate"), 
             size = 2.5) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) + 
  scale_colour_manual(limits = c("other", "coming-to-know predicate", "know"),
                      values = c("grey20", "green3", "red"))
ggsave("../graphs/projection-by-predicate-imminency.pdf", height = 4, width = 13)

### with emoComms ----
ggplot() +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(data = mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = "other"), 
             alpha = 0.5) +
  geom_point(data = mean.proj %>% filter(predicateType2 == "emoComm"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "communicative with emotive component")) +
  geom_point(data = mean.proj %>% filter(verb_renamed == "know"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "know")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("other", "communicative with emotive component", "know"),
                      values = c("grey50", "blue", "red"))
ggsave("../graphs/projection-by-predicate-emoComm-NO.pdf", height = 4, width = 13)




