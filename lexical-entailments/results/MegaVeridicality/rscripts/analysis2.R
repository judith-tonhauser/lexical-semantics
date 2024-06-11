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
# library(ggforce)
# library(ordinal)
# library(patchwork)
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
                                 polarity == "negative" & conditional == "True" ~ "q+cond+neg"))

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
           commType, changeOfState, volitional)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volitional)
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
ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = volitional)) +
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
predicateType2_names <- c( "cognitive" = "cognitive", 
                           "emoComm" = "communicative with\nemotive component", 
                           "emotive" = "emotive", 
                           "evidential" = "evidential", 
                           "nonEmoComm" = "communicative without\nemotive component")

mean.proj %>%
  filter(predicateType2 != "other" & predicateType2 != "comPriv") %>% 
ggplot(aes(x = verb_renamed, y = Mean.Proj, colour = volitional)) +
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

