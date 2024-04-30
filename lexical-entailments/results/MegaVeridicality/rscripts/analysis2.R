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
                                   TRUE ~ "no"))

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
           commType, changeOfState)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState)
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

# only specific predicates (those discussed by Korotkova & Anand (2024) and similar ones + know)
a <- mean.proj %>% 
  filter(verb_renamed %in% c("assess", "conclude", "deduce", "detect", "determine", 
                             "discover", "evaluate", "figure out", "find out", 
                             "gather", "identify", "infer", "learn", "notice", 
                             "piece together", "realize", "reason out", "reason", 
                             "recognize", "know"))

# load predicate attribute data
b <-  read.csv("../data/predicates2.csv")
nrow(b) # 19

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

cols = data.frame(predicate = levels(mean.proj.cosevi2$verb_renamed))
cols$category = as.factor(ifelse(cols$predicate %in% KA, "KA", 
                              ifelse(cols$predicate == "know", "KNOW", 
                                     "X")))
cols$imminency = as.factor(ifelse(cols$predicate %in% immi, "immi", "non-immi"))
cols$colours =  ifelse(cols$category == "KA", "deeppink", 
                       ifelse(cols$category == "KNOW", "grey60",
                              "black"))
cols$faces =  ifelse(cols$immi == "immi", "bold", "plain")
cols

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
  labs(x = "Predicate",
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
  labs(x = "Predicate",
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
  labs(x = "Predicate",
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
               filter(evidence_compatibility_anti_abduction_eliminative_reasoning == "yes"), 
             shape = 1, size = 8, stroke = 1.5, 
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
  labs(x = "Predicate",
       y = "Mean projection rating", 
       colour = "Compatible evidence types",
       caption = "Predicates highlighted in pink are those discussed by Korotkova & Anand (2024).\nPredicates in bold are those with an imminency reading.") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = c("gold3","purple", "red", "blue", "green2"))
ggsave("../graphs/projection-by-cos-evidential-types-of-evidence.pdf", height = 5, width = 13)
