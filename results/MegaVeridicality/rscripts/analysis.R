# Testing hypotheses based on 
# White & Rawlins' MegaVeridicality I dataset 
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)
theme_set(theme_bw())

# load data
d = read.csv("../data/d.csv")
nrow(d) #21692
names(d)

# load predicate coding
y = read.csv("../data/predicate-coding.csv")
nrow(y) #544
names(y)

d = left_join(d,y,by=c("verb","voice"))
nrow(d) #21692


# H1: The lexical meaning of the predicate matters ----
# We find that emotive predicates project more (by and large) than cognitive, evidential and communicative ones.
# Potential confound: stative vs eventive

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType column
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                 emotive == "yes" ~ "emotive",
                                 cognitive == "yes" ~ "cognitive",
                                 evidential == "yes" ~ "evidential",
                                 TRUE ~ "other"))

table(d.proj$predicateType)

# how many predicates in which type and which voice?
d.proj %>%
  select(predicateType, verb, voice) %>% 
  unique() %>% 
  group_by(predicateType,voice) %>% 
  summarize(count=n())

# color code the predicates
cols = d.proj %>%
  select(c(verb_renamed,predicateType)) %>%
  distinct(verb_renamed,predicateType)
cols
nrow(cols) #544

# color-code the predicates
cols$Colors =  ifelse(cols$predicateType == "comPriv", "pink",
                      ifelse(cols$predicateType == "emotive", "#D55E00", 
                          ifelse(cols$predicateType == "cognitive", "#5b43c4", 
                             ifelse(cols$predicateType == "communicative", "gray", 
                                    ifelse(cols$predicateType == "inferential", "green", "black")))))
cols

## by-predicateType ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  #filter(voice == "active") %>%
  group_by(predicateType, voice) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) #10
levels(mean.proj$predicateType)

ggplot(mean.proj, aes(x=predicateType, y=Mean.Proj, color = voice)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-by-predicateType.pdf",height=4,width=5)

## by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) #544
levels(mean.proj$verb_renamed)

# add predicateType, verb and voice to the means
tmp = d.proj %>%
  #filter(voice == "active") %>%
  select(c(verb,verb_renamed,voice,predicateType)) %>%
  distinct(verb,verb_renamed,voice,predicateType)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

cols$verb_renamed = factor(cols$verb_renamed, levels = mean.proj$verb_renamed[order(mean.proj$Mean.Proj)], ordered = TRUE)

# remove "other" and "comPriv" predicates
mean.proj = mean.proj %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj) #524

ggplot(mean.proj, aes(x=verb_renamed, y=Mean.Proj, fill = predicateType, color = predicateType)) +
  geom_point() +
  #geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  #theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-by-predicatel.pdf",height=4,width=13)

ggplot(mean.proj, aes(x=verb_renamed, y=Mean.Proj, fill = predicateType, color = predicateType)) +
  geom_point() +
  #geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  #theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  facet_grid(. ~ predicateType) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/projection-faceted-by-predicatel.pdf",height=4,width=13)

## only "emotive" predicates ----
mean.proj = mean.proj %>%
  filter(predicateType == "emotive")
nrow(mean.proj) #148

ggplot(mean.proj, aes(x = verb_renamed, y = Mean.Proj, colour = voice)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_colour_discrete(name = element_blank(), 
                        labels = c("verbal predicate", "adjectival predicate")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Emotive predicate")
ggsave("../graphs/projection-emotive-predicates.pdf", height = 4, width = 8)

# only emotives which occur in both "active" and "passive" sentence frames
mean.proj = mean.proj %>%
  group_by(verb) %>%
  filter(n()>1) %>%
  ungroup()
nrow(mean.proj) #10

ggplot(mean.proj, aes(x = verb, y = Mean.Proj, colour = voice)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Proj,ymax = YMax.Proj), width = 0) +
  geom_hline(yintercept = 0) +
  theme(legend.position = "top",
        axis.text.x = element_text(vjust = 1, hjust = 0.5),
        panel.grid.major.x = element_blank()) +
  scale_colour_discrete(name = element_blank(), 
                        labels = c("verbal predicate", "adjectival predicate")) +
  scale_x_discrete(labels = function(x) sub("_", " ", x)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Emotive predicate")
ggsave("../graphs/projection-emotives-verb-adjective-comparison.pdf", height = 4,width = 8)

# H1a: emotive component ----
# Among the communication predicates, does the CC of the ones that have an emotive meaning component
# project more than the CCs of those that don't?

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType and emotiveComponent columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no")) 

## by-predicateType ----
# calculate by-predicateType means
mean.proj = d.proj %>%
  group_by(predicateType, emotiveComponent) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) #7
levels(mean.proj$predicateType)

ggplot(mean.proj, aes(x=predicateType, y=Mean.Proj, color = emotiveComponent)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       color ="Emotive component")
ggsave("../graphs/projection-by-predicateType-emotiveComponent.pdf",height = 8, width = 12)

## by-predicate ----
# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) #544
levels(mean.proj$verb_renamed)

# add predicateType, verb and emotiveComponent to the means
tmp = d.proj %>%
  select(c(verb, verb_renamed, predicateType, emotiveComponent)) %>%
  distinct(verb, verb_renamed, predicateType, emotiveComponent)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

# only communicative predicates
mean.proj = mean.proj %>%
  filter(predicateType == "communicative")
nrow(mean.proj) #237

ggplot(mean.proj, aes(x=verb_renamed, y=Mean.Proj, color = emotiveComponent)) +
  geom_point() +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  labs(x = "Predicate",
       y = "Mean projection rating",
       color ="Emotive component")
ggsave("../graphs/projection-by-communicative-predicate.pdf", height = 4, width = 13)

# H1b: valence and arousal ----

# load valence and arousal data
z = read.csv("../data/BRM-emot-submit.csv")
nrow(z) #13915

d2 = z %>%
  filter(Word %in% d$verb) %>%
  rename(verb = Word) %>%
  inner_join(d, by = "verb")
nrow(d2) #17747
n_distinct(d2$verb) #423

# create predicateType and emotiveComponent columns
d2 = d2 %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         emotiveComponent = case_when(emotive_component == "yes" ~ "yes",
                                      TRUE ~ "no"),
         predicateType2 = case_when(predicateType == "comPriv" ~ "comPriv",
                                    predicateType == "communicative" & emotiveComponent == "yes" ~ "emoComm",
                                    predicateType == "communicative" & emotiveComponent == "no" ~ "nonEmoComm",
                                    predicateType == "emotive" ~ "emotive",
                                    predicateType == "cognitive" ~ "cognitive",
                                    predicateType == "evidential" ~ "evidential",
                                    predicateType == "other" ~ "other"))

# remove "other" and "comPriv" predicates
d2 = d2 %>%
  filter(predicateType != "other" & predicateType != "comPriv")

# how many predicates?
n_distinct(d2$verb_renamed) # 429
n_distinct(d2$verb) # 411

d3 = d2 %>%
  distinct(verb_renamed, verb, voice, predicateType, predicateType2, V.Mean.Sum, A.Mean.Sum, D.Mean.Sum)

table(d3$predicateType)
# cognitive communicative       emotive    evidential 
#        82           205           101            41 

table(d3$predicateType2)
# cognitive    emoComm    emotive evidential nonEmoComm 
#        82         41        101         41        164 

# valence by predicate
ggplot(d3, aes(x = reorder(verb_renamed, V.Mean.Sum), y=V.Mean.Sum, colour = predicateType)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean valence rating", colour = "Predicate type") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy", "neutral", "completely\nhappy"))
ggsave("../graphs/valence-by-predicate.pdf", height = 4, width = 13)

# valence by predicate with emotive component distinction for communicatives
ggplot(d3, aes(x = reorder(verb_renamed, V.Mean.Sum), y=V.Mean.Sum, colour = predicateType2)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean valence rating", colour = "Predicate type") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy", "neutral", "completely\nhappy")) +
  scale_colour_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                 "evidential", "communicative without\nemotive component"))
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

# arousal by predicate
ggplot(d3, aes(x = reorder(verb_renamed, A.Mean.Sum), y = A.Mean.Sum, colour = predicateType)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean arousal rating", colour = "Predicate type") +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited"))
ggsave("../graphs/arousal-by-predicate.pdf", height = 4, width = 13)

# arousal by predicate with emotive component
ggplot(d3, aes(x = reorder(verb_renamed, A.Mean.Sum), y = A.Mean.Sum, colour = predicateType2)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean arousal rating", colour = "Predicate type") +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_colour_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                   "evidential", "communicative without\nemotive component"))
ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)

# dominance by predicate
ggplot(d3, aes(x = reorder(verb_renamed, D.Mean.Sum), y = D.Mean.Sum, colour = predicateType)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean dominance rating", colour = "Predicate type") +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncontrolled", "neutral", "completely\nin control"))
ggsave("../graphs/dominance-by-predicate.pdf", height = 4, width = 13)

# dominance by predicate with emotive component
ggplot(d3, aes(x = reorder(verb_renamed, D.Mean.Sum), y = D.Mean.Sum, colour = predicateType2)) +
  geom_point() +
  labs(x = "Predicate", y = "Mean dominance rating", colour = "Predicate type") +
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncontrolled", "neutral", "completely\nin control")) +
  scale_colour_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                   "evidential", "communicative without\nemotive component"))
ggsave("../graphs/dominance-by-predicate2.pdf", height = 4, width = 13)

# distribution of valence, arousal and dominance ratings by predicate type

d3 %>% select(verb_renamed, predicateType, Valence = V.Mean.Sum, Arousal = A.Mean.Sum, Dominance = D.Mean.Sum) %>%
  pivot_longer(., cols = c(Valence, Arousal, Dominance), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType)) +
  geom_violin() +
  labs(y = "Mean rating", fill = "Predicate type") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy/\ncalm/\ncontrolled", "neutral", 
                                "completely\nhappy/\nexcited/\nin control")) + 
  scale_x_discrete(limits = c("Valence", "Arousal", "Dominance"))
ggsave("../graphs/valence-arousal-dominance-by-predicateType.pdf", height = 8, width = 16)

# distribution of valence, arousal and dominance ratings by predicate type including emotive component

d3 %>% select(verb_renamed, predicateType2, Valence = V.Mean.Sum, Arousal = A.Mean.Sum, Dominance = D.Mean.Sum) %>%
  pivot_longer(., cols = c(Valence, Arousal, Dominance), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = predicateType2)) +
  geom_violin() +
  labs(y = "Mean rating", fill = "Predicate type") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  scale_y_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy/\ncalm/\ncontrolled", "neutral", 
                                "completely\nhappy/\nexcited/\nin control")) + 
  scale_x_discrete(limits = c("Valence", "Arousal", "Dominance")) +
  scale_fill_discrete(labels = c("cognitive", "communicative with\nemotive component", "emotive",
                                 "evidential", "communicative without\nemotive component"))
ggsave("../graphs/valence-arousal-dominance-by-predicateType2.pdf", height = 8, width = 16)

# valence, arousal, dominance against projection ratings

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

mean.proj2 = left_join(d3, mean.proj, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj2) # 429

# projection by valence
ggplot(mean.proj2, aes(x = V.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean valence rating", y = "Mean projection rating",
    colour = "Predicate type") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy", "neutral", "completely\nhappy")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-valence.pdf", height = 8, width = 20)

# projection by valence faceted
ggplot(mean.proj2, aes(x = V.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean valence rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy", "neutral", "completely\nhappy")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType)
ggsave("../graphs/projection-by-valence-faceted.pdf", height = 8, width = 20)

# projection by valence faceted with emotive component
mean.proj2 %>%
  mutate(predicateType2 = recode(predicateType2, "cognitive" = "cognitive", 
                                 "emoComm" = "communicative with\nemotive component", 
                                 "emotive" = "emotive", 
                                 "evidential" = "evidential", 
                                 "nonEmoComm" = "communicative without\nemotive component")) %>%
ggplot(aes(x = V.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean valence rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\nunhappy", "neutral", "completely\nhappy")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType2)
ggsave("../graphs/projection-by-valence-faceted2.pdf", height = 8, width = 20)

# projection by arousal
ggplot(mean.proj2, aes(x = A.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean arousal rating", y = "Mean projection rating",
    colour = "Predicate type") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-arousal.pdf", height = 8, width = 20)

# projection by arousal faceted
ggplot(mean.proj2, aes(x = A.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean arousal rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType)
ggsave("../graphs/projection-by-arousal-faceted.pdf", height = 8, width = 20)

# projection by arousal faceted with emotive component
mean.proj2 %>%
  mutate(predicateType2 = recode(predicateType2, "cognitive" = "cognitive", 
                                 "emoComm" = "communicative with\nemotive component", 
                                 "emotive" = "emotive", 
                                 "evidential" = "evidential", 
                                 "nonEmoComm" = "communicative without\nemotive component")) %>%
ggplot(aes(x = A.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean arousal rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncalm", "neutral", "completely\nexcited")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType2)
ggsave("../graphs/projection-by-arousal-faceted2.pdf", height = 8, width = 20)

# projection by dominance
ggplot(mean.proj2, aes(x = D.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean dominance rating", y = "Mean projection rating",
    colour = "Predicate type") +
  theme(legend.position = "top",
        panel.spacing.x = unit(1, "cm"),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncontrolled", "neutral", "completely\nin control")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1))
ggsave("../graphs/projection-by-dominance.pdf", height = 8, width = 20)

# projection by dominance faceted
ggplot(mean.proj2, aes(x = D.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean dominance rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncontrolled", "neutral", "completely\nin control")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType)
ggsave("../graphs/projection-by-dominance-faceted.pdf", height = 8, width = 20)

# projection by dominance faceted with emotive component
mean.proj2 %>%
  mutate(predicateType2 = recode(predicateType2, "cognitive" = "cognitive", 
                                 "emoComm" = "communicative with\nemotive component", 
                                 "emotive" = "emotive", 
                                 "evidential" = "evidential", 
                                 "nonEmoComm" = "communicative without\nemotive component")) %>%
ggplot(aes(x = D.Mean.Sum, y = Mean.Proj)) +
  geom_point(aes(colour = predicateType2)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean dominance rating", y = "Mean projection rating") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(vjust = -2),
        panel.grid.minor.y = element_blank(), 
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(1,9), breaks = c(1, 5, 9), minor_breaks = seq(1:9), 
                     labels = c("completely\ncontrolled", "neutral", "completely\nin control")) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  facet_grid(. ~ predicateType2)
ggsave("../graphs/projection-by-dominance-faceted2.pdf", height = 8, width = 20)

# predicates in both active and passive sentence frames with same predicate type
d3[duplicated(d3[,cbind(2,4)]),] %>%
  select(verb, predicateType)

#       verb predicateType
# 195 grieve       emotive
# 251 marvel       emotive
# 276  panic       emotive
# 427  worry       emotive

# Of the 101 emotives with valence/arousal/dominance ratings, four predicates occur in both
# sentence frames in the MV data set. The "active" and "passive voice" versions of these 
# predicates were assigned the same valence/arousal/dominance ratings. Due to the small number
# of these cases, this should not affect the overall distributions plotted above.


# H2: Eventive vs stative ----
# plot projection by dynamicity to see if dynamicity modulates projection

# create dataset for projection inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291

# create predicateType and Dynamicity columns
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"),
         Dynamicity = case_when(stative_predicate == "yes" ~ "stative",
                                dynamic_predicate == "yes" ~ "dynamic",
                                TRUE ~ "unclear"))

# how many predicates in which predicateType and with which Dynamicity?
d.proj %>%
  select(predicateType, verb, Dynamicity) %>% 
  unique() %>% 
  group_by(predicateType, Dynamicity) %>% 
  summarize(count=n()) 

mean.proj = d.proj %>%
  group_by(predicateType, Dynamicity) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, predicateType = fct_reorder(as.factor(predicateType),Mean.Proj))
mean.proj
nrow(mean.proj) #11

ggplot(mean.proj, aes(x=predicateType, y=Mean.Proj, color = Dynamicity)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  geom_hline(yintercept=0) +
  theme(legend.position="top",
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-.2,1),breaks = c(-.2,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate type")
ggsave("../graphs/projection-by-predicateType-and-Dynamicity.pdf",height=4,width=5)


# H3: There is a positive correlation between veridicality and projection ratings ----

# create datasets for projection and veridicality inferences
d.proj = droplevels(subset(d,d$polarity == "negative" | d$conditional2 == "conditional"))
nrow(d.proj) #16291
d.verid = droplevels(subset(d,d$polarity != "negative" & d$conditional2 != "conditional"))
nrow(d.verid) #5401

# create predicateType column
d.proj = d.proj %>%
  mutate(predicateType = case_when(communicative == "yes" & private == "yes" ~ "comPriv",
                                   communicative == "yes" ~ "communicative",   
                                   emotive == "yes" ~ "emotive",
                                   cognitive == "yes" ~ "cognitive",
                                   evidential == "yes" ~ "evidential",
                                   TRUE ~ "other"))

# calculate by-predicate projection means 
mean.proj = d.proj %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
mean.proj
nrow(mean.proj) #544
levels(mean.proj$verb_renamed)

# add predicateType, verb and voice to the means
tmp = d.proj %>%
  #filter(voice == "active") %>%
  select(c(verb,verb_renamed,voice,predicateType)) %>%
  distinct(verb,verb_renamed,voice,predicateType)
nrow(tmp) #544

mean.proj = left_join(mean.proj, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Proj))
nrow(mean.proj) #544

# calculate by-predicate veridicality means 
mean.verid = d.verid %>%
  group_by(verb_renamed) %>%
  summarize(Mean.Verid = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Verid = Mean.Verid - CILow, YMax.Verid = Mean.Verid + CIHigh, verb_renamed = fct_reorder(as.factor(verb_renamed),Mean.Verid))
mean.verid
nrow(mean.verid) #544
levels(mean.verid$verb_renamed)

mean.proj.verid = mean.proj %>%
  inner_join(mean.verid,by=("verb_renamed"))
mean.proj.verid

# remove "other" and "comPriv" predicates
mean.proj.verid = mean.proj.verid %>%
  filter(predicateType != "other" & predicateType != "comPriv")
nrow(mean.proj.verid) #524

# Mean projection by mean veridicality
ggplot(mean.proj.verid, aes(x=Mean.Verid,y=Mean.Proj, group = predicateType)) +
  geom_point(shape=21) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbarh(aes(xmin=YMin.Verid,xmax=YMax.Verid),alpha=.8,color="gray") +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),alpha=.8,color="gray") +
  #geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  xlab("Mean veridicality rating") +
  ylab("Mean projection rating") +
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(-1,1)) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none", axis.title.x = element_text(color="black", size=14), axis.title.y = element_text(color="black", size=14))
ggsave("../graphs/projection-by-veridicality-with-CIs.pdf", height = 4,width = 5)

ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, color = predicateType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean veridicality rating", y = "Mean projection rating",
    color = "Predicate type") +
  theme(legend.position = "top") +
  coord_fixed(ratio = 1)
ggsave("../graphs/projection-by-veridicality.pdf", height = 4,width = 8)

# What are the predicates with negative mean projection rating and positive mean veridicality rating?
ggplot(mean.proj.verid, aes(x = Mean.Verid, y = Mean.Proj, color = predicateType)) +
  geom_point(alpha = .3) +
  geom_label_repel(data=subset(mean.proj.verid, Mean.Verid > 0 & Mean.Proj < 0),
                   aes(x = Mean.Verid, y = Mean.Proj, label=verb_renamed)) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.5) +
  labs(
    x = "Mean veridicality rating", y = "Mean projection rating",
    color = "Predicate type") +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  geom_abline(intercept=0,slope=0, color="gray", linetype="dashed") +
  #geom_abline(intercept=0,slope=0, color="gray", linetype="dashed") +
  geom_vline(xintercept=0, color="gray", linetype="dashed") +
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(-1,1)) +
  coord_fixed(ratio = 1) + 
  facet_wrap(~predicateType)
ggsave("../graphs/projection-by-veridicality2.pdf", height = 4,width = 6)




#  end of script for now ----

tmp = mean.proj %>% 
  left_join(mean.verid)
View(tmp)

ggplot(tmp, aes(x=Mean.Verid, y=Mean.Proj)) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  #scale_color_manual(values = colors) +
  #geom_line(aes(color=type), size=.5) + 
  geom_hline(yintercept=.3) +
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/H1.pdf",height=4,width=13)

# create data relevant to investigate projection (embedding is negation, conditional or both)
d_tmp <- droplevels(subset(d, d$polarity == "negative" | d$conditional2 == "conditional"))
t <- table(d_tmp$verb)
min(t)
mean(t)
max(t) #29-60 ratings per predicate under negation, cond or both
length(unique(d_tmp$verb)) #517 verbs
length(unique(d_tmp$participant)) #290 participants gave ratings

table(d_tmp$polarity, d_tmp$conditional2)

d_tmp$embedding = case_when(d_tmp$polarity == "positive" & d_tmp$conditional2 == "conditional" ~ "condPos",
                              d_tmp$polarity == "negative" & d_tmp$conditional2 == "conditional" ~ "condNeg",
                              d_tmp$polarity == "negative" & d_tmp$conditional2 == "matrix" ~ "matrixNeg",
                              TRUE ~ "error")

table(d_tmp$embedding)

# calculate mean for all 517 predicates, and embedding
p_means = d_tmp %>%
  group_by(verb, embedding) %>%
  summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
p_means
levels(p_means$verb) # verbs sorted by projectivity mean 

# create data subsets for the factives
p_meansFactives <- droplevels(subset(p_means,p_means$verb %in% factives))
p_meansFactives
str(p_meansFactives$verb)
levels(p_meansFactives$verb) # sorted by projectivity mean (pretend...be annoyed)

p_means <- mutate(p_means, verb = fct_reorder(verb, Mean, .fun = mean))
p_means <- mutate(p_means, embedding = fct_reorder(embedding, Mean, .fun = mean))
p_meansFactives <- mutate(p_meansFactives, verb = fct_reorder(verb, Mean, .fun = mean))
p_meansFactives <- mutate(p_meansFactives, embedding = fct_reorder(embedding, Mean, .fun = mean))

# Color blind friendly palette with black (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(p_meansFactives,aes(x=fct_reorder(verb, Mean), y=Mean, group = embedding, fill = embedding, color = embedding)) +
coord_cartesian(ylim=c(-1,1)) +
geom_point(aes(shape = embedding), size = 3) + 
scale_shape_manual(values=rev(c(23, 24, 25)),
                     labels=rev(c("cond/polar","negation","cond/neg/polar")),
                     name="Embedding") +
scale_fill_manual(values=rev(c("#000000", "#E69F00", "#56B4E9")),
                  labels=rev(c("cond/polar","negation","cond/neg/polar")),
                   name="Embedding") +
scale_colour_manual(values=rev(c("#000000", "#E69F00", "#56B4E9"))) +
#geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
geom_line() +
guides(color = "none") +
#geom_text_repel(data=p_meansFactives,aes(x=verb,y=Mean,label=verb,
                                           #color=VeridicalityGroup),segment.color="black",nudge_x=.2,nudge_y=-.6) +
#scale_color_manual(values=rev(c("tomato","black"))) +
scale_y_continuous(limits = c(-1,2),breaks = c(-1,0,1,2)) +
ylab("Mean projection rating") +
xlab("Predicate")
ggsave("../graphs/proj-by-both.pdf",height=4.7,width=12)
