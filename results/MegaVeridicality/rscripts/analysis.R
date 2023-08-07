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

# H1: emotive vs... ----

table(d$verb)

evidential = c("discover", "dream", "establish", "feel", "figure_out", "figure", "find_out",
               "generalize", "hallucinate", "hear", "imagine", "infer", "learn", "listen", 
               "notice", "observe", "overhear", "perceive", "piece_together", "realize",
               "reason_out", "recognize", "see")

emotive = c("trust", "desire", "fear", "worry", "mourn", "grieve", "frighten", "envy", "scare", "anger",
            "freak_out", "frustrate", "petrify", "puzzle", "regret", "devastate", "disappoint", "embitter", "shame",
            "dismay", "be annoyed", "content", "detest", "disturb", "hate", "traumatize", "enjoy", "embarrass",
            "irritate", "amuse", "elate", "love", "pain", "upset", "bother", "resent")
length(emotive) #37

cognitive = c("feel", "presuppose", "think", "believe", "suppose", "find", "discover", "see", "remember", "know", "comprehend",
              "conceive", "contemplate", "disbelieve", "dispute", "doubt", "estimate", "gather", "find_out", 
              "learn", "maintain", "notice", "realize", "recognize", "rediscover", "reveal", "see", "hear",
              "understand", "assume")
length(cognitive) #29

communicative = c("add", "advise", "alert", "argue", "announce", "assert", "assure", "babble", "bark", "cackle", "claim", 
                  "communicate", "comment", "confess", "confirm", "convey", "declare", "discuss", "elaborate", "email",
                  "express", "fax", "gab", "gloat", "gossip", "growl", "grunt", "gush", "hint", "inform", "insist", 
                  "lecture", "lie", "maintain", "mention", "mutter", "narrate", "note", "phone", "publicize", "publish",
                  "quip", "quote", "rant", "reassert", "recap", "reiterate", "repeat", "report", "say", "sing", "sob",
                  "state", "stress", "tell", "underline", "utter", "vow", "warn", "weep", "whimper", "whine", "whisper",
                  "wow", "yell")
length(communicative) #64

inferential = c("anticipate", "calculate", "compute", "conclude", "conjecture", "deduce", "derive", "establish", "estimate",
                "expect", "figure", "figure_out", "generalize", "infer", "piece_together", "pinpoint", "predict",
                "reason", "reason_out", "verify")
length(inferential) #20

# chose here for plotting
all_verbs <- c(emotive,cognitive,communicative,inferential,evidential)
#all_verbs <- c(evidential)
all_verbs
length(unique(all_verbs)) #148

# restrict data to verbs in all_verbs and to embedding under entailment-canceling operator
d.h1 <- droplevels(subset(d, d$verb %in% all_verbs))
d.h1 <- droplevels(subset(d.h1, d.h1$polarity == "negative"))

# by-predicate means
mean.proj = d.h1 %>%
  group_by(verb) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, verb = fct_reorder(as.factor(verb),Mean.Proj))
mean.proj

# mean.verid = d.verid %>%
#   group_by(verb) %>%
#   summarize(Mean.Verid = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
#   mutate(YMin.Verid = Mean.Verid - CILow, YMax.Verid = Mean.Verid + CIHigh, verb = fct_reorder(as.factor(verb),mean.proj$Mean.Proj))

nrow(mean.proj)
nrow(mean.verid)
levels(mean.proj$verb) # ordered by mean, not alphabetically

# # by-type mean
# d$type = as.factor(
#   ifelse(d$verb %in% cognitive, "cognitive", 
#          ifelse(d$verb %in% emotive, "emotive", 
#                 ifelse(d$verb %in% inferential, "inferential", "communicative"))))
# 
# table(d$type)
# summary(d)
# 
# mean.type = d %>%
#   group_by(type) %>%
#   summarize(Mean = mean(veridicality_num), CILow = ci.low(veridicality_num), CIHigh = ci.high(veridicality_num)) %>%
#   mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, type = fct_reorder(as.factor(type),Mean))
# 
# nrow(mean.type) #4

# color code the verbs
cols = data.frame(verb=levels(mean.proj$verb))
cols
nrow(cols)

cols$type = as.factor(
  ifelse(cols$verb %in% cognitive, "cognitive", 
         ifelse(cols$verb %in% emotive, "emotive", 
                ifelse(cols$verb %in% inferential, "inferential", 
                       ifelse(cols$verb %in% communicative, "communicative", "evidential")))))
cols
table(cols$type)

cols$Colors =  ifelse(cols$type == "emotive", "#D55E00", 
                      ifelse(cols$type == "cognitive", "#5b43c4", 
                             ifelse(cols$type == "communicative", "gray", 
                                    ifelse(cols$type == "inferential", "green", "black"))))
cols

# add type to the dataset
mean.proj$type = as.factor(
  ifelse(mean.proj$verb %in% cognitive, "cognitive", 
         ifelse(mean.proj$verb %in% emotive, "emotive", 
                ifelse(mean.proj$verb %in% inferential, "inferential", 
                       ifelse(mean.proj$verb %in% "communicative", "communicative", "evidential")))))

# mean.verid$type = as.factor(
#   ifelse(mean.verid$verb %in% cognitive, "cognitive", 
#          ifelse(mean.verid$verb %in% emotive, "emotive", 
#                 ifelse(mean.verid$verb %in% inferential, "inferential", 
#                        ifelse(mean.verid$verb %in% "communicative", "communicative", "evidential")))))


mean.proj$Colors =  ifelse(mean.proj$type == "emotive", "#D55E00", 
                           ifelse(mean.proj$type == "cognitive", "#5b43c4", 
                                  ifelse(mean.proj$type == "communicative", "gray", 
                                         ifelse(mean.proj$type == "inferential", "green", "black"))))

# mean.verid$Colors =  ifelse(mean.verid$type == "emotive", "#D55E00", 
#                             ifelse(mean.verid$type == "cognitive", "#5b43c4", 
#                                    ifelse(mean.verid$type == "communicative", "gray", 
#                                           ifelse(mean.verid$type == "inferential", "green", "black"))))

names(mean.proj)
ggplot(mean.proj, aes(x=verb, y=Mean.Proj,fill = type,color = type)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj),width=0) +
  #scale_color_manual(values = colors) +
  #geom_line(aes(color=type), size=.5) + 
  geom_hline(yintercept=.3) +
  theme(axis.ticks.x=element_blank(),legend.position="top") +
  #theme(axis.text.x = element_text(face = ifelse(levels(means$verb) %in% our_preds,"bold","plain"))) +
  theme(axis.text.x = element_text(color=cols$Colors,size=10,angle = 75, hjust = 1)) +
  scale_y_continuous(limits = c(-1,1),breaks = c(-1,0,1)) +
  ylab("Mean projection rating") +
  xlab("Predicate")
ggsave("../graphs/veridicality-by-predicate-and-type.pdf",height=4,width=13)
ggsave("../graphs/projection-for-evidential.pdf",height=4,width=13)

##  end of script for now

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
