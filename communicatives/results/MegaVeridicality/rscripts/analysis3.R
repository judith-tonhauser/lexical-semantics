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
theme_set(theme_bw())

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
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)
nrow(tmp) # 544

mean.proj.all <- left_join(mean.proj.all, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

mean.proj.all %>%
  group_by(predicateType2) %>%
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
# 3 emotive          149
# 4 evidential        87
# 5 nonEmoComm       185

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

ggplot(mean.proj.bt, aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicateType.pdf", height = 4, width = 10)

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
        axis.title.x = element_text(vjust = -1),
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
ggsave("../graphs/projection-by-predicateType2.pdf", height = 4, width = 10)

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


# C Communicatives ----
## C.1 overall ----
### C.1.1 by-predicateType ----
#### plot ----
# calculate by-predicateType means
mean.proj.comm.bt <-  d.proj.comm %>%
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj.comm.bt) # 3

ggplot(mean.proj.comm.bt, aes(x = commType, y = Mean.Proj, colour = commType)) +
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
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132)) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state\nchanging", "discourse\nparticipation", "pure")) + 
  scale_colour_manual(values = c("pure" = "orange", 
                                 "discourse participation" = "darkorange", 
                                 "state changing" = "orangered"))
ggsave("../graphs/projection-by-predicateType-commType.pdf", height = 4, width = 10)

### C.1.2 by-predicate ----
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
lm(Mean.Proj ~ fct_relevel(sayVerbType, "mode verb"), data = mean.proj.comm) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   ***
# mode verb             ***
# say                   n.s.

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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


## X VAD ratings ----

# load valence and arousal data
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

# Warriner et al.' (2013) dataset contains valence, arousal and dominance ratings 
# for 388 of the predicates in the MV dataset which belong to (only) one of our 5
# predicate types and have a mean acceptability rating greater than 4. 

mean.proj.vad %>% 
  group_by(predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType     n
#   <chr>         <int>
# 1 cognitive        47
# 2 communicative   177
# 3 emotive          97
# 4 evidential       67

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

### X.01 rescale V + A + D ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings are rescaled to range from 0 to 1. 
new.scale <- mean.proj.vad %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))

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

### X.02 data frame for ordinal models ----
d.proj.vad = w2 %>% 
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


### X.03 duplicates ----
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
#   nrow() # 98

# Of the 98 emotives with valence/arousal/dominance ratings, four predicates occur 
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
ggsave("../graphs/valence-by-predicate.pdf", height = 4, width = 13)

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
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### X.1.2 by predicate type ----
##### X.1.2.1 overall ----
###### plots ----
# calculate valence by predicateType means
mean.valence <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Valence))
mean.valence

ggplot(mean.valence, aes(x = predicateType, y = Mean.Valence, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_colour_manual(values = cols)
ggsave("../graphs/valence-by-predicateType.pdf", height = 8, width = 10)

# calculate valence by predicateType2 means (communicatives with/without emotive component)
mean.valence2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2.pdf", height = 8, width = 10)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.35654    0.03333  10.697  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.04297    0.04154  -1.035 0.301514    
# fct_relevel(predicateType2, "emoComm")emotive     0.08545    0.03749   2.279 0.023198 *  
# fct_relevel(predicateType2, "emoComm")evidential -0.13926    0.03911  -3.561 0.000416 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.15133    0.03605  -4.198 3.35e-05 ***

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
ggsave("../graphs/valence-by-predicateType-and-direction.pdf", height = 8, width = 10)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

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
ggsave("../graphs/arousal-by-predicate.pdf", height = 4, width = 13)

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
ggsave("../graphs/arousal-by-predicate2.pdf", height = 4, width = 13)


#### X.2.2 by predicate type ----
##### X.2.2.1 overall ----
###### plots ----
# calculate arousal by predicateType means
mean.arousal <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Arousal))
mean.arousal

ggplot(mean.arousal, aes(x = predicateType, y = Mean.Arousal, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_colour_manual(values = cols)
ggsave("../graphs/arousal-by-predicateType.pdf", height = 8, width = 10)

# calculate arousal by predicateType2 means (communicatives with/without emotive component)
mean.arousal2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
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
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2.pdf", height = 8, width = 10)

###### linear model ----
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

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
                          colour = V.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean arousal rating",
       colour = "Direction of valence") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "orangered", "positive" = "seagreen3"))
ggsave("../graphs/arousal-by-predicateType-and-direction-of-valence.pdf", height = 8, width = 10)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()

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
ggsave("../graphs/valence-by-predicate2.pdf", height = 4, width = 13)

#### X.3.2 by predicate type ----
##### X 3.2.1 overall ----
###### plots ----
# calculate dominance by predicateType2 means (communicatives with/without emotive component)
mean.dominance <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
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
ggsave("../graphs/dominance-by-predicateType2.pdf", height = 8, width = 10)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.19404    0.02789   6.958 1.48e-11 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.06080    0.03476   1.749   0.0810 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.05015    0.03137   1.598   0.1107    
# fct_relevel(predicateType2, "emoComm")evidential  0.07259    0.03272   2.218   0.0271 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.01305    0.03016   0.433   0.6655  



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
                          colour = V.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       colour = "Direction of valence") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "orangered", "positive" = "seagreen3"))
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-valence.pdf", height = 8, width = 10)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

##### X.3.2.3 with 'direction' of dominance ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance3 <- new.scale %>%
  group_by(predicateType2, D.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance3
nrow(mean.dominance3) # 10

ggplot(mean.dominance3, aes(x = predicateType2, y = Mean.Dominance, 
                            colour = D.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       colour = "'Direction' of dominance") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "deeppink3", "positive" = "gold4"))
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-dominance.pdf", height = 8, width = 10)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * D.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

###### tables: what is "direction of dominance"? ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
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
# 1 cognitive         37
# 2 emoComm           12
# 3 emotive           43
# 4 evidential        55
# 5 nonEmoComm       111

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
# 10    be worried      0.4575
# 11         worry      0.4575

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
# 4 evidential        12
# 5 nonEmoComm        41


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
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = cols2, face = "bold"),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence / arousal / dominance rating") +
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
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal", "Dominance")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-arousal-dominance-by-communicative-emotive.pdf", height = 6, width = 10)

#### X.4.3 rating correlations ----
##### X.4.3.1 valence + arousal ----
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

###### linear models -----
lm(V.Mean.Sum2 ~ A.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.01596    0.03327   0.480    0.632    
# A.Mean.Sum2  0.66335    0.07764   8.544 2.94e-16 ***

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

lm(A.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.343505   0.009699  35.417  < 2e-16 ***
# V.Mean.Sum2 0.237155   0.027758   8.544 2.94e-16 ***

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      


##### X.4.3.2 valence + dominance ----
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

###### linear models -----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18530    0.01775  10.437  < 2e-16 ***
# D.Mean.Sum2  0.44915    0.06515   6.894 2.18e-11 ***

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       
# emoComm         .
# emotive         
# evidential      
# nonEmoComm      

lm(D.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.16182    0.01223  13.231  < 2e-16 ***
# V.Mean.Sum2  0.24132    0.03500   6.894 2.18e-11 ***

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       
# emoComm         .
# emotive         
# evidential      
# nonEmoComm      


##### X.4.3.3 arousal + dominance ----
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

###### linear models -----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.40503    0.01123  36.052   <2e-16 ***
# D.Mean.Sum2  0.03065    0.04122   0.743    0.458 

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

lm(D.Mean.Sum2 ~ A.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.21266    0.02654   8.012 1.31e-14 ***
# A.Mean.Sum2  0.04606    0.06195   0.743    0.458 

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

##### X.4.3.4 direction of valence + direction of dominance ----
###### tables ----
contingency_table <- table(new.scale$D.Mean.Sum2.direction, new.scale$V.Mean.Sum2.direction)
print(contingency_table)
#          negative positive
# negative      111       19
# positive       62      196

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 129.23, df = 1, p-value < 2.2e-16

# The directions of dominance and valence seem highly correlated.

# distribution of direction within predicate types
new.scale %>% 
  count(predicateType2, D.Mean.Sum2.direction)
#    predicateType2 D.Mean.Sum2.direction   n
# 1       cognitive              negative  10 21% of cognitives
# 2       cognitive              positive  37
# 3         emoComm              negative  13 52% of emoComms
# 4         emoComm              positive  12
# 5         emotive              negative  54 56% of emotives
# 6         emotive              positive  43
# 7      evidential              negative  12 18% of evidentials
# 8      evidential              positive  55
# 9      nonEmoComm              negative  41 27% of nonEmoComms
# 10     nonEmoComm              positive 111

new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#    predicateType2 V.Mean.Sum2.direction  n
# 1       cognitive              negative 14 30% of cognitives
# 2       cognitive              positive 33
# 3         emoComm              negative 21 84% of emoComms
# 4         emoComm              positive  4
# 5         emotive              negative 63 65% of emotives
# 6         emotive              positive 34
# 7      evidential              negative 16 24% of evidentials
# 8      evidential              positive 51
# 9      nonEmoComm              negative 59 39% of nonEmoComms
# 10     nonEmoComm              positive 93

# The proportion of negative compared to posiitve valence predicates is higher than
# the proportion of negative vs positive dominance predicates in all predicate
# types. The largest difference is found in the communicatives with an emotive
# meaning component.

#### X.4.4 predicate types predicting VAD ratings ----
##### linear models ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       
# emoComm         ***
# emotive         
# evidential      
# nonEmoComm      

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       
# emoComm         ***
# emotive         
# evidential      
# nonEmoComm      

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       
# emoComm         ***
# emotive         
# evidential      
# nonEmoComm       

# All predicate types seem to be highly correlated with VAD ratings.


### X.5 projection: VAD against projection ratings ----
#### X.5.1 valence ----
##### X.5.1.1 overall ---- 
###### plots ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence.pdf", height = 6, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
ggsave("../graphs/projection-by-valence2.pdf", height = 6, width = 8)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-faceted.pdf", height = 6, width = 10)

# projection by valence faceted with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
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
# (Intercept)  0.26839    0.02368  11.334  < 2e-16 ***
# V.Mean.Sum2  0.45288    0.06777   6.683 8.11e-11 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         .

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
# 130         dream      cognitive -0.7435833
# 163     be fooled     evidential -0.6981746
# 85       daydream      cognitive -0.6785519
# 267       pretend     nonEmoComm -0.6781747
# 232     be misled     evidential -0.6722626
# 366    be tricked     evidential -0.6476087
# 39    be bothered        emotive  0.6347813
# 29  be astonished        emotive  0.6025800
# 152     fantasize      cognitive -0.5949041
# 195       imagine      cognitive -0.5865111

# predicates grouped by predicate type
cbind(new.scale, residual) %>% 
  group_by(predicateType2) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual) %>% 
  print(n = 50)
#   verb_renamed  predicateType2 residual
#   <fct>         <chr>             <dbl>
# 1 dream         cognitive        -0.744
# 2 daydream      cognitive        -0.679
# 3 fantasize     cognitive        -0.595
# 4 imagine       cognitive        -0.587
# 5 be deluded    cognitive        -0.558
# 6 picture       cognitive        -0.531
# 7 believe       cognitive        -0.501
# 8 think         cognitive        -0.492
# 9 hope          cognitive        -0.483
# 10 trust         cognitive        -0.455
# 11 quarrel       emoComm          -0.380
# 12 cry           emoComm           0.363
# 13 scream        emoComm          -0.293
# 14 flaunt        emoComm           0.229
# 15 pout          emoComm           0.209
# 16 implore       emoComm          -0.195
# 17 whine         emoComm           0.185
# 18 grunt         emoComm          -0.185
# 19 weep          emoComm           0.125
# 20 complain      emoComm           0.116
# 21 be bothered   emotive           0.635
# 22 be astonished emotive           0.603
# 23 be floored    emotive           0.582
# 24 be startled   emotive           0.559
# 25 be offended   emotive           0.505
# 26 resent        emotive           0.503
# 27 be astounded  emotive           0.484
# 28 be stunned    emotive           0.481
# 29 be jarred     emotive           0.479
# 30 be disgusted  emotive           0.475
# 31 be fooled     evidential       -0.698
# 32 be misled     evidential       -0.672
# 33 be tricked    evidential       -0.648
# 34 be promised   evidential       -0.554
# 35 be warned     evidential        0.487
# 36 be duped      evidential       -0.448
# 37 realize       evidential        0.435
# 38 spot          evidential        0.418
# 39 be informed   evidential        0.411
# 40 check         evidential       -0.411
# 41 pretend       nonEmoComm       -0.678
# 42 fake          nonEmoComm       -0.505
# 43 suggest       nonEmoComm       -0.497
# 44 joke          nonEmoComm       -0.495
# 45 feign         nonEmoComm       -0.464
# 46 apologize     nonEmoComm        0.449
# 47 reject        nonEmoComm       -0.434
# 48 lie           nonEmoComm       -0.397
# 49 claim         nonEmoComm       -0.385
# 50 promise       nonEmoComm       -0.354



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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.45237    0.02094  21.603  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.09372    0.02786  -3.364 0.000844 ***

lm(Mean.Proj ~ V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.28841    0.03608   7.994 1.50e-14 ***
# V.Mean.Sum2                                0.50945    0.09338   5.456 8.69e-08 ***
# V.Mean.Sum2.directionpositive             -0.01832    0.04790  -0.382    0.702    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.17444    0.13615  -1.281    0.201  

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
ggsave("../graphs/projection-by-valence-communicative-faceted.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-valence-sayverb-faceted.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-valence-modeverb-faceted.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-valence-saybymeans-faceted.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-arousal-faceted.pdf", height = 6, width = 10)

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
# (Intercept)  0.13827    0.04939   2.800  0.00537 ** 
# A.Mean.Sum2  0.63369    0.11527   5.497 6.97e-08 ***

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

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

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2  n
# 1               negative      cognitive 14
# 2               negative        emoComm 21
# 3               negative        emotive 63
# 4               negative     evidential 16
# 5               negative     nonEmoComm 59
# 6               positive      cognitive 33
# 7               positive        emoComm  4
# 8               positive        emotive 35
# 9               positive     evidential 53
# 10              positive     nonEmoComm 94

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
ggsave("../graphs/projection-by-arousal-communicative-faceted.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-arousal-sayverb-faceted.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-arousal-modeverb-faceted.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-arousal-saybymeans-faceted.pdf", height = 4, width = 9)


#### X.5.3 dominance ---- 
##### X.5.3.1 overall ----
###### plots ----
# projection by dominance with emotive component 
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

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.38541    0.02658  14.498   <2e-16 ***
# D.Mean.Sum2  0.06053    0.09755   0.621    0.535 

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm:        *
# nonEmoComm:     n.s.
# emotive:        n.s.
# evidential:     n.s.
# cognitive:      n.s.

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2.pdf", 
       height = 6, width = 10)

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2.pdf", 
       height = 6, width = 10)


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
ggsave("../graphs/projection-by-dominance-communicative-faceted.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-dominance-sayverb-faceted.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-dominance-modeverb-faceted.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-dominance-saybymeans-faceted.pdf", height = 4, width = 9)


#### X.5.4 valence + arousal ----
##### linear model ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.13254    0.04800   2.761  0.00603 ** 
# V.Mean.Sum2  0.35909    0.07294   4.923 1.26e-06 ***
# A.Mean.Sum2  0.39549    0.12200   3.242  0.00129 ** 

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
##### plot ----
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

vplot + aplot + dplot + plot_layout(guides = "collect", axis_titles = "collect")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence.pdf", height = 3.5, width = 12)

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

# W ----
mean.proj.comm %>% 
  filter(sayByMeansVerbType == "form") %>% 
  select(verb_renamed) %>% 
  print(n = Inf)

# two possible interpretation: CC was not said vs was not said in a certain way /
# with a certain attitude etc.

# emoComms: all
# commType discourse participation: mixed, more single interpretation
# commType pure: mixed, more focus-sensitive interpretation
# commType state changing: mixed
# say verbs discourse role: mainly single interpretation (exceptions: apologize, insist...)  
# mode verbs say-with-attitude: all
# say-by-means verbs sound: all, manner: all, form: none


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



# >>> NEGATION ONLY ----------------------------------------------------------------------------
# Modified copy of the code above, including only projection ratings based on items with negation as 
# embedding environment.
# Includes predicates with MEAN ACCEPTABILITY RATINGS greater than 4 based on negation-only items,
# i.e., those predicates that fall within this range of acceptability across all embedding environments 
# but not in the negation-only environment are EXCLUDED here. The number of predicates considered 
# here is therefore smaller than in the original code above.

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
library(ggforce)
library(ordinal)
library(patchwork)
theme_set(theme_bw())

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
d.proj <- droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

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
  group_by(polarity, conditional) %>% 
  count()
#   polarity conditional     n
#   <chr>    <chr>       <int>
# 1 negative False        5411

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
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)
nrow(tmp) # 544

mean.proj.all <- left_join(mean.proj.all, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

mean.proj.all %>%
  group_by(predicateType2) %>%
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
length(acc.preds) # 430

# acceptable communicatives
acc.comms <- mean.proj %>%
  filter(predicateType == "communicative" & Mean.Acc > 4) %>% 
  distinct(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
length(acc.comms) # 171

# by-predicate projection means for "acceptable" predicates
mean.proj.acc <- mean.proj %>% 
  filter(Mean.Acc > 4)
nrow(mean.proj.acc) # 430

# by-predicate projection means for "acceptable" communicatives
mean.proj.comm <- mean.proj.acc %>%
  filter(predicateType == "communicative")
nrow(mean.proj.comm) # 171

d.proj.acc <- d.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(d.proj.acc) # 4278

d.proj.comm <- d.proj.acc %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 1703


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
ggsave("../graphs/projection-by-acceptability-line-NO.pdf", height = 6, width = 8)

### ordinal model ----
clmm(as.factor(veridicality) ~ acceptability + (1 | participant), data = d.proj) %>% 
  summary()
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# acceptability  0.43314    0.02235   19.38   <2e-16 ***

## A.2 distributions ----
# how many items with which acceptability rating?
d.proj %>% 
  group_by(acceptability) %>% 
  summarise(count = n())
#   acceptability count
#           <int> <int>
# 1             1   152
# 2             2   440
# 3             3   697
# 4             4   702
# 5             5  1139
# 6             6  1115
# 7             7  1166

# distribution of ratings within each acceptability subgroup
o <- d.proj %>% 
  group_by(acceptability, veridicality) %>% 
  count() %>% 
  group_by(acceptability) %>%
  mutate(percentage =  n * 100 / sum(n)) %>% 
  print(n = Inf)
#   acceptability veridicality     n percentage
#           <int> <chr>        <int>      <dbl>
# 1             1 maybe          101      66.4 
# 2             1 no              23      15.1 
# 3             1 yes             28      18.4 
# 4             2 maybe          276      62.7 
# 5             2 no              42       9.55
# 6             2 yes            122      27.7 
# 7             3 maybe          446      64.0 
# 8             3 no              75      10.8 
# 9             3 yes            176      25.3 
# 10             4 maybe          428      61.0 
# 11             4 no              46       6.55
# 12             4 yes            228      32.5 
# 13             5 maybe          552      48.5 
# 14             5 no              86       7.55
# 15             5 yes            501      44.0 
# 16             6 maybe          448      40.2 
# 17             6 no              48       4.30
# 18             6 yes            619      55.5 
# 19             7 maybe          413      35.4 
# 20             7 no              49       4.20
# 21             7 yes            704      60.4  

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

## A.3 excluded predicates ----
# predicates with mean acceptability ratings of less than or equal to 4
mean.proj %>% 
  filter(! predicateType %in% c("comPriv", "other") & Mean.Acc <= 4) %>% 
  arrange(Mean.Acc) %>% 
  reframe(verb_renamed, Mean.Acc, Mean.Proj, predicateType2) %>% 
  print(n = Inf)
# verb_renamed  Mean.Acc Mean.Proj predicateType2
# <fct>            <dbl>     <dbl> <chr>         
# 1 bark               2.33     0.111 nonEmoComm    
# 2 snap               2.7      0     emoComm       
# 3 vote               2.7      0.3   nonEmoComm    
# 4 be nonplussed      2.9      0.5   emotive       
# 5 negotiate          2.9      0.3   nonEmoComm    
# 6 stutter            2.9      0.2   nonEmoComm    
# 7 be bet             3        0.222 evidential    
# 8 be cheered         3.1      0.6   emotive       
# 9 be tricked         3.1     -0.1   evidential    
# 10 curse              3.1      0     emoComm       
# 11 gab                3.1      0.4   nonEmoComm    
# 12 okay               3.1      0.4   nonEmoComm    
# 13 quip               3.1      0     nonEmoComm    
# 14 growl              3.11     0.111 emoComm       
# 15 sing               3.11     0     nonEmoComm    
# 16 be tweeted         3.2      0.1   evidential    
# 17 scribble           3.2      0     nonEmoComm    
# 18 will               3.2      0     cognitive     
# 19 simulate           3.3      0.3   nonEmoComm    
# 20 muse               3.4      0.1   cognitive     
# 21 rediscover         3.4      0     evidential    
# 22 request            3.4      0.2   nonEmoComm    
# 23 underscore         3.4      0.1   nonEmoComm    
# 24 grunt              3.5      0.1   emoComm       
# 25 maintain           3.5      0     nonEmoComm    
# 26 underline          3.5      0.3   nonEmoComm    
# 27 be quoted          3.6      0.1   evidential    
# 28 prophesy           3.6      0.2   nonEmoComm    
# 29 spout              3.6      0.3   nonEmoComm    
# 30 whimper            3.6      0.3   emoComm       
# 31 yell               3.6      0     nonEmoComm    
# 32 add                3.7      0.2   nonEmoComm    
# 33 be invigorated     3.7      0.8   emotive       
# 34 be maddened        3.7      0.9   emotive       
# 35 cackle             3.7      0.5   emoComm       
# 36 chant              3.7      0     nonEmoComm    
# 37 hoot               3.7      0.1   emoComm       
# 38 jest               3.7      0.4   nonEmoComm    
# 39 lie                3.7      0.5   nonEmoComm    
# 40 measure            3.7      0.1   evidential    
# 41 rationalize        3.7      0.3   evidential    
# 42 underestimate      3.7      0.2   cognitive     
# 43 be befuddled       3.8      0.4   emotive       
# 44 be charmed         3.8      0.6   emotive       
# 45 be stupefied       3.8      0.4   emotive       
# 46 elect              3.8      0.1   evidential    
# 47 giggle             3.8     -0.1   emoComm       
# 48 lecture            3.8      0.3   nonEmoComm    
# 49 manufacture        3.8      0.2   nonEmoComm    
# 50 mutter             3.8      0     emoComm       
# 51 reason             3.8     -0.1   evidential    
# 52 rule               3.8     -0.2   nonEmoComm    
# 53 shriek             3.8      0.2   emoComm       
# 54 volunteer          3.8      0.1   nonEmoComm    
# 55 be signalled       3.9      0.4   evidential    
# 56 be tantalized      3.9      0.5   emotive       
# 57 categorize         3.9      0.2   evidential    
# 58 hope               3.9      0.2   cognitive     
# 59 howl               3.9      0.3   emoComm       
# 60 mumble             3.9      0     nonEmoComm    
# 61 presuppose         3.9     -0.1   cognitive     
# 62 require            3.9     -0.1   cognitive     
# 63 snitch             3.9      0.3   nonEmoComm    
# 64 summarize          3.9      0.3   nonEmoComm    
# 65 test               3.9     -0.1   nonEmoComm    
# 66 be galled          4        0.667 emotive       
# 67 chatter            4        0.3   nonEmoComm    
# 68 depict             4        0.1   nonEmoComm    
# 69 derive             4       -0.1   evidential    
# 70 rant               4        0.4   emoComm       
# 71 reply              4       -0.2   nonEmoComm    
# 72 sketch             4        0.3   nonEmoComm    
# 73 submit             4        0     nonEmoComm  

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
# 1 cognitive               53             6              11.3 
# 2 emoComm                 33            12              36.4 
# 3 emotive                148             9               6.08
# 4 evidential              85            12              14.1 
# 5 nonEmoComm             184            34              18.5 



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

ggplot(mean.proj.bt, aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicateType-acc-NO.pdf", height = 4, width = 10)

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
        axis.title.x = element_text(vjust = -1),
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
ggsave("../graphs/projection-by-predicateType2-acc-NO.pdf", height = 4, width = 10)

### distribution ----
mean.proj.acc %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
# 1 cognitive         47
# 2 emoComm           21
# 3 emotive          139
# 4 evidential        73
# 5 nonEmoComm       150

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
ggsave("../graphs/projection-by-predicate-communicative-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoComm-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-nonEmoComm-acc-NO.pdf", height = 4, width = 13)


# C Communicatives ----
## C.1 overall ----
### C.1.1 by-predicateType ----
#### plot ----
# calculate by-predicateType means
mean.proj.comm.bt <-  d.proj.comm %>%
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj.comm.bt) # 3

ggplot(mean.proj.comm.bt, aes(x = commType, y = Mean.Proj, colour = commType)) +
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
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132)) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state\nchanging", "discourse\nparticipation", "pure")) + 
  scale_colour_manual(values = c("pure" = "orange", 
                                 "discourse participation" = "darkorange", 
                                 "state changing" = "orangered"))
ggsave("../graphs/projection-by-predicateType-commType-acc-NO.pdf", height = 4, width = 10)

### C.1.2 by-predicate ----
# How many of which type of communicative predicate?
mean.proj.comm %>%
  select(commType, verb_renamed) %>% 
  unique() %>% 
  group_by(commType) %>% 
  summarize(count=n())
#   commType                count
#   <chr>                   <int>
# 1 discourse participation    92
# 2 pure                       60
# 3 state changing             19

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
ggsave("../graphs/projection-by-communicative-predicate-acc-NO.pdf", height = 4, width = 10)


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
# 2 communicative no         81
# 3 communicative yes        90


### C.3.1 types of communicatives ----
#### distribution ----
d.proj.comm %>% 
  group_by(sayVerb) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerb     n
#   <chr>   <int>
# 1 no         81
# 2 yes        90

#### plot ----
mean.proj.commsay <- d.proj.comm %>%
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-communication-type-acc-NO.pdf", height = 4, width = 10)

### C.3.2 types of say verbs ----
#### distribution ----
d.proj.comm %>% 
  filter((sayVerb == "yes")) %>%  
  group_by(sayVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayVerbType             n
#   <chr>               <int>
# 1 discourse role verb    48
# 2 mode verb              41
# 3 say                     1

#### plots ----
mean.proj.saytype <- d.proj.comm %>%
  filter(sayVerb == "yes") %>%  
  group_by(sayVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-sayverb-type-acc-NO.pdf", height = 4, width = 10)

mean.proj.saytype2 <- d.proj.comm %>%
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-sayverb-type2-acc-NO.pdf", height = 4, width = 10)


#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayVerbType, "say"), data = mean.proj.comm) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   **
# mode verb             ***
# say                   n.s.

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "say") +  (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(sayVerbType, "discourse role verb")mode verb  0.74500    0.16998   4.383 1.17e-05 ***
# fct_relevel(sayVerbType, "discourse role verb")say        0.09779    0.75477   0.130    0.897   

# fct_relevel(sayVerbType, "mode verb")discourse role verb  -0.7450     0.1700  -4.383 1.17e-05 ***
# fct_relevel(sayVerbType, "mode verb")say                  -0.6472     0.7561  -0.856    0.392  

# fct_relevel(sayVerbType, "say")discourse role verb -0.09779    0.75506  -0.130    0.897
# fct_relevel(sayVerbType, "say")mode verb            0.64721    0.75637   0.856    0.392

### C.3.3 types of mode verbs ----
#### distribution ----
d.proj.comm %>% 
  filter(sayVerbType == "mode verb") %>%  
  group_by(modeVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   modeVerbType          n
#   <chr>             <int>
# 1 say-by-means         31
# 2 say-with-attitude    10

#### plot ----
mean.proj.modetype <- d.proj.comm %>%
  filter(sayVerb == "yes" & verb_renamed %in% acc.comms) %>%  
  group_by(sayVerbType, modeVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-modeverb-type-acc-NO.pdf", height = 4, width = 10)

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-with-attitude") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude   0.4639     0.2915   1.591    0.112

# fct_relevel(modeVerbType, "say-with-attitude")say-by-means  -0.4639     0.2915  -1.591    0.112

### C.3.4 types of say-by-means verbs ----
#### distribution ----
d.proj.comm %>% 
  filter(modeVerbType == "say-by-means") %>%  
  group_by(sayByMeansVerbType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   sayByMeansVerbType     n
#   <chr>              <int>
# 1 form                  13
# 2 manner                16
# 3 sound                  2

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
ggsave("../graphs/projection-by-saybymeansverb-type-acc-NO.pdf", height = 4, width = 10)

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayByMeansVerbType, "form") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(sayByMeansVerbType, "form")manner  0.39531    0.27905   1.417    0.157
# fct_relevel(sayByMeansVerbType, "form")sound   0.06601    0.56570   0.117    0.907

# fct_relevel(sayByMeansVerbType, "manner")form   -0.3953     0.2791  -1.417    0.157
# fct_relevel(sayByMeansVerbType, "manner")sound  -0.3293     0.5583  -0.590    0.555

# fct_relevel(sayByMeansVerbType, "sound")form   -0.06601    0.56570  -0.117    0.907
# fct_relevel(sayByMeansVerbType, "sound")manner  0.32929    0.55824   0.590    0.555

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### C.3.5 overall ----
#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, commType, sayVerbType, modeVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType         modeVerbType          n
#   <chr>          <chr>                   <chr>               <chr>             <int>
# 1 emoComm        discourse participation mode verb           say-with-attitude     4
# 2 emoComm        discourse participation NA                  NA                    2
# 3 emoComm        pure                    mode verb           say-by-means         11
# 4 emoComm        pure                    mode verb           say-with-attitude     2
# 5 emoComm        pure                    NA                  NA                    2
# 6 nonEmoComm     discourse participation discourse role verb NA                   39
# 7 nonEmoComm     discourse participation mode verb           say-with-attitude     1
# 8 nonEmoComm     discourse participation NA                  NA                   46
# 9 nonEmoComm     pure                    discourse role verb NA                    5
# 10 nonEmoComm     pure                    mode verb           say-by-means         20
# 11 nonEmoComm     pure                    mode verb           say-with-attitude     3
# 12 nonEmoComm     pure                    say                 NA                    1
# 13 nonEmoComm     pure                    NA                  NA                   16
# 14 nonEmoComm     state changing          discourse role verb NA                    4
# 15 nonEmoComm     state changing          NA                  NA                   15

#### plot ----
mean.proj.overall <- d.proj.comm %>%
  filter(sayVerb == "yes") %>%  
  group_by(sayVerbType, modeVerbType, sayByMeansVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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
ggsave("../graphs/projection-by-saybymeansverb-type2-acc-NO.pdf", height = 4, width = 10)


## X VAD ratings ----

# load valence and arousal data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  rename(verb = Word) %>%
  inner_join(d.proj.acc, by = "verb")
nrow(w2) # 3513
n_distinct(w2$verb) # 339
n_distinct(w2$verb_renamed) # 353

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
nrow(mean.proj.vad) # 353

# Warriner et al.' (2013) dataset contains valence, arousal and dominance ratings 
# for 353 of the predicates in the MV dataset which belong to (only) one of our 5
# predicate types and have a mean acceptability rating greater than 4 under negation. 

mean.proj.vad %>% 
  group_by(predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType     n
#   <chr>         <int>
# 1 cognitive        42
# 2 communicative   151
# 3 emotive          96
# 4 evidential       64

mean.proj.vad %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()  
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         42
# 2 emoComm           19
# 3 emotive           96
# 4 evidential        64
# 5 nonEmoComm       132

### X.01 rescale V + A + D ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings are rescaled to range from 0 to 1. 
new.scale <- mean.proj.vad %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))

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

### X.02 data frame for ordinal models ----
d.proj.vad = w2 %>% 
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
ggsave("../graphs/valence-by-predicate-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/valence-by-predicate2-acc-NO.pdf", height = 4, width = 13)

#### X.1.2 by predicate type ----
##### X.1.2.1 overall ----
###### plots ----
# calculate valence by predicateType means
mean.valence <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Valence))
mean.valence

ggplot(mean.valence, aes(x = predicateType, y = Mean.Valence, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_colour_manual(values = cols)
ggsave("../graphs/valence-by-predicateType-acc-NO.pdf", height = 8, width = 10)

# calculate valence by predicateType2 means (communicatives with/without emotive component)
mean.valence2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence2

ggplot(mean.valence2, aes(x = predicateType2, y = Mean.Valence, colour = predicateType2)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Valence, ymax = YMax.Valence), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/valence-by-predicateType2-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.38132    0.03908   9.757  < 2e-16 ***
# fct_relevel(predicateType2, "emoComm")cognitive  -0.06024    0.04710  -1.279 0.201718    
# fct_relevel(predicateType2, "emoComm")emotive     0.06900    0.04277   1.613 0.107647    
# fct_relevel(predicateType2, "emoComm")evidential -0.15530    0.04451  -3.489 0.000546 ***
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.18433    0.04180  -4.410 1.38e-05 ***

##### X.1.2.2 with direction of valence ----
###### plot ----
# calculate valence by predicateType2 means and direction of valence
mean.valence3 <- new.scale %>%
  group_by(predicateType2, V.Mean.Sum2.direction) %>%
  summarize(Mean.Valence = mean(V.Mean.Sum2), 
            CILow = ci.low(V.Mean.Sum2), 
            CIHigh = ci.high(V.Mean.Sum2)) %>%
  mutate(YMin.Valence = Mean.Valence - CILow, 
         YMax.Valence = Mean.Valence + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Valence))
mean.valence3
nrow(mean.valence3) # 10

# valence by predicate type and direction of valence
ggplot(mean.valence3, aes(x = predicateType2, y = Mean.Valence, 
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
ggsave("../graphs/valence-by-predicateType-and-direction-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "evidential") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

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
ggsave("../graphs/arousal-by-predicate-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/arousal-by-predicate2-acc-NO.pdf", height = 4, width = 13)


#### X.2.2 by predicate type ----
##### X.2.2.1 overall ----
###### plots ----
# calculate arousal by predicateType means
mean.arousal <- new.scale %>%
  group_by(predicateType) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), 
            CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
         predicateType = fct_reorder(as.factor(predicateType), Mean.Arousal))
mean.arousal

ggplot(mean.arousal, aes(x = predicateType, y = Mean.Arousal, colour = predicateType)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean arousal rating") +
  scale_colour_manual(values = cols)
ggsave("../graphs/arousal-by-predicateType-acc-NO.pdf", height = 8, width = 10)

# calculate arousal by predicateType2 means (communicatives with/without emotive component)
mean.arousal2 <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Arousal = mean(A.Mean.Sum2), 
            CILow = ci.low(A.Mean.Sum2), 
            CIHigh = ci.high(A.Mean.Sum2)) %>%
  mutate(YMin.Arousal = Mean.Arousal - CILow, 
         YMax.Arousal = Mean.Arousal + CIHigh, 
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
  scale_colour_manual(values = cols2)
ggsave("../graphs/arousal-by-predicateType2-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "evidential"), new.scale) %>% 
  summary()
# predicateType2  significance
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

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
                          colour = V.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Arousal, ymax = YMax.Arousal), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean arousal rating",
       colour = "Direction of valence") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "orangered", "positive" = "seagreen3"))
ggsave("../graphs/arousal-by-predicateType-and-direction-of-valence-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()

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
ggsave("../graphs/valence-by-predicate2-acc-NO.pdf", height = 4, width = 13)

#### X.3.2 by predicate type ----
##### X 3.2.1 overall ----
###### plots ----
# calculate dominance by predicateType2 means (communicatives with/without emotive component)
mean.dominance <- new.scale %>%
  group_by(predicateType2) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
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
ggsave("../graphs/dominance-by-predicateType2-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.21539    0.03301   6.525  2.4e-10 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.04121    0.03978   1.036    0.301    
# fct_relevel(predicateType2, "emoComm")emotive     0.03476    0.03613   0.962    0.337    
# fct_relevel(predicateType2, "emoComm")evidential  0.04445    0.03759   1.182    0.238    
# fct_relevel(predicateType2, "emoComm")nonEmoComm -0.01320    0.03531  -0.374    0.709 



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
                            colour = V.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       colour = "Direction of valence") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "orangered", "positive" = "seagreen3"))
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-valence-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "evidential") * V.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

##### X.3.2.3 with 'direction' of dominance ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance3 <- new.scale %>%
  group_by(predicateType2, D.Mean.Sum2.direction) %>%
  summarize(Mean.Dominance = mean(D.Mean.Sum2), CILow = ci.low(D.Mean.Sum2), 
            CIHigh = ci.high(D.Mean.Sum2)) %>%
  mutate(YMin.Dominance = Mean.Dominance - CILow, YMax.Dominance = Mean.Dominance + CIHigh, 
         predicateType2 = fct_reorder(as.factor(predicateType2), Mean.Dominance))
mean.dominance3
nrow(mean.dominance3) # 10

ggplot(mean.dominance3, aes(x = predicateType2, y = Mean.Dominance, 
                            colour = D.Mean.Sum2.direction)) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = YMin.Dominance, ymax = YMax.Dominance), width = 0, 
                position = position_dodge(0.2)) +
  theme(legend.position = "top",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y ="Mean dominance rating",
       colour = "'Direction' of dominance") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(values = c("negative" = "deeppink3", "positive" = "gold4"))
ggsave("../graphs/Dominance-by-predicateType-and-direction-of-dominance-acc-NO.pdf", height = 8, width = 10)

###### linear model ----
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm") * D.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

###### tables: what is "direction of dominance"? ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
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
# 1 cognitive         33
# 2 emoComm           10
# 3 emotive           42
# 4 evidential        52
# 5 nonEmoComm        99

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
# 10    be worried      0.4575
# 11         worry      0.4575

new.scale %>% 
  filter(D.Mean.Sum2.direction == "negative") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive          9
# 2 emoComm            9
# 3 emotive           54
# 4 evidential        12
# 5 nonEmoComm        33


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
ggsave("../graphs/valence-by-arousal-acc-NO.pdf", height = 8, width = 12)

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
ggsave("../graphs/valence-by-dominance-acc-NO.pdf", height = 8, width = 12)

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
ggsave("../graphs/arousal-by-dominance-acc-NO.pdf", height = 8, width = 12)


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
ggsave("../graphs/valence-arousal-by-predicateType-acc-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/valence-arousal-by-predicateType2-acc-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/valence-dominance-by-predicateType-acc-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/valence-dominance-by-predicateType2-acc-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/arousal-dominance-by-predicateType2-acc-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/valence-arousal-dominance-by-predicateType2-acc-NO.pdf", height = 6, width = 10)

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
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = cols2, face = "bold"),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate type",
       y = "Mean valence / arousal / dominance rating") +
  scale_x_discrete(labels = predicateType2_names) +
  scale_colour_manual(limits = c("valence", "arousal", "dominance"),
                      values = c("blue3", "red3", "grey30"))
ggsave("../graphs/valence-arousal-dominance-by-predicateType2-2-acc-NO.pdf", height = 6, width = 10)


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
ggsave("../graphs/valence-arousal-dominance-by-communicative-acc-NO.pdf", height = 6, width = 10)

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
       caption = "valence rating: neutral (0) to completely happy / unhappy (1)\narousal rating: completely calm (0) to completely excited (1)\ndominance rating: neutral (0) to completely in control / controlled (1)") +
  scale_x_discrete(limits = c("Valence", "Arousal", "Dominance")) +
  scale_fill_manual(values = cols2, 
                    labels = predicateType2_names)
ggsave("../graphs/valence-arousal-dominance-by-communicative-emotive-acc-NO.pdf", height = 6, width = 10)

#### X.4.3 rating correlations ----
##### X.4.3.1 valence + arousal ----
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
ggsave("../graphs/arousal-by-valence-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/arousal-by-valence-faceted-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/valence-by-arousal-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/valence-by-arousal-faceted-acc-NO.pdf", height = 6, width = 8)

###### linear models -----
lm(V.Mean.Sum2 ~ A.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.009038   0.035718   0.253      0.8    
# A.Mean.Sum2 0.688846   0.082523   8.347 1.63e-15 ***

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

lm(A.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.34520    0.01029  33.546  < 2e-16 ***
# V.Mean.Sum2  0.24045    0.02881   8.347 1.63e-15 ***

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      


##### X.4.3.2 valence + dominance ----
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
ggsave("../graphs/dominance-by-valence-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/dominance-by-valence-faceted-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/valence-by-dominance-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/valence-by-dominance-faceted-acc-NO.pdf", height = 6, width = 8)

###### linear models -----
lm(V.Mean.Sum2 ~ D.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.18000    0.01885   9.548  < 2e-16 ***
# D.Mean.Sum2  0.49738    0.06871   7.239 2.87e-12 ***

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

lm(D.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.15561    0.01289  12.073  < 2e-16 ***
# V.Mean.Sum2  0.26118    0.03608   7.239 2.87e-12 ***

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       
# emoComm         .
# emotive         
# evidential      
# nonEmoComm      


##### X.4.3.3 arousal + dominance ----
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
ggsave("../graphs/dominance-by-arousal-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/dominance-by-arousal-faceted-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/arousal-by-dominance-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/arousal-by-dominance-faceted-acc-NO.pdf", height = 6, width = 8)

###### linear models -----
lm(A.Mean.Sum2 ~ D.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.40588    0.01192  34.042   <2e-16 ***
# D.Mean.Sum2  0.04489    0.04345   1.033    0.302 

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

lm(D.Mean.Sum2 ~ A.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.20476    0.02829   7.237 2.91e-12 ***
# A.Mean.Sum2  0.06753    0.06537   1.033    0.302 

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       
# emoComm         n.s.
# emotive         
# evidential      
# nonEmoComm      

##### X.4.3.4 direction of valence + direction of dominance ----
###### tables ----
table(new.scale$D.Mean.Sum2.direction, new.scale$V.Mean.Sum2.direction) 
print(contingency_table)
#          negative positive
# negative       98       19
# positive       55      181

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 113.97, df = 1, p-value < 2.2e-16

# The directions of dominance and valence seem highly correlated.

# distribution of direction within predicate types
new.scale %>% 
  count(predicateType2, D.Mean.Sum2.direction)
#    predicateType2 D.Mean.Sum2.direction   n
# 1       cognitive              negative  9
# 2       cognitive              positive 33
# 3         emoComm              negative  9
# 4         emoComm              positive 10
# 5         emotive              negative 54
# 6         emotive              positive 42
# 7      evidential              negative 12
# 8      evidential              positive 52
# 9      nonEmoComm              negative 33
# 10     nonEmoComm              positive 99

new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#    predicateType2 V.Mean.Sum2.direction  n
# 1       cognitive              negative 12
# 2       cognitive              positive 30
# 3         emoComm              negative 16
# 4         emoComm              positive  3
# 5         emotive              negative 62
# 6         emotive              positive 34
# 7      evidential              negative 14
# 8      evidential              positive 50
# 9      nonEmoComm              negative 49
# 10     nonEmoComm              positive 83

# The proportion of negative compared to posiitve valence predicates is higher than
# the proportion of negative vs positive dominance predicates in all predicate
# types. The largest difference is found in the communicatives with an emotive
# meaning component.

#### X.4.4 predicate types predicting VAD ratings ----
##### linear models ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       
# emoComm         ***
# emotive         
# evidential      
# nonEmoComm      

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       
# emoComm         ***
# emotive         
# evidential      
# nonEmoComm      

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "emoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       
# emoComm         ***
# emotive         
# evidential      
# nonEmoComm       

# All predicate types seem to be highly correlated with VAD ratings.


### X.5 projection: VAD against projection ratings ----
#### X.5.1 valence ----
##### X.5.1.1 overall ---- 
###### plots ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-acc-NO.pdf", height = 6, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
ggsave("../graphs/projection-by-valence2-acc-NO.pdf", height = 6, width = 8)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-faceted-acc-NO.pdf", height = 6, width = 10)

# projection by valence faceted with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2-acc-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17154    0.02981   5.755 1.89e-08 ***
# V.Mean.Sum2  0.69480    0.08344   8.327 1.88e-15 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# emoComm         .

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   2.8428     0.1936   14.68   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2
# cognitive         n.s.
# emoComm           **
# emotive           n.s.
# evidential        n.s.
# nonEmoComm        n.s.


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
ggsave("../graphs/projection-by-valence-with-direction-acc-NO.pdf", height = 3.5, width = 6)

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-acc-NO.pdf", 
       height = 6, width = 10)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.48228    0.02676  18.025  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.18567    0.03555  -5.223 3.02e-07 ***

lm(Mean.Proj ~ V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.28520    0.04563   6.251 1.19e-09 ***
# V.Mean.Sum2                                0.58895    0.11444   5.146 4.44e-07 ***
# V.Mean.Sum2.directionpositive             -0.17114    0.05975  -2.864  0.00443 ** 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive  0.09693    0.16568   0.585  0.55891 

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.69280    0.07297  -9.494   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "emoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                                                                Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive                                                  -0.39608    0.43727  -0.906 0.365039    
# fct_relevel(predicateType2, "emoComm")cognitive                                -0.44431    0.26393  -1.683 0.092294 .  
# fct_relevel(predicateType2, "emoComm")emotive                                   1.55946    0.20391   7.648 2.05e-14 ***
# fct_relevel(predicateType2, "emoComm")evidential                               -0.68064    0.25575  -2.661 0.007782 ** 
# fct_relevel(predicateType2, "emoComm")nonEmoComm                               -0.70750    0.19953  -3.546 0.000391 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")cognitive   0.03590    0.49528   0.072 0.942214    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")emotive     0.61658    0.47473   1.299 0.194014    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")evidential  0.46744    0.48585   0.962 0.336000    
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.03088    0.45401  -0.068 0.945777   


clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                2.67721    0.27282   9.813  < 2e-16 ***
# V.Mean.Sum2.directionpositive             -0.51931    0.13358  -3.888 0.000101 ***
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.07409    0.37994  -0.195 0.845388   

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction significance of V.Mean.Sum2
# cognitive       negative  
# emoComm         negative  
# emotive         negative  
# evidential      negative  
# nonEmoComm      negative  
# cognitive       positive  
# emoComm         positive  
# emotive         positive  
# evidential      positive  
# nonEmoComm      positive  

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
ggsave("../graphs/projection-by-valence-communicative-faceted-acc-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-valence-sayverb-faceted-acc-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-valence-modeverb-faceted-acc-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-valence-saybymeans-faceted-acc-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-arousal2-acc-NO.pdf", height = 3.5, width = 6)

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
ggsave("../graphs/projection-by-arousal-faceted-acc-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/projection-by-arousal-faceted2-acc-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.02497    0.06307  -0.396    0.692    
# A.Mean.Sum2  0.96570    0.14571   6.628 1.29e-10 ***

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.6621     0.3188   11.49   <2e-16 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "evidential") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s. 
# emotive         n.s.  
# evidential      n.s.
# nonEmoComm      n.s.


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
ggsave("../graphs/projection-by-arousal-with-direction-acc-NO.pdf", height = 3.5, width = 6)

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-acc-NO.pdf", 
       height = 6, width = 10)

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                 2.7752     0.4949   5.608 2.05e-08 ***
# V.Mean.Sum2.directionpositive              -0.6231     0.2936  -2.122   0.0338 *  
# A.Mean.Sum2:V.Mean.Sum2.directionpositive   0.4000     0.6715   0.596   0.5513   

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction	significance of A.Mean.Sum2
# cognitive	      negative  
# emoComm		      negative	n.s.
# emotive		      negative	
# evidential      negative  
# nonEmoComm	    negative  .
# cognitive	      positive  
# emoComm		      positive  warning: Variance-covariance matrix of the parameters is not defined
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  n.s.

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2  n
# 1               negative      cognitive 12
# 2               negative        emoComm 16
# 3               negative        emotive 62
# 4               negative     evidential 14
# 5               negative     nonEmoComm 49
# 6               positive      cognitive 30
# 7               positive        emoComm  3
# 8               positive        emotive 34
# 9               positive     evidential 50
# 10              positive     nonEmoComm 83

###### negative / positive valence only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients: 
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.6787     0.5024   5.332 9.73e-08 *** [negative]

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.3034     0.4708   7.017 2.26e-12 *** [positive]

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
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
# nonEmoComm	    positive  n.s

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
ggsave("../graphs/projection-by-arousal-communicative-faceted-acc-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-arousal-sayverb-faceted-acc-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-arousal-modeverb-faceted-acc-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-acc-NO.pdf", height = 4, width = 9)


#### X.5.3 dominance ---- 
##### X.5.3.1 overall ----
###### plots ----
# projection by dominance with emotive component 
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
ggsave("../graphs/projection-by-dominance2-acc-NO.pdf", height = 6, width = 8)

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
ggsave("../graphs/projection-by-dominance-faceted2-acc-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.34788    0.03453  10.075   <2e-16 ***
# D.Mean.Sum2  0.12542    0.12583   0.997     0.32  

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm:        n.s.
# nonEmoComm:     n.s.
# emotive:        n.s.
# evidential:     n.s.
# cognitive:      n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# D.Mean.Sum2   0.6346     0.2462   2.578  0.00995 **

clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.   
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
ggsave("../graphs/projection-by-domiance-with-direction-of-valence-acc-NO.pdf", height = 3.5, width = 6)

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
ggsave("../graphs/projection-by-dominance-and-direction-of-valence-faceted2-acc-NO.pdf", 
       height = 6, width = 10)

###### ordinal models ----
# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.2743     0.3875   3.288  0.00101 ** 
# V.Mean.Sum2.directionpositive              -0.7131     0.1368  -5.211 1.88e-07 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.2120     0.5121  -0.414  0.67884 

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         *
# negative  evidential      
# negative  nonEmoComm      n.s.
# positive  cognitive       
# positive  emotive         
# positive  emoComm         .
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
ggsave("../graphs/projection-by-domiance-with-direction-of-dominance-acc-NO.pdf", height = 3.5, width = 6)

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
ggsave("../graphs/projection-by-dominance-and-direction-of-dominance-faceted2-acc-NO.pdf", 
       height = 6, width = 10)


###### ordinal models ----
# with 'direction' of dominance
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * D.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 2.1212     0.4297   4.937 7.95e-07 ***
# D.Mean.Sum2.directionpositive              -0.3650     0.1377  -2.651  0.00803 ** 
# D.Mean.Sum2:D.Mean.Sum2.directionpositive  -1.7103     0.5321  -3.214  0.00131 ** 

# with 'direction' of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         .
# negative  evidential      
# negative  nonEmoComm      n.s.
# positive  cognitive       
# positive  emotive         
# positive  emoComm         n.s.
# positive  evidential      
# positive  nonEmoComm      n.s.

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
ggsave("../graphs/projection-by-dominance-communicative-faceted-acc-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-dominance-sayverb-faceted-acc-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-dominance-modeverb-faceted-acc-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-acc-NO.pdf", height = 4, width = 9)


#### X.5.4 valence + arousal ----
##### linear model ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.02998    0.05997  -0.500 0.617423    
# V.Mean.Sum2  0.55443    0.08960   6.188 1.71e-09 ***
# A.Mean.Sum2  0.58378    0.15166   3.849 0.000141 ***

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2               2.1065     0.6387   3.298 0.000973 ***
# V.Mean.Sum2               2.2263     0.7280   3.058 0.002228 ** 
# A.Mean.Sum2:V.Mean.Sum2   0.3125     1.6119   0.194 0.846287 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "nonEmoComm") + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive           
# emoComm         n.s.   ***  
# emotive          
# evidential      
# nonEmoComm      n.s.  n.s.

##### negative / positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [negative]
# A.Mean.Sum2                1.859      1.002   1.855  0.06361 . 
# V.Mean.Sum2                3.620      1.195   3.030  0.00245 **
# A.Mean.Sum2:V.Mean.Sum2   -2.901      2.496  -1.162  0.24508  


# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [positive]
# A.Mean.Sum2               1.2136     0.8860   1.370    0.171
# V.Mean.Sum2               1.3193     0.9601   1.374    0.169
# A.Mean.Sum2:V.Mean.Sum2   2.8246     2.2533   1.254    0.210

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive	      negative       
# emoComm	      	negative	n.s.  **
# emotive		      negative	
# evidential	    negative  
# nonEmoComm	    negative  n.s.  n.s.
# cognitive	      positive  
# emoComm		      positive  n.s.  n.s.
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  n.s.  n.s.


#### X.5.5 valence + arousal + dominance ----
##### plot ----
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

vplot + aplot + dplot + plot_layout(guides = "collect", axis_titles = "collect")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-acc-NO.pdf", height = 3.5, width = 12)

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
ggsave("../graphs/projection-by-volition-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-volition-and-predicateType2-acc-NO.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.20000    0.02404    8.32 1.19e-15 ***
# fct_relevel(volition, "volitional")non-volitional  0.32747    0.03051   10.73  < 2e-16 ***

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol <- d.proj.acc %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
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
ggsave("../graphs/projection-by-predicateType-and-volition-acc-NO.pdf", height = 4, width = 10)

#### distribution ----
# how many predicates of which predicateType are non-/volitional?
d.proj.acc %>%
  select(predicateType2, verb_renamed, volition) %>%
  unique() %>%
  group_by(predicateType2, volition) %>%
  summarize(count=n())

#   predicateType2 volition       count
#   <chr>          <chr>          <int>
# 1 cognitive      non-volitional    38
# 2 cognitive      volitional         9
# 3 emoComm        non-volitional    13
# 4 emoComm        volitional         8
# 5 emotive        non-volitional   139
# 6 evidential     non-volitional    69
# 7 evidential     volitional         4
# 8 nonEmoComm     non-volitional     8
# 9 nonEmoComm     volitional       142

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
ggsave("../graphs/projection-by-volition-comm-acc-NO.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.19007    0.02043   9.304  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.20093    0.05830   3.447 0.000716 ***


#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, volition) %>% 
  count()
#   predicateType2 volition           n
#   <chr>          <chr>          <int>
# 1 emoComm        non-volitional    13
# 2 emoComm        volitional         8
# 3 nonEmoComm     non-volitional     8
# 4 nonEmoComm     volitional       142



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
ggsave("../graphs/projection-by-communicative-acc-NO.pdf", height = 4, width = 13)

## with highly projective predicates ---- 
# communicative predicates that project as much or more strongly than 'know'
mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType)
#   verb_renamed Mean.Proj commType                sayVerbType        
#   <fct>            <dbl> <chr>                   <chr>              
# 1 apologize        1     discourse participation discourse role verb
# 2 fess up          0.9   discourse participation NA                 
# 3 disclose         0.8   discourse participation NA                 
# 4 warn             0.778 state changing          NA                 
# 5 cry              0.7   pure                    mode verb          
# 6 flaunt           0.7   discourse participation NA                 
# 7 log              0.7   pure                    mode verb          
# 8 point out        0.7   discourse participation discourse role verb
# 9 weep             0.7   pure                    mode verb          
# 10 whine            0.7   pure                    mode verb    

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
ggsave("../graphs/projection-by-communicative-highest-acc-NO.pdf", height = 4, width = 13)


## with extreme predicates ----
# calculate difference in mean projection rating between the predicate with the 
# highest rating and 'know':
slice_max(mean.proj.comm, Mean.Proj, with_ties = FALSE)$Mean.Proj - 
  subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj
# 0.3

### lists ----
# communicative predicates whose projection rating ranges between that of 'think'
# and think + 0.3:
aaa <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                                  Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                  Mean.Proj >= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj) %>% 
  arrange(Mean.Proj) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

# communicative predicates with similar projection ratings as 'know': 
# (know - 0.2) to max [= know + 0.3] 
bbb <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                                  Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

### plots ----
# with labels for extreme predicates
#### 41-9 ----
# [name based on original plot] 
# 0 to (think + 0.3) and know to (know + 0.3)
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
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
                                                     Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
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
ggsave("../graphs/projection-by-communicative-41-9-acc-NO.pdf", height = 4, width = 13)

#### 12-9 ----
# [name based on original plot]

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
ggsave("../graphs/projection-by-communicative-extremes-12-9-acc-NO.pdf", height = 4, width = 13)

#### 28-26 ----
# [name based on original plot]

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
ggsave("../graphs/projection-by-communicative-extremes-28-26-acc-NO.pdf", height = 4, width = 13)


#### 41-44 ----
# [name based on original plot]
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
ggsave("../graphs/projection-by-communicative-extremes-41-44-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-emoComms-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoComms-acc-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude-acc-NO.pdf", height = 4, width = 13)


## acceptability > 5 ----
# the following 7 emoComms are considered:
zzz <- mean.proj.acc %>% filter(predicateType2 == "emoComm" & Mean.Acc > 5) %>% 
  select(verb_renamed) %>% droplevels() %>% unlist() %>% as.vector() %>% print()
# "bitch"  "boast"  "cry"    "fuss"   "gloat"  "squeal" "whine" 

# the following 14 predicates with ratings of up to 5 are excluded:
www <- mean.proj.acc %>% filter(predicateType2 == "emoComm") %>% select(verb_renamed) %>% 
  droplevels() %>% unlist() %>% as.vector() 
setdiff(www, zzz)
# "brag"     "cheer"    "complain" "exclaim"  "groan"    "grumble"  "moan"     "pout"  "quarrel"  
# "rave"     "scream"   "sigh"     "sob"  "weep"  

mean.proj.comm %>% filter(verb_renamed %in% www) %>% group_by(emoCommType) %>% count()
#   emoCommType     n
#   <chr>       <int>
# 1 attitude       10
# 2 manner         11

mean.proj.comm %>% filter(verb_renamed %in% zzz) %>% group_by(emoCommType) %>% count()
# emoCommType     n
# <chr>       <int>
# 1 attitude        4
# 2 manner          3

### 34-32 ----
# [name based on original plot]
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
ggsave("../graphs/projection-by-communicative-extremes-34-32-acc5-acc-NO.pdf", height = 4, width = 13)


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
ggsave("../graphs/projection-by-communicative-emoComms-acc5-acc-NO.pdf", height = 4, width = 13)


# >>> NEGATION ONLY 2 ----------------------------------------------------------------------------
# Modified copy of the code above, including only projection ratings based on items with negation as 
# embedding environment. 
# Includes predicates with MEAN ACCEPTABILITY RATINGS greater than 4 across embedding environments,
# i.e., those predicates that fall within this range of acceptability across all embedding environments 
# but not in the negation-only environment are INCLUDED here. The number of predicates considered 
# here is therefore the same as in the original code above.

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
library(ggforce)
library(ordinal)
library(patchwork)
theme_set(theme_bw())

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
d.proj <- droplevels(subset(d, d$polarity == "negative" & d$conditional == "False"))
nrow(d.proj) # 5411

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
  group_by(polarity, conditional) %>% 
  count()
#   polarity conditional     n
#   <chr>    <chr>       <int>
# 1 negative False        5411

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
  select(c(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, emoCommType, changeOfState, volition, sayVerb, sayVerbType, 
           modeVerbType, sayByMeansVerbType)
nrow(tmp) # 544

mean.proj.all <- left_join(mean.proj.all, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

mean.proj.all %>%
  group_by(predicateType2) %>%
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
# ratings greater than 4 ACROSS ALL EMBEDDING ENVIRONMENTS (= 'original' acceptable predicates)

# acceptable predicates
acc.preds <- c("add", "address", "admit", "advertise", "advise", "affirm", "agonize", "allege", 
               "announce", "answer", "anticipate", "apologize", "appreciate", "argue", "articulate", 
               "ascertain", "assert", "assess", "assume", "attest", "babble", "be advised", 
               "be aggravated", "be agitated", "be agonized", "be alarmed", "be alerted", "be amazed", 
               "be amused", "be angered", "be anguished", "be annoyed", "be appalled", "be aroused", 
               "be assured", "be astonished", "be astounded", "be baffled", "be befuddled", "be boggled", 
               "be bothered", "be bugged", "be charmed", "be chastised", "be comforted", "be concerned", 
               "be congratulated", "be consoled", "be content", "be convinced", "be crushed", "be dazed", 
               "be deceived", "be delighted", "be deluded", "be demoralized", "be depressed", "be devastated", 
               "be disappointed", "be disgraced", "be disgruntled", "be disgusted", "be disheartened", 
               "be disillusioned", "be dismayed", "be dissatisfied", "be distressed", "be disturbed", 
               "be duped", "be elated", "be emailed", "be embarrassed", "be embittered", "be encouraged", 
               "be enraged", "be enthralled", "be enthused", "be exasperated", "be excited", "be exhilarated", 
               "be fascinated", "be flattered", "be flipped out", "be floored", "be flustered", "be fooled", 
               "be foretold", "be freaked out", "be frightened", "be frustrated", "be galled", "be gladdened", 
               "be gratified", "be grieved", "be guaranteed", "be heartened", "be horrified", "be humbled", 
               "be hurt", "be impressed", "be incensed", "be informed", "be infuriated", "be instructed", 
               "be insulted", "be interested", "be intimidated", "be intrigued", "be invigorated", 
               "be irked", "be irritated", "be jarred", "be maddened", "be marvelled", "be miffed", 
               "be misinformed", "be misled", "be mortified", "be moved", "be mystified", "be notified", 
               "be obsessed", "be offended", "be outraged", "be overwhelmed", "be pained", "be panicked", 
               "be perplexed", "be persuaded", "be pestered", "be petrified", "be pleased", "be promised", 
               "be puzzled", "be quoted", "be radioed", "be reassured", "be relieved", "be reminded", 
               "be revolted", "be roused", "be saddened", "be satisfied", "be scared", "be shocked", 
               "be shown", "be sickened", "be signalled", "be spellbound", "be spooked", "be startled", 
               "be stressed", "be stunned", "be stupefied", "be surprised", "be tantalized", "be terrified", 
               "be thrilled", "be told", "be tortured", "be traumatized", "be tricked", "be troubled", 
               "be unnerved", "be unsettled", "be updated", "be upset", "be vexed", "be warned", "be worried", 
               "be wowed", "believe", "bellow", "bicker", "bitch", "blog", "boast", "brag", "broadcast", 
               "calculate", "care", "certify", "challenge", "chant", "charge", "chatter", "check", "cheer", 
               "chronicle", "circulate", "claim", "clarify", "comment", "communicate", "complain", 
               "comprehend", "compute", "concede", "conceive", "conclude", "confess", "confide", "confirm", 
               "conjecture", "contemplate", "contend", "contest", "convey", "corroborate", "cry", "daydream", 
               "debate", "decide", "declare", "decree", "deduce", "demand", "demonstrate", "denounce", 
               "deny", "depict", "derive", "describe", "desire", "despair", "despise", "detail", "detect", 
               "determine", "detest", "dictate", "disapprove", "disbelieve", "disclose", "discover", 
               "discuss", "dislike", "dismiss", "dispel", "display", "dispute", "disregard", "divulge", 
               "document", "doubt", "dread", "dream", "elaborate", "email", "embellish", "emphasize", 
               "endorse", "enjoy", "envision", "envy", "establish", "estimate", "evaluate", "exclaim", 
               "exhibit", "expect", "explain", "expose", "express", "fabricate", "fake", "fantasize", 
               "fax", "fear", "feel", "feign", "fess up", "figure", "figure out", "find", "find out", 
               "flaunt", "forecast", "foresee", "foretell", "forget", "forgive", "freak out", "fret", 
               "fuss", "gab", "gasp", "gather", "gauge", "generalize", "gloat", "gossip", "grieve", 
               "groan", "grumble", "grunt", "guarantee", "gush", "hallucinate", "hate", "hear", "hint", 
               "holler", "hope", "howl", "identify", "ignore", "imagine", "imply", "indicate", "infer", 
               "insinuate", "insist", "intercept", "investigate", "jest", "joke", "judge", "know", 
               "leak", "learn", "lecture", "lie", "like", "loathe", "log", "love", "maintain", "mark", 
               "marvel", "mention", "miss", "moan", "mourn", "mumble", "murmur", "muse", "mutter", 
               "narrate", "notice", "observe", "ordain", "overhear", "overlook", "panic", "perceive", 
               "phone", "picture", "piece together", "point out", "ponder", "posit", "post", "pout", 
               "praise", "pray", "preach", "predict", "prejudge", "presume", "pretend", "proclaim", 
               "promise", "prophesy", "propose", "prove", "publicize", "publish", "quarrel", "question", 
               "quip", "radio", "rant", "rationalize", "rave", "read", "reaffirm", "realize", "reason", 
               "reason out", "reassert", "recall", "recap", "reckon", "recognize", "recollect", 
               "reconsider", "record", "rediscover", "regret", "reiterate", "reject", "relate", "relish", 
               "remark", "remember", "repeat", "reply", "report", "repress", "request", "require", 
               "research", "resent", "resolve", "respond", "restate", "retort", "retract", "reveal", 
               "rule", "ruminate", "say", "scoff", "scream", "scribble", "see", "select", "share", 
               "shout", "show", "showcase", "shriek", "sigh", "signal", "signify", "sing", "sketch", 
               "snitch", "sob", "specify", "spot", "spout", "squeal", "stammer", "state", "stipulate", 
               "stress", "submit", "suggest", "summarize", "suppose", "surmise", "suspect", "swear", 
               "tease", "testify", "think", "trust", "tweet", "type", "uncover", "underestimate", 
               "underline", "underscore", "understand", "uphold", "utter", "videotape", "voice", "vow", 
               "wager", "warn", "warrant", "weep", "whimper", "whine", "whisper", "will", "wish", 
               "witness", "worry", "write", "yell")
length(acc.preds) # 474

# acceptable communicatives

acc.comms <- c("add", "address", "admit", "advertise", "advise", "affirm", "allege", "announce", 
               "answer", "apologize", "argue", "articulate", "assert", "attest", "babble", "bellow", 
               "bicker", "bitch", "blog", "boast", "brag", "broadcast", "certify", "challenge", 
               "chant", "charge", "chatter", "cheer", "chronicle", "circulate", "claim", "clarify", 
               "comment", "communicate", "complain", "concede", "confess", "confide", "confirm", 
               "contend", "contest", "convey", "corroborate", "cry", "debate", "declare", "decree", 
               "demand", "demonstrate", "denounce", "deny", "depict", "describe", "detail", "dictate", 
               "disclose", "discuss", "dismiss", "dispel", "display", "dispute", "divulge", "document", 
               "elaborate", "email", "embellish", "emphasize", "endorse", "establish", "exclaim", 
               "exhibit", "explain", "expose", "express", "fabricate", "fake", "fax", "feign", "fess up", 
               "flaunt", "forecast", "foretell", "fuss", "gab", "gasp", "gloat", "gossip", "groan", 
               "grumble", "grunt", "guarantee", "gush", "hint", "holler", "howl", "imply", "indicate", 
               "insinuate", "insist", "jest", "joke", "leak", "lecture", "lie", "log", "maintain", 
               "mark", "mention", "moan", "mumble", "murmur", "mutter", "narrate", "ordain", "phone", 
               "point out", "posit", "post", "pout", "praise", "preach", "predict", "pretend", "proclaim", 
               "promise", "prophesy", "propose", "prove", "publicize", "publish", "quarrel", "question", 
               "quip", "radio", "rant", "rave", "reaffirm", "reassert", "recap", "record", "reiterate", 
               "reject", "relate", "remark", "repeat", "reply", "report", "request", "respond", "restate", 
               "retort", "retract", "reveal", "rule", "say", "scoff", "scream", "scribble", "share", 
               "shout", "show", "showcase", "shriek", "sigh", "signal", "signify", "sing", "sketch", 
               "snitch", "sob", "specify", "spout", "squeal", "stammer", "state", "stipulate", "stress", 
               "submit", "suggest", "summarize", "swear", "tease", "testify", "tweet", "type", "uncover", 
               "underline", "underscore", "uphold", "utter", "voice", "vow", "wager", "warn", "warrant", 
               "weep", "whimper", "whine", "whisper", "write", "yell")
length(acc.comms) # 201

# by-predicate projection means for "acceptable" predicates
mean.proj.acc <- mean.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(mean.proj.acc) # 474

# by-predicate projection means for "acceptable" communicatives
mean.proj.comm <- mean.proj.acc %>%
  filter(verb_renamed %in% acc.comms)
nrow(mean.proj.comm) # 201

d.proj.acc <- d.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(d.proj.acc) # 4717

d.proj.comm <- d.proj.acc %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 2002



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

ggplot(mean.proj.bt, aes(x = predicateType, y = Mean.Proj, colour = predicateType)) +
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
ggsave("../graphs/projection-by-predicateType-NO.pdf", height = 4, width = 10)

# calculate by-predicateType2 means
mean.proj.bt2 <- d.proj.acc %>%
  group_by(predicateType2) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
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
        axis.title.x = element_text(vjust = -1),
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
ggsave("../graphs/projection-by-predicateType2-NO.pdf", height = 4, width = 10)

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
ggsave("../graphs/projection-by-predicate-communicative-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoComm-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-nonEmoComm-NO.pdf", height = 4, width = 13)


# C Communicatives ----
## C.1 overall ----
### C.1.1 by-predicateType ----
#### plot ----
# calculate by-predicateType means
mean.proj.comm.bt <-  d.proj.comm %>%
  group_by(commType) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
         commType = fct_reorder(as.factor(commType), Mean.Proj))
nrow(mean.proj.comm.bt) # 3

ggplot(mean.proj.comm.bt, aes(x = commType, y = Mean.Proj, colour = commType)) +
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
        panel.grid.major.x = element_blank(),
        plot.margin = margin(5.5, 132, 5.5, 132)) +
  labs(x = "Type of communicative",
       y = "Mean projection rating") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_discrete(labels = c("state\nchanging", "discourse\nparticipation", "pure")) + 
  scale_colour_manual(values = c("pure" = "orange", 
                                 "discourse participation" = "darkorange", 
                                 "state changing" = "orangered"))
ggsave("../graphs/projection-by-predicateType-commType-NO.pdf", height = 4, width = 10)

### C.1.2 by-predicate ----
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
ggsave("../graphs/projection-by-communicative-predicate-NO.pdf", height = 4, width = 10)


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
# 2 communicative no         91
# 3 communicative yes       110


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

#### plot ----
mean.proj.commsay <- d.proj.comm %>%
  group_by(sayVerb) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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

### C.3.2 types of say verbs ----
#### distribution ----
d.proj.comm %>% 
  filter(sayVerb == "yes") %>%  
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
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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

mean.proj.saytype2 <- d.proj.comm %>%
  group_by(sayVerb, sayVerbType) %>% 
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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


#### linear model ----
lm(Mean.Proj ~ fct_relevel(sayVerbType, "discourse role verb"), data = mean.proj.comm) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   **
# mode verb             ***
# say                   n.s.

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "discourse role verb") +  (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                          Estimate Std. Error z value Pr(>|z|)    
# fct_relevel(sayVerbType, "discourse role verb")mode verb   0.5404     0.1481   3.649 0.000263 ***
# fct_relevel(sayVerbType, "discourse role verb")say        -0.0392     0.7636  -0.051 0.959065    

# fct_relevel(sayVerbType, "mode verb")discourse role verb  -0.5404     0.1481  -3.649 0.000263 ***
# fct_relevel(sayVerbType, "mode verb")say                  -0.5796     0.7625  -0.760 0.447145  

# fct_relevel(sayVerbType, "say")discourse role verb  0.03919    0.76377   0.051    0.959
# fct_relevel(sayVerbType, "say")mode verb            0.57963    0.76261   0.760    0.447

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
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(modeVerbType, "say-by-means") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                                             Estimate Std. Error z value Pr(>|z|)
# fct_relevel(modeVerbType, "say-by-means")say-with-attitude   0.5531     0.2529   2.187   0.0287 *

# fct_relevel(modeVerbType, "say-with-attitude")say-by-means  -0.5531     0.2529  -2.187   0.0287 *

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
ggsave("../graphs/projection-by-saybymeansverb-type-NO.pdf", height = 4, width = 10)

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayByMeansVerbType, "sound") + (1 | participant), 
     data = d.proj.comm) %>% 
  summary()
# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)  
# fct_relevel(sayByMeansVerbType, "form")manner   0.1309     0.2521   0.519    0.604
# fct_relevel(sayByMeansVerbType, "form")sound    0.0270     0.3790   0.071    0.943

# fct_relevel(sayByMeansVerbType, "manner")form   -0.1309     0.2521  -0.519    0.604
# fct_relevel(sayByMeansVerbType, "manner")sound  -0.1039     0.3652  -0.284    0.776

# fct_relevel(sayByMeansVerbType, "sound")form    -0.0270     0.3790  -0.071    0.943
# fct_relevel(sayByMeansVerbType, "sound")manner   0.1039     0.3652   0.284    0.776

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### C.3.5 overall ----
#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, commType, sayVerbType, modeVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType         modeVerbType          n
#   <chr>          <chr>                   <chr>               <chr>             <int>
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
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh)
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


## X VAD ratings ----

# load valence and arousal data
w <-  read.csv("../data/BRM-emot-submit.csv")
nrow(w) # 13915

w2 <-  w %>%
  rename(verb = Word) %>%
  inner_join(d.proj.acc, by = "verb")
nrow(w2) # 3862
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

# Warriner et al.' (2013) dataset contains valence, arousal and dominance ratings for 388 of the 
# predicates in the MV dataset which belong to (only) one of our 5 predicate types and have a mean 
# acceptability rating greater than 4 across embedding environments. 

mean.proj.vad %>% 
  group_by(predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType     n
#   <chr>         <int>
# 1 cognitive        47
# 2 communicative   177
# 3 emotive          97
# 4 evidential       67

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

### X.01 rescale V + A + D ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings are rescaled to range from 0 to 1. 
new.scale <- mean.proj.vad %>% 
  mutate(V.Mean.Sum2 = abs(V.Mean.Sum - 5)/4,
         V.Mean.Sum2.direction = case_when(V.Mean.Sum >= 5 ~ "positive",
                                           V.Mean.Sum < 5 ~ "negative"),
         A.Mean.Sum2 = (A.Mean.Sum - 1)/8,
         D.Mean.Sum2 = abs(D.Mean.Sum - 5)/4,
         D.Mean.Sum2.direction = case_when(D.Mean.Sum >= 5 ~ "positive",
                                           D.Mean.Sum < 5 ~ "negative"))

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

### X.02 data frame for ordinal models ----
d.proj.vad = w2 %>% 
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


### X.5 projection: VAD against projection ratings ----
#### X.5.1 valence ----
##### X.5.1.1 overall ---- 
###### plots ----
# projection by valence
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-NO.pdf", height = 6, width = 6)

# projection by valence with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType2), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(), 
        aspect.ratio = 1) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating",
       colour = "Predicate type") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1,1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2, 
                      labels = predicateType2_names)
ggsave("../graphs/projection-by-valence2-NO.pdf", height = 6, width = 8)

# projection by valence faceted
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = predicateType), alpha = 0.8) +
  geom_smooth(method = "lm", colour = "grey30", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.spacing.x = unit(0.3, "cm"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")) +
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  facet_wrap(~ predicateType, ncol = 4) +
  scale_colour_manual(values = cols)
ggsave("../graphs/projection-by-valence-faceted-NO.pdf", height = 6, width = 10)

# projection by valence faceted with emotive component
ggplot(new.scale, aes(x = V.Mean.Sum2, y = Mean.Proj)) +
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
  labs(x = "Mean valence rating", 
       y = "Mean projection rating") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), minor_breaks = seq(0, 1, 1/4)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(values = cols2) +
  facet_wrap(~ predicateType2, ncol = 5, labeller = as_labeller(predicateType2_names)) 
ggsave("../graphs/projection-by-valence-faceted2-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.16156    0.02818   5.733 1.99e-08 ***
# V.Mean.Sum2  0.66813    0.08058   8.292 1.88e-15 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       *
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   2.7649     0.1849   14.95   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2
# cognitive         **
# emoComm           ***
# emotive           n.s.
# evidential        n.s.
# nonEmoComm        .


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
ggsave("../graphs/projection-by-valence-with-direction-NO.pdf", height = 3.5, width = 6)

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-valence-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

###### linear models ----
lm(Mean.Proj ~ V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.44367    0.02508   17.69  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.16073    0.03369   -4.77 2.61e-06 ***

lm(Mean.Proj ~ V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.23894    0.04193   5.699 2.40e-08 ***
# V.Mean.Sum2                                0.64149    0.10909   5.880 8.92e-09 ***
# V.Mean.Sum2.directionpositive             -0.11588    0.05623  -2.061    0.040 *  
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.03735    0.16024  -0.233    0.816  

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.59411    0.06888  -8.625   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "evidential") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType 2   significance of V.Mean.Sum2.direction
# cognitive         n.s.
# emoComm           n.s.
# emotive           n.s.
# evidential        n.s.
# nonEmoComm        **


clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                 2.8647     0.2579  11.109  < 2e-16 ***
# V.Mean.Sum2.directionpositive              -0.3398     0.1255  -2.708  0.00678 ** 
# V.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.4922     0.3646  -1.350  0.17697   

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "cognitive") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction significance of V.Mean.Sum2
# cognitive       negative  *
# emoComm         negative  *
# emotive         negative  n.s.
# evidential      negative  n.s.
# nonEmoComm      negative  n.s.
# cognitive       positive  **
# emoComm         positive  *
# emotive         positive  n.s.
# evidential      positive  n.s.
# nonEmoComm      positive  *

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
ggsave("../graphs/projection-by-valence-communicative-faceted-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-valence-sayverb-faceted-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-valence-modeverb-faceted-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-valence-saybymeans-faceted-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-arousal2-NO.pdf", height = 3.5, width = 6)

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
ggsave("../graphs/projection-by-arousal-faceted-NO.pdf", height = 6, width = 10)

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
ggsave("../graphs/projection-by-arousal-faceted2-NO.pdf", height = 4, width = 9)

###### linear models ----
lm(Mean.Proj ~ A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.04564    0.05937  -0.769    0.443    
# A.Mean.Sum2  0.96881    0.13823   7.008 1.08e-11 ***

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), data = new.scale) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s.
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.7472     0.3048   12.29   <2e-16 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         n.s. 
# emotive         n.s.  
# evidential      n.s.
# nonEmoComm      n.s.


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
ggsave("../graphs/projection-by-arousal-with-direction-NO.pdf", height = 3.5, width = 6)

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
                                 .cols = predicateType2_names)) 
ggsave("../graphs/projection-by-arousal-and-direction-of-valence-faceted2-NO.pdf", 
       height = 6, width = 10)

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2                                 3.3553     0.4567   7.347 2.02e-13 ***
# V.Mean.Sum2.directionpositive              -0.2774     0.2721  -1.019    0.308    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.1897     0.6313  -0.300    0.764    

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), data = d.proj.vad) %>% 
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
# nonEmoComm	    positive  n.s.

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2  n
# 1               negative      cognitive 14
# 2               negative        emoComm 21
# 3               negative        emotive 63
# 4               negative     evidential 16
# 5               negative     nonEmoComm 59
# 6               positive      cognitive 33
# 7               positive        emoComm  4
# 8               positive        emotive 34
# 9               positive     evidential 51
# 10              positive     nonEmoComm 93

###### negative / positive valence only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# Coefficients: 
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.4233     0.4652   7.359 1.85e-13 *** [negative]

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   3.1694     0.4558   6.953 3.57e-12 *** [positive]

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm") + 
       (1 | participant), 
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
# nonEmoComm	    positive  n.s

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
ggsave("../graphs/projection-by-arousal-communicative-faceted-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-arousal-sayverb-faceted-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-arousal-modeverb-faceted-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-arousal-saybymeans-faceted-NO.pdf", height = 4, width = 9)


#### X.5.3 dominance ---- 
##### X.5.3.1 overall ----
###### plots ----
# projection by dominance with emotive component 
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

###### linear models ----
lm(Mean.Proj ~ D.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.34130    0.03261  10.467   <2e-16 ***
# D.Mean.Sum2  0.05747    0.11957   0.481    0.631  

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), data = new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# emoComm:        n.s.
# nonEmoComm:     n.s.
# emotive:        n.s.
# evidential:     n.s.
# cognitive:      n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# D.Mean.Sum2   0.6346     0.2462   2.578  0.00995 **

clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive") + 
       (1 | participant), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.   
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

###### ordinal models ----
# with direction of valence
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * V.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.0960     0.3631   3.018  0.00254 ** 
# V.Mean.Sum2.directionpositive              -0.5719     0.1304  -4.386 1.15e-05 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.3617     0.4880  -0.741  0.45854  

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         *
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


###### ordinal models ----
# with 'direction' of dominance
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * D.Mean.Sum2.direction + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)    
# D.Mean.Sum2                                 1.7843     0.4039   4.418 9.97e-06 ***
# D.Mean.Sum2.directionpositive              -0.2828     0.1302  -2.172  0.02984 *  
# D.Mean.Sum2:D.Mean.Sum2.directionpositive  -1.6450     0.5030  -3.270  0.00107 ** 

# with 'direction' of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "emoComm") +  (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       
# negative  emotive         
# negative  emoComm         *
# negative  evidential      
# negative  nonEmoComm      n.s.
# positive  cognitive       
# positive  emotive         
# positive  emoComm         n.s.
# positive  evidential      
# positive  nonEmoComm      n.s.

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
ggsave("../graphs/projection-by-dominance-communicative-faceted-NO.pdf", height = 4, width = 9)

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
ggsave("../graphs/projection-by-dominance-sayverb-faceted-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-dominance-modeverb-faceted-NO.pdf", height = 4, width = 6.5)

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
ggsave("../graphs/projection-by-dominance-saybymeans-faceted-NO.pdf", height = 4, width = 9)


#### X.5.4 valence + arousal ----
##### linear model ----
lm(Mean.Proj ~ V.Mean.Sum2 + A.Mean.Sum2, data = new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.05188    0.05682  -0.913    0.362    
# V.Mean.Sum2  0.52057    0.08597   6.055 3.34e-09 ***
# A.Mean.Sum2  0.61982    0.14427   4.296 2.20e-05 ***

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2               2.1330     0.5963   3.577 0.000348 ***
# V.Mean.Sum2               1.9614     0.6988   2.807 0.005006 ** 
# A.Mean.Sum2:V.Mean.Sum2   0.6482     1.5431   0.420 0.674420 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive           
# emoComm         n.s.   *  
# emotive          
# evidential      
# nonEmoComm      n.s.  n.s.

##### negative / positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [negative]
# A.Mean.Sum2               2.5037     0.9017   2.777 0.005492 ** 
# V.Mean.Sum2               3.8308     1.1238   3.409 0.000652 ***
# A.Mean.Sum2:V.Mean.Sum2  -3.2042     2.3393  -1.370 0.170773   


# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [positive]
# A.Mean.Sum2               1.3366     0.8416   1.588    0.112
# V.Mean.Sum2               1.1989     0.9344   1.283    0.199
# A.Mean.Sum2:V.Mean.Sum2   2.4832     2.1828   1.138    0.255

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emoComm") + (1 | participant), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive	      negative       
# emoComm	      	negative	n.s.  **
# emotive		      negative	
# evidential	    negative  
# nonEmoComm	    negative  n.s.  n.s.
# cognitive	      positive  
# emoComm		      positive  Variance-covariance matrix of the parameters is not defined
# emotive		      positive  
# evidential	    positive  
# nonEmoComm	    positive  Variance-covariance matrix of the parameters is not defined


#### X.5.5 valence + arousal + dominance ----
##### plot ----
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

vplot + aplot + dplot + plot_layout(guides = "collect", axis_titles = "collect")
ggsave("../graphs/projection-by-VAD-with-direction-of-valence-NO.pdf", height = 3.5, width = 12)

# V Volition ----
## V.1 all predicates ----
### V.1.1 by predicate ----
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
ggsave("../graphs/projection-by-volition-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-volition-and-predicateType2-NO.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.acc) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.19140    0.02246   8.522   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.31211    0.02881  10.832   <2e-16 ***

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol <- d.proj.acc %>%
  group_by(predicateType2, volition) %>%
  summarize(Mean.Proj = mean(veridicality_num), 
            CILow = ci.low(veridicality_num), 
            CIHigh = ci.high(veridicality_num)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, 
         YMax.Proj = Mean.Proj + CIHigh, 
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
ggsave("../graphs/projection-by-predicateType-and-volition-NO.pdf", height = 4, width = 10)

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
ggsave("../graphs/projection-by-volition-comm-NO.pdf", height = 4, width = 13)

#### linear model ----
lm(Mean.Proj ~ fct_relevel(volition, "volitional"), data = mean.proj.comm) %>% 
  summary()
# Coefficients:
#                                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        0.18320    0.01849   9.909  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.15856    0.04867   3.258  0.00132 ** 


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
ggsave("../graphs/projection-by-communicative-NO.pdf", height = 4, width = 13)

## with highly projective predicates ---- 
# communicative predicates that project as much or more strongly than 'know'
mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType)
#   verb_renamed Mean.Proj commType                sayVerbType        
#   <fct>            <dbl> <chr>                   <chr>              
# 1 apologize        1     discourse participation discourse role verb
# 2 fess up          0.9   discourse participation NA                 
# 3 disclose         0.8   discourse participation NA                 
# 4 warn             0.778 state changing          NA                 
# 5 cry              0.7   pure                    mode verb          
# 6 flaunt           0.7   discourse participation NA                 
# 7 log              0.7   pure                    mode verb          
# 8 point out        0.7   discourse participation discourse role verb
# 9 weep             0.7   pure                    mode verb          
# 10 whine            0.7   pure                    mode verb    

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
ggsave("../graphs/projection-by-communicative-highest-NO.pdf", height = 4, width = 13)


## with extreme predicates ----
# calculate difference in mean projection rating between the predicate with the 
# highest rating and 'know':
slice_max(mean.proj.comm, Mean.Proj, with_ties = FALSE)$Mean.Proj - 
  subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj
# 0.3

### lists ----
# communicative predicates whose projection rating ranges between that of 'think'
# and think + 0.3:
aaa <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                                  Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
                                  Mean.Proj >= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj) %>% 
  arrange(Mean.Proj) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

# communicative predicates with similar projection ratings as 'know': 
# (know - 0.2) to max [= know + 0.3] 
bbb <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                                  Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)

### plots ----
# with labels for extreme predicates
#### 41-9 ----
# [name based on original plot] 
# 0 to (think + 0.3) and know to (know + 0.3)
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = mean.proj.acc %>% filter(predicateType == "communicative" & 
                                               Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
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
                                                     Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.3 & 
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
ggsave("../graphs/projection-by-communicative-41-9-NO.pdf", height = 4, width = 13)

#### 12-9 ----
# [name based on original plot]

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
ggsave("../graphs/projection-by-communicative-extremes-12-9-NO.pdf", height = 4, width = 13)

#### 28-26 ----
# [name based on original plot]

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
ggsave("../graphs/projection-by-communicative-extremes-28-26-NO.pdf", height = 4, width = 13)


#### 41-44 ----
# [name based on original plot]
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
ggsave("../graphs/projection-by-communicative-extremes-41-44-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-emoComms-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-predicate-emoComms-NO.pdf", height = 4, width = 13)

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
ggsave("../graphs/projection-by-communicative-emoComms-manner-attitude-NO.pdf", height = 4, width = 13)

