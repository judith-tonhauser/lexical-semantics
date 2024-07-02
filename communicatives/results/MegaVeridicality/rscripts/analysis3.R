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
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)) %>%
  distinct(verb, verb_renamed, predicateType, predicateType2, emotiveComponent, 
           commType, changeOfState, volition, sayVerb, sayVerbType, modeVerbType, 
           sayByMeansVerbType)
nrow(tmp) # 544

mean.proj.all <- left_join(mean.proj.all, tmp, by = c("verb_renamed")) %>%
  distinct() %>%
  mutate(verb_renamed = fct_reorder(as.factor(verb_renamed), Mean.Proj))
nrow(mean.proj.all) # 544

# mean.proj.all %>%
#   group_by(predicateType2) %>% 
#   distinct(verb_renamed) %>% 
#   count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         53
# 2 comPriv            9
# 3 emoComm           47
# 4 emotive          148
# 5 evidential        86
# 6 nonEmoComm       189
# 7 other             12

# exclude "comPriv" and "other" predicates from analysis.
mean.proj <- mean.proj.all %>% 
  filter(! predicateType %in% c("comPriv", "other"))
nrow(mean.proj) # 523  
# 
# mean.proj %>%
#   group_by(predicateType2) %>% 
#   distinct(verb_renamed) %>% 
#   count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         53
# 2 emoComm           47
# 3 emotive          148
# 4 evidential        86
# 5 nonEmoComm       189

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
length(acc.preds) # 490

# acceptable communicatives
acc.comms <- mean.proj %>%
  filter(predicateType == "communicative" & Mean.Acc > 4) %>% 
  distinct(verb_renamed) %>% 
  droplevels() %>% 
  unlist() %>% 
  as.vector()
length(acc.comms) # 216

# by-predicate projection means for "acceptable" predicates
mean.proj.acc <- mean.proj %>% 
  filter(Mean.Acc > 4)
nrow(mean.proj.acc) # 490

# by-predicate projection means for "acceptable" communicatives
mean.proj.comm <- mean.proj.acc %>%
  filter(predicateType == "communicative")
nrow(mean.proj.comm) # 216 (= 236 - 20)

d.proj.acc <- d.proj %>% 
  filter(verb_renamed %in% acc.preds)
nrow(d.proj.acc) # 14676

d.proj.comm <- d.proj.acc %>% 
  filter(predicateType == "communicative")
nrow(d.proj.comm) # 6471


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

# distribution of ratings within each acceptability subgroup
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
  filter(! predicateType %in% c("comPriv", "other") & Mean.Acc <= 4) %>% 
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
# 32 presuppose        4      -0.1    cognitive     
# 33 update            4       0.367  nonEmoComm  

# What proportion of predicates within each type has a mean acceptability rating 
# of less than or equal to 4?
count <- mean.proj %>% 
  filter(! predicateType %in% c("comPriv", "other")) %>% 
  group_by(predicateType2) %>% 
  summarise(total.count = n())
count2 <- mean.proj %>% 
  filter(! predicateType %in% c("comPriv", "other") & Mean.Acc <= 4) %>% 
  group_by(predicateType2) %>% 
  summarise(low.acc.count = n())
left_join(count, count2, by = "predicateType2") %>% 
  mutate(low.acc.percentage = low.acc.count * 100 / total.count)
#   predicateType2 total.count low.acc.count low.acc.percentage
#   <chr>                <int>         <int>              <dbl>
# 1 cognitive               54             1               1.85
# 2 emoComm                 47             9              19.1 
# 3 emotive                148             4               2.70
# 4 evidential              86             8               9.30
# 5 nonEmoComm             188            11               5.85

# Due to their low acceptability ratings, the 33 predicates (incl. 20 communicatives)
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
                              "communicative without\nemotive component", 
                              "communicative with\nemotive component", "emotive")) +
  scale_colour_manual(values = cols2)
ggsave("../graphs/projection-by-predicateType2.pdf", height = 4, width = 10)

### distribution ----
mean.proj.acc %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         53
# 2 emoComm           38
# 3 emotive          144
# 4 evidential        78
# 5 nonEmoComm       177

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
# 1 discourse participation   109
# 2 pure                       84
# 3 state changing             22

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
# 1 discourse participation   109
# 2 pure                       84
# 3 state changing             22

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

#### vs "discourse role verbs" (Grimshaw 2015) ----
mean.proj.comm %>% 
  filter(commType == "discourse participation" | sayVerbType == "discourse role verb") %>% 
  group_by(predicateType2, commType, sayVerbType) %>% 
  count()
#   predicateType2 commType                sayVerbType             n
#   <chr>          <chr>                   <chr>               <int>
# 1 emoComm        discourse participation mode verb               4 - A
# 2 emoComm        discourse participation NA                      3
# 3 nonEmoComm     discourse participation discourse role verb    46
# 4 nonEmoComm     discourse participation mode verb               1 - B
# 5 nonEmoComm     discourse participation NA                     55
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
# 1 conceal          0.6  
# 2 warn             0.552
# 3 expose           0.533
# 4 advise           0.367
# 5 certify          0.367
# 6 showcase         0.333
# 7 advertise        0.3  
# 8 guarantee        0.267
# 9 swear            0.233
# 10 vow              0.233

slice_min(subset(mean.proj.comm, commType == "state changing"), Mean.Proj, n = 10) %>% 
  select(verb_renamed, Mean.Proj)
#   verb_renamed Mean.Proj
#   <fct>            <dbl>
# 1 pretend        -0.367 
# 2 fabricate      -0.2   
# 3 feign          -0.133 
# 4 fake            0.0333
# 5 generalize      0.1   
# 6 promise         0.1   
# 7 implore         0.133 
# 8 demonstrate     0.138 
# 9 insist          0.167 
# 10 lie             0.167 

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
# 2 communicative no        104
# 3 communicative yes       111

d.proj.acc %>% 
  filter(predicateType == "cognitive" & sayVerb == "yes") %>% 
  distinct(verb_renamed)
# verb_renamed
# 1        think
# 2         pray
# These predicates "report internal linguistic formulation only" (Grimshaw 2015: 84).

# The MV dataset contains 216 communication predicates with mean acceptability 
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
# 1 no        104
# 2 yes       111
# Of the 215 communicatives included in this investigation, 111 are say-predicates.

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
# 1 discourse role verb    56
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
lm(Mean.Proj ~ fct_relevel(sayVerbType, "say"), data = mean.proj.comm) %>% 
  summary()
# sayVerbType           significance
# discourse role verb   ***
# mode verb             ***
# say                   n.s.

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### ordinal model ----
clmm(as.factor(veridicality_num) ~ fct_relevel(sayVerbType, "say") + 
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
# 2 emoComm        discourse participation NA                  NA                    3
# 3 emoComm        pure                    mode verb           say-by-means         16
# 4 emoComm        pure                    mode verb           say-with-attitude     7
# 5 emoComm        pure                    NA                  NA                    7
# 6 emoComm        state changing          NA                  NA                    1
# 7 nonEmoComm     discourse participation discourse role verb NA                   46
# 8 nonEmoComm     discourse participation mode verb           say-with-attitude     1
# 9 nonEmoComm     discourse participation NA                  NA                   55
# 10 nonEmoComm     pure                    discourse role verb NA                    5
# 11 nonEmoComm     pure                    mode verb           say-by-means         25
# 12 nonEmoComm     pure                    mode verb           say-with-attitude     1
# 13 nonEmoComm     pure                    say                 NA                    1
# 14 nonEmoComm     pure                    NA                  NA                   22
# 15 nonEmoComm     state changing          discourse role verb NA                    5
# 16 nonEmoComm     state changing          NA                  NA                   16

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
nrow(w2) # 12041
n_distinct(w2$verb) # 389
n_distinct(w2$verb_renamed) # 402

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
nrow(mean.proj.vad) # 402

# Warriner et al.' (2013) dataset contains valence, arousal and dominance ratings 
# for 402 of the predicates in the MV dataset which belong to (only) one of our 5
# predicate types and have a mean acceptability rating greater than 4. 

mean.proj.vad %>% 
  group_by(predicateType) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType     n
#   <chr>         <int>
# 1 cognitive        47
# 2 communicative   188
# 3 emotive          98
# 4 evidential       69

mean.proj.vad %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()  
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         47
# 2 emoComm           33
# 3 emotive           98
# 4 evidential        69
# 5 nonEmoComm       155

### X.01 rescale V + A + D ratings ----
# Valence and dominance have extremes (unhappy - happy, controlled - controlling)
# and a neutral state in between. The neutral state of arousal is not in the middle 
# of the "calm - aroused" scale, but at the lower end: calmness is the absence of
# arousal; there is no such thing as "negative" arousal. 
# The ratings for valence and arousal are rescaled to range from 0 to 1. 
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
new.scale[duplicated(new.scale[,cbind(2,14)]),] %>%
  select(verb.x, predicateType2)
#     verb.x predicateType2
# 185 grieve        emotive
# 237 marvel        emotive
# 260  panic        emotive
# 400  worry        emotive
new.scale %>% 
  filter(predicateType == "emotive") %>% 
  nrow() # 98
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
mean.valence = new.scale %>%
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
mean.valence2 = new.scale %>%
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
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "emotive"), new.scale) %>% 
  summary()
# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                       0.44199    0.01719  25.708  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")cognitive  -0.12843    0.03020  -4.253 2.63e-05 ***
# fct_relevel(predicateType2, "emotive")emoComm    -0.08343    0.03425  -2.436   0.0153 *  
# fct_relevel(predicateType2, "emotive")evidential -0.22471    0.02675  -8.401 7.94e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm -0.23896    0.02196 -10.879  < 2e-16 ***

##### X.1.2.2 with direction of valence ----
###### plot ----
# calculate valence by predicateType2 means and direction of valence
mean.valence3 = new.scale %>%
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
mean.arousal = new.scale %>%
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
mean.arousal2 = new.scale %>%
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
lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), new.scale) %>% 
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
mean.arousal3 = new.scale %>%
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
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "evidential") * V.Mean.Sum2.direction, 
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
# (Intercept)                                       0.19053    0.02489   7.656 1.47e-13 ***
# fct_relevel(predicateType2, "emoComm")cognitive   0.06110    0.03261   1.873   0.0617 .  
# fct_relevel(predicateType2, "emoComm")emotive     0.05365    0.02877   1.865   0.0630 .  
# fct_relevel(predicateType2, "emoComm")evidential  0.07610    0.03026   2.515   0.0123 *  
# fct_relevel(predicateType2, "emoComm")nonEmoComm  0.02110    0.02739   0.770   0.4415  

# For all five predicate types there is a correlation with dominance ratings at the
# 0.001 significance level.

##### X.3.2.2 with direction of valence ----
###### plot ----
# calculate dominance by predicateType2 means and direction of valence
mean.dominance2 = new.scale %>%
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
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive") * V.Mean.Sum2.direction, 
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
lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive") * D.Mean.Sum2.direction, 
   new.scale) %>% 
  summary()
# Significant at the 0.001 level for all predicate types.

###### tables: what is "direction of dominance"? ----
new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  slice_max(D.Mean.Sum2, n = 10) %>% 
  select(verb_renamed, D.Mean.Sum2, predicateType2)
#    verb_renamed D.Mean.Sum2 predicateType2
# 1  be delighted      0.5725        emotive
# 2         enjoy      0.5700        emotive
# 3       approve      0.5650     nonEmoComm
# 4  be comforted      0.5450        emotive
# 5      research      0.5400     evidential
# 6        detail      0.5350     nonEmoComm
# 7     recognize      0.5275     evidential
# 8     determine      0.5225     evidential
# 9        signal      0.5125     nonEmoComm
# 10 be signalled      0.5125     evidential

new.scale %>% 
  filter(D.Mean.Sum2.direction == "positive") %>% 
  group_by(predicateType2) %>% 
  distinct(verb_renamed) %>% 
  count()
#   predicateType2     n
#   <chr>          <int>
# 1 cognitive         37
# 2 emoComm           19
# 3 emotive           44
# 4 evidential        57
# 5 nonEmoComm       112

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
# 2 emoComm           14
# 3 emotive           54
# 4 evidential        12
# 5 nonEmoComm        43


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
# (Intercept)  0.01340    0.03293   0.407    0.684    
# A.Mean.Sum2  0.66792    0.07665   8.714   <2e-16 ***

lm(V.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         **
# evidential      *
# nonEmoComm      **

lm(A.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.344125   0.009586  35.901   <2e-16 ***
# V.Mean.Sum2 0.238867   0.027412   8.714   <2e-16 ***

lm(A.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "nonEmoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         **
# evidential      *
# nonEmoComm      **


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
# (Intercept)  0.18431    0.01753  10.517  < 2e-16 ***
# D.Mean.Sum2  0.45334    0.06420   7.062 7.33e-12 ***

lm(V.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         *
# emotive         ***
# evidential      .
# nonEmoComm      *

lm(D.Mean.Sum2 ~ V.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.16109    0.01211  13.304  < 2e-16 ***
# V.Mean.Sum2  0.24451    0.03463   7.062 7.33e-12 ***

lm(D.Mean.Sum2 ~ V.Mean.Sum2 * fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       ***
# emoComm         .
# emotive         ***
# evidential      *
# nonEmoComm      **


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
# (Intercept)  0.40525    0.01110  36.493   <2e-16 ***
# D.Mean.Sum2  0.03452    0.04068   0.849    0.397 

lm(A.Mean.Sum2 ~ D.Mean.Sum2 * fct_relevel(predicateType2, "evidential"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s. 

lm(D.Mean.Sum2 ~ A.Mean.Sum2, new.scale) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.21034    0.02636   7.981 1.55e-14 ***
# A.Mean.Sum2  0.05206    0.06135   0.849    0.397 

lm(D.Mean.Sum2 ~ A.Mean.Sum2 * fct_relevel(predicateType2, "emotive"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       n.s.
# emoComm         *
# emotive         n.s.
# evidential      n.s.
# nonEmoComm      n.s. 

##### X.4.3.4 direction of valence + direction of dominance ----
###### tables ----
contingency_table <- table(new.scale$D.Mean.Sum2.direction, new.scale$V.Mean.Sum2.direction)
print(contingency_table)
#          negative positive
# negative      114       19
# positive       63      206

# Chi-square test
chisq.test(contingency_table) %>% 
  print()
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  contingency_table
# X-squared = 137.63, df = 1, p-value < 2.2e-16

# The direction of dominance and valence seem highly correlated.

# distribution of direction within predicate types
new.scale %>% 
  count(predicateType2, D.Mean.Sum2.direction)
#    predicateType2 D.Mean.Sum2.direction   n
# 1       cognitive              negative  10 21% of cognitives
# 2       cognitive              positive  37
# 3         emoComm              negative  14 42% of emoComms
# 4         emoComm              positive  19
# 5         emotive              negative  54 55% of emotives
# 6         emotive              positive  44
# 7      evidential              negative  12 17% of evidentials
# 8      evidential              positive  57
# 9      nonEmoComm              negative  43 28% of nonEmoComms
# 10     nonEmoComm              positive 112

new.scale %>% 
  count(predicateType2, V.Mean.Sum2.direction)
#    predicateType2 V.Mean.Sum2.direction  n
# 1       cognitive              negative 14 30% of cognitives
# 2       cognitive              positive 32
# 3         emoComm              negative 25 76 % of emoComms
# 4         emoComm              positive  8
# 5         emotive              negative 63 64% of emotives
# 6         emotive              positive 35
# 7      evidential              negative 16 23% of evidentials
# 8      evidential              positive 53
# 9      nonEmoComm              negative 59 38% of nonEmoComms
# 10     nonEmoComm              positive 97

# The proportion of negative compared to posiitve valence predicates is higher than
# the proportion of negative vs positive dominance predicates in all predicate
# types. The largest difference is found in the communicatives with an emotive
# meaning component.

#### X.4.4 predicate types predicting VAD ratings ----
##### linear models ----
lm(V.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of V.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

lm(A.Mean.Sum2 ~ fct_relevel(predicateType2, "cognitive"), new.scale) %>% 
  summary()
# predicateType2  significance of A.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      ***

lm(D.Mean.Sum2 ~ fct_relevel(predicateType2, "nonEmoComm"), new.scale) %>% 
  summary()
# predicateType2  significance of D.Mean.Sum2
# cognitive       ***
# emoComm         ***
# emotive         ***
# evidential      ***
# nonEmoComm      *** 

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
# (Intercept)  0.26617    0.02329  11.430  < 2e-16 ***
# V.Mean.Sum2  0.46447    0.06659   6.975 1.28e-11 ***

lm(Mean.Proj ~ V.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# Coefficients:
#                                                              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                   0.30707    0.07628   4.025 6.83e-05 ***
# V.Mean.Sum2                                                   0.33922    0.18944   1.791   0.0741 .  
# fct_relevel(predicateType2, "emoComm")cognitive              -0.08963    0.09599  -0.934   0.3510    
# fct_relevel(predicateType2, "emoComm")emotive                 0.41805    0.09081   4.603 5.62e-06 ***
# fct_relevel(predicateType2, "emoComm")evidential              0.02848    0.08839   0.322   0.7474    
# fct_relevel(predicateType2, "emoComm")nonEmoComm             -0.02911    0.08079  -0.360   0.7188    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")cognitive  -0.33153    0.24660  -1.344   0.1796    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")emotive    -0.32522    0.21503  -1.512   0.1312    
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")evidential -0.52484    0.25670  -2.045   0.0416 *  
# V.Mean.Sum2:fct_relevel(predicateType2, "emoComm")nonEmoComm -0.29548    0.21673  -1.363   0.1735     

# not significant for any of the other predicate types.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 + (1 | participant) + (1 | environment),
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2   1.9787     0.1022   19.35   <2e-16 ***

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
# 136         dream      cognitive -0.7483372
# 169     be fooled     evidential -0.7000484
# 91       daydream      cognitive -0.6824912
# 274       pretend     nonEmoComm -0.6769648
# 239     be misled     evidential -0.6751837
# 373    be tricked     evidential -0.6456133
# 42    be bothered        emotive  0.6354675
# 30  be astonished        emotive  0.6032954
# 158     fantasize      cognitive -0.5984070
# 201       imagine      cognitive -0.5905959

# predicates grouped by predicate type
cbind(new.scale, residual) %>% 
  group_by(predicateType2) %>% 
  arrange(desc(abs(residual))) %>% 
  slice_head(n = 10) %>% 
  select(verb_renamed, predicateType2, residual) %>% 
  print(n = 50)
#   verb_renamed  predicateType2 residual
#   <fct>         <chr>             <dbl>
# 1 dream         cognitive        -0.748
# 2 daydream      cognitive        -0.682
# 3 fantasize     cognitive        -0.598
# 4 imagine       cognitive        -0.591
# 5 be deluded    cognitive        -0.560
# 6 picture       cognitive        -0.534
# 7 believe       cognitive        -0.504
# 8 think         cognitive        -0.495
# 9 hope          cognitive        -0.487
# 10 trust         cognitive        -0.460
# 11 joke          emoComm          -0.501
# 12 quarrel       emoComm          -0.384
# 13 cry           emoComm           0.360
# 14 jest          emoComm          -0.339
# 15 cringe        emoComm           0.329
# 16 flaunt        emoComm           0.230
# 17 bicker        emoComm          -0.220
# 18 pout          emoComm           0.207
# 19 implore       emoComm          -0.194
# 20 grunt         emoComm          -0.186
# 21 be bothered   emotive           0.635
# 22 be astonished emotive           0.603
# 23 be floored    emotive           0.584
# 24 be startled   emotive           0.560
# 25 be offended   emotive           0.502
# 26 resent        emotive           0.501
# 27 be astounded  emotive           0.483
# 28 be stunned    emotive           0.482
# 29 be jarred     emotive           0.479
# 30 be shocked    emotive           0.473
# 31 be fooled     evidential       -0.700
# 32 be misled     evidential       -0.675
# 33 be tricked    evidential       -0.646
# 34 be promised   evidential       -0.557
# 35 be warned     evidential        0.489
# 36 be duped      evidential       -0.449
# 37 realize       evidential        0.433
# 38 spot          evidential        0.420
# 39 be informed   evidential        0.413
# 40 check         evidential       -0.413
# 41 pretend       nonEmoComm       -0.677
# 42 fake          nonEmoComm       -0.509
# 43 suggest       nonEmoComm       -0.497
# 44 feign         nonEmoComm       -0.463
# 45 apologize     nonEmoComm        0.451
# 46 reject        nonEmoComm       -0.438
# 47 lie           nonEmoComm       -0.403
# 48 claim         nonEmoComm       -0.384
# 49 promise       nonEmoComm       -0.357
# 50 sing          nonEmoComm       -0.350



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
# (Intercept)                    0.45041    0.02059  21.874  < 2e-16 ***
# V.Mean.Sum2.directionpositive -0.08899    0.02752  -3.233  0.00133 ** 

lm(Mean.Proj ~ V.Mean.Sum2 * V.Mean.Sum2.direction, data = new.scale) %>% 
  summary()
# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.28464    0.03523   8.080 7.85e-15 ***
# V.Mean.Sum2                                0.52054    0.09202   5.657 2.96e-08 ***
# V.Mean.Sum2.directionpositive             -0.01820    0.04699  -0.387    0.699    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.16426    0.13356  -1.230    0.219  

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction + (1 | participant) + 
       (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive -0.39482    0.03895  -10.14   <2e-16 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2.direction * fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                                                            Estimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2.directionpositive                                                   0.26072    0.10004   2.606 0.009158 ** 
# fct_relevel(predicateType2, "emotive")cognitive                                -1.93073    0.12058 -16.012  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")emoComm                                  -1.25391    0.09827 -12.760  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")evidential                               -2.33737    0.11853 -19.719  < 2e-16 ***
# fct_relevel(predicateType2, "emotive")nonEmoComm                               -1.91601    0.07816 -24.513  < 2e-16 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")cognitive  -0.70222    0.16042  -4.377  1.2e-05 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")emoComm    -0.66522    0.18809  -3.537 0.000405 ***
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")evidential  0.29768    0.15257   1.951 0.051035 .  
# V.Mean.Sum2.directionpositive:fct_relevel(predicateType2, "emotive")nonEmoComm -0.24621    0.11855  -2.077 0.037816 *  

clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * V.Mean.Sum2.direction + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                                            stimate Std. Error z value Pr(>|z|)    
# V.Mean.Sum2                                2.28242    0.14449  15.796  < 2e-16 ***
# V.Mean.Sum2.directionpositive             -0.07741    0.06979  -1.109    0.267    
# V.Mean.Sum2:V.Mean.Sum2.directionpositive -0.80908    0.20361  -3.974 7.08e-05 ***

# with predicate type
clmm(as.factor(veridicality_num) ~ V.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "cognitive") +  (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2  direction significance of V.Mean.Sum2
# cognitive       negative  .
# emoComm         negative  ***
# emotive         negative  n.s.
# evidential      negative  n.s.
# nonEmoComm      negative  n.s.
# cognitive       positive  n.s.
# emoComm         positive  .
# emotive         positive  *
# evidential      positive  .
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
# (Intercept)  0.13526    0.04875   2.774  0.00579 ** 
# A.Mean.Sum2  0.64208    0.11348   5.658 2.92e-08 ***

lm(Mean.Proj ~ A.Mean.Sum2 * fct_relevel(predicateType2, "evidential"), data = new.scale) %>% 
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
# A.Mean.Sum2   2.5850     0.1685   15.34   <2e-16 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "evidential") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2
# cognitive       *
# emoComm         * 
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
# A.Mean.Sum2                                 2.3725     0.2549   9.307   <2e-16 ***
# V.Mean.Sum2.directionpositive              -0.1481     0.1521  -0.973    0.330    
# A.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.1832     0.3523  -0.520    0.603     

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * 
       fct_relevel(V.Mean.Sum2.direction, "negative") * 
       fct_relevel(predicateType2, "emotive") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# predicateType2  direction	significance of A.Mean.Sum2
# cognitive	      negative  n.s.
# emoComm		      negative	*
# emotive		      negative	*
# evidential      negative  *
# nonEmoComm	    negative  n.s.
# cognitive	      positive  *
# emoComm		      positive  **
# emotive		      positive  ***
# evidential	    positive  n.s.
# nonEmoComm	    positive  n.s.

# how many predicates in each predicate types within the two valence categories?
new.scale %>% 
  count(V.Mean.Sum2.direction, predicateType2)
#    V.Mean.Sum2.direction predicateType2  n
# 1               negative      cognitive 14
# 2               negative        emoComm 25
# 3               negative        emotive 63
# 4               negative     evidential 16
# 5               negative     nonEmoComm 59
# 6               positive      cognitive 32
# 7               positive        emoComm  8
# 8               positive        emotive 35
# 9               positive     evidential 53
# 10              positive     nonEmoComm 97

###### negative / positive valence only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj.vad %>%  filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients: 
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.2145     0.2525   8.769   <2e-16 *** [negative]

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# A.Mean.Sum2   2.3366     0.2547   9.175   <2e-16 *** [positive]

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * fct_relevel(predicateType2, "evidential") + 
       (1 | participant) + (1 | environment), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2
# cognitive	      negative  n.s.
# emoComm	      	negative	.
# emotive		      negative	*
# evidential	    negative  n.s.
# nonEmoComm	    negative  n.s.
# cognitive	      positive  *
# emoComm		      positive  **
# emotive		      positive  ***
# evidential	    positive  n.s.
# nonEmoComm	    positive  n.s.

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
# (Intercept)  0.38078    0.02619  14.539   <2e-16 ***
# D.Mean.Sum2  0.08550    0.09594   0.891    0.373 

lm(Mean.Proj ~ D.Mean.Sum2 * fct_relevel(predicateType2, "emoComm"), data = new.scale) %>% 
  summary()
# emoComm: 0.05
# nonEmoComm: n.s.
# emotive: n.s.
# evidential: n.s.
# cognitive: n.s.

###### ordinal models ----
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 + (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# D.Mean.Sum2   0.4336     0.1332   3.255  0.00113 **

clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(predicateType2, "evidential") + 
       (1 | participant) + (1 | environment), data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of D.Mean.Sum2
# cognitive       
# emoComm         
# emotive            
# evidential      
# nonEmoComm      


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
# D.Mean.Sum2                                 0.9302     0.2094   4.442 8.91e-06 ***
# V.Mean.Sum2.directionpositive              -0.3935     0.0735  -5.354 8.62e-08 ***
# D.Mean.Sum2:V.Mean.Sum2.directionpositive  -0.2469     0.2785  -0.886    0.375 

# with direction of valence + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(V.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") +  (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       .
# negative  emotive         ***
# negative  emoComm         ***
# negative  evidential      ***
# negative  nonEmoComm      **
# positive  cognitive       **
# positive  emotive         n.s.
# positive  emoComm         **
# positive  evidential      *
# positive  nonEmoComm      *
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
# D.Mean.Sum2                                1.38723    0.23043   6.020 1.74e-09 ***
# D.Mean.Sum2.directionpositive             -0.25866    0.07395  -3.498 0.000469 ***
# D.Mean.Sum2:D.Mean.Sum2.directionpositive -1.05333    0.28664  -3.675 0.000238 ***

# with 'direction' of dominance + predicate type
clmm(as.factor(veridicality_num) ~ D.Mean.Sum2 * fct_relevel(D.Mean.Sum2.direction, "positive") * 
       fct_relevel(predicateType2, "nonEmoComm") +  (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# direction predicateType2  significance of D.Mean.Sum2
# negative  cognitive       n.s.
# negative  emotive         ***
# negative  emoComm         ***
# negative  evidential      ***
# negative  nonEmoComm      .
# positive  cognitive       n.s.
# positive  emotive         n.s.
# positive  emoComm         n.s.
# positive  evidential      **
# positive  nonEmoComm      **

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
# (Intercept)  0.13030    0.04727   2.756  0.00611 ** 
# V.Mean.Sum2  0.37015    0.07176   5.158 3.94e-07 ***
# A.Mean.Sum2  0.39484    0.12000   3.290  0.00109 ** 

##### ordinal models ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
       (1 | environment), data = d.proj.vad) %>% 
  summary()
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)   
# A.Mean.Sum2               1.0198     0.3324   3.068  0.00216 **
# V.Mean.Sum2               0.9229     0.3879   2.380  0.01733 * 
# A.Mean.Sum2:V.Mean.Sum2   1.5926     0.8495   1.875  0.06083 . 

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "evidential") + (1 | participant) + (1 | environment), 
     data = d.proj.vad) %>% 
  summary()
# predicateType2	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive       **    ***
# emoComm         .     * 
# emotive         n.s.  n.s. 
# evidential      n.s.  n.s.
# nonEmoComm      n.s.  n.s.

##### negative / positive valence predicates only ----
clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 + (1 | participant) + 
       (1 | environment), data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "negative")) %>% 
  summary()
# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [negative]
# A.Mean.Sum2               1.9393     0.4987   3.888 0.000101 ***
# V.Mean.Sum2               3.6853     0.6300   5.850 4.92e-09 ***
# A.Mean.Sum2:V.Mean.Sum2  -3.8546     1.3069  -2.949 0.003184 ** 


# Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    [positive]
# A.Mean.Sum2              0.008514   0.475030   0.018    0.986    
# V.Mean.Sum2             -0.787948   0.517284  -1.523    0.128    
# A.Mean.Sum2:V.Mean.Sum2  5.038373   1.193637   4.221 2.43e-05 ***

clmm(as.factor(veridicality_num) ~ A.Mean.Sum2 * V.Mean.Sum2 * 
       fct_relevel(predicateType2, "emotive") + (1 | participant) + (1 | environment), 
     data = d.proj.vad %>% filter(V.Mean.Sum2.direction == "positive")) %>% 
  summary()
# predicateType2	direction	significance of A.Mean.Sum2 / V.Mean.Sum2
# cognitive	      negative  .     *
# emoComm	      	negative	.     *
# emotive		      negative	n.s., n.s.
# evidential	    negative  n.s.  *
# nonEmoComm	    negative  n.s.  n.s.
# cognitive	      positive  **    ***
# emoComm		      positive  n.s.  n.s.
# emotive		      positive  **    ***
# evidential	    positive  n.s.  .
# nonEmoComm	    positive  n.s.  n.s.


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
# (Intercept)                                        0.28830    0.01852  15.569   <2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.22172    0.02391   9.275   <2e-16 ***

### V.1.2 by predicate type ----
#### plot ----
mean.proj.vol = d.proj.acc %>%
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
# 1 cognitive      non-volitional    43
# 2 cognitive      volitional        10
# 3 emoComm        non-volitional    19
# 4 emoComm        volitional        19
# 5 emotive        non-volitional   144
# 6 evidential     non-volitional    75
# 7 evidential     volitional         3
# 8 nonEmoComm     non-volitional    13
# 9 nonEmoComm     volitional       164

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
# (Intercept)                                        0.28546    0.01331  21.442  < 2e-16 ***
# fct_relevel(volition, "volitional")non-volitional  0.15983    0.03459   4.621  6.6e-06 ***


#### distribution ----
mean.proj.comm %>% 
  group_by(predicateType2, volition) %>% 
  count()
#   predicateType2 volition           n
#   <chr>          <chr>          <int>
# 1 emoComm        non-volitional    19
# 2 emoComm        volitional        19
# 3 nonEmoComm     non-volitional    13
# 4 nonEmoComm     volitional       164

# W ----
mean.proj.comm %>% 
  filter(sayByMeansVerbType == "form") %>% 
  select(verb_renamed) %>% 
  print(n = Inf)

# two possible interpretation: CC was not said vs was not said in a certain way /
# with a certain attitude etc.

# emoComms: 38/38
# commType discourse participation: mixed, more single interpretation
# commType pure: mixed, more focus-sensitive interpretation
# commType state changing: mixed
# say verbs discourse role: mainly single interpretation (exceptions: apologize, insist...)  
# mode verbs say-with-attitude: 13/13
# say-by-means verbs sound: 5/5, manner: 22/22, form: 0/14


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
                   colour = "dodgerblue4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "think"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "deeppink") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "believe"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = -0.2,
                   colour = "orange2") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "believe", "know"),
                      values = c("deepskyblue2", "dodgerblue4", "deeppink", "orange2", "darkgreen"))
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
# 2 flip out         0.833 pure                    NA                 
# 3 cringe           0.8   pure                    NA                 
# 4 celebrate        0.733 pure                    NA                 
# 5 fess up          0.733 discourse participation NA                 
# 6 apologize        0.724 discourse participation discourse role verb
# 7 whine            0.7   pure                    mode verb          
# 8 approve          0.667 discourse participation NA                 
# 9 disclose         0.667 discourse participation NA                 
# 10 grimace          0.667 pure                    NA                 
# 11 pout             0.667 pure                    NA                 
# 12 bitch            0.633 discourse participation mode verb          
# 13 document         0.633 pure                    mode verb          
# 14 weep             0.633 pure                    mode verb   

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
                   colour = "dodgerblue4") +
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
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "dodgerblue4", "deeppink", "green4", "darkgreen"))
ggsave("../graphs/projection-by-communicative-highest.pdf", height = 4, width = 13)


## with extreme predicates ----
# calculate difference in mean projection rating between the predicate with the 
# highest rating and 'know':
slice_max(mean.proj.comm, Mean.Proj, with_ties = FALSE)$Mean.Proj - 
  subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj
# 0.2

# communicative predicates whose projection rating ranges between that of 'think'
# and think + 0.2:
aaa <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj <= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj + 0.2 & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "think")$Mean.Proj) %>% 
  arrange(Mean.Proj) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)
aaa %>% write.csv("../data/barely-projective-communicatives.csv")
# verb_renamed Mean.Proj commType                sayVerbType        
# <fct>            <dbl> <chr>                   <chr>              
#   1 charge         -0.0333 discourse participation NA                 
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
# 15 bet             0.1    discourse participation NA                 
# 16 dispel          0.1    discourse participation NA                 
# 17 generalize      0.1    state changing          NA                 
# 18 holler          0.1    pure                    mode verb          
# 19 joke            0.1    pure                    mode verb          
# 20 posit           0.1    discourse participation discourse role verb
# 21 promise         0.1    state changing          discourse role verb
# 22 type            0.1    pure                    mode verb          
# 23 wager           0.1    discourse participation NA                 
# 24 depict          0.133  pure                    NA                 
# 25 implore         0.133  state changing          NA                 
# 26 narrate         0.133  pure                    NA                 
# 27 proclaim        0.133  discourse participation discourse role verb
# 28 prophesy        0.133  pure                    NA                 
# 29 quarrel         0.133  discourse participation NA                 
# 30 recap           0.133  discourse participation discourse role verb
# 31 request         0.133  discourse participation NA                 
# 32 rule            0.133  discourse participation discourse role verb
# 33 demonstrate     0.138  state changing          NA                 
# 34 challenge       0.167  discourse participation NA                 
# 35 corroborate     0.167  discourse participation NA                 
# 36 debate          0.167  discourse participation NA                 
# 37 establish       0.167  discourse participation discourse role verb
# 38 express         0.167  pure                    NA                 
# 39 indict          0.167  discourse participation NA                 
# 40 insinuate       0.167  discourse participation NA                 
# 41 insist          0.167  state changing          discourse role verb
# 42 lie             0.167  state changing          discourse role verb
# 43 mark            0.167  pure                    discourse role verb
# 44 post            0.167  pure                    mode verb          
# 45 reassert        0.167  discourse participation discourse role verb
# 46 submit          0.167  discourse participation NA                 
# 47 tease           0.167  discourse participation mode verb  

# communicative predicates with similar projection ratings as 'know': 
# (know - 0.2) to max [= know + 0.2] 
bbb <- mean.proj.acc %>% filter(predicateType == "communicative" & 
                           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.2) %>% 
  arrange(desc(Mean.Proj)) %>% 
  select(verb_renamed, Mean.Proj, commType, sayVerbType) %>% 
  print(n = Inf)
bbb %>% write.csv("../data/highly-projective-communicatives.csv")
# verb_renamed Mean.Proj commType                sayVerbType        
# <fct>            <dbl> <chr>                   <chr>              
# 1 cry              0.833 pure                    mode verb          
# 2 flip out         0.833 pure                    NA                 
# 3 cringe           0.8   pure                    NA                 
# 4 celebrate        0.733 pure                    NA                 
# 5 fess up          0.733 discourse participation NA                 
# 6 apologize        0.724 discourse participation discourse role verb
# 7 whine            0.7   pure                    mode verb          
# 8 approve          0.667 discourse participation NA                 
# 9 disclose         0.667 discourse participation NA                 
# 10 grimace          0.667 pure                    NA                 
# 11 pout             0.667 pure                    NA                 
# 12 bitch            0.633 discourse participation mode verb          
# 13 document         0.633 pure                    mode verb          
# 14 weep             0.633 pure                    mode verb          
# 15 complain         0.6   discourse participation mode verb          
# 16 conceal          0.6   state changing          NA                 
# 17 explain          0.6   discourse participation discourse role verb
# 18 stress           0.6   discourse participation NA                 
# 19 publicize        0.586 pure                    NA                 
# 20 flaunt           0.567 discourse participation NA                 
# 21 fuss             0.567 discourse participation NA                 
# 22 leak             0.567 discourse participation NA                 
# 23 point out        0.567 discourse participation discourse role verb
# 24 reveal           0.567 discourse participation NA                 
# 25 share            0.567 discourse participation NA                 
# 26 warn             0.552 state changing          NA                 
# 27 announce         0.533 discourse participation discourse role verb
# 28 cheer            0.533 pure                    mode verb          
# 29 expose           0.533 state changing          NA                 
# 30 log              0.533 pure                    mode verb          
# 31 reiterate        0.533 discourse participation discourse role verb
# 32 snitch           0.533 discourse participation NA                 
# 33 divulge          0.5   discourse participation NA                 
# 34 emphasize        0.5   pure                    NA                 
# 35 gab              0.5   pure                    NA                 
# 36 grumble          0.5   pure                    mode verb          
# 37 address          0.467 discourse participation NA                 
# 38 gasp             0.467 pure                    mode verb          
# 39 gush             0.467 pure                    mode verb          
# 40 moan             0.467 pure                    mode verb          
# 41 shriek           0.467 pure                    mode verb          
# 42 sob              0.467 pure                    mode verb          
# 43 whimper          0.467 pure                    mode verb          
# 44 admit            0.433 discourse participation discourse role verb
# 45 broadcast        0.433 pure                    mode verb          
# 46 detail           0.433 discourse participation NA                 
# 47 murmur           0.433 pure                    mode verb          
# 48 spout            0.433 pure                    NA                 
# 49 stammer          0.433 pure                    mode verb          
# 50 voice            0.433 pure                    NA    

### plots ----
# with labels for extreme predicates
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
                   colour = "dodgerblue4") +
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
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "dodgerblue4", "hotpink", "deeppink", "green4", "darkgreen"))
ggsave("../graphs/projection-by-communicative-extremes.pdf", height = 4, width = 13)


# with labels for 13/14 extreme predicates on both ends
# There are 14 predicates that project as much or more strongly than 'know'
# The 13 least projective predicates from 0 have a mean projection rating of up 
# to 0.0690. Predicates 14 to 22 have a rating of 0.1. Therefore, on the lower 
# end of the projection scale only 13 predicates are included.

ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                         Mean.Proj >= 0), Mean.Proj, n = 13), 
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
                   colour = "dodgerblue4") +
  geom_label_repel(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                                Mean.Proj >= 0), Mean.Proj, n = 13),
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
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "dodgerblue4", "hotpink", "deeppink", "green4", "darkgreen"))
ggsave("../graphs/projection-by-communicative-extremes-14.pdf", height = 4, width = 13)

# some more predicates - how many?
mean.proj.acc %>% 
  filter(predicateType == "communicative" & 
           Mean.Proj >= subset(mean.proj.acc, verb_renamed == "know")$Mean.Proj - 0.1) %>% 
  nrow() # 32
# The first 32 predicates from 0 include those with a mean projection rating of 
# up to 0.133. The 33rd predicate has a rating of 0.167. 32 is therefore a good 
# number of predicates to consider on both ends.

# with labels for 32 extreme predicates on both ends
ggplot(mean.proj.acc %>% 
         filter(predicateType == "communicative" | verb_renamed %in% c("think", "know")), 
       aes(x = verb_renamed, y = Mean.Proj)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(aes(colour = "communicative predicate"), alpha = 0.5) +
  geom_point(data = mean.proj.acc %>% filter(verb_renamed == "say"), 
             aes(x = verb_renamed, y = Mean.Proj, colour = "say")) +
  geom_point(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                          Mean.Proj >= 0), Mean.Proj, n = 32), 
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
                   colour = "dodgerblue4") +
  geom_label_repel(data = slice_min(mean.proj.acc %>% filter(predicateType == "communicative" & 
                                                                Mean.Proj >= 0), Mean.Proj, n = 32),
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
                   colour = "green4") +
  geom_label_repel(data = mean.proj.acc %>% filter(verb_renamed == "know"),
                   aes(label = verb_renamed), 
                   min.segment.length = 0,
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "darkgreen") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Predicate",
       y = "Mean projection rating") + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  scale_colour_manual(limits = c("communicative predicate", "say", "barely projective", "think", "highly projective", "know"),
                      values = c("deepskyblue2", "dodgerblue4", "hotpink", "deeppink", "green4", "darkgreen"))
ggsave("../graphs/projection-by-communicative-extremes-32.pdf", height = 4, width = 13)
