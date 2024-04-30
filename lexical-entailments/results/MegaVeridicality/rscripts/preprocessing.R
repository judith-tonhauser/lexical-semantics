# Testing hypotheses based on 
# White & Rawlins' MegaVeridicality I dataset 
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(ggrepel)

# set the background of figures to white
theme_set(theme_bw())

# load raw data
# d: judgments of "did that thing happen?" for positive and negated predicates with "that" complements
d = read.csv("../data/mega-veridicality-v1/mega-veridicality-v1.csv")

nrow(d) #21760

### exclude nonAmerican English speakers
length(unique(d$participant)) #291

table(d$nativeenglish)

str(d$nativeenglish)

d <- droplevels(subset(d, (d$nativeenglish != "False")))

length(unique(d$participant)) #290

# recode their responses to numerical values
# MV has ratings on a 3-point Likert scale in response to "Did that thing happen?"
# "veridicality" codes the response options: "yes" (happened) "no" (didn't happen) "maybe" (maybe happened)

# create veridicality_num which codes "yes" as 1, "no" as -1 and "maybe" as 0
table(d$veridicality)
str(d$veridicality)

d$veridicality_num[d$veridicality == "maybe"] <- 0 
d$veridicality_num[d$veridicality == "no"] <- -1
d$veridicality_num[d$veridicality == "yes"] <- 1
str(d$veridicality_num)

# check that all is in order
table(d$veridicality)
table(d$veridicality_num)

table(d$verb)
table(d$verb) #verb
table(d$frame) #only that_S
table(d$voice) #active, passive
table(d$polarity) #negative, positive
table(d$conditional) #true, false

str(d$conditional)
str(d$conditional2)
d$conditional2 <- as.character(d$conditional)
d$conditional2[d$conditional2 == "True"] <- "conditional"
d$conditional2[d$conditional2 == "False"] <- "matrix"

table(d$conditional2) #conditional, matrix

d$item <- paste(d$verb,d$frame,d$voice,d$polarity,d$conditional2, sep = "-")
table(d$item)

# save data
write.csv(d, "../data/d.csv")
nrow(d) #21692

