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
e = read.csv("../data/combined.csv")
nrow(e) # 5951
length(unique(e$participant_id)) # 349

# delete bot captcha and consent response rows
e1 <- e %>% filter(! trial_type %in% c("survey-text", "html-button-response"))
nrow(e1) # 4886 (no. of participants x 14 [11 stimuli, 2 controls, 1 demographics])

# demographics ----
# identify rows that contain demographics data
demographics_rows <- e1 %>%
  filter(trial_type == "survey")

# create the "demographics" column for every row based on participant_id
e1 <- e1 %>%
  left_join(demographics_rows %>% select(participant_id, demographics = response), 
            by = "participant_id")

# remove demographics rows from the original data
e1 <- e1 %>%
  filter(trial_type != "survey")
length(unique(e1$participant_id)) # 349
nrow(e1) # 4537 (no. of participants x 13 [11 stimuli, 2 controls])

# separate the demographics string into individual columns
e1 <- e1 %>%
  mutate(age = str_extract(demographics, '(?<=\\"age\\":\\")\\d+'),
         gender = str_extract(demographics, '(?<=\\"gender\\":\\").*?(?=\\")'),
         gender_comment = str_extract(demographics, '(?<=\\"gender-Comment\\":\\").*?(?=\\")'),
         native_English = str_extract(demographics, '(?<=\\"language\\":\\").*?(?=\\")'),
         American_English = str_extract(demographics, '(?<=\\"AE\\":\\").*?(?=\\")'),
         comments = str_extract(demographics, '(?<=\\"comments\\":\\").*?(?=\\")')) %>%
  # Replace empty values with NA
  mutate(across(c(age, gender, gender_comment, native_English, American_English, comments), ~ na_if(., ""))) 

# ratings ----
# rescale projection ratings to range from -1 (no) to 1 (yes)
e1 <- e1 %>% 
  mutate(response = as.integer(response), rating = response/50-1)

# language ----
# how many participants are not native speakers of American English?
# e1 %>% filter(native_English == "no") %>% distinct(participant_id) %>% nrow() # 1
# e1 %>% filter(American_English == "no") %>% distinct(participant_id) %>% nrow() # 6
# e1 %>% filter(is.na(native_English)) %>% distinct(participant_id) %>% nrow() # 5
# e1 %>% filter(is.na(American_English)) %>% distinct(participant_id) %>% nrow() # 1
# e1 %>% filter(native_English == "no" | American_English == "no" | is.na(native_English) |
#                  is.na(American_English)) %>% distinct(participant_id) %>% nrow() # 11

# exclude participants who are not native speakers of American English
e1 <- e1 %>% 
  filter(native_English == "yes" & American_English == "yes")
# NOTE: this also excludes the two sets of responses recorded in the original data 
# during test runs of the survey prior to its publication on Prolific.
length(unique(e1$participant_id)) # 338
nrow(e1) # 4394 (no. of participants x 13 [11 stimuli, 2 controls])

# control items ---- 
# create columns to record whether a participant passed the attention checks 
e1 <- e1 %>%
  mutate(
    attention_check_yes = case_when(
      str_detect(stimulus, "Alex") & response >= 90 ~ "passed",
      str_detect(stimulus, "Alex") & response < 90 ~ "failed",
      TRUE ~ NA_character_),
    attention_check_no = case_when(
      str_detect(stimulus, "Taylor") & response <= 10 ~ "passed",
      str_detect(stimulus, "Taylor") & response > 10 ~ "failed",
      TRUE ~ NA_character_)
  )

# repeat the values in attention_check_yes and attention_check_no in each row for each participant
e1 <- e1 %>%
  group_by(participant_id) %>%
  fill(attention_check_yes, attention_check_no, .direction = "downup") %>%
  ungroup()

# remove the rows that contain the control items
e1 <- e1 %>%
  filter(!str_detect(stimulus, "Alex|Taylor"))
length(unique(e1$participant_id)) # 338
nrow(e1) # 3718 (no. of participants x 11 stimuli)

# how many native American English speaking participants failed which of the attention checks?
# e1 %>% filter(attention_check_yes == "failed") %>% distinct(participant_id) %>% nrow() # 24
# e1 %>% filter(attention_check_no == "failed") %>% distinct(participant_id) %>% nrow() # 32
# e1 %>% filter(attention_check_yes == "failed" & attention_check_no == "failed") %>% 
#   distinct(participant_id) %>% nrow() # 11

# participants who failed the attention check(s) with responses
# e1 %>% filter(attention_check_yes == "failed") %>% select(participant_id, response)
# e1 %>% filter(attention_check_no == "failed") %>% select(participant_id, response)

# extract comments
# comments <- e1 %>% 
#   filter(! is.na(comments)) %>%
#   distinct(participant_id, comments, attention_check_yes, attention_check_no)
# write.csv(comments, "../data/comments.csv", row.names = FALSE)
# NOTE: there were no relevant comments by non-native/non-American English speakers, i.e., their
# previous exclusion (lines 66-67) does not affect the extraction of possibly insightful feedback.

# exclude the 45 participants who failed at least one of the attention checks
e1 <- e1 %>% 
  filter(attention_check_yes == "passed" & attention_check_no == "passed")
length(unique(e1$participant_id)) # 293
nrow(e1) # 3223 (no. of participants x 11 stimuli)

# stimuli ----
# extract information from stimuli into separate columns
e1 <- e1 %>%
  mutate(
    name = str_extract(stimulus, "(?<=<i>)[^ ]+"),
    predicate = str_extract(stimulus, "(?<=didn't )[\\w\\s]+(?= that)"),
    CC = str_extract(stimulus, "(?<=that ).+?(?=\\.)")
  )

view(e1)

# select columns relevant for investigation
e1 <- e1 %>% 
  select(predicate, rating, name, CC, participant_id, age, gender, rt, trial_index)

view(e1)

# save data
write.csv(e1, "../data/e1.csv", row.names = FALSE)
