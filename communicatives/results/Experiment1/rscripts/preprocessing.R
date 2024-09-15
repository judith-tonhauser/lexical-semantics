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
e1 = read.csv("../data/combined.csv")
nrow(e1) # 121
length(unique(e1$participant_id)) # 7

# delete bot captcha and consent response rows
e1 <- e1 %>% filter(! trial_type %in% c("survey-text", "html-button-response"))
nrow(e1) # (no. of participants x 14 [11 stimuli, 2 controls, 1 demographics])

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
length(unique(e1$participant_id)) # 7
nrow(e1) # (no. of participants x 13 [11 stimuli, 2 controls])

# separate the demographics string into individual columns
e1 <- e1 %>%
  mutate(age = str_extract(demographics, '"age":"(\\d+)"') %>% 
           str_remove_all('"age":"|\"'),
         gender = str_extract(demographics, '"gender":"(.*?)"') %>% 
           str_remove_all('"gender":"|\"'),
         gender_comment = str_extract(demographics, '"gender-Comment":"(.*?)"') %>% 
           str_remove_all('"gender-Comment":"|\"'),
         native_English = str_extract(demographics, '"language":"(.*?)"') %>% 
           str_remove_all('"language":"|\"'),
         American_English = str_extract(demographics, '"AE":"(.*?)"') %>% 
           str_remove_all('"AE":"|\"'),
         comments = str_extract(demographics, '"comments":"(.*?)"') %>% 
           str_remove_all('"comments":"|\"')) %>%
  # Replace empty values with NA
  mutate(across(c(age, gender, gender_comment, native_English, American_English, comments), ~ na_if(., ""))) %>% 
  # rescale projection ratings to range from -1 (no) to 1 (yes)
  mutate(response = as.integer(response),
    rating = response/50-1)

# only include native speakers of American English
e1 <- e1 %>% 
  filter(native_English == "yes" & American_English == "yes")
# Note: this also excludes the two sets of responses recorded in the original data 
# during test runs of the survey prior to its publication on Prolific.
length(unique(e1$participant_id)) # 5
nrow(e1) #  (no. of participants x 13 [11 stimuli, 2 controls])

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

# repeat the values in attention_check_yes and attention_check_no in each row 
# for each participant
e1 <- e1 %>%
  group_by(participant_id) %>%
  fill(attention_check_yes, attention_check_no, .direction = "downup") %>%
  ungroup()

# remove the rows that contain the control items
e1 <- e1 %>%
  filter(!str_detect(stimulus, "Alex|Taylor"))
length(unique(e1$participant_id)) # 5
nrow(e1) # 55 # (no. of participants x 11 stimuli)

# extract information from stimuli into separate columns
e1 <- e1 %>%
  mutate(
    name = str_extract(stimulus, "(?<=<i>)[^ ]+"),
    predicate = str_extract(stimulus, "(?<=didn't )[\\w\\s]+(?= that)"),
    CC = str_extract(stimulus, "(?<=that ).+?(?=\\.)")
  )

# exclude participants who failed either of the attention checks
e1 <- e1 %>% 
  filter(attention_check_yes == "passed" & attention_check_no == "passed")

length(unique(e1$participant_id)) # 5
nrow(e1) # 55 (no. of participants x 11 stimuli)

view(e1)

# select columns relevant for investigation
e1 <- e1 %>% 
  select(predicate, rating, name, CC, participant_id, age, gender, rt, trial_index)

view(e1)

# save data
write.csv(e1, "../data/e1.csv", row.names = FALSE)
