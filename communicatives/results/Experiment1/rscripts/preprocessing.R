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
# NOTE: this also excludes the two sets of responses recorded in the original data during test runs 
# of the survey prior to its publication on Prolific. The relevant number of excluded participants
# is therefore 9.
length(unique(e1$participant_id)) # 338
nrow(e1) # 4394 (no. of participants x 13 [11 stimuli, 2 controls])

# control items ---- 
control.yes <- e1 %>% 
  filter(str_detect(stimulus, "Alex")) %>%
  summarise(mean_response_yes = mean(response),
            sd_response_yes = sd(response)) 
#   mean_response_yes sd_response_yes
# 1           97.2574        11.92417
control.no <- e1 %>% 
  filter(str_detect(stimulus, "Taylor")) %>%
  summarise(mean_response_no = mean(response),
            sd_response_no = sd(response))
#   mean_response_no sd_response_no
# 1          6.06213       20.88869

e1 <- e1 %>%
  mutate(
    attention_check_yes = case_when(
      str_detect(stimulus, "Alex") & response < control.yes$mean_response_yes - 2*control.yes$sd_response_yes ~ "failed",
      str_detect(stimulus, "Alex") & response >= control.yes$mean_response_yes - 2*control.yes$sd_response_yes ~ "passed",
      TRUE ~ NA_character_),
    attention_check_no = case_when(
      str_detect(stimulus, "Taylor") & response > control.no$mean_response_no + 2*control.no$sd_response_no ~ "failed",
      str_detect(stimulus, "Taylor") & response <= control.no$mean_response_no + 2*control.no$sd_response_no ~ "passed",
      TRUE ~ NA_character_))

e1 %>% 
  filter(attention_check_yes == "failed") %>%
  select(participant_id, response, attention_check_yes) %>% 
  arrange(response)
#                          participant_id response attention_check_yes
# 1 fcda0aa2-e74f-4c62-ab51-a751b166569a        0              failed
# 2 01d115ca-d825-440b-9826-aa7cf0c11f3d        0              failed
# 3 89d80dcf-9861-457b-babb-16d6f56e522b        0              failed
# 4 022ded77-441e-4dbf-a72f-b8e2fe4ee8ec       11              failed
# 5 eebc8d1f-e391-42a5-9698-52d0bb05519f       48              failed
# 6 f22bb5f5-4056-49a2-b7da-5ba9f54f1605       54              failed
# 7 69ccdc2a-8a00-4a7d-8cc5-f460dfa31967       70              failed
# 8 39e9d351-be5c-4249-a477-e7f2b75912bf       71              failed

e1 %>% 
  filter(attention_check_no == "failed") %>%
  select(participant_id, response, attention_check_no) %>% 
  arrange(desc(response))

#                          participant_id response attention_check_no
# 1  366790d9-c818-4aa2-aebe-fb871c1a3a5d      100             failed
# 2  ac0a886d-f06b-4a4c-8399-9fe7ece72b8a      100             failed
# 3  603872f7-3a8c-44c0-a747-94a25c4c16d5      100             failed
# 4  652d8fd0-5366-44b6-a581-ebd2e0712294      100             failed
# 5  42480b51-d437-4880-86e1-32f1f479f367      100             failed
# 6  4e0a537a-6867-4cb5-81ad-e6a02f252064      100             failed
# 7  a88305c9-2db9-4f2a-a6ae-d75812cce64b      100             failed
# 8  63801cfc-65fe-4346-8be8-91d63e3f91a4      100             failed
# 9  d45939b9-5178-45e9-a6fe-5c2a4f74162c      100             failed
# 10 7290dec8-d11d-4f93-9c0f-3f92e208aeeb      100             failed
# 11 5d2423ad-1350-4be3-9c3d-06b08d53b252      100             failed
# 12 8fca6f9a-ba6c-4197-8a5f-b7d32189dfb0       95             failed
# 13 39e9d351-be5c-4249-a477-e7f2b75912bf       82             failed
# 14 ee5bd867-e2ac-45cb-ace3-c016b4cdbe5b       78             failed
# 15 69ccdc2a-8a00-4a7d-8cc5-f460dfa31967       77             failed
# 16 aff67e52-5a52-4f5d-9b9a-76c774966447       75             failed
# 17 eebc8d1f-e391-42a5-9698-52d0bb05519f       57             failed
# 18 f22bb5f5-4056-49a2-b7da-5ba9f54f1605       54             failed
# 19 34259e98-de49-4125-ac8b-80415bb8275d       52             failed
# 20 940b85aa-6b2f-4e6f-912d-d3cac2a0b293       49             failed 

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
e1 %>% filter(attention_check_yes == "failed") %>% distinct(participant_id) %>% nrow() # 8
e1 %>% filter(attention_check_no == "failed") %>% distinct(participant_id) %>% nrow() # 20
e1 %>% filter(attention_check_yes == "failed" & attention_check_no == "failed") %>% 
   distinct(participant_id) %>% nrow() # 4
# For the "yes" control, 8 participants' response was more than two standard deviations below the mean.
# For the "no" control, 20 participants' response was more than two standard deviations above the mean.
# 4 participant failed both attention checks.

# extract comments
comments <- e1 %>%
  filter(! is.na(comments)) %>%
  distinct(participant_id, comments, attention_check_yes, attention_check_no)
write.csv(comments, "../data/comments.csv", row.names = FALSE)
# NOTE: there were no relevant comments by non-native/non-American English speakers, i.e., their
# previous exclusion (lines 66-67) does not affect the extraction of possibly insightful feedback.

# exclude the 24 participants who failed at least one of the attention checks
e1 <- e1 %>% 
  filter(attention_check_yes == "passed" & attention_check_no == "passed")
length(unique(e1$participant_id)) # 314
nrow(e1) # 3454 (no. of participants x 11 stimuli)

# variance ----
variances <- e1 %>%
  group_by(participant_id) %>% 
  summarize(variance = var(response),
            mean_response = mean(response)) %>%
  mutate(mean_variance = mean(variance),
         sd_variance = sd(variance),
         low_variance = variance < mean_variance - 2 * sd_variance) %>% 
  arrange(variance) %>% print()
#  participant_id                       variance mean_response mean_variance sd_variance low_variance
#   <chr>                                   <dbl>         <dbl>         <dbl>       <dbl> <lgl>       
# 1 0a3ad530-4aab-45c6-a362-475078a28f2c        0           100          833.        742. FALSE       
# 2 1984e371-3866-4945-8282-83882ea9260a        0           100          833.        742. FALSE       
# 3 1c7b4f5a-482b-4a3a-a483-bd8098c76b6e        0           100          833.        742. FALSE       
# ...
# The mean of participants' variances is 833, the standard deviation of these variances is 742. Therefore,
# no participant's variance is more than 2 standard deviations below mean by-participant variance, not
# even for those with a variance of 0, and no-one's responses are excluded from analysis because of 
# this criterion. The plot below shows the skewed distribution of the variances. Low variance is very 
# frequent in the dataset; high variance is relatively rare. 

write.csv(variances, "../data/variances.csv", row.names = FALSE)

ggplot(variances, aes(variance)) + 
  geom_histogram(bins = 100) + 
  geom_vline(aes(xintercept = mean_variance, colour = "μ"), linewidth = 0.7) + 
  geom_vline(aes(xintercept = mean_variance - 1 * sd_variance, colour = "μ ± σ"), linetype = 2, linewidth = 0.7) + 
  geom_vline(aes(xintercept = mean_variance + 1 * sd_variance, colour = "μ ± σ"), linetype = 2, linewidth = 0.7) +
  geom_vline(aes(xintercept = mean_variance - 2 * sd_variance, colour = "μ ± 2σ"), linetype = 2, linewidth = 0.7) +
  geom_vline(aes(xintercept = mean_variance + 2 * sd_variance, colour = "μ ± 2σ"), linetype = 2, linewidth = 0.7) +
  geom_vline(aes(xintercept = mean_variance - 3 * sd_variance, colour = "μ ± 3σ"), linetype = 2, linewidth = 0.7) +
  geom_vline(aes(xintercept = mean_variance + 3 * sd_variance, colour = "μ ± 3σ"), linetype = 2, linewidth = 0.7) + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(11, "pt"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(5.5, 5.5, 11, 5.5, "pt")) +
  labs(x = "Variance",
       y = "Count (with 100 bins)") +
  guides(colour = guide_legend(byrow = TRUE)) + 
scale_colour_manual(breaks = c("μ", "μ ± σ", "μ ± σ", "μ ± 2σ", "μ ± 2σ", "μ ± 3σ", "μ ± 3σ"),
                    values = c("black", "blue", "blue", "red", "red", "grey70", "grey70"))
ggsave("../graphs/variances.pdf", height = 8, width = 8, device = cairo_pdf)

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
