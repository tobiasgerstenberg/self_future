# Load packages  ------------------------------------------------------------------------------

library(Hmisc)
library(afex)
library(tidyverse)

# Read in data  -------------------------------------------------------------------------------

df.data = read.csv("../../data/self_future_contrastive.csv",stringsAsFactors = F)

df.long = df.data %>% 
  filter(Progress == '100') %>% 
  select(X1_Valence.Relig_12:X4_RIntensity.Job_12) %>% 
  set_names(colnames(.) %>% 
              tolower %>% 
              str_replace_all("_12","")) %>% 
  mutate_all(funs(as.numeric(.))) %>% 
  mutate_at(vars(contains("_r")),funs(100-.)) %>% #scale reversal for counterbalanced scenarios
  mutate(participant = 1:nrow(.)) %>% 
  gather(situation,rating,-participant) %>% 
  filter(rating != "") %>% 
  mutate(valence_left = NA,
         intensity_left = NA,
         perspective_left = NA,
         valence_right = NA,
         intensity_right = NA,
         perspective_right = NA,
         valence_left = ifelse(str_detect(situation,paste(c("valence", "x1_perspective", "x2_perspective", "x1_intensity", "x3_intensity"), collapse = "|")),'positive','negative'),
         intensity_left = ifelse(str_detect(situation,paste(c("intensity", "x1_perspective", "x3_perspective", "x1_valence", "x3_valence"), collapse = "|")),'high','low'),
         perspective_left = ifelse(str_detect(situation,paste(c("perspective", "x1_valence", "x2_valence", "x1_intensity", "x2_intensity"), collapse = "|")),'near','far'),
         
         valence_right = ifelse(str_detect(situation,paste(c("valence", "x3_perspective", "x4_perspective", "x2_intensity", "x4_intensity"), collapse = "|")),'negative','positive'),
         intensity_right = ifelse(str_detect(situation,paste(c("intensity", "x2_perspective", "x4_perspective", "x2_valence", "x4_valence"), collapse = "|")),'low','high'),
         perspective_right = ifelse(str_detect(situation,paste(c("perspective", "x3_valence", "x4_valence", "x3_intensity", "x4_intensity"), collapse = "|")),'far','near'),
         situation = str_extract_all(situation, paste(c('relig','job'),collapse = "|")),
         situation = situation %>% unlist(),
         scenario = ifelse(situation == 'relig','religion',situation),
         rating = as.numeric(rating)
         ) %>% 
  select(-rating,everything(),-situation,rating) %>% 
  arrange(participant)

#rating: 0 = preference for simulating left scenario, 100 = preference for simulating right scenario 

# Analysis: t-tests  --------------------------------------------------------------------------

# HYPOTHESIS 1: We expect that participants are more likely to simulate scenarios with positive compared to negative valence 

df.valence = df.long %>% 
  filter(valence_left != valence_right) %>%
  mutate(rating = ifelse(valence_left == 'positive',100-rating,rating),
         valence = 'positive_vs_negative') %>% 
  select(participant,scenario,valence,rating) %>% 
  group_by(participant,valence) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup()

t.test(df.valence$rating,alternative = "greater", mu = 50)

# HYPOTHESIS 2: We expect that participants are more likely to simulate scenarios that are less intense compared to more intense

df.intensity = df.long %>% 
  filter(intensity_left != intensity_right) %>%
  mutate(rating = ifelse(intensity_left == 'high',100-rating,rating),
         intensity = 'low_vs_high') %>% 
  select(participant,scenario,intensity,rating) %>% 
  group_by(participant,intensity) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup()

# t.test: mean is greater than 50 
t.test(df.intensity$rating,alternative = "greater", mu = 50)

# HYPOTHESIS 3: We expect that participants are more likely to simulate scenarios that feel near rather than far away 

df.perspective = df.long %>% 
  filter(perspective_left != perspective_right) %>%
  mutate(rating = ifelse(perspective_left == 'near',100-rating,rating),
         perspective = 'near_vs_far') %>% 
  select(participant,scenario,perspective,rating) %>% 
  group_by(participant,perspective) %>% 
  summarise(rating = mean(rating)) %>% 
  ungroup()

# t.test: mean is greater than 50 
t.test(df.perspective$rating,alternative = "greater", mu = 50)

