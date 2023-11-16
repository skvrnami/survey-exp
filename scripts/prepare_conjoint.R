library(here)
library(dplyr)
library(haven)
library(cregg)

cj_data <- read_dta(here("data/conjoint_data.dta"))
cj_data2 <- cj_data %>% 
  mutate(
    gender = factor(orig_cand_gender_string, levels = c("Male", "Female")),
    experience = case_when(
      orig_0ys == 1 ~ "No experience", 
      orig_1ys == 1 ~ "1 year in politics",
      orig_3ys == 1 ~ "3 years in politics", 
      orig_8ys == 1 ~ "8 years in politics" 
    ) %>% factor(., levels = c("No experience", 
                               "1 year in politics",
                               "3 years in politics", 
                               "8 years in politics" )), 
    spouse = case_when(
      orig_UN_sp == 1 ~ "Unmarried", 
      orig_FM_sp == 1 ~ "Farmer", 
      orig_MD_sp == 1 ~ "Doctor"
    ) %>% factor(., levels = c("Unmarried", 
                               "Farmer", 
                               "Doctor")), 
    occupation = case_when(
      orig_teach == 1 ~ "teacher", 
      orig_law == 1 ~ "corporate lawyer", 
      orig_may == 1 ~ "mayor", 
      orig_leg == 1 ~ "state legislator"
    ) %>% factor(., levels = c("teacher", "corporate lawyer", 
                               "mayor", "state legislator")), 
    children = case_when(
      orig_0ch == 1 ~ "No children", 
      orig_1ch == 1 ~ "1 child", 
      orig_3ch == 1 ~ "3 children"
    ) %>% factor(., levels = c("No children", 
                               "1 child", 
                               "3 children")), 
    age = case_when(
      orig_29 == 1 ~ "29", 
      orig_45 == 1 ~ "45", 
      orig_65 == 1 ~ "65"
    ) %>% factor(., levels = c("29", "45", "65")), 
    respondent_party_id = case_when(
      democrat_respondent == 1 ~ "Democrat", 
      republican_respondent == 1 ~ "Republican"
    ) %>% factor(., levels = c("Democrat", "Republican")), 
    respondent_gender = case_when(
      female_respondent == 1 ~ "Female", 
      TRUE ~ "Male" 
    ) %>% factor(., levels = c("Male", "Female")), 
    respondent_gender_party = case_when(
      respondent_party_id == "Democrat" & respondent_gender == "Female" ~ "Democrat female",
      respondent_party_id == "Democrat" & respondent_gender == "Male" ~ "Democrat male",
      respondent_party_id == "Republican" & respondent_gender == "Female" ~ "Republican female",
      respondent_party_id == "Republican" & respondent_gender == "Male" ~ "Republican male"
    ) %>% factor(., levels = c("Republican male", "Republican female", 
                               "Democrat male", "Democrat female"))
  ) %>% select(responseid, dv, contest, winner, sample, 
               gender, experience, spouse, occupation, children, age, 
               respondent_party_id, respondent_gender, 
               respondent_gender_party)

voters <- cj_data2 %>% filter(sample == "usa voter")
legislators <- cj_data2 %>% filter(sample == "usa leg")

saveRDS(voters, "data/voters.rds")
saveRDS(legislators, "data/legislators.rds")

