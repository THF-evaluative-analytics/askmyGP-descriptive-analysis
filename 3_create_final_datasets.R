# =========================================#
#                                          #
#     DIGITAL FIRST PRIMARY CARE STUDY     #
#                                          #
#        AskmyGP Phase I Analysis          #
# -----------------------------------------#
#                                          #
# (3_create_final_datasets.r):
#                                          #
# (1)  Explore AskmyGP data                #
# (2)  Create summary plots                #
#                                          #
# -----------------------------------------#
#                                          #
# Authors:                                 #
# Geraldine Clarke                         #
# Paris Pariza                             #
#                                          #
# =========================================#


# Load preamble ----

source(here::here("R", "codes for GitHub", paste0("0_preamble.R")))


# Read in data ----

df_england <- readRDS(here::here('Data', 'clean data', 'up to july2020', version, 'all_request_england.rds'))  

fb_england = readRDS(here::here('Data', 'clean data', 'up to july2020', version, 'all_feedback_england.rds'))

gp_ref_in <- read_csv(here::here("Data", "clean data", 'up to july2020', version, "gp_ref_clean.csv"))

gp_ref = gp_ref_in %>%
  group_by(year_month) %>%
  mutate(imd_rank = row_number(imdscore15)) %>%
  mutate(imd_quintile = ntile(imd_rank, 5)) %>%
  mutate(imd_high = ntile(imd_rank, 2)-1) %>%
  mutate(imd_diff = ifelse(imd_quintile %in% c(1,2), 1, 
                           ifelse(imd_quintile %in% c(4,5), 2, NA))) %>%
  mutate(white_rank = row_number(white)) %>%
  mutate(white_quintile = ntile(white_rank, 5)) %>%
  mutate(white_high = ntile(white_rank, 2)-1) %>%
  mutate(white_diff = ifelse(white_quintile %in% c(1,2), 1, 
                             ifelse(white_quintile %in% c(4,5), 2, NA))) %>%
  as.data.frame

gp_ref_size = gp_ref %>%
  dplyr::select(practice_code, year_month, gp_size, gpfte) %>%
  as.data.frame()

amg_list <- read.csv(here::here("Data", "clean data", "up to july2020", version, "amg_list.csv"))

amg_summary_data= read.csv(here::here("Data", "clean data", "up to july2020", "amg_summary_data.csv"))

early_set_list <- read.csv(here::here('Data', 'clean data', 'up to july2020', version, "early_set_list.csv")) %>% as.data.frame()
late_set_list <- read.csv(here::here('Data', 'clean data', 'up to july2020', version, "late_set_list.csv")) %>% as.data.frame()
covid_set_list <- read.csv(here::here('Data', 'clean data', 'up to july2020', version, "covid_set_list.csv")) %>% as.data.frame()
pre_covid_set_list <- read.csv(here::here('Data', 'clean data', 'up to july2020', version, "pre_covid_set_list.csv")) %>% as.data.frame()


# Manipulate data ----

#Create datasets

##requests data

df1 <- df_england %>%
  unite("year_month", c(thread_opened_year, thread_opened_month)) %>%
  left_join(amg_summary_data, by=c("practice_code", "year_month")) %>%
  filter(closure_method_f %in% closure_types_all) %>%
  mutate(age_cat=case_when(age_groups=="0-4"~"age00_04",
                           age_groups=="5-9"~"age05_14",
                           age_groups=="10-14"~"age05_14",
                           age_groups=="15-19"~"age15_24",
                           age_groups=="20-24"~"age15_24",
                           age_groups=="25-29"~"age25_64",
                           age_groups=="30-34"~"age25_64",
                           age_groups=="35-39"~"age25_64",
                           age_groups=="40-44"~"age25_64",
                           age_groups=="45-49"~"age25_64",
                           age_groups=="50-54"~"age25_64",
                           age_groups=="55-59"~"age25_64",
                           age_groups=="60-64"~"age25_64",
                           age_groups=="65-69"~"age65_74",
                           age_groups=="70-74"~"age65_74",
                           patient_age_groups>=75~"age75over")) %>%
  mutate(age_groups_2=ifelse(age_groups %in% age_group_0, "<20",
                             ifelse(age_groups %in% age_group_20, "20-64",
                                    ifelse(age_groups %in% age_group_65, ">64", 
                                           "unknown")))) %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06"), "2020_03", paste(year_month))) %>%
  mutate(closure_method_f = factor(closure_method_f)) %>%
  mutate(repeat_user = ifelse(duplicated(patient_id), 1, 0)) %>%  
  mutate(time = match(year_month, sort(unique(year_month)))) %>% 
  mutate(adopter = ifelse(practice_code %in% early_set_list$practice_code,'early_adopter',
                          ifelse(practice_code %in% late_set_list$practice_code, 'late_adopter',
                                 ifelse(practice_code %in% covid_set_list$practice_code, 'covid_adopter',
                                        ''))) ) %>% 
  mutate(adopter_2 = ifelse(practice_code %in% pre_covid_set_list$practice_code,'pre_covid_adopter',
                                 ifelse(practice_code %in% covid_set_list$practice_code, 'covid_adopter',
                                        '')) ) %>% 
  mutate(uptake_cat_max = ifelse(max_all < 15, 0, ifelse(max_all < 25, 1, 2) )) %>%
  mutate(uptake_cat_month = ifelse(uptake < 15, 0, ifelse(uptake < 25, 1, 2) )) %>%
  mutate(dow_cat = ifelse(thread_opened_dow %in% c("Monday", "Tuesday", "Wednesday"),'early weekday',
                          ifelse(thread_opened_dow %in% c("Thursday", "Friday"),'mid weekday',
                                 'weekend'))) %>%
  mutate(days_to_close = as.character(thread_closed_date - thread_opened_date)) %>% 
  mutate(days_to_close_cat = ifelse(days_to_close == "0", "0 ", 
                                    ifelse(days_to_close == "1", "1",
                                           ifelse(days_to_close == "2", "2", 
                                                  ">2"))) )%>% 
  mutate(registration = ifelse(adopter=="covid_adopter", "covid", "early")) %>%
  mutate(attach_true = ifelse(attachments_v2>0, 1, 0)) %>%
  left_join(gp_ref, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  as.data.frame()


##feedback data

fb1 = fb_england %>%
  filter(practice_code %in% amg_practices) %>%
  left_join(ccgs_list, by="practice_code") %>% 
  unite("year_month", c("year", "month")) %>%
  mutate(time = match(year_month, sort(unique(year_month)))) %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06"), "2020_03", paste(year_month))) %>%
  left_join(amg_summary_data, by=c("practice_code", "year_month")) %>%
  left_join(gp_ref, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(repeat_user = ifelse(duplicated(unique_id), 1, 0)) %>%  
  mutate(adopter = ifelse(practice_code %in% early_set_list$practice_code,'early_adopter',
                          ifelse(practice_code %in% late_set_list$practice_code, 'late_adopter',
                                 ifelse(practice_code %in% covid_set_list$practice_code, 'covid_adopter',
                                        ''))) ) %>% 
  mutate(adopter_2 = ifelse(practice_code %in% pre_covid_set_list$practice_code,'pre_covid_adopter',
                            ifelse(practice_code %in% covid_set_list$practice_code, 'covid_adopter',
                                   '')) ) %>% 
  mutate(uptake_cat_max = ifelse(max_all < 15, 0, ifelse(max_all < 25, 1, 2) )) %>%
  mutate(uptake_cat_month = ifelse(uptake < 15, 0, ifelse(uptake < 25, 1, 2) )) %>%
  mutate(overall_is_the_new_system_cat = ifelse(overall_is_the_new_system == "Better",'Better', 'Not Better')) %>%
  mutate(please_rate_your_experience_ease_of_use_cat = ifelse(please_rate_your_experience_ease_of_use %in% c("Good", "Very Good")
                                                              ,'Positive', 'Not Positive')) %>%
  mutate(please_rate_your_experience_solving_your_problem_cat = ifelse(please_rate_your_experience_solving_your_problem %in% c("Good", "Very Good")
                                                              ,'Positive', 'Not Positive')) %>%
  mutate(please_rate_your_experience_speed_cat = ifelse(please_rate_your_experience_speed %in% c("Good", "Very Good")
                                                                       ,'Positive', 'Not Positive')) %>%
  rename(imd_cat=imd_quintile) %>% 
  as.data.frame()


## Write files out for easy use later ----

saveRDS(df1, here::here('Data', 'clean data', 'up to july2020', version,  'amg_tidy_df.rds'))
saveRDS(fb1, here::here('Data', 'clean data', 'up to july2020', version,  'amg_tidy_fb.rds'))
write_csv(gp_ref, here::here('Data', 'clean data', 'up to july2020', version,  'amg_tidy_gp_ref.csv'))

