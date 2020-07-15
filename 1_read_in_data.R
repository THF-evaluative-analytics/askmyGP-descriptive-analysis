# =========================================#
#                                          #
#     DIGITAL FIRST PRIMARY CARE STUDY     #
#                                          #
#        AskmyGP Phase I Analysis          #
# -----------------------------------------#
#                                          #
# (1_read_in_data.r):                      #
#                                          #
# (1)  Read AskmyGP data                   #
# (2)  Format and rename variables         #
#                                          #
# -----------------------------------------#
#                                          #
# Authors:                                 #
# Emma Vestesson                           #
# Geraldine Clarke                         #
# Paris Pariza                             #
#                                          #
# =========================================#


# Load preamble ----

source(here::here("R", "codes for GitHub", paste0("0_preamble.R")))


# Read in all data ----

#AMG data - requests
amGP_2020m7 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data 9 july to 7 July 2020/9 july-7july data.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2020m6 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data 19 May to 8 june 2020/askmygp activity 19may-8june.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2020m5 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data 23 Apr to 18 May 2020/23 apr-18may askmygp activity.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2020m4 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Feb to Apr 2020/22 feb-22 apr askmygp activity.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2020m2 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Jan 2019 to Feb 2020/2020 data to 22 feb.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2019q1q2 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Jan 2019 to Feb 2020/2019 Q1 and Q2 data.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2019q3q4 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Jan 2019 to Feb 2020/2019 Q3 and Q4 data.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
amGP_2018 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Sep 2018 to Dec 2018/2018 data.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)

df.in <- bind_rows(amGP_2020m7, amGP_2020m6, amGP_2020m5, amGP_2020m4, amGP_2020m2, amGP_2019q1q2, amGP_2019q3q4, amGP_2018) %>% 
  janitor::clean_names() %>%
  as.data.table()

#AMG data - feedback
feedback_2020m2 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Jan 2019 to Feb 2020/feedback responses.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
feedback_2020m4 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data Feb to Apr 2020/askmygp feedback 22feb-22apr 2020.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
feedback_2020m5 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data 23 Apr to 18 May 2020/23 apr-18may askmygp feedback.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
feedback_2020m6 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data 19 May to 8 june 2020/feedback 19may-8june.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)
feedback_2020m7 <- read.csv("D:/D050 IAU DFPC AskmyGP/Initial Data 9 july to 7 July 2020/9 july-7 july feedback.csv", fileEncoding= 'UTF-16', sep='\t', stringsAsFactors = FALSE)

#GP Reference File
gp_ref <- read_csv(here::here("Data", "Original data", "gp_ff_new.csv")) 

#Format fields in file
gp_ref_clean = gp_ref %>%
  rename(practice_code = gpprac) %>%
  mutate(month_pad = str_pad(month, 2, pad="0")) %>%
  unite("year_month", c(year, month_pad), remove=F) %>%
  filter(year_month %in% study_year_month) %>%
  dplyr::select(-month_pad) %>%
  dplyr::select(-time, -year, -month, -london, -fyr, -fqtr, -qof_gp_size) %>%
  as.data.frame()


# Create new variables, rename existing ones, filter data ----

df_N <- df.in[,.N, by=practice_code]

df <- left_join(df.in,  df_N) %>% 
  mutate(attachments_v2=replace_na(attachments_v2,0),
         thread_opened_date_time=dmy_hms(all_thread_opened_time),
         thread_opened_date=date(thread_opened_date_time),
         thread_opened_time=format(thread_opened_date_time, '%H:%M:%S'),
         thread_opened_hour=hour(thread_opened_date_time),
         thread_opened_week=week(thread_opened_date), 
         thread_opened_dow=lubridate::wday(thread_opened_date, label=TRUE, week_start=1, abbr = FALSE),
         thread_opened_month=str_pad(month(thread_opened_date),2, pad="0"),
         thread_opened_year=year(thread_opened_date), 
         thread_closed_date_time=dmy_hms(thread_closed_time),
         thread_closed_date=date(thread_closed_date_time), 
         thread_closed_dow=lubridate::wday(thread_closed_date, label=TRUE, week_start=1, abbr = FALSE), 
         closing_time=difftime(thread_closed_date_time,thread_opened_date_time,units= 'mins'),
         opened_out_of_core_hours=case_when(thread_opened_dow %in% c('Saturday', 'Sunday') ~ 1,
                                            thread_opened_time>'18:30:00' ~ 1,
                                            thread_opened_hour<8 ~ 1,
                                            as.character(thread_opened_date) %in% c('2019-01-01', '2019-04-19', '2019-04-22', '2019-05-06', 
                                                                                    '2019-05-27', '2019-08-26', '2019-12-25', '2019-12-26',
                                                                                    '2020-01-01') ~ 1,
                                            TRUE ~ 0)) %>% 
  mutate_if(is.character,~na_if(.,'')) %>% 
  mutate(age_groups=case_when(patient_age_groups==0~"0-4",
                              patient_age_groups==5~"5-9",
                              patient_age_groups==10~"10-14",
                              patient_age_groups==15~"15-19",
                              patient_age_groups==20~"20-24",
                              patient_age_groups==25~"25-29",
                              patient_age_groups==30~"30-34",
                              patient_age_groups==35~"35-39",
                              patient_age_groups==40~"40-44",
                              patient_age_groups==45~"45-49",
                              patient_age_groups==50~"50-54",
                              patient_age_groups==55~"55-59",
                              patient_age_groups==60~"60-64",
                              patient_age_groups==65~"65-69",
                              patient_age_groups==70~"70-74",
                              patient_age_groups==75~"75-79",
                              patient_age_groups==80~"80-84",
                              patient_age_groups==85~"85-89",
                              patient_age_groups==90~"90-94",
                              patient_age_groups==95~"95+"), 
         age_groups=factor(age_groups),
         age_groups=fct_relevel(age_groups, "5-9", after=1),
         sex=fct_recode(gender_from_data, Women= 'f', Men='m', Unknown='u', Unknown='unknown')) %>% 
  mutate_at(vars(requested_contact_method_f, resolved_by_requested_staff_member_v3, demand_source,closure_method_f), ~as.factor(str_to_sentence(.))) %>%
  group_by(practice_code) %>% 
  mutate(start=min(thread_opened_date), last=max(thread_opened_date)) %>% 
  mutate(england=!str_detect(practice_code,'^W|^S')) %>% 
  ungroup() %>% 
  dplyr::select(c(-all_thread_opened_time, -thread_opened_date_time, -thread_closed_date_time)) %>%
  rename(gender = gender_from_data) %>%
  as.data.frame()


f1 <- feedback_2020m2 %>% 
  janitor::clean_names() %>%
  mutate(date_time=dmy_hms(time),
         date=date(date_time),
         week=week(date),
         dow=lubridate::wday(date, label=TRUE, week_start=1, abbr = FALSE),
         month=str_pad(month(date),2, pad="0"),
         year=year(date)) %>% 
  select(-date_time) %>% 
  as.data.frame()

f456 <- bind_rows(feedback_2020m4, feedback_2020m5, feedback_2020m6) %>% 
  janitor::clean_names() %>%
  mutate(date_time=dmy(day_of_time),
         date=date(date_time),
         dow=lubridate::wday(date, label=TRUE, week_start=1, abbr = FALSE),
         month=str_pad(month(date),2, pad="0"),
         year=year(date)) %>% 
  select(-day_of_time, -date_time) %>% 
  as.data.table()


fb.in <- bind_rows(f1, f456) %>% 
  as.data.frame()

fb = fb.in %>%
         mutate(age_groups=case_when(patient_age<=4~"0-4",
                              patient_age>4 & patient_age<=9 ~"5-9",
                              patient_age>9 & patient_age<=14~"10-14",
                              patient_age>14 & patient_age<=19~"15-19",
                              patient_age>19 & patient_age<=24~"20-24",
                              patient_age>24 & patient_age<=29~"25-29",
                              patient_age>29 & patient_age<=34~"30-34",
                              patient_age>34 & patient_age<=39~"35-39",
                              patient_age>39 & patient_age<=44~"40-44",
                              patient_age>44 & patient_age<=49~"45-49",
                              patient_age>49 & patient_age<=54~"50-54",
                              patient_age>54 & patient_age<=59~"55-59",
                              patient_age>59 & patient_age<=64~"60-64",
                              patient_age>64 & patient_age<=69~"65-69",
                              patient_age>69 & patient_age<=74~"70-74",
                              patient_age>74 & patient_age<=79~"75-79",
                              patient_age>79 & patient_age<=84~"80-84",
                              patient_age>84 & patient_age<=89~"85-89",
                              patient_age>89 & patient_age<=94~"90-94",
                              patient_age>94 ~"95+"),
         age_groups=factor(age_groups),
         age_groups=fct_relevel(age_groups, "5-9", after=1),
         sex=fct_recode(gender, Women= 'f', Men='m', Unknown='u', Unknown=''),
         #friends_family_test=ifelse(fft_response=="", "Unknown", fft_response)) %>%
         fft_cat = case_when(fft_response %in% c("Extremely Likely", "Very good")~5,
                             fft_response %in% c("Likely", "Good")~4,
                             fft_response %in% c("Neither likely nor unlikely", "Neither good nor poor")~3,
                             fft_response %in% c("Unlikely", "Poor")~2,
                             fft_response %in% c("Extremely unlikely", "Very poor")~1,
                             fft_response %in% c("Don't know")~0)) %>%
  group_by(practice_code) %>%
  mutate(england=!str_detect(practice_code,'^W|^S|^Y|^X')) %>%
  ungroup() %>%
  dplyr::select(-time) %>%
  as.data.frame()


dub_threads <- df %>% 
  dplyr::group_by(thread_id) %>% 
  summarise(threads_per_id=n()) %>% 
  as.data.frame()

dub_threads %>%  dplyr::filter(threads_per_id > 1) %>% n_distinct()

df_no_dub <- df %>% distinct(thread_id, .keep_all=TRUE)
fb_no_dub <- fb %>% distinct(unique_id, .keep_all=TRUE)

feedback_england = fb_no_dub %>% 
  filter(england==TRUE) %>%
  filter(!practice_code %in% c("X99998", "Y00006", "Y00011")) %>% # Excluded non-vallid GP practices
  as.data.frame()

df_england <- df_no_dub %>% 
  filter(england==TRUE) %>% 
  filter(!practice_code %in% c("X99998", "Y00006", "Y00011")) %>% # Excluded non-vallid GP practices
  as.data.frame()

df_england_small <- df_england %>%
  filter(N>1000) %>% 
  group_by(practice_code) %>% 
  filter(start<'2019-02-01' & last>'2020-01-01') %>% 
  ungroup() %>% 
  as.data.frame()

# Save clean datasets ----

write_csv(gp_ref_clean,  here::here("Data", "clean data", "gp_ref_clean.csv"))

saveRDS(df, here::here('Data', 'clean data', 'up to july2020', version, 'all_request.rds'))
saveRDS(df_england, here::here('Data', 'clean data', 'up to july2020', version, 'all_request_england.rds'))
saveRDS(df_england_small, here::here('Data', 'clean data', 'up to july2020', version, 'small_request_england.rds'))
saveRDS(feedback_england, here::here('Data', 'clean data', 'up to july2020', version, 'all_feedback_england.rds'))

