# ============================================== #
#                                                #
#     DIGITAL FIRST PRIMARY CARE STUDY           #
#                                                #
#        AskmyGP Phase I Analysis                #
# ---------------------------------------------- #
#                                                #  
# (4_descriptive_analysis_tables_high_volume.r): #
#                                                #
# (1)  Explore AskmyGP data                      #
# (2)  Create summary tables                     #
#                                                #
# ---------------------------------------------- #
#                                                #
# Authors:                                       #
# Geraldine Clarke                               #
# Paris Pariza                                   #
#                                                #
# ============================================== #


# Load preamble ----

source(here::here("R", "codes for GitHub", paste0("0_preamble.R")))


# Read in data ----

df1 <- readRDS(here::here('Data', 'clean data', 'up to july2020', version,  'amg_tidy_df.rds'))

gp_ref <- read_csv(here::here('Data', 'clean data', 'up to july2020', version,  'amg_tidy_gp_ref.csv'))
amg_list <- read.csv(here::here("Data", "clean data", "up to july2020", version, "amg_list.csv"))
amg_summary_data= read.csv(here::here("Data", "clean data", "up to july2020", version, "amg_summary_data.csv"))

gp_ages = sort(names(gp_ref)[grep("age", names(gp_ref))])

gp_ref_size = gp_ref %>%
  dplyr::select(practice_code, year_month, gp_size, !!gp_ages) %>%
  mutate_at(vars(gp_ages), funs(round((.)*gp_size))) %>%
  as.data.frame()

gp_ref_size_long = gp_ref_size %>%
  gather(age, gp_size_age, c(-practice_code, -year_month, -gp_size)) %>%
  as.data.frame()

gp_ref_gender = gp_ref %>%
  dplyr::select(practice_code, year_month, pcntmale, gp_size) %>%
  mutate_at(vars(pcntmale), funs(m=round((.)*gp_size))) %>%
  mutate(f = gp_size - m) %>%
  dplyr::select(-pcntmale) %>%
  as.data.frame()

gp_ref_gender_long = gp_ref_gender %>%
  gather(gender, gp_size_gender, c(-practice_code, -year_month, -gp_size)) %>%
  as.data.frame()

gp_ages_gender = c(paste0(gp_ages, "_m"), paste0(gp_ages, "_f"))

gp_ref_gender_age = gp_ref %>%
  mutate_at(vars(gp_ages), funs(m = (.)*pcntmale, f = (.)*(1-pcntmale))) %>%
  dplyr::select(practice_code, year_month, gp_ages_gender, gp_size) %>%
  mutate_at(vars(gp_ages_gender), funs(round((.)*gp_size))) %>%
  as.data.frame() 

gp_ref_gender_age_long = gp_ref_gender_age %>%
  gather(age_gender, gp_size_age_gender, c(-practice_code, -year_month, -gp_size)) %>%
  as.data.frame()


# Descriptive information on AMG GP practices -----

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)
j=1
uptakes = list(c(2), c(1,2))[1]

df1a = df1 %>%
  filter(adopter %in% list_starts[[j]]) %>%
  as.data.frame()

s00 = df1a %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot) %>%
  group_by(practice_code, year_month) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref, by=c("practice_code"="practice_code", 
                         "year_month_match" = "year_month")) %>%
  as.data.frame()

s00a = s00 %>%
  group_by(practice_code) %>% 
  summarise_at( vars(gp_size, pcntmale, white, age65_74, age75over, ru11, gpfte), funs(mean) ) %>% 
  as.data.frame()

s1=apply(s00a[,-1], 2, mean)

s00b = s00 %>% 
  group_by(practice_code) %>% 
  summarise_at(vars(imd_quintile), funs(mean)) %>% 
  group_by(imd_quintile) %>% 
  tally() %>% 
  as.data.frame()


# Crude rates for key variables ----

#1. Crude Rates 2019 vs. COVID PERIOD

uptakes_list = list(c(2), c(1,2))[1]
ct=1

for(uptakes in uptakes_list){
  
#######################
###a. All
#######################
closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)

##By all
s21 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot
  ) %>%
  group_by(practice_code, year_month, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, registration) %>% 
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(var = "All",
         value="All") %>%
  dplyr::select("var", "value", everything()) %>%
  as.data.frame()

#######################
###b. CLOSURE
#######################

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)
s22 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot
          ) %>%
  group_by(practice_code, year_month, closure_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, closure_method_f, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(var = "Closure") %>%
  rename(value=closure_method_f) %>%
  dplyr::select("var", "value", everything()) %>%
  as.data.frame()

#######################
##c. AGE
#######################

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)
s23 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         !is.na(age_cat)
         
  ) %>%
  group_by(practice_code, year_month, age_cat, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size_long, by=c("practice_code"="practice_code", 
                                   "year_month_match" = "year_month",
                                   "age_cat" = "age")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size_age) %>%
  group_by(year, age_cat, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size_age))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(var = "Age") %>%
  rename(value=age_cat) %>%
  dplyr::select("var", "value", everything()) %>%
  as.data.frame()

#######################
##d. IMD, Eth and rurality
#######################

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)
group_vars = c("imd_diff", "white_diff", "ru11", 
               "attach_true", "repeat_user", 
               "requested_contact_method_f",
               "resolved_by_requested_staff_member_v3")
for(group_var in group_vars){
s24 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         !is.na(get(group_var))
  ) %>%
  group_by(practice_code, year_month, group_var=get(group_var), registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, group_var, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(var = !!group_var) %>%
  rename(value=group_var) %>%
  dplyr::select("var", "value", everything()) %>%
  as.data.frame()
  assign(paste0("s24.", group_var), s24)
}

#######################
##e. IMD and closure
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)
s25 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         !is.na(imd_diff)
         
  ) %>%
  group_by(practice_code, year_month, imd_diff, closure_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, imd_diff, closure_method_f, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  unite("value", c("imd_diff", "closure_method_f")) %>%
  mutate(var = "IMDclosure") %>%
  dplyr::select("var", everything()) %>%
  as.data.frame()

#######################
##e. IMD and request
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)
s26 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         requested_contact_method_f %in% closure_types_to_plot,
         !is.na(imd_diff)
  ) %>%
  group_by(practice_code, year_month, imd_diff,  requested_contact_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, imd_diff,  requested_contact_method_f, registration) %>%

  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  unite("value", c("imd_diff", "requested_contact_method_f")) %>%
  mutate(var = "IMDrequest") %>%
  dplyr::select("var", everything()) %>%
  as.data.frame()

#######################
##f. GENDER
#######################

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)
s27 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         gender %in% c("f", "m"),
  ) %>%
  group_by(practice_code, year_month, gender, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_gender_long, by=c("practice_code"="practice_code", 
                                   "year_month_match" = "year_month",
                                   "gender" = "gender")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size_gender) %>%
  group_by(year, gender, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size_gender))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(var = "Gender") %>%
  rename(value=gender) %>%
  dplyr::select("var", "value", everything()) %>%
  as.data.frame()

#######################
##g. AGE and GENDER
#######################

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)
s28 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         gender %in% c("f", "m"),
         !is.na(age_cat)
         
  ) %>%
  unite("age_gender", c("age_cat", "gender")) %>%
  group_by(practice_code, year_month, age_gender, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_gender_age_long, by=c("practice_code"="practice_code", 
                                     "year_month_match" = "year_month",
                                     "age_gender" = "age_gender")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size_age_gender) %>%
  group_by(year, age_gender, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size_age_gender))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(var = "age_gender") %>%
  rename(value=age_gender) %>%
  dplyr::select("var", "value", everything()) %>%
  as.data.frame()

#######################
##h. ru11 and closure
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)
s29 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         !is.na(ru11)
         
  ) %>%
  group_by(practice_code, year_month, ru11, closure_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, ru11, closure_method_f, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  unite("value", c("ru11", "closure_method_f")) %>%
  mutate(var = "ru11closure") %>%
  dplyr::select("var", everything()) %>%
  as.data.frame()

#######################
##i. ru11 and request
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)
s30 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         requested_contact_method_f %in% closure_types_to_plot,
         !is.na(ru11)
  ) %>%
  group_by(practice_code, year_month, ru11,  requested_contact_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, ru11,  requested_contact_method_f, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  unite("value", c("ru11", "requested_contact_method_f")) %>%
  mutate(var = "ru11request") %>%
  dplyr::select("var", everything()) %>%
  as.data.frame()

#######################
##j.Ethnicity and closure
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)
s31 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         !is.na(white_diff)
         
  ) %>%
  group_by(practice_code, year_month, white_diff, closure_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, white_diff, closure_method_f, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  unite("value", c("white_diff", "closure_method_f")) %>%
  mutate(var = "Ethclosure") %>%
  dplyr::select("var", everything()) %>%
  as.data.frame()

#######################
##i. white_diff and request
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)
s32 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         requested_contact_method_f %in% closure_types_to_plot,
         !is.na(white_diff)
  ) %>%
  group_by(practice_code, year_month, white_diff,  requested_contact_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size) %>%
  group_by(year, white_diff,  requested_contact_method_f, registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  unite("value", c("white_diff", "requested_contact_method_f")) %>%
  mutate(var = "Ethrequest") %>%
  dplyr::select("var", everything()) %>%
  as.data.frame()

##Create final output
s2_out = rbind(s21, s22, s23, s27,
               s24.imd_diff, 
               s24.white_diff, 
               s24.ru11, 
               s24.requested_contact_method_f, 
               s25,
               s26, 
               s28, s29,s30,s31, s32) %>%
    dplyr::select("var", "value", "Y2019_early", "Y2020_early", everything()) %>%
  mutate(change_early = 
           round(100*(Y2020_early - Y2019_early)/Y2019_early,2),
         change_covid = 
           round(100*(Y2020_covid - Y2019_early)/Y2019_early,2)
           ) %>%
  as.data.frame()

 assign(paste0("sum_", c("max_uptake_25", "max_uptake_15")[ct]), s2_out)
  ct=ct+1 
}  

write.csv(sum_max_uptake_25, here::here("Plots", "Useful plots all data", "up to july2020", version, "misc", 
                  "crude_rates_max_uptake_25.csv"))



##2. Additional crude rates

uptakes_list = list(c(2), c(1,2))[1]
ct=1

for(uptakes in uptakes_list){
  
#######################
##a. AGE and request
#######################
  
closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)

s30 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         requested_contact_method_f %in% closure_types_to_plot,
         !is.na(age_cat)
         
  ) %>%
  group_by(practice_code, year_month, age_cat, requested_contact_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size_long, by=c("practice_code"="practice_code", 
                                   "year_month_match" = "year_month",
                                   "age_cat" = "age")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size_age) %>%
  group_by(year, age_cat,requested_contact_method_f,  registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size_age))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
 mutate(group1=age_cat,
         group2=requested_contact_method_f) %>%
  unite("value", c("age_cat", "requested_contact_method_f")) %>%
    mutate(var = "Agerequest") %>%
    dplyr::select("var", everything()) %>%
    as.data.frame()  
  
s30.a = s30 %>%
  group_by(group1) %>%
  mutate_at(vars("Y2019_early", "Y2020_early", "Y2020_covid"), funs(pct = 100*(.)/sum(.))) %>%
  as.data.frame()

#######################
##b. AGE and closure
#######################

closure_included = "closure_types_active_volume"
closure_types_to_plot = get(closure_included)

s31 = df1 %>%
  filter(uptake_cat_max %in% uptakes,
         closure_method_f %in% closure_types_to_plot,
         !is.na(age_cat)
         
  ) %>%
  group_by(practice_code, year_month, age_cat, closure_method_f, registration) %>%
  tally() %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref_size_long, by=c("practice_code"="practice_code", 
                                   "year_month_match" = "year_month",
                                   "age_cat" = "age")) %>%
  mutate(year = ifelse(grepl("2019", year_month), "Y2019", 
                       ifelse(year_month%in% c("2020_03", "2020_04", "2020_05", "2020_06", "2020_07"), "Y2020", NA))) %>%
  filter(!is.na(year)) %>%
  mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
         appt_freq_month = n_fix/gp_size_age) %>%
  group_by(year, age_cat,closure_method_f,  registration) %>%
  summarise_at(vars(n_fix), funs(appt_freq_yr = 12*sum(n_fix)/sum(gp_size_age))) %>%
  unite("group", c("year", "registration")) %>%
  spread(group, appt_freq_yr) %>%
  mutate(group1=age_cat,
         group2=closure_method_f) %>%
  unite("value", c("age_cat", "closure_method_f")) %>%
  mutate(var = "Ageclosure") %>%
  dplyr::select("var", everything()) %>%
  dplyr::select("var", "value", "group1", "group2", "Y2019_early", "Y2020_early", everything()) %>%
  as.data.frame()  

s31.a = s31 %>%
  group_by(group1) %>%
  mutate_at(vars("Y2019_early", "Y2020_early", "Y2020_covid"), funs(pct = 100*(.)/sum(.))) %>%
  as.data.frame()

s3_out = rbind(s30.a, s31.a) 

assign(paste0("sum_", c("max_uptake_25", "max_uptake_15")[ct]), s3_out)
ct=ct+1 
}  

write.csv(sum_max_uptake_25, here::here("Plots", "Useful plots all data", "up to july2020", version, "misc", 
                               "additional_crude_rates_max_uptake_25.csv"))

