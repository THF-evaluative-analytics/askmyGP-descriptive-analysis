# =========================================#
#                                          #
#     DIGITAL FIRST PRIMARY CARE STUDY     #
#                                          #
#        AskmyGP Phase I Analysis          #
# -----------------------------------------#
#                                          #
# (2_select_askmyGP_practices.r):          #
#                                          #
# (1)  Calculate uptake                    #
# (2)  Categorise and select askmyGP GPs   #
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


# Read in data ----

##Read askmyGP data
df_england <- readRDS(here::here('Data', 'clean data', 'up to july2020', version, 'all_request_england.rds')) 
  
amg_practices = unique(df_england$practice_code)
length(amg_practices)

##Read GP Reference File
gp_ref <- read_csv(here::here("Data", "clean data", 'up to july2020', version, "gp_ref_clean.csv"))


# Summarise total counts and total unique counts by month and link to gp reference file ----

df_counts_01 <- df_england %>% 
  mutate(start_year = year(ymd(start)),
         start_month = month(ymd(start))) %>%
  mutate(start_month_pad = str_pad(start_month, 2, pad="0")) %>%
  unite("start_year_month", c(start_year, start_month_pad)) %>%
  unite("year_month", thread_opened_year:thread_opened_month) %>%
  group_by(practice_code, start_year_month, year_month) %>%
  tally() %>%
  rename(amg_thread_count = n) %>%
   as.data.frame()

df_counts_02 = df_counts_01 %>%
  mutate(year_month_match = ifelse(year_month %in% 
                                     c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
  left_join(gp_ref, by=c("practice_code"="practice_code", "year_month_match" = "year_month")) %>%
  dplyr::select(practice_code, year_month, start_year_month, gp_size, amg_thread_count) %>%
  as.data.frame()

df_uptake = df_counts_02 %>%
  mutate(uptake = (amg_thread_count/gp_size)*100) %>%
  mutate(uptake_fix = ifelse(year_month=="2020_07", uptake*31/6, uptake)) %>%  # because max(df_england$thread_opened_date)="2020-07-06"
  select(-uptake) %>% 
  rename(uptake=uptake_fix) %>% 
  group_by(practice_code) %>%
  mutate(max_all = max(uptake), mean_all = mean(uptake), min_all = min(uptake)) %>%
  ungroup %>%
  as.data.frame()

df_uptake$ccg = gp_ref[match(df_uptake$practice_code, gp_ref$practice_code),]$ccg_gp


# Define GP practices groupings ----

early_set <- df_uptake %>%
  filter(start_year_month < early_adopters_end) %>%
  distinct(practice_code, .keep_all=TRUE) %>% 
  as.data.frame

covid_set <- df_uptake %>%
  filter(start_year_month > late_adopters_end) %>%
  distinct(practice_code, .keep_all=TRUE) %>%
  as.data.frame()

late_set <- df_uptake %>% 
  filter(!practice_code %in% c(early_set$practice_code, covid_set$practice_code))%>%
  distinct(practice_code, .keep_all=TRUE) %>%
  as.data.frame()

pre_covid_set <- full_join(early_set, late_set)%>%
  as.data.frame()

early_set_list = data.frame(practice_code = early_set$practice_code, ccg = early_set$ccg)
late_set_list = data.frame(practice_code = late_set$practice_code, ccg = late_set$ccg)
covid_set_list = data.frame(practice_code = covid_set$practice_code, ccg = covid_set$ccg)
pre_covid_set_list = data.frame(practice_code = pre_covid_set$practice_code, ccg = pre_covid_set$ccg)


## This code determines high-volume practices to be used for analysis for the below cases:
#                1. max_uptake>15 in any months 
#                2. max_uptake>15 in only those months (total triage months) 
#                3. max_uptake>25 in any months 
#                4. max_uptake>25 in only those months (total triage months)
#
##Making a selection on the threshold of uptake that would give us high volume sets of GP practices

max_uptake_15_all_months_set_list = df_uptake %>%
  filter(max_all > 15) %>%
  distinct(practice_code, .keep_all=TRUE) %>% 
  as.data.frame()

max_uptake_15_only_months_set_list = df_uptake %>%
  filter(uptake > 15) %>%
  select(practice_code, ccg, year_month) %>% 
  as.data.frame()

max_uptake_25_all_months_set_list = df_uptake %>%
  filter(max_all > 25) %>%
  distinct(practice_code, .keep_all=TRUE) %>% 
  as.data.frame()

max_uptake_25_only_months_set_list = df_uptake %>%
  filter(uptake > 25) %>%
  select(practice_code, ccg, year_month) %>% 
  as.data.frame()


# Plot GP practices and their uptake ----

for(i in 1:7){
  
  if(i==1){
    filename = here::here("Plots", "select practices", "up to july2020", version, "GP_uptake_01.pdf"); a=df_uptake
    }
  if(i==2){
    filename = here::here("Plots", "select practices", "up to july2020", version,"GP_max_uptake_15_all_months_set.pdf"); a=max_uptake_15_all_months_set_list
    }
  if(i==3){
    filename = here::here("Plots", "select practices", "up to july2020", version,"GP_max_uptake_25_all_months_set.pdf"); a=max_uptake_25_all_months_set_list
    }
  if(i==4){
    filename = here::here("Plots", "select practices", "up to july2020", version, "GP_early.pdf"); a=early_set
    }
  if(i==5){
    filename = here::here("Plots", "select practices", "up to july2020", version, "GP_late.pdf"); a=late_set
    }
  if(i==6){
    filename = here::here("Plots", "select practices", "up to july2020", version, "GP_covid.pdf"); a=covid_set
    }
  if(i==7){
      filename = here::here("Plots", "select practices", "up to july2020", version, "GP_pre_covid.pdf"); a=pre_covid_set  
    }

i1.month = c(grep("2018_", colnames(a)),grep("2019_", colnames(a)),grep("2020_", colnames(a)))
pdf(
  file = filename,
  width = 16,
  height = 10
)

par(mfrow=c(3,3))
par(oma = c(2,2,2,2))
par(mar = c(8,2,6,2))
for(i in 1:nrow(a)){
  x=barplot(as.numeric(a[i,i1.month]), main=paste(a[i,]$ccg, a[i,]$practice_code), xaxt="n", ylim=c(0,30))
  text(cex=0.9, x=x-.25, y=-10, colnames(a)[i1.month], xpd=T, srt=45)
  abline(v=7, lty=1, lwd=2, col="red")
  abline(v=3, lty=2, lwd=2, col="red")
}
dev.off()
}


# Save files created ----

write.csv(df_uptake, here::here("Data", "clean data", "up to july2020", "amg_summary_data.csv"), row.names=F)

write.csv(max_uptake_15_all_months_set_list, here::here("Data", "clean data", "up to july2020", version, "max_uptake_15_all_months_set_list.csv"), row.names=F)
write.csv(max_uptake_25_all_months_set_list, here::here("Data", "clean data", "up to july2020", version, "max_uptake_25_all_months_set_list.csv"), row.names=F)
write.csv(max_uptake_15_only_months_set_list, here::here("Data", "clean data", "up to july2020", version, "max_uptake_15_only_months_set_list.csv"), row.names=F)
write.csv(max_uptake_25_only_months_set_list, here::here("Data", "clean data", "up to july2020", version, "max_uptake_25_only_months_set_list.csv"), row.names=F)

write.csv(early_set_list, here::here("Data", "clean data", "up to july2020", version, "early_set_list.csv"), row.names=F)
write.csv(late_set_list, here::here("Data", "clean data", "up to july2020", version, "late_set_list.csv"), row.names=F)
write.csv(covid_set_list, here::here("Data", "clean data", "up to july2020", version, "covid_set_list.csv"), row.names=F)
write.csv(pre_covid_set_list, here::here("Data", "clean data", "up to july2020", version, "pre_covid_set_list.csv"), row.names=F)

amg_list = data.frame(practice_code = unique(df_uptake$practice_code),
                      ccg = gp_ref[match(unique(df_uptake$practice_code), gp_ref$practice_code),]$ccg_gp)
write.csv(amg_list, here::here("Data", "clean data", "up to july2020", version, "amg_list.csv"), row.names=F)

