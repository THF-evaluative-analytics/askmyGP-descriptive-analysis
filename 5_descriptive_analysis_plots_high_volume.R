# ============================================== #
#                                                #
#     DIGITAL FIRST PRIMARY CARE STUDY           #  
#                                                #
#        AskmyGP Phase I Analysis                #
# ---------------------------------------------- #
#                                                #
# (5_descriptive_analysis_plots_high_volume.r):  #
#                                                # 
# (1)  Explore AskmyGP data                      #
# (2)  Create summary plots                      #
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

gp_ages = sort(names(gp_ref)[grep("age", names(gp_ref))])

gp_ref_size = gp_ref %>%
  dplyr::select(practice_code, year_month, gp_size, !!gp_ages) %>%
  mutate_at(vars(gp_ages), funs(round((.)*gp_size))) %>%
  as.data.frame()

gp_ref_size_long = gp_ref_size %>%
  gather(age, gp_size_age, c(-practice_code, -year_month, -gp_size)) %>%
  as.data.frame()


# Create useful graphs for long read ----



# 1. Consultation rate by  month

uptakes_list = list(c(2), c(1,2)) 
ct=1

closure_included = "closure_types_ok"
closure_types_to_plot = get(closure_included)

for(uptakes in uptakes_list){

  for(k in c(1)){

      filename = here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                            paste0("cons_rates_per_month_", "uptake_max_", c(25,15)[k], ".pdf"))
    
    pdf(
      file = filename,
      width = 16,
      height = 10
    )
    
    par(mfrow=c(1,1))
    
    for(j in c(1:2)){
      
      df1a = df1 %>%
        filter(adopter %in% list_starts[[j]]) %>%
        as.data.frame()

        s23a.1 = df1a %>%
          filter(year_month %in% study_year_month_plot) %>%
          filter(uptake_cat_max %in% uptakes_list[[k]],
                 closure_method_f %in% closure_types_to_plot
          ) %>%
          group_by(practice_code, year_month) %>%
          tally() %>%
          mutate(year_month_match = ifelse(year_month %in% 
                                             c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
          left_join(gp_ref_size, by=c("practice_code"="practice_code", 
                                      "year_month_match" = "year_month")) %>%
          mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
                 appt_freq_month = n_fix/gp_size) %>%
          group_by(year_month) %>%
          summarise_at(vars(n_fix, gp_size), list(sum, length)) %>%
          mutate(appt_freq = 12*n_fix_fn1/gp_size_fn1) %>%
          mutate(time = match(year_month, study_year_month_plot)) %>%
          dplyr::select(-n_fix_fn2) %>%
          as.data.frame()
      
    write.csv(s23a.1, here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                                     paste0("cons_rates_per_month_", names(list_starts)[j], "_uptake_max_", c(25,15)[k], ".csv")))
    
      p1=s23a.1
      plot(p1$time, p1$appt_freq, type="n", axes=F, xlab="", ylab="rate per patient person-year", font=2, ylim=c(0, 8),
           main=paste("Consultation rates"))
      
      lines(p1$time, p1$appt_freq, col=1, lty=1, lwd=3, type="b")
      
      axis(side=1, at=c(1:length(study_year_month_plot)), labels=study_year_month_plot, font=2)
      axis(side=2, font=2)
      
    }
    
    dev.off()
  }
}


# 2. Consultation rate by week

uptakes_list = list(c(2), c(1,2)) 
ct=1
  
  for(k in c(1)){
     
      filename = here::here("Plots", "Useful plots all data", "up to july2020", version, "misc",
                            paste0("cons_rates_per_week_", "uptake_max_", c(25,15)[k], ".pdf"))
    pdf(
      file = filename,
      width = 16,
      height = 10
    )
    
    for(j in c(1:2)){
      
      df1a = df1 %>%
        filter(adopter %in% list_starts[[j]]) %>%
        as.data.frame()
      
      closure_included = "closure_types_ok"
      closure_types_to_plot = get(closure_included)
      
      s23c.0 = df1a %>%
        mutate(thread_opened_year=year(thread_opened_date), 
               thread_opened_week_pad=str_pad(thread_opened_week, 2, pad="0")) %>%
        as.data.frame()

      s23c.1 = s23c.0 %>%
          mutate(year = thread_opened_year) %>%
          unite("year_week", c(thread_opened_year, thread_opened_week_pad)) %>%
          filter(year_month %in% study_year_month_plot) %>%
          filter(uptake_cat_max %in% uptakes_list[[k]],
                 closure_method_f %in% closure_types_to_plot
          ) %>%
          as.data.frame()

      year_weeks = sort(unique(s23c.1$year_week))
      
      s23c.2 = s23c.1 %>%
        group_by(practice_code, year_week, year, thread_opened_week) %>%
        tally() %>%
        mutate(week_month_proxy = str_pad(ceiling(thread_opened_week/4.5), 2, pad="0")) %>%
        unite("year_month_proxy", c(year, week_month_proxy)) %>%
        mutate(year_month_match = ifelse(year_month_proxy %in% 
                                           c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month_proxy))) %>%
        left_join(gp_ref_size, by=c("practice_code"="practice_code", 
                                    "year_month_match" = "year_month")) %>%
        group_by(year_week) %>%
 
        summarise_at(vars(n), funs(appt_freq = 52*sum(n)/sum(gp_size),
                                   n = sum(n), gp_size=sum(gp_size))) %>%
        mutate(time = match(year_week, year_weeks)) %>%
        as.data.frame()
      
      if(j==1){
        year_weeks_pretty = c("24/12/18", "21/01/19", "18/02/19", "18/03/19", "15/04/19", "13/05/19", 
                              "10/06/19", "08/07/19", "05/08/19", "02/09/19", "30/09/19",
                              "28/10/19", "25/11/19", "23/12/19", "20/01/20", "17/02/20",
                              "16/03/20", "13/04/20", "11/05/20", "08/06/20", "06/07/20")
        year_weeks_pretty_at = seq(0,80,b=4)
        
        x1 = match(s23c.2$time, year_weeks_pretty_at)
        
        s23c.2$year_week_pretty = year_weeks_pretty[x1]
        s23c.2$year_week_pretty[match(year_weeks_pretty_at[-1], 
                                      s23c.2$time)] = year_weeks_pretty[-1]
      }
      
      write.csv(s23c.2, here::here("Plots", "Useful plots all data", "up to july2020", version, "misc",
                                     paste0("cons_rates_per_week_", names(list_starts)[j], "_uptake_max_", c(25,15)[k], ".csv")))
      
      s23c.3 = s23c.2 %>%
        filter(!year_week %in% c("2019_53", "2020_23")) %>%
        filter(year_week > c("2019_23")) %>%
        as.data.frame()
      
      cols=brewer.pal(6, "Spectral")
      
      plot(s23c.3$time, s23c.3$appt_freq, type="n", axes=F, xlab="", ylab="rate per patient person-year", font=2, ylim=c(0, 11),
           main=paste("Consultation rates"))
      
      lines(s23c.3$time, s23c.3$appt_freq, col=cols[1], lty=1, lwd=3, type="b")
      axis(side=1, at=year_weeks_pretty_at, labels=year_weeks_pretty, font=2, las=2)
      axis(side=2, font=2)
    }
    
    dev.off()
  }


# 3. Consultation rate by age group by month

uptakes_list = list(c(2), c(1,2)) 
ct=1

  for(k in c(1)){
  
  filename = here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                            paste0("cons_age_rates_per_month_", "uptake_max_", c(25,15)[k], ".pdf"))

    pdf(
      file = filename,
      width = 16,
      height = 10
    )
    
    par(mfrow=c(1,1))
    
    for(j in 1:2) {
      
      df1a = df1 %>%
        filter(adopter %in% list_starts[[j]]) %>%
        as.data.frame()
      
      
      closure_included = "closure_types_ok"
      closure_types_to_plot = get(closure_included)
       
        s23b.1 = df1a %>%
          filter(year_month %in% study_year_month_plot) %>%
          filter(uptake_cat_max %in% uptakes_list[[k]],
                 closure_method_f %in% closure_types_to_plot,
                 !is.na(age_cat) ) %>%
          group_by(practice_code, year_month, age_cat) %>%
          tally() %>%
          mutate(year_month_match = ifelse(year_month %in% 
                                             c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
          left_join(gp_ref_size_long, by=c("practice_code"="practice_code", 
                                           "year_month_match" = "year_month",
                                           "age_cat" = "age")) %>%
          mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
                 appt_freq_month = n_fix/gp_size_age) %>%
          group_by(year_month, age_cat) %>%
          summarise_at(vars(n_fix), funs(appt_freq = 12*sum(n_fix)/sum(gp_size_age), 
                                         n=sum(n_fix), gp_size=sum(gp_size_age))) %>%
          mutate(time = match(year_month, study_year_month_plot)) %>%
          as.data.frame()

        write.csv(s23b.1, here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                                     paste0("cons_age_rates_per_month_", names(list_starts)[j], "_uptake_max_", c(25,15)[k], ".csv")))
  
      if(j==1){
        s23b.1d.3 = s23b.1 %>%
          filter(year_month > c("2019_05")) %>%
          as.data.frame() 
      }
       
      ## Line charts   
        
      plot(s23b.1$time, s23b.1$appt_freq, type="n", axes=F, xlab="", ylab="rate per patient person-year", font=2, ylim=c(0, 8),
           main=paste("Consultation rates"))
      types=unique(s23b.1$age_cat)
      n=length(types)
      
      for(i in 1:n){
        s23.i = s23b.1[which(s23b.1$age_cat == types[i]),]
        lines(s23.i$time, s23.i$appt_freq, col=i, lty=1, lwd=3, type="b")
      }
      axis(side=1, at=c(1:length(study_year_month_plot)), labels=study_year_month_plot, font=2)
      axis(side=2, font=2)
      legend("topleft", legend = types, col=c(1:n), lwd=rep(3,n))
      
    }
    
    dev.off()
  }
       
        ## Bar charts

filename = here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                      paste0("cons_age_diff_non_covid_uptake_max_25.pdf"))

pdf(
  file = filename,
  width = 16,
  height = 10
)

s23b.1 <- read.csv(here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                             paste0("cons_age_rates_per_month_non_covid_uptake_max_25.csv")))

b = s23b.1 %>%
  filter(year_month %in% c(  "2019_03", "2019_04", "2019_05", "2019_06", 
                             "2020_03", "2020_04", "2020_05", "2020_06")) %>%
  mutate(month = substring(year_month, 6,7)) %>%
  mutate(year = substring(year_month, 1,4)) %>%
  as.data.frame()

b.wide = b %>%
  dplyr::select(year_month, age_cat, appt_freq) %>%
  spread(year_month, appt_freq) %>%
  mutate(age_cat = case_when(age_cat=="age00_04"~"< 4 years",
                             age_cat=="age05_14"~"5-14 years",
                             age_cat=="age15_24"~"15-24 years",
                             age_cat=="age25_64"~"25-64 years",
                             age_cat=="age65_74"~"65-74 years",
                             age_cat=="age75over"~">75 years")) %>%
  as.data.frame()

age.chg = 100*(b.wide[,6:9] - b.wide[,2:5])/b.wide[,2:5]
names(age.chg) = paste0(c("March", "April", "May", "June"), "_20-19chg")
age.out = cbind(b.wide, age.chg)

b1=age.out[,c(1,grep("_20-19chg", names(age.out)))]
names(b1) = c("age", "March", "April", "May", "June")
names.tmp = names(b1[,-1])

barplot(as.matrix(b1[,-1]), 
        legend.text = b1$age, 
        names.arg = names.tmp,
        beside=T,
        args.legend = list(x= "topleft", 
                           xpd=T, cex=0.8,
                           ncol=6,
                           bty="n"),
        col=brewer.pal(6, "Set1"), 
        main=paste0("Consultation by Age"), 
        ylab="Relative percentage change",
        cex.names=1,
        font=2
)

write.csv(age.out, here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                                    paste0("cons_age_diff_non_covid_uptake_max_25.csv")))

dev.off()


# 4. Consultation rate by consultation type (closure method) by month

uptakes_list = list(c(2), c(1,2)) 
ct=1

  for(k in c(1)){

      filename = here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                            paste0("cons_closure_rates_per_month_", "uptake_max_", c(25,15)[k], ".pdf"))
    
    pdf(
      file = filename,
      width = 16,
      height = 10
    )
    
    par(mfrow=c(1,1))
    
    for(j in 1:2) {
      df1a = df1 %>%
        filter(adopter %in% list_starts[[j]]) %>%
        as.data.frame()
      
      
      closure_included = "closure_types_ok_known"
      closure_types_to_plot = get(closure_included)
      
        s23c.1 = df1a %>%
          filter(year_month %in% study_year_month_plot) %>%
          filter(uptake_cat_max %in% uptakes_list[[k]],
                 closure_method_f %in% closure_types_to_plot
          ) %>%
          group_by(practice_code, year_month, closure_method_f) %>%
          tally() %>%
          mutate(year_month_match = ifelse(year_month %in% 
                                             c("2020_04", "2020_05", "2020_06", "2020_07"), "2020_03", paste(year_month))) %>%
          left_join(gp_ref_size, by=c("practice_code"="practice_code", 
                                      "year_month_match" = "year_month")) %>%
          mutate(n_fix = ifelse(year_month=="2020_07", n*31/6, n), 
                 appt_freq_month = n_fix/gp_size) %>%
          group_by(year_month, closure_method_f) %>%
          summarise_at(vars(n_fix), funs( n=sum(n_fix), gp_size=sum(gp_size))) %>%
          group_by(year_month) %>%
          mutate(gp_size_max = max(gp_size)) %>%
          ungroup() %>%
          mutate(appt_freq = 12*n/gp_size_max)%>%
          mutate(time = match(year_month, study_year_month_plot)) %>%
          as.data.frame()
  
        write.csv(s23c.1, here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                                     paste0("cons_closure_rates_per_month_", names(list_starts)[j], "_uptake_max_", c(25,15)[k], ".csv")))
      
      if(j==1){
        s23c.2 = s23c.1 %>%
          filter(year_month > c("2019_05")) %>%
          as.data.frame() 
      } 
        else {
        s23c.2 = s23c.1
      }
      
      ## Line charts
        
      plot(s23c.2$time, s23c.2$appt_freq, type="n", axes=F, xlab="", ylab="rate per patient person-year", 
           font=2,
           main=paste("Consultation rates"))
      types=unique(s23c.2$closure_method_f)
      n=length(types)
      for(i in 1:n){
        s23.i = s23c.2[which(s23c.2$closure_method_f == types[i]),]
        lines(s23.i$time, s23.i$appt_freq, col=i, lty=1, lwd=3, type="b")
      }
      axis(side=1, at=c(1:length(study_year_month_plot)), labels=study_year_month_plot, font=2)
      axis(side=2, font=2)
      legend("topleft", legend = types, col=c(1:n), lwd=rep(3,n))
    }
    
    dev.off()
  }


     ## Bar charts 

filename = here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                      paste0("cons_closure_diff_non_covid_uptake_max_25.pdf"))

pdf(
  file = filename,
  width = 16,
  height = 10
)

s23c.1 <- read.csv(here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                              paste0("cons_closure_rates_per_month_non_covid_uptake_max_25.csv")))

par(mfrow=c(2,1))
par(oma=c(2,4,2,2))
i=2

c = s23c.1 %>%
  filter(year_month %in% c(   "2019_03", "2019_04", "2019_05", "2019_06", 
                              "2020_03", "2020_04", "2020_05", "2020_06")) %>%
  mutate(year = substring(year_month, 1,4)) %>%
  group_by(year_month) %>%
  mutate(appt_prop = 100*appt_freq/sum(appt_freq)) %>%
  ungroup() %>%
  as.data.frame()

c.wide = c %>%
  dplyr::select(year_month, closure_method_f, appt_prop) %>%
  spread(year_month, appt_prop) %>%
  as.data.frame()

closure.chg = c.wide[,grep("2020", names(c.wide))] - c.wide[,grep("2019", names(c.wide))]
names(closure.chg) = paste0(c("March", "April", "May", "June"), "_20-19chg")

closure.out = cbind(c.wide, closure.chg)

c1=closure.out[,c(1,grep("_20-19chg", names(closure.out)))]
names(c1) = c("Mode", "March", "April", "May", "June")
names.tmp = names(c1[,-1])

barplot(as.matrix(c1[,-1]), 
        legend.text = c.wide$closure_method_f, 
        names.arg = names.tmp,
        beside=T,
        args.legend = list(x= "topleft", inset=c(-0.05,-0.08), 
                           xpd=T, cex=0.8,
                           ncol=5,
                           bty="n"),
        col=brewer.pal(5, "Set1"), 
        main=paste0("Consultation type"), 
        ylab="Absolute change in proportion",
        cex.names=1,
        font=2
)

write.csv(closure.out, here::here("Plots", "Useful plots all data", "up to July2020", version, "misc",
                              paste0("cons_closure_diff_non_covid_uptake_max_25.csv")))

dev.off()

