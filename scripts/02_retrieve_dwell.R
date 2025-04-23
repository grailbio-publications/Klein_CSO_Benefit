#dwell times associated with cancers
#this extends the earlier paper so uses the same dwell time scenarios
#we use the two fastest of the old scenarios here.

dwell_all<-read_tsv("data/20210816_aggregate_dwell.tsv")

source("R/slip_rate_from_dwell.R") # get slip rate computation

#screening at multiple potential intervals for PPV thresholds
#also in case "ignore CSO every other year" is a valid strategy
multiple_dwell_slip_rate<-bind_rows(sapply(1:3,function(z){
  exact_slip_rate_from_dwell(dwell_all,screen_interval=z,weibull_shape=1)
},simplify=FALSE)) %>%
  distinct() #handle the repeated MIS problem by ensuring only one copy

dwell_no_rate<-multiple_dwell_slip_rate %>% 
  filter(scenario=="MIS") %>%
  mutate(dwell=0,
         screen_interval=1,
         slip=1.0,
         slip_clinical=1.0,
         scenario="NO")

multiple_dwell_slip_rate <-multiple_dwell_slip_rate %>%
  bind_rows(dwell_no_rate)

#ignore reliability for this purpose

index_scenario<-c("scenario","screen_interval") #components for different scenarios

#simplify for testing - remove scenarios we don't use
#remove different screening intervals
multiple_dwell_slip_rate<-multiple_dwell_slip_rate %>%
  filter(!grepl("new_",scenario)) %>%
  filter(scenario!="old_1",scenario!="old_2") %>%
  mutate(screen_interval=replace_na(screen_interval,replace=0)) %>%
  filter(screen_interval!=3,screen_interval!=2)