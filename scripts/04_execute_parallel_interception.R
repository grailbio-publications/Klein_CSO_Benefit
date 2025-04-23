# generate two-population interception model
# only depends on grouping columns, incidence, sensitivity
# add survival, lead time to tibble later

source("R/compute_stage_shift.R")
#inputs: local_sens_table
#seer_incidence

#local_sens_table<-final_sens_table
#seer_incidence<-seer_raw_all
#local_slip<-multiple_dwell_slip_rate

#all key columns to isolate unique cancer cases
#index_cancer<-c(Sex","Exposure","Lower","Upper","SEER_Draw","NET")
#all key columns to isolate unique scenarios in dwell time
#index_scenario<-c("scenario","screen_interval")

#seer_incidence contains cancer incidence for each cancer and unique situation in index_cancer
#local_sens_table contains sensitivity for each cancer
#local_slip contains slip rate information for each dwell time scenario and screening interval in index_scenario
#index_cancer - cancer + any additional columns providing unique situations
#index_scenario - scenario + screening interval + any additional columns providing unique screening 

local_seer_all<-seer_raw_all 

interception_stage_shift<-compute_parallel_interception_stage_shift(local_seer_all,
                                                                    final_sens_table,
                                                                    multiple_dwell_slip_rate,
                                                                    index_cancer,
                                                                    index_scenario)

#generate fp per scenario,dwell,index (non-seer-draw)
#generate fp to go with everything else
run_parallel_fp_model<-function(seer_incidence,
                                fp_rate, #corresponds to sensitivity
                                local_slip,
                                index_cancer,index_scenario,
                                reliability=1.0,population=100000){
  
  fake_index_cancer<-index_cancer[index_cancer!="SEER_Draw"]
  #generate false positives
  mis_fp<-seer_incidence %>%
    group_by(across(all_of(fake_index_cancer))) %>%
    summarize(caught=fp_rate*population) %>%
    ungroup()
  
  #add all the scenario levels
  #adjust for screening intensity - fewer fp per year when screening less of the population
  screen_fp<-mis_fp %>%
    left_join(local_slip %>% 
                select(all_of(index_scenario))%>% 
                distinct(),by=character()) %>%
    mutate(caught=caught/screen_interval,
           prevalence_caught=caught*screen_interval) 
  
  
  fp_df<-screen_fp %>%
    mutate(SEER_Draw="Non-cancer",clinical=NA,prequel=NA,cfdna_detectable="yes",found_using="cfdna") %>%
    select(all_of(c(index_cancer,index_scenario,
                    "clinical","prequel","cfdna_detectable","found_using",
                    "caught","prevalence_caught")))
  
  fp_df
}

interception_fp<-run_parallel_fp_model(local_seer_all,
                                       0.005, #fp rate corresponds to sensitivity
                                       multiple_dwell_slip_rate,
                                       index_cancer,
                                       index_scenario)

#fix up the no-screening scenario
#fix up the NET for non-cancers to have only one case
#which is "NotExpected"
interception_fp<-interception_fp %>%
  mutate(caught=case_when(scenario=="NO" ~ 0.0,
                          TRUE ~ caught),
         prevalence_caught=case_when(scenario=="NO" ~ 0.0,
                                     TRUE ~ caught)) %>%
  filter(NET=="NotExpected")

interception_stage_shift<-interception_stage_shift %>% 
  bind_rows(interception_fp) %>%
  filter(scenario!="MIS") #take out the theoretical max scenario

