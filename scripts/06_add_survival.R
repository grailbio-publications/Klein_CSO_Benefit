#in this case just use usual CSS
#including any differences from NET

survival_data<-read_tsv(sprintf("generated_data/%s_total_survival.tsv",input_date_code))

#this could be split into cfdna+ and cfdna- with appropriate hazard ratio
#in sensitivity analysis later
survival_filtered_five<-survival_data %>%
  filter(Months==60) %>%
  mutate(number_stage=match(Stage,c("I","II","III","IV","NotStaged"))) %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,number_stage,CSS,OS,NCS)

#add original/shifted survival
#note using usual model of 'from original diagnosis'
#individuals survive to cancer diagnosis if minimally not overdiagnosed
#lead time can be computed if we care
interception_plus_CSO_survival<-interception_plus_CSO %>%
  left_join(survival_filtered_five %>% rename(clinical=number_stage,original_CSS=CSS, original_OS=OS,original_NCS=NCS)) %>%
  left_join(survival_filtered_five %>% rename(prequel=number_stage,shifted_CSS=CSS, shifted_OS=OS,shifted_NCS=NCS)) %>% 
  mutate(original_CSS=replace_na(original_CSS,replace=1.0),
         original_OS=replace_na(original_OS,replace=1.0),
         original_NCS=replace_na(original_NCS,replace=1.0),
         shifted_CSS=replace_na(shifted_CSS,replace=1.0),
         shifted_OS=replace_na(shifted_OS,replace=1.0),
         shifted_NCS=replace_na(shifted_NCS,replace=1.0)) %>% #mostly non-cancer, who of course survive cancer - ignoring delta for false positives
  mutate(delta_css=pmax(0,shifted_CSS-original_CSS),
         delta_os=pmax(0,shifted_OS-original_OS)) %>%
  mutate(lives_CSS_saved_CSO=caught_CSO*delta_css,
         prevalence_CSS_lives_saved_CSO=prevalence_caught_CSO*delta_css,
         lives_OS_saved_CSO=caught_CSO*delta_os,
         prevalence_OS_lives_saved_CSO=prevalence_caught_CSO*delta_os) 
