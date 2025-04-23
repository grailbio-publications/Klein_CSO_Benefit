#use previously generated sensitivity table
sens_table<-read_tsv(sprintf("generated_data/%s_ccga3_iso_sens.tsv",input_date_code))

#find all cancer types and stages that need sensitivity
#note using same sensitivity for NET subset of cancers 
seer_draw_basic<-seer_raw_all %>% select(SEER_Draw,Stage) %>% distinct()

#make sure every seer entry has a corresponding sensitivity
final_sens_table<-seer_draw_basic %>% 
  left_join(sens_table %>% 
              rename(SEER_Draw=Cancer)) %>%
  select(SEER_Draw,Stage,c,n,sens) %>%
  mutate(sens=replace_na(sens,replace=0.0),
         n=replace_na(n,replace=0),
         c=replace_na(c,replace=0))