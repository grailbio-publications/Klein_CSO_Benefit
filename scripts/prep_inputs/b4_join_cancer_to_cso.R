#final step
#for each cancer type
#match to the appropriate column

cancer_to_cso_raw<-read_excel(path=sprintf("data/%s",klein_confusion_file),
                       sheet="CSO",
                       range="A1:C39")



tp_cancer_setup<-cancer_to_cso_raw %>%
  left_join(all_tp_matrix,multiple="all")

fp_noncancer_setup<-tribble(~Cancer,~NET,~`Expected CSO`,
                            "Non-cancer","NotExpected","NoneExpected") %>%
  left_join(all_fp_matrix,multiple="all")
  
#final piece of the puzzle
#assign fictitious CSO to anything found by usual care procedures
#this is always only the expected outcome

uc_cancer_setup<-cancer_to_cso_raw %>%  
  cross_join(tribble(~Sex,"Male","Female")) %>%
  mutate(`Predicted CSO` = `Expected CSO`) %>%
  mutate(final_confusion=1.0) 


#assemble the pieces
#if found by cfdna, assign probabilties for each cso given the cancer
#if found by usual care, assign the assumed correct cso 100% of the time as a useful fiction
all_confusion_possible<-tp_cancer_setup %>% 
  mutate(mode_found="cfdna") %>%
  bind_rows(fp_noncancer_setup %>% mutate(mode_found="cfdna")) %>%
  bind_rows(uc_cancer_setup %>% mutate(mode_found="uc"))

write_tsv(all_confusion_possible,sprintf("generated_data/%s_confusion_sex_matched.tsv",date_code))