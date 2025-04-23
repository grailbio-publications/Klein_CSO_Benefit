
klein_confusion_file<-"20240802 Klein Confusion Raw.xlsx"

tp_cso_raw<-read_excel(path=sprintf("data/%s",klein_confusion_file),
                       sheet="TP",
                       range="A1:C79")

fp_cso_raw<-read_excel(path=sprintf("data/%s",klein_confusion_file),
                       sheet="FP",
                       range="A1:C7")

#confusion matrix needs all entries
#so complete true positive matrix

tp_cso_complete<-tp_cso_raw %>% 
  complete(`Expected CSO`,`Predicted CSO`,fill=list(N=0))


fp_cso_complete<-fp_cso_raw %>%
  bind_rows(tp_cso_complete %>% 
              select(`Predicted CSO`) %>% 
                       distinct() %>% mutate(N=0,`Expected CSO`="None")) %>%
  group_by(`Predicted CSO`) %>%
  summarize(N=sum(N)) %>%
  ungroup() %>%
  mutate(`Expected CSO`="NoneExpected") %>%
  select(`Expected CSO`,`Predicted CSO`,N)

write_tsv(bind_rows(tp=tp_cso_complete,fp=fp_cso_complete,.id="positive_type"),
          sprintf("generated_data/%s_raw_confusion_complete.tsv",date_code))