#read in css, os, generate ncss for general utility

css_raw_male<-read_excel(path=sprintf("data/%s",seer_cso_file),
                    sheet="CSO benefit CSS male",
                    range="A3:J27362",
                    col_names=c("Age","SEER_Draw","Stage","Months","N","MX","CSS","SE","LB_CSS","UB_CSS"),
                    col_types="text") 

css_raw_female<-read_excel(path=sprintf("data/%s",seer_cso_file),
                         sheet="CSO benefit CSS female",
                         range="A3:J27362",
                         col_names=c("Age","SEER_Draw","Stage","Months","N","MX","CSS","SE","LB_CSS","UB_CSS"),
                         col_types="text")


css_raw_all<-bind_rows(css_raw_male %>% mutate(Sex="Male"),css_raw_female %>% mutate(Sex="Female"))

css_basic<-css_raw_all %>%
  mutate(Age=gsub(" years","",Age)) %>%
  separate(Age,into=c("Lower","Upper"),sep="-") %>%
  mutate(NET=case_when(!grepl("NET",SEER_Draw) ~ "NotExpected",
                       grepl("non-NET",SEER_Draw) ~ "No",
                       TRUE ~ "NET")) %>%
  mutate(SEER_Draw=gsub(" non-NET","",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub(" NET","",SEER_Draw)) %>%
  mutate(Months=gsub(" mo","",Months)) %>%
  type_convert() %>%
  mutate(CSS=parse_double(CSS)) %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,Stage,Months,CSS) %>%
  mutate(CSS_flag=is.na(CSS)) %>%
  group_by(Sex,Lower,Upper,SEER_Draw,NET,Stage) %>%
  fill(CSS) %>%
  ungroup() %>%
  filter(!(Sex=="Male" & SEER_Draw %in% c("Cervix","Uterus","Ovary"))) %>%
  filter(!(Sex=="Female" & SEER_Draw=="Prostate")) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "NotStaged",
                         TRUE ~ Stage)) %>%
  mutate(CSS=replace_na(CSS,replace=0.0)) # no survivors if no data - won't affect anything

#do for OS

os_raw_male<-read_excel(path=sprintf("data/%s",seer_cso_file),
                        sheet="CSO benefit OS male",
                        range="A3:J27362",
                        col_names=c("Age","SEER_Draw","Stage","Months","N","MX","OS","SE","LB_OS","UB_OS"),
                        col_types="text")

os_raw_female<-read_excel(path=sprintf("data/%s",seer_cso_file),
                          sheet="CSO benefit OS female",
                          range="A3:J27362",
                          col_names=c("Age","SEER_Draw","Stage","Months","N","MX","OS","SE","LB_OS","UB_OS"),
                          col_types="text")

os_raw_all<-bind_rows(os_raw_male %>% mutate(Sex="Male"),os_raw_female %>% mutate(Sex="Female"))

os_basic<-os_raw_all %>%
  mutate(Age=gsub(" years","",Age)) %>%
  separate(Age,into=c("Lower","Upper"),sep="-") %>%
  mutate(NET=case_when(!grepl("NET",SEER_Draw) ~ "NotExpected",
                       grepl("non-NET",SEER_Draw) ~ "No",
                       TRUE ~ "NET")) %>%
  mutate(SEER_Draw=gsub(" non-NET","",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub(" NET","",SEER_Draw)) %>%
  mutate(Months=gsub(" mo","",Months)) %>%
  type_convert() %>%
  mutate(OS=parse_double(OS)) %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,Stage,Months,OS) %>%
  mutate(OS_flag=is.na(OS)) %>%
  group_by(Sex,Lower,Upper,SEER_Draw,NET,Stage) %>%
  fill(OS) %>%
  ungroup() %>%
  filter(!(Sex=="Male" & SEER_Draw %in% c("Cervix","Uterus","Ovary"))) %>%
  filter(!(Sex=="Female" & SEER_Draw=="Prostate")) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "NotStaged",
                         TRUE ~ Stage)) %>%
  mutate(OS=replace_na(OS,replace=0.0)) # no survivors if no data - won't affect anything

#very cheap computation of NCS - non cancer risk of dying
#OS = survived both CS and NCS causes i.e. (1-died CS)*(1-died NCS) (independent)
#NCS ~ OS/CSS
#do some cheap isotonics to clean up bad data points
#we only use NCS when trying to isolate hazard ratio effects
total_survival<-css_basic %>%
  left_join(os_basic) %>%
  mutate(NCS=pmin(1,OS/CSS)) %>%
  group_by(Sex,Lower,SEER_Draw,NET,Stage) %>%
  fill(NCS) %>%
  mutate(NCS=cummin(NCS)) %>%
  ungroup()

write_tsv(total_survival,sprintf("generated_data/%s_total_survival.tsv",date_code))


