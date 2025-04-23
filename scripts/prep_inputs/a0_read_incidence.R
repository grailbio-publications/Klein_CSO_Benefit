#read cancer incidence

seer_cso_file<-"CSO benefit incidence, CSS, and OS by site (non-NET, NET) and AJCC6 stage SEER17 2006-2015 20240806.xlsx"

incidence_raw<-read_excel(path=sprintf("data/%s",seer_cso_file),
                          sheet="CSO benefit incidence",
                          range="A2:G3421",
                          col_names=c("Sex","Age","SEER_Draw","Stage","IR","N","Pop"))

#fix up data
#get more decimal places for incidence
#extract NET status
#parse age range
incidence_basic<-incidence_raw %>%
  mutate(Age=gsub(" years","",Age)) %>%
  separate(Age,into=c("Lower","Upper"),sep="-") %>%
  mutate(Rate=N/Pop*1e5) %>%
  mutate(NET=case_when(!grepl("NET",SEER_Draw) ~ "NotExpected",
                       grepl("non-NET",SEER_Draw) ~ "No",
                       TRUE ~ "NET")) %>%
  mutate(SEER_Draw=gsub(" non-NET","",SEER_Draw)) %>%
  mutate(SEER_Draw=gsub(" NET","",SEER_Draw))

write_tsv(incidence_basic,sprintf("generated_data/%s_seer_basic_incidence.tsv",date_code))