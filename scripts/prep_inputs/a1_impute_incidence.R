not_staged<-c("Lymphoid Leukemia","Myeloid Neoplasm","Plasma Cell Neoplasm","CUP","Brain")

seer_basic_numbers<-incidence_basic

seer_basic_imputable<-seer_basic_numbers %>%
  filter(!SEER_Draw %in% not_staged, SEER_Draw!="[OTHER]")

seer_basic_imputed<-seer_basic_imputable %>% 
  group_by(Sex,SEER_Draw,NET,Lower,Upper) %>%
  mutate(Uflag=1*(Stage=="Unknown/missing"),
         IR=Rate*sum(Rate)/sum(Rate*(1-Uflag))) %>%
  ungroup() %>%
  filter(Stage!="Unknown/missing") %>%
  mutate(IR=case_when(is.nan(IR) ~ 0.0,
                      TRUE ~ IR)) %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,Stage,IR)

seer_basic_not_staged<-seer_basic_numbers %>%
  filter(SEER_Draw %in% not_staged) %>%
  filter(Stage=="Unknown/missing") %>%
  mutate(Stage="NotStaged") %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,Stage,IR=Rate)

seer_basic_other<-seer_basic_numbers %>%
  filter(SEER_Draw=="[OTHER]") %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "NotStaged",
                         TRUE ~ Stage)) %>%
  select(Sex,Lower,Upper,SEER_Draw,NET,Stage,IR=Rate)

seer_imputed_all<-seer_basic_imputed %>%
  bind_rows(seer_basic_not_staged) %>%
  bind_rows(seer_basic_other)