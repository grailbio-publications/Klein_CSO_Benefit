#plot OS vs CSS differences by age range

#tentatively use these two diagrams

ppv_situation_df %>% 
  filter(Exposure=="Any",Lower>=50,scenario=="old_3") %>%
  select(Sex,Lower,Exposure,scenario,predicted_CSO,diagnostic_ls_os_first,diagnostic_ls_css_first) %>%
  pivot_longer(cols=c("diagnostic_ls_os_first","diagnostic_ls_css_first")) %>%
  ggplot(aes(x=factor(Lower),y=value))+
  geom_boxplot(aes(color=name),position=position_dodge())+
  scale_color_manual(name="Mortality Measure",
                     values=(c("red","darkblue")),
                     labels=c("diagnostic_ls_css_first"="CSS","diagnostic_ls_os_first"="OS"))+
  coord_cartesian(y=c(0,240))+
  facet_wrap(~Sex)+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Age Range",y="Diagnostic Tests Per Life Saved (CSO-Directed)")+
  ggtitle("Diagnostic Tests Per Life Saved:\nAny Smoking")


ggsave(filename=sprintf("figs/%s_supplement_CSS_vs_OS_cso_driven.pdf",figure_date_code),
       width=11,
       height=13)

ppv_situation_df %>% 
  filter(Exposure=="Any",Lower>=50,scenario=="old_3") %>%
  select(Sex,Lower,Exposure,scenario,predicted_CSO,diagnostic_ls_os_xct,diagnostic_ls_css_xct) %>%
  pivot_longer(cols=c("diagnostic_ls_os_xct","diagnostic_ls_css_xct")) %>%
  ggplot(aes(x=factor(Lower),y=value))+
  geom_boxplot(aes(color=name),position=position_dodge())+
  scale_color_manual(name="Mortality Measure",
                     values=(c("red","darkblue")),
                     labels=c("diagnostic_ls_css_xct"="CSS","diagnostic_ls_os_xct"="OS"))+
  coord_cartesian(y=c(0,240))+
  facet_wrap(~Sex)+
  theme_bw()+
  theme(text=element_text(size=16),
        legend.position="top")+
  labs(x="Age Range",y="Diagnostic Tests Per Life Saved (post-CSO-Directed)")+
  ggtitle("Diagnostic Tests Per Life Saved:\nAny Smoking")

ggsave(filename=sprintf("figs/%s_supplement_CSS_vs_OS_after_cso_driven.pdf",figure_date_code),
       width=11,
       height=13)

#get a cool summary number that can be used
#again: forbidden by sex, and not-modelable
ppv_summary_os_css_ratio<-ppv_situation_df %>%
  select(Sex,Lower,Exposure,scenario, predicted_CSO, diagnostic_ls_css_first,diagnostic_ls_os_first,
         diagnostic_ls_css_xct,diagnostic_ls_os_xct) %>%
  filter(scenario=="old_3",Exposure=="Any") %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>%
  filter(!is.na(diagnostic_ls_css_first)) %>% 
  filter(!is.infinite(diagnostic_ls_css_first)) %>%
  mutate(first_ratio=diagnostic_ls_os_first/diagnostic_ls_css_first,
         xct_ratio=diagnostic_ls_os_xct/diagnostic_ls_css_xct) %>%
  select(Sex,Lower,first_ratio,xct_ratio) %>%
  group_by(Sex,Lower) %>%
  reframe(across(where(is.numeric),helper_q_summary)) %>%
  ungroup() %>%
  unnest(cols=c(first_ratio,xct_ratio),names_sep=":") %>%
  mutate(master_quant=`first_ratio:quant`) %>%
  select(!contains(":quant")) %>% 
  rename_with(~ gsub(":val","",.x,fixed=TRUE)) %>%
  rename(quant=master_quant) %>%
  pivot_longer(contains("ratio"),names_to="vtype",values_to="LS") %>%
  mutate(LS=case_when(is.infinite(LS) ~ NA,
                      TRUE ~ LS)) %>%
  pivot_wider(values_from=LS,names_prefix="q_",names_from=quant)

#absolute margin still high
#filter out "unmodelable" entries
#forbidden by sex
#lives-saved not modelable


ppv_summary_os_css_margin<-ppv_situation_df %>%
  select(Sex,Lower,Exposure,scenario, predicted_CSO, diagnostic_ls_css_first,diagnostic_ls_os_first,
         diagnostic_ls_css_xct,diagnostic_ls_os_xct) %>%
  filter(scenario=="old_3",Exposure=="Any") %>%
  filter(Lower>=50,Lower<=75) %>%
  anti_join(forbidden_sex_cso %>% rename(predicted_CSO=label)) %>%
  anti_join(no_stage_modeled %>% rename(predicted_CSO=label)) %>%
  anti_join(no_shift_expected %>% rename(predicted_CSO=label)) %>%
  anti_join(boring_cases %>% rename(predicted_CSO=label)) %>%
  mutate(first_css_margin=diagnostic_ls_css_first<margin_limit,
         first_os_margin=diagnostic_ls_os_first<margin_limit,
         xct_css_margin=diagnostic_ls_css_xct<margin_limit,
         xct_os_margin=diagnostic_ls_os_xct<margin_limit) %>%
  filter(!is.na(first_css_margin)) %>% 
  filter(!is.infinite(diagnostic_ls_css_first)) %>%
  select(Sex,Lower,first_css_margin,first_os_margin,xct_css_margin,xct_os_margin) %>%
  group_by(Sex,Lower) %>%
  summarize(first_css_fraction=mean(first_css_margin),
            first_os_fraction=mean(first_os_margin),
            xct_css_fraction=mean(xct_css_margin),
            xct_os_fraction=mean(xct_os_margin)) %>%
  ungroup() 

write_tsv(ppv_summary_os_css_margin, sprintf("reports/%s_summary_limits_os_vs_css.tsv",date_code))

#this occurs at lower ages because so few lives saved
#the only ones that fail LS - female cervix 50 (both cases, xct)
# male anus 50 xct, only OS (but marginal)


