#utility: add figure summary stats as xls


#summary stats for use in text

text_summary_stats<-ppv_situation_df %>% 
  filter(scenario=="old_3",Lower>=50, Upper<80) %>% 
  group_by(Lower,Sex,Exposure) %>% 
  summarize(overall_median=median(overall_ppv,na.rm=TRUE),
            ppv_first_median=median(ppv_first,na.rm=TRUE),
            ppv_cancer_median=median(ppv_cancer,na.rm=TRUE)) 


my_output_sheets<-list("figure_two"=figure_two_summary_stats,
                       "figure_three"=figure_three_summary_stats,
                       "figure_four"=figure_four_summary_stats,
                       "figure_five"=figure_five_summary_stats,
                       "figure_six"=figure_six_summary_stats,
                       "text"=text_summary_stats)

write.xlsx(my_output_sheets,file=sprintf("reports/%s_figure_summary_data.xlsx",date_code))