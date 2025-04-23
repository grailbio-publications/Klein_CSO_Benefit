#generate FP draws

fp_counts<-read_tsv("data/klein_fp_counts.tsv")

#reproducibility
set.seed(seed=my_seed+11235) #break correlations

#posterior beta for rate
#do not do additional binomal draw
#discrete artifacts will change posterior distribution
fp_draws<-fp_counts %>%
  mutate(FP=Total-TN) %>%
  cross_join(tibble(iter=1:nsample)) %>%
  mutate(beta_fp=rbeta(length(iter),(FP+0.5),(TN+0.5))) %>%
  mutate(fp_draw=beta_fp*Total) 

write_tsv(fp_draws %>% select(iter,fp_draw,Total),sprintf("generated_data/%s_fp_draws.tsv",date_code))