#generate posterior draws from dirichlet multinomial for confusion matrix

raw_confusion_complete_counts<-read_tsv(sprintf("generated_data/%s_raw_confusion_complete.tsv",input_date_code))

#used for reference prior in dirichlet_multiomial
estimate_classifier_bias<-raw_confusion_complete_counts %>%
  filter(positive_type=="tp") %>%
  group_by(`Predicted CSO`) %>%
  summarize(total=sum(N)+1) %>%
  ungroup() %>%
  mutate(ref_frac=total/sum(total)) #latent frequency estimate

#because of very small counts of FP
#need to mostly draw from classifier bias
augmented_counts<-raw_confusion_complete_counts %>%
  left_join(estimate_classifier_bias) %>%
  mutate(posterior_N=case_when(positive_type=="tp" ~ N+ref_frac,
                                   TRUE ~ N+25*ref_frac)) %>%
  group_by(positive_type,`Expected CSO`) %>%
  mutate(Ndraw=sum(N)) %>%
  ungroup() 

n_cso_sample<-500
set.seed(my_seed+100000) #offset to avoid correlation

#gamma produces many very small numbers
#don't believe so unlikely an event
#blunt the distribution slightly for 'realism'
dirichlet_draw_cso<-augmented_counts %>%
  cross_join(tibble(iter=1:n_cso_sample)) %>%
  mutate(gamma_draw=pmax(rgamma(length(posterior_N),posterior_N,1),1e-2)) %>%
  group_by(positive_type,`Expected CSO`,iter) %>%
  mutate(dirichlet_draw=gamma_draw/sum(gamma_draw),
         orig_frac=posterior_N/sum(posterior_N)) %>%
  ungroup()

#now that dirichlet is generated for each expected CSO
#multinomial draw will result in discretization artifacts in posterior
#just keep the rate
multinom_draw_cso<-dirichlet_draw_cso %>%
  group_by(positive_type, `Expected CSO`,iter) %>%
  mutate(m_draw=Ndraw*dirichlet_draw) %>%
  ungroup() %>%
  select(positive_type,`Expected CSO`,`Predicted CSO`,N=m_draw,iter)

#write out the generated confusion matrices
write_tsv(multinom_draw_cso,sprintf("generated_data/%s_raw_multinom_complete.tsv",date_code))