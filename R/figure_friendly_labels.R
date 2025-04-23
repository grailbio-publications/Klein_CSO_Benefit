#make some pretty components for figures as a resource

pretty_cso_labels<-tribble(~internal_CSO,~pretty_CSO,
                           "Plasma cell neoplasm","Plasma Cell Neoplasm",
                           "Myeloid lineage","Myeloid Neoplasm",
                           "Lymphoid lineage","Lymphoid Neoplasm",
                           "Uterus","Uterus",
                           "Thyroid gland","Thyroid",
                           "Stomach, esophagus","Stomach, Esophagus",
                           "Bone and soft tissue","Sarcoma",
                           "Prostate","Prostate",
                           "Pancreas, gallbladder","Pancreas, Gallbladder",
                           "Ovary","Ovary",
                           "Neuroendocrine cells","Neuroendocrine",
                           "Melanocytic lineage","Melanoma",
                           "Lung","Lung",
                           "Liver, bile duct","Liver/Bile\u00adduct",
                           "Kidney","Kidney",
                           "Head and neck","Head and Neck",
                           "Colon, rectum","Colon/Rectum",
                           "Cervix","Cervix",
                           "Breast","Breast",
                           "Bladder, urothelial","Bladder and Urothelial",
                           "Anus","Anus") %>%
  mutate(pretty_CSO=factor(pretty_CSO,levels=pretty_CSO))

diagnostic_labels<-c('overall_ppv' = "PPV Any Cancer",
                     'ppv_first' = "PPV First CSO",
                     'ppv_xct'  = "PPV Other Cancers")

ls_labels<-c('diagnostic_ls_css_first'="CSO\u00adDirected Tests Needed Per Life Saved (CSS)",
             'diagnostic_ls_css_xct' = "Non\u00adsite\u00adspecific Tests Needed Per Life Saved (CSS)",
             'diagnostic_ls_os_first'="CSO\u00adDirected Tests Needed Per Life Saved (OS)",
             'diagnostic_ls_os_xct' = "Non\u00adsite\u00adspecific Tests Needed Per Life Saved (OS)")
