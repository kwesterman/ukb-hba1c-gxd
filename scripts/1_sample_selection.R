library(tidyverse)


samplefile <- read_delim("/humgen/diabetes/UKBB_app27892/ukb2789_imp_chr17_v2_s487395.sample",
                         delim=" ", skip=2, col_names=c("FID", "IID", "missing")) %>%
  select(id=IID)

eur_unrelateds <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_genoQC_reportedANDgeneticEUR_unrelatedsoly_N378142_FID_IID_noheader.txt", 
                       col_names=c("FID", "IID")) %>%
  select(id=IID)

withdrawn_consent <- scan("/humgen/diabetes/UKBB_app27892/w27892_20200204.csv", what=character())

ids_to_include <- samplefile$id[samplefile$id %in% eur_unrelateds$id &
                                  !(samplefile$id %in% withdrawn_consent)]

write(ids_to_include, file="../data/processed/ids_to_include.txt")