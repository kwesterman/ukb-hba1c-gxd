library(tidyverse)

# famfile <- read_delim("/humgen/diabetes/UKBB_app27892/ukb27892_cal_chrAUT_v2_s488363.fam", 
#                       delim=" ", col_names=c("FID", "IID", "Fa", "Mo", "Sex", "Pheno")) %>%
#   select(id=IID)
samplefile <- read_delim("/humgen/diabetes/UKBB_app27892/ukb2789_imp_chr17_v2_s487395.sample",
                         delim=" ", skip=2, col_names=c("FID", "IID", "missing")) %>%
  select(id=IID)

heritable_traits <- scan("/humgen/diabetes2/users/jcole/UKBB/diet/diet_ffq_143sigtraits.txt", what=character())
heritable_pcs <- grep("PC", heritable_traits, value=T)

diet_phenos <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/diet/BOLT_UKB_diet_genoQCEUR450K_170FFQphenotypes_agesexadj_INV") %>%
  select(id=IID, one_of(heritable_pcs))

confounders <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_genoQC_reportedANDgeneticEUR_N455146_FLOREZ_EUR_PCA_covariates.txt") %>%
  select(id=FLOREZ_IID, contains("PC"), cov_GENO_ARRAY)

unrelateds <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_genoQC_reportedANDgeneticEUR_unrelatedsoly_N378142_FID_IID_noheader.txt", 
                       col_names=c("FID", "IID")) %>%
  select(id=IID)

withdrawn_consent <- scan("/humgen/diabetes/UKBB_app27892/w27892_20181016.csv", what=character())

admin <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_assessment_center_f.54_birthplace_f.1647.txt") %>%
  select(id=Florez_FID, assessment_centre=f.54.0.0_categorical, birthplace=f.1647.0.0_categorical)

phenos <- read_tsv("/humgen/diabetes/users/jcole/UKBB/pheno/round2_T2D_updateMay2019/UKBiobank_genoQC_reportedANDgeneticEUR_N455146_raw_and_diabetesphenotypes_complete_updateAug2019.txt") %>%
  select(id=FLOREZ_IID,
         t2d_strict=prob_poss_t2dm_all_plus_t2d_controls_strict_hba1c,
         t2d_loose=prob_poss_t2dm_all_plus_t2d_controls_loose_hba1c,
         hba1c=hba1c.30750.NGSP.max,
         age=age_months_average,
         sex=f.31.0.0)

# Merge all phenotypes and covariates
gwis_phenos <- samplefile %>%
  left_join(diet_phenos) %>%
  left_join(confounders) %>%
  left_join(admin) %>%
  left_join(phenos)
gwis_outcomes <- c("hba1c", "t2d_strict", "t2d_loose")  # GWIS outcomes
gwis_phenos <- gwis_phenos %>%  # Set phenotypes to missing if related or withdrew consent
  mutate_at(gwis_outcomes, function(x) {
    ifelse(!(gwis_phenos$id %in% unrelateds$id) | 
             (gwis_phenos$id %in% withdrawn_consent), NA, x)
  })

write_csv(gwis_phenos, "../data/processed/ukbb_diet_gwis_phenos_raw.csv")

# Convert boolean and categorical variables to numeric
gwis_phenos <- rename(gwis_phenos, ac=assessment_centre, bp=birthplace)  # Reduce variable name size
gwis_model_phenos <- model.matrix(
  ~., data=model.frame(~., data=gwis_phenos, na.action=na.pass)
) %>%
  as.data.frame() %>%
  select(-1) %>%
  setNames(make.names(names(.)))

# Inverse-normal transformation of continuous outcome
INT <- function(x) qnorm((rank(x, na.last="keep") - 0.5) / sum(!is.na(x)))
gwis_model_phenos <- gwis_model_phenos %>%
  mutate(hba1c=INT(hba1c))

write_csv(gwis_model_phenos, "../data/processed/ukbb_diet_gwis_phenos.csv")
write_csv(slice(gwis_model_phenos, 1:1000), "../data/processed/ukbb_diet_gwis_phenos_1k.csv")
write_csv(slice(gwis_model_phenos, 1:10000), "../data/processed/ukbb_diet_gwis_phenos_10k.csv")
