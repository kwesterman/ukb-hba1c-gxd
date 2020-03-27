library(tidyverse)


eur_unrelateds <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_genoQC_reportedANDgeneticEUR_unrelatedsoly_N378142_FID_IID_noheader.txt",
                           col_names=c("FID", "IID")) %>%
  select(id=IID)

# ffq_pca_fit <- readRDS("../data/processed/ffq_pca_fit.rds")  # R prcomp() object
# pc_var_exp <- ffq_pca_fit$sdev ** 2 / sum(ffq_pca_fit$sdev ** 2)  # Variance explained by each FFQ-PC
# keep_PCs <- paste0("ffq_PC", seq(1, sum(pc_var_exp > 0.01)))  # Vector of FFQ-PCs w/ >1% var. expl.

diet_phenos <- read_tsv("../data/processed/ffq_PCs.txt") %>%  # FFQ-PCs (for only unrelated, European ancestry)
  select(id, everything())  # Could also select only a subset of dietary traits

confounders <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_genoQC_reportedANDgeneticEUR_N455146_FLOREZ_EUR_PCA_covariates_40dim.txt") %>%
  select(id=Florez_IID, contains("PC"), cov_GENO_ARRAY=genotyping.array)

admin <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_assessment_center_f.54_birthplace_f.1647.txt") %>%
  select(id=Florez_FID, assessment_centre=f.54.0.0_categorical, birthplace=f.1647.0.0_categorical)

phenos1 <- read_tsv("/humgen/diabetes/users/jcole/UKBB/pheno/round2_T2D_updateMay2019/UKBiobank_genoQC_reportedANDgeneticEUR_N455146_raw_and_diabetesphenotypes_complete_updateAug2019.txt") %>%
  select(id=FLOREZ_IID,
         t1d=prob_poss_t1dm_all_plus,
         t2d=prob_poss_t2dm_all_plus_t2d_controls_strict_hba1c,
         hba1c_max=hba1c.30750.NGSP.max,
         age=age_months.0.0,
         sex=f.31.0.0) %>%  
  mutate(sex=c(Male=0, Female=1)[sex],  # Convert sex to F=0, M=1
         age_squared=age ** 2)
phenos2 <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb28679.tab.gz") %>%
  select(id=f.eid, hba1c_1=f.30750.0.0, hba1c_2=f.30750.1.0)
phenos <- full_join(phenos1, phenos2, by="id")

# Merge all phenotypes and covariates
gwis_phenos <- eur_unrelateds %>%
  left_join(diet_phenos) %>%
  left_join(confounders) %>%
  left_join(admin) %>%
  left_join(phenos) %>%
  mutate(id=format(id, scientific=F))  # Allows for better read-in after writing

write_csv(gwis_phenos, "../data/processed/ukbb_diet_gwis_phenos_raw.csv")

# Convert boolean and categorical variables to numeric
gwis_phenos <- rename(gwis_phenos, ac=assessment_centre, bp=birthplace)  # Reduce variable name size
gwis_model_phenos <- model.matrix(
  ~., data=model.frame(~., data=gwis_phenos, na.action=na.pass)
) %>%
  as.data.frame() %>%
  select(-1) %>%
  setNames(make.names(names(.)))

# HbA1c-specific processing
INT <- function(x) qnorm((rank(x, na.last="keep") - 0.5) / sum(!is.na(x)))
gwis_model_phenos <- gwis_model_phenos %>%
  mutate(hba1c=ifelse(.$t1d %in% c(F, NA) & .$t2d %in% c(F, NA),
                          hba1c_1, NA),  # Remove those with confirmed diabetes
         hba1c=ifelse(  # Remove outliers (outside of 3 IQRs from 25th/75th percentiles)
           findInterval(hba1c, quantile(hba1c, c(0.25, 0.75), na.rm=T) + 
                          3 * c(-1, 1) * IQR(hba1c, na.rm=T)) == 1,
           hba1c, NA
         ),
         hba1c_INT=INT(hba1c),
         hba1c_log=log(hba1c),
         hba1c_withDM=hba1c_1)

# Create FFQ-PC-based "environmental risk score"
keep_PCs <- grep("ffq_PC", names(gwis_model_phenos), value=T)
lm_formula <- as.formula(paste0("hba1c ~ ", paste(keep_PCs, collapse=" + ")))
lm_fit <- lm(lm_formula, data=gwis_model_phenos)  # Linear model predicting HbA1c from all relevant FFQ-PCs
saveRDS(lm_fit, "../data/processed/ERS_lm_fit.rds")
gwis_model_phenos$ffq_PC_ERS <- predict(lm_fit, newdata=gwis_model_phenos)  # Generate ERS for each individual (including individuals w/ DM)
gwis_model_phenos$id <- format(gwis_model_phenos$id, scientific=F)  # Allows for better read-in after writing

write_csv(gwis_model_phenos, "../data/processed/ukbb_diet_gwis_phenos.csv")
write_csv(slice(gwis_model_phenos, 1:1000), "../data/processed/ukbb_diet_gwis_phenos_1k.csv")
write_csv(slice(gwis_model_phenos, 1:10000), "../data/processed/ukbb_diet_gwis_phenos_10k.csv")



# heritable_traits <- scan("/humgen/diabetes2/users/jcole/UKBB/diet/diet_ffq_143sigtraits.txt", what=character())
# heritable_pcs <- grep("PC", heritable_traits, value=T)
# 
# diet_phenos2 <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/diet/BOLT_UKB_diet_genoQCEUR450K_170FFQphenotypes_agesexadj_INV") %>%
#   select(id=IID, one_of(heritable_pcs))
