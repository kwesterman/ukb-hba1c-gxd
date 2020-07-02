library(tidyverse)


##### DATA READ-IN #####

# base_dataset <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb10528_detailed.tab.gz")


# NEED:
# sex (31)
# sex mismatch (22051)
# age: f.21022.0.0
# QC: sample QC for genotyping, sex mismatch, race mismatch?
# Genotyping array: (22000?)
# diabetes (??)
# genetic PCs: (f.22009.0.1, f.22009.0.2, etc.)
# Ethnicity: f.21000.0.0 (way to confirm that genetic ancestry matches this?)


# GLYCEMIC PHENOTYPES AND COVARIATES ------------------------------------------

admin <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_assessment_center_f.54_birthplace_f.1647.txt") %>%
  select(id=Florez_FID, assessment_centre=f.54.0.0_categorical, birthplace=f.1647.0.0_categorical)

phenos1 <- read_tsv("/humgen/diabetes/users/jcole/UKBB/pheno/round2_T2D_updateMay2019/UKBiobank_ALLethnicities_diabetes_complete_2020Feb.txt") %>%
  select(id=f.eid,
         t1d=prob_poss_t1dm_all_plus,
         t2d=prob_poss_t2dm_all_plus_t2d_controls_strict_hba1c,
         hba1c_max=hba1c.30750.NGSP.max,
         age=age_months.0.0,
         sex=f.31.0.0) %>%  
  mutate(sex=c(Male=0, Female=1)[sex],  # Convert sex to M=0, F=1
         age_squared=age ** 2)

phenos2 <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb28679.tab.gz") %>%
  select(id=f.eid, hba1c_1=f.30750.0.0, hba1c_2=f.30750.1.0)

phenos3 <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb10528_detailed.tab.gz",
                    col_types=cols_only(f.eid="d", f.21001.0.0="d", f.20116.0.0="c")) %>%
  select(id=f.eid,
         bmi=f.21001.0.0,
         smoking=f.20116.0.0#,
         # walk_dpw=f.864.0.0,
         # walk_mpd=f.874.0.0,
         # mod_PA_dpw=f.884.0.0,
         # mod_PA_mpd=f.894.0.0,
         # vig_PA_dpw=f.904.0.0,
         # vig_PA_mpd=f.914.0.0
  ) %>%
  mutate(smk_012=case_when(smoking == "Never" ~ 0,
                           smoking == "Previous" ~ 1,
                           smoking == "Current" ~ 2,
                           TRUE ~ as.numeric(NA)),
         smk_yn=case_when(smoking == "Never" ~ 0,
                          smoking %in% c("Previous", "Current") ~ 1,
                          TRUE ~ as.numeric(NA))
         # pa_mets_wk=3.3 * walk_dpw * walk_mpd +
         #   4.0 * mod_PA_dpw * mod_PA_mpd +
         #   8.0 * vig_PA_dpw * vig_PA_mpd - 1
  ) %>%
  select(-smoking)

phenos <- full_join(phenos1, phenos2, by="id") %>%  # Primary phenotypes
  left_join(phenos3, by="id") %>%  # For sensitivity models
  left_join(admin, by="id")

withdrawn_consent <- scan("/humgen/diabetes/UKBB_app27892/w27892_20200204.csv", what=character())
phenos <- filter(phenos, !(id %in% withdrawn_consent))

# DIET ----------------------------------------------------------------------

diet_traits <- read_tsv("../data/processed/diet_baseline.txt")
  
INT <- function(x) qnorm((rank(x, na.last="keep") - 0.5) / sum(!is.na(x)))

## ANCESTRY-SPECIFIC IDS AND GENETIC PCS ---------------------------------------

ancestries <- c("AFR", "EAS", "SAS")

anc_spec_phenos <- lapply(setNames(ancestries, ancestries), function(anc) {
  idfile <- grep(paste0("_", anc, "_"), 
                 list.files("/humgen/diabetes2/users/jcole/UKBB/pheno/multiethnic/",
                            full.names=T),
                 value=T)
  anc_phenos <- read_tsv(idfile) %>%
    filter(unrelateds == 1) %>%
    select(id=Florez_IID, cov_GENO_ARRAY, all_of(paste0("PC", 1:40))) %>%
    inner_join(phenos, by="id") %>%
    inner_join(diet_traits, by="id") %>%
    mutate_at(vars(contains("_QT")), INT)
  
  anc_phenos <- rename(anc_phenos, ac=assessment_centre, bp=birthplace)  # Reduce variable name size
  if (length(unique(anc_phenos$cov_GENO_ARRAY)) == 1) anc_phenos$cov_GENO_ARRAY <- NULL
  anc_model_phenos <- model.matrix(
    ~., data=model.frame(~., 
                         data=mutate(anc_phenos, id=as.integer(id)), 
                         na.action=na.pass)
  ) %>%
    as.data.frame() %>%
    select(-1) %>%
    setNames(make.names(names(.)))
  
  anc_model_phenos <- anc_model_phenos %>%
    mutate(hba1c=ifelse(.$t1d %in% c(F, NA) & .$t2d %in% c(F, NA),
                        hba1c_1, NA),  # Remove those with confirmed diabetes
           hba1c=ifelse(  # Remove outliers (outside of 3 IQRs from 25th/75th percentiles)
             findInterval(hba1c, quantile(hba1c, c(0.25, 0.75), na.rm=T) + 
                            3 * c(-1, 1) * IQR(hba1c, na.rm=T)) == 1,
             hba1c, NA
           ),
           hba1c_INT=INT(hba1c),
           hba1c_log=log(hba1c),
           hba1c_withDM=pmin(hba1c_1, max(hba1c, na.rm=T)))  # Winsorize diabetic HbA1c values at upper outlier of non-diabetics
  
  anc_model_phenos
})


for (anc in ancestries) {
  write_csv(anc_spec_phenos[[anc]], 
            paste0("../data/processed/ukbb_diet_gwis_phenos_", anc, ".csv"))
}




