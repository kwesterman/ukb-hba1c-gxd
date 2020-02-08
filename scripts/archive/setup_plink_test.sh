PLINK2=../../opt/plink2

$PLINK2 --bgen ../data/processed/chr22_random_10k.bgen --sample ../data/processed/ukb27892_imp_chrAUT_v3_s487395.sample --make-pgen --out ../data/processed/plinktest

R --no-save << EOF
library(tidyverse)
phenos <- read_csv("../data/processed/ukbb_diet_gwis_phenos.csv") %>%
  mutate(`#FID`=id, IID=id) %>%
  select(`#FID`, IID, everything())
write_tsv(phenos, "../data/processed/plink_phenos.tsv")
EOF

$PLINK2 --pfile ../data/processed/plinktest --pheno ../data/processed/plink_phenos.tsv --pheno-name hba1c --covar-name age sexMale PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 --glm --out ../data/processed/plinktest
