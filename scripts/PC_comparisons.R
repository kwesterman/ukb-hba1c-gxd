library(tidyverse)


kwPCs <- read_tsv("../data/processed/ffq_median_PCs.txt")
jcPCs <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/diet/BOLT_UKB_diet_genoQCEUR450K_170FFQphenotypes_agesexadj_INV") %>%
  select(id=IID, everything(), -FID)

combined <- inner_join(kwPCs, jcPCs, by="id", suffix=c(".kw", ".jc"))

pc_corrs <- sapply(seq(1, ncol(kwPCs) - 1), function(i) {
  fields <- paste0("ffq_median_PC", i, c(".kw", ".jc"))
  cor(combined[[fields[1]]], combined[[fields[2]]])
})
pc_corr_df <- tibble(`FFQ-PC`=1:length(pc_corrs),
                     abs_pearson=abs(pc_corrs))

pc_corr_plt <- ggplot(pc_corr_df, aes(x=`FFQ-PC`, y=abs_pearson)) +
  geom_bar(stat="identity") +
  labs(y="Absolute value of Pearson correlation (JC vs. KW PCs)")
ggsave("../results/jc_kw_PC_correlations.pdf", pc_corr_plt)

kw_pca_fit <- readRDS("../data/processed/ffq_pca_fit.rds")
load("/humgen/diabetes2/users/jcole/UKBB/diet/ffq_median.pca_11212018update.Rdata")
jc_pca_fit <- ffq_median.pca

pdf("pc25_loadings.pdf")
plot(x=kw_pca_fit$rot[,25], y=jc_pca_fit$rot[,25], 
     xlab="KW loadings", ylab="JC loadings")
text(x=kw_pca_fit$rot[,25], y=jc_pca_fit$rot[,25], 
     labels=rownames(kw_pca_fit$rot), 
     cex=0.3, srt=30)
dev.off()




# on uger:
a <- lapply(pca_fields, function(f) round(sum(!is.na(diet1[[f]])) / nrow(diet1), 2))
names(a) <- pca_fields
saveRDS(a, "sanity_check_fields.rds")

a <- readRDS("sanity_check_fields.rds")
kw_missingness <- tibble(trait=names(a), frac=unlist(a))

b <- read_tsv("~/Downloads/UKB_diet_SampleSizeByTrait.txt")
jc_missingness <- tibble(trait=b$trait, frac=round(b$N / 449210, 2))
jc_missingness2 <- mutate(jc_missingness, trait=gsub("\\.average", "", trait))

compare_df <- full_join(kw_missingness, jc_missingness2, by="trait", suffix=c("_kw", "_jc"))







