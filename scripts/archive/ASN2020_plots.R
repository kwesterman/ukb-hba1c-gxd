library(tidyverse)
library(RColorBrewer)


### Diet PCA ###
kw_pca <- readRDS("../data/processed/ffq_pca_fit.rds")
kw_loadings <- kw_pca$rotation
rownames(kw_loadings) <- c(
  "Cooked vegetables (tbsp/d)", "Raw vegetables (tbsp/d)",
  "Fresh fruit (pieces/d)", "Raw fruit (pieces/d)",
  "Bread (slices/d)", "Cereal (bowls/d)",
  "Tea (cups/d)", "Coffee (cups/d)", "Water (glasses/d)",
  "Alcohol (glasses/mo)", "Oily fish (servings/yr)", "Non-oily fish (servings/yr)",
  "Processed meat (servings/yr)", "Poultry (servings/yr)", "Beef (servings/yr)",
  "Lamb/mutton (servings/yr)", "Pork (servings/yr)", "Cheese (servings/yr)",
  "Add salt to food", "Hot drink temperature", "Level of fat in milk",
  "Avoid dairy", "Avoid eggs", "Avoid wheat", "Avoid sugar",
  "Dairy milk vs. any other", "Butter vs. other spreads",
  "White bread vs. brown or wholemeal", "Whole grain cereal vs. other types",
  "Decaf vs. regular cofee"
)
pheatmap::pheatmap(
  kw_loadings[, 1:10],
  breaks=seq(-max(abs(kw_loadings)), max(abs(kw_loadings)), length.out=100),
  cluster_rows=T, treeheight_row=0,
  cluster_cols=F,
  angle_col=45,
  filename="dPC_loadings.tiff",
  width=5, height=5
)


### Manhattan ###
exposures <- c(paste0("ffq_PC", 1:10), "ffq_PC_ERS10")
minimal_ss <- lapply(exposures, function(dPC) {
  read_tsv(paste0("../data/processed/main_ffq_PC_gwis_res/", dPC, "_merged_p0.01.gz")) %>%
    filter(CHR != "X") %>%
    mutate(CHR=as.integer(CHR)) %>%
    select(rsID, CHR, POS, P=P_Value_Interaction, P_joint=P_Value_Joint, P_marg=P_Value_Main)
})
names(minimal_ss) <- exposures

# full_ss <- lapply(c("ffq_PC_ERS10"), function(dPC) {
#   read_tsv(paste0("../data/processed/main_ffq_PC_gwis_res/", dPC, "_merged.gz")) %>%
#     filter(CHR != "X") %>%
#     mutate(CHR=as.integer(CHR)) %>%
#     select(rsID, CHR, POS, P_Value_Interaction)
# })
# names(full_ss) <- c("ffq_PC_ERS10")
# 
# big_df <- do.call(bind_rows, 
#                   c(minimal_ss[-which(names(minimal_ss) == "ffq_PC_ERS10")], 
#                     full_ss,
#                     .id="exposure"))

joint_df <- data.frame(rsID=character(0), CHR=integer(0), POS=integer(0), P=numeric(0))
for (exposure in exposures) {
  joint_df <- full_join(joint_df, minimal_ss[[exposure]], 
                        by=c("rsID", "CHR", "POS"),
                        suffix=c("", paste0(".", exposure)))
}
joint_df$P <- NULL

highlight_snps <- lapply(setNames(exposures, exposures), function(e) {
  df <- minimal_ss[[e]]
  df$rsID[df$P < 1e-6]
})
highlight_cols <- as.list(
  setNames(brewer.pal(n=length(exposures[4:11]), "Dark2"), exposures[4:11])
)
highlight_cols <- as.list(setNames(viridis::viridis(length(exposures)), exposures))



joint_df <- do.call(bind_rows, c(minimal_ss, .id="exposure"))
nlps <- -log10(joint_df$P)

# Trim points in crowded regions (credit to RaMWAS package for code snippet)
yfac = as.integer(nlps * 100) + 1L
yorder = sort.list(yfac)
levels(yfac) = as.character(seq_len(max(yfac)))
class(yfac) = "factor"
ygroup = split(seq_along(yfac), yfac)
for (i in seq_along(ygroup)) {
  if (length(ygroup[[i]]) > 300) {
    ygroup[[i]] = sample(ygroup[[i]], size=300, replace=FALSE)
  }
}
keep = unlist(ygroup, use.names=FALSE)

d <- cbind(select(joint_df, SNP=rsID, CHR, POS, exposure), pvalue=nlps)[keep, ]
d$POS <- as.numeric(as.character(d$POS))
d$CHR <- factor(d$CHR, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "X", "Y"))
d_order <- d[order(d$CHR, d$POS), ]
d_order$pos_index <- seq.int(nrow(d_order))
chr_lengths <- sapply(1:22, function(chr) max(d[d$CHR == chr, "POS"]))
chr_start_pos <- cumsum(chr_lengths) - chr_lengths
d_order$x_coord <- chr_start_pos[d_order$CHR] + d_order$POS
d_order_sub <- d_order[, c("SNP", "CHR", "POS", "exposure", "pvalue", "pos_index", "x_coord")]
maxRows <- by(d_order_sub, d_order_sub$CHR, function(x) x[which.max(x$x_coord),])
minRows <- by(d_order_sub, d_order_sub$CHR, function(x) x[which.min(x$x_coord),])
milimits <- do.call(rbind, minRows)
malimits <- do.call(rbind, maxRows)
lims <- merge(milimits, malimits, by="CHR")
names(lims) <- c("Color", 
                 "snpx", "posx", "exposure.x", "px", "posidxx", "xcoordmin",
                 "snpy", "posy", "exposure.y", "py", "posidxy", "xcoordmax")
lims$av <- (lims$xcoordmin + lims$xcoordmax)/2
lims <- lims[order(lims$Color),]

# d_order$Color <- case_when(
#   d_order$SNP %in% int_marg_snps ~ "30",
#   d_order$SNP %in% int_nomarg_snps ~ "31",
#   d_order$SNP %in% joint_nomarg_snps ~ "32",
#   TRUE ~ as.character(d_order$CHR)
# )
d_order$Color <- as.character(d_order$CHR)
for (exposure in exposures) {
  d_order$Color <- ifelse(d_order$exposure == exposure & d_order$pvalue > 5, 
                          exposure, d_order$Color)
}
d_order <- arrange(d_order, as.integer(Color), desc(pvalue)) %>%
  distinct(SNP, .keep_all=T)
newcols <-c(rep(x=c("#AAAAAA", "#4A4A4A"), length.out=22, each=1),  # Gray/dark gray for alternating chromosomes
            rev(viridis::viridis(11)))
names(newcols) <-c(levels(factor(lims$Color)), exposures)

p1 <- ggplot() +
  geom_point(data=d_order, 
             aes(x=x_coord, y=pvalue, color=factor(Color)), 
             size=0.75, alpha=1) +
  geom_hline(yintercept=-log10(5e-8), linetype="dashed", color="black") + 
  scale_x_continuous(breaks=lims$av[c(1:16, 18, 20, 20, 22)], 
                     labels=lims$Color[c(1:16, 18, 20, 20, 22)], 
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(2, 8), expand=c(0,0),
                     name=expression(-log[10](italic(p)))) +
  scale_colour_manual(name="", values=newcols,
                      breaks=names(newcols)[grepl("ffq", names(newcols))],
                      labels=gsub("ffq_", "d", 
                                  names(newcols)[grepl("ffq", names(newcols))])) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(vjust = -1.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  guides(color=guide_legend(ncol=2))

ggsave("asn_manhattan.png", width=12, height=2)


### Pruning and novelty ###
prune_chromosome <- function(chr_df, pval_col, locus_width) {
  # Prune variants given a chromosome-specific summary statistic data frame
  df <- arrange(chr_df, !!sym(pval_col))  # Sort by ascending p-value
  pruned_df <- tibble()
  while(nrow(df) > 0) {
    pruned_df <- bind_rows(pruned_df, df[1, ])  # Add lowest p-value to pruned dataset
    df <- filter(df, (POS < df$POS[1] - locus_width / 2) |  # Remove rest of variants in that distance-based locus
                   (POS > df$POS[1] + locus_width / 2))
  }
  pruned_df
}

prune_variants <- function(ss_df, pval_col, locus_width=500000) {
  # Prune variants across all chromosomes using a simple distance-based approach
  ss_df %>%
    nest(data=-CHR) %>%
    mutate(pruned_ss=map(data, prune_chromosome, pval_col, locus_width)) %>%
    unnest(pruned_ss) %>%
    select(-data) %>%
    dplyr::rename(index_var=rsID)
}

pruned_int_df <- minimal_ss %>%
  bind_rows(.id="exposure") %>%
  prune_variants(pval_col="P")
gw_int_loci <- pruned_int_df %>%
  filter(P < 5e-8)
suggestive_int_loci <- pruned_int_df %>%
  filter(P < 1e-6)

pruned_joint_df <- minimal_ss %>%
  bind_rows(.id="exposure") %>%
  prune_variants(pval_col="P_joint")
gw_joint_loci <- pruned_joint_df %>%
  filter(P_joint < 5e-8) %>%
  mutate(novel=P_marg > 5e-8)
suggestive_joint_loci <- pruned_joint_df %>%
  filter(P_joint < 1e-6)


### Sensitivity models ###
sm1_list <- lapply(setNames(exposures, exposures), function(e) {
  read_tsv(paste0("../data/processed/sensitivity_models/", e, "_SM1"))
}) %>%
  bind_rows(.id="exposure")
sm2_list <- lapply(setNames(exposures, exposures), function(e) {
  read_tsv(paste0("../data/processed/sensitivity_models/", e, "_SM2"))
}) %>%
  bind_rows(.id="exposure")
sm3_list <- lapply(setNames(exposures, exposures), function(e) {
  read_tsv(paste0("../data/processed/sensitivity_models/", e, "_SM3"))
}) %>%
  bind_rows(.id="exposure")
sm4_list <- lapply(setNames(exposures, exposures), function(e) {
  read_tsv(paste0("../data/processed/sensitivity_models/", e, "_SM4"))
}) %>%
  bind_rows(.id="exposure")


