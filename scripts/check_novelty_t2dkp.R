library(tidyverse)
library(readxl)
library(jsonlite)


query_region_t2dkp <- function(start, end, chr, pheno) {
  # Make call to T2DKP using "GET" request
  base_url <- "http://public.type2diabeteskb.org/dccservices/getAggregatedDataSimple?"
  start_str <- paste0("start=", start)
  end_str <- paste0("end=", end)
  chrom_str <- paste0("chrom=", chr)
  pheno_str <- paste0("phenotype=", pheno)
  full_get_str <- paste0(base_url, start_str, "&", end_str, "&", chrom_str, 
                         "&", pheno_str, "&limit=10") 
  json_output <- system(paste0("curl -X GET '", 
                               full_get_str, "' -H 'accept: application/json'"),
                        intern=T)
  fromJSON(json_output)$variants %>%  # Convert JSON output to data frame
    select(P_VALUE, dataset, DBSNP_ID) %>%
    setNames(paste0("prior_", pheno, c("_pval", "_dataset", "_snpID"))) %>%  # "Prior" refers to prior evidence in T2DKP
    dplyr::slice(1)
}

# Construct table of loci to query (make sure it is hg19!)
locus_df <- read_tsv("") %>%
  separate(XXX, into=c("chr", "PosRefAlt"), sep=":") %>%
  separate(PosRefAlt, into=c("pos", "ref", "alt"), sep="_") %>%
  mutate(start=as.integer(pos) - 500000,
         end=as.integer(pos) + 500000)

# Query T2DKP for top loci
top_hits_prior <- locus_df %>%
  mutate(prior_evidence=pmap(list(start, end, chr), function(s, e, ch) {
    query_region_t2dkp(s, e, ch, "HBA1C")
  }))

# write_csv(top_hits_prior, "prior_findings_at_index_loci.csv")
# 
# putative_novel_regions <- top_hits_prior %>%
#   filter(prior_FG_pval > 5e-8 & prior_FI_pval > 5e-8 & 
#            prior_FGadjBMI_pval > 5e-8 & prior_FIadjBMI_pval) %>%
#   mutate_at(vars(start, end), as.numeric) %>%
#   inner_join(select(cred_set_loci, Region, start, end), by="Region", suffix=c("_hg19", "_GRCh38"))
# write_csv(putative_novel_regions, "putatively_novel_loci.csv")
