library(tidyverse)


base_dataset <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb10528_detailed.tab.gz")

##### PROCESS FOLLOW-UP HBA1C #####

baseline_phenos <- read_csv("../data/processed/ukbb_diet_gwis_phenos.csv")

baseline_phenos <- baseline_phenos %>%
  mutate(hba1c_fu=ifelse(.$t1d %in% c(F, NA) & .$t2d %in% c(F, NA),
                      hba1c_2, NA),  # Remove those with confirmed diabetes
         hba1c_fu=ifelse(  # Remove outliers (outside of 3 IQRs from 25th/75th percentiles)
           findInterval(hba1c_fu, quantile(hba1c_fu, c(0.25, 0.75), na.rm=T) + 
                          3 * c(-1, 1) * IQR(hba1c_fu, na.rm=T)) == 1,
           hba1c_fu, NA
         ))  # Winsorize diabetic HbA1c values at upper outlier of non-diabetics

##### ADD FASTING GLUCOSE #####

sample_df <- select(base_dataset, id=f.eid, fasting_hrs=f.74.0.0)

glucose_df <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb28679.tab.gz") %>%
  select(id=f.eid, random_glucose=f.30740.0.0) %>%
  inner_join(sample_df, by="id") %>%
  mutate(fasting_glucose=ifelse(fasting_hrs >= 8, random_glucose, NA))

baseline_phenos <- left_join(baseline_phenos, glucose_df, by="id")

##### PROCESS FOLLOW-UP DIET #####

alcohol_dataset <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb40167.tab.gz")

diet2 <- base_dataset %>%
  rename(id=f.eid,
         year_of_birth.34=f.34.0.0,
         month_of_birth.52=f.52.0.0,
         date_assessmentcentre.53.1.0=f.53.1.0)

##### DATES #####

date2 <- str_split_fixed(diet2$date_assessmentcentre.53.1.0, "-", 3)
date2 <- apply(date2, 2, as.numeric)
colnames(date2) <- c("DIET2_date_year.1.0", "DIET2_date_month.1.0", "DIET2_date_day.1.0")
diet2 <- cbind(diet2, date2)

date_cols <- c("DIET2_date_year.1.0", "DIET2_date_month.1.0", "DIET2_date_day.1.0")
diet2 <- diet2 %>%
  separate(date_assessmentcentre.53.1.0, date_cols, "-") %>% # Split DATE into 3 columns (yr/mo/day)
  mutate_at(date_cols, as.numeric) %>%
  mutate(month_of_birth_numeric.52=match(month_of_birth.52, month.name),  # Convert month in text -> numeric
         DIET2_age_months.1.0=(DIET2_date_year.1.0 - year_of_birth.34) * 12 +  # Create age variable
           (DIET2_date_month.1.0 - month_of_birth_numeric.52))

##### CONTINUOUS #####

clean_cont <- function (x) {
  # -10 = <1; -1 = "do not know"; -3 = "prefer not to answer"
  case_when(
    x == -10 ~ 0.5,
    is.na(x) | x == -1 | x == -3 ~ as.numeric(NA),
    TRUE ~ as.numeric(x)
  )
}

diet2 <- diet2 %>%
  mutate(
    cookedveg_TBSperday.1289.1.0_QT=clean_cont(f.1289.1.0),
    rawveg_TBSperday.1299.1.0_QT=clean_cont(f.1299.1.0),
    freshfruit_piecesperday.1309.1.0_QT=clean_cont(f.1309.1.0),
    driedfruit_piecesperday.1319.1.0_QT=clean_cont(f.1319.1.0),
    bread_slicesperweek.1438.1.0_QT=clean_cont(f.1438.1.0),
    cereal_bowlsperweek.1458.1.0_QT=clean_cont(f.1458.1.0),
    tea_cupsperday.1488.1.0_QT=clean_cont(f.1488.1.0),
    coffee_cupsperday.1498.1.0_QT=clean_cont(f.1498.1.0),
    water_glassesperday.1528.1.0_QT=clean_cont(f.1528.1.0)
  )

##### ALCOHOL #####

alc2 <- alcohol_dataset %>%
  rename(id=f.eid,  # More informative names for alcohol fields
         alcohol_overallfreq.1558.1.0=f.1558.1.0,
         champagnewhitewine_glassesperweek.1578.1.0=f.1578.1.0,
         champagnewhitewine_glassespermonth.4418.1.0=f.4418.1.0,
         redwine_glassesperweek.1568.1.0=f.1568.1.0,
         redwine_glassespermonth.4407.1.0=f.4407.1.0,
         beercider_pintsperweek.1588.1.0=f.1588.1.0,
         beercider_pintspermonth.4429.1.0=f.4429.1.0,
         spirits_measuresperweek.1598.1.0=f.1598.1.0,
         spirits_measurespermonth.4440.1.0=f.4440.1.0,
         fortwine_glassesperweek.1608.1.0=f.1608.1.0,
         fortwine_glassespermonth.4451.1.0=f.4451.1.0,
         otheralcohol_glassesperweek.5364.1.0=f.5364.1.0,
         otheralcohol_glassespermonth.4462.1.0=f.4462.1.0,
         alcoholdrinkerstatus.20117.1.0=f.20117.1.0,
         alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0=f.1618.1.0) %>%
  select(id, contains("alcohol"), contains("per")) %>%
  mutate_at(vars(-id), as.numeric)

median_replace <- function(x) {
  # Replace (-1, -3) with median of the rest of the values
  ifelse(x %in% c(-1, -3), median(x[!(x %in% c(-1, -3))], na.rm=T), x)
}

alc2 <- alc2 %>%
  mutate(
    champagnewhitewine_glassesperweek.1578.1.0_QT=champagnewhitewine_glassesperweek.1578.1.0,
    champagnewhitewine_glassespermonth.4418.1.0_QT=champagnewhitewine_glassespermonth.4418.1.0,
    champagnewhitewine_glassespermonth.derived.1.0_QT=case_when(
      alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * champagnewhitewine_glassesperweek.1578.1.0_QT,
      alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ champagnewhitewine_glassespermonth.4418.1.0_QT,
      alcohol_overallfreq.1558.1.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    redwine_glassesperweek.1568.1.0_QT=redwine_glassesperweek.1568.1.0,
    redwine_glassespermonth.4407.1.0_QT=redwine_glassespermonth.4407.1.0,
    redwine_glassespermonth.derived.1.0_QT=case_when(
      alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * redwine_glassesperweek.1568.1.0_QT,
      alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ redwine_glassespermonth.4407.1.0_QT,
      alcohol_overallfreq.1558.1.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    beercider_pintsperweek.1588.1.0_QT=beercider_pintsperweek.1588.1.0,
    beercider_pintspermonth.4429.1.0_QT=beercider_pintspermonth.4429.1.0,
    beercider_pintspermonth.derived.1.0_QT=case_when(
      alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * beercider_pintsperweek.1588.1.0_QT,
      alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ beercider_pintspermonth.4429.1.0_QT,
      alcohol_overallfreq.1558.1.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    spirits_measuresperweek.1598.1.0_QT=spirits_measuresperweek.1598.1.0,
    spirits_measurespermonth.4440.1.0_QT=spirits_measurespermonth.4440.1.0,
    spirits_measurespermonth.derived.1.0_QT=case_when(
      alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * spirits_measuresperweek.1598.1.0_QT,
      alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ spirits_measurespermonth.4440.1.0_QT,
      alcohol_overallfreq.1558.1.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    fortwine_glassesperweek.1608.1.0_QT=fortwine_glassesperweek.1608.1.0,
    fortwine_glassespermonth.4451.1.0_QT=fortwine_glassespermonth.4451.1.0,
    fortwine_glassespermonth.derived.1.0_QT=case_when(
      alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * fortwine_glassesperweek.1608.1.0_QT,
      alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ fortwine_glassespermonth.4451.1.0_QT,
      alcohol_overallfreq.1558.1.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    otheralcohol_glassesperweek.5364.1.0_QT=otheralcohol_glassesperweek.5364.1.0,
    otheralcohol_glassespermonth.4462.1.0_QT=otheralcohol_glassespermonth.4462.1.0,
    otheralcohol_glassespermonth.derived.1.0_QT=case_when(
      alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * otheralcohol_glassesperweek.5364.1.0_QT,
      alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ otheralcohol_glassespermonth.4462.1.0_QT,
      alcohol_overallfreq.1558.1.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc2$anyalcohol_glassespermonth.derived.1.0_QT <- rowSums(  # Glasses of ANY alcohol per month
  select(alc2, contains("permonth.derived.1.0_QT")), na.rm=T
)

diet2 <- left_join(diet2, alc2, by="id")  # Add alcohol to main diet dataset


##### CATEGORICAL TO CONTINUOUS #####

cat_to_qt <- function(x) {
  case_when(  # Data-coding 100377
    x == "Once or more daily" ~ 365,
    x == "5-6 times a week" ~ 286,
    x == "2-4 times a week" ~ 156,
    x == "Once a week" ~ 52,
    x == "Less than once a week" ~ 20,
    x == "Never" ~ 0,
    TRUE ~ as.numeric(NA)
  )
}

diet2 <- diet2 %>%
  mutate(
    oilyfish_overallfreq.1329.1.0_QT=cat_to_qt(f.1329.1.0),
    nonoilyfish_overallfreq.1339.1.0_QT=cat_to_qt(f.1339.1.0),
    processmeat_overallfreq.1349.1.0_QT=cat_to_qt(f.1349.1.0),
    poultry_overallfreq.1359.1.0_QT=cat_to_qt(f.1359.1.0),
    beef_overallfreq.1369.1.0_QT=cat_to_qt(f.1369.1.0),
    lambmutton_overallfreq.1379.1.0_QT=cat_to_qt(f.1379.1.0),
    pork_overallfreq.1389.1.0_QT=cat_to_qt(f.1389.1.0),
    cheese_overallfreq.1408.1.0_QT=cat_to_qt(f.1408.1.0)
  )
ctqt_numbers <- c("1329", "1339", "1349", "1359", "1369", "1379", "1389", "1408")

diet2 <- diet2 %>%
  mutate(doyouaddsalt.1478.1.0_QT=case_when(  # Data-coding 100394
    f.1478.1.0 == "Always" ~ 10,
    f.1478.1.0 == "Usually" ~ 7,
    f.1478.1.0 == "Sometimes" ~ 3,
    f.1478.1.0 == "Never/rarely" ~ 0,
    TRUE ~ as.numeric(NA))
  )

diet2 <- diet2 %>%
  mutate(
    hotdrinktemp.1518.1.0_QT=case_when(  # Data-coding 100398
      f.1518.1.0 == "Very hot" ~ 10,
      f.1518.1.0 == "Hot" ~ 7,
      f.1518.1.0 == "Warm" ~ 3,
      f.1518.1.0 == "Do not drink hot drinks" ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

##### MISC #####

make_binary_var <- function(x, yes_values, all_values, na_values) {
  # Given a categorical vector, transform into a binary variable based on inputs
  # defining positive values, all values, and values to leave or set as missing
  case_when(
    x %in% yes_values ~ 1,
    x %in% setdiff(all_values, yes_values) ~ 0,
    x %in% na_values ~ as.numeric(NA),
    TRUE ~ as.numeric(NA)
  )
}

diet2$nevereatcategories.6144.1.combined <- paste(
  diet2$f.6144.1.0, diet2$f.6144.1.1,
  diet2$f.6144.1.2, diet2$f.6144.1.3,
  sep =":"
)

necomb_values <- c(
  "Dairy products:NA:NA:NA", "Dairy products:Sugar or foods/drinks containing sugar:NA:NA",
  "Dairy products:Wheat products:NA:NA", "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA",
  "Eggs or foods containing eggs:Dairy products:NA:NA",
  "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA",
  "Eggs or foods containing eggs:Dairy products:Wheat products:NA",
  "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar",
  "Eggs or foods containing eggs:NA:NA:NA",
  "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA",
  "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA",
  "Eggs or foods containing eggs:Wheat products:NA:NA",
  "I eat all of the above:NA:NA:NA", "Sugar or foods/drinks containing sugar:NA:NA:NA",
  "Wheat products:NA:NA:NA", "Wheat products:Sugar or foods/drinks containing sugar:NA:NA"
)

general_na_values <- c("Do not know", "Prefer not to answer", "NA")

necomb_na_values <- c(
  general_na_values, "NA:NA:NA:NA", "Prefer not to answer:NA:NA:NA"
)

diet2 <- diet2 %>%
  mutate(  # Data-coding 100385
    nevereatcategories.6144.1.0_bin2=make_binary_var(  # Eggs or foods containing eggs (1) vs. everyone else (0)
      nevereatcategories.6144.1.combined, grep("Eggs", necomb_values, value=T),
      necomb_values, necomb_na_values
    ),
    nevereatcategories.6144.1.0_bin4=make_binary_var(  # Dairy products (1)  vs. everyone else (0)
      nevereatcategories.6144.1.combined, grep("Dairy", necomb_values, value=T),
      necomb_values, necomb_na_values
    ),
    nevereatcategories.6144.1.0_bin6=make_binary_var(  # Wheat products (1)  vs. everyone else (0)
      nevereatcategories.6144.1.combined, grep("Wheat", necomb_values, value=T),
      necomb_values, necomb_na_values
    ),
    nevereatcategories.6144.1.0_bin8=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. everyone else (0))
      nevereatcategories.6144.1.combined, grep("Sugar", necomb_values, value=T),
      necomb_values, necomb_na_values
    )
  )

diet2$nevereatcategories.6144.1.combined <- NULL

milk_values <- c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Other type of milk", "Never/rarely have milk")
milk_na_values <- c(general_na_values)

diet2 <- diet2 %>%
  mutate(  # Data-coding 100387
    milk_typeused.1418.1.0_bin=make_binary_var(  # 1binary: milk x3 vs. never
      diet2$f.1418.1.0,
      c("Full cream", "Semi-skimmed", "Skimmed"), milk_values, milk_na_values
    ),
    milk_typeused.1418.1.0_QT14=case_when(  # 14continuous: skimmed (0), semi-skimmed (1), full cream (2)
      f.1418.1.0 == "Full cream" ~ 2,
      f.1418.1.0 == "Semi-skimmed" ~ 1,
      f.1418.1.0 == "Skimmed" ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

# Data-coding 100388 (butter type)
# Data-coding 100389 (alternate spread type)
# Field 2654 was collected if a non-butter spread was indicated for 1428

diet2 <- diet2 %>%
  mutate(spread_typeused.1428.0.nonbutterspread_typeused.2654.combined=paste(
    f.1428.1.0, f.2654.1.0, sep=":"
  ))

butter_values <- c(
  "Butter/spreadable butter:NA", "Do not know:Hard (block) margarine",
  "Other type of spread/margarine:Hard (block) margarine", "Do not know:Soft (tub) margarine",
  "Other type of spread/margarine:Soft (tub) margarine", "Do not know:Flora Pro-Active or Benecol",
  "Flora Pro-Active/Benecol:NA", "Other type of spread/margarine:Flora Pro-Active or Benecol",
  "Do not know:Olive oil based spread (eg: Bertolli)", "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)",
  "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)",
  "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)",
  "Do not know:Other low or reduced fat spread",
  "Other type of spread/margarine:Other low or reduced fat spread",
  "Other type of spread/margarine:Other type of spread/margarine",
  "Other type of spread/margarine:Do not know", "Do not know:Other type of spread/margarine",
  "Never/rarely use spread:NA"
)
butter_na_values <- c(
  "Other type of spread/margarine:Do not know", "Do not know:Do not know",
  "Do not know:Prefer not to answer", "Do not know:Other type of spread/margarine",
  "Prefer not to answer:NA", "Other type of spread/margarine:Prefer not to answer", "NA:NA"
)

diet2 <- diet2 %>%
  mutate(
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7=make_binary_var(  #7 Butter vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      "Butter/spreadable butter:NA", butter_values, butter_na_values
    )
  )

diet2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined <- NULL


# Field 1448 was collected if eat bread < 1 slice/wk based on 1438
bread_values <- c("White", "Brown", "Wholemeal or wholegrain", "Other type of bread")
diet2 <- diet2 %>%
  mutate(  # Data-coding 100391
    bread_typeused.1448.1.0_bin4=make_binary_var(  #5 White vs. Brown + Wholemeal
      f.1448.1.0, c("White"),
      c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
    )
  )

# Field 1468 was collected if eat cereal < 1 bowl/wk based on 1458
cereal_values <- c(
  "Biscuit cereal (e.g. Weetabix)", "Bran cereal (e.g. All Bran, Branflakes)",
  "Oat cereal (e.g. Ready Brek, porridge)", "Muesli", "Other (e.g. Cornflakes, Frosties)"
)
diet2 <- diet2 %>%
  mutate(  # Data-coding 100393
    cereal_typeused.1468.1.0_bin5=make_binary_var(  # Other vs. All Other
      f.1468.1.0,
      "Other (e.g. Cornflakes, Frosties)", cereal_values, general_na_values
    )
  )


# Field 1508 was collected if drink coffee < 1 cup/day or at least 1 cup/day based on 1498
coffee_values <- c(
  "Ground coffee (include espresso, filter etc)", "Instant coffee",
  "Decaffeinated coffee (any type)", "Other type of coffee"
)
coffee_NA_values <- c("Do not know", "Prefer not to answer", "NA")
diet2 <- diet2 %>%
  mutate(  # Data-coding 100397
    coffee_typeused.1508.1.0_bin3=make_binary_var(
      f.1508.1.0,
      "Decaffeinated coffee (any type)", coffee_values, general_na_values
    )
  )


##### SAVE #####

keep_cols_2 <- names(diet2)[!grepl("^f\\.", names(diet2))]
save_diet2 = diet2[, keep_cols_2]
saveRDS(save_diet2, "../data/processed/ukbiobank_diet2.rds")


##### COMBINE FOLLOW-UP DIETARY DATA #####

diet_FU <- readRDS("../data/processed/ukbiobank_diet2.rds")  # Follow-up dietary data
names(diet_FU) <- gsub("\\.1\\.0_", "_", names(diet_FU))  # Remove visit ID component from field name
diet_FU <- diet_FU %>%
  rename(age_months=DIET2_age_months.1.0) %>%
  left_join(select(base_dataset, id=f.eid, selfreportsex.31=f.31.0.0), by="id") %>%
  mutate(id=format(id, scientific=F))

pca_fields_subset <- c(
  "id",
  "selfreportsex.31",
  "age_months",
  "cookedveg_TBSperday.1289_QT",
  "rawveg_TBSperday.1299_QT",
  "freshfruit_piecesperday.1309_QT",
  "driedfruit_piecesperday.1319_QT",
  "bread_slicesperweek.1438_QT",
  "cereal_bowlsperweek.1458_QT",
  "tea_cupsperday.1488_QT",
  "coffee_cupsperday.1498_QT",
  "water_glassesperday.1528_QT",
  "anyalcohol_glassespermonth.derived_QT",
  "oilyfish_overallfreq.1329_QT",
  "nonoilyfish_overallfreq.1339_QT",
  "processmeat_overallfreq.1349_QT",
  "poultry_overallfreq.1359_QT",
  "beef_overallfreq.1369_QT",
  "lambmutton_overallfreq.1379_QT",
  "pork_overallfreq.1389_QT",
  "cheese_overallfreq.1408_QT",
  "doyouaddsalt.1478_QT",
  "hotdrinktemp.1518_QT",
  "milk_typeused.1418_QT14",
  "nevereatcategories.6144_bin2",
  "nevereatcategories.6144_bin4",
  "nevereatcategories.6144_bin6",
  "nevereatcategories.6144_bin8",
  "milk_typeused.1418_bin",
  "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7",
  "bread_typeused.1448_bin4",
  "cereal_typeused.1468_bin5",
  "coffee_typeused.1508_bin3"
)

diet_FU %>%
  select(all_of(pca_fields_subset)) %>%
  setNames(c("id", "selfreportsex.31", "age_months", 
           paste0(pca_fields_subset[-(1:3)], "_fu"))) %>%
write_tsv("../data/processed/diet_followup.txt")

##### FILTERING AND ADJUSTMENTS #####

diet_FU <- read_tsv("../data/processed/diet_followup.txt")

ids_to_include <- scan("../data/processed/ids_to_include.txt", what=integer())

diet_FU <- filter(diet_FU, id %in% ids_to_include) %>%
  rename(sex=selfreportsex.31)

INT <- function(x) qnorm((rank(x, na.last="keep") - 0.5) / sum(!is.na(x)))

diet_FU_adj <- diet_FU %>%
  mutate_at(vars(contains("_QT")), INT)

write_tsv(diet_FU_adj, "../data/processed/final_followup_ffq_data.txt")

baseline_phenos %>%
  left_join(select(rename(diet_FU_adj, age_fu=age_months), -sex), by="id") %>%
  write_csv("../data/processed/ukbb_diet_gwis_phenos_longitudinal.csv")
