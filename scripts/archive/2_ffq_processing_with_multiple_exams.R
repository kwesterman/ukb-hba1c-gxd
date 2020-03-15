library(tidyverse)
library(stringr)

######################## STEP01-03 combined from Hirschhorn -> Florez Dir

### see column files for fields.

# columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_other > \
# /humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_8785_other_columns
# 
# columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_other > \
# /humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_6184_other_columns
# 
# columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_diet1.0 > \
# /humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_8785_diet1.0_columns
# 
# columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_diet1.0 > \
# /humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_6184_diet1.0_columns
# 
# columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_7052_diet1.0 > \
# /humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_7052_diet1.0_columns


#########################################################################################################
#########################################################################################################
################### FFQ Diet Phenotypes - Florez Application
################### STEP 1: Sample exclusions, Derive QT averages from all instances.
########## Original code last updated: July 6, 2018 (JBC)
########## Update for reformatting: February 2019 (KEW)
##############################x###########################################################################
#########################################################################################################


# # derive more informative field names
# cols1 <- read_delim("/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_8785_diet1.0_columns",
#                     delim=" ", col_names=c("idx", "field"))
# cols2 <- read_delim("/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_6184_diet1.0_columns",
#                     delim=" ", col_names=c("idx", "field"))
# cols3 <- read_delim("/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_7052_diet1.0_columns",
#                     delim=" ", col_names=c("idx", "field"))
# col_dict <- tibble(informative=c(cols1$field, cols2$field, cols3$field)) %>%
#   filter(informative != "ID") %>%
#   separate(informative, c("name", "fieldID", "col3", "col4"), sep="\\.") %>%
#   mutate(raw_1.0=paste("f", fieldID, "0", col4, sep="."),
#          informative_1.0=paste(name, fieldID, "0", col4, sep="."),
#          raw_1.1=paste("f", fieldID, "1", col4, sep="."),
#          informative_1.1=paste(name, fieldID, "0", col4, sep="."),
#          raw_1.2=paste("f", fieldID, "2", col4, sep="."),
#          informative_1.2=paste(name, fieldID, "0", col4, sep="."))c

##### DATA READ-IN #####

base_dataset <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb10528_detailed.tab.gz")

alcohol_dataset <- read_tsv("/humgen/diabetes/UKBB_app27892/ukb40167.tab.gz")

##############################
########## DIET 1.0 ##########
##############################

diet1.0 <- base_dataset %>%
  rename(id=f.eid,
         year_of_birth.34=f.34.0.0,
         month_of_birth.52=f.52.0.0,
         date_assessmentcentre.53.0.0=f.53.0.0)
# names(diet1.0) <- with(col_dict, {
#   # raw_names <- paste("f", fieldID, "0.0", sep=".")
#   # informative_names <- paste(name, fieldID, "0.0", sep=".")
#   ifelse(names(diet1.0) %in% raw_1.0,
#          informative_1.0[match(names(diet1.0), raw_1.0)],
#          names(diet1.0))
# })
# diet1.0 <- select(diet1.0, which(!grepl("^f\\.", names(diet1.0))))

### DATES
# Create appropriate age variables per instance
# DIET1: date_assessmentcentre.53.0.0 -> birth year + birth month

date1.0 <- str_split_fixed(diet1.0$date_assessmentcentre.53.0.0, "-", 3)
date1.0 <- apply(date1.0, 2, as.numeric)
colnames(date1.0) <- c("DIET1_date_year.0.0", "DIET1_date_month.0.0", "DIET1_date_day.0.0")
diet1.0 <- cbind(diet1.0, date1.0)

date_cols <- c("DIET1_date_year.0.0", "DIET1_date_month.0.0", "DIET1_date_day.0.0")
diet1.0 <- diet1.0 %>%
  separate(date_assessmentcentre.53.0.0, date_cols, "-") %>% # Split DATE into 3 columns (yr/mo/day)
  mutate_at(date_cols, as.numeric) %>%
  mutate(month_of_birth_numeric.52=match(month_of_birth.52, month.name),  # Convert month in text -> numeric
         DIET1_age_months.0.0=(DIET1_date_year.0.0 - year_of_birth.34) * 12 +  # Create age variable
           (DIET1_date_month.0.0 - month_of_birth_numeric.52))

##### EXCLUSIONS #####

# diet1.0 <- diet1.0 %>%
#   left_join(cancer, by="id") %>%
#   filter(f.3140.0.0 != "Yes" | is.na(f.3140.0.0))  # Not pregnant
#          # !(id %in% esrd))  # No ESRD
#
# calc_months_from_cancer_diag <- function(x, df) {
#   diet_month <- df$DIET1_date_month.0.0
#   diet_year <- df$DIET1_date_year.0.0
#   cancer_month <- df[[paste0("date_cancerdiag_month.40005.", x)]]
#   cancer_year <- df[[paste0("date_cancerdiag_year.40005.", x)]]
#   12 * (diet_year - cancer_year) + (diet_month - cancer_month)
# }
# for (dn in paste0(0:10, ".0")) {
#   diet1.0[[paste0("DIET1_monthsfromcancerdiag.", dn)]] <- calc_months_from_cancer_diag(dn, diet1.0)
# }
# diet1.0 <- filter_at(diet1.0, vars(contains("DIET1_monthsfromcancerdiag.")),
#                      all_vars((. > 12) | (. < 0) | is.na(.))) %>%  # No cancer diagnosis <=1 year from current age (ideally would be from remission...)
#   select(-contains("cancer"))

##### CONTINUOUS #####

clean_cont <- function (x) {
  # -10 = <1; -1 = "do not know"; -3 = "prefer not to answer"
  case_when(
    x == -10 ~ 0.5,
    is.na(x) | x == -1 | x == -3 ~ as.numeric(NA),
    TRUE ~ as.numeric(x)
  )
}

diet1.0 <- diet1.0 %>%
  mutate(
    cookedveg_TBSperday.1289.0.0_QT=clean_cont(f.1289.0.0),
    rawveg_TBSperday.1299.0.0_QT=clean_cont(f.1299.0.0),
    freshfruit_piecesperday.1309.0.0_QT=clean_cont(f.1309.0.0),
    driedfruit_piecesperday.1319.0.0_QT=clean_cont(f.1319.0.0),
    bread_slicesperweek.1438.0.0_QT=clean_cont(f.1438.0.0),
    cereal_bowlsperweek.1458.0.0_QT=clean_cont(f.1458.0.0),
    tea_cupsperday.1488.0.0_QT=clean_cont(f.1488.0.0),
    coffee_cupsperday.1498.0.0_QT=clean_cont(f.1498.0.0),
    water_glassesperday.1528.0.0_QT=clean_cont(f.1528.0.0)
  )

# for (qt in grep("_QT", names(diet1.0), value=T)) print(table(diet1.0[[qt]], useNA="always"))

##### ALCOHOL #####

alc1.0 <- alcohol_dataset %>%
  rename(id=f.eid,  # More informative names for alcohol fields
         alcohol_overallfreq.1558.0.0=f.1558.0.0,
         champagnewhitewine_glassesperweek.1578.0.0=f.1578.0.0,
         champagnewhitewine_glassespermonth.4418.0.0=f.4418.0.0,
         redwine_glassesperweek.1568.0.0=f.1568.0.0,
         redwine_glassespermonth.4407.0.0=f.4407.0.0,
         beercider_pintsperweek.1588.0.0=f.1588.0.0,
         beercider_pintspermonth.4429.0.0=f.4429.0.0,
         spirits_measuresperweek.1598.0.0=f.1598.0.0,
         spirits_measurespermonth.4440.0.0=f.4440.0.0,
         fortwine_glassesperweek.1608.0.0=f.1608.0.0,
         fortwine_glassespermonth.4451.0.0=f.4451.0.0,
         otheralcohol_glassesperweek.5364.0.0=f.5364.0.0,
         otheralcohol_glassespermonth.4462.0.0=f.4462.0.0,
         alcoholdrinkerstatus.20117.0.0=f.20117.0.0,
         alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0=f.1618.0.0) %>%
  select(id, contains("alcohol"), contains("per")) %>%
  mutate_at(vars(-id), as.numeric)

# Fields (1568 1578 1588 1598 1608) collected if drink alcohol > once or twice per week based on 1558
# table(alc1.0$alcohol_overallfreq.1558.0.0, alc1.0$champagnewhitewine_glassesperweek.1578.0.0)

# Fields (4407 4418 4429 4440 4451 4462) collected if drink alcohol > once or twice per week based on 1558
# table(alc1.0$alcohol_overallfreq.1558.0.0, alc1.0$redwine_glassespermonth.4407.0.0)

# Make one field of glasses per month based on glasses per week and glasses per month
# if 1558 in (1-2/wk, 3-4/wk, daily) -> glassesperweek * 4
# if 1558 in (1-3/mo, special occasions) -> glassespermonth
# if 1558 = Never -> 0
# if 1558 in (prefer not to answer, NA) -> MICE imputation  ### WAS THIS DONE?
# Missingness strategy:
# -1 (do not know) -> median of that group:
# if glassesperweek = -1 or -3 -> median(of those that said once or twice a week or more)

median_replace <- function(x) {
  # Replace (-1, -3) with median of the rest of the values
  ifelse(x %in% c(-1, -3), median(x[!(x %in% c(-1, -3))]), x)
}

# 1 = "Daily or ..."
# 2 = "Three or four..."
# 3 = "Once or twice..."
# 4 = "One to three..."
# 5 = "Special..."
# 6 = "Never"
# -3 = "Prefer not..."

alc1.0 <- alc1.0 %>%
  mutate(
    champagnewhitewine_glassesperweek.1578.0.0_QT=median_replace(
      champagnewhitewine_glassesperweek.1578.0.0
    ),
    champagnewhitewine_glassespermonth.4418.0.0_QT=median_replace(
      champagnewhitewine_glassespermonth.4418.0.0
    ),
    champagnewhitewine_glassespermonth.derived.0.0_QT=case_when(
      alcohol_overallfreq.1558.0.0 %in% c(1, 2, 3) ~ 4 * champagnewhitewine_glassesperweek.1578.0.0_QT,
      alcohol_overallfreq.1558.0.0 %in% c(4, 5) ~ champagnewhitewine_glassespermonth.4418.0.0_QT,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc1.0 <- alc1.0 %>%
  mutate(
    redwine_glassesperweek.1568.0.0_QT=median_replace(
      redwine_glassesperweek.1568.0.0
    ),
    redwine_glassespermonth.4407.0.0_QT=median_replace(
      redwine_glassespermonth.4407.0.0
    ),
    redwine_glassespermonth.derived.0.0_QT=case_when(
      alcohol_overallfreq.1558.0.0 %in% c(1, 2, 3) ~ 4 * redwine_glassesperweek.1568.0.0_QT,
      alcohol_overallfreq.1558.0.0 %in% c(4, 5) ~ redwine_glassespermonth.4407.0.0_QT,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc1.0 <- alc1.0 %>%
  mutate(
    beercider_pintsperweek.1588.0.0_QT=median_replace(
      beercider_pintsperweek.1588.0.0
    ),
    beercider_pintspermonth.4429.0.0_QT=median_replace(
      beercider_pintspermonth.4429.0.0
    ),
    beercider_pintspermonth.derived.0.0_QT=case_when(
      alcohol_overallfreq.1558.0.0 %in% c(1, 2, 3) ~ 4 * beercider_pintsperweek.1588.0.0_QT,
      alcohol_overallfreq.1558.0.0 %in% c(4, 5) ~ beercider_pintspermonth.4429.0.0_QT,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc1.0 <- alc1.0 %>%
  mutate(
    spirits_measuresperweek.1598.0.0_QT=median_replace(
      spirits_measuresperweek.1598.0.0
    ),
    spirits_measurespermonth.4440.0.0_QT=median_replace(
      spirits_measurespermonth.4440.0.0
    ),
    spirits_measurespermonth.derived.0.0_QT=case_when(
      alcohol_overallfreq.1558.0.0 %in% c(1, 2, 3) ~ 4 * spirits_measuresperweek.1598.0.0_QT,
      alcohol_overallfreq.1558.0.0 %in% c(4, 5) ~ spirits_measurespermonth.4440.0.0_QT,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc1.0 <- alc1.0 %>%
  mutate(
    fortwine_glassesperweek.1608.0.0_QT=median_replace(
      fortwine_glassesperweek.1608.0.0
    ),
    fortwine_glassespermonth.4451.0.0_QT=median_replace(
      fortwine_glassespermonth.4451.0.0
    ),
    fortwine_glassespermonth.derived.0.0_QT=case_when(
      alcohol_overallfreq.1558.0.0 %in% c(1, 2, 3) ~ 4 * fortwine_glassesperweek.1608.0.0_QT,
      alcohol_overallfreq.1558.0.0 %in% c(4, 5) ~ fortwine_glassespermonth.4451.0.0_QT,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc1.0 <- alc1.0 %>%
  mutate(
    otheralcohol_glassesperweek.5364.0.0_QT=median_replace(
      otheralcohol_glassesperweek.5364.0.0
    ),
    otheralcohol_glassespermonth.4462.0.0_QT=median_replace(
      otheralcohol_glassespermonth.4462.0.0
    ),
    otheralcohol_glassespermonth.derived.0.0_QT=case_when(
      alcohol_overallfreq.1558.0.0 %in% c(1, 2, 3) ~ 4 * otheralcohol_glassesperweek.5364.0.0_QT,
      alcohol_overallfreq.1558.0.0 %in% c(4, 5) ~ otheralcohol_glassespermonth.4462.0.0_QT,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

alc1.0$anyalcohol_glassespermonth.derived.0.0_QT <- rowSums(  # Glasses of ANY alcohol per month
  select(alc1.0, contains("permonth.derived.0.0_QT")), na.rm=T
)

# lapply(select(alc1.0, -id), table, useNA="always")

diet1.0 <- left_join(diet1.0, alc1.0, by="id")  # Add alcohol to main diet dataset


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

diet1.0 <- diet1.0 %>%
  mutate(
    oilyfish_overallfreq.1329.0.0_QT=cat_to_qt(f.1329.0.0),
    nonoilyfish_overallfreq.1339.0.0_QT=cat_to_qt(f.1339.0.0),
    processmeat_overallfreq.1349.0.0_QT=cat_to_qt(f.1349.0.0),
    poultry_overallfreq.1359.0.0_QT=cat_to_qt(f.1359.0.0),
    beef_overallfreq.1369.0.0_QT=cat_to_qt(f.1369.0.0),
    lambmutton_overallfreq.1379.0.0_QT=cat_to_qt(f.1379.0.0),
    pork_overallfreq.1389.0.0_QT=cat_to_qt(f.1389.0.0),
    cheese_overallfreq.1408.0.0_QT=cat_to_qt(f.1408.0.0)
  )
ctqt_numbers <- c("1329", "1339", "1349", "1359", "1369", "1379", "1389", "1408")
# for (n in ctqt_numbers) {
#   trait <- grep(paste0(n, ".0.0_QT"), names(diet1.0), value=T)
#   print(table(diet1.0[[trait]], useNA="always"))
# }

diet1.0 <- diet1.0 %>%
  mutate(doyouaddsalt.1478.0.0_QT=case_when(  # Data-coding 100394
    f.1478.0.0 == "Always" ~ 10,
    f.1478.0.0 == "Usually" ~ 7,
    f.1478.0.0 == "Sometimes" ~ 3,
    f.1478.0.0 == "Never/rarely" ~ 0,
    TRUE ~ as.numeric(NA))
  )

diet1.0 <- diet1.0 %>%
  mutate(
    hotdrinktemp.1518.0.0_QT=case_when(  # Data-coding 100398
      f.1518.0.0 == "Very hot" ~ 10,
      f.1518.0.0 == "Hot" ~ 7,
      f.1518.0.0 == "Warm" ~ 3,
      f.1518.0.0 == "Do not drink hot drinks" ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

diet1.0 <- diet1.0 %>%
  mutate(
    alcohol_overallfreq.1558.0.0_QT=case_when(  # Data-coding 100402
      alcohol_overallfreq.1558.0.0 == 1 ~ 300,
      alcohol_overallfreq.1558.0.0 == 2 ~ 182,
      alcohol_overallfreq.1558.0.0 == 3 ~ 104,
      alcohol_overallfreq.1558.0.0 == 4 ~ 24,
      alcohol_overallfreq.1558.0.0 == 5 ~ 6,
      alcohol_overallfreq.1558.0.0 == 6 ~ 0,
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

diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 90 (0=Never, 1=Previous, 2=Current, -3=Prefer not to answer)
    alcoholdrinkerstatus.20117.0.0_bin=case_when(  # Never (0) vs. Current + Previous (1)
      alcoholdrinkerstatus.20117.0.0 == 0 ~ 0,
      alcoholdrinkerstatus.20117.0.0 %in% c(1, 2) ~ 1,
      TRUE ~ as.numeric(NA)
    ),
    alcoholdrinkerstatus.20117.0.0_bin2=case_when(  # Never (0) vs. Current only (1)
      alcoholdrinkerstatus.20117.0.0 == 0 ~ 0,
      alcoholdrinkerstatus.20117.0.0 ==  2 ~ 1,
      TRUE ~ as.numeric(NA)
    ),
  )

# JBC: this will likely impute previous -> never as the correlation with 0 drinks above will be high
# that's ok. better than making a new variable for this.


### Data structure as follows:
### Data-coding 100352
### Categorical Variable with:
	# Yes
	# No
	# Prefer not to answer
	# NA
#table(diet1.0$alcohol_formerdrinker_amongcurrentnondrinkers.3731.0.0, useNA="always")
# no additional information not captured in diet1.0$alcoholdrinkerstatus.20117.0.0

alc_values <- c(1, -6, 0)
diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 100416 (1=Yes, 0=No, -6=It varies, -1=Do not know, -3=Prefer not to answer)
    alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin=make_binary_var(  # No (0) vs. Yes + It varies (1)
      alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0, c(1, -6), alc_values, c()
    ),
    alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin2=make_binary_var(  # No (0) vs. Yes (1)
      alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0, c(1), c(1, 0), c()
    ),
    alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_QT3=case_when(  # No (0), It varies (1), Yes (2)
      alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == 0 ~ 0,
      alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == -6 ~ 1,
      alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == 1 ~ 2,
      TRUE ~ as.numeric(NA)
    )
  )

diet1.0$nevereatcategories.6144.0.combined <- paste(
  diet1.0$f.6144.0.0, diet1.0$f.6144.0.1,
  diet1.0$f.6144.0.2, diet1.0$f.6144.0.3,
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

diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 100385
    nevereatcategories.6144.0.0_bin=make_binary_var(  # Eggs or foods containing eggs (1) vs. I eat all of the above (0)
      nevereatcategories.6144.0.combined, grep("Eggs", necomb_values, value=T),
      grep("Eggs|above", necomb_values, value=T), necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin2=make_binary_var(  # Eggs or foods containing eggs (1) vs. everyone else (0)
      nevereatcategories.6144.0.combined, grep("Eggs", necomb_values, value=T),
      necomb_values, necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin3=make_binary_var(  # Dairy products (1) vs. I eat all of the above (0)
      nevereatcategories.6144.0.combined, grep("Dairy", necomb_values, value=T),
      grep("Dairy|above", necomb_values, value=T), necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin4=make_binary_var(  # Dairy products (1)  vs. everyone else (0)
      nevereatcategories.6144.0.combined, grep("Dairy", necomb_values, value=T),
      necomb_values, necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin5=make_binary_var(  # Wheat products (1) vs. I eat all of the above (0)
      nevereatcategories.6144.0.combined, grep("Wheat", necomb_values, value=T),
      grep("Wheat|above", necomb_values, value=T), necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin6=make_binary_var(  # Wheat products (1)  vs. everyone else (0)
      nevereatcategories.6144.0.combined, grep("Wheat", necomb_values, value=T),
      necomb_values, necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin7=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)
      nevereatcategories.6144.0.combined, grep("Sugar", necomb_values, value=T),
      grep("Sugar|above", necomb_values, value=T), necomb_na_values
    ),
    nevereatcategories.6144.0.0_bin8=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. everyone else (0))
      nevereatcategories.6144.0.combined, grep("Sugar", necomb_values, value=T),
      necomb_values, necomb_na_values
    )
  )

diet1.0$nevereatcategories.6144.0.combined <- NULL

milk_values <- c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Never/rarely have milk")
milk_na_values <- c(general_na_values, "Other type of milk")

diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 100387
    milk_typeused.1418.0.0_bin=make_binary_var(  # 1binary: milk x3 vs. never
      diet1.0$f.1418.0.0,
      c("Full cream", "Semi-skimmed", "Skimmed"), milk_values, milk_na_values
    ),
    milk_typeused.1418.0.0_bin2=make_binary_var(  # 2binary: milk x5 vs. never
      diet1.0$f.1418.0.0,
      c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Other type of milk"),
      milk_values, milk_na_values
    ),
    milk_typeused.1418.0.0_QT3=case_when(  # 3continuous: never (0), skimmed (1), semi-skimmed (2), full cream (3)
      f.1418.0.0 == "Full cream" ~ 3,
      f.1418.0.0 == "Semi-skimmed" ~ 2,
      f.1418.0.0 == "Skimmed" ~ 1,
      f.1418.0.0 == "Never/rarely have milk" ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    milk_typeused.1418.0.0_bin4=make_binary_var(  # 4full cream (1) vs. never (0)
      diet1.0$f.1418.0.0,
      "Full cream", c("Full cream", "Never/rarely have milk"), milk_na_values
    ),
    milk_typeused.1418.0.0_bin5=make_binary_var(  # 5full cream (1) vs. all other milk (0)
      diet1.0$f.1418.0.0, "Full cream", milk_values, milk_na_values
    ),
    milk_typeused.1418.0.0_bin6=make_binary_var(  # 6semi (1) vs. never (0)
      diet1.0$f.1418.0.0,
      "Semi-skimmed", c("Semi-skimmed", "Never/rarely have milk"), milk_na_values
    ),
    milk_typeused.1418.0.0_bin7=make_binary_var(  # 7semi (1) vs. all other milk (0)
      diet1.0$f.1418.0.0, "Semi-skimmed", milk_values, milk_na_values
    ),
    milk_typeused.1418.0.0_bin8=make_binary_var(  # 8skim (1) vs. never (0) 
      diet1.0$f.1418.0.0,
      "Skimmed", c("Skimmed", "Never/rarely have milk"), milk_na_values
    ),
    milk_typeused.1418.0.0_bin9=make_binary_var(  # 9skim (1) vs. all other milk (0)
      diet1.0$f.1418.0.0, "Skimmed", milk_values, milk_na_values
    ),
    milk_typeused.1418.0.0_bin10=make_binary_var(  # 10soy (1) vs. never (0)
      diet1.0$f.1418.0.0,
      "Soya", c("Soya", "Never/rarely have milk"), milk_na_values
    ),
    milk_typeused.1418.0.0_bin11=make_binary_var(  # 11soy (1) vs. all other milk (0)
      diet1.0$f.1418.0.0,
      "Soya", milk_values, milk_na_values
    ),
    milk_typeused.1418.0.0_bin12=make_binary_var(  # 12other (1) vs. never (0)
      diet1.0$f.1418.0.0,
      "Other type of milk", c("Other type of milk", "Never/rarely have milk"), milk_na_values
    ),
    milk_typeused.1418.0.0_bin13=make_binary_var(  # 13other (1) vs. all other milk (0)
      diet1.0$f.1418.0.0, "Other type of milk", milk_values, milk_na_values
    ),
    
    
    milk_typeused.1418.0.0_QT14=case_when(  # 14continuous: skimmed (0), semi-skimmed (1), full cream (2)
      f.1418.0.0 == "Full cream" ~ 2,
      f.1418.0.0 == "Semi-skimmed" ~ 1,
      f.1418.0.0 == "Skimmed" ~ 0,
      TRUE ~ as.numeric(NA)
    )
  )

# Data-coding 100388 (butter type)
# Data-coding 100389 (alternate spread type)
# Field 2654 was collected if a non-butter spread was indicated for 1428

diet1.0 <- diet1.0 %>%
  mutate(spread_typeused.1428.0.nonbutterspread_typeused.2654.combined=paste(
    f.1428.0.0, f.2654.0.0, sep=":"
  ))
# table(diet1.0$spread_typeused.1428.0.0,diet1.0$nonbutterspread_typeused.2654.0.0)
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined, useNA="always")

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

diet1.0 <- diet1.0 %>%
  mutate(
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1=make_binary_var(  #1 All vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      setdiff(butter_values, "Never/rarely use spread:NA"), butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2=make_binary_var(  #2 Butter/Margarine vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Butter|Hard|Soft|Flora Pro", butter_values, value=T),
      grep("Butter|Hard|Soft|Flora Pro|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3=make_binary_var(  #3 Oil-based vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("oil", butter_values, value=T),
      grep("oil|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4=make_binary_var(  #4 Butter (with flora proactive) vs. Oil
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Butter|Hard|Soft|Benecol", butter_values, value=T),
      grep("Butter|Hard|Soft|Benecol|oil", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5=make_binary_var(  #5 Butter (without flora proactive)vs. Oil
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Butter|Hard|Soft", butter_values, value=T),
      grep("Butter|Hard|Soft|oil", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6=make_binary_var(  #6 Butter vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      "Butter/spreadable butter:NA",
      c("Butter/spreadable butter:NA", "Never/rarely use spread:NA"),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7=make_binary_var(  #7 Butter vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      "Butter/spreadable butter:NA", butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8=make_binary_var(  #8 Hard (block) margarine vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Hard", butter_values, value=T),
      grep("Hard|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9=make_binary_var(  #9 Hard (block) margarine vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Hard", butter_values, value=T), butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10=make_binary_var(  #10 Soft (tub) margarine vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Soft", butter_values, value=T),
      grep("Soft|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11=make_binary_var(  #11 Soft (tub) margarine vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Soft", butter_values, value=T), butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12=make_binary_var(  #12 Flora vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Benecol", butter_values, value=T),
      grep("Benecol|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13=make_binary_var(  #13 Flora vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Benecol", butter_values, value=T), butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14=make_binary_var(  #14 Olive oil based spread (eg: Bertolli) vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Olive", butter_values, value=T),
      grep("Olive|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15=make_binary_var(  #15 Olive oil based spread (eg: Bertolli) vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("Olive", butter_values, value=T), butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16=make_binary_var(  #16 Flora oil/sunflower oil vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("sunflower", butter_values, value=T),
      grep("sunflower|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17=make_binary_var(  #17 Flora oil/sunflower oil vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("sunflower", butter_values, value=T), butter_values, butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18=make_binary_var(  #18 Other low or reduced fat spread vs. Never
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("reduced", butter_values, value=T),
      grep("reduced|Never", butter_values, value=T),
      butter_na_values
    ),
    spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19=make_binary_var(  #19 Other low or reduced fat spread vs. Other
      spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
      grep("reduced", butter_values, value=T), butter_values, butter_na_values
    )
  )

# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18, useNA="always")
# table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19, useNA="always")

diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined <- NULL


# Field 1448 was collected if eat bread < 1 slice/wk based on 1438
bread_values <- c("White", "Brown", "Wholemeal or wholegrain", "Other type of bread")
diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 100391
    bread_typeused.1448.0.0_bin=make_binary_var(  #1 White vs Other
      f.1448.0.0, "White", bread_values, general_na_values
    ),
    bread_typeused.1448.0.0_bin2=make_binary_var(  #2 Brown vs. Other
      f.1448.0.0, "Brown", bread_values, general_na_values
    ),
    bread_typeused.1448.0.0_bin3=make_binary_var(  # Wholemeal vs. Other
      f.1448.0.0, "Wholemeal or wholegrain", bread_values, general_na_values
    ),
    bread_typeused.1448.0.0_bin4=make_binary_var(  #5 White vs. Brown + Wholemeal
      f.1448.0.0, c("White"),
      c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
    ),
    bread_typeused.1448.0.0_bin5=make_binary_var(  #5 White + Brown vs. Wholemeal
      f.1448.0.0, c("White", "Brown"),
      c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
    )
  )

# Field 1468 was collected if eat cereal < 1 bowl/wk based on 1458
cereal_values <- c(
  "Biscuit cereal (e.g. Weetabix)", "Bran cereal (e.g. All Bran, Branflakes)",
  "Oat cereal (e.g. Ready Brek, porridge)", "Muesli", "Other (e.g. Cornflakes, Frosties)"
)
diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 100393
    cereal_typeused.1468.0.0_bin=make_binary_var(  # Biscuit vs. All Other
      f.1468.0.0,
      "Biscuit cereal (e.g. Weetabix)", cereal_values, general_na_values
    ),
    cereal_typeused.1468.0.0_bin2=make_binary_var(  # Bran vs. All Other
      f.1468.0.0,
      "Bran cereal (e.g. All Bran, Branflakes)", cereal_values, general_na_values
    ),
    cereal_typeused.1468.0.0_bin3=make_binary_var(  # Oat vs. All Other
      f.1468.0.0,
      "Oat cereal (e.g. Ready Brek, porridge)", cereal_values, general_na_values
    ),
    cereal_typeused.1468.0.0_bin4=make_binary_var(  # Muesli vs. All Other
      f.1468.0.0,
      "Muesli", cereal_values, general_na_values
    ),
    cereal_typeused.1468.0.0_bin5=make_binary_var(  # Other vs. All Other
      f.1468.0.0,
      "Other (e.g. Cornflakes, Frosties)", cereal_values, general_na_values
    )
  )


# Field 1508 was collected if drink coffee < 1 cup/day or at least 1 cup/day based on 1498
coffee_values <- c(
  "Ground coffee (include espresso, filter etc)", "Instant coffee",
  "Decaffeinated coffee (any type)", "Other type of coffee"
)
coffee_NA_values <- c("Do not know", "Prefer not to answer", "NA")
diet1.0 <- diet1.0 %>%
  mutate(  # Data-coding 100397
    coffee_typeused.1508.0.0_bin=make_binary_var(  # Ground vs. Other
      f.1508.0.0,
      "Ground coffee (include espresso, filter etc)", coffee_values, general_na_values
    ),
    coffee_typeused.1508.0.0_bin2=make_binary_var(  # instant_ground vs. other
      f.1508.0.0,
      c("Ground coffee (include espresso, filter etc)", "Instant coffee"),
      coffee_values, general_na_values
    ),
    coffee_typeused.1508.0.0_bin3=make_binary_var(
      f.1508.0.0,
      "Decaffeinated coffee (any type)", coffee_values, general_na_values
    )
  )


##### SAVE #####

keep_cols_1.0 <- names(diet1.0)[!grepl("^f\\.", names(diet1.0))]
save_diet1.0 = diet1.0[, keep_cols_1.0]
write_tsv(save_diet1.0, "../data/processed/ukbiobank_diet1.0")
saveRDS(save_diet1.0, "../data/processed/ukbiobank_diet1.0.rds")


##############################################
########## COMBINE ALL DIETARY DATA ##########
##############################################

diet_BL <- readRDS("../data/processed/ukbiobank_diet1.0.rds")  # Baseline dietary data
names(diet_BL) <- gsub("\\.0\\.0_", "_", names(diet_BL))  # Remove visit ID component from field name
diet_BL <- diet_BL %>%
  rename(age_months=DIET1_age_months.0.0) %>%
  left_join(select(base_dataset, id=f.eid, selfreportsex.31=f.31.0.0), by="id") %>%
  mutate(id=format(id, scientific=F))

pca_fields_jbc <- c(
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
"champagnewhitewine_glassespermonth.derived_QT",
"redwine_glassespermonth.derived_QT",
"beercider_pintspermonth.derived_QT",
"spirits_measurespermonth.derived_QT",
"fortwine_glassespermonth.derived_QT",
"otheralcohol_glassespermonth.derived_QT",
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
"alcohol_overallfreq.1558_QT",
"alcoholdrinkerstatus.20117_bin",
"alcoholdrinkerstatus.20117_bin2",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618_bin",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618_bin2",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618_QT3",
"nevereatcategories.6144_bin",
"nevereatcategories.6144_bin2",
"nevereatcategories.6144_bin3",
"nevereatcategories.6144_bin4",
"nevereatcategories.6144_bin5",
"nevereatcategories.6144_bin6",
"nevereatcategories.6144_bin7",
"nevereatcategories.6144_bin8",
"milk_typeused.1418_bin",
"milk_typeused.1418_bin2",
"milk_typeused.1418_QT3",
"milk_typeused.1418_bin4",
"milk_typeused.1418_bin5",
"milk_typeused.1418_bin6",
"milk_typeused.1418_bin7",
"milk_typeused.1418_bin8",
"milk_typeused.1418_bin9",
"milk_typeused.1418_bin10",
"milk_typeused.1418_bin11",
"milk_typeused.1418_bin12",
"milk_typeused.1418_bin13",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18",
"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19",
"bread_typeused.1448_bin",
"bread_typeused.1448_bin2",
"bread_typeused.1448_bin3",
"bread_typeused.1448_bin4",
"bread_typeused.1448_bin5",
"cereal_typeused.1468_bin",
"cereal_typeused.1468_bin2",
"cereal_typeused.1468_bin3",
"cereal_typeused.1468_bin4",
"cereal_typeused.1468_bin5",
"coffee_typeused.1508_bin",
"coffee_typeused.1508_bin2",
"coffee_typeused.1508_bin3"
)

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

write_tsv(select(diet_BL, pca_fields_subset), "../data/processed/diet_baseline.txt")

write_tsv(select(diet_BL, pca_fields_jbc), "../data/processed/diet_baseline_jbcFields.txt")

#########################################################################################################
#########################################################################################################
################### FFQ Diet Phenotypes - Florez Application
################### STEP 2: Filter, adjust and transform
########## Original code last updated: July 6, 2018 (JBC)
########## Update for reformatting: February 2019 (KEW)
#########################################################################################################
#########################################################################################################

# Load in phenotype file:
diet <- read_tsv("../data/processed/diet_baseline.txt")

diet_jbcFields <- read_tsv("../data/processed/diet_baseline_jbcFields.txt")

#### FILTERING ####

ids_to_include <- scan("../data/processed/ids_to_include.txt", what=integer())

diet <- filter(diet, id %in% ids_to_include) %>%
  rename(sex=selfreportsex.31)

diet_jbcFields <- filter(diet_jbcFields, id %in% ids_to_include) %>%
  rename(sex=selfreportsex.31)

#### ADJUSTMENTS ####

INT <- function(x) qnorm((rank(x, na.last="keep") - 0.5) / sum(!is.na(x)))

diet_adj <- diet %>%
  mutate_at(vars(contains("_QT")), INT)

calc_age_sex_resids_INT_QT <- function(x) {
  lm_fit <- lm(
    as.numeric(x) ~ age_months + sex, data=diet_jbcFields,
    na.action=na.exclude
  )
  lm_resids <- resid(lm_fit)
  qnorm((rank(lm_resids, na.last="keep") - 0.5) / sum(!is.na(lm_resids)))
}

calc_age_sex_resids_binary <- function(x) {
  glm_fit <- glm(
    as.numeric(x) ~ age_months + sex, data=diet_jbcFields,
    family=binomial, na.action=na.exclude
  )
  resid(glm_fit)
}

diet_jbcFields_adj <- diet_jbcFields %>%
  mutate_at(vars(contains("_QT")), calc_age_sex_resids_INT_QT) %>%
  mutate_at(vars(contains("_bin")), calc_age_sex_resids_binary)

#### SAVE ####

write_tsv(diet_adj, "../data/processed/final_baseline_ffq_data.txt")

write_tsv(diet_jbcFields_adj, "../data/processed/final_baseline_ffq_data_jbcFields.txt")

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
################### FFQ Diet Phenotypes - Florez Application
################### STEP3: PCA
########## Original code last updated: November 2018 (JBC)
########## Update for reformatting: February 2020 (KEW)
######################################################################################################################################

pca_input <- read_tsv("../data/processed/final_baseline_ffq_data.txt") %>%
  select(-sex, -age_months) %>%
  filter_at(vars(-id), any_vars(!is.na(.))) %>%  # Remove rows that have all diet variables missing
  mutate_at(vars(-id), function(x) ifelse(is.na(x), median(x, na.rm=T), x))  # Median impute

ffq_pca_fit <- prcomp(select(pca_input, -id), scale.=T)  # Run PCA
saveRDS(ffq_pca_fit, "../data/processed/ffq_pca_fit.rds")

save_PCs <- cbind(pca_input$id, ffq_pca_fit$x)
colnames(save_PCs) <- c("id", paste0("ffq_", colnames(ffq_pca_fit$x)))
write_tsv(as_tibble(save_PCs), "../data/processed/ffq_PCs.txt")



pca_input_jbcFields <- read_tsv("../data/processed/final_baseline_ffq_data_jbcFields.txt") %>%
  select(-sex, -age_months) %>%
  filter_at(vars(-id), any_vars(!is.na(.))) %>%  # Remove rows that have all diet variables missing
  mutate_at(vars(-id), function(x) ifelse(is.na(x), median(x, na.rm=T), x))  # Median impute

ffq_pca_fit_jbcFields <- prcomp(select(pca_input_jbcFields, -id), scale.=T)  # Run PCA
saveRDS(ffq_pca_fit_jbcFields, "../data/processed/ffq_pca_fit_jbcFields.rds")

save_PCs_jbcFields <- cbind(pca_input_jbcFields$id, ffq_pca_fit_jbcFields$x)
colnames(save_PCs_jbcFields) <- c("id", paste0("ffq_", colnames(ffq_pca_fit_jbcFields$x)))
write_tsv(as_tibble(save_PCs_jbcFields), "../data/processed/ffq_PCs_jbcFields.txt")








# ##############################
# ########## DIET 1.1 ##########
# ##############################
# 
# diet1.1 <- base_dataset %>%
#   rename(id=f.eid,
#          year_of_birth.34=f.34.0.0,
#          month_of_birth.52=f.52.0.0,
#          date_assessmentcentre.53.1.0=f.53.1.0)
# # names(diet1.1) <- with(col_dict, {
# #   # raw_names <- paste("f", fieldID, "0.0", sep=".")
# #   # informative_names <- paste(name, fieldID, "0.0", sep=".")
# #   ifelse(names(diet1.1) %in% raw_1.0,
# #          informative_1.0[match(names(diet1.1), raw_1.0)],
# #          names(diet1.1))
# # })
# # diet1.1 <- select(diet1.1, which(!grepl("^f\\.", names(diet1.1))))
# 
# ### DATES
# # Create appropriate age variables per instance
# # DIET1: date_assessmentcentre.53.1.0 -> birth year + birth month
# 
# date1.0 <- str_split_fixed(diet1.1$date_assessmentcentre.53.1.0, "-", 3)
# date1.0 <- apply(date1.0, 2, as.numeric)
# colnames(date1.0) <- c("DIET1_date_year.1.0", "DIET1_date_month.1.0", "DIET1_date_day.1.0")
# diet1.1 <- cbind(diet1.1, date1.0)
# 
# date_cols <- c("DIET1_date_year.1.0", "DIET1_date_month.1.0", "DIET1_date_day.1.0")
# diet1.1 <- diet1.1 %>%
#   separate(date_assessmentcentre.53.1.0, date_cols, "-") %>% # Split DATE into 3 columns (yr/mo/day)
#   mutate_at(date_cols, as.numeric) %>%
#   mutate(month_of_birth_numeric.52=match(month_of_birth.52, month.name),  # Convert month in text -> numeric
#          DIET1_age_months.1.0=(DIET1_date_year.1.0 - year_of_birth.34) * 12 +  # Create age variable
#            (DIET1_date_month.1.0 - month_of_birth_numeric.52))
# 
# ##### CONTINUOUS #####
# 
# clean_cont <- function (x) {
#   # -10 = <1; -1 = "do not know"; -3 = "prefer not to answer"
#   case_when(
#     x == -10 ~ 0.5,
#     is.na(x) | x == -1 | x == -3 ~ as.numeric(NA),
#     TRUE ~ as.numeric(x)
#   )
# }
# 
# diet1.1 <- diet1.1 %>%
#   mutate(
#     cookedveg_TBSperday.1289.1.0_QT=clean_cont(f.1289.1.0),
#     rawveg_TBSperday.1299.1.0_QT=clean_cont(f.1299.1.0),
#     freshfruit_piecesperday.1309.1.0_QT=clean_cont(f.1309.1.0),
#     driedfruit_piecesperday.1319.1.0_QT=clean_cont(f.1319.1.0),
#     bread_slicesperweek.1438.1.0_QT=clean_cont(f.1438.1.0),
#     cereal_bowlsperweek.1458.1.0_QT=clean_cont(f.1458.1.0),
#     tea_cupsperday.1488.1.0_QT=clean_cont(f.1488.1.0),
#     coffee_cupsperday.1498.1.0_QT=clean_cont(f.1498.1.0),
#     water_glassesperday.1528.1.0_QT=clean_cont(f.1528.1.0)
#   )
# 
# # for (qt in grep("_QT", names(diet1.1), value=T)) print(table(diet1.1[[qt]], useNA="always"))
# 
# ##### ALCOHOL #####
# 
# alc1.1 <- alcohol_dataset %>%
#   rename(id=f.eid,  # More informative names for alcohol fields
#          alcohol_overallfreq.1558.1.0=f.1558.1.0,
#          champagnewhitewine_glassesperweek.1578.1.0=f.1578.1.0,
#          champagnewhitewine_glassespermonth.4418.1.0=f.4418.1.0,
#          redwine_glassesperweek.1568.1.0=f.1568.1.0,
#          redwine_glassespermonth.4407.1.0=f.4407.1.0,
#          beercider_pintsperweek.1588.1.0=f.1588.1.0,
#          beercider_pintspermonth.4429.1.0=f.4429.1.0,
#          spirits_measuresperweek.1598.1.0=f.1598.1.0,
#          spirits_measurespermonth.4440.1.0=f.4440.1.0,
#          fortwine_glassesperweek.1608.1.0=f.1608.1.0,
#          fortwine_glassespermonth.4451.1.0=f.4451.1.0,
#          otheralcohol_glassesperweek.5364.1.0=f.5364.1.0,
#          otheralcohol_glassespermonth.4462.1.0=f.4462.1.0,
#          alcoholdrinkerstatus.20117.1.0=f.20117.1.0,
#          alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0=f.1618.1.0) %>%
#   select(id, contains("alcohol"), contains("per")) %>%
#   mutate_at(vars(-id), as.numeric)
# 
# # Fields (1568 1578 1588 1598 1608) collected if drink alcohol > once or twice per week based on 1558
# # table(alc1.1$alcohol_overallfreq.1558.1.0, alc1.1$champagnewhitewine_glassesperweek.1578.1.0)
# 
# # Fields (4407 4418 4429 4440 4451 4462) collected if drink alcohol > once or twice per week based on 1558
# # table(alc1.1$alcohol_overallfreq.1558.1.0, alc1.1$redwine_glassespermonth.4407.1.0)
# 
# # Make one field of glasses per month based on glasses per week and glasses per month
# # if 1558 in (1-2/wk, 3-4/wk, daily) -> glassesperweek * 4
# # if 1558 in (1-3/mo, special occasions) -> glassespermonth
# # if 1558 = Never -> 0
# # if 1558 in (prefer not to answer, NA) -> MICE imputation  ### WAS THIS DONE?
# # Missingness strategy:
# # -1 (do not know) -> median of that group:
# # if glassesperweek = -1 or -3 -> median(of those that said once or twice a week or more)
# 
# median_replace <- function(x) {
#   # Replace (-1, -3) with median of the rest of the values
#   ifelse(x %in% c(-1, -3), median(x[!(x %in% c(-1, -3))]), x)
# }
# 
# # 1 = "Daily or ..."
# # 2 = "Three or four..."
# # 3 = "Once or twice..."
# # 4 = "One to three..."
# # 5 = "Special..."
# # 6 = "Never"
# # -3 = "Prefer not..."
# 
# alc1.1 <- alc1.1 %>%
#   mutate(
#     champagnewhitewine_glassesperweek.1578.1.0_QT=median_replace(
#       champagnewhitewine_glassesperweek.1578.1.0
#     ),
#     champagnewhitewine_glassespermonth.4418.1.0_QT=median_replace(
#       champagnewhitewine_glassespermonth.4418.1.0
#     ),
#     champagnewhitewine_glassespermonth.derived.1.0_QT=case_when(
#       alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * champagnewhitewine_glassesperweek.1578.1.0_QT,
#       alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ champagnewhitewine_glassespermonth.4418.1.0_QT,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.1 <- alc1.1 %>%
#   mutate(
#     redwine_glassesperweek.1568.1.0_QT=median_replace(
#       redwine_glassesperweek.1568.1.0
#     ),
#     redwine_glassespermonth.4407.1.0_QT=median_replace(
#       redwine_glassespermonth.4407.1.0
#     ),
#     redwine_glassespermonth.derived.1.0_QT=case_when(
#       alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * redwine_glassesperweek.1568.1.0_QT,
#       alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ redwine_glassespermonth.4407.1.0_QT,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.1 <- alc1.1 %>%
#   mutate(
#     beercider_pintsperweek.1588.1.0_QT=median_replace(
#       beercider_pintsperweek.1588.1.0
#     ),
#     beercider_pintspermonth.4429.1.0_QT=median_replace(
#       beercider_pintspermonth.4429.1.0
#     ),
#     beercider_pintspermonth.derived.1.0_QT=case_when(
#       alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * beercider_pintsperweek.1588.1.0_QT,
#       alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ beercider_pintspermonth.4429.1.0_QT,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.1 <- alc1.1 %>%
#   mutate(
#     spirits_measuresperweek.1598.1.0_QT=median_replace(
#       spirits_measuresperweek.1598.1.0
#     ),
#     spirits_measurespermonth.4440.1.0_QT=median_replace(
#       spirits_measurespermonth.4440.1.0
#     ),
#     spirits_measurespermonth.derived.1.0_QT=case_when(
#       alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * spirits_measuresperweek.1598.1.0_QT,
#       alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ spirits_measurespermonth.4440.1.0_QT,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.1 <- alc1.1 %>%
#   mutate(
#     fortwine_glassesperweek.1608.1.0_QT=median_replace(
#       fortwine_glassesperweek.1608.1.0
#     ),
#     fortwine_glassespermonth.4451.1.0_QT=median_replace(
#       fortwine_glassespermonth.4451.1.0
#     ),
#     fortwine_glassespermonth.derived.1.0_QT=case_when(
#       alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * fortwine_glassesperweek.1608.1.0_QT,
#       alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ fortwine_glassespermonth.4451.1.0_QT,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.1 <- alc1.1 %>%
#   mutate(
#     otheralcohol_glassesperweek.5364.1.0_QT=median_replace(
#       otheralcohol_glassesperweek.5364.1.0
#     ),
#     otheralcohol_glassespermonth.4462.1.0_QT=median_replace(
#       otheralcohol_glassespermonth.4462.1.0
#     ),
#     otheralcohol_glassespermonth.derived.1.0_QT=case_when(
#       alcohol_overallfreq.1558.1.0 %in% c(1, 2, 3) ~ 4 * otheralcohol_glassesperweek.5364.1.0_QT,
#       alcohol_overallfreq.1558.1.0 %in% c(4, 5) ~ otheralcohol_glassespermonth.4462.1.0_QT,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.1$anyalcohol_glassespermonth.derived.1.0_QT <- rowSums(  # Glasses of ANY alcohol per month
#   select(alc1.1, contains("permonth.derived.1.0_QT")), na.rm=T
# )
# 
# # lapply(select(alc1.1, -id), table, useNA="always")
# 
# diet1.1 <- left_join(diet1.1, alc1.1, by="id")  # Add alcohol to main diet dataset
# 
# 
# ##### CATEGORICAL TO CONTINUOUS #####
# 
# cat_to_qt <- function(x) {
#   case_when(  # Data-coding 100377
#     x == "Once or more daily" ~ 365,
#     x == "5-6 times a week" ~ 286,
#     x == "2-4 times a week" ~ 156,
#     x == "Once a week" ~ 52,
#     x == "Less than once a week" ~ 20,
#     x == "Never" ~ 0,
#     TRUE ~ as.numeric(NA)
#   )
# }
# 
# diet1.1 <- diet1.1 %>%
#   mutate(
#     oilyfish_overallfreq.1329.1.0_QT=cat_to_qt(f.1329.1.0),
#     nonoilyfish_overallfreq.1339.1.0_QT=cat_to_qt(f.1339.1.0),
#     processmeat_overallfreq.1349.1.0_QT=cat_to_qt(f.1349.1.0),
#     poultry_overallfreq.1359.1.0_QT=cat_to_qt(f.1359.1.0),
#     beef_overallfreq.1369.1.0_QT=cat_to_qt(f.1369.1.0),
#     lambmutton_overallfreq.1379.1.0_QT=cat_to_qt(f.1379.1.0),
#     pork_overallfreq.1389.1.0_QT=cat_to_qt(f.1389.1.0),
#     cheese_overallfreq.1408.1.0_QT=cat_to_qt(f.1408.1.0)
#   )
# ctqt_numbers <- c("1329", "1339", "1349", "1359", "1369", "1379", "1389", "1408")
# # for (n in ctqt_numbers) {
# #   trait <- grep(paste0(n, ".1.0_QT"), names(diet1.1), value=T)
# #   print(table(diet1.1[[trait]], useNA="always"))
# # }
# 
# diet1.1 <- diet1.1 %>%
#   mutate(doyouaddsalt.1478.1.0_QT=case_when(  # Data-coding 100394
#     f.1478.1.0 == "Always" ~ 10,
#     f.1478.1.0 == "Usually" ~ 7,
#     f.1478.1.0 == "Sometimes" ~ 3,
#     f.1478.1.0 == "Never/rarely" ~ 0,
#     TRUE ~ as.numeric(NA))
#   )
# # table(diet1.1$f.1478.1.0, useNA="always")
# # table(diet1.1$doyouaddsalt.1478.1.0_QT, useNA="always")
# 
# diet1.1 <- diet1.1 %>%
#   mutate(
#     hotdrinktemp.1518.1.0_QT=case_when(  # Data-coding 100398
#       f.1518.1.0 == "Very hot" ~ 10,
#       f.1518.1.0 == "Hot" ~ 7,
#       f.1518.1.0 == "Warm" ~ 3,
#       f.1518.1.0 == "Do not drink hot drinks" ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# # table(diet1.1$f.1518.1.0, useNA="always")
# # table(diet1.1$hotdrinktemp.1518.1.0_QT, useNA="always")
# 
# diet1.1 <- diet1.1 %>%
#   mutate(
#     alcohol_overallfreq.1558.1.0_QT=case_when(  # Data-coding 100402
#       alcohol_overallfreq.1558.1.0 == 1 ~ 300,
#       alcohol_overallfreq.1558.1.0 == 2 ~ 182,
#       alcohol_overallfreq.1558.1.0 == 3 ~ 104,
#       alcohol_overallfreq.1558.1.0 == 4 ~ 24,
#       alcohol_overallfreq.1558.1.0 == 5 ~ 6,
#       alcohol_overallfreq.1558.1.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# # table(diet1.1$alcohol_overallfreq.1558.1.0, useNA="always")
# # table(diet1.1$alcohol_overallfreq.1558.1.0_QT, useNA="always")
# 
# ##### MISC #####
# 
# make_binary_var <- function(x, yes_values, all_values, na_values) {
#   # Given a categorical vector, transform into a binary variable based on inputs
#   # defining positive values, all values, and values to leave or set as missing
#   case_when(
#     x %in% yes_values ~ 1,
#     x %in% setdiff(all_values, yes_values) ~ 0,
#     x %in% na_values ~ as.numeric(NA),
#     TRUE ~ as.numeric(NA)
#   )
# }
# 
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 90 (0=Never, 1=Previous, 2=Current, -3=Prefer not to answer)
#     alcoholdrinkerstatus.20117.1.0_bin=case_when(  # Never (0) vs. Current + Previous (1)
#       alcoholdrinkerstatus.20117.1.0 == 0 ~ 0,
#       alcoholdrinkerstatus.20117.1.0 %in% c(1, 2) ~ 1,
#       TRUE ~ as.numeric(NA)
#     ),
#     alcoholdrinkerstatus.20117.1.0_bin2=case_when(  # Never (0) vs. Current only (1)
#       alcoholdrinkerstatus.20117.1.0 == 0 ~ 0,
#       alcoholdrinkerstatus.20117.1.0 ==  2 ~ 1,
#       TRUE ~ as.numeric(NA)
#     ),
#   )
# 
# # table(diet1.1$alcoholdrinkerstatus.20117.1.0, useNA="always")
# # table(diet1.1$alcoholdrinkerstatus.20117.1.0_bin, useNA="always")
# # table(diet1.1$alcoholdrinkerstatus.20117.1.0_bin2, useNA="always")
# # JBC: this will likely impute previous -> never as the correlation with 0 drinks above will be high
# # that's ok. better than making a new variable for this.
# 
# 
# ### Data structure as follows:
# ### Data-coding 100352
# ### Categorical Variable with:
# # Yes
# # No
# # Prefer not to answer
# # NA
# #table(diet1.1$alcohol_formerdrinker_amongcurrentnondrinkers.3731.1.0, useNA="always")
# # no additional information not captured in diet1.1$alcoholdrinkerstatus.20117.1.0
# 
# alc_values <- c(1, -6, 0)
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 100416 (1=Yes, 0=No, -6=It varies, -1=Do not know, -3=Prefer not to answer)
#     alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin=make_binary_var(  # No (0) vs. Yes + It varies (1)
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0, c(1, -6), alc_values, c()
#     ),
#     alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin2=make_binary_var(  # No (0) vs. Yes (1)
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0, c(1), c(1, 0), c()
#     ),
#     alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_QT3=case_when(  # No (0), It varies (1), Yes (2)
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == 0 ~ 0,
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == -6 ~ 1,
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == 1 ~ 2,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# # table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0, useNA="always")
# # table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin, useNA="always")
# # table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin2, useNA="always")
# # table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_QT3, useNA="always")
# 
# diet1.1$nevereatcategories.6144.0.combined <- paste(
#   diet1.1$f.6144.1.0, diet1.1$f.6144.0.1,
#   diet1.1$f.6144.0.2, diet1.1$f.6144.0.3,
#   sep =":"
# )
# # table(diet1.1$nevereatcategories.6144.0.combined, useNA="always")
# 
# necomb_values <- c(
#   "Dairy products:NA:NA:NA", "Dairy products:Sugar or foods/drinks containing sugar:NA:NA",
#   "Dairy products:Wheat products:NA:NA", "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA",
#   "Eggs or foods containing eggs:Dairy products:NA:NA",
#   "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA",
#   "Eggs or foods containing eggs:Dairy products:Wheat products:NA",
#   "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar",
#   "Eggs or foods containing eggs:NA:NA:NA",
#   "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA",
#   "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA",
#   "Eggs or foods containing eggs:Wheat products:NA:NA",
#   "I eat all of the above:NA:NA:NA", "Sugar or foods/drinks containing sugar:NA:NA:NA",
#   "Wheat products:NA:NA:NA", "Wheat products:Sugar or foods/drinks containing sugar:NA:NA"
# )
# 
# general_na_values <- c("Do not know", "Prefer not to answer", "NA")
# 
# necomb_na_values <- c(
#   general_na_values, "NA:NA:NA:NA", "Prefer not to answer:NA:NA:NA"
# )
# 
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 100385
#     nevereatcategories.6144.1.0_bin=make_binary_var(  # Eggs or foods containing eggs (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Eggs", necomb_values, value=T),
#       grep("Eggs|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin2=make_binary_var(  # Eggs or foods containing eggs (1) vs. everyone else (0)
#       nevereatcategories.6144.0.combined, grep("Eggs", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin3=make_binary_var(  # Dairy products (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Dairy", necomb_values, value=T),
#       grep("Dairy|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin4=make_binary_var(  # Dairy products (1)  vs. everyone else (0)
#       nevereatcategories.6144.0.combined, grep("Dairy", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin5=make_binary_var(  # Wheat products (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Wheat", necomb_values, value=T),
#       grep("Wheat|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin6=make_binary_var(  # Wheat products (1)  vs. everyone else (0)
#       nevereatcategories.6144.0.combined, grep("Wheat", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin7=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Sugar", necomb_values, value=T),
#       grep("Sugar|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.1.0_bin8=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. everyone else (0))
#       nevereatcategories.6144.0.combined, grep("Sugar", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     )
#   )
# 
# # table(diet1.1$f.6144.1.0, useNA="always")
# # table(diet1.1$f.6144.0.1, useNA="always")
# # table(diet1.1$f.6144.0.2, useNA="always")
# # table(diet1.1$f.6144.0.3, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin2, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin3, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin4, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin5, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin6, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin7, useNA="always")
# # table(diet1.1$nevereatcategories.6144.1.0_bin8, useNA="always")
# 
# diet1.1$nevereatcategories.6144.0.combined <- NULL
# 
# milk_values <- c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Never/rarely have milk")
# milk_na_values <- c(general_na_values, "Other type of milk")
# 
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 100387
#     milk_typeused.1418.1.0_bin=make_binary_var(  # 1binary: milk x3 vs. never
#       diet1.1$f.1418.1.0,
#       c("Full cream", "Semi-skimmed", "Skimmed"), milk_values, milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin2=make_binary_var(  # 2binary: milk x5 vs. never
#       diet1.1$f.1418.1.0,
#       c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Other type of milk"),
#       milk_values, milk_na_values
#     ),
#     milk_typeused.1418.1.0_QT3=case_when(  # 3continuous: never (0), skimmed (1), semi-skimmed (2), full cream (3)
#       f.1418.1.0 == "Full cream" ~ 3,
#       f.1418.1.0 == "Semi-skimmed" ~ 2,
#       f.1418.1.0 == "Skimmed" ~ 1,
#       f.1418.1.0 == "Never/rarely have milk" ~ 0,
#       TRUE ~ as.numeric(NA)
#     ),
#     milk_typeused.1418.1.0_bin4=make_binary_var(  # 4full cream (1) vs. never (0)
#       diet1.1$f.1418.1.0,
#       "Full cream", c("Full cream", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin5=make_binary_var(  # 5full cream (1) vs. all other milk (0)
#       diet1.1$f.1418.1.0, "Full cream", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin6=make_binary_var(  # 6semi (1) vs. never (0)
#       diet1.1$f.1418.1.0,
#       "Semi-skimmed", c("Semi-skimmed", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin7=make_binary_var(  # 7semi (1) vs. all other milk (0)
#       diet1.1$f.1418.1.0, "Semi-skimmed", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin8=make_binary_var(  # 8skim (1) vs. never (0)
#       diet1.1$f.1418.1.0,
#       "Skimmed", c("Skimmed", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin9=make_binary_var(  # 9skim (1) vs. all other milk (0)
#       diet1.1$f.1418.1.0, "Skimmed", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin10=make_binary_var(  # 10soy (1) vs. never (0)
#       diet1.1$f.1418.1.0,
#       "Soya", c("Soya", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin11=make_binary_var(  # 11soy (1) vs. all other milk (0)
#       diet1.1$f.1418.1.0,
#       "Soya", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin12=make_binary_var(  # 12other (1) vs. never (0)
#       diet1.1$f.1418.1.0,
#       "Other type of milk", c("Other type of milk", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.1.0_bin13=make_binary_var(  # 13other (1) vs. all other milk (0)
#       diet1.1$f.1418.1.0, "Other type of milk", milk_values, milk_na_values
#     )
#   )
# 
# # table(diet1.1$milk_typeused.1418.1.0_bin, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin2, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_QT3, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin4, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin5, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin6, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin7, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin8, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin9, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin10, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin11, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin12, useNA="always")
# # table(diet1.1$milk_typeused.1418.1.0_bin13, useNA="always")
# 
# # Data-coding 100388 (butter type)
# # Data-coding 100389 (alternate spread type)
# # Field 2654 was collected if a non-butter spread was indicated for 1428
# 
# diet1.1 <- diet1.1 %>%
#   mutate(spread_typeused.1428.0.nonbutterspread_typeused.2654.combined=paste(
#     f.1428.1.0, f.2654.1.0, sep=":"
#   ))
# # table(diet1.1$spread_typeused.1428.1.0,diet1.1$nonbutterspread_typeused.2654.1.0)
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined, useNA="always")
# 
# butter_values <- c(
#   "Butter/spreadable butter:NA", "Do not know:Hard (block) margarine",
#   "Other type of spread/margarine:Hard (block) margarine", "Do not know:Soft (tub) margarine",
#   "Other type of spread/margarine:Soft (tub) margarine", "Do not know:Flora Pro-Active or Benecol",
#   "Flora Pro-Active/Benecol:NA", "Other type of spread/margarine:Flora Pro-Active or Benecol",
#   "Do not know:Olive oil based spread (eg: Bertolli)", "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)",
#   "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)",
#   "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)",
#   "Do not know:Other low or reduced fat spread",
#   "Other type of spread/margarine:Other low or reduced fat spread",
#   "Other type of spread/margarine:Other type of spread/margarine",
#   "Other type of spread/margarine:Do not know", "Do not know:Other type of spread/margarine",
#   "Never/rarely use spread:NA"
# )
# butter_na_values <- c(
#   "Other type of spread/margarine:Do not know", "Do not know:Do not know",
#   "Do not know:Prefer not to answer", "Do not know:Other type of spread/margarine",
#   "Prefer not to answer:NA", "Other type of spread/margarine:Prefer not to answer", "NA:NA"
# )
# 
# diet1.1 <- diet1.1 %>%
#   mutate(
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1=make_binary_var(  #1 All vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       setdiff(butter_values, "Never/rarely use spread:NA"), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2=make_binary_var(  #2 Butter/Margarine vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Butter|Hard|Soft|Flora Pro", butter_values, value=T),
#       grep("Butter|Hard|Soft|Flora Pro|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3=make_binary_var(  #3 Oil-based vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("oil", butter_values, value=T),
#       grep("oil|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4=make_binary_var(  #4 Butter (with flora proactive) vs. Oil
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Butter|Hard|Soft|Benecol", butter_values, value=T),
#       grep("Butter|Hard|Soft|Benecol|oil", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5=make_binary_var(  #5 Butter (without flora proactive)vs. Oil
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Butter|Hard|Soft", butter_values, value=T),
#       grep("Butter|Hard|Soft|oil", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6=make_binary_var(  #6 Butter vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       "Butter/spreadable butter:NA",
#       c("Butter/spreadable butter:NA", "Never/rarely use spread:NA"),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7=make_binary_var(  #7 Butter vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       "Butter/spreadable butter:NA", butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8=make_binary_var(  #8 Hard (block) margarine vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Hard", butter_values, value=T),
#       grep("Hard|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9=make_binary_var(  #9 Hard (block) margarine vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Hard", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10=make_binary_var(  #10 Soft (tub) margarine vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Soft", butter_values, value=T),
#       grep("Soft|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11=make_binary_var(  #11 Soft (tub) margarine vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Soft", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12=make_binary_var(  #12 Flora vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Benecol", butter_values, value=T),
#       grep("Benecol|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13=make_binary_var(  #13 Flora vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Benecol", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14=make_binary_var(  #14 Olive oil based spread (eg: Bertolli) vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Olive", butter_values, value=T),
#       grep("Olive|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15=make_binary_var(  #15 Olive oil based spread (eg: Bertolli) vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Olive", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16=make_binary_var(  #16 Flora oil/sunflower oil vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("sunflower", butter_values, value=T),
#       grep("sunflower|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17=make_binary_var(  #17 Flora oil/sunflower oil vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("sunflower", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18=make_binary_var(  #18 Other low or reduced fat spread vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("reduced", butter_values, value=T),
#       grep("reduced|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19=make_binary_var(  #19 Other low or reduced fat spread vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("reduced", butter_values, value=T), butter_values, butter_na_values
#     )
#   )
# 
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18, useNA="always")
# # table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19, useNA="always")
# 
# diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined <- NULL
# 
# 
# # Field 1448 was collected if eat bread < 1 slice/wk based on 1438
# bread_values <- c("White", "Brown", "Wholemeal or wholegrain", "Other type of bread")
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 100391
#     bread_typeused.1448.1.0_bin=make_binary_var(  #1 White vs Other
#       f.1448.1.0, "White", bread_values, general_na_values
#     ),
#     bread_typeused.1448.1.0_bin2=make_binary_var(  #2 Brown vs. Other
#       f.1448.1.0, "Brown", bread_values, general_na_values
#     ),
#     bread_typeused.1448.1.0_bin3=make_binary_var(  # Wholemeal vs. Other
#       f.1448.1.0, "Wholemeal or wholegrain", bread_values, general_na_values
#     ),
#     bread_typeused.1448.1.0_bin4=make_binary_var(  #5 White vs. Brown + Wholemeal
#       f.1448.1.0, c("White"),
#       c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
#     ),
#     bread_typeused.1448.1.0_bin5=make_binary_var(  #5 White + Brown vs. Wholemeal
#       f.1448.1.0, c("White", "Brown"),
#       c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
#     )
#   )
# # table(diet1.1$f.1448.1.0, useNA="always")
# # table(diet1.1$bread_typeused.1448.1.0_bin, useNA="always")
# # table(diet1.1$bread_typeused.1448.1.0_bin2, useNA="always")
# # table(diet1.1$bread_typeused.1448.1.0_bin3, useNA="always")
# # table(diet1.1$bread_typeused.1448.1.0_bin4, useNA="always")
# # table(diet1.1$bread_typeused.1448.1.0_bin5, useNA="always")
# 
# 
# # Field 1468 was collected if eat cereal < 1 bowl/wk based on 1458
# cereal_values <- c(
#   "Biscuit cereal (e.g. Weetabix)", "Bran cereal (e.g. All Bran, Branflakes)",
#   "Oat cereal (e.g. Ready Brek, porridge)", "Muesli", "Other (e.g. Cornflakes, Frosties)"
# )
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 100393
#     cereal_typeused.1468.1.0_bin=make_binary_var(  # Biscuit vs. All Other
#       f.1468.1.0,
#       "Biscuit cereal (e.g. Weetabix)", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.1.0_bin2=make_binary_var(  # Bran vs. All Other
#       f.1468.1.0,
#       "Bran cereal (e.g. All Bran, Branflakes)", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.1.0_bin3=make_binary_var(  # Oat vs. All Other
#       f.1468.1.0,
#       "Oat cereal (e.g. Ready Brek, porridge)", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.1.0_bin4=make_binary_var(  # Muesli vs. All Other
#       f.1468.1.0,
#       "Muesli", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.1.0_bin5=make_binary_var(  # Other vs. All Other
#       f.1468.1.0,
#       "Other (e.g. Cornflakes, Frosties)", cereal_values, general_na_values
#     )
#   )
# # table(diet1.1$f.1468.1.0, useNA="always")
# # table(diet1.1$cereal_typeused.1468.1.0_bin, useNA="always")
# # table(diet1.1$cereal_typeused.1468.1.0_bin2, useNA="always")
# # table(diet1.1$cereal_typeused.1468.1.0_bin3, useNA="always")
# # table(diet1.1$cereal_typeused.1468.1.0_bin4, useNA="always")
# # table(diet1.1$cereal_typeused.1468.1.0_bin5, useNA="always")
# 
# 
# # Field 1508 was collected if drink coffee < 1 cup/day or at least 1 cup/day based on 1498
# coffee_values <- c(
#   "Ground coffee (include espresso, filter etc)", "Instant coffee",
#   "Decaffeinated coffee (any type)", "Other type of coffee"
# )
# coffee_NA_values <- c("Do not know", "Prefer not to answer", "NA")
# diet1.1 <- diet1.1 %>%
#   mutate(  # Data-coding 100397
#     coffee_typeused.1508.1.0_bin=make_binary_var(  # Ground vs. Other
#       f.1508.1.0,
#       "Ground coffee (include espresso, filter etc)", coffee_values, general_na_values
#     ),
#     coffee_typeused.1508.1.0_bin2=make_binary_var(  # instant_ground vs. other
#       f.1508.1.0,
#       c("Ground coffee (include espresso, filter etc)", "Instant coffee"),
#       coffee_values, general_na_values
#     ),
#     coffee_typeused.1508.1.0_bin3=make_binary_var(
#       f.1508.1.0,
#       "Decaffeinated coffee (any type)", coffee_values, general_na_values
#     )
#   )
# # table(diet1.1$f.1508.1.0, useNA="always")
# # table(diet1.1$coffee_typeused.1508.1.0_bin, useNA="always")
# # table(diet1.1$coffee_typeused.1508.1.0_bin2, useNA="always")
# # table(diet1.1$coffee_typeused.1508.1.0_bin3, useNA="always")
# 
# 
# ##### SAVE #####
# 
# keep_cols_1.1 <- names(diet1.1)[!grepl("^f\\.", names(diet1.1))]
# save_diet1.1 = diet1.1[, keep_cols_1.1]
# write_tsv(save_diet1.1, "../data/processed/ukbiobank_diet1.1")
# saveRDS(save_diet1.1, "../data/processed/ukbiobank_diet1.1.rds")
# 
# 
# ##############################
# ########## DIET 1.2 ##########
# ##############################
# 
# diet1.2 <- base_dataset %>%
#   rename(id=f.eid,
#          year_of_birth.34=f.34.0.0,
#          month_of_birth.52=f.52.0.0,
#          date_assessmentcentre.53.2.0=f.53.2.0)
# # names(diet1.2) <- with(col_dict, {
# #   # raw_names <- paste("f", fieldID, "0.0", sep=".")
# #   # informative_names <- paste(name, fieldID, "0.0", sep=".")
# #   ifelse(names(diet1.2) %in% raw_1.0,
# #          informative_1.0[match(names(diet1.2), raw_1.0)],
# #          names(diet1.2))
# # })
# # diet1.2 <- select(diet1.2, which(!grepl("^f\\.", names(diet1.2))))
# 
# ### DATES
# # Create appropriate age variables per instance
# # DIET1: date_assessmentcentre.53.2.0 -> birth year + birth month
# 
# date1.0 <- str_split_fixed(diet1.2$date_assessmentcentre.53.2.0, "-", 3)
# date1.0 <- apply(date1.0, 2, as.numeric)
# colnames(date1.0) <- c("DIET1_date_year.2.0", "DIET1_date_month.2.0", "DIET1_date_day.2.0")
# diet1.2 <- cbind(diet1.2, date1.0)
# 
# date_cols <- c("DIET1_date_year.2.0", "DIET1_date_month.2.0", "DIET1_date_day.2.0")
# diet1.2 <- diet1.2 %>%
#   separate(date_assessmentcentre.53.2.0, date_cols, "-") %>% # Split DATE into 3 columns (yr/mo/day)
#   mutate_at(date_cols, as.numeric) %>%
#   mutate(month_of_birth_numeric.52=match(month_of_birth.52, month.name),  # Convert month in text -> numeric
#          DIET1_age_months.2.0=(DIET1_date_year.2.0 - year_of_birth.34) * 12 +  # Create age variable
#            (DIET1_date_month.2.0 - month_of_birth_numeric.52))
# 
# ##### CONTINUOUS #####
# 
# clean_cont <- function (x) {
#   # -10 = <1; -1 = "do not know"; -3 = "prefer not to answer"
#   case_when(
#     x == -10 ~ 0.5,
#     is.na(x) | x == -1 | x == -3 ~ as.numeric(NA),
#     TRUE ~ as.numeric(x)
#   )
# }
# 
# diet1.2 <- diet1.2 %>%
#   mutate(
#     cookedveg_TBSperday.1289.2.0_QT=clean_cont(f.1289.2.0),
#     rawveg_TBSperday.1299.2.0_QT=clean_cont(f.1299.2.0),
#     freshfruit_piecesperday.1309.2.0_QT=clean_cont(f.1309.2.0),
#     driedfruit_piecesperday.1319.2.0_QT=clean_cont(f.1319.2.0),
#     bread_slicesperweek.1438.2.0_QT=clean_cont(f.1438.2.0),
#     cereal_bowlsperweek.1458.2.0_QT=clean_cont(f.1458.2.0),
#     tea_cupsperday.1488.2.0_QT=clean_cont(f.1488.2.0),
#     coffee_cupsperday.1498.2.0_QT=clean_cont(f.1498.2.0),
#     water_glassesperday.1528.2.0_QT=clean_cont(f.1528.2.0)
#   )
# 
# # for (qt in grep("_QT", names(diet1.2), value=T)) print(table(diet1.2[[qt]], useNA="always"))
# 
# ##### ALCOHOL #####
# 
# alc1.2 <- alcohol_dataset %>%
#   rename(id=f.eid,  # More informative names for alcohol fields
#          alcohol_overallfreq.1558.2.0=f.1558.2.0,
#          champagnewhitewine_glassesperweek.1578.2.0=f.1578.2.0,
#          champagnewhitewine_glassespermonth.4418.2.0=f.4418.2.0,
#          redwine_glassesperweek.1568.2.0=f.1568.2.0,
#          redwine_glassespermonth.4407.2.0=f.4407.2.0,
#          beercider_pintsperweek.1588.2.0=f.1588.2.0,
#          beercider_pintspermonth.4429.2.0=f.4429.2.0,
#          spirits_measuresperweek.1598.2.0=f.1598.2.0,
#          spirits_measurespermonth.4440.2.0=f.4440.2.0,
#          fortwine_glassesperweek.1608.2.0=f.1608.2.0,
#          fortwine_glassespermonth.4451.2.0=f.4451.2.0,
#          otheralcohol_glassesperweek.5364.2.0=f.5364.2.0,
#          otheralcohol_glassespermonth.4462.2.0=f.4462.2.0,
#          alcoholdrinkerstatus.20117.2.0=f.20117.2.0,
#          alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0=f.1618.2.0) %>%
#   select(id, contains("alcohol"), contains("per")) %>%
#   mutate_at(vars(-id), as.numeric)
# 
# # Fields (1568 1578 1588 1598 1608) collected if drink alcohol > once or twice per week based on 1558
# # table(alc1.2$alcohol_overallfreq.1558.2.0, alc1.2$champagnewhitewine_glassesperweek.1578.2.0)
# 
# # Fields (4407 4418 4429 4440 4451 4462) collected if drink alcohol > once or twice per week based on 1558
# # table(alc1.2$alcohol_overallfreq.1558.2.0, alc1.2$redwine_glassespermonth.4407.2.0)
# 
# # Make one field of glasses per month based on glasses per week and glasses per month
# # if 1558 in (1-2/wk, 3-4/wk, daily) -> glassesperweek * 4
# # if 1558 in (1-3/mo, special occasions) -> glassespermonth
# # if 1558 = Never -> 0
# # if 1558 in (prefer not to answer, NA) -> MICE imputation  ### WAS THIS DONE?
# # Missingness strategy:
# # -1 (do not know) -> median of that group:
# # if glassesperweek = -1 or -3 -> median(of those that said once or twice a week or more)
# 
# median_replace <- function(x) {
#   # Replace (-1, -3) with median of the rest of the values
#   ifelse(x %in% c(-1, -3), median(x[!(x %in% c(-1, -3))]), x)
# }
# 
# # 1 = "Daily or ..."
# # 2 = "Three or four..."
# # 3 = "Once or twice..."
# # 4 = "One to three..."
# # 5 = "Special..."
# # 6 = "Never"
# # -3 = "Prefer not..."
# 
# alc1.2 <- alc1.2 %>%
#   mutate(
#     champagnewhitewine_glassesperweek.1578.2.0_QT=median_replace(
#       champagnewhitewine_glassesperweek.1578.2.0
#     ),
#     champagnewhitewine_glassespermonth.4418.2.0_QT=median_replace(
#       champagnewhitewine_glassespermonth.4418.2.0
#     ),
#     champagnewhitewine_glassespermonth.derived.2.0_QT=case_when(
#       alcohol_overallfreq.1558.2.0 %in% c(1, 2, 3) ~ 4 * champagnewhitewine_glassesperweek.1578.2.0_QT,
#       alcohol_overallfreq.1558.2.0 %in% c(4, 5) ~ champagnewhitewine_glassespermonth.4418.2.0_QT,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.2 <- alc1.2 %>%
#   mutate(
#     redwine_glassesperweek.1568.2.0_QT=median_replace(
#       redwine_glassesperweek.1568.2.0
#     ),
#     redwine_glassespermonth.4407.2.0_QT=median_replace(
#       redwine_glassespermonth.4407.2.0
#     ),
#     redwine_glassespermonth.derived.2.0_QT=case_when(
#       alcohol_overallfreq.1558.2.0 %in% c(1, 2, 3) ~ 4 * redwine_glassesperweek.1568.2.0_QT,
#       alcohol_overallfreq.1558.2.0 %in% c(4, 5) ~ redwine_glassespermonth.4407.2.0_QT,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.2 <- alc1.2 %>%
#   mutate(
#     beercider_pintsperweek.1588.2.0_QT=median_replace(
#       beercider_pintsperweek.1588.2.0
#     ),
#     beercider_pintspermonth.4429.2.0_QT=median_replace(
#       beercider_pintspermonth.4429.2.0
#     ),
#     beercider_pintspermonth.derived.2.0_QT=case_when(
#       alcohol_overallfreq.1558.2.0 %in% c(1, 2, 3) ~ 4 * beercider_pintsperweek.1588.2.0_QT,
#       alcohol_overallfreq.1558.2.0 %in% c(4, 5) ~ beercider_pintspermonth.4429.2.0_QT,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.2 <- alc1.2 %>%
#   mutate(
#     spirits_measuresperweek.1598.2.0_QT=median_replace(
#       spirits_measuresperweek.1598.2.0
#     ),
#     spirits_measurespermonth.4440.2.0_QT=median_replace(
#       spirits_measurespermonth.4440.2.0
#     ),
#     spirits_measurespermonth.derived.2.0_QT=case_when(
#       alcohol_overallfreq.1558.2.0 %in% c(1, 2, 3) ~ 4 * spirits_measuresperweek.1598.2.0_QT,
#       alcohol_overallfreq.1558.2.0 %in% c(4, 5) ~ spirits_measurespermonth.4440.2.0_QT,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.2 <- alc1.2 %>%
#   mutate(
#     fortwine_glassesperweek.1608.2.0_QT=median_replace(
#       fortwine_glassesperweek.1608.2.0
#     ),
#     fortwine_glassespermonth.4451.2.0_QT=median_replace(
#       fortwine_glassespermonth.4451.2.0
#     ),
#     fortwine_glassespermonth.derived.2.0_QT=case_when(
#       alcohol_overallfreq.1558.2.0 %in% c(1, 2, 3) ~ 4 * fortwine_glassesperweek.1608.2.0_QT,
#       alcohol_overallfreq.1558.2.0 %in% c(4, 5) ~ fortwine_glassespermonth.4451.2.0_QT,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.2 <- alc1.2 %>%
#   mutate(
#     otheralcohol_glassesperweek.5364.2.0_QT=median_replace(
#       otheralcohol_glassesperweek.5364.2.0
#     ),
#     otheralcohol_glassespermonth.4462.2.0_QT=median_replace(
#       otheralcohol_glassespermonth.4462.2.0
#     ),
#     otheralcohol_glassespermonth.derived.2.0_QT=case_when(
#       alcohol_overallfreq.1558.2.0 %in% c(1, 2, 3) ~ 4 * otheralcohol_glassesperweek.5364.2.0_QT,
#       alcohol_overallfreq.1558.2.0 %in% c(4, 5) ~ otheralcohol_glassespermonth.4462.2.0_QT,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# alc1.2$anyalcohol_glassespermonth.derived.2.0_QT <- rowSums(  # Glasses of ANY alcohol per month
#   select(alc1.2, contains("permonth.derived.2.0_QT")), na.rm=T
# )
# 
# # lapply(select(alc1.2, -id), table, useNA="always")
# 
# diet1.2 <- left_join(diet1.2, alc1.2, by="id")  # Add alcohol to main diet dataset
# 
# 
# ##### CATEGORICAL TO CONTINUOUS #####
# 
# cat_to_qt <- function(x) {
#   case_when(  # Data-coding 100377
#     x == "Once or more daily" ~ 365,
#     x == "5-6 times a week" ~ 286,
#     x == "2-4 times a week" ~ 156,
#     x == "Once a week" ~ 52,
#     x == "Less than once a week" ~ 20,
#     x == "Never" ~ 0,
#     TRUE ~ as.numeric(NA)
#   )
# }
# 
# diet1.2 <- diet1.2 %>%
#   mutate(
#     oilyfish_overallfreq.1329.2.0_QT=cat_to_qt(f.1329.2.0),
#     nonoilyfish_overallfreq.1339.2.0_QT=cat_to_qt(f.1339.2.0),
#     processmeat_overallfreq.1349.2.0_QT=cat_to_qt(f.1349.2.0),
#     poultry_overallfreq.1359.2.0_QT=cat_to_qt(f.1359.2.0),
#     beef_overallfreq.1369.2.0_QT=cat_to_qt(f.1369.2.0),
#     lambmutton_overallfreq.1379.2.0_QT=cat_to_qt(f.1379.2.0),
#     pork_overallfreq.1389.2.0_QT=cat_to_qt(f.1389.2.0),
#     cheese_overallfreq.1408.2.0_QT=cat_to_qt(f.1408.2.0)
#   )
# ctqt_numbers <- c("1329", "1339", "1349", "1359", "1369", "1379", "1389", "1408")
# # for (n in ctqt_numbers) {
# #   trait <- grep(paste0(n, ".2.0_QT"), names(diet1.2), value=T)
# #   print(table(diet1.2[[trait]], useNA="always"))
# # }
# 
# diet1.2 <- diet1.2 %>%
#   mutate(doyouaddsalt.1478.2.0_QT=case_when(  # Data-coding 100394
#     f.1478.2.0 == "Always" ~ 10,
#     f.1478.2.0 == "Usually" ~ 7,
#     f.1478.2.0 == "Sometimes" ~ 3,
#     f.1478.2.0 == "Never/rarely" ~ 0,
#     TRUE ~ as.numeric(NA))
#   )
# # table(diet1.2$f.1478.2.0, useNA="always")
# # table(diet1.2$doyouaddsalt.1478.2.0_QT, useNA="always")
# 
# diet1.2 <- diet1.2 %>%
#   mutate(
#     hotdrinktemp.1518.2.0_QT=case_when(  # Data-coding 100398
#       f.1518.2.0 == "Very hot" ~ 10,
#       f.1518.2.0 == "Hot" ~ 7,
#       f.1518.2.0 == "Warm" ~ 3,
#       f.1518.2.0 == "Do not drink hot drinks" ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# # table(diet1.2$f.1518.2.0, useNA="always")
# # table(diet1.2$hotdrinktemp.1518.2.0_QT, useNA="always")
# 
# diet1.2 <- diet1.2 %>%
#   mutate(
#     alcohol_overallfreq.1558.2.0_QT=case_when(  # Data-coding 100402
#       alcohol_overallfreq.1558.2.0 == 1 ~ 300,
#       alcohol_overallfreq.1558.2.0 == 2 ~ 182,
#       alcohol_overallfreq.1558.2.0 == 3 ~ 104,
#       alcohol_overallfreq.1558.2.0 == 4 ~ 24,
#       alcohol_overallfreq.1558.2.0 == 5 ~ 6,
#       alcohol_overallfreq.1558.2.0 == 6 ~ 0,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# # table(diet1.2$alcohol_overallfreq.1558.2.0, useNA="always")
# # table(diet1.2$alcohol_overallfreq.1558.2.0_QT, useNA="always")
# 
# ##### MISC #####
# 
# make_binary_var <- function(x, yes_values, all_values, na_values) {
#   # Given a categorical vector, transform into a binary variable based on inputs
#   # defining positive values, all values, and values to leave or set as missing
#   case_when(
#     x %in% yes_values ~ 1,
#     x %in% setdiff(all_values, yes_values) ~ 0,
#     x %in% na_values ~ as.numeric(NA),
#     TRUE ~ as.numeric(NA)
#   )
# }
# 
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 90 (0=Never, 1=Previous, 2=Current, -3=Prefer not to answer)
#     alcoholdrinkerstatus.20117.2.0_bin=case_when(  # Never (0) vs. Current + Previous (1)
#       alcoholdrinkerstatus.20117.2.0 == 0 ~ 0,
#       alcoholdrinkerstatus.20117.2.0 %in% c(1, 2) ~ 1,
#       TRUE ~ as.numeric(NA)
#     ),
#     alcoholdrinkerstatus.20117.2.0_bin2=case_when(  # Never (0) vs. Current only (1)
#       alcoholdrinkerstatus.20117.2.0 == 0 ~ 0,
#       alcoholdrinkerstatus.20117.2.0 ==  2 ~ 1,
#       TRUE ~ as.numeric(NA)
#     ),
#   )
# 
# # table(diet1.2$alcoholdrinkerstatus.20117.2.0, useNA="always")
# # table(diet1.2$alcoholdrinkerstatus.20117.2.0_bin, useNA="always")
# # table(diet1.2$alcoholdrinkerstatus.20117.2.0_bin2, useNA="always")
# # JBC: this will likely impute previous -> never as the correlation with 0 drinks above will be high
# # that's ok. better than making a new variable for this.
# 
# 
# ### Data structure as follows:
# ### Data-coding 100352
# ### Categorical Variable with:
# # Yes
# # No
# # Prefer not to answer
# # NA
# #table(diet1.2$alcohol_formerdrinker_amongcurrentnondrinkers.3731.2.0, useNA="always")
# # no additional information not captured in diet1.2$alcoholdrinkerstatus.20117.2.0
# 
# alc_values <- c(1, -6, 0)
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 100416 (1=Yes, 0=No, -6=It varies, -1=Do not know, -3=Prefer not to answer)
#     alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin=make_binary_var(  # No (0) vs. Yes + It varies (1)
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0, c(1, -6), alc_values, c()
#     ),
#     alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin2=make_binary_var(  # No (0) vs. Yes (1)
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0, c(1), c(1, 0), c()
#     ),
#     alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_QT3=case_when(  # No (0), It varies (1), Yes (2)
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == 0 ~ 0,
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == -6 ~ 1,
#       alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == 1 ~ 2,
#       TRUE ~ as.numeric(NA)
#     )
#   )
# 
# # table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0, useNA="always")
# # table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin, useNA="always")
# # table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin2, useNA="always")
# # table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_QT3, useNA="always")
# 
# diet1.2$nevereatcategories.6144.0.combined <- paste(
#   diet1.2$f.6144.2.0, diet1.2$f.6144.0.1,
#   diet1.2$f.6144.0.2, diet1.2$f.6144.0.3,
#   sep =":"
# )
# # table(diet1.2$nevereatcategories.6144.0.combined, useNA="always")
# 
# necomb_values <- c(
#   "Dairy products:NA:NA:NA", "Dairy products:Sugar or foods/drinks containing sugar:NA:NA",
#   "Dairy products:Wheat products:NA:NA", "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA",
#   "Eggs or foods containing eggs:Dairy products:NA:NA",
#   "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA",
#   "Eggs or foods containing eggs:Dairy products:Wheat products:NA",
#   "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar",
#   "Eggs or foods containing eggs:NA:NA:NA",
#   "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA",
#   "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA",
#   "Eggs or foods containing eggs:Wheat products:NA:NA",
#   "I eat all of the above:NA:NA:NA", "Sugar or foods/drinks containing sugar:NA:NA:NA",
#   "Wheat products:NA:NA:NA", "Wheat products:Sugar or foods/drinks containing sugar:NA:NA"
# )
# 
# general_na_values <- c("Do not know", "Prefer not to answer", "NA")
# 
# necomb_na_values <- c(
#   general_na_values, "NA:NA:NA:NA", "Prefer not to answer:NA:NA:NA"
# )
# 
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 100385
#     nevereatcategories.6144.2.0_bin=make_binary_var(  # Eggs or foods containing eggs (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Eggs", necomb_values, value=T),
#       grep("Eggs|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin2=make_binary_var(  # Eggs or foods containing eggs (1) vs. everyone else (0)
#       nevereatcategories.6144.0.combined, grep("Eggs", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin3=make_binary_var(  # Dairy products (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Dairy", necomb_values, value=T),
#       grep("Dairy|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin4=make_binary_var(  # Dairy products (1)  vs. everyone else (0)
#       nevereatcategories.6144.0.combined, grep("Dairy", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin5=make_binary_var(  # Wheat products (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Wheat", necomb_values, value=T),
#       grep("Wheat|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin6=make_binary_var(  # Wheat products (1)  vs. everyone else (0)
#       nevereatcategories.6144.0.combined, grep("Wheat", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin7=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)
#       nevereatcategories.6144.0.combined, grep("Sugar", necomb_values, value=T),
#       grep("Sugar|above", necomb_values, value=T), necomb_na_values
#     ),
#     nevereatcategories.6144.2.0_bin8=make_binary_var(  # Sugar or foods/drinks containing sugar (1) vs. everyone else (0))
#       nevereatcategories.6144.0.combined, grep("Sugar", necomb_values, value=T),
#       necomb_values, necomb_na_values
#     )
#   )
# 
# # table(diet1.2$f.6144.2.0, useNA="always")
# # table(diet1.2$f.6144.0.1, useNA="always")
# # table(diet1.2$f.6144.0.2, useNA="always")
# # table(diet1.2$f.6144.0.3, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin2, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin3, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin4, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin5, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin6, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin7, useNA="always")
# # table(diet1.2$nevereatcategories.6144.2.0_bin8, useNA="always")
# 
# diet1.2$nevereatcategories.6144.0.combined <- NULL
# 
# milk_values <- c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Never/rarely have milk")
# milk_na_values <- c(general_na_values, "Other type of milk")
# 
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 100387
#     milk_typeused.1418.2.0_bin=make_binary_var(  # 1binary: milk x3 vs. never
#       diet1.2$f.1418.2.0,
#       c("Full cream", "Semi-skimmed", "Skimmed"), milk_values, milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin2=make_binary_var(  # 2binary: milk x5 vs. never
#       diet1.2$f.1418.2.0,
#       c("Full cream", "Semi-skimmed", "Skimmed", "Soya", "Other type of milk"),
#       milk_values, milk_na_values
#     ),
#     milk_typeused.1418.2.0_QT3=case_when(  # 3continuous: never (0), skimmed (1), semi-skimmed (2), full cream (3)
#       f.1418.2.0 == "Full cream" ~ 3,
#       f.1418.2.0 == "Semi-skimmed" ~ 2,
#       f.1418.2.0 == "Skimmed" ~ 1,
#       f.1418.2.0 == "Never/rarely have milk" ~ 0,
#       TRUE ~ as.numeric(NA)
#     ),
#     milk_typeused.1418.2.0_bin4=make_binary_var(  # 4full cream (1) vs. never (0)
#       diet1.2$f.1418.2.0,
#       "Full cream", c("Full cream", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin5=make_binary_var(  # 5full cream (1) vs. all other milk (0)
#       diet1.2$f.1418.2.0, "Full cream", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin6=make_binary_var(  # 6semi (1) vs. never (0)
#       diet1.2$f.1418.2.0,
#       "Semi-skimmed", c("Semi-skimmed", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin7=make_binary_var(  # 7semi (1) vs. all other milk (0)
#       diet1.2$f.1418.2.0, "Semi-skimmed", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin8=make_binary_var(  # 8skim (1) vs. never (0)
#       diet1.2$f.1418.2.0,
#       "Skimmed", c("Skimmed", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin9=make_binary_var(  # 9skim (1) vs. all other milk (0)
#       diet1.2$f.1418.2.0, "Skimmed", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin10=make_binary_var(  # 10soy (1) vs. never (0)
#       diet1.2$f.1418.2.0,
#       "Soya", c("Soya", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin11=make_binary_var(  # 11soy (1) vs. all other milk (0)
#       diet1.2$f.1418.2.0,
#       "Soya", milk_values, milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin12=make_binary_var(  # 12other (1) vs. never (0)
#       diet1.2$f.1418.2.0,
#       "Other type of milk", c("Other type of milk", "Never/rarely have milk"), milk_na_values
#     ),
#     milk_typeused.1418.2.0_bin13=make_binary_var(  # 13other (1) vs. all other milk (0)
#       diet1.2$f.1418.2.0, "Other type of milk", milk_values, milk_na_values
#     )
#   )
# 
# # table(diet1.2$milk_typeused.1418.2.0_bin, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin2, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_QT3, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin4, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin5, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin6, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin7, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin8, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin9, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin10, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin11, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin12, useNA="always")
# # table(diet1.2$milk_typeused.1418.2.0_bin13, useNA="always")
# 
# # Data-coding 100388 (butter type)
# # Data-coding 100389 (alternate spread type)
# # Field 2654 was collected if a non-butter spread was indicated for 1428
# 
# diet1.2 <- diet1.2 %>%
#   mutate(spread_typeused.1428.0.nonbutterspread_typeused.2654.combined=paste(
#     f.1428.2.0, f.2654.2.0, sep=":"
#   ))
# # table(diet1.2$spread_typeused.1428.2.0,diet1.2$nonbutterspread_typeused.2654.2.0)
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined, useNA="always")
# 
# butter_values <- c(
#   "Butter/spreadable butter:NA", "Do not know:Hard (block) margarine",
#   "Other type of spread/margarine:Hard (block) margarine", "Do not know:Soft (tub) margarine",
#   "Other type of spread/margarine:Soft (tub) margarine", "Do not know:Flora Pro-Active or Benecol",
#   "Flora Pro-Active/Benecol:NA", "Other type of spread/margarine:Flora Pro-Active or Benecol",
#   "Do not know:Olive oil based spread (eg: Bertolli)", "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)",
#   "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)",
#   "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)",
#   "Do not know:Other low or reduced fat spread",
#   "Other type of spread/margarine:Other low or reduced fat spread",
#   "Other type of spread/margarine:Other type of spread/margarine",
#   "Other type of spread/margarine:Do not know", "Do not know:Other type of spread/margarine",
#   "Never/rarely use spread:NA"
# )
# butter_na_values <- c(
#   "Other type of spread/margarine:Do not know", "Do not know:Do not know",
#   "Do not know:Prefer not to answer", "Do not know:Other type of spread/margarine",
#   "Prefer not to answer:NA", "Other type of spread/margarine:Prefer not to answer", "NA:NA"
# )
# 
# diet1.2 <- diet1.2 %>%
#   mutate(
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1=make_binary_var(  #1 All vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       setdiff(butter_values, "Never/rarely use spread:NA"), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2=make_binary_var(  #2 Butter/Margarine vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Butter|Hard|Soft|Flora Pro", butter_values, value=T),
#       grep("Butter|Hard|Soft|Flora Pro|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3=make_binary_var(  #3 Oil-based vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("oil", butter_values, value=T),
#       grep("oil|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4=make_binary_var(  #4 Butter (with flora proactive) vs. Oil
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Butter|Hard|Soft|Benecol", butter_values, value=T),
#       grep("Butter|Hard|Soft|Benecol|oil", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5=make_binary_var(  #5 Butter (without flora proactive)vs. Oil
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Butter|Hard|Soft", butter_values, value=T),
#       grep("Butter|Hard|Soft|oil", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6=make_binary_var(  #6 Butter vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       "Butter/spreadable butter:NA",
#       c("Butter/spreadable butter:NA", "Never/rarely use spread:NA"),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7=make_binary_var(  #7 Butter vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       "Butter/spreadable butter:NA", butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8=make_binary_var(  #8 Hard (block) margarine vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Hard", butter_values, value=T),
#       grep("Hard|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9=make_binary_var(  #9 Hard (block) margarine vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Hard", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10=make_binary_var(  #10 Soft (tub) margarine vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Soft", butter_values, value=T),
#       grep("Soft|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11=make_binary_var(  #11 Soft (tub) margarine vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Soft", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12=make_binary_var(  #12 Flora vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Benecol", butter_values, value=T),
#       grep("Benecol|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13=make_binary_var(  #13 Flora vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Benecol", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14=make_binary_var(  #14 Olive oil based spread (eg: Bertolli) vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Olive", butter_values, value=T),
#       grep("Olive|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15=make_binary_var(  #15 Olive oil based spread (eg: Bertolli) vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("Olive", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16=make_binary_var(  #16 Flora oil/sunflower oil vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("sunflower", butter_values, value=T),
#       grep("sunflower|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17=make_binary_var(  #17 Flora oil/sunflower oil vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("sunflower", butter_values, value=T), butter_values, butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18=make_binary_var(  #18 Other low or reduced fat spread vs. Never
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("reduced", butter_values, value=T),
#       grep("reduced|Never", butter_values, value=T),
#       butter_na_values
#     ),
#     spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19=make_binary_var(  #19 Other low or reduced fat spread vs. Other
#       spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,
#       grep("reduced", butter_values, value=T), butter_values, butter_na_values
#     )
#   )
# 
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18, useNA="always")
# # table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19, useNA="always")
# 
# diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined <- NULL
# 
# 
# # Field 1448 was collected if eat bread < 1 slice/wk based on 1438
# bread_values <- c("White", "Brown", "Wholemeal or wholegrain", "Other type of bread")
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 100391
#     bread_typeused.1448.2.0_bin=make_binary_var(  #1 White vs Other
#       f.1448.2.0, "White", bread_values, general_na_values
#     ),
#     bread_typeused.1448.2.0_bin2=make_binary_var(  #2 Brown vs. Other
#       f.1448.2.0, "Brown", bread_values, general_na_values
#     ),
#     bread_typeused.1448.2.0_bin3=make_binary_var(  # Wholemeal vs. Other
#       f.1448.2.0, "Wholemeal or wholegrain", bread_values, general_na_values
#     ),
#     bread_typeused.1448.2.0_bin4=make_binary_var(  #5 White vs. Brown + Wholemeal
#       f.1448.2.0, c("White"),
#       c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
#     ),
#     bread_typeused.1448.2.0_bin5=make_binary_var(  #5 White + Brown vs. Wholemeal
#       f.1448.2.0, c("White", "Brown"),
#       c("White", "Brown", "Wholemeal or wholegrain"), general_na_values
#     )
#   )
# # table(diet1.2$f.1448.2.0, useNA="always")
# # table(diet1.2$bread_typeused.1448.2.0_bin, useNA="always")
# # table(diet1.2$bread_typeused.1448.2.0_bin2, useNA="always")
# # table(diet1.2$bread_typeused.1448.2.0_bin3, useNA="always")
# # table(diet1.2$bread_typeused.1448.2.0_bin4, useNA="always")
# # table(diet1.2$bread_typeused.1448.2.0_bin5, useNA="always")
# 
# 
# # Field 1468 was collected if eat cereal < 1 bowl/wk based on 1458
# cereal_values <- c(
#   "Biscuit cereal (e.g. Weetabix)", "Bran cereal (e.g. All Bran, Branflakes)",
#   "Oat cereal (e.g. Ready Brek, porridge)", "Muesli", "Other (e.g. Cornflakes, Frosties)"
# )
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 100393
#     cereal_typeused.1468.2.0_bin=make_binary_var(  # Biscuit vs. All Other
#       f.1468.2.0,
#       "Biscuit cereal (e.g. Weetabix)", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.2.0_bin2=make_binary_var(  # Bran vs. All Other
#       f.1468.2.0,
#       "Bran cereal (e.g. All Bran, Branflakes)", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.2.0_bin3=make_binary_var(  # Oat vs. All Other
#       f.1468.2.0,
#       "Oat cereal (e.g. Ready Brek, porridge)", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.2.0_bin4=make_binary_var(  # Muesli vs. All Other
#       f.1468.2.0,
#       "Muesli", cereal_values, general_na_values
#     ),
#     cereal_typeused.1468.2.0_bin5=make_binary_var(  # Other vs. All Other
#       f.1468.2.0,
#       "Other (e.g. Cornflakes, Frosties)", cereal_values, general_na_values
#     )
#   )
# # table(diet1.2$f.1468.2.0, useNA="always")
# # table(diet1.2$cereal_typeused.1468.2.0_bin, useNA="always")
# # table(diet1.2$cereal_typeused.1468.2.0_bin2, useNA="always")
# # table(diet1.2$cereal_typeused.1468.2.0_bin3, useNA="always")
# # table(diet1.2$cereal_typeused.1468.2.0_bin4, useNA="always")
# # table(diet1.2$cereal_typeused.1468.2.0_bin5, useNA="always")
# 
# 
# # Field 1508 was collected if drink coffee < 1 cup/day or at least 1 cup/day based on 1498
# coffee_values <- c(
#   "Ground coffee (include espresso, filter etc)", "Instant coffee",
#   "Decaffeinated coffee (any type)", "Other type of coffee"
# )
# coffee_NA_values <- c("Do not know", "Prefer not to answer", "NA")
# diet1.2 <- diet1.2 %>%
#   mutate(  # Data-coding 100397
#     coffee_typeused.1508.2.0_bin=make_binary_var(  # Ground vs. Other
#       f.1508.2.0,
#       "Ground coffee (include espresso, filter etc)", coffee_values, general_na_values
#     ),
#     coffee_typeused.1508.2.0_bin2=make_binary_var(  # instant_ground vs. other
#       f.1508.2.0,
#       c("Ground coffee (include espresso, filter etc)", "Instant coffee"),
#       coffee_values, general_na_values
#     ),
#     coffee_typeused.1508.2.0_bin3=make_binary_var(
#       f.1508.2.0,
#       "Decaffeinated coffee (any type)", coffee_values, general_na_values
#     )
#   )
# # table(diet1.2$f.1508.2.0, useNA="always")
# # table(diet1.2$coffee_typeused.1508.2.0_bin, useNA="always")
# # table(diet1.2$coffee_typeused.1508.2.0_bin2, useNA="always")
# # table(diet1.2$coffee_typeused.1508.2.0_bin3, useNA="always")
# 
# 
# ##### SAVE #####
# 
# keep_cols_1.2 <- names(diet1.2)[!grepl("^f\\.", names(diet1.2))]
# save_diet1.2 = diet1.2[, keep_cols_1.2]
# write_tsv(save_diet1.2, "../data/processed/ukbiobank_diet1.2")
# saveRDS(save_diet1.2, "../data/processed/ukbiobank_diet1.2.rds")
# 
# 
# ##############################################
# ########## COMBINE ALL DIETARY DATA ##########
# ##############################################
# 
# diet1.0 <- readRDS("../data/processed/ukbiobank_diet1.0.rds")
# diet1.1 <- readRDS("../data/processed/ukbiobank_diet1.1.rds")
# diet1.2 <- readRDS("../data/processed/ukbiobank_diet1.2.rds")
# 
# names(diet1.0) <- gsub("\\.0\\.0_", "_", names(diet1.0))  # Remove visit ID component from field name
# names(diet1.1) <- gsub("\\.1\\.0_", "_", names(diet1.1))
# names(diet1.2) <- gsub("\\.2\\.0_", "_", names(diet1.2))
# 
# diet_avg <- bind_rows(list("1.0"=diet1.0,
#                            "1.1"=diet1.1,
#                            "1.2"=diet1.2),
#                       .id="visit") %>%
#   group_by(id) %>%
#   summarise_all(mean, na.rm=T)
# 
# diet_avg$DIET1_age_months.average=rowMeans(diet_avg[, (
#   c("DIET1_age_months.0.0", "DIET1_age_months.1.0", "DIET1_age_months.2.0")
# )], na.rm=T)
# diet_avg <- diet_avg %>%
#   left_join(select(base_dataset, id=f.eid, selfreportsex.31=f.31.0.0), by="id")
# 
# pca_fields <- c(
#   "id",
#   "selfreportsex.31",
#   "DIET1_age_months.average",
#   "cookedveg_TBSperday.1289_QT",
#   "rawveg_TBSperday.1299_QT",
#   "freshfruit_piecesperday.1309_QT",
#   "driedfruit_piecesperday.1319_QT",
#   "bread_slicesperweek.1438_QT",
#   "cereal_bowlsperweek.1458_QT",
#   "tea_cupsperday.1488_QT",
#   "coffee_cupsperday.1498_QT",
#   "water_glassesperday.1528_QT",
#   "champagnewhitewine_glassespermonth.derived_QT",
#   "redwine_glassespermonth.derived_QT",
#   "beercider_pintspermonth.derived_QT",
#   "spirits_measurespermonth.derived_QT",
#   "fortwine_glassespermonth.derived_QT",
#   "otheralcohol_glassespermonth.derived_QT",
#   "anyalcohol_glassespermonth.derived_QT",
#   "oilyfish_overallfreq.1329_QT",
#   "nonoilyfish_overallfreq.1339_QT",
#   "processmeat_overallfreq.1349_QT",
#   "poultry_overallfreq.1359_QT",
#   "beef_overallfreq.1369_QT",
#   "lambmutton_overallfreq.1379_QT",
#   "pork_overallfreq.1389_QT",
#   "cheese_overallfreq.1408_QT",
#   "doyouaddsalt.1478_QT",
#   "hotdrinktemp.1518_QT",
#   "alcohol_overallfreq.1558_QT",
#   "alcoholdrinkerstatus.20117_bin",
#   "alcoholdrinkerstatus.20117_bin2",
#   "alcohol_usuallywithmeals_amongcurrentdrinkers.1618_bin",
#   "alcohol_usuallywithmeals_amongcurrentdrinkers.1618_bin2",
#   "alcohol_usuallywithmeals_amongcurrentdrinkers.1618_QT3",
#   "nevereatcategories.6144_bin",
#   "nevereatcategories.6144_bin2",
#   "nevereatcategories.6144_bin3",
#   "nevereatcategories.6144_bin4",
#   "nevereatcategories.6144_bin5",
#   "nevereatcategories.6144_bin6",
#   "nevereatcategories.6144_bin7",
#   "nevereatcategories.6144_bin8",
#   "milk_typeused.1418_bin",
#   "milk_typeused.1418_bin2",
#   "milk_typeused.1418_QT3",
#   "milk_typeused.1418_bin4",
#   "milk_typeused.1418_bin5",
#   "milk_typeused.1418_bin6",
#   "milk_typeused.1418_bin7",
#   "milk_typeused.1418_bin8",
#   "milk_typeused.1418_bin9",
#   "milk_typeused.1418_bin10",
#   "milk_typeused.1418_bin11",
#   "milk_typeused.1418_bin12",
#   "milk_typeused.1418_bin13",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18",
#   "spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19",
#   "bread_typeused.1448_bin",
#   "bread_typeused.1448_bin2",
#   "bread_typeused.1448_bin3",
#   "bread_typeused.1448_bin4",
#   "bread_typeused.1448_bin5",
#   "cereal_typeused.1468_bin",
#   "cereal_typeused.1468_bin2",
#   "cereal_typeused.1468_bin3",
#   "cereal_typeused.1468_bin4",
#   "cereal_typeused.1468_bin5",
#   "coffee_typeused.1508_bin",
#   "coffee_typeused.1508_bin2",
#   "coffee_typeused.1508_bin3"
# )
# 
# write_tsv(select(diet_avg, pca_fields), "../data/processed/diet_avg.txt")
# 
# 
# ##### DISEASE #####
# 
# ### Gather info on ESRD and cancer diagnosis in the last year
# health_outcome_cols <- grep("f.4[0-9]{4}.*", names(base_dataset), value=T)
# cancer_cols <- grep("f\\.40005\\.", names(base_dataset), value=T)
# pregnancy_col <- "f.3140.0.0"
# diet_date_col <- "f.53.0.0"
# 
# filter_data <- base_dataset %>%
#   select(f.eid, health_outcome_cols, cancer_cols, pregnancy_col, diet_date_col) %>%
#   separate(f.53.0.0, into=c("DIET1_date_year.0.0", "DIET1_date_month.0.0", 
#                             "DIET1_date_day.0.0"),
#            sep="-") %>%
#   mutate_at(vars(contains("DIET1")), as.numeric) %>%
#   rename(id=f.eid)
# 
# esrd_codes <- c("I120", "I131", "I132", "N180", "N188", "N189", "Y841", "Z992")
# esrd <- filter_data$id[apply(filter_data[, health_outcome_cols], 1,  # Individuals with ESRD related ICD10 codes 
#                              function(r) any(r %in% esrd_codes))]
# 
# for (yr in paste0(0:10, ".0")) {  # Dates of cancer diagnosis
#   cancerdiag_ymd_df <- str_split_fixed(filter_data[[paste0("f.40005.", yr)]], "-", 3)
#   cancerdiag_ymd_df <- cancerdiag_ymd_df %>%
#     as_tibble() %>%
#     setNames(paste0("date_cancerdiag_", c("year", "month", "dat"), 
#                     ".40005.", yr)) %>%
#     mutate_all(as.numeric)
#   filter_data <- cbind(filter_data, cancerdiag_ymd_df)
# }
# 
# calc_months_from_cancer_diag <- function(x, df) {
#   diet_month <- df$DIET1_date_month.0.0
#   diet_year <- df$DIET1_date_year.0.0
#   cancer_month <- df[[paste0("date_cancerdiag_month.40005.", x)]]
#   cancer_year <- df[[paste0("date_cancerdiag_year.40005.", x)]]
#   12 * (diet_year - cancer_year) + (diet_month - cancer_month)
# }
# for (dn in paste0(0:10, ".0")) {
#   filter_data[[paste0("DIET1_monthsfromcancerdiag.", dn)]] <- calc_months_from_cancer_diag(dn, filter_data)
# }
# 
# #### FILTERING ####
# 
# # ids_to_include <- scan("../data/processed/ids_to_include.txt", what=integer())
# # 
# # filter_data <- filter_data %>%
# #   filter(id %in% ids_to_include),
# #        f.3140.0.0 != "Yes" | is.na(f.3140.0.0),  # Not pregnant
# #        !(id %in% esrd)) %>%  # No ESRD
# # filter_at(vars(contains("DIET1_monthsfromcancerdiag.")),
# #           all_vars((. > 12) | (. < 0) | is.na(.)))  # No cancer diagnosis <=1 year from current age (ideally would be from remission...)
# #### LAST DATA CLEANING ####
# 
# diet1$sex <- c(Male=0, Female=1)[diet1$selfreportsex.31]  # Convert sex to 0/1
# 
# # centers <- c(
# #   "11012"="Barts", "11021"="Birmingham", "11011"="Bristol", "11008"="Bury",
# #   "11003"="Cardiff", "11024"="Cheadle_revisit", "11020"="Croydon", 
# #   "11005"="Edinburgh", "11004"="Glasgow", "11018"="Hounslow", "11010"="Leeds", 
# #   "11016"="Liverpool", "11001"="Manchester", "11017"="Middlesborough", 
# #   "11009"="Newcastle", "11013"="Nottingham", "11002"="Oxford",
# #   "11007"="Reading", "11014"="Sheffield", "10003"="Stockport_pilot",
# #   "11006"="Stoke", "11022"="Swansea", "11023"="Wrexham", 
# #   "11025"="Cheadle_pilot", "11027"="Newcastle_pilot"
# # )
# # center$f.54.0.0_categorical <- centers[center$f.54.0.0]
# 
# admin_data <- read_tsv("/humgen/diabetes2/users/jcole/UKBB/pheno/UKBiobank_assessment_center_f.54_birthplace_f.1647.txt") %>%
#   select(id=Florez_FID, f.54.0.0_categorical, f.1647.0.0_categorical)
# 
# # Split diet1 into bin and QT
# bin_fields <- c("id", "DIET1_age_months.average", "sex", grep("_bin", names(diet1), value=T))
# diet1_bin = select(diet1, bin_fields)
# 
# qt_fields <- c("id", "DIET1_age_months.average", "sex", grep("_QT", names(diet1), value=T))
# diet1_QT = select(diet1, qt_fields)
