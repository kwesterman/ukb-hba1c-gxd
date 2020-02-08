######################## STEP01-03 combined from Hirschhorn -> FLorez Dir

### see column files for fields.

columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_other > \
/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_8785_other_columns

columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_other > \
/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_6184_other_columns

columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_diet1.0 > \
/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_8785_diet1.0_columns

columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_diet1.0 > \
/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_6184_diet1.0_columns

columns /broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_7052_diet1.0 > \
/humgen/diabetes2/users/jcole/UKBB/diet/src/ukbiobank_diet_7052_diet1.0_columns


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
################### How to FFQ Diet Phenotypes - Hirschhorn Application
################### STEP1: Sample exclusions, Derive QT averages from all instances.
########## last updated: July 6, 2018
########## JBC
######################################################################################################################################



### Remove individuals with ESRD
### Remove individuals with cancer diagnosis in the last year

## read in ICD codes and cancer codes file
library(data.table)
## fat mass file, never actually used these fields
other_8785 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_other", verbose=TRUE, data.table=FALSE)
## ICD codes + cancer diag
other_6184 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_other", verbose=TRUE, data.table=FALSE)
other = merge(other_8785, other_6184, by = "ID", all = TRUE)

## create a list of individuals with ESRD related ICD10 codes 
esrd = NULL
for (i in 38:758)
	{x = other[which(other[,i] == "I120" | other[,i] == "I131" | other[,i] == "I132" | other[,i] == "N180" | other[,i] == "N188" | other[,i] == "N189" | other[,i] == "Y841" | other[,i] == "Z992"), "ID"]
	esrd = c(esrd,x)
	}
esrd = unique(esrd)

####### date of cancer diagnosis
library(stringr)
x = str_split_fixed(other$date_cancerdiag.40005.0.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.0.0", "date_cancerdiag_month.40005.0.0", "date_cancerdiag_day.40005.0.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.1.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.1.0", "date_cancerdiag_month.40005.1.0", "date_cancerdiag_day.40005.1.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.2.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.2.0", "date_cancerdiag_month.40005.2.0", "date_cancerdiag_day.40005.2.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.3.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.3.0", "date_cancerdiag_month.40005.3.0", "date_cancerdiag_day.40005.3.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.4.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.4.0", "date_cancerdiag_month.40005.4.0", "date_cancerdiag_day.40005.4.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.5.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.5.0", "date_cancerdiag_month.40005.5.0", "date_cancerdiag_day.40005.5.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.6.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.6.0", "date_cancerdiag_month.40005.6.0", "date_cancerdiag_day.40005.6.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.7.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.7.0", "date_cancerdiag_month.40005.7.0", "date_cancerdiag_day.40005.7.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.8.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.8.0", "date_cancerdiag_month.40005.8.0", "date_cancerdiag_day.40005.8.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.9.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.9.0", "date_cancerdiag_month.40005.9.0", "date_cancerdiag_day.40005.9.0")
other = cbind(other,x)
x = str_split_fixed(other$date_cancerdiag.40005.10.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("date_cancerdiag_year.40005.10.0", "date_cancerdiag_month.40005.10.0", "date_cancerdiag_day.40005.10.0")
other = cbind(other,x)

cancer = other[,c(1,759:791)]

################### ################### ################### ################### ################### ################### ###################
#load in and merge diet1.0
diet1.0_8785 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_diet1.0", verbose=TRUE, data.table=FALSE)
diet1.0_6184 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_diet1.0", verbose=TRUE, data.table=FALSE)
diet1.0_7052 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_7052_diet1.0", verbose=TRUE, data.table=FALSE)


diet1.0tmp = merge(diet1.0_6184, diet1.0_8785, by = "ID", all = T)
diet1.0 = merge(diet1.0tmp, diet1.0_7052, by = "ID", all = T)

###################
#create appropriate age variables per instance
#DIET1: date_assessmentcentre.53.0.0 -> birth year + birth month

## split DATE -> 3 columns of year, month, day
library(stringr)

x = str_split_fixed(diet1.0$date_assessmentcentre.53.0.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("DIET1_date_year.0.0", "DIET1_date_month.0.0", "DIET1_date_day.0.0")
diet1.0 = cbind(diet1.0,x)

#table(diet1.0$month_of_birth.52,useNA="always")

##convert month in text -> numeric

diet1.0$month_of_birth_numeric.52 = ifelse(diet1.0$month_of_birth.52 == "January", 1, 
				 ifelse(diet1.0$month_of_birth.52 == "February", 2, 
				 ifelse(diet1.0$month_of_birth.52 == "March", 3, 
				 ifelse(diet1.0$month_of_birth.52 == "April", 4, 
				 ifelse(diet1.0$month_of_birth.52 == "May", 5, 
				 ifelse(diet1.0$month_of_birth.52 == "June", 6, 
				 ifelse(diet1.0$month_of_birth.52 == "July", 7, 
				 ifelse(diet1.0$month_of_birth.52 == "August", 8, 
				 ifelse(diet1.0$month_of_birth.52 == "September", 9, 
				 ifelse(diet1.0$month_of_birth.52 == "October", 10, 
				 ifelse(diet1.0$month_of_birth.52 == "November", 11, 
				 ifelse(diet1.0$month_of_birth.52 == "December", 12, NA))))))))))))

##create age variable

diet1.0$DIET1_age_months.0.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$month_of_birth_numeric.52, (diet1.0$DIET1_date_year.0.0 - diet1.0$year_of_birth.34)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$month_of_birth_numeric.52),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$year_of_birth.34)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$month_of_birth_numeric.52)))

###################
#exclusions to diet1.0

#pregnant
#table(diet1.0$pregnant.3140.0.0, useNA="always")
diet1.0 = subset(diet1.0, pregnant.3140.0.0 != "Yes" | is.na(pregnant.3140.0.0))

#non-esrd
diet1.0 = subset(diet1.0, !ID %in% esrd)

#non-cancer diagnosis <=1 year from current age
#(ideally would be from remission...)
diet1.0 = merge(diet1.0,cancer, by ="ID",all.x = T)

diet1.0$DIET1_monthsfromcancerdiag.0.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.0.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.0.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.0.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.0.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.0.0)))
diet1.0$DIET1_monthsfromcancerdiag.1.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.1.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.1.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.1.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.1.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.1.0)))
diet1.0$DIET1_monthsfromcancerdiag.2.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.2.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.2.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.2.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.2.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.2.0)))
diet1.0$DIET1_monthsfromcancerdiag.3.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.3.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.3.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.3.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.3.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.3.0)))
diet1.0$DIET1_monthsfromcancerdiag.4.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.4.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.4.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.4.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.4.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.4.0)))
diet1.0$DIET1_monthsfromcancerdiag.5.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.5.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.5.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.5.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.5.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.5.0)))
diet1.0$DIET1_monthsfromcancerdiag.6.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.6.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.6.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.6.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.6.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.6.0)))
diet1.0$DIET1_monthsfromcancerdiag.7.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.7.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.7.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.7.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.7.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.7.0)))
diet1.0$DIET1_monthsfromcancerdiag.8.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.8.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.8.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.8.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.8.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.8.0)))
diet1.0$DIET1_monthsfromcancerdiag.9.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.9.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.9.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.9.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.9.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.9.0)))
diet1.0$DIET1_monthsfromcancerdiag.10.0 = ifelse(diet1.0$DIET1_date_month.0.0 >= diet1.0$date_cancerdiag_month.40005.10.0, (diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.10.0)*12 + (diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.10.0),
								  	(diet1.0$DIET1_date_year.0.0 - diet1.0$date_cancerdiag_year.40005.10.0)*12 - abs((diet1.0$DIET1_date_month.0.0 - diet1.0$date_cancerdiag_month.40005.10.0)))

diet1.0 = subset(diet1.0, (DIET1_monthsfromcancerdiag.0.0 > 12 | DIET1_monthsfromcancerdiag.0.0 < 0 | is.na(DIET1_monthsfromcancerdiag.0.0)) &
			  (DIET1_monthsfromcancerdiag.1.0 > 12 | DIET1_monthsfromcancerdiag.1.0 < 0 | is.na(DIET1_monthsfromcancerdiag.1.0)) &
			  (DIET1_monthsfromcancerdiag.2.0 > 12 | DIET1_monthsfromcancerdiag.2.0 < 0 | is.na(DIET1_monthsfromcancerdiag.2.0)) &
			  (DIET1_monthsfromcancerdiag.3.0 > 12 | DIET1_monthsfromcancerdiag.3.0 < 0 | is.na(DIET1_monthsfromcancerdiag.3.0)) &
			  (DIET1_monthsfromcancerdiag.4.0 > 12 | DIET1_monthsfromcancerdiag.4.0 < 0 | is.na(DIET1_monthsfromcancerdiag.4.0)) &
			  (DIET1_monthsfromcancerdiag.5.0 > 12 | DIET1_monthsfromcancerdiag.5.0 < 0 | is.na(DIET1_monthsfromcancerdiag.5.0)) &
			  (DIET1_monthsfromcancerdiag.6.0 > 12 | DIET1_monthsfromcancerdiag.6.0 < 0 | is.na(DIET1_monthsfromcancerdiag.6.0)) &
			  (DIET1_monthsfromcancerdiag.7.0 > 12 | DIET1_monthsfromcancerdiag.7.0 < 0 | is.na(DIET1_monthsfromcancerdiag.7.0)) &
			  (DIET1_monthsfromcancerdiag.8.0 > 12 | DIET1_monthsfromcancerdiag.8.0 < 0 | is.na(DIET1_monthsfromcancerdiag.8.0)) &
			  (DIET1_monthsfromcancerdiag.9.0 > 12 | DIET1_monthsfromcancerdiag.9.0 < 0 | is.na(DIET1_monthsfromcancerdiag.9.0)) &
			  (DIET1_monthsfromcancerdiag.10.0 > 12 | DIET1_monthsfromcancerdiag.10.0 < 0 | is.na(DIET1_monthsfromcancerdiag.10.0)))
			  
###################
#create instance field
diet1.0$instance = "diet1.0"
###################
#create usable phenotypes. 

### Data structure as follows:
### Data-coding 100373
### Continuous Variable with:
	# -10 = <1
	# -1 = do not know
	# -3 = prefer not to answer
	
	#convert -10 = .5
	#convert -1 & -3 = "NA"

#table(diet1.0$cookedveg_TBSperday.1289.0.0,useNA="always")
diet1.0$cookedveg_TBSperday.1289.0.0_QT = as.numeric(ifelse(diet1.0$cookedveg_TBSperday.1289.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$cookedveg_TBSperday.1289.0.0) | diet1.0$cookedveg_TBSperday.1289.0.0 == -1 | diet1.0$cookedveg_TBSperday.1289.0.0 == -3, "NA", 
					diet1.0$cookedveg_TBSperday.1289.0.0)))
	
#table(diet1.0$rawveg_TBSperday.1299.0.0,useNA="always")
diet1.0$rawveg_TBSperday.1299.0.0_QT = as.numeric(ifelse(diet1.0$rawveg_TBSperday.1299.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$rawveg_TBSperday.1299.0.0) | diet1.0$rawveg_TBSperday.1299.0.0 == -1 | diet1.0$rawveg_TBSperday.1299.0.0 == -3, "NA", 
					diet1.0$rawveg_TBSperday.1299.0.0)))

#table(diet1.0$freshfruit_piecesperday.1309.0.0,useNA="always")
diet1.0$freshfruit_piecesperday.1309.0.0_QT = as.numeric(ifelse(diet1.0$freshfruit_piecesperday.1309.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$freshfruit_piecesperday.1309.0.0) | diet1.0$freshfruit_piecesperday.1309.0.0 == -1 | diet1.0$freshfruit_piecesperday.1309.0.0 == -3, "NA", 
					diet1.0$freshfruit_piecesperday.1309.0.0)))

#table(diet1.0$driedfruit_piecesperday.1319.0.0,useNA="always")
diet1.0$driedfruit_piecesperday.1319.0.0_QT = as.numeric(ifelse(diet1.0$driedfruit_piecesperday.1319.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$driedfruit_piecesperday.1319.0.0) | diet1.0$driedfruit_piecesperday.1319.0.0 == -1 | diet1.0$driedfruit_piecesperday.1319.0.0 == -3, "NA", 
					diet1.0$driedfruit_piecesperday.1319.0.0)))

#table(diet1.0$bread_slicesperweek.1438.0.0,useNA="always")
diet1.0$bread_slicesperweek.1438.0.0_QT = as.numeric(ifelse(diet1.0$bread_slicesperweek.1438.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$bread_slicesperweek.1438.0.0) | diet1.0$bread_slicesperweek.1438.0.0 == -1 | diet1.0$bread_slicesperweek.1438.0.0 == -3, "NA", 
					diet1.0$bread_slicesperweek.1438.0.0)))

#table(diet1.0$cereal_bowlsperweek.1458.0.0,useNA="always")
diet1.0$cereal_bowlsperweek.1458.0.0_QT = as.numeric(ifelse(diet1.0$cereal_bowlsperweek.1458.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$cereal_bowlsperweek.1458.0.0) | diet1.0$cereal_bowlsperweek.1458.0.0 == -1 | diet1.0$cereal_bowlsperweek.1458.0.0 == -3, "NA", 
					diet1.0$cereal_bowlsperweek.1458.0.0)))

#table(diet1.0$tea_cupsperday.1488.0.0,useNA="always")
diet1.0$tea_cupsperday.1488.0.0_QT = as.numeric(ifelse(diet1.0$tea_cupsperday.1488.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$tea_cupsperday.1488.0.0) | diet1.0$tea_cupsperday.1488.0.0 == -1 | diet1.0$tea_cupsperday.1488.0.0 == -3, "NA", 
					diet1.0$tea_cupsperday.1488.0.0)))

#table(diet1.0$coffee_cupsperday.1498.0.0,useNA="always")
diet1.0$coffee_cupsperday.1498.0.0_QT = as.numeric(ifelse(diet1.0$coffee_cupsperday.1498.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$coffee_cupsperday.1498.0.0) | diet1.0$coffee_cupsperday.1498.0.0 == -1 | diet1.0$coffee_cupsperday.1498.0.0 == -3, "NA", 
					diet1.0$coffee_cupsperday.1498.0.0)))

#table(diet1.0$water_glassesperday.1528.0.0,useNA="always")
diet1.0$water_glassesperday.1528.0.0_QT = as.numeric(ifelse(diet1.0$water_glassesperday.1528.0.0 == -10, 0.5,
					ifelse(is.na(diet1.0$water_glassesperday.1528.0.0) | diet1.0$water_glassesperday.1528.0.0 == -1 | diet1.0$water_glassesperday.1528.0.0 == -3, "NA", 
					diet1.0$water_glassesperday.1528.0.0)))


### Fields 1568 1578 1588 1598 1608
### was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Field 1558
### it actually is for those that answered:
table(diet1.0$alcohol_overallfreq.1558.0.0,diet1.0$champagnewhitewine_glassesperweek.1578.0.0)
	## Once or twice a weekf
	## Three or four times a week
	## Daily or almost daily

### Fields 4407 4418 4429 4440 4451 4462
### was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Field 1558
### it actually is for those that answered:
table(diet1.0$alcohol_overallfreq.1558.0.0,diet1.0$redwine_glassespermonth.4407.0.0)
	## One to three times a month
	## Special occasions only

### Make one field of glasses per month based on glasses per week and glasses per month
### let's put them all on a month scale. 
### if they answered 1558
	## Once or twice a week
	## Three or four times a week
	## Daily or almost daily
		### -> glassesperweek * 4
### if they answered 1558
	## One to three times a month
	## Special occasions only
		### -> glasses per month
### if they answered 1558
	### Prefer not to answer
		### -> NA -> MICE imputation
### if they answered 1558
	### Never
		### -> 0

## to avoid missingness:
	## -1 meaning do not know will be the median of that group:
	## if glassesperweek = -1 or -3 -> median(of those that said once or twice a week or more)

#####################################################################
#table(diet1.0$champagnewhitewine_glassesperweek.1578.0.0,useNA="always")
diet1.0$champagnewhitewine_glassesperweek.1578.0.0_QT = as.numeric(ifelse(
							diet1.0$champagnewhitewine_glassesperweek.1578.0.0 == -1 | 
							diet1.0$champagnewhitewine_glassesperweek.1578.0.0 == -3, median(diet1.0$champagnewhitewine_glassesperweek.1578.0.0, na.rm=T), 
							diet1.0$champagnewhitewine_glassesperweek.1578.0.0))	
table(diet1.0$champagnewhitewine_glassesperweek.1578.0.0_QT, useNA="always")			
#table(diet1.0$champagnewhitewine_glassespermonth.4418.0.0,useNA="always")
diet1.0$champagnewhitewine_glassespermonth.4418.0.0_QT = as.numeric(ifelse(
							diet1.0$champagnewhitewine_glassespermonth.4418.0.0 == -1 | 
							diet1.0$champagnewhitewine_glassespermonth.4418.0.0 == -3, median(diet1.0$champagnewhitewine_glassespermonth.4418.0.0, na.rm=T),
							diet1.0$champagnewhitewine_glassespermonth.4418.0.0))
table(diet1.0$champagnewhitewine_glassespermonth.4418.0.0_QT, useNA="always")			
							
							
diet1.0$champagnewhitewine_glassespermonth.derived.0.0_QT = as.numeric(
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily") , 
								4*diet1.0$champagnewhitewine_glassesperweek.1578.0.0_QT, 
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only" ) , 
								diet1.0$champagnewhitewine_glassespermonth.4418.0.0_QT, 
							ifelse(
							diet1.0$alcohol_overallfreq.1558.0.0 == "Never",
								0, "NA"))))
table(diet1.0$champagnewhitewine_glassespermonth.derived.0.0_QT, useNA="always")	
#####################################################################
#table(diet1.0$redwine_glassesperweek.1568.0.0,useNA="always")
diet1.0$redwine_glassesperweek.1568.0.0_QT = as.numeric(ifelse(
							diet1.0$redwine_glassesperweek.1568.0.0 == -1 | 
							diet1.0$redwine_glassesperweek.1568.0.0 == -3, median(diet1.0$redwine_glassesperweek.1568.0.0, na.rm=T), 
							diet1.0$redwine_glassesperweek.1568.0.0))	
table(diet1.0$redwine_glassesperweek.1568.0.0_QT, useNA="always")			
#table(diet1.0$redwine_glassespermonth.4407.0.0,useNA="always")
diet1.0$redwine_glassespermonth.4407.0.0_QT = as.numeric(ifelse(
							diet1.0$redwine_glassespermonth.4407.0.0 == -1 | 
							diet1.0$redwine_glassespermonth.4407.0.0 == -3, median(diet1.0$redwine_glassespermonth.4407.0.0, na.rm=T),
							diet1.0$redwine_glassespermonth.4407.0.0))
table(diet1.0$redwine_glassespermonth.4407.0.0_QT, useNA="always")			
							
							
diet1.0$redwine_glassespermonth.derived.0.0_QT = as.numeric(
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily") , 
								4*diet1.0$redwine_glassesperweek.1568.0.0_QT, 
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only" ) , 
								diet1.0$redwine_glassespermonth.4407.0.0_QT, 
							ifelse(
							diet1.0$alcohol_overallfreq.1558.0.0 == "Never",
								0, "NA"))))
table(diet1.0$redwine_glassespermonth.derived.0.0_QT, useNA="always")
#####################################################################
#table(diet1.0$beercider_pintsperweek.1588.0.0,useNA="always")
diet1.0$beercider_pintsperweek.1588.0.0_QT = as.numeric(ifelse(
							diet1.0$beercider_pintsperweek.1588.0.0 == -1 | 
							diet1.0$beercider_pintsperweek.1588.0.0 == -3, median(diet1.0$beercider_pintsperweek.1588.0.0, na.rm=T), 
							diet1.0$beercider_pintsperweek.1588.0.0))	
table(diet1.0$beercider_pintsperweek.1588.0.0_QT, useNA="always")			
#table(diet1.0$beercider_pintspermonth.4429.0.0,useNA="always")
diet1.0$beercider_pintspermonth.4429.0.0_QT = as.numeric(ifelse(
							diet1.0$beercider_pintspermonth.4429.0.0 == -1 | 
							diet1.0$beercider_pintspermonth.4429.0.0 == -3, median(diet1.0$beercider_pintspermonth.4429.0.0, na.rm=T),
							diet1.0$beercider_pintspermonth.4429.0.0))
table(diet1.0$beercider_pintspermonth.4429.0.0_QT, useNA="always")			
							
							
diet1.0$beercider_pintspermonth.derived.0.0_QT = as.numeric(
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily") , 
								4*diet1.0$beercider_pintsperweek.1588.0.0_QT, 
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only" ) , 
								diet1.0$beercider_pintspermonth.4429.0.0_QT, 
							ifelse(
							diet1.0$alcohol_overallfreq.1558.0.0 == "Never",
								0, "NA"))))
table(diet1.0$beercider_pintspermonth.derived.0.0_QT, useNA="always")
#####################################################################
#table(diet1.0$spirits_measuresperweek.1598.0.0,useNA="always")
diet1.0$spirits_measuresperweek.1598.0.0_QT = as.numeric(ifelse(
							diet1.0$spirits_measuresperweek.1598.0.0 == -1 | 
							diet1.0$spirits_measuresperweek.1598.0.0 == -3, median(diet1.0$spirits_measuresperweek.1598.0.0, na.rm=T), 
							diet1.0$spirits_measuresperweek.1598.0.0))	
table(diet1.0$spirits_measuresperweek.1598.0.0_QT, useNA="always")			
#table(diet1.0$spirits_measurespermonth.4440.0.0,useNA="always")
diet1.0$spirits_measurespermonth.4440.0.0_QT = as.numeric(ifelse(
							diet1.0$spirits_measurespermonth.4440.0.0 == -1 | 
							diet1.0$spirits_measurespermonth.4440.0.0 == -3, median(diet1.0$spirits_measurespermonth.4440.0.0, na.rm=T),
							diet1.0$spirits_measurespermonth.4440.0.0))
table(diet1.0$spirits_measurespermonth.4440.0.0_QT, useNA="always")			
														
diet1.0$spirits_measurespermonth.derived.0.0_QT = as.numeric(
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily") , 
								4*diet1.0$spirits_measuresperweek.1598.0.0_QT, 
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only" ) , 
								diet1.0$spirits_measurespermonth.4440.0.0_QT, 
							ifelse(
							diet1.0$alcohol_overallfreq.1558.0.0 == "Never",
								0, "NA"))))
table(diet1.0$spirits_measurespermonth.derived.0.0_QT, useNA="always")
#####################################################################
#table(diet1.0$fortwine_glassesperweek.1608.0.0,useNA="always")
diet1.0$fortwine_glassesperweek.1608.0.0_QT = as.numeric(ifelse(
							diet1.0$fortwine_glassesperweek.1608.0.0 == -1 | 
							diet1.0$fortwine_glassesperweek.1608.0.0 == -3, median(diet1.0$fortwine_glassesperweek.1608.0.0, na.rm=T), 
							diet1.0$fortwine_glassesperweek.1608.0.0))	
table(diet1.0$fortwine_glassesperweek.1608.0.0_QT, useNA="always")			
#table(diet1.0$fortwine_glassespermonth.4451.0.0,useNA="always")
diet1.0$fortwine_glassespermonth.4451.0.0_QT = as.numeric(ifelse(
							diet1.0$fortwine_glassespermonth.4451.0.0 == -1 | 
							diet1.0$fortwine_glassespermonth.4451.0.0 == -3, median(diet1.0$fortwine_glassespermonth.4451.0.0, na.rm=T),
							diet1.0$fortwine_glassespermonth.4451.0.0))
table(diet1.0$fortwine_glassespermonth.4451.0.0_QT, useNA="always")			
							
							
diet1.0$fortwine_glassespermonth.derived.0.0_QT = as.numeric(
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily") , 
								4*diet1.0$fortwine_glassesperweek.1608.0.0_QT, 
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only" ) , 
								diet1.0$fortwine_glassespermonth.4451.0.0_QT, 
							ifelse(
							diet1.0$alcohol_overallfreq.1558.0.0 == "Never",
								0, "NA"))))
table(diet1.0$fortwine_glassespermonth.derived.0.0_QT, useNA="always")
#####################################################################
#table(diet1.0$otheralcohol_glassesperweek.5364.0.0,useNA="always")
diet1.0$otheralcohol_glassesperweek.5364.0.0_QT = as.numeric(ifelse(
							diet1.0$otheralcohol_glassesperweek.5364.0.0 == -1 | 
							diet1.0$otheralcohol_glassesperweek.5364.0.0 == -3, median(diet1.0$otheralcohol_glassesperweek.5364.0.0, na.rm=T), 
							diet1.0$otheralcohol_glassesperweek.5364.0.0))	
table(diet1.0$otheralcohol_glassesperweek.5364.0.0_QT, useNA="always")			
#table(diet1.0$otheralcohol_glassespermonth.4462.0.0,useNA="always")
diet1.0$otheralcohol_glassespermonth.4462.0.0_QT = as.numeric(ifelse(
							diet1.0$otheralcohol_glassespermonth.4462.0.0 == -1 | 
							diet1.0$otheralcohol_glassespermonth.4462.0.0 == -3, median(diet1.0$otheralcohol_glassespermonth.4462.0.0, na.rm=T),
							diet1.0$otheralcohol_glassespermonth.4462.0.0))
table(diet1.0$otheralcohol_glassespermonth.4462.0.0_QT, useNA="always")			
							
							
diet1.0$otheralcohol_glassespermonth.derived.0.0_QT = as.numeric(
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily") , 
								4*diet1.0$otheralcohol_glassesperweek.5364.0.0_QT, 
							ifelse((
							diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month" | 
							diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only" ) , 
								diet1.0$otheralcohol_glassespermonth.4462.0.0_QT, 
							ifelse(
							diet1.0$alcohol_overallfreq.1558.0.0 == "Never",
								0, "NA"))))
table(diet1.0$otheralcohol_glassespermonth.derived.0.0_QT, useNA="always")
#####################################################################
#Make 1 field of glasses of ANY alcohol per month

diet1.0$anyalcohol_glassespermonth.derived.0.0_QT = rowSums(subset(diet1.0, select = c(champagnewhitewine_glassespermonth.derived.0.0_QT, redwine_glassespermonth.derived.0.0_QT, 
						beercider_pintspermonth.derived.0.0_QT, spirits_measurespermonth.derived.0.0_QT,
						fortwine_glassespermonth.derived.0.0_QT, otheralcohol_glassespermonth.derived.0.0_QT)), na.rm = TRUE)
table(diet1.0$anyalcohol_glassespermonth.derived.0.0_QT, useNA="always")
#####################################################################
				
### Data structure as follows:
### Data-coding 100377
### Categorical Variable -> Continuous
	#Never -> 0
	#Less than once a week -> 20
	#Once a week -> 52
	#2-4 times a week -> 156
	#5-6 times a week -> 286
	#Once or more daily -> 365
	#do not know -> set to NA
	#prefer not to answer -> set to NA

#table(diet1.0$oilyfish_overallfreq.1329.0.0,useNA="always")		
diet1.0$oilyfish_overallfreq.1329.0.0_QT = as.numeric(ifelse(diet1.0$oilyfish_overallfreq.1329.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$oilyfish_overallfreq.1329.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$oilyfish_overallfreq.1329.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$oilyfish_overallfreq.1329.0.0 == "Once a week", 52, 
					ifelse(diet1.0$oilyfish_overallfreq.1329.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$oilyfish_overallfreq.1329.0.0 == "Never", 0, "NA")))))))
					
			
#table(diet1.0$nonoilyfish_overallfreq.1339.0.0,useNA="always")
diet1.0$nonoilyfish_overallfreq.1339.0.0_QT = as.numeric(ifelse(diet1.0$nonoilyfish_overallfreq.1339.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$nonoilyfish_overallfreq.1339.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$nonoilyfish_overallfreq.1339.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$nonoilyfish_overallfreq.1339.0.0 == "Once a week", 52, 
					ifelse(diet1.0$nonoilyfish_overallfreq.1339.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$nonoilyfish_overallfreq.1339.0.0 == "Never", 0, "NA")))))))
					
#table(diet1.0$processmeat_overallfreq.1349.0.0,useNA="always")
diet1.0$processmeat_overallfreq.1349.0.0_QT = as.numeric(ifelse(diet1.0$processmeat_overallfreq.1349.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$processmeat_overallfreq.1349.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$processmeat_overallfreq.1349.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$processmeat_overallfreq.1349.0.0 == "Once a week", 52, 
					ifelse(diet1.0$processmeat_overallfreq.1349.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$processmeat_overallfreq.1349.0.0 == "Never", 0, "NA")))))))
					
#table(diet1.0$poultry_overallfreq.1359.0.0,useNA="always")
diet1.0$poultry_overallfreq.1359.0.0_QT = as.numeric(ifelse(diet1.0$poultry_overallfreq.1359.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$poultry_overallfreq.1359.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$poultry_overallfreq.1359.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$poultry_overallfreq.1359.0.0 == "Once a week", 52, 
					ifelse(diet1.0$poultry_overallfreq.1359.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$poultry_overallfreq.1359.0.0 == "Never", 0, "NA")))))))

#table(diet1.0$beef_overallfreq.1369.0.0,useNA="always")
diet1.0$beef_overallfreq.1369.0.0_QT = as.numeric(ifelse(diet1.0$beef_overallfreq.1369.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$beef_overallfreq.1369.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$beef_overallfreq.1369.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$beef_overallfreq.1369.0.0 == "Once a week", 52, 
					ifelse(diet1.0$beef_overallfreq.1369.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$beef_overallfreq.1369.0.0 == "Never", 0, "NA")))))))

#table(diet1.0$lambmutton_overallfreq.1379.0.0,useNA="always")
diet1.0$lambmutton_overallfreq.1379.0.0_QT = as.numeric(ifelse(diet1.0$lambmutton_overallfreq.1379.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$lambmutton_overallfreq.1379.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$lambmutton_overallfreq.1379.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$lambmutton_overallfreq.1379.0.0 == "Once a week", 52, 
					ifelse(diet1.0$lambmutton_overallfreq.1379.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$lambmutton_overallfreq.1379.0.0 == "Never", 0, "NA")))))))

#table(diet1.0$pork_overallfreq.1389.0.0,useNA="always")
diet1.0$pork_overallfreq.1389.0.0_QT = as.numeric(ifelse(diet1.0$pork_overallfreq.1389.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$pork_overallfreq.1389.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$pork_overallfreq.1389.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$pork_overallfreq.1389.0.0 == "Once a week", 52, 
					ifelse(diet1.0$pork_overallfreq.1389.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$pork_overallfreq.1389.0.0 == "Never", 0, "NA")))))))

#table(diet1.0$cheese_overallfreq.1408.0.0,useNA="always")
diet1.0$cheese_overallfreq.1408.0.0_QT = as.numeric(ifelse(diet1.0$cheese_overallfreq.1408.0.0 == "Once or more daily", 365, 
					ifelse(diet1.0$cheese_overallfreq.1408.0.0 == "5-6 times a week", 286, 
					ifelse(diet1.0$cheese_overallfreq.1408.0.0 == "2-4 times a week", 156, 
					ifelse(diet1.0$cheese_overallfreq.1408.0.0 == "Once a week", 52, 
					ifelse(diet1.0$cheese_overallfreq.1408.0.0 == "Less than once a week", 20, 
					ifelse(diet1.0$cheese_overallfreq.1408.0.0 == "Never", 0, "NA")))))))

### Data structure as follows:
### Data-coding 100394
### Categorical Variable -> Continuous
	# Always -> 10
	# Usually -> 7
	# Sometimes -> 3
	# Never/rarely -> 0
	# Prefer not to answer -> NA
	# NA

#table(diet1.0$doyouaddsalt.1478.0.0,useNA="always")
diet1.0$doyouaddsalt.1478.0.0_QT = as.numeric(ifelse(diet1.0$doyouaddsalt.1478.0.0 == "Always", 10, 
					ifelse(diet1.0$doyouaddsalt.1478.0.0 == "Usually", 7, 
					ifelse(diet1.0$doyouaddsalt.1478.0.0 == "Sometimes", 3, 
					ifelse(diet1.0$doyouaddsalt.1478.0.0 == "Never/rarely", 0, "NA")))))		

### Data structure as follows:
### Data-coding 100398
### Categorical Variable -> Continuous
	# Very hot -> 10
	# Hot -> 7
	# Warm -> 3
	# Do not drink hot drinks -> 0
	# Prefer not to answer -> set to NA
	# NA

#table(diet1.0$hotdrinktemp.1518.0.0,useNA="always")
diet1.0$hotdrinktemp.1518.0.0_QT = as.numeric(ifelse(diet1.0$hotdrinktemp.1518.0.0 == "Very hot", 10, 
					ifelse(diet1.0$hotdrinktemp.1518.0.0 == "Hot", 7, 
					ifelse(diet1.0$hotdrinktemp.1518.0.0 == "Warm", 3, 
					ifelse(diet1.0$hotdrinktemp.1518.0.0 == "Do not drink hot drinks", 0, "NA")))))

### Data structure as follows:
### Data-coding 100402
### Categorical Variable -> Continuous
	# Daily or almost daily -> 300
	# Three or four times a week -> 182
	# Once or twice a week -> 104
	# One to three times a month -> 24
	# Special occasions only -> 6
	# Never -> 0
	# Prefer not to answer -> set to NA
	# NA

#table(diet1.0$alcohol_overallfreq.1558.0.0,useNA="always")
diet1.0$alcohol_overallfreq.1558.0.0_QT = as.numeric(ifelse(diet1.0$alcohol_overallfreq.1558.0.0 == "Daily or almost daily", 300, 
						ifelse(diet1.0$alcohol_overallfreq.1558.0.0 == "Three or four times a week", 182, 
						ifelse(diet1.0$alcohol_overallfreq.1558.0.0 == "Once or twice a week", 104, 
						ifelse(diet1.0$alcohol_overallfreq.1558.0.0 == "One to three times a month", 24, 
						ifelse(diet1.0$alcohol_overallfreq.1558.0.0 == "Special occasions only", 6, 
						ifelse(diet1.0$alcohol_overallfreq.1558.0.0 == "Never", 0, "NA")))))))

### Data structure as follows:
### Data-coding 90
### Categorical Variable -> Binary x2
	# Current
	# Previous
	# Never
	# Prefer not to answer
	# NA

#table(diet1.0$alcoholdrinkerstatus.20117.0.0,useNA="always")

	# Never (0) vs. Current + Previous (1)
		
diet1.0$alcoholdrinkerstatus.20117.0.0_bin = as.numeric(ifelse(diet1.0$alcoholdrinkerstatus.20117.0.0 == "Never", 0, ifelse(diet1.0$alcoholdrinkerstatus.20117.0.0 == "Previous" | diet1.0$alcoholdrinkerstatus.20117.0.0 == "Current", 1, "NA")))
#table(diet1.0$alcoholdrinkerstatus.20117.0.0_bin,useNA="always")


### this will likely impute previous -> never as the correlation with 0 drinks above will be high
### that's ok. better than making a new variable for this.

	# Never (0) vs. Current only (1)

diet1.0$alcoholdrinkerstatus.20117.0.0_bin2 = as.numeric(ifelse(diet1.0$alcoholdrinkerstatus.20117.0.0 == "Never", 0, ifelse(diet1.0$alcoholdrinkerstatus.20117.0.0 == "Current", 1, "NA")))
#table(diet1.0$alcoholdrinkerstatus.20117.0.0_bin2,useNA="always")

### Data structure as follows:
### Data-coding 100352
### Categorical Variable with:
	# Yes
	# No
	# Prefer not to answer
	# NA

#table(diet1.0$alcohol_formerdrinker_amongcurrentnondrinkers.3731.0.0,useNA="always")
# no additional information not captured in diet1.0$alcoholdrinkerstatus.20117.0.0


### Data structure as follows:
### Data-coding 100416
### Categorical Variable -> Binary x2
### Categorical Variable -> Continuous x1
	# Yes
	# It varies
	# No
	# Do not know
	# Prefer not to answer
	# NA

#table(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0,useNA="always")

	# No (0) vs. Yes + It varies (1)

diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin = as.numeric(ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "No", 0, ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "Yes" | diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "It varies", 1, "NA")))
#table(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin,useNA="always")

	# No (0) vs. Yes (1)

diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin2 = as.numeric(ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "No", 0, ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "Yes", 1, "NA")))
#table(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin2,useNA="always")

	# No (0), It varies (1), Yes (2) 

diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_QT3 = as.numeric(ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "No", 0, 
							ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "Yes", 2,
							ifelse(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0 == "It varies", 1, "NA"))))
#table(diet1.0$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_QT3,useNA="always")

### Data structure as follows:
### Data-coding 100385
### Categorical Variable with:
	# Eggs or foods containing eggs (1) vs. I eat all of the above (0)
	# Dairy products (1) vs. I eat all of the above (0)
	# Wheat products (1) vs. I eat all of the above (0)
	# Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)
	# I eat all of the above : if selected no other choices were allowed
	# prefer not to answer : if selected no other choices were allowed -> set to NA


#table(diet1.0$nevereatcategories.6144.0.0,useNA="always")
#table(diet1.0$nevereatcategories.6144.0.1,useNA="always")
#table(diet1.0$nevereatcategories.6144.0.2,useNA="always")
#table(diet1.0$nevereatcategories.6144.0.3,useNA="always")

diet1.0$nevereatcategories.6144.0.combined = paste(diet1.0$nevereatcategories.6144.0.0, diet1.0$nevereatcategories.6144.0.1, diet1.0$nevereatcategories.6144.0.2,
							diet1.0$nevereatcategories.6144.0.3, sep =":")

#table(diet1.0$nevereatcategories.6144.0.combined,useNA="always")

# Eggs or foods containing eggs (1) vs. I eat all of the above (0)
diet1.0$nevereatcategories.6144.0.0_bin = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin,useNA="always")
# Eggs or foods containing eggs (1) vs. everyone else (0)
diet1.0$nevereatcategories.6144.0.0_bin2 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin2,useNA="always")

# Dairy products (1) vs. I eat all of the above (0)
diet1.0$nevereatcategories.6144.0.0_bin3 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin3,useNA="always")

# Dairy products (1)  vs. everyone else (0)

diet1.0$nevereatcategories.6144.0.0_bin4 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin4,useNA="always")

# Wheat products (1) vs. I eat all of the above (0)
diet1.0$nevereatcategories.6144.0.0_bin5 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", "NA", 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin5,useNA="always")

# Wheat products (1)  vs. everyone else (0)
diet1.0$nevereatcategories.6144.0.0_bin6 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 0, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin6,useNA="always")
	
# Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)	
diet1.0$nevereatcategories.6144.0.0_bin7 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin7,useNA="always")

# Sugar or foods/drinks containing sugar (1) vs. everyone else (0))	
diet1.0$nevereatcategories.6144.0.0_bin8 = as.numeric(
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.0$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.0$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.0$nevereatcategories.6144.0.0_bin8,useNA="always")	
	
	
diet1.0$nevereatcategories.6144.0.combined = NULL	

### Data structure as follows:
### Data-coding 100387
### Categorical Variable with:
	# Full cream
	# Semi-skimmed
	# Skimmed
	# Soya
	# Never/rarely have milk
	# Other type of milk
	# Do not know
	# Prefer not to answer
	# NA

# 1binary: milk x3 vs. never
diet1.0$milk_typeused.1418.0.0_bin = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", "NA", 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin,useNA="always")

# 2binary: milk x5 vs. never
diet1.0$milk_typeused.1418.0.0_bin2 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 1, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin2,useNA="always")

# 3continuous: never (0), skimmed (1), semi-skimmed (2), full cream (3)
diet1.0$milk_typeused.1418.0.0_QT3 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 3,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 2,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", "NA", 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_QT3,useNA="always")

# 4full cream (1) vs. never (0)
diet1.0$milk_typeused.1418.0.0_bin4 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", "NA", 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin4,useNA="always")

# 5full cream (1) vs. all other milk (0)
diet1.0$milk_typeused.1418.0.0_bin5 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 0, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin5,useNA="always")

# 6semi (1) vs. never (0)
diet1.0$milk_typeused.1418.0.0_bin6 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", "NA", 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin6,useNA="always")

# 7semi (1) vs. all other milk (0)
diet1.0$milk_typeused.1418.0.0_bin7 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 0, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin7,useNA="always")

# 8skim (1) vs. never (0)
diet1.0$milk_typeused.1418.0.0_bin8 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", "NA", 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin8,useNA="always")

# 9skim (1) vs. all other milk (0)
diet1.0$milk_typeused.1418.0.0_bin9 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 0, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin9,useNA="always")

# 10soy (1) vs. never (0)
diet1.0$milk_typeused.1418.0.0_bin10 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", "NA", 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin10,useNA="always")

# 11soy (1) vs. all other milk (0)
diet1.0$milk_typeused.1418.0.0_bin11 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 1,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 0, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin11,useNA="always")

# 12other (1) vs. never (0)
diet1.0$milk_typeused.1418.0.0_bin12 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 1, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin12,useNA="always")

# 13other (1) vs. all other milk (0)
diet1.0$milk_typeused.1418.0.0_bin13 = as.numeric(
ifelse(diet1.0$milk_typeused.1418.0.0 == "Full cream", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Semi-skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Skimmed", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Soya", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Other type of milk", 1, 
ifelse(diet1.0$milk_typeused.1418.0.0 == "Never/rarely have milk", 0,
ifelse(diet1.0$milk_typeused.1418.0.0 == "Do not know", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$milk_typeused.1418.0.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.0$milk_typeused.1418.0.0_bin13,useNA="always")

### Data structure as follows:
### Data-coding 100388
### Categorical Variable with:
	# Butter/spreadable butter
	# Flora Pro-Active/Benecol
	# Other type of spread/margarine
	# Never/rarely use spread
	# Do not know
	# Prefer not to answer
	# NA
### Data structure as follows:
### Data-coding 100389
### Categorical Variable with:
	# Hard (block) margarine
	# Soft (tub) margarine
	# Flora Pro-Active or Benecol
	# Olive oil based spread (eg: Bertolli)
	# Polyunsaturated/sunflower oil based spread (eg: Flora)
	# Other low or reduced fat spread
	# Other type of spread/margarine
	# Do not know
	# Prefer not to answer
	# NA

## Field 2654 was collected from participants who indicated they mainly use another type of spread/margarine 
## rather than butter/spreadable butter or do not know what they use, as defined by their answers to Field 1428
table(diet1.0$spread_typeused.1428.0.0,diet1.0$nonbutterspread_typeused.2654.0.0)


diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined = paste(diet1.0$spread_typeused.1428.0.0, diet1.0$nonbutterspread_typeused.2654.0.0, sep =":")
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,useNA="always")	

## butter like spreads
	#Butter/spreadable butter:NA 

	#Do not know:Hard (block) margarine 
	#Other type of spread/margarine:Hard (block) margarine 

	#Do not know:Soft (tub) margarine 
	#Other type of spread/margarine:Soft (tub) margarine 

	#Do not know:Flora Pro-Active or Benecol
	#Flora Pro-Active/Benecol:NA
	#Other type of spread/margarine:Flora Pro-Active or Benecol 

## oil like spreads
	#Do not know:Olive oil based spread (eg: Bertolli) 
	#Other type of spread/margarine:Olive oil based spread (eg: Bertolli) 

	#Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora) 
	#Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora) 

## other low fat/reduced
	#Do not know:Other low or reduced fat spread 
	#Other type of spread/margarine:Other low or reduced fat spread  

## other
	#Other type of spread/margarine:Other type of spread/margarine 

## never
	#Never/rarely use spread:NA 

## NA
	#Other type of spread/margarine:Do not know
	#Do not know:Do not know
	#Do not know:Prefer not to answer
	#Do not know:Other type of spread/margarine 
	#Prefer not to answer:NA
	#Other type of spread/margarine:Prefer not to answer 
	#NA:NA 


#1 All vs. Never

diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1,useNA="always")

#2 Butter/Margarine vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2,useNA="always")

#3 Oil-based vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3,useNA="always")

#4 Butter (with flora proactive)vs. Oil

diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4,useNA="always")

#5 Butter (without flora proactive)vs. Oil
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5,useNA="always")

#6 Butter vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6,useNA="always")

#7 Butter vs. Other
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7,useNA="always")

#8 Hard (block) margarine vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8,useNA="always")

#9 Hard (block) margarine vs. Other
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9,useNA="always")

#10 Soft (tub) margarine
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10,useNA="always")

#11 Soft (tub) margarine
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11,useNA="always")

#12 Flora vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12,useNA="always")

#13 Flora vs. Other
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13,useNA="always")

#14 Olive oil based spread (eg: Bertolli) vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14,useNA="always")

#15 Olive oil based spread (eg: Bertolli) vs. Other
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15,useNA="always")

#16 Flora oil/sunflower oil vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16,useNA="always")

#17 Flora oil/sunflower oil vs. Other
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17,useNA="always")

#18 Other low or reduced fat spread vs. Never
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18,useNA="always")

#19 Other low or reduced fat spread vs. Other
diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19 = as.numeric(
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19,useNA="always")

diet1.0$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined = NULL

### Field 1448 was collected from all participants except those who indicated that they do not eat bread or less than one slice each week, as defined by their answers to Field 1438


### Data structure as follows:
### Data-coding 100391
### Categorical Variable with:
	# White
	# Brown
	# Wholemeal or wholegrain
	# Other type of bread
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.0$bread_typeused.1448.0.0,useNA="always")

##1 White vs Other
diet1.0$bread_typeused.1448.0.0_bin = as.numeric(
ifelse(diet1.0$bread_typeused.1448.0.0 == "White", 1,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Brown", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Other type of bread", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Do not know", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$bread_typeused.1448.0.0_bin,useNA="always")

##2 Brown vs. Other
diet1.0$bread_typeused.1448.0.0_bin2 = as.numeric(
ifelse(diet1.0$bread_typeused.1448.0.0 == "White", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Brown", 1,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Other type of bread", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Do not know", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$bread_typeused.1448.0.0_bin2,useNA="always")

##3 Wholemeal vs. Other
diet1.0$bread_typeused.1448.0.0_bin3 = as.numeric(
ifelse(diet1.0$bread_typeused.1448.0.0 == "White", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Brown", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Wholemeal or wholegrain", 1,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Other type of bread", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Do not know", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$bread_typeused.1448.0.0_bin3,useNA="always")

##4 White vs. Brown + Wholemeal
diet1.0$bread_typeused.1448.0.0_bin4 = as.numeric(
ifelse(diet1.0$bread_typeused.1448.0.0 == "White", 1,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Brown", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Other type of bread", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Do not know", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$bread_typeused.1448.0.0_bin4,useNA="always")

##5 White + Brown vs. Wholemeal
diet1.0$bread_typeused.1448.0.0_bin5 = as.numeric(
ifelse(diet1.0$bread_typeused.1448.0.0 == "White", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Brown", 0,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Wholemeal or wholegrain", 1,
ifelse(diet1.0$bread_typeused.1448.0.0 == "Other type of bread", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Do not know", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$bread_typeused.1448.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$bread_typeused.1448.0.0_bin5,useNA="always")

### Field 1468 was collected from all participants except those who indicated that they do not eat cereal or less than one bowl of cereal each week, as defined by their answers to Field 1458

### Data structure as follows:
### Data-coding 100393
### Categorical Variable with:
	# Biscuit cereal (e.g. Weetabix) 
	# Bran cereal (e.g. All Bran, Branflakes)
	# Oat cereal (e.g. Ready Brek, porridge)
	# Muesli
	# Other (e.g. Cornflakes, Frosties)
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.0$cereal_typeused.1468.0.0,useNA="always")

# Biscuit vs. All Other
diet1.0$cereal_typeused.1468.0.0_bin = as.numeric(
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Biscuit cereal (e.g. Weetabix)", 1,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Muesli", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Do not know", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "NA", "NA",
"NA")))))))))
#table(diet1.0$cereal_typeused.1468.0.0_bin,useNA="always")

# Bran vs. All Other
diet1.0$cereal_typeused.1468.0.0_bin2 = as.numeric(
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)", 1,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Muesli", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Do not know", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "NA", "NA",
"NA")))))))))
#table(diet1.0$cereal_typeused.1468.0.0_bin2,useNA="always")

# Oat vs. All Other
diet1.0$cereal_typeused.1468.0.0_bin3 = as.numeric(
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Oat cereal (e.g. Ready Brek, porridge)", 1,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Muesli", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Do not know", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "NA", "NA",
"NA")))))))))
#table(diet1.0$cereal_typeused.1468.0.0_bin3,useNA="always")

# Muesli vs. All Other
diet1.0$cereal_typeused.1468.0.0_bin4 = as.numeric(
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Muesli", 1,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Do not know", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "NA", "NA",
"NA")))))))))
#table(diet1.0$cereal_typeused.1468.0.0_bin4,useNA="always")

# Other vs. All Other
diet1.0$cereal_typeused.1468.0.0_bin5 = as.numeric(
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Muesli", 0,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Other (e.g. Cornflakes, Frosties)", 1,
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Do not know", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$cereal_typeused.1468.0.0 == "NA", "NA",
"NA")))))))))
#table(diet1.0$cereal_typeused.1468.0.0_bin5,useNA="always")

### Field 1508 was collected from participants who indicated that they drank at least 1 cup of coffee each day or less than one cup each day, as defined by their answers to Field 1498

### Data structure as follows:
### Data-coding 100397
### Categorical Variable with:
	# Ground coffee (include espresso, filter etc)
	# Instant coffee
	# Decaffeinated coffee (any type)
	# Other type of coffee
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.0$coffee_typeused.1508.0.0,useNA="always")

# Ground vs. Other
diet1.0$coffee_typeused.1508.0.0_bin = as.numeric(
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Ground coffee (include espresso, filter etc)", 1,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Instant coffee", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Decaffeinated coffee (any type)", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Other type of coffee", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Do not know", "NA",
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$coffee_typeused.1508.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$coffee_typeused.1508.0.0_bin,useNA="always")

# instant_ground vs. other
diet1.0$coffee_typeused.1508.0.0_bin2 = as.numeric(
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Ground coffee (include espresso, filter etc)", 1,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Instant coffee", 1,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Decaffeinated coffee (any type)", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Other type of coffee", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Do not know", "NA",
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$coffee_typeused.1508.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$coffee_typeused.1508.0.0_bin2,useNA="always")

# decaff vs. other
diet1.0$coffee_typeused.1508.0.0_bin3 = as.numeric(
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Ground coffee (include espresso, filter etc)", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Instant coffee", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Decaffeinated coffee (any type)", 1,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Other type of coffee", 0,
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Do not know", "NA",
ifelse(diet1.0$coffee_typeused.1508.0.0 == "Prefer not to answer", "NA",
ifelse(diet1.0$coffee_typeused.1508.0.0 == "NA", "NA",
"NA"))))))))
#table(diet1.0$coffee_typeused.1508.0.0_bin3,useNA="always")


####################
#save diet1.0 dataset
colnames(diet1.0)
save_diet1.0 = diet1.0[,c(1,2,79, 124:221)]
write.table(save_diet1.0, "/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1.0_07062018update", col.names = TRUE, row.names = FALSE, quote = FALSE, sep ="\t")



##########################################################################################################################
##########################################################################################################################
################### ################### ################### ################### ################### ################### ###################
#load in and merge diet1.1
diet1.1_8785 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_diet1.1", verbose=TRUE, data.table=FALSE)
diet1.1_6184 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_diet1.1", verbose=TRUE, data.table=FALSE)
diet1.1_7052 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_7052_diet1.1", verbose=TRUE, data.table=FALSE)


diet1.1tmp = merge(diet1.1_6184, diet1.1_8785, by = "ID", all = T)
diet1.1 = merge(diet1.1tmp, diet1.1_7052, by = "ID", all = T)

###################
#create appropriate age variables per instance
#DIET1: date_assessmentcentre.53.1.0 -> birth year + birth month

## split DATE -> 3 columns of year, month, day
library(stringr)

x = str_split_fixed(diet1.1$date_assessmentcentre.53.1.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("DIET1_date_year.1.0", "DIET1_date_month.1.0", "DIET1_date_day.1.0")
diet1.1 = cbind(diet1.1,x)

#table(diet1.1$month_of_birth.52,useNA="always")

##convert month in text -> numeric

diet1.1$month_of_birth_numeric.52 = ifelse(diet1.1$month_of_birth.52 == "January", 1, 
				 ifelse(diet1.1$month_of_birth.52 == "February", 2, 
				 ifelse(diet1.1$month_of_birth.52 == "March", 3, 
				 ifelse(diet1.1$month_of_birth.52 == "April", 4, 
				 ifelse(diet1.1$month_of_birth.52 == "May", 5, 
				 ifelse(diet1.1$month_of_birth.52 == "June", 6, 
				 ifelse(diet1.1$month_of_birth.52 == "July", 7, 
				 ifelse(diet1.1$month_of_birth.52 == "August", 8, 
				 ifelse(diet1.1$month_of_birth.52 == "September", 9, 
				 ifelse(diet1.1$month_of_birth.52 == "October", 10, 
				 ifelse(diet1.1$month_of_birth.52 == "November", 11, 12)))))))))))

##create age variable

diet1.1$DIET1_age_months.1.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$month_of_birth_numeric.52, (diet1.1$DIET1_date_year.1.0 - diet1.1$year_of_birth.34)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$month_of_birth_numeric.52),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$year_of_birth.34)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$month_of_birth_numeric.52)))

###################
#exclusions to diet1.1

#pregnant
#table(diet1.1$pregnant.3140.1.0, useNA="always")
diet1.1 = subset(diet1.1, pregnant.3140.1.0 != "Yes" | is.na(pregnant.3140.1.0))

#non-esrd
diet1.1 = subset(diet1.1, !ID %in% esrd)

#non-cancer diagnosis <=1 year from current age
#(ideally would be from remission...)
diet1.1 = merge(diet1.1,cancer, by ="ID",all.x = T)

diet1.1$DIET1_monthsfromcancerdiag.0.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.0.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.0.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.0.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.0.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.0.0)))
diet1.1$DIET1_monthsfromcancerdiag.1.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.1.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.1.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.1.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.1.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.1.0)))
diet1.1$DIET1_monthsfromcancerdiag.2.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.2.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.2.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.2.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.2.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.2.0)))
diet1.1$DIET1_monthsfromcancerdiag.3.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.3.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.3.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.3.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.3.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.3.0)))
diet1.1$DIET1_monthsfromcancerdiag.4.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.4.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.4.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.4.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.4.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.4.0)))
diet1.1$DIET1_monthsfromcancerdiag.5.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.5.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.5.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.5.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.5.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.5.0)))
diet1.1$DIET1_monthsfromcancerdiag.6.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.6.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.6.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.6.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.6.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.6.0)))
diet1.1$DIET1_monthsfromcancerdiag.7.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.7.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.7.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.7.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.7.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.7.0)))
diet1.1$DIET1_monthsfromcancerdiag.8.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.8.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.8.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.8.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.8.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.8.0)))
diet1.1$DIET1_monthsfromcancerdiag.9.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.9.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.9.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.9.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.9.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.9.0)))
diet1.1$DIET1_monthsfromcancerdiag.10.0 = ifelse(diet1.1$DIET1_date_month.1.0 >= diet1.1$date_cancerdiag_month.40005.10.0, (diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.10.0)*12 + (diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.10.0),
								  	(diet1.1$DIET1_date_year.1.0 - diet1.1$date_cancerdiag_year.40005.10.0)*12 - abs((diet1.1$DIET1_date_month.1.0 - diet1.1$date_cancerdiag_month.40005.10.0)))

diet1.1 = subset(diet1.1, (DIET1_monthsfromcancerdiag.0.0 > 12 | DIET1_monthsfromcancerdiag.0.0 < 0 | is.na(DIET1_monthsfromcancerdiag.0.0)) &
			  (DIET1_monthsfromcancerdiag.1.0 > 12 | DIET1_monthsfromcancerdiag.1.0 < 0 | is.na(DIET1_monthsfromcancerdiag.1.0)) &
			  (DIET1_monthsfromcancerdiag.2.0 > 12 | DIET1_monthsfromcancerdiag.2.0 < 0 | is.na(DIET1_monthsfromcancerdiag.2.0)) &
			  (DIET1_monthsfromcancerdiag.3.0 > 12 | DIET1_monthsfromcancerdiag.3.0 < 0 | is.na(DIET1_monthsfromcancerdiag.3.0)) &
			  (DIET1_monthsfromcancerdiag.4.0 > 12 | DIET1_monthsfromcancerdiag.4.0 < 0 | is.na(DIET1_monthsfromcancerdiag.4.0)) &
			  (DIET1_monthsfromcancerdiag.5.0 > 12 | DIET1_monthsfromcancerdiag.5.0 < 0 | is.na(DIET1_monthsfromcancerdiag.5.0)) &
			  (DIET1_monthsfromcancerdiag.6.0 > 12 | DIET1_monthsfromcancerdiag.6.0 < 0 | is.na(DIET1_monthsfromcancerdiag.6.0)) &
			  (DIET1_monthsfromcancerdiag.7.0 > 12 | DIET1_monthsfromcancerdiag.7.0 < 0 | is.na(DIET1_monthsfromcancerdiag.7.0)) &
			  (DIET1_monthsfromcancerdiag.8.0 > 12 | DIET1_monthsfromcancerdiag.8.0 < 0 | is.na(DIET1_monthsfromcancerdiag.8.0)) &
			  (DIET1_monthsfromcancerdiag.9.0 > 12 | DIET1_monthsfromcancerdiag.9.0 < 0 | is.na(DIET1_monthsfromcancerdiag.9.0)) &
			  (DIET1_monthsfromcancerdiag.10.0 > 12 | DIET1_monthsfromcancerdiag.10.0 < 0 | is.na(DIET1_monthsfromcancerdiag.10.0)))
			  
###################
#create instance field
diet1.1$instance = "diet1.1"
###################
#create usable phenotypes. 



### Data structure as follows:
### Data-coding 100373
### Continuous Variable with:
	# -10 = <1
	# -1 = do not know
	# -3 = prefer not to answer
	
	#convert -10 = .5
	#convert -1 & -3 = "NA"

#table(diet1.1$cookedveg_TBSperday.1289.1.0,useNA="always")
diet1.1$cookedveg_TBSperday.1289.1.0_QT = as.numeric(ifelse(diet1.1$cookedveg_TBSperday.1289.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$cookedveg_TBSperday.1289.1.0) | diet1.1$cookedveg_TBSperday.1289.1.0 == -1 | diet1.1$cookedveg_TBSperday.1289.1.0 == -3, "NA", 
					diet1.1$cookedveg_TBSperday.1289.1.0)))
	
#table(diet1.1$rawveg_TBSperday.1299.1.0,useNA="always")
diet1.1$rawveg_TBSperday.1299.1.0_QT = as.numeric(ifelse(diet1.1$rawveg_TBSperday.1299.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$rawveg_TBSperday.1299.1.0) | diet1.1$rawveg_TBSperday.1299.1.0 == -1 | diet1.1$rawveg_TBSperday.1299.1.0 == -3, "NA", 
					diet1.1$rawveg_TBSperday.1299.1.0)))

#table(diet1.1$freshfruit_piecesperday.1309.1.0,useNA="always")
diet1.1$freshfruit_piecesperday.1309.1.0_QT = as.numeric(ifelse(diet1.1$freshfruit_piecesperday.1309.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$freshfruit_piecesperday.1309.1.0) | diet1.1$freshfruit_piecesperday.1309.1.0 == -1 | diet1.1$freshfruit_piecesperday.1309.1.0 == -3, "NA", 
					diet1.1$freshfruit_piecesperday.1309.1.0)))

#table(diet1.1$driedfruit_piecesperday.1319.1.0,useNA="always")
diet1.1$driedfruit_piecesperday.1319.1.0_QT = as.numeric(ifelse(diet1.1$driedfruit_piecesperday.1319.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$driedfruit_piecesperday.1319.1.0) | diet1.1$driedfruit_piecesperday.1319.1.0 == -1 | diet1.1$driedfruit_piecesperday.1319.1.0 == -3, "NA", 
					diet1.1$driedfruit_piecesperday.1319.1.0)))

#table(diet1.1$bread_slicesperweek.1438.1.0,useNA="always")
diet1.1$bread_slicesperweek.1438.1.0_QT = as.numeric(ifelse(diet1.1$bread_slicesperweek.1438.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$bread_slicesperweek.1438.1.0) | diet1.1$bread_slicesperweek.1438.1.0 == -1 | diet1.1$bread_slicesperweek.1438.1.0 == -3, "NA", 
					diet1.1$bread_slicesperweek.1438.1.0)))

#table(diet1.1$cereal_bowlsperweek.1458.1.0,useNA="always")
diet1.1$cereal_bowlsperweek.1458.1.0_QT = as.numeric(ifelse(diet1.1$cereal_bowlsperweek.1458.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$cereal_bowlsperweek.1458.1.0) | diet1.1$cereal_bowlsperweek.1458.1.0 == -1 | diet1.1$cereal_bowlsperweek.1458.1.0 == -3, "NA", 
					diet1.1$cereal_bowlsperweek.1458.1.0)))

#table(diet1.1$tea_cupsperday.1488.1.0,useNA="always")
diet1.1$tea_cupsperday.1488.1.0_QT = as.numeric(ifelse(diet1.1$tea_cupsperday.1488.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$tea_cupsperday.1488.1.0) | diet1.1$tea_cupsperday.1488.1.0 == -1 | diet1.1$tea_cupsperday.1488.1.0 == -3, "NA", 
					diet1.1$tea_cupsperday.1488.1.0)))

#table(diet1.1$coffee_cupsperday.1498.1.0,useNA="always")
diet1.1$coffee_cupsperday.1498.1.0_QT = as.numeric(ifelse(diet1.1$coffee_cupsperday.1498.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$coffee_cupsperday.1498.1.0) | diet1.1$coffee_cupsperday.1498.1.0 == -1 | diet1.1$coffee_cupsperday.1498.1.0 == -3, "NA", 
					diet1.1$coffee_cupsperday.1498.1.0)))

#table(diet1.1$water_glassesperday.1528.1.0,useNA="always")
diet1.1$water_glassesperday.1528.1.0_QT = as.numeric(ifelse(diet1.1$water_glassesperday.1528.1.0 == -10, 0.5,
					ifelse(is.na(diet1.1$water_glassesperday.1528.1.0) | diet1.1$water_glassesperday.1528.1.0 == -1 | diet1.1$water_glassesperday.1528.1.0 == -3, "NA", 
					diet1.1$water_glassesperday.1528.1.0)))


### Fields 1568 1578 1588 1598 1608
### was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Field 1558
### it actually is for those that answered:
table(diet1.1$alcohol_overallfreq.1558.1.0,diet1.1$champagnewhitewine_glassesperweek.1578.1.0)
	## Once or twice a week
	## Three or four times a week
	## Daily or almost daily

### Fields 4407 4418 4429 4440 4451 4462
### was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Field 1558
### it actually is for those that answered:
table(diet1.1$alcohol_overallfreq.1558.1.0,diet1.1$redwine_glassespermonth.4407.1.0)
	## One to three times a month
	## Special occasions only

### Make one field of glasses per month based on glasses per week and glasses per month
### let's put them all on a month scale. 
### if they answered 1558
	## Once or twice a week
	## Three or four times a week
	## Daily or almost daily
		### -> glassesperweek * 4
### if they answered 1558
	## One to three times a month
	## Special occasions only
		### -> glasses per month
### if they answered 1558
	### Prefer not to answer
		### -> NA -> MICE imputation
### if they answered 1558
	### Never
		### -> 0

## to avoid missingness:
	## -1 meaning do not know will be the median of that group:
	## if glassesperweek = -1 or -3 -> median(of those that said once or twice a week or more)

#####################################################################
#table(diet1.1$champagnewhitewine_glassesperweek.1578.1.0,useNA="always")
diet1.1$champagnewhitewine_glassesperweek.1578.1.0_QT = as.numeric(ifelse(
							diet1.1$champagnewhitewine_glassesperweek.1578.1.0 == -1 | 
							diet1.1$champagnewhitewine_glassesperweek.1578.1.0 == -3, median(diet1.1$champagnewhitewine_glassesperweek.1578.1.0, na.rm=T), 
							diet1.1$champagnewhitewine_glassesperweek.1578.1.0))	
table(diet1.1$champagnewhitewine_glassesperweek.1578.1.0_QT, useNA="always")			
#table(diet1.1$champagnewhitewine_glassespermonth.4418.1.0,useNA="always")
diet1.1$champagnewhitewine_glassespermonth.4418.1.0_QT = as.numeric(ifelse(
							diet1.1$champagnewhitewine_glassespermonth.4418.1.0 == -1 | 
							diet1.1$champagnewhitewine_glassespermonth.4418.1.0 == -3, median(diet1.1$champagnewhitewine_glassespermonth.4418.1.0, na.rm=T),
							diet1.1$champagnewhitewine_glassespermonth.4418.1.0))
table(diet1.1$champagnewhitewine_glassespermonth.4418.1.0_QT, useNA="always")			
							
							
diet1.1$champagnewhitewine_glassespermonth.derived.1.0_QT = as.numeric(
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily") , 
								4*diet1.1$champagnewhitewine_glassesperweek.1578.1.0_QT, 
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only" ) , 
								diet1.1$champagnewhitewine_glassespermonth.4418.1.0_QT, 
							ifelse(
							diet1.1$alcohol_overallfreq.1558.1.0 == "Never",
								0, "NA"))))
table(diet1.1$champagnewhitewine_glassespermonth.derived.1.0_QT, useNA="always")	
#####################################################################
#table(diet1.1$redwine_glassesperweek.1568.1.0,useNA="always")
diet1.1$redwine_glassesperweek.1568.1.0_QT = as.numeric(ifelse(
							diet1.1$redwine_glassesperweek.1568.1.0 == -1 | 
							diet1.1$redwine_glassesperweek.1568.1.0 == -3, median(diet1.1$redwine_glassesperweek.1568.1.0, na.rm=T), 
							diet1.1$redwine_glassesperweek.1568.1.0))	
table(diet1.1$redwine_glassesperweek.1568.1.0_QT, useNA="always")			
#table(diet1.1$redwine_glassespermonth.4407.1.0,useNA="always")
diet1.1$redwine_glassespermonth.4407.1.0_QT = as.numeric(ifelse(
							diet1.1$redwine_glassespermonth.4407.1.0 == -1 | 
							diet1.1$redwine_glassespermonth.4407.1.0 == -3, median(diet1.1$redwine_glassespermonth.4407.1.0, na.rm=T),
							diet1.1$redwine_glassespermonth.4407.1.0))
table(diet1.1$redwine_glassespermonth.4407.1.0_QT, useNA="always")			
							
							
diet1.1$redwine_glassespermonth.derived.1.0_QT = as.numeric(
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily") , 
								4*diet1.1$redwine_glassesperweek.1568.1.0_QT, 
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only" ) , 
								diet1.1$redwine_glassespermonth.4407.1.0_QT, 
							ifelse(
							diet1.1$alcohol_overallfreq.1558.1.0 == "Never",
								0, "NA"))))
table(diet1.1$redwine_glassespermonth.derived.1.0_QT, useNA="always")
#####################################################################
#table(diet1.1$beercider_pintsperweek.1588.1.0,useNA="always")
diet1.1$beercider_pintsperweek.1588.1.0_QT = as.numeric(ifelse(
							diet1.1$beercider_pintsperweek.1588.1.0 == -1 | 
							diet1.1$beercider_pintsperweek.1588.1.0 == -3, median(diet1.1$beercider_pintsperweek.1588.1.0, na.rm=T), 
							diet1.1$beercider_pintsperweek.1588.1.0))	
table(diet1.1$beercider_pintsperweek.1588.1.0_QT, useNA="always")			
#table(diet1.1$beercider_pintspermonth.4429.1.0,useNA="always")
diet1.1$beercider_pintspermonth.4429.1.0_QT = as.numeric(ifelse(
							diet1.1$beercider_pintspermonth.4429.1.0 == -1 | 
							diet1.1$beercider_pintspermonth.4429.1.0 == -3, median(diet1.1$beercider_pintspermonth.4429.1.0, na.rm=T),
							diet1.1$beercider_pintspermonth.4429.1.0))
table(diet1.1$beercider_pintspermonth.4429.1.0_QT, useNA="always")			
							
							
diet1.1$beercider_pintspermonth.derived.1.0_QT = as.numeric(
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily") , 
								4*diet1.1$beercider_pintsperweek.1588.1.0_QT, 
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only" ) , 
								diet1.1$beercider_pintspermonth.4429.1.0_QT, 
							ifelse(
							diet1.1$alcohol_overallfreq.1558.1.0 == "Never",
								0, "NA"))))
table(diet1.1$beercider_pintspermonth.derived.1.0_QT, useNA="always")
#####################################################################
#table(diet1.1$spirits_measuresperweek.1598.1.0,useNA="always")
diet1.1$spirits_measuresperweek.1598.1.0_QT = as.numeric(ifelse(
							diet1.1$spirits_measuresperweek.1598.1.0 == -1 | 
							diet1.1$spirits_measuresperweek.1598.1.0 == -3, median(diet1.1$spirits_measuresperweek.1598.1.0, na.rm=T), 
							diet1.1$spirits_measuresperweek.1598.1.0))	
table(diet1.1$spirits_measuresperweek.1598.1.0_QT, useNA="always")			
#table(diet1.1$spirits_measurespermonth.4440.1.0,useNA="always")
diet1.1$spirits_measurespermonth.4440.1.0_QT = as.numeric(ifelse(
							diet1.1$spirits_measurespermonth.4440.1.0 == -1 | 
							diet1.1$spirits_measurespermonth.4440.1.0 == -3, median(diet1.1$spirits_measurespermonth.4440.1.0, na.rm=T),
							diet1.1$spirits_measurespermonth.4440.1.0))
table(diet1.1$spirits_measurespermonth.4440.1.0_QT, useNA="always")			
														
diet1.1$spirits_measurespermonth.derived.1.0_QT = as.numeric(
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily") , 
								4*diet1.1$spirits_measuresperweek.1598.1.0_QT, 
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only" ) , 
								diet1.1$spirits_measurespermonth.4440.1.0_QT, 
							ifelse(
							diet1.1$alcohol_overallfreq.1558.1.0 == "Never",
								0, "NA"))))
table(diet1.1$spirits_measurespermonth.derived.1.0_QT, useNA="always")
#####################################################################
#table(diet1.1$fortwine_glassesperweek.1608.1.0,useNA="always")
diet1.1$fortwine_glassesperweek.1608.1.0_QT = as.numeric(ifelse(
							diet1.1$fortwine_glassesperweek.1608.1.0 == -1 | 
							diet1.1$fortwine_glassesperweek.1608.1.0 == -3, median(diet1.1$fortwine_glassesperweek.1608.1.0, na.rm=T), 
							diet1.1$fortwine_glassesperweek.1608.1.0))	
table(diet1.1$fortwine_glassesperweek.1608.1.0_QT, useNA="always")			
#table(diet1.1$fortwine_glassespermonth.4451.1.0,useNA="always")
diet1.1$fortwine_glassespermonth.4451.1.0_QT = as.numeric(ifelse(
							diet1.1$fortwine_glassespermonth.4451.1.0 == -1 | 
							diet1.1$fortwine_glassespermonth.4451.1.0 == -3, median(diet1.1$fortwine_glassespermonth.4451.1.0, na.rm=T),
							diet1.1$fortwine_glassespermonth.4451.1.0))
table(diet1.1$fortwine_glassespermonth.4451.1.0_QT, useNA="always")			
							
							
diet1.1$fortwine_glassespermonth.derived.1.0_QT = as.numeric(
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily") , 
								4*diet1.1$fortwine_glassesperweek.1608.1.0_QT, 
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only" ) , 
								diet1.1$fortwine_glassespermonth.4451.1.0_QT, 
							ifelse(
							diet1.1$alcohol_overallfreq.1558.1.0 == "Never",
								0, "NA"))))
table(diet1.1$fortwine_glassespermonth.derived.1.0_QT, useNA="always")
#####################################################################
#table(diet1.1$otheralcohol_glassesperweek.5364.1.0,useNA="always")
diet1.1$otheralcohol_glassesperweek.5364.1.0_QT = as.numeric(ifelse(
							diet1.1$otheralcohol_glassesperweek.5364.1.0 == -1 | 
							diet1.1$otheralcohol_glassesperweek.5364.1.0 == -3, median(diet1.1$otheralcohol_glassesperweek.5364.1.0, na.rm=T), 
							diet1.1$otheralcohol_glassesperweek.5364.1.0))	
table(diet1.1$otheralcohol_glassesperweek.5364.1.0_QT, useNA="always")			
#table(diet1.1$otheralcohol_glassespermonth.4462.1.0,useNA="always")
diet1.1$otheralcohol_glassespermonth.4462.1.0_QT = as.numeric(ifelse(
							diet1.1$otheralcohol_glassespermonth.4462.1.0 == -1 | 
							diet1.1$otheralcohol_glassespermonth.4462.1.0 == -3, median(diet1.1$otheralcohol_glassespermonth.4462.1.0, na.rm=T),
							diet1.1$otheralcohol_glassespermonth.4462.1.0))
table(diet1.1$otheralcohol_glassespermonth.4462.1.0_QT, useNA="always")			
							
							
diet1.1$otheralcohol_glassespermonth.derived.1.0_QT = as.numeric(
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily") , 
								4*diet1.1$otheralcohol_glassesperweek.5364.1.0_QT, 
							ifelse((
							diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month" | 
							diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only" ) , 
								diet1.1$otheralcohol_glassespermonth.4462.1.0_QT, 
							ifelse(
							diet1.1$alcohol_overallfreq.1558.1.0 == "Never",
								0, "NA"))))
table(diet1.1$otheralcohol_glassespermonth.derived.1.0_QT, useNA="always")
#####################################################################
#####################################################################
#Make 1 field of glasses of ANY alcohol per month

diet1.1$anyalcohol_glassespermonth.derived.1.0_QT = rowSums(subset(diet1.1, select = c(champagnewhitewine_glassespermonth.derived.1.0_QT, redwine_glassespermonth.derived.1.0_QT, 
						beercider_pintspermonth.derived.1.0_QT, spirits_measurespermonth.derived.1.0_QT,
						fortwine_glassespermonth.derived.1.0_QT, otheralcohol_glassespermonth.derived.1.0_QT)), na.rm = TRUE)
table(diet1.1$anyalcohol_glassespermonth.derived.1.0_QT, useNA="always")
#####################################################################


				
### Data structure as follows:
### Data-coding 100377
### Categorical Variable -> Continuous
	#Never -> 0
	#Less than once a week -> 20
	#Once a week -> 52
	#2-4 times a week -> 156
	#5-6 times a week -> 286
	#Once or more daily -> 365
	#do not know -> set to NA
	#prefer not to answer -> set to NA

#table(diet1.1$oilyfish_overallfreq.1329.1.0,useNA="always")		
diet1.1$oilyfish_overallfreq.1329.1.0_QT = as.numeric(ifelse(diet1.1$oilyfish_overallfreq.1329.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$oilyfish_overallfreq.1329.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$oilyfish_overallfreq.1329.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$oilyfish_overallfreq.1329.1.0 == "Once a week", 52, 
					ifelse(diet1.1$oilyfish_overallfreq.1329.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$oilyfish_overallfreq.1329.1.0 == "Never", 0, "NA")))))))
					
			
#table(diet1.1$nonoilyfish_overallfreq.1339.1.0,useNA="always")
diet1.1$nonoilyfish_overallfreq.1339.1.0_QT = as.numeric(ifelse(diet1.1$nonoilyfish_overallfreq.1339.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$nonoilyfish_overallfreq.1339.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$nonoilyfish_overallfreq.1339.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$nonoilyfish_overallfreq.1339.1.0 == "Once a week", 52, 
					ifelse(diet1.1$nonoilyfish_overallfreq.1339.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$nonoilyfish_overallfreq.1339.1.0 == "Never", 0, "NA")))))))
					
#table(diet1.1$processmeat_overallfreq.1349.1.0,useNA="always")
diet1.1$processmeat_overallfreq.1349.1.0_QT = as.numeric(ifelse(diet1.1$processmeat_overallfreq.1349.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$processmeat_overallfreq.1349.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$processmeat_overallfreq.1349.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$processmeat_overallfreq.1349.1.0 == "Once a week", 52, 
					ifelse(diet1.1$processmeat_overallfreq.1349.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$processmeat_overallfreq.1349.1.0 == "Never", 0, "NA")))))))
					
#table(diet1.1$poultry_overallfreq.1359.1.0,useNA="always")
diet1.1$poultry_overallfreq.1359.1.0_QT = as.numeric(ifelse(diet1.1$poultry_overallfreq.1359.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$poultry_overallfreq.1359.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$poultry_overallfreq.1359.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$poultry_overallfreq.1359.1.0 == "Once a week", 52, 
					ifelse(diet1.1$poultry_overallfreq.1359.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$poultry_overallfreq.1359.1.0 == "Never", 0, "NA")))))))

#table(diet1.1$beef_overallfreq.1369.1.0,useNA="always")
diet1.1$beef_overallfreq.1369.1.0_QT = as.numeric(ifelse(diet1.1$beef_overallfreq.1369.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$beef_overallfreq.1369.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$beef_overallfreq.1369.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$beef_overallfreq.1369.1.0 == "Once a week", 52, 
					ifelse(diet1.1$beef_overallfreq.1369.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$beef_overallfreq.1369.1.0 == "Never", 0, "NA")))))))

#table(diet1.1$lambmutton_overallfreq.1379.1.0,useNA="always")
diet1.1$lambmutton_overallfreq.1379.1.0_QT = as.numeric(ifelse(diet1.1$lambmutton_overallfreq.1379.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$lambmutton_overallfreq.1379.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$lambmutton_overallfreq.1379.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$lambmutton_overallfreq.1379.1.0 == "Once a week", 52, 
					ifelse(diet1.1$lambmutton_overallfreq.1379.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$lambmutton_overallfreq.1379.1.0 == "Never", 0, "NA")))))))

#table(diet1.1$pork_overallfreq.1389.1.0,useNA="always")
diet1.1$pork_overallfreq.1389.1.0_QT = as.numeric(ifelse(diet1.1$pork_overallfreq.1389.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$pork_overallfreq.1389.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$pork_overallfreq.1389.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$pork_overallfreq.1389.1.0 == "Once a week", 52, 
					ifelse(diet1.1$pork_overallfreq.1389.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$pork_overallfreq.1389.1.0 == "Never", 0, "NA")))))))

#table(diet1.1$cheese_overallfreq.1408.1.0,useNA="always")
diet1.1$cheese_overallfreq.1408.1.0_QT = as.numeric(ifelse(diet1.1$cheese_overallfreq.1408.1.0 == "Once or more daily", 365, 
					ifelse(diet1.1$cheese_overallfreq.1408.1.0 == "5-6 times a week", 286, 
					ifelse(diet1.1$cheese_overallfreq.1408.1.0 == "2-4 times a week", 156, 
					ifelse(diet1.1$cheese_overallfreq.1408.1.0 == "Once a week", 52, 
					ifelse(diet1.1$cheese_overallfreq.1408.1.0 == "Less than once a week", 20, 
					ifelse(diet1.1$cheese_overallfreq.1408.1.0 == "Never", 0, "NA")))))))

### Data structure as follows:
### Data-coding 100394
### Categorical Variable -> Continuous
	# Always -> 10
	# Usually -> 7
	# Sometimes -> 3
	# Never/rarely -> 0
	# Prefer not to answer -> NA
	# NA

#table(diet1.1$doyouaddsalt.1478.1.0,useNA="always")
diet1.1$doyouaddsalt.1478.1.0_QT = as.numeric(ifelse(diet1.1$doyouaddsalt.1478.1.0 == "Always", 10, 
					ifelse(diet1.1$doyouaddsalt.1478.1.0 == "Usually", 7, 
					ifelse(diet1.1$doyouaddsalt.1478.1.0 == "Sometimes", 3, 
					ifelse(diet1.1$doyouaddsalt.1478.1.0 == "Never/rarely", 0, "NA")))))		

### Data structure as follows:
### Data-coding 100398
### Categorical Variable -> Continuous
	# Very hot -> 10
	# Hot -> 7
	# Warm -> 3
	# Do not drink hot drinks -> 0
	# Prefer not to answer -> set to NA
	# NA

#table(diet1.1$hotdrinktemp.1518.1.0,useNA="always")
diet1.1$hotdrinktemp.1518.1.0_QT = as.numeric(ifelse(diet1.1$hotdrinktemp.1518.1.0 == "Very hot", 10, 
					ifelse(diet1.1$hotdrinktemp.1518.1.0 == "Hot", 7, 
					ifelse(diet1.1$hotdrinktemp.1518.1.0 == "Warm", 3, 
					ifelse(diet1.1$hotdrinktemp.1518.1.0 == "Do not drink hot drinks", 0, "NA")))))

### Data structure as follows:
### Data-coding 100402
### Categorical Variable -> Continuous
	# Daily or almost daily -> 300
	# Three or four times a week -> 182
	# Once or twice a week -> 104
	# One to three times a month -> 24
	# Special occasions only -> 6
	# Never -> 0
	# Prefer not to answer -> set to NA
	# NA

#table(diet1.1$alcohol_overallfreq.1558.1.0,useNA="always")
diet1.1$alcohol_overallfreq.1558.1.0_QT = as.numeric(ifelse(diet1.1$alcohol_overallfreq.1558.1.0 == "Daily or almost daily", 300, 
						ifelse(diet1.1$alcohol_overallfreq.1558.1.0 == "Three or four times a week", 182, 
						ifelse(diet1.1$alcohol_overallfreq.1558.1.0 == "Once or twice a week", 104, 
						ifelse(diet1.1$alcohol_overallfreq.1558.1.0 == "One to three times a month", 24, 
						ifelse(diet1.1$alcohol_overallfreq.1558.1.0 == "Special occasions only", 6, 
						ifelse(diet1.1$alcohol_overallfreq.1558.1.0 == "Never", 0, "NA")))))))

### Data structure as follows:
### Data-coding 90
### Categorical Variable -> Binary x2
	# Current
	# Previous
	# Never
	# Prefer not to answer
	# NA

#table(diet1.1$alcoholdrinkerstatus.20117.1.0,useNA="always")

	# Never (0) vs. Current + Previous (1)
		
diet1.1$alcoholdrinkerstatus.20117.1.0_bin = as.numeric(ifelse(diet1.1$alcoholdrinkerstatus.20117.1.0 == "Never", 0, ifelse(diet1.1$alcoholdrinkerstatus.20117.1.0 == "Previous" | diet1.1$alcoholdrinkerstatus.20117.1.0 == "Current", 1, "NA")))
#table(diet1.1$alcoholdrinkerstatus.20117.1.0_bin,useNA="always")


### this will likely impute previous -> never as the correlation with 0 drinks above will be high
### that's ok. better than making a new variable for this.

	# Never (0) vs. Current only (1)

diet1.1$alcoholdrinkerstatus.20117.1.0_bin2 = as.numeric(ifelse(diet1.1$alcoholdrinkerstatus.20117.1.0 == "Never", 0, ifelse(diet1.1$alcoholdrinkerstatus.20117.1.0 == "Current", 1, "NA")))
#table(diet1.1$alcoholdrinkerstatus.20117.1.0_bin2,useNA="always")

### Data structure as follows:
### Data-coding 100352
### Categorical Variable with:
	# Yes
	# No
	# Prefer not to answer
	# NA

#table(diet1.1$alcohol_formerdrinker_amongcurrentnondrinkers.3731.1.0,useNA="always")
# no additional information not captured in diet1.1$alcoholdrinkerstatus.20117.1.0


### Data structure as follows:
### Data-coding 100416
### Categorical Variable -> Binary x2
### Categorical Variable -> Continuous x1
	# Yes
	# It varies
	# No
	# Do not know
	# Prefer not to answer
	# NA

#table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0,useNA="always")

	# No (0) vs. Yes + It varies (1)

diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin = as.numeric(ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "No", 0, ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "Yes" | diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "It varies", 1, "NA")))
#table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin,useNA="always")

	# No (0) vs. Yes (1)

diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin2 = as.numeric(ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "No", 0, ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "Yes", 1, "NA")))
#table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_bin2,useNA="always")

	# No (0), It varies (1), Yes (2) 

diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_QT3 = as.numeric(ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "No", 0, 
							ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "Yes", 2,
							ifelse(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0 == "It varies", 1, "NA"))))
#table(diet1.1$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.1.0_QT3,useNA="always")

### Data structure as follows:
### Data-coding 100385
### Categorical Variable with:
	# Eggs or foods containing eggs (1) vs. I eat all of the above (0)
	# Dairy products (1) vs. I eat all of the above (0)
	# Wheat products (1) vs. I eat all of the above (0)
	# Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)
	# I eat all of the above : if selected no other choices were allowed
	# prefer not to answer : if selected no other choices were allowed -> set to NA


#table(diet1.1$nevereatcategories.6144.1.0,useNA="always")
#table(diet1.1$nevereatcategories.6144.0.1,useNA="always")
#table(diet1.1$nevereatcategories.6144.0.2,useNA="always")
#table(diet1.1$nevereatcategories.6144.0.3,useNA="always")

diet1.1$nevereatcategories.6144.0.combined = paste(diet1.1$nevereatcategories.6144.1.0, diet1.1$nevereatcategories.6144.0.1, diet1.1$nevereatcategories.6144.0.2,
							diet1.1$nevereatcategories.6144.0.3, sep =":")

#table(diet1.1$nevereatcategories.6144.0.combined,useNA="always")

# Eggs or foods containing eggs (1) vs. I eat all of the above (0)
diet1.1$nevereatcategories.6144.1.0_bin = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin,useNA="always")
# Eggs or foods containing eggs (1) vs. everyone else (0)
diet1.1$nevereatcategories.6144.1.0_bin2 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin2,useNA="always")

# Dairy products (1) vs. I eat all of the above (0)
diet1.1$nevereatcategories.6144.1.0_bin3 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin3,useNA="always")

# Dairy products (1)  vs. everyone else (0)

diet1.1$nevereatcategories.6144.1.0_bin4 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin4,useNA="always")

# Wheat products (1) vs. I eat all of the above (0)
diet1.1$nevereatcategories.6144.1.0_bin5 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", "NA", 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin5,useNA="always")

# Wheat products (1)  vs. everyone else (0)
diet1.1$nevereatcategories.6144.1.0_bin6 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 0, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin6,useNA="always")
	
# Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)	
diet1.1$nevereatcategories.6144.1.0_bin7 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin7,useNA="always")

# Sugar or foods/drinks containing sugar (1) vs. everyone else (0))	
diet1.1$nevereatcategories.6144.1.0_bin8 = as.numeric(
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.1$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.1$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.1$nevereatcategories.6144.1.0_bin8,useNA="always")	
	
	
diet1.1$nevereatcategories.6144.0.combined = NULL	

### Data structure as follows:
### Data-coding 100387
### Categorical Variable with:
	# Full cream
	# Semi-skimmed
	# Skimmed
	# Soya
	# Never/rarely have milk
	# Other type of milk
	# Do not know
	# Prefer not to answer
	# NA

# 1binary: milk x3 vs. never
diet1.1$milk_typeused.1418.1.0_bin = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", "NA", 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin,useNA="always")

# 2binary: milk x5 vs. never
diet1.1$milk_typeused.1418.1.0_bin2 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 1, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin2,useNA="always")

# 3continuous: never (0), skimmed (1), semi-skimmed (2), full cream (3)
diet1.1$milk_typeused.1418.1.0_QT3 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 3,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 2,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", "NA", 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_QT3,useNA="always")

# 4full cream (1) vs. never (0)
diet1.1$milk_typeused.1418.1.0_bin4 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", "NA", 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin4,useNA="always")

# 5full cream (1) vs. all other milk (0)
diet1.1$milk_typeused.1418.1.0_bin5 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 0, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin5,useNA="always")

# 6semi (1) vs. never (0)
diet1.1$milk_typeused.1418.1.0_bin6 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", "NA", 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin6,useNA="always")

# 7semi (1) vs. all other milk (0)
diet1.1$milk_typeused.1418.1.0_bin7 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 0, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin7,useNA="always")

# 8skim (1) vs. never (0)
diet1.1$milk_typeused.1418.1.0_bin8 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", "NA", 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin8,useNA="always")

# 9skim (1) vs. all other milk (0)
diet1.1$milk_typeused.1418.1.0_bin9 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 0, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin9,useNA="always")

# 10soy (1) vs. never (0)
diet1.1$milk_typeused.1418.1.0_bin10 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", "NA", 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin10,useNA="always")

# 11soy (1) vs. all other milk (0)
diet1.1$milk_typeused.1418.1.0_bin11 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 1,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 0, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin11,useNA="always")

# 12other (1) vs. never (0)
diet1.1$milk_typeused.1418.1.0_bin12 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 1, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin12,useNA="always")

# 13other (1) vs. all other milk (0)
diet1.1$milk_typeused.1418.1.0_bin13 = as.numeric(
ifelse(diet1.1$milk_typeused.1418.1.0 == "Full cream", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Semi-skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Skimmed", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Soya", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Other type of milk", 1, 
ifelse(diet1.1$milk_typeused.1418.1.0 == "Never/rarely have milk", 0,
ifelse(diet1.1$milk_typeused.1418.1.0 == "Do not know", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$milk_typeused.1418.1.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.1$milk_typeused.1418.1.0_bin13,useNA="always")

### Data structure as follows:
### Data-coding 100388
### Categorical Variable with:
	# Butter/spreadable butter
	# Flora Pro-Active/Benecol
	# Other type of spread/margarine
	# Never/rarely use spread
	# Do not know
	# Prefer not to answer
	# NA
### Data structure as follows:
### Data-coding 100389
### Categorical Variable with:
	# Hard (block) margarine
	# Soft (tub) margarine
	# Flora Pro-Active or Benecol
	# Olive oil based spread (eg: Bertolli)
	# Polyunsaturated/sunflower oil based spread (eg: Flora)
	# Other low or reduced fat spread
	# Other type of spread/margarine
	# Do not know
	# Prefer not to answer
	# NA

## Field 2654 was collected from participants who indicated they mainly use another type of spread/margarine 
## rather than butter/spreadable butter or do not know what they use, as defined by their answers to Field 1428
table(diet1.1$spread_typeused.1428.1.0,diet1.1$nonbutterspread_typeused.2654.1.0)


diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined = paste(diet1.1$spread_typeused.1428.1.0, diet1.1$nonbutterspread_typeused.2654.1.0, sep =":")
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,useNA="always")	

## butter like spreads
	#Butter/spreadable butter:NA 

	#Do not know:Hard (block) margarine 
	#Other type of spread/margarine:Hard (block) margarine 

	#Do not know:Soft (tub) margarine 
	#Other type of spread/margarine:Soft (tub) margarine 

	#Do not know:Flora Pro-Active or Benecol
	#Flora Pro-Active/Benecol:NA
	#Other type of spread/margarine:Flora Pro-Active or Benecol 

## oil like spreads
	#Do not know:Olive oil based spread (eg: Bertolli) 
	#Other type of spread/margarine:Olive oil based spread (eg: Bertolli) 

	#Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora) 
	#Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora) 

## other low fat/reduced
	#Do not know:Other low or reduced fat spread 
	#Other type of spread/margarine:Other low or reduced fat spread  

## other
	#Other type of spread/margarine:Other type of spread/margarine 

## never
	#Never/rarely use spread:NA 

## NA
	#Other type of spread/margarine:Do not know
	#Do not know:Do not know
	#Do not know:Prefer not to answer
	#Do not know:Other type of spread/margarine 
	#Prefer not to answer:NA
	#Other type of spread/margarine:Prefer not to answer 
	#NA:NA 


#1 All vs. Never

diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1,useNA="always")

#2 Butter/Margarine vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2,useNA="always")

#3 Oil-based vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3,useNA="always")

#4 Butter (with flora proactive)vs. Oil

diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4,useNA="always")

#5 Butter (without flora proactive)vs. Oil
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5,useNA="always")

#6 Butter vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6,useNA="always")

#7 Butter vs. Other
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7 = flas.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7,useNA="always")

#8 Hard (block) margarine vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8,useNA="always")

#9 Hard (block) margarine vs. Other
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9,useNA="always")

#10 Soft (tub) margarine
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10,useNA="always")

#11 Soft (tub) margarine
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11,useNA="always")

#12 Flora vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12,useNA="always")

#13 Flora vs. Other
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13,useNA="always")

#14 Olive oil based spread (eg: Bertolli) vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14,useNA="always")

#15 Olive oil based spread (eg: Bertolli) vs. Other
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15,useNA="always")

#16 Flora oil/sunflower oil vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16,useNA="always")

#17 Flora oil/sunflower oil vs. Other
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17,useNA="always")

#18 Other low or reduced fat spread vs. Never
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18,useNA="always")

#19 Other low or reduced fat spread vs. Other
diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19 = as.numeric(
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19,useNA="always")

diet1.1$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined = NULL

### Field 1448 was collected from all participants except those who indicated that they do not eat bread or less than one slice each week, as defined by their answers to Field 1438


### Data structure as follows:
### Data-coding 100391
### Categorical Variable with:
	# White
	# Brown
	# Wholemeal or wholegrain
	# Other type of bread
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.1$bread_typeused.1448.1.0,useNA="always")

##1 White vs Other
diet1.1$bread_typeused.1448.1.0_bin = as.numeric(
ifelse(diet1.1$bread_typeused.1448.1.0 == "White", 1,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Brown", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Other type of bread", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Do not know", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$bread_typeused.1448.1.0_bin,useNA="always")

##2 Brown vs. Other
diet1.1$bread_typeused.1448.1.0_bin2 = as.numeric(
ifelse(diet1.1$bread_typeused.1448.1.0 == "White", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Brown", 1,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Other type of bread", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Do not know", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$bread_typeused.1448.1.0_bin2,useNA="always")

##3 Wholemeal vs. Other
diet1.1$bread_typeused.1448.1.0_bin3 = as.numeric(
ifelse(diet1.1$bread_typeused.1448.1.0 == "White", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Brown", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Wholemeal or wholegrain", 1,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Other type of bread", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Do not know", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$bread_typeused.1448.1.0_bin3,useNA="always")

##4 White vs. Brown + Wholemeal
diet1.1$bread_typeused.1448.1.0_bin4 = as.numeric(
ifelse(diet1.1$bread_typeused.1448.1.0 == "White", 1,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Brown", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Other type of bread", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Do not know", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$bread_typeused.1448.1.0_bin4,useNA="always")

##5 White + Brown vs. Wholemeal
diet1.1$bread_typeused.1448.1.0_bin5 = as.numeric(
ifelse(diet1.1$bread_typeused.1448.1.0 == "White", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Brown", 0,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Wholemeal or wholegrain", 1,
ifelse(diet1.1$bread_typeused.1448.1.0 == "Other type of bread", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Do not know", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$bread_typeused.1448.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$bread_typeused.1448.1.0_bin5,useNA="always")

### Field 1468 was collected from all participants except those who indicated that they do not eat cereal or less than one bowl of cereal each week, as defined by their answers to Field 1458

### Data structure as follows:
### Data-coding 100393
### Categorical Variable with:
	# Biscuit cereal (e.g. Weetabix) 
	# Bran cereal (e.g. All Bran, Branflakes)
	# Oat cereal (e.g. Ready Brek, porridge)
	# Muesli
	# Other (e.g. Cornflakes, Frosties)
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.1$cereal_typeused.1468.1.0,useNA="always")

# Biscuit vs. All Other
diet1.1$cereal_typeused.1468.1.0_bin = as.numeric(
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Biscuit cereal (e.g. Weetabix)", 1,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Muesli", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Do not know", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "NA", "NA",
"NA")))))))))
#table(diet1.1$cereal_typeused.1468.1.0_bin,useNA="always")

# Bran vs. All Other
diet1.1$cereal_typeused.1468.1.0_bin2 = as.numeric(
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Bran cereal (e.g. All Bran, Branflakes)", 1,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Muesli", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Do not know", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "NA", "NA",
"NA")))))))))
#table(diet1.1$cereal_typeused.1468.1.0_bin2,useNA="always")

# Oat vs. All Other
diet1.1$cereal_typeused.1468.1.0_bin3 = as.numeric(
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Oat cereal (e.g. Ready Brek, porridge)", 1,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Muesli", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Do not know", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "NA", "NA",
"NA")))))))))
#table(diet1.1$cereal_typeused.1468.1.0_bin3,useNA="always")

# Muesli vs. All Other
diet1.1$cereal_typeused.1468.1.0_bin4 = as.numeric(
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Muesli", 1,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Do not know", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "NA", "NA",
"NA")))))))))
#table(diet1.1$cereal_typeused.1468.1.0_bin4,useNA="always")

# Other vs. All Other
diet1.1$cereal_typeused.1468.1.0_bin5 = as.numeric(
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Muesli", 0,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Other (e.g. Cornflakes, Frosties)", 1,
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Do not know", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$cereal_typeused.1468.1.0 == "NA", "NA",
"NA")))))))))
#table(diet1.1$cereal_typeused.1468.1.0_bin5,useNA="always")

### Field 1508 was collected from participants who indicated that they drank at least 1 cup of coffee each day or less than one cup each day, as defined by their answers to Field 1498

### Data structure as follows:
### Data-coding 100397
### Categorical Variable with:
	# Ground coffee (include espresso, filter etc)
	# Instant coffee
	# Decaffeinated coffee (any type)
	# Other type of coffee
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.1$coffee_typeused.1508.1.0,useNA="always")

# Ground vs. Other
diet1.1$coffee_typeused.1508.1.0_bin = as.numeric(
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Ground coffee (include espresso, filter etc)", 1,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Instant coffee", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Decaffeinated coffee (any type)", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Other type of coffee", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Do not know", "NA",
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$coffee_typeused.1508.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$coffee_typeused.1508.1.0_bin,useNA="always")

# instant vs. other
diet1.1$coffee_typeused.1508.1.0_bin2 = as.numeric(
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Ground coffee (include espresso, filter etc)", 1,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Instant coffee", 1,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Decaffeinated coffee (any type)", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Other type of coffee", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Do not know", "NA",
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$coffee_typeused.1508.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$coffee_typeused.1508.1.0_bin2,useNA="always")

# decaff vs. other
diet1.1$coffee_typeused.1508.1.0_bin3 = as.numeric(
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Ground coffee (include espresso, filter etc)", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Instant coffee", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Decaffeinated coffee (any type)", 1,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Other type of coffee", 0,
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Do not know", "NA",
ifelse(diet1.1$coffee_typeused.1508.1.0 == "Prefer not to answer", "NA",
ifelse(diet1.1$coffee_typeused.1508.1.0 == "NA", "NA",
"NA"))))))))
#table(diet1.1$coffee_typeused.1508.1.0_bin3,useNA="always")


####################
#save diet1.1 dataset
colnames(diet1.1)
save_diet1.1 = diet1.1[,c(1,2,79, 124:221)]
write.table(save_diet1.1, "/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1.1_07062018update", col.names = TRUE, row.names = FALSE, quote = FALSE, sep ="\t")




##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

################### ################### ################### ################### ################### ################### ###################
#load in and merge diet1.2
diet1.2_8785 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_8785_diet1.2", verbose=TRUE, data.table=FALSE)
diet1.2_6184 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_6184_diet1.2", verbose=TRUE, data.table=FALSE)
diet1.2_7052 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet_7052_diet1.2", verbose=TRUE, data.table=FALSE)


diet1.2tmp = merge(diet1.2_6184, diet1.2_8785, by = "ID", all = T)
diet1.2 = merge(diet1.2tmp, diet1.2_7052, by = "ID", all = T)

###################
#create appropriate age variables per instance
#DIET1: date_assessmentcentre.53.2.0 -> birth year + birth month

## split DATE -> 3 columns of year, month, day
library(stringr)

x = str_split_fixed(diet1.2$date_assessmentcentre.53.2.0, "-", 3)
x = apply(x, 2 ,as.numeric)
colnames(x) = c("DIET1_date_year.2.0", "DIET1_date_month.2.0", "DIET1_date_day.2.0")
diet1.2 = cbind(diet1.2,x)

#table(diet1.2$month_of_birth.52,useNA="always")

##convert month in text -> numeric

diet1.2$month_of_birth_numeric.52 = ifelse(diet1.2$month_of_birth.52 == "January", 1, 
				 ifelse(diet1.2$month_of_birth.52 == "February", 2, 
				 ifelse(diet1.2$month_of_birth.52 == "March", 3, 
				 ifelse(diet1.2$month_of_birth.52 == "April", 4, 
				 ifelse(diet1.2$month_of_birth.52 == "May", 5, 
				 ifelse(diet1.2$month_of_birth.52 == "June", 6, 
				 ifelse(diet1.2$month_of_birth.52 == "July", 7, 
				 ifelse(diet1.2$month_of_birth.52 == "August", 8, 
				 ifelse(diet1.2$month_of_birth.52 == "September", 9, 
				 ifelse(diet1.2$month_of_birth.52 == "October", 10, 
				 ifelse(diet1.2$month_of_birth.52 == "November", 11, 12)))))))))))

##create age variable

diet1.2$DIET1_age_months.2.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$month_of_birth_numeric.52, (diet1.2$DIET1_date_year.2.0 - diet1.2$year_of_birth.34)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$month_of_birth_numeric.52),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$year_of_birth.34)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$month_of_birth_numeric.52)))

###################
#exclusions to diet1.2

#pregnant
#table(diet1.2$pregnant.3140.2.0, useNA="always")
diet1.2 = subset(diet1.2, pregnant.3140.2.0 != "Yes" | is.na(pregnant.3140.2.0))

#non-esrd
diet1.2 = subset(diet1.2, !ID %in% esrd)

#non-cancer diagnosis <=1 year from current age
#(ideally would be from remission...)
diet1.2 = merge(diet1.2,cancer, by ="ID",all.x = T)

diet1.2$DIET1_monthsfromcancerdiag.0.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.0.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.0.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.0.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.0.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.0.0)))
diet1.2$DIET1_monthsfromcancerdiag.1.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.1.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.1.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.1.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.1.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.1.0)))
diet1.2$DIET1_monthsfromcancerdiag.2.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.2.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.2.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.2.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.2.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.2.0)))
diet1.2$DIET1_monthsfromcancerdiag.3.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.3.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.3.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.3.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.3.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.3.0)))
diet1.2$DIET1_monthsfromcancerdiag.4.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.4.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.4.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.4.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.4.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.4.0)))
diet1.2$DIET1_monthsfromcancerdiag.5.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.5.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.5.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.5.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.5.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.5.0)))
diet1.2$DIET1_monthsfromcancerdiag.6.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.6.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.6.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.6.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.6.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.6.0)))
diet1.2$DIET1_monthsfromcancerdiag.7.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.7.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.7.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.7.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.7.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.7.0)))
diet1.2$DIET1_monthsfromcancerdiag.8.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.8.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.8.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.8.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.8.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.8.0)))
diet1.2$DIET1_monthsfromcancerdiag.9.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.9.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.9.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.9.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.9.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.9.0)))
diet1.2$DIET1_monthsfromcancerdiag.10.0 = ifelse(diet1.2$DIET1_date_month.2.0 >= diet1.2$date_cancerdiag_month.40005.10.0, (diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.10.0)*12 + (diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.10.0),
								  	(diet1.2$DIET1_date_year.2.0 - diet1.2$date_cancerdiag_year.40005.10.0)*12 - abs((diet1.2$DIET1_date_month.2.0 - diet1.2$date_cancerdiag_month.40005.10.0)))

diet1.2 = subset(diet1.2, (DIET1_monthsfromcancerdiag.0.0 > 12 | DIET1_monthsfromcancerdiag.0.0 < 0 | is.na(DIET1_monthsfromcancerdiag.0.0)) &
			  (DIET1_monthsfromcancerdiag.1.0 > 12 | DIET1_monthsfromcancerdiag.1.0 < 0 | is.na(DIET1_monthsfromcancerdiag.1.0)) &
			  (DIET1_monthsfromcancerdiag.2.0 > 12 | DIET1_monthsfromcancerdiag.2.0 < 0 | is.na(DIET1_monthsfromcancerdiag.2.0)) &
			  (DIET1_monthsfromcancerdiag.3.0 > 12 | DIET1_monthsfromcancerdiag.3.0 < 0 | is.na(DIET1_monthsfromcancerdiag.3.0)) &
			  (DIET1_monthsfromcancerdiag.4.0 > 12 | DIET1_monthsfromcancerdiag.4.0 < 0 | is.na(DIET1_monthsfromcancerdiag.4.0)) &
			  (DIET1_monthsfromcancerdiag.5.0 > 12 | DIET1_monthsfromcancerdiag.5.0 < 0 | is.na(DIET1_monthsfromcancerdiag.5.0)) &
			  (DIET1_monthsfromcancerdiag.6.0 > 12 | DIET1_monthsfromcancerdiag.6.0 < 0 | is.na(DIET1_monthsfromcancerdiag.6.0)) &
			  (DIET1_monthsfromcancerdiag.7.0 > 12 | DIET1_monthsfromcancerdiag.7.0 < 0 | is.na(DIET1_monthsfromcancerdiag.7.0)) &
			  (DIET1_monthsfromcancerdiag.8.0 > 12 | DIET1_monthsfromcancerdiag.8.0 < 0 | is.na(DIET1_monthsfromcancerdiag.8.0)) &
			  (DIET1_monthsfromcancerdiag.9.0 > 12 | DIET1_monthsfromcancerdiag.9.0 < 0 | is.na(DIET1_monthsfromcancerdiag.9.0)) &
			  (DIET1_monthsfromcancerdiag.10.0 > 12 | DIET1_monthsfromcancerdiag.10.0 < 0 | is.na(DIET1_monthsfromcancerdiag.10.0)))
			  
###################
#create instance field
diet1.2$instance = "diet1.2"
###################
#create usable phenotypes. 



### Data structure as follows:
### Data-coding 100373
### Continuous Variable with:
	# -10 = <1
	# -1 = do not know
	# -3 = prefer not to answer
	
	#convert -10 = .5
	#convert -1 & -3 = "NA"

#table(diet1.2$cookedveg_TBSperday.1289.2.0,useNA="always")
diet1.2$cookedveg_TBSperday.1289.2.0_QT = as.numeric(ifelse(diet1.2$cookedveg_TBSperday.1289.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$cookedveg_TBSperday.1289.2.0) | diet1.2$cookedveg_TBSperday.1289.2.0 == -1 | diet1.2$cookedveg_TBSperday.1289.2.0 == -3, "NA", 
					diet1.2$cookedveg_TBSperday.1289.2.0)))
	
#table(diet1.2$rawveg_TBSperday.1299.2.0,useNA="always")
diet1.2$rawveg_TBSperday.1299.2.0_QT = as.numeric(ifelse(diet1.2$rawveg_TBSperday.1299.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$rawveg_TBSperday.1299.2.0) | diet1.2$rawveg_TBSperday.1299.2.0 == -1 | diet1.2$rawveg_TBSperday.1299.2.0 == -3, "NA", 
					diet1.2$rawveg_TBSperday.1299.2.0)))

#table(diet1.2$freshfruit_piecesperday.1309.2.0,useNA="always")
diet1.2$freshfruit_piecesperday.1309.2.0_QT = as.numeric(ifelse(diet1.2$freshfruit_piecesperday.1309.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$freshfruit_piecesperday.1309.2.0) | diet1.2$freshfruit_piecesperday.1309.2.0 == -1 | diet1.2$freshfruit_piecesperday.1309.2.0 == -3, "NA", 
					diet1.2$freshfruit_piecesperday.1309.2.0)))

#table(diet1.2$driedfruit_piecesperday.1319.2.0,useNA="always")
diet1.2$driedfruit_piecesperday.1319.2.0_QT = as.numeric(ifelse(diet1.2$driedfruit_piecesperday.1319.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$driedfruit_piecesperday.1319.2.0) | diet1.2$driedfruit_piecesperday.1319.2.0 == -1 | diet1.2$driedfruit_piecesperday.1319.2.0 == -3, "NA", 
					diet1.2$driedfruit_piecesperday.1319.2.0)))

#table(diet1.2$bread_slicesperweek.1438.2.0,useNA="always")
diet1.2$bread_slicesperweek.1438.2.0_QT = as.numeric(ifelse(diet1.2$bread_slicesperweek.1438.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$bread_slicesperweek.1438.2.0) | diet1.2$bread_slicesperweek.1438.2.0 == -1 | diet1.2$bread_slicesperweek.1438.2.0 == -3, "NA", 
					diet1.2$bread_slicesperweek.1438.2.0)))

#table(diet1.2$cereal_bowlsperweek.1458.2.0,useNA="always")
diet1.2$cereal_bowlsperweek.1458.2.0_QT = as.numeric(ifelse(diet1.2$cereal_bowlsperweek.1458.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$cereal_bowlsperweek.1458.2.0) | diet1.2$cereal_bowlsperweek.1458.2.0 == -1 | diet1.2$cereal_bowlsperweek.1458.2.0 == -3, "NA", 
					diet1.2$cereal_bowlsperweek.1458.2.0)))

#table(diet1.2$tea_cupsperday.1488.2.0,useNA="always")
diet1.2$tea_cupsperday.1488.2.0_QT = as.numeric(ifelse(diet1.2$tea_cupsperday.1488.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$tea_cupsperday.1488.2.0) | diet1.2$tea_cupsperday.1488.2.0 == -1 | diet1.2$tea_cupsperday.1488.2.0 == -3, "NA", 
					diet1.2$tea_cupsperday.1488.2.0)))

#table(diet1.2$coffee_cupsperday.1498.2.0,useNA="always")
diet1.2$coffee_cupsperday.1498.2.0_QT = as.numeric(ifelse(diet1.2$coffee_cupsperday.1498.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$coffee_cupsperday.1498.2.0) | diet1.2$coffee_cupsperday.1498.2.0 == -1 | diet1.2$coffee_cupsperday.1498.2.0 == -3, "NA", 
					diet1.2$coffee_cupsperday.1498.2.0)))

#table(diet1.2$water_glassesperday.1528.2.0,useNA="always")
diet1.2$water_glassesperday.1528.2.0_QT = as.numeric(ifelse(diet1.2$water_glassesperday.1528.2.0 == -10, 0.5,
					ifelse(is.na(diet1.2$water_glassesperday.1528.2.0) | diet1.2$water_glassesperday.1528.2.0 == -1 | diet1.2$water_glassesperday.1528.2.0 == -3, "NA", 
					diet1.2$water_glassesperday.1528.2.0)))


### Fields 1568 1578 1588 1598 1608
### was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Field 1558
### it actually is for those that answered:
table(diet1.2$alcohol_overallfreq.1558.2.0,diet1.2$champagnewhitewine_glassesperweek.1578.2.0)
	## Once or twice a week
	## Three or four times a week
	## Daily or almost daily

### Fields 4407 4418 4429 4440 4451 4462
### was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Field 1558
### it actually is for those that answered:
table(diet1.2$alcohol_overallfreq.1558.2.0,diet1.2$redwine_glassespermonth.4407.2.0)
	## One to three times a month
	## Special occasions only

### Make one field of glasses per month based on glasses per week and glasses per month
### let's put them all on a month scale. 
### if they answered 1558
	## Once or twice a week
	## Three or four times a week
	## Daily or almost daily
		### -> glassesperweek * 4
### if they answered 1558
	## One to three times a month
	## Special occasions only
		### -> glasses per month
### if they answered 1558
	### Prefer not to answer
		### -> NA -> MICE imputation
### if they answered 1558
	### Never
		### -> 0

## to avoid missingness:
	## -1 meaning do not know will be the median of that group:
	## if glassesperweek = -1 or -3 -> median(of those that said once or twice a week or more)

#####################################################################
#table(diet1.2$champagnewhitewine_glassesperweek.1578.2.0,useNA="always")
diet1.2$champagnewhitewine_glassesperweek.1578.2.0_QT = as.numeric(ifelse(
							diet1.2$champagnewhitewine_glassesperweek.1578.2.0 == -1 | 
							diet1.2$champagnewhitewine_glassesperweek.1578.2.0 == -3, median(diet1.2$champagnewhitewine_glassesperweek.1578.2.0, na.rm=T), 
							diet1.2$champagnewhitewine_glassesperweek.1578.2.0))	
table(diet1.2$champagnewhitewine_glassesperweek.1578.2.0_QT, useNA="always")			
#table(diet1.2$champagnewhitewine_glassespermonth.4418.2.0,useNA="always")
diet1.2$champagnewhitewine_glassespermonth.4418.2.0_QT = as.numeric(ifelse(
							diet1.2$champagnewhitewine_glassespermonth.4418.2.0 == -1 | 
							diet1.2$champagnewhitewine_glassespermonth.4418.2.0 == -3, median(diet1.2$champagnewhitewine_glassespermonth.4418.2.0, na.rm=T),
							diet1.2$champagnewhitewine_glassespermonth.4418.2.0))
table(diet1.2$champagnewhitewine_glassespermonth.4418.2.0_QT, useNA="always")			
							
							
diet1.2$champagnewhitewine_glassespermonth.derived.2.0_QT = as.numeric(
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily") , 
								4*diet1.2$champagnewhitewine_glassesperweek.1578.2.0_QT, 
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only" ) , 
								diet1.2$champagnewhitewine_glassespermonth.4418.2.0_QT, 
							ifelse(
							diet1.2$alcohol_overallfreq.1558.2.0 == "Never",
								0, "NA"))))
table(diet1.2$champagnewhitewine_glassespermonth.derived.2.0_QT, useNA="always")	
#####################################################################
#table(diet1.2$redwine_glassesperweek.1568.2.0,useNA="always")
diet1.2$redwine_glassesperweek.1568.2.0_QT = as.numeric(ifelse(
							diet1.2$redwine_glassesperweek.1568.2.0 == -1 | 
							diet1.2$redwine_glassesperweek.1568.2.0 == -3, median(diet1.2$redwine_glassesperweek.1568.2.0, na.rm=T), 
							diet1.2$redwine_glassesperweek.1568.2.0))	
table(diet1.2$redwine_glassesperweek.1568.2.0_QT, useNA="always")			
#table(diet1.2$redwine_glassespermonth.4407.2.0,useNA="always")
diet1.2$redwine_glassespermonth.4407.2.0_QT = as.numeric(ifelse(
							diet1.2$redwine_glassespermonth.4407.2.0 == -1 | 
							diet1.2$redwine_glassespermonth.4407.2.0 == -3, median(diet1.2$redwine_glassespermonth.4407.2.0, na.rm=T),
							diet1.2$redwine_glassespermonth.4407.2.0))
table(diet1.2$redwine_glassespermonth.4407.2.0_QT, useNA="always")			
							
							
diet1.2$redwine_glassespermonth.derived.2.0_QT = as.numeric(
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily") , 
								4*diet1.2$redwine_glassesperweek.1568.2.0_QT, 
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only" ) , 
								diet1.2$redwine_glassespermonth.4407.2.0_QT, 
							ifelse(
							diet1.2$alcohol_overallfreq.1558.2.0 == "Never",
								0, "NA"))))
table(diet1.2$redwine_glassespermonth.derived.2.0_QT, useNA="always")
#####################################################################
#table(diet1.2$beercider_pintsperweek.1588.2.0,useNA="always")
diet1.2$beercider_pintsperweek.1588.2.0_QT = as.numeric(ifelse(
							diet1.2$beercider_pintsperweek.1588.2.0 == -1 | 
							diet1.2$beercider_pintsperweek.1588.2.0 == -3, median(diet1.2$beercider_pintsperweek.1588.2.0, na.rm=T), 
							diet1.2$beercider_pintsperweek.1588.2.0))	
table(diet1.2$beercider_pintsperweek.1588.2.0_QT, useNA="always")			
#table(diet1.2$beercider_pintspermonth.4429.2.0,useNA="always")
diet1.2$beercider_pintspermonth.4429.2.0_QT = as.numeric(ifelse(
							diet1.2$beercider_pintspermonth.4429.2.0 == -1 | 
							diet1.2$beercider_pintspermonth.4429.2.0 == -3, median(diet1.2$beercider_pintspermonth.4429.2.0, na.rm=T),
							diet1.2$beercider_pintspermonth.4429.2.0))
table(diet1.2$beercider_pintspermonth.4429.2.0_QT, useNA="always")			
							
							
diet1.2$beercider_pintspermonth.derived.2.0_QT = as.numeric(
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily") , 
								4*diet1.2$beercider_pintsperweek.1588.2.0_QT, 
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only" ) , 
								diet1.2$beercider_pintspermonth.4429.2.0_QT, 
							ifelse(
							diet1.2$alcohol_overallfreq.1558.2.0 == "Never",
								0, "NA"))))
table(diet1.2$beercider_pintspermonth.derived.2.0_QT, useNA="always")
#####################################################################
#table(diet1.2$spirits_measuresperweek.1598.2.0,useNA="always")
diet1.2$spirits_measuresperweek.1598.2.0_QT = as.numeric(ifelse(
							diet1.2$spirits_measuresperweek.1598.2.0 == -1 | 
							diet1.2$spirits_measuresperweek.1598.2.0 == -3, median(diet1.2$spirits_measuresperweek.1598.2.0, na.rm=T), 
							diet1.2$spirits_measuresperweek.1598.2.0))	
table(diet1.2$spirits_measuresperweek.1598.2.0_QT, useNA="always")			
#table(diet1.2$spirits_measurespermonth.4440.2.0,useNA="always")
diet1.2$spirits_measurespermonth.4440.2.0_QT = as.numeric(ifelse(
							diet1.2$spirits_measurespermonth.4440.2.0 == -1 | 
							diet1.2$spirits_measurespermonth.4440.2.0 == -3, median(diet1.2$spirits_measurespermonth.4440.2.0, na.rm=T),
							diet1.2$spirits_measurespermonth.4440.2.0))
table(diet1.2$spirits_measurespermonth.4440.2.0_QT, useNA="always")			
														
diet1.2$spirits_measurespermonth.derived.2.0_QT = as.numeric(
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily") , 
								4*diet1.2$spirits_measuresperweek.1598.2.0_QT, 
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only" ) , 
								diet1.2$spirits_measurespermonth.4440.2.0_QT, 
							ifelse(
							diet1.2$alcohol_overallfreq.1558.2.0 == "Never",
								0, "NA"))))
table(diet1.2$spirits_measurespermonth.derived.2.0_QT, useNA="always")
#####################################################################
#table(diet1.2$fortwine_glassesperweek.1608.2.0,useNA="always")
diet1.2$fortwine_glassesperweek.1608.2.0_QT = as.numeric(ifelse(
							diet1.2$fortwine_glassesperweek.1608.2.0 == -1 | 
							diet1.2$fortwine_glassesperweek.1608.2.0 == -3, median(diet1.2$fortwine_glassesperweek.1608.2.0, na.rm=T), 
							diet1.2$fortwine_glassesperweek.1608.2.0))	
table(diet1.2$fortwine_glassesperweek.1608.2.0_QT, useNA="always")			
#table(diet1.2$fortwine_glassespermonth.4451.2.0,useNA="always")
diet1.2$fortwine_glassespermonth.4451.2.0_QT = as.numeric(ifelse(
							diet1.2$fortwine_glassespermonth.4451.2.0 == -1 | 
							diet1.2$fortwine_glassespermonth.4451.2.0 == -3, median(diet1.2$fortwine_glassespermonth.4451.2.0, na.rm=T),
							diet1.2$fortwine_glassespermonth.4451.2.0))
table(diet1.2$fortwine_glassespermonth.4451.2.0_QT, useNA="always")			
							
							
diet1.2$fortwine_glassespermonth.derived.2.0_QT = as.numeric(
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily") , 
								4*diet1.2$fortwine_glassesperweek.1608.2.0_QT, 
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only" ) , 
								diet1.2$fortwine_glassespermonth.4451.2.0_QT, 
							ifelse(
							diet1.2$alcohol_overallfreq.1558.2.0 == "Never",
								0, "NA"))))
table(diet1.2$fortwine_glassespermonth.derived.2.0_QT, useNA="always")
#####################################################################
#table(diet1.2$otheralcohol_glassesperweek.5364.2.0,useNA="always")
diet1.2$otheralcohol_glassesperweek.5364.2.0_QT = as.numeric(ifelse(
							diet1.2$otheralcohol_glassesperweek.5364.2.0 == -1 | 
							diet1.2$otheralcohol_glassesperweek.5364.2.0 == -3, median(diet1.2$otheralcohol_glassesperweek.5364.2.0, na.rm=T), 
							diet1.2$otheralcohol_glassesperweek.5364.2.0))	
table(diet1.2$otheralcohol_glassesperweek.5364.2.0_QT, useNA="always")			
#table(diet1.2$otheralcohol_glassespermonth.4462.2.0,useNA="always")
diet1.2$otheralcohol_glassespermonth.4462.2.0_QT = as.numeric(ifelse(
							diet1.2$otheralcohol_glassespermonth.4462.2.0 == -1 | 
							diet1.2$otheralcohol_glassespermonth.4462.2.0 == -3, median(diet1.2$otheralcohol_glassespermonth.4462.2.0, na.rm=T),
							diet1.2$otheralcohol_glassespermonth.4462.2.0))
table(diet1.2$otheralcohol_glassespermonth.4462.2.0_QT, useNA="always")			
							
							
diet1.2$otheralcohol_glassespermonth.derived.2.0_QT = as.numeric(
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily") , 
								4*diet1.2$otheralcohol_glassesperweek.5364.2.0_QT, 
							ifelse((
							diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month" | 
							diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only" ) , 
								diet1.2$otheralcohol_glassespermonth.4462.2.0_QT, 
							ifelse(
							diet1.2$alcohol_overallfreq.1558.2.0 == "Never",
								0, "NA"))))
table(diet1.2$otheralcohol_glassespermonth.derived.2.0_QT, useNA="always")
#####################################################################
#Make 1 field of glasses of ANY alcohol per month

diet1.2$anyalcohol_glassespermonth.derived.2.0_QT = rowSums(subset(diet1.2, select = c(champagnewhitewine_glassespermonth.derived.2.0_QT, redwine_glassespermonth.derived.2.0_QT, 
						beercider_pintspermonth.derived.2.0_QT, spirits_measurespermonth.derived.2.0_QT,
						fortwine_glassespermonth.derived.2.0_QT, otheralcohol_glassespermonth.derived.2.0_QT)), na.rm = TRUE)
table(diet1.2$anyalcohol_glassespermonth.derived.2.0_QT, useNA="always")


				
### Data structure as follows:
### Data-coding 100377
### Categorical Variable -> Continuous
	#Never -> 0
	#Less than once a week -> 20
	#Once a week -> 52
	#2-4 times a week -> 156
	#5-6 times a week -> 286
	#Once or more daily -> 365
	#do not know -> set to NA
	#prefer not to answer -> set to NA

#table(diet1.2$oilyfish_overallfreq.1329.2.0,useNA="always")		
diet1.2$oilyfish_overallfreq.1329.2.0_QT = as.numeric(ifelse(diet1.2$oilyfish_overallfreq.1329.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$oilyfish_overallfreq.1329.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$oilyfish_overallfreq.1329.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$oilyfish_overallfreq.1329.2.0 == "Once a week", 52, 
					ifelse(diet1.2$oilyfish_overallfreq.1329.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$oilyfish_overallfreq.1329.2.0 == "Never", 0, "NA")))))))
					
			
#table(diet1.2$nonoilyfish_overallfreq.1339.2.0,useNA="always")
diet1.2$nonoilyfish_overallfreq.1339.2.0_QT = as.numeric(ifelse(diet1.2$nonoilyfish_overallfreq.1339.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$nonoilyfish_overallfreq.1339.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$nonoilyfish_overallfreq.1339.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$nonoilyfish_overallfreq.1339.2.0 == "Once a week", 52, 
					ifelse(diet1.2$nonoilyfish_overallfreq.1339.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$nonoilyfish_overallfreq.1339.2.0 == "Never", 0, "NA")))))))
					
#table(diet1.2$processmeat_overallfreq.1349.2.0,useNA="always")
diet1.2$processmeat_overallfreq.1349.2.0_QT = as.numeric(ifelse(diet1.2$processmeat_overallfreq.1349.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$processmeat_overallfreq.1349.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$processmeat_overallfreq.1349.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$processmeat_overallfreq.1349.2.0 == "Once a week", 52, 
					ifelse(diet1.2$processmeat_overallfreq.1349.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$processmeat_overallfreq.1349.2.0 == "Never", 0, "NA")))))))
					
#table(diet1.2$poultry_overallfreq.1359.2.0,useNA="always")
diet1.2$poultry_overallfreq.1359.2.0_QT = as.numeric(ifelse(diet1.2$poultry_overallfreq.1359.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$poultry_overallfreq.1359.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$poultry_overallfreq.1359.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$poultry_overallfreq.1359.2.0 == "Once a week", 52, 
					ifelse(diet1.2$poultry_overallfreq.1359.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$poultry_overallfreq.1359.2.0 == "Never", 0, "NA")))))))

#table(diet1.2$beef_overallfreq.1369.2.0,useNA="always")
diet1.2$beef_overallfreq.1369.2.0_QT = as.numeric(ifelse(diet1.2$beef_overallfreq.1369.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$beef_overallfreq.1369.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$beef_overallfreq.1369.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$beef_overallfreq.1369.2.0 == "Once a week", 52, 
					ifelse(diet1.2$beef_overallfreq.1369.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$beef_overallfreq.1369.2.0 == "Never", 0, "NA")))))))

#table(diet1.2$lambmutton_overallfreq.1379.2.0,useNA="always")
diet1.2$lambmutton_overallfreq.1379.2.0_QT = as.numeric(ifelse(diet1.2$lambmutton_overallfreq.1379.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$lambmutton_overallfreq.1379.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$lambmutton_overallfreq.1379.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$lambmutton_overallfreq.1379.2.0 == "Once a week", 52, 
					ifelse(diet1.2$lambmutton_overallfreq.1379.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$lambmutton_overallfreq.1379.2.0 == "Never", 0, "NA")))))))

#table(diet1.2$pork_overallfreq.1389.2.0,useNA="always")
diet1.2$pork_overallfreq.1389.2.0_QT = as.numeric(ifelse(diet1.2$pork_overallfreq.1389.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$pork_overallfreq.1389.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$pork_overallfreq.1389.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$pork_overallfreq.1389.2.0 == "Once a week", 52, 
					ifelse(diet1.2$pork_overallfreq.1389.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$pork_overallfreq.1389.2.0 == "Never", 0, "NA")))))))

#table(diet1.2$cheese_overallfreq.1408.2.0,useNA="always")
diet1.2$cheese_overallfreq.1408.2.0_QT = as.numeric(ifelse(diet1.2$cheese_overallfreq.1408.2.0 == "Once or more daily", 365, 
					ifelse(diet1.2$cheese_overallfreq.1408.2.0 == "5-6 times a week", 286, 
					ifelse(diet1.2$cheese_overallfreq.1408.2.0 == "2-4 times a week", 156, 
					ifelse(diet1.2$cheese_overallfreq.1408.2.0 == "Once a week", 52, 
					ifelse(diet1.2$cheese_overallfreq.1408.2.0 == "Less than once a week", 20, 
					ifelse(diet1.2$cheese_overallfreq.1408.2.0 == "Never", 0, "NA")))))))

### Data structure as follows:
### Data-coding 100394
### Categorical Variable -> Continuous
	# Always -> 10
	# Usually -> 7
	# Sometimes -> 3
	# Never/rarely -> 0
	# Prefer not to answer -> NA
	# NA

#table(diet1.2$doyouaddsalt.1478.2.0,useNA="always")
diet1.2$doyouaddsalt.1478.2.0_QT = as.numeric(ifelse(diet1.2$doyouaddsalt.1478.2.0 == "Always", 10, 
					ifelse(diet1.2$doyouaddsalt.1478.2.0 == "Usually", 7, 
					ifelse(diet1.2$doyouaddsalt.1478.2.0 == "Sometimes", 3, 
					ifelse(diet1.2$doyouaddsalt.1478.2.0 == "Never/rarely", 0, "NA")))))		

### Data structure as follows:
### Data-coding 100398
### Categorical Variable -> Continuous
	# Very hot -> 10
	# Hot -> 7
	# Warm -> 3
	# Do not drink hot drinks -> 0
	# Prefer not to answer -> set to NA
	# NA

#table(diet1.2$hotdrinktemp.1518.2.0,useNA="always")
diet1.2$hotdrinktemp.1518.2.0_QT = as.numeric(ifelse(diet1.2$hotdrinktemp.1518.2.0 == "Very hot", 10, 
					ifelse(diet1.2$hotdrinktemp.1518.2.0 == "Hot", 7, 
					ifelse(diet1.2$hotdrinktemp.1518.2.0 == "Warm", 3, 
					ifelse(diet1.2$hotdrinktemp.1518.2.0 == "Do not drink hot drinks", 0, "NA")))))

### Data structure as follows:
### Data-coding 100402
### Categorical Variable -> Continuous
	# Daily or almost daily -> 300
	# Three or four times a week -> 182
	# Once or twice a week -> 104
	# One to three times a month -> 24
	# Special occasions only -> 6
	# Never -> 0
	# Prefer not to answer -> set to NA
	# NA

#table(diet1.2$alcohol_overallfreq.1558.2.0,useNA="always")
diet1.2$alcohol_overallfreq.1558.2.0_QT = as.numeric(ifelse(diet1.2$alcohol_overallfreq.1558.2.0 == "Daily or almost daily", 300, 
						ifelse(diet1.2$alcohol_overallfreq.1558.2.0 == "Three or four times a week", 182, 
						ifelse(diet1.2$alcohol_overallfreq.1558.2.0 == "Once or twice a week", 104, 
						ifelse(diet1.2$alcohol_overallfreq.1558.2.0 == "One to three times a month", 24, 
						ifelse(diet1.2$alcohol_overallfreq.1558.2.0 == "Special occasions only", 6, 
						ifelse(diet1.2$alcohol_overallfreq.1558.2.0 == "Never", 0, "NA")))))))

### Data structure as follows:
### Data-coding 90
### Categorical Variable -> Binary x2
	# Current
	# Previous
	# Never
	# Prefer not to answer
	# NA

#table(diet1.2$alcoholdrinkerstatus.20117.2.0,useNA="always")

	# Never (0) vs. Current + Previous (1)
		
diet1.2$alcoholdrinkerstatus.20117.2.0_bin = as.numeric(ifelse(diet1.2$alcoholdrinkerstatus.20117.2.0 == "Never", 0, ifelse(diet1.2$alcoholdrinkerstatus.20117.2.0 == "Previous" | diet1.2$alcoholdrinkerstatus.20117.2.0 == "Current", 1, "NA")))
#table(diet1.2$alcoholdrinkerstatus.20117.2.0_bin,useNA="always")


### this will likely impute previous -> never as the correlation with 0 drinks above will be high
### that's ok. better than making a new variable for this.

	# Never (0) vs. Current only (1)

diet1.2$alcoholdrinkerstatus.20117.2.0_bin2 = as.numeric(ifelse(diet1.2$alcoholdrinkerstatus.20117.2.0 == "Never", 0, ifelse(diet1.2$alcoholdrinkerstatus.20117.2.0 == "Current", 1, "NA")))
#table(diet1.2$alcoholdrinkerstatus.20117.2.0_bin2,useNA="always")

### Data structure as follows:
### Data-coding 100352
### Categorical Variable with:
	# Yes
	# No
	# Prefer not to answer
	# NA

#table(diet1.2$alcohol_formerdrinker_amongcurrentnondrinkers.3731.2.0,useNA="always")
# no additional information not captured in diet1.2$alcoholdrinkerstatus.20117.2.0


### Data structure as follows:
### Data-coding 100416
### Categorical Variable -> Binary x2
### Categorical Variable -> Continuous x1
	# Yes
	# It varies
	# No
	# Do not know
	# Prefer not to answer
	# NA

#table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0,useNA="always")

	# No (0) vs. Yes + It varies (1)

diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin = as.numeric(ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "No", 0, ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "Yes" | diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "It varies", 1, "NA")))
#table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin,useNA="always")

	# No (0) vs. Yes (1)

diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin2 = as.numeric(ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "No", 0, ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "Yes", 1, "NA")))
#table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_bin2,useNA="always")

	# No (0), It varies (1), Yes (2) 

diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_QT3 = as.numeric(ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "No", 0, 
							ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "Yes", 2,
							ifelse(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0 == "It varies", 1, "NA"))))
#table(diet1.2$alcohol_usuallywithmeals_amongcurrentdrinkers.1618.2.0_QT3,useNA="always")

### Data structure as follows:
### Data-coding 100385
### Categorical Variable with:
	# Eggs or foods containing eggs (1) vs. I eat all of the above (0)
	# Dairy products (1) vs. I eat all of the above (0)
	# Wheat products (1) vs. I eat all of the above (0)
	# Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)
	# I eat all of the above : if selected no other choices were allowed
	# prefer not to answer : if selected no other choices were allowed -> set to NA


#table(diet1.2$nevereatcategories.6144.2.0,useNA="always")
#table(diet1.2$nevereatcategories.6144.0.1,useNA="always")
#table(diet1.2$nevereatcategories.6144.0.2,useNA="always")
#table(diet1.2$nevereatcategories.6144.0.3,useNA="always")

diet1.2$nevereatcategories.6144.0.combined = paste(diet1.2$nevereatcategories.6144.2.0, diet1.2$nevereatcategories.6144.0.1, diet1.2$nevereatcategories.6144.0.2,
							diet1.2$nevereatcategories.6144.0.3, sep =":")

#table(diet1.2$nevereatcategories.6144.0.combined,useNA="always")

# Eggs or foods containing eggs (1) vs. I eat all of the above (0)
diet1.2$nevereatcategories.6144.2.0_bin = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin,useNA="always")
# Eggs or foods containing eggs (1) vs. everyone else (0)
diet1.2$nevereatcategories.6144.2.0_bin2 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin2,useNA="always")

# Dairy products (1) vs. I eat all of the above (0)
diet1.2$nevereatcategories.6144.2.0_bin3 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin3,useNA="always")

# Dairy products (1)  vs. everyone else (0)

diet1.2$nevereatcategories.6144.2.0_bin4 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin4,useNA="always")

# Wheat products (1) vs. I eat all of the above (0)
diet1.2$nevereatcategories.6144.2.0_bin5 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", "NA", 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin5,useNA="always")

# Wheat products (1)  vs. everyone else (0)
diet1.2$nevereatcategories.6144.2.0_bin6 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 0, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin6,useNA="always")
	
# Sugar or foods/drinks containing sugar (1) vs. I eat all of the above (0)	
diet1.2$nevereatcategories.6144.2.0_bin7 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin7,useNA="always")

# Sugar or foods/drinks containing sugar (1) vs. everyone else (0))	
diet1.2$nevereatcategories.6144.2.0_bin8 = as.numeric(
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Dairy products:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Sugar or foods/drinks containing sugar:NA", 1, 
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Dairy products:Wheat products:Sugar or foods/drinks containing sugar", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Eggs or foods containing eggs:Wheat products:Sugar or foods/drinks containing sugar:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "I eat all of the above:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Sugar or foods/drinks containing sugar:NA:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:NA:NA:NA", 0,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Wheat products:Sugar or foods/drinks containing sugar:NA:NA", 1,
ifelse(diet1.2$nevereatcategories.6144.0.combined == "NA:NA:NA:NA", "NA",
ifelse(diet1.2$nevereatcategories.6144.0.combined == "Prefer not to answer:NA:NA:NA", "NA",
"NA")))))))))))))))))))
#table(diet1.2$nevereatcategories.6144.2.0_bin8,useNA="always")	
	
	
diet1.2$nevereatcategories.6144.0.combined = NULL	

### Data structure as follows:
### Data-coding 100387
### Categorical Variable with:
	# Full cream
	# Semi-skimmed
	# Skimmed
	# Soya
	# Never/rarely have milk
	# Other type of milk
	# Do not know
	# Prefer not to answer
	# NA

# 1binary: milk x3 vs. never
diet1.2$milk_typeused.1418.2.0_bin = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", "NA", 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin,useNA="always")

# 2binary: milk x5 vs. never
diet1.2$milk_typeused.1418.2.0_bin2 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 1, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin2,useNA="always")

# 3continuous: never (0), skimmed (1), semi-skimmed (2), full cream (3)
diet1.2$milk_typeused.1418.2.0_QT3 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 3,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 2,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", "NA", 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_QT3,useNA="always")

# 4full cream (1) vs. never (0)
diet1.2$milk_typeused.1418.2.0_bin4 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", "NA", 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin4,useNA="always")

# 5full cream (1) vs. all other milk (0)
diet1.2$milk_typeused.1418.2.0_bin5 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 0, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin5,useNA="always")

# 6semi (1) vs. never (0)
diet1.2$milk_typeused.1418.2.0_bin6 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", "NA", 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin6,useNA="always")

# 7semi (1) vs. all other milk (0)
diet1.2$milk_typeused.1418.2.0_bin7 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 0, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin7,useNA="always")

# 8skim (1) vs. never (0)
diet1.2$milk_typeused.1418.2.0_bin8 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", "NA", 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin8,useNA="always")

# 9skim (1) vs. all other milk (0)
diet1.2$milk_typeused.1418.2.0_bin9 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 0, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin9,useNA="always")

# 10soy (1) vs. never (0)
diet1.2$milk_typeused.1418.2.0_bin10 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", "NA", 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin10,useNA="always")

# 11soy (1) vs. all other milk (0)
diet1.2$milk_typeused.1418.2.0_bin11 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 1,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 0, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin11,useNA="always")

# 12other (1) vs. never (0)
diet1.2$milk_typeused.1418.2.0_bin12 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 1, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin12,useNA="always")

# 13other (1) vs. all other milk (0)
diet1.2$milk_typeused.1418.2.0_bin13 = as.numeric(
ifelse(diet1.2$milk_typeused.1418.2.0 == "Full cream", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Semi-skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Skimmed", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Soya", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Other type of milk", 1, 
ifelse(diet1.2$milk_typeused.1418.2.0 == "Never/rarely have milk", 0,
ifelse(diet1.2$milk_typeused.1418.2.0 == "Do not know", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$milk_typeused.1418.2.0 == "NA", "NA",
"NA"))))))))))
#table(diet1.2$milk_typeused.1418.2.0_bin13,useNA="always")

### Data structure as follows:
### Data-coding 100388
### Categorical Variable with:
	# Butter/spreadable butter
	# Flora Pro-Active/Benecol
	# Other type of spread/margarine
	# Never/rarely use spread
	# Do not know
	# Prefer not to answer
	# NA
### Data structure as follows:
### Data-coding 100389
### Categorical Variable with:
	# Hard (block) margarine
	# Soft (tub) margarine
	# Flora Pro-Active or Benecol
	# Olive oil based spread (eg: Bertolli)
	# Polyunsaturated/sunflower oil based spread (eg: Flora)
	# Other low or reduced fat spread
	# Other type of spread/margarine
	# Do not know
	# Prefer not to answer
	# NA

## Field 2654 was collected from participants who indicated they mainly use another type of spread/margarine 
## rather than butter/spreadable butter or do not know what they use, as defined by their answers to Field 1428
table(diet1.2$spread_typeused.1428.2.0,diet1.2$nonbutterspread_typeused.2654.2.0)


diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined = paste(diet1.2$spread_typeused.1428.2.0, diet1.2$nonbutterspread_typeused.2654.2.0, sep =":")
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined,useNA="always")	

## butter like spreads
	#Butter/spreadable butter:NA 

	#Do not know:Hard (block) margarine 
	#Other type of spread/margarine:Hard (block) margarine 

	#Do not know:Soft (tub) margarine 
	#Other type of spread/margarine:Soft (tub) margarine 

	#Do not know:Flora Pro-Active or Benecol
	#Flora Pro-Active/Benecol:NA
	#Other type of spread/margarine:Flora Pro-Active or Benecol 

## oil like spreads
	#Do not know:Olive oil based spread (eg: Bertolli) 
	#Other type of spread/margarine:Olive oil based spread (eg: Bertolli) 

	#Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora) 
	#Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora) 

## other low fat/reduced
	#Do not know:Other low or reduced fat spread 
	#Other type of spread/margarine:Other low or reduced fat spread  

## other
	#Other type of spread/margarine:Other type of spread/margarine 

## never
	#Never/rarely use spread:NA 

## NA
	#Other type of spread/margarine:Do not know
	#Do not know:Do not know
	#Do not know:Prefer not to answer
	#Do not know:Other type of spread/margarine 
	#Prefer not to answer:NA
	#Other type of spread/margarine:Prefer not to answer 
	#NA:NA 


#1 All vs. Never

diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1,useNA="always")

#2 Butter/Margarine vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2,useNA="always")

#3 Oil-based vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3,useNA="always")

#4 Butter (with flora proactive)vs. Oil

diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4,useNA="always")

#5 Butter (without flora proactive)vs. Oil
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5,useNA="always")

#6 Butter vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6,useNA="always")

#7 Butter vs. Other
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7,useNA="always")

#8 Hard (block) margarine vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8,useNA="always")

#9 Hard (block) margarine vs. Other
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9,useNA="always")

#10 Soft (tub) margarine
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10,useNA="always")

#11 Soft (tub) margarine
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11,useNA="always")

#12 Flora vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12,useNA="always")

#13 Flora vs. Other
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13,useNA="always")

#14 Olive oil based spread (eg: Bertolli) vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14,useNA="always")

#15 Olive oil based spread (eg: Bertolli) vs. Other
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15,useNA="always")

#16 Flora oil/sunflower oil vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16,useNA="always")

#17 Flora oil/sunflower oil vs. Other
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17,useNA="always")

#18 Other low or reduced fat spread vs. Never
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18,useNA="always")

#19 Other low or reduced fat spread vs. Other
diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19 = as.numeric(
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Butter/spreadable butter:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Hard (block) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Soft (tub) margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Flora Pro-Active/Benecol:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Flora Pro-Active or Benecol", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Olive oil based spread (eg: Bertolli)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Polyunsaturated/sunflower oil based spread (eg: Flora)", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other low or reduced fat spread", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other low or reduced fat spread", 1,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Do not know", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Other type of spread/margarine", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Never/rarely use spread:NA", 0,
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Do not know", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Do not know:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Prefer not to answer:NA", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "Other type of spread/margarine:Prefer not to answer", "NA",
ifelse(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined == "NA:NA", "NA",
"NA"))))))))))))))))))))))))
#table(diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19,useNA="always")

diet1.2$spread_typeused.1428.0.nonbutterspread_typeused.2654.combined = NULL

### Field 1448 was collected from all participants except those who indicated that they do not eat bread or less than one slice each week, as defined by their answers to Field 1438


### Data structure as follows:
### Data-coding 100391
### Categorical Variable with:
	# White
	# Brown
	# Wholemeal or wholegrain
	# Other type of bread
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.2$bread_typeused.1448.2.0,useNA="always")

##1 White vs Other
diet1.2$bread_typeused.1448.2.0_bin = as.numeric(
ifelse(diet1.2$bread_typeused.1448.2.0 == "White", 1,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Brown", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Other type of bread", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Do not know", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$bread_typeused.1448.2.0_bin,useNA="always")

##2 Brown vs. Other
diet1.2$bread_typeused.1448.2.0_bin2 = as.numeric(
ifelse(diet1.2$bread_typeused.1448.2.0 == "White", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Brown", 1,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Other type of bread", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Do not know", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$bread_typeused.1448.2.0_bin2,useNA="always")

##3 Wholemeal vs. Other
diet1.2$bread_typeused.1448.2.0_bin3 = as.numeric(
ifelse(diet1.2$bread_typeused.1448.2.0 == "White", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Brown", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Wholemeal or wholegrain", 1,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Other type of bread", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Do not know", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$bread_typeused.1448.2.0_bin3,useNA="always")

##4 White vs. Brown + Wholemeal
diet1.2$bread_typeused.1448.2.0_bin4 = as.numeric(
ifelse(diet1.2$bread_typeused.1448.2.0 == "White", 1,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Brown", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Wholemeal or wholegrain", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Other type of bread", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Do not know", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$bread_typeused.1448.2.0_bin4,useNA="always")

##5 White + Brown vs. Wholemeal
diet1.2$bread_typeused.1448.2.0_bin5 = as.numeric(
ifelse(diet1.2$bread_typeused.1448.2.0 == "White", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Brown", 0,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Wholemeal or wholegrain", 1,
ifelse(diet1.2$bread_typeused.1448.2.0 == "Other type of bread", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Do not know", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$bread_typeused.1448.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$bread_typeused.1448.2.0_bin5,useNA="always")

### Field 1468 was collected from all participants except those who indicated that they do not eat cereal or less than one bowl of cereal each week, as defined by their answers to Field 1458

### Data structure as follows:
### Data-coding 100393
### Categorical Variable with:
	# Biscuit cereal (e.g. Weetabix) 
	# Bran cereal (e.g. All Bran, Branflakes)
	# Oat cereal (e.g. Ready Brek, porridge)
	# Muesli
	# Other (e.g. Cornflakes, Frosties)
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.2$cereal_typeused.1468.2.0,useNA="always")

# Biscuit vs. All Other
diet1.2$cereal_typeused.1468.2.0_bin = as.numeric(
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Biscuit cereal (e.g. Weetabix)", 1,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Muesli", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Do not know", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "NA", "NA",
"NA")))))))))
#table(diet1.2$cereal_typeused.1468.2.0_bin,useNA="always")

# Bran vs. All Other
diet1.2$cereal_typeused.1468.2.0_bin2 = as.numeric(
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Bran cereal (e.g. All Bran, Branflakes)", 1,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Muesli", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Do not know", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "NA", "NA",
"NA")))))))))
#table(diet1.2$cereal_typeused.1468.2.0_bin2,useNA="always")

# Oat vs. All Other
diet1.2$cereal_typeused.1468.2.0_bin3 = as.numeric(
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Oat cereal (e.g. Ready Brek, porridge)", 1,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Muesli", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Do not know", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "NA", "NA",
"NA")))))))))
#table(diet1.2$cereal_typeused.1468.2.0_bin3,useNA="always")

# Muesli vs. All Other
diet1.2$cereal_typeused.1468.2.0_bin4 = as.numeric(
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Muesli", 1,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Other (e.g. Cornflakes, Frosties)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Do not know", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "NA", "NA",
"NA")))))))))
#table(diet1.2$cereal_typeused.1468.2.0_bin4,useNA="always")

# Other vs. All Other
diet1.2$cereal_typeused.1468.2.0_bin5 = as.numeric(
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Biscuit cereal (e.g. Weetabix)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Bran cereal (e.g. All Bran, Branflakes)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Oat cereal (e.g. Ready Brek, porridge)", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Muesli", 0,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Other (e.g. Cornflakes, Frosties)", 1,
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Do not know", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$cereal_typeused.1468.2.0 == "NA", "NA",
"NA")))))))))
#table(diet1.2$cereal_typeused.1468.2.0_bin5,useNA="always")

### Field 1508 was collected from participants who indicated that they drank at least 1 cup of coffee each day or less than one cup each day, as defined by their answers to Field 1498

### Data structure as follows:
### Data-coding 100397
### Categorical Variable with:
	# Ground coffee (include espresso, filter etc)
	# Instant coffee
	# Decaffeinated coffee (any type)
	# Other type of coffee
	# Do not know
	# Prefer not to answer
	# NA
	
#table(diet1.2$coffee_typeused.1508.2.0,useNA="always")

# Ground vs. Other
diet1.2$coffee_typeused.1508.2.0_bin = as.numeric(
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Ground coffee (include espresso, filter etc)", 1,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Instant coffee", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Decaffeinated coffee (any type)", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Other type of coffee", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Do not know", "NA",
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$coffee_typeused.1508.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$coffee_typeused.1508.2.0_bin,useNA="always")

# instant vs. other
diet1.2$coffee_typeused.1508.2.0_bin2 = as.numeric(
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Ground coffee (include espresso, filter etc)", 1,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Instant coffee", 1,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Decaffeinated coffee (any type)", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Other type of coffee", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Do not know", "NA",
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$coffee_typeused.1508.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$coffee_typeused.1508.2.0_bin2,useNA="always")

# decaff vs. other
diet1.2$coffee_typeused.1508.2.0_bin3 = as.numeric(
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Ground coffee (include espresso, filter etc)", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Instant coffee", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Decaffeinated coffee (any type)", 1,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Other type of coffee", 0,
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Do not know", "NA",
ifelse(diet1.2$coffee_typeused.1508.2.0 == "Prefer not to answer", "NA",
ifelse(diet1.2$coffee_typeused.1508.2.0 == "NA", "NA",
"NA"))))))))
#table(diet1.2$coffee_typeused.1508.2.0_bin3,useNA="always")


####################
#save diet1.2 dataset
colnames(diet1.2)
save_diet1.2 = diet1.2[,c(1,2,77, 122:219)]
write.table(save_diet1.2, "/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1.2_07062018update", col.names = TRUE, row.names = FALSE, quote = FALSE, sep ="\t")



##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

diet1.0 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1.0_07062018update", verbose=TRUE, data.table=FALSE)
diet1.1 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1.1_07062018update", verbose=TRUE, data.table=FALSE)
diet1.2 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1.2_07062018update", verbose=TRUE, data.table=FALSE)

colnames(diet1.0)[which(colnames(diet1.0) == "instance")] = "instance.0"
colnames(diet1.1)[which(colnames(diet1.1) == "instance")] = "instance.1"
colnames(diet1.2)[which(colnames(diet1.2) == "instance")] = "instance.2"



## In theory, averaging them will only increase the accuracy but reducing the SE for SOME individuals. Can't hurt, right?!
## potential downside is if different instances add too much noise due to other confounding factors
## but that's what h2g analysis is for!

tmp = merge(diet1.0, diet1.1, by="ID", all=T)
data = merge(tmp, diet1.2, by="ID", all=T)

## pheno are columns 5-101, 
## 5, 98, 191 are starters
save = NULL
for (i in 5:101)
{
x = rowMeans(subset(data[,c(i,i+100,i+200)]), na.rm=TRUE)
save = cbind(save, x)
}

colnames(save) = paste0(colnames(data)[5:101], "_average")
data2 = cbind(data,save)


## create a new age variable if data was averaged.
data2$DIET1_age_months.average = ifelse((is.na(data2$DIET1_age_months.2.0) & is.na(data2$DIET1_age_months.1.0) & !is.na(data2$DIET1_age_months.0.0)), data2$DIET1_age_months.0.0, # just 0
				ifelse((is.na(!data2$DIET1_age_months.2.0) & is.na(data2$DIET1_age_months.1.0) & is.na(data2$DIET1_age_months.0.0)), data2$DIET1_age_months.2.0, # just 2
				ifelse((is.na(data2$DIET1_age_months.2.0) & !is.na(data2$DIET1_age_months.1.0) & is.na(data2$DIET1_age_months.0.0)), data2$DIET1_age_months.1.0, # just 1
				ifelse((is.na(data2$DIET1_age_months.2.0) & !is.na(data2$DIET1_age_months.1.0) & !is.na(data2$DIET1_age_months.0.0)), rowMeans(subset(data2, select = c(DIET1_age_months.0.0, DIET1_age_months.1.0)), na.rm = TRUE), # 0 & 1
				ifelse((!is.na(data2$DIET1_age_months.2.0) & !is.na(data2$DIET1_age_months.1.0) & is.na(data2$DIET1_age_months.0.0)), rowMeans(subset(data2, select = c(DIET1_age_months.1.0, DIET1_age_months.2.0)), na.rm = TRUE), # 1 & 2 
				ifelse((!is.na(data2$DIET1_age_months.2.0) & is.na(data2$DIET1_age_months.1.0) & !is.na(data2$DIET1_age_months.0.0)), rowMeans(subset(data2, select = c(DIET1_age_months.0.0, DIET1_age_months.2.0)), na.rm = TRUE), # 0 & 2
				ifelse((!is.na(data2$DIET1_age_months.2.0) & !is.na(data2$DIET1_age_months.1.0) & !is.na(data2$DIET1_age_months.0.0)), rowMeans(subset(data2, select = c(DIET1_age_months.0.0, DIET1_age_months.1.0, DIET1_age_months.2.0)), na.rm = TRUE),"NA"))))))) # 0 & 1 & 2


colnames(data2)
saveme = data2[,c(1,2,302:399)]
colnames(saveme)[2] = "selfreportsex.31"


colnames(saveme) = c(
"ID",
"selfreportsex.31",
"cookedveg_TBSperday.1289.average_QT",
"rawveg_TBSperday.1299.average_QT",
"freshfruit_piecesperday.1309.average_QT",
"driedfruit_piecesperday.1319.average_QT",
"bread_slicesperweek.1438.average_QT",
"cereal_bowlsperweek.1458.average_QT",
"tea_cupsperday.1488.average_QT",
"coffee_cupsperday.1498.average_QT",
"water_glassesperday.1528.average_QT",
"champagnewhitewine_glassesperweek.1578.average_QT",
"champagnewhitewine_glassespermonth.4418.average_QT",
"champagnewhitewine_glassespermonth.derived.average_QT",
"redwine_glassesperweek.1568.average_QT",
"redwine_glassespermonth.4407.average_QT",
"redwine_glassespermonth.derived.average_QT",
"beercider_pintsperweek.1588.average_QT",
"beercider_pintspermonth.4429.average_QT",
"beercider_pintspermonth.derived.average_QT",
"spirits_measuresperweek.1598.average_QT",
"spirits_measurespermonth.4440.average_QT",
"spirits_measurespermonth.derived.average_QT",
"fortwine_glassesperweek.1608.average_QT",
"fortwine_glassespermonth.4451.average_QT",
"fortwine_glassespermonth.derived.average_QT",
"otheralcohol_glassesperweek.5364.average_QT",
"otheralcohol_glassespermonth.4462.average_QT",
"otheralcohol_glassespermonth.derived.average_QT",
"anyalcohol_glassespermonth.derived.average_QT",
"oilyfish_overallfreq.1329.average_QT",
"nonoilyfish_overallfreq.1339.average_QT",
"processmeat_overallfreq.1349.average_QT",
"poultry_overallfreq.1359.average_QT",
"beef_overallfreq.1369.average_QT",
"lambmutton_overallfreq.1379.average_QT",
"pork_overallfreq.1389.average_QT",
"cheese_overallfreq.1408.average_QT",
"doyouaddsalt.1478.average_QT",
"hotdrinktemp.1518.average_QT",
"alcohol_overallfreq.1558.average_QT",
"alcoholdrinkerstatus.20117.average_bin",
"alcoholdrinkerstatus.20117.average_bin2",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.average_bin",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.average_bin2",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.average_QT3",
"nevereatcategories.6144.average_bin",
"nevereatcategories.6144.average_bin2",
"nevereatcategories.6144.average_bin3",
"nevereatcategories.6144.average_bin4",
"nevereatcategories.6144.average_bin5",
"nevereatcategories.6144.average_bin6",
"nevereatcategories.6144.average_bin7",
"nevereatcategories.6144.average_bin8",
"milk_typeused.1418.average_bin",
"milk_typeused.1418.average_bin2",
"milk_typeused.1418.average_QT3",
"milk_typeused.1418.average_bin4",
"milk_typeused.1418.average_bin5",
"milk_typeused.1418.average_bin6",
"milk_typeused.1418.average_bin7",
"milk_typeused.1418.average_bin8",
"milk_typeused.1418.average_bin9",
"milk_typeused.1418.average_bin10",
"milk_typeused.1418.average_bin11",
"milk_typeused.1418.average_bin12",
"milk_typeused.1418.average_bin13",
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
"bread_typeused.1448.average_bin",
"bread_typeused.1448.average_bin2",
"bread_typeused.1448.average_bin3",
"bread_typeused.1448.average_bin4",
"bread_typeused.1448.average_bin5",
"cereal_typeused.1468.average_bin",
"cereal_typeused.1468.average_bin2",
"cereal_typeused.1468.average_bin3",
"cereal_typeused.1468.average_bin4",
"cereal_typeused.1468.average_bin5",
"coffee_typeused.1508.average_bin",
"coffee_typeused.1508.average_bin2",
"coffee_typeused.1508.average_bin3",
"DIET1_age_months.average"
)


write.table(saveme, "/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1_average_07062018update", col.names = TRUE, row.names = FALSE, quote = FALSE, sep ="\t")


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
################### How to FFQ Diet Phenotypes - Hirschhorn Application
################### STEP2: Adjust and transform
########## last updated: July 6, 2018
########## JBC
######################################################################################################################################

##
##########################################################################################################################
##########################################################################################################################
# load in phenotype files:
library(data.table)
diet1 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet1_average_07062018update", data.table=FALSE,verbose=TRUE)
diet2ever = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet2_evernever_06222018", data.table=FALSE,verbose=TRUE)
diet2prop = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/ukbiobank_diet2_prop_EB_062218", data.table=FALSE,verbose=TRUE)


# limit to EUR clean
clean = fread("/broad/jhlab_ukbiobank/users/jcole/Pheno/UKBiobank_genoQC_reportedANDgeneticEUR_N455146_FID_IID_noheader.txt", data.table=FALSE,verbose=TRUE)

diet1 = diet1[(diet1$ID %in% clean$V1),]
diet2ever = diet2ever[(diet2ever$ID %in% clean$V1),]
diet2prop = diet2prop[(diet2prop$ID %in% clean$V1),]


#################################
## Covariates & Transformation
## start with just sex and age and assessment center (as a proxy for location)
## array + 10 PCs (in genetic model)
##############
## I don't know why but I'm freaking converting sex to 0/1
diet1$sex = ifelse(diet1$selfreportsex.31 == "Male", 0,ifelse(diet1$selfreportsex.31 == "Female", 1, "NA"))
diet2ever$sex = ifelse(diet2ever$selfreportsex.31 == "Male", 0,ifelse(diet2ever$selfreportsex.31 == "Female", 1, "NA"))
###sex is not complete in diet2prop because it is only based on 1 instance
###therefore use the sex from the other diet2 file (which I corrected in the original diet2evernever commands)
sex = diet2ever[,c("ID","sex")]
tmp = merge(diet2prop,sex, by ="ID",all.x=T)
diet2prop = tmp

## read in asessment center covariate
## awk '{print $20}' 
#	gunzip -c /broad/jhlab_ukbiobank/PhenoData/ukb6184_detailed.tab.gz | \
#	awk '{FS = "\t"; OFS = "\t"; print $1, $20 \
#	}' > /broad/jhlab_ukbiobank/users/jcole/Pheno/ukbiobank_assessmentcenter_54.0.0

#	gunzip -c /broad/jhlab_ukbiobank/PhenoData/ukb7052_detailed.tab.gz | \
#	awk '{FS = "\t"; OFS = "\t"; print $1, $86 \
#	}' > /broad/jhlab_ukbiobank/users/jcole/Pheno/ukbiobank_countryofbirth_1647.0.0

birth = read.table("/broad/jhlab_ukbiobank/users/jcole/Pheno/ukbiobank_countryofbirth_1647.0.0", header = T, sep ="\t",stringsAsFactors=F)
center = read.table("/broad/jhlab_ukbiobank/users/jcole/Pheno/ukbiobank_assessmentcenter_54.0.0", header = T)

center$f.54.0.0_categorical <- NA
center$f.54.0.0_categorical[center$f.54.0.0 == 11012] <- "Barts"
center$f.54.0.0_categorical[center$f.54.0.0 == 11021] <- "Birmingham"
center$f.54.0.0_categorical[center$f.54.0.0 == 11011] <- "Bristol"
center$f.54.0.0_categorical[center$f.54.0.0 == 11008] <- "Bury"
center$f.54.0.0_categorical[center$f.54.0.0 == 11003] <- "Cardiff"
center$f.54.0.0_categorical[center$f.54.0.0 == 11024] <- "Cheadle_revisit"
center$f.54.0.0_categorical[center$f.54.0.0 == 11020] <- "Croydon"
center$f.54.0.0_categorical[center$f.54.0.0 == 11005] <- "Edinburgh"
center$f.54.0.0_categorical[center$f.54.0.0 == 11004] <- "Glasgow"
center$f.54.0.0_categorical[center$f.54.0.0 == 11018] <- "Hounslow"
center$f.54.0.0_categorical[center$f.54.0.0 == 11010] <- "Leeds"
center$f.54.0.0_categorical[center$f.54.0.0 == 11016] <- "Liverpool"
center$f.54.0.0_categorical[center$f.54.0.0 == 11001] <- "Manchester"
center$f.54.0.0_categorical[center$f.54.0.0 == 11017] <- "Middlesborough"
center$f.54.0.0_categorical[center$f.54.0.0 == 11009] <- "Newcastle"
center$f.54.0.0_categorical[center$f.54.0.0 == 11013] <- "Nottingham"
center$f.54.0.0_categorical[center$f.54.0.0 == 11002] <- "Oxford"
center$f.54.0.0_categorical[center$f.54.0.0 == 11007] <- "Reading"
center$f.54.0.0_categorical[center$f.54.0.0 == 11014] <- "Sheffield"
center$f.54.0.0_categorical[center$f.54.0.0 == 10003] <- "Stockport_pilot"
center$f.54.0.0_categorical[center$f.54.0.0 == 11006] <- "Stoke"
center$f.54.0.0_categorical[center$f.54.0.0 == 11022] <- "Swansea"
center$f.54.0.0_categorical[center$f.54.0.0 == 11023] <- "Wrexham"
center$f.54.0.0_categorical[center$f.54.0.0 == 11025] <- "Cheadle_pilot"
center$f.54.0.0_categorical[center$f.54.0.0 == 11027] <- "Newcastle_pilot"

saveme = merge(center, birth, by ="f.eid",all=T)

saveme$f.1647.0.0_categorical = saveme$f.1647.0.0
saveme$f.1647.0.0_categorical[is.na(saveme$f.1647.0.0)] <- "Do not know"
table(saveme$f.1647.0.0_categorical, useNA="always")
saveme$f.1647.0.0_categorical[saveme$f.1647.0.0 == "Prefer not to answer"] <- "Do not know"
table(saveme$f.1647.0.0_categorical, useNA="always")

write.table(saveme,"/broad/jhlab_ukbiobank/users/jcole/Pheno/ukbiobank_assessmentcenter_54.0.0", col.names =T, sep ="\t", row.names=F, eol="\n", quote = F)
center = read.table("/broad/jhlab_ukbiobank/users/jcole/Pheno/ukbiobank_assessmentcenter_54.0.0", header = T, sep ="\t")


#### split diet1 into bin and QT
##colnames(diet1)
##diet1_bin = diet1[,c(
##"ID",
##"DIET1_age_months.0.0",
##"sex",
##"alcoholdrinkerstatus.20117.0.0_bin",
##"alcoholdrinkerstatus.20117.0.0_bin2",
##"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin",
##"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_bin2",
##"nevereatcategories.6144.0.0_bin",
##"nevereatcategories.6144.0.0_bin2",
##"nevereatcategories.6144.0.0_bin3",
##"nevereatcategories.6144.0.0_bin4",
##"nevereatcategories.6144.0.0_bin5",
##"nevereatcategories.6144.0.0_bin6",
##"nevereatcategories.6144.0.0_bin7",
##"nevereatcategories.6144.0.0_bin8",
##"milk_typeused.1418.0.0_bin",
##"milk_typeused.1418.0.0_bin2",
##"milk_typeused.1418.0.0_bin4",
##"milk_typeused.1418.0.0_bin5",
##"milk_typeused.1418.0.0_bin6",
##"milk_typeused.1418.0.0_bin7",
##"milk_typeused.1418.0.0_bin8",
##"milk_typeused.1418.0.0_bin9",
##"milk_typeused.1418.0.0_bin10",
##"milk_typeused.1418.0.0_bin11",
##"milk_typeused.1418.0.0_bin12",
##"milk_typeused.1418.0.0_bin13",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin1",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin2",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin3",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin4",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin5",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin6",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin7",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin8",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin9",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin10",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin11",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin12",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin13",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin14",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin15",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin16",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin17",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin18",
##"spread_typeused.1428.0.nonbutterspread_typeused.2654.combined_bin19",
##"bread_typeused.1448.0.0_bin",
##"bread_typeused.1448.0.0_bin2",
##"bread_typeused.1448.0.0_bin3",
##"bread_typeused.1448.0.0_bin4",
##"bread_typeused.1448.0.0_bin5",
##"cereal_typeused.1468.0.0_bin",
##"cereal_typeused.1468.0.0_bin2",
##"cereal_typeused.1468.0.0_bin3",
##"cereal_typeused.1468.0.0_bin4",
##"cereal_typeused.1468.0.0_bin5",
##"coffee_typeused.1508.0.0_bin",
##"coffee_typeused.1508.0.0_bin2",
##"coffee_typeused.1508.0.0_bin3"
##)]

diet1_bin = diet1[,c(
"ID",
"DIET1_age_months.average",
"sex",
"alcoholdrinkerstatus.20117.average_bin",
"alcoholdrinkerstatus.20117.average_bin2",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.average_bin",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.average_bin2",
"nevereatcategories.6144.average_bin",
"nevereatcategories.6144.average_bin2",
"nevereatcategories.6144.average_bin3",
"nevereatcategories.6144.average_bin4",
"nevereatcategories.6144.average_bin5",
"nevereatcategories.6144.average_bin6",
"nevereatcategories.6144.average_bin7",
"nevereatcategories.6144.average_bin8",
"milk_typeused.1418.average_bin",
"milk_typeused.1418.average_bin2",
"milk_typeused.1418.average_bin4",
"milk_typeused.1418.average_bin5",
"milk_typeused.1418.average_bin6",
"milk_typeused.1418.average_bin7",
"milk_typeused.1418.average_bin8",
"milk_typeused.1418.average_bin9",
"milk_typeused.1418.average_bin10",
"milk_typeused.1418.average_bin11",
"milk_typeused.1418.average_bin12",
"milk_typeused.1418.average_bin13",
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
"bread_typeused.1448.average_bin",
"bread_typeused.1448.average_bin2",
"bread_typeused.1448.average_bin3",
"bread_typeused.1448.average_bin4",
"bread_typeused.1448.average_bin5",
"cereal_typeused.1468.average_bin",
"cereal_typeused.1468.average_bin2",
"cereal_typeused.1468.average_bin3",
"cereal_typeused.1468.average_bin4",
"cereal_typeused.1468.average_bin5",
"coffee_typeused.1508.average_bin",
"coffee_typeused.1508.average_bin2",
"coffee_typeused.1508.average_bin3")]



##diet1_QT = diet1[,c(
##"ID",
##"DIET1_age_months.0.0",
##"sex",
##"cookedveg_TBSperday.1289.0.0_QT",
##"rawveg_TBSperday.1299.0.0_QT",
##"freshfruit_piecesperday.1309.0.0_QT",
##"driedfruit_piecesperday.1319.0.0_QT",
##"bread_slicesperweek.1438.0.0_QT",
##"cereal_bowlsperweek.1458.0.0_QT",
##"tea_cupsperday.1488.0.0_QT",
##"coffee_cupsperday.1498.0.0_QT",
##"water_glassesperday.1528.0.0_QT",
##"champagnewhitewine_glassesperweek.1578.0.0_QT",
##"redwine_glassesperweek.1568.0.0_QT",
##"beercider_pintsperweek.1588.0.0_QT",
##"spirits_measuresperweek.1598.0.0_QT",
##"fortwine_glassesperweek.1608.0.0_QT",
##"redwine_glassespermonth.4407.0.0_QT",
##"champagnewhitewine_glassespermonth.4418.0.0_QT",
##"beercider_pintspermonth.4429.0.0_QT",
##"spirits_measurespermonth.4440.0.0_QT",
##"fortwine_glassespermonth.4451.0.0_QT",
##"otheralcohol_glassespermonth.4462.0.0_QT",
##"otheralcohol_glassesperweek.5364.0.0_QT",
##"oilyfish_overallfreq.1329.0.0_QT",
##"nonoilyfish_overallfreq.1339.0.0_QT",
##"processmeat_overallfreq.1349.0.0_QT",
##"poultry_overallfreq.1359.0.0_QT",
##"beef_overallfreq.1369.0.0_QT",
##"lambmutton_overallfreq.1379.0.0_QT",
##"pork_overallfreq.1389.0.0_QT",
##"cheese_overallfreq.1408.0.0_QT",
##"doyouaddsalt.1478.0.0_QT",
##"hotdrinktemp.1518.0.0_QT",
##"alcohol_overallfreq.1558.0.0_QT",
##"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.0.0_QT3",
##"milk_typeused.1418.0.0_QT3"
##)]

diet1_QT = diet1[,c(
"ID",
"DIET1_age_months.average",
"sex",
"cookedveg_TBSperday.1289.average_QT",
"rawveg_TBSperday.1299.average_QT",
"freshfruit_piecesperday.1309.average_QT",
"driedfruit_piecesperday.1319.average_QT",
"bread_slicesperweek.1438.average_QT",
"cereal_bowlsperweek.1458.average_QT",
"tea_cupsperday.1488.average_QT",
"coffee_cupsperday.1498.average_QT",
"water_glassesperday.1528.average_QT",
"champagnewhitewine_glassesperweek.1578.average_QT",
"champagnewhitewine_glassespermonth.4418.average_QT",
"champagnewhitewine_glassespermonth.derived.average_QT",
"redwine_glassesperweek.1568.average_QT",
"redwine_glassespermonth.4407.average_QT",
"redwine_glassespermonth.derived.average_QT",
"beercider_pintsperweek.1588.average_QT",
"beercider_pintspermonth.4429.average_QT",
"beercider_pintspermonth.derived.average_QT",
"spirits_measuresperweek.1598.average_QT",
"spirits_measurespermonth.4440.average_QT",
"spirits_measurespermonth.derived.average_QT",
"fortwine_glassesperweek.1608.average_QT",
"fortwine_glassespermonth.4451.average_QT",
"fortwine_glassespermonth.derived.average_QT",
"otheralcohol_glassesperweek.5364.average_QT",
"otheralcohol_glassespermonth.4462.average_QT",
"otheralcohol_glassespermonth.derived.average_QT",
"anyalcohol_glassespermonth.derived.average_QT",
"oilyfish_overallfreq.1329.average_QT",
"nonoilyfish_overallfreq.1339.average_QT",
"processmeat_overallfreq.1349.average_QT",
"poultry_overallfreq.1359.average_QT",
"beef_overallfreq.1369.average_QT",
"lambmutton_overallfreq.1379.average_QT",
"pork_overallfreq.1389.average_QT",
"cheese_overallfreq.1408.average_QT",
"doyouaddsalt.1478.average_QT",
"hotdrinktemp.1518.average_QT",
"alcohol_overallfreq.1558.average_QT",
"alcohol_usuallywithmeals_amongcurrentdrinkers.1618.average_QT3",
"milk_typeused.1418.average_QT3")]


#### IDs being covariate file - PCs and array
#diet1_bin_merged = merge(diet1_bin,IDS, by = "ID", all.y = T)
#diet1_bin_adj = NULL
#for (i in 4:59)
#	{
#	a = resid(glm(as.numeric(diet1_bin_merged[,i])~as.numeric(DIET1_age_months.0.0) + sex + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = diet1_bin_merged, na.action = na.exclude, family = binomial))
#	diet1_bin_adj = cbind(diet1_bin_adj,a)
#	}
#test = cbind(diet1_bin$ID,diet1_bin_adj)
#colnames(diet1_bin_adj) = colnames(diet1_bin_merged)[4:59]
#diet1_bin_adj = cbind(diet1_bin_merged$ID, diet1_bin_adj)
#colnames(diet1_bin_adj)[1] <- "ID"

##diet1_bin = merge(diet1_bin, center, by.x ="ID", by.y = "f.eid")
##
##diet1_bin_adj = NULL
##for (i in 4:59)
##	{
##	a = resid(glm(as.numeric(diet1_bin[,i])~as.numeric(DIET1_age_months.0.0) + sex + f.54.0.0, data = diet1_bin, na.action = na.exclude, family = binomial))
##	diet1_bin_adj = cbind(diet1_bin_adj,a)
##	}
##colnames(diet1_bin_adj) = colnames(diet1_bin)[4:59]
##diet1_bin_adj = cbind(diet1_bin$ID, diet1_bin_adj)
##colnames(diet1_bin_adj)[1] <- "ID"


diet1_bin = merge(diet1_bin, center, by.x ="ID", by.y = "f.eid")

diet1_bin_adj = NULL
for (i in 4:59)
	{
	a = resid(glm(as.numeric(diet1_bin[,i])~as.numeric(DIET1_age_months.average) + sex, data = diet1_bin, na.action = na.exclude, family = binomial))
	diet1_bin_adj = cbind(diet1_bin_adj,a)
	}
colnames(diet1_bin_adj) = colnames(diet1_bin)[4:59]
diet1_bin_adj = cbind(diet1_bin$ID, diet1_bin_adj)
colnames(diet1_bin_adj)[1] <- "ID"

##### THIS tests whether running logistic regression on binary variables that are no longer binary because we averaged over all repeat visits is OK
#	x = resid(glm(as.numeric(diet1_bin[,4])~as.numeric(DIET1_age_months.average) + sex + f.54.0.0, data = diet1_bin, na.action = na.exclude, family = binomial))
#	y = resid(lm(as.numeric(diet1_bin[,4])~as.numeric(DIET1_age_months.average) + sex + f.54.0.0, data = diet1_bin, na.action = na.exclude))
#	z = cbind(x,y)
# 	w = df(z) 
#	cor(w$x,w$y,use="complete")
	# 0.9972296
# good enough for now, but maybe worth changing later


##### now let's test the center variable
##### as a dummy coded categorical variable it makes a lot more sense
##### let's also put in place of birth

########fit = glm(as.numeric(diet1_bin[,4])~as.numeric(DIET1_age_months.average) + sex + f.54.0.0, data = diet1_bin, na.action = na.exclude, family = binomial)
########summary(fit)
########                                       Estimate Std. Error z value Pr(>|z|)    
########	(Intercept)                           8.450e+00  3.242e+00   2.607  0.00915 ** 
########	as.numeric(DIET1_age_months.average) -3.452e-03  9.592e-05 -35.989  < 2e-16 ***
########	sex1                                 -9.943e-01  1.963e-02 -50.665  < 2e-16 ***
########	f.54.0.0                             -1.791e-04  2.944e-04  -0.609  0.54283    
########
########fit = glm(as.numeric(diet1_bin[,4])~as.numeric(DIET1_age_months.average) + sex + f.54.0.0_categorical, data = diet1_bin, na.action = na.exclude, family = binomial)
########summary(fit)
########
########	Coefficients:
########					       Estimate Std. Error z value Pr(>|z|)    
########	(Intercept)                           6.679e+00  9.567e-02  69.809  < 2e-16 ***
########	as.numeric(DIET1_age_months.average) -3.461e-03  9.616e-05 -35.987  < 2e-16 ***
########	sex1                                 -9.982e-01  1.964e-02 -50.830  < 2e-16 ***
########	f.54.0.0_categoricalBirmingham       -4.084e-01  7.497e-02  -5.448 5.09e-08 ***
########	f.54.0.0_categoricalBristol          -1.394e-01  7.204e-02  -1.935 0.053036 .  
########	f.54.0.0_categoricalBury             -1.800e-01  7.457e-02  -2.414 0.015785 *  
########	f.54.0.0_categoricalCardiff          -3.320e-01  7.802e-02  -4.255 2.09e-05 ***
########	f.54.0.0_categoricalCroydon          -1.581e-01  7.691e-02  -2.056 0.039805 *  
########	f.54.0.0_categoricalEdinburgh        -3.482e-02  8.168e-02  -0.426 0.669919    
########	f.54.0.0_categoricalGlasgow          -5.633e-01  7.531e-02  -7.479 7.48e-14 ***
########	f.54.0.0_categoricalHounslow          3.922e-02  7.879e-02   0.498 0.618633    
########	f.54.0.0_categoricalLeeds            -1.645e-01  7.172e-02  -2.294 0.021793 *  
########	f.54.0.0_categoricalLiverpool        -9.363e-02  7.401e-02  -1.265 0.205875    
########	f.54.0.0_categoricalManchester       -2.951e-02  8.641e-02  -0.342 0.732689    
########	f.54.0.0_categoricalMiddlesborough   -2.688e-01  7.642e-02  -3.517 0.000436 ***
########	f.54.0.0_categoricalNewcastle        -3.104e-01  7.187e-02  -4.319 1.56e-05 ***
########	f.54.0.0_categoricalNottingham       -1.925e-01  7.313e-02  -2.632 0.008492 ** 
########	f.54.0.0_categoricalOxford            1.271e-02  8.553e-02   0.149 0.881904    
########	f.54.0.0_categoricalReading          -5.833e-02  7.544e-02  -0.773 0.439397    
########	f.54.0.0_categoricalSheffield        -1.197e-01  7.445e-02  -1.608 0.107808    
########	f.54.0.0_categoricalStockport_pilot   6.976e-02  3.205e-01   0.218 0.827685    
########	f.54.0.0_categoricalStoke            -3.879e-01  7.659e-02  -5.065 4.08e-07 ***
########	f.54.0.0_categoricalSwansea          -2.343e-01  1.396e-01  -1.678 0.093337 .  
########	f.54.0.0_categoricalWrexham          -6.898e-01  2.000e-01  -3.450 0.000561 ***
########
############# what about the place of birth variable
########
########fit = glm(as.numeric(diet1_bin[,4])~as.numeric(DIET1_age_months.average) + sex + f.54.0.0_categorical + f.1647.0.0_categorical, data = diet1_bin, na.action = na.exclude, family = binomial)
########summary(fit)



#diet1_QT_merged = merge(diet1_QT,IDS, by = "ID", all.y = T)

#diet1_QT_adj = NULL
#for (i in 4:37)
#	{
#	a = resid(lm(as.numeric(diet1_QT_merged[,i])~as.numeric(DIET1_age_months.0.0) + sex + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = diet1_QT_merged, na.action = na.exclude))
#	x = qnorm((rank(a,na.last="keep")-0.5)/sum(!is.na(a)))
#	diet1_QT_adj = cbind(diet1_QT_adj,x)
#	}
#colnames(diet1_QT_adj) = colnames(diet1_QT_merged)[4:37]
#diet1_QT_adj = cbind(diet1_QT_merged$ID, diet1_QT_adj)
#colnames(diet1_QT_adj)[1] <- "ID"

##diet1_QT = merge(diet1_QT, center, by.x ="ID", by.y = "f.eid")
##
##diet1_QT_adj = NULL
##for (i in 4:37)
##	{
##	a = resid(lm(as.numeric(diet1_QT[,i])~as.numeric(DIET1_age_months.0.0) + sex + f.54.0.0, data = diet1_QT, na.action = na.exclude))
##	x = qnorm((rank(a,na.last="keep")-0.5)/sum(!is.na(a)))
##	diet1_QT_adj = cbind(diet1_QT_adj,x)
##	}
##colnames(diet1_QT_adj) = colnames(diet1_QT)[4:37]
##diet1_QT_adj = cbind(diet1_QT$ID, diet1_QT_adj)
##colnames(diet1_QT_adj)[1] <- "ID"

diet1_QT = merge(diet1_QT, center, by.x ="ID", by.y = "f.eid")

diet1_QT_adj = NULL
for (i in 4:44)
	{
	a = resid(lm(as.numeric(diet1_QT[,i])~as.numeric(DIET1_age_months.average) + sex, data = diet1_QT, na.action = na.exclude))
	x = qnorm((rank(a,na.last="keep")-0.5)/sum(!is.na(a)))
	diet1_QT_adj = cbind(diet1_QT_adj,x)
	}
colnames(diet1_QT_adj) = colnames(diet1_QT)[4:44]
diet1_QT_adj = cbind(diet1_QT$ID, diet1_QT_adj)
colnames(diet1_QT_adj)[1] <- "ID"


#average age in months
#diet2 
	#DIET2_age_months.0.0
	#DIET2_age_months.1.0
	#DIET2_age_months.2.0
	#DIET2_age_months.3.0
	#DIET2_age_months.4.0


diet2ever$DIET2_age_months_average = rowMeans(subset(diet2ever, select = c(DIET2_age_months.0.0,
							DIET2_age_months.1.0,
							DIET2_age_months.2.0,
							DIET2_age_months.3.0,
							DIET2_age_months.4.0
							)), na.rm = TRUE)


diet2ever_bin = diet2ever[,c(
"ID",
"DIET2_age_months_average",
"sex",
"vitamin_yesterday.20084_multi_ever",
"vitamin_yesterday.20084_fishoil_ever",
"vitamin_yesterday.20084_gluco_chondro_ever",
"vitamin_yesterday.20084_eveningprimrose_ever",
"vitamin_yesterday.20084_vitA_ever",
"vitamin_yesterday.20084_vitB6_ever",
"vitamin_yesterday.20084_vitB12_ever",
"vitamin_yesterday.20084_vitC_ever",
"vitamin_yesterday.20084_vitD_ever",
"vitamin_yesterday.20084_vitE_ever",
"vitamin_yesterday.20084_folicacid_ever",
"vitamin_yesterday.20084_chromium_ever",
"vitamin_yesterday.20084_magnesium_ever",
"vitamin_yesterday.20084_selenium_ever",
"vitamin_yesterday.20084_calcium_ever",
"vitamin_yesterday.20084_iron_ever",
"vitamin_yesterday.20084_any_ever",
"vitsupuser_yesterday.104670_any_ever",
"specialdiet_yesterday.20086_glutenfree_ever",
"specialdiet_yesterday.20086_lactosefree_ever",
"specialdiet_yesterday.20086_lowcal_ever",
"specialdiet_yesterday.20086_vegan_ever",
"specialdiet_yesterday.20086_veget_ever",
"specialdiet_yesterday.20086_veg_ever",
"spreadtype_breadcrackers_yesterday.20087_butter_ever",
"spreadtype_breadcrackers_yesterday.20087_oliveoil_ever",
"spreadtype_breadcrackers_yesterday.20087_margarine_ever",
"spreadtype_breadcrackers_yesterday.20087_dairy_ever",
"spreadtype_breadcrackers_yesterday.20087_soya_ever",
"spreadtype_breadcrackers_yesterday.20087_lowchol_ever",
"spreadtype_breadcrackers_yesterday.20087_lowfat_ever",
"spreadtype_breadcrackers_yesterday.20087_any_ever",
"anybutteronbread_yesterday.101300_any_ever",
"spreadsaucetype_yesterday.20088_jamhoney_ever",
"spreadsaucetype_yesterday.20088_cream_ever",
"spreadsaucetype_yesterday.20088_peanutbutter_ever",
"spreadsaucetype_yesterday.20088_yeast_ever",
"spreadsaucetype_yesterday.20088_hummus_ever",
"spreadsaucetype_yesterday.20088_guacamole_ever",
"spreadsaucetype_yesterday.20088_chutney_ever",
"spreadsaucetype_yesterday.20088_ketchup_ever",
"spreadsaucetype_yesterday.20088_brown_ever",
"spreadsaucetype_yesterday.20088_mayo_ever",
"spreadsaucetype_yesterday.20088_dressing_ever",
"spreadsaucetype_yesterday.20088_oil_ever",
"spreadsaucetype_yesterday.20088_pesto_ever",
"spreadsaucetype_yesterday.20088_tomato_ever",
"spreadsaucetype_yesterday.20088_cheese_ever",
"spreadsaucetype_yesterday.20088_whitecream_ever",
"spreadsaucetype_yesterday.20088_gravy_ever",
"spreadsaucetype_yesterday.20088_anysauce_ever",
"anyspreadsauce_yesterday.103310_any_ever",
"mealtype_yesterday.20089_takeaway_ever",
"mealtype_yesterday.20089_restaurant_ever",
"mealtype_yesterday.20089_boughtsandwich_ever",
"mealtype_yesterday.20089_readymeals_ever",
"mealtype_yesterday.20089_homecooked_ever",
"mealtype_yesterday.20089_nothomecooked_ever",
"fatoiltype_yesterday.20090_oil_ever",
"fatoiltype_yesterday.20090_butter_ever",
"fatoiltype_yesterday.20090_olivespread_ever",
"fatoiltype_yesterday.20090_polymarg_ever",
"fatoiltype_yesterday.20090_dairyspread_ever",
"fatoiltype_yesterday.20090_soyamarg_ever",
"fatoiltype_yesterday.20090_softmarg_ever",
"fatoiltype_yesterday.20090_anymarg_ever",
"fatoiltype_yesterday.20090_lard_ever",
"fatoiltype_yesterday.20090_anyfat_ever",
"fatoiltype_yesterday.20090_anylowfat_ever",
"fatoiltype_yesterday.20090_anylowchol_ever",
"breadtype_yesterday.20091_white_ever",
"breadtype_yesterday.20091_mixed_ever",
"breadtype_yesterday.20091_wholemeal_ever",
"breadtype_yesterday.20091_seeded_ever",
"breadtype_yesterday.20091_any_ever",
"baguettetype_yesterday.20092_white_ever",
"baguettetype_yesterday.20092_mixed_ever",
"baguettetype_yesterday.20092_wholemeal_ever",
"baguettetype_yesterday.20092_seeded_ever",
"baguettetype_yesterday.20092_any_ever",
"baptype_yesterday.20093_white_ever",
"baptype_yesterday.20093_mixed_ever",
"baptype_yesterday.20093_wholemeal_ever",
"baptype_yesterday.20093_seeded_ever",
"baptype_yesterday.20093_any_ever",
"rolltype_yesterday.20094_white_ever",
"rolltype_yesterday.20094_mixed_ever",
"rolltype_yesterday.20094_wholemeal_ever",
"rolltype_yesterday.20094_seeded_ever",
"rolltype_yesterday.20094_any_ever",
"anybreadtype_yesterday_white_ever",
"anybreadtype_yesterday_mixed_ever",
"anybreadtype_yesterday_wholemeal_ever",
"anybreadtype_yesterday_seeded_ever",
"anybreadtype_yesterday_any_ever",
"anybread_yesterday.100940_any_ever",
"liquidforporridge_yesterday.20105_milk_ever",
"liquidforporridge_yesterday.20105_water_ever",
"yogurttype_yesterday.20106_lowfat_ever",
"yogurttype_yesterday.20106_fullfat_ever",
"cannedsouptype_yesterday.20108_fish_ever",
"cannedsouptype_yesterday.20108_meat_ever",
"cannedsouptype_yesterday.20108_pasta_ever",
"cannedsouptype_yesterday.20108_pulses_ever",
"cannedsouptype_yesterday.20108_vegetables_ever",
"cannedsouptype_yesterday.20108_any_ever",
"homemadesouptype_yesterday.20109_fish_ever",
"homemadesouptype_yesterday.20109_meat_ever",
"homemadesouptype_yesterday.20109_pasta_ever",
"homemadesouptype_yesterday.20109_pulses_ever",
"homemadesouptype_yesterday.20109_vegetables_ever",
"homemadesouptype_yesterday.20109_any_ever",
"anysouptype_yesterday_fish_ever",
"anysouptype_yesterday_meat_ever",
"anysouptype_yesterday_pasta_ever",
"anysouptype_yesterday_pulses_ever",
"anysouptype_yesterday_vegetables_ever",
"anysouptype_yesterday_any_ever",
"starchyfoodservings_yesterday.102700_any_ever",
"portionsize_yesterday.100010_smaller_ever",
"portionsize_yesterday.100010_average_ever",
"portionsize_yesterday.100010_larger_ever",
"anycoffee_yesterday.100240_any_ever",
"milkinanycoffee_yesterday_any_ever",
"milkinanycoffeetea_yesterday_any_ever",
"anydecafcoffee_yesterday.100360_any_ever",
"anytea_yesterday.100390_any_ever",
"anydecaftea_yesterday.100470_any_ever",
"anydecafcoffeetea_yesterday_any_ever",
"anyalcoholglasses_yesterday.100580_any_ever",
"anybreakfastcereal_yesterday.100760_any_ever",
"driedfruitincereal_yesterday.100880_any_ever",
"milkincereal_yesterday.100890_any_ever",
"milktype_yesterday.100920_semiskimmed_ever",
"milktype_yesterday.100920_skimmedever",
"milktype_yesterday.100920_wholemilk_ever",
"milktype_yesterday.100920_soya_ever",
"milktype_yesterday.100920_goatsheep_ever",
"milktype_yesterday.100920_riceoatveg_ever",
"milktype_yesterday.100920_powdered_ever",
"milktype_yesterday.100920_lowchol_ever",
"milktype_yesterday.100920_any_ever",
"milktype_yesterday.100920_real_ever",
"milktype_yesterday.100920_alt_ever",
"anyyogurtoricecream_yesterday.102080_any_ever",
"anydessert_yesterday.102130_any_ever",
"anysweets_yesterday.102250_any_ever",
"anysavorysnacks_yesterday.102400_any_ever",
"anycheese_yesterday.102800_any_ever",
"anyeggs_yesterday.102930_any_ever",
"anymeat_yesterday.103000_any_ever",
"fatremovedfrommeat_yesterday.103120_any_ever",
"skinremovedfrompoultry_yesterday.103130_any_ever",
"anyfish_yesterday.103140_any_ever",
"vegaltservings_yesterday.103250_any_ever",
"anyveg_yesterday.103990_any_ever",
"butteronpotatoes_yesterday.104040_any_ever",
"anyfruit_yesterday.104400_any_ever",
"addsalttofood_yesterday.104660_any_ever"
)]

diet2ever_QT = diet2ever[,c(
"ID",
"DIET2_age_months_average",
"sex",
"porridgebowls_yesterday.100770_average",
"mueslibowls_yesterday.100800_average",
"oatcrunchbowls_yesterday.100810_average",
"sweetcerealbowls_yesterday.100820_average",
"plaincerealbowls_yesterday.100830_average",
"brancerealbowls_yesterday.100840_average",
"wholewheatcerealbowls_yesterday.100850_average",
"anycerealbowls_yesterday_totalaverage",
"whitepastaservings_yesterday.102710_average",
"wholemealpastaservings_yesterday.102720_average",
"pasta_yesterday_totalaverage",
"whitericeservings_yesterday.102730_average",
"brownriceservings_yesterday.102740_average",
"sushiservings_yesterday.102750_average",
"snackpotservings_yesterday.102760_average",
"couscousservings_yesterday.102770_average",
"othergrainservings_yesterday.102780_average",
"tspsugarincoffee_yesterday.100370_average",
"tspartsweetincoffee_yesterday.100380_average",
"tspsugarintea_yesterday.100490_average",
"tspartsweetintea_yesterday.100500_average",
"tspsugarincereal_yesterday.100900_average",
"tspartsweetincereal_yesterday.100910_average",
"tspsugar_yesterday_totalaverage",
"tspartsweet_yesterday_totalaverage",
"yogurtservings_yesterday.102090_average",
"icecreamservings_yesterday.102120_average",
"puddingservings_yesterday.102140_average",
"milkbaseddessertservings_yesterday.102150_average",
"soyadessertservings_yesterday.102170_average",
"fruitcakeservings_yesterday.102180_average",
"cakeservings_yesterday.102190_average",
"doughnutservings_yesterday.102200_average",
"spongepuddingservings_yesterday.102210_average",
"cheesecakeservings_yesterday.102220_average",
"otherdessertservings_yesterday.102230_average",
"saltpeanutservings_yesterday.102410_average",
"unsaltpeanutservings_yesterday.102420_average",
"saltnutservings_yesterday.102430_average",
"unsaltnutservings_yesterday.102440_average",
"nuts_yesterday_totalaverage",
"seedservings_yesterday.102450_average",
"crispservings_yesterday.102460_average",
"savorybiscuitservings_yesterday.102470_average",
"cheesybiscuitservings_yesterday.102480_average",
"oliveservings_yesterday.102490_average",
"othersavorysnackservings_yesterday.102500_average",
"lowfathardcheeseservings_yesterday.102810_average",
"hardcheeseservings_yesterday.102820_average",
"softcheeseservings_yesterday.102830_average",
"bluecheeseservings_yesterday.102840_average",
"lowfatcheesespreadservings_yesterday.102850_average",
"cheesespreadservings_yesterday.102860_average",
"cottagecheeseservings_yesterday.102870_average",
"fetacheeseservings_yesterday.102880_average",
"mozzcheeseservings_yesterday.102890_average",
"goatscheeseservings_yesterday.102900_average",
"othercheeseservings_yesterday.102910_average",
"cheese_yesterday_totalaverage",
"wholeeggservings_yesterday.102940_average",
"omeletteservings_yesterday.102950_average",
"eggsinsandwichservings_yesterday.102960_average",
"scotcheggservings_yesterday.102970_average",
"othereggservings_yesterday.102980_average",
"eggs_yesterday_totalaverage",
"doublecrustpieslices_yesterday.101970_average",
"singlecrustpieslices_yesterday.101980_average",
"crumbleslices_yesterday.101990_average",
"dessert_yesterday_totalaverage",
"pizzaslices_yesterday.102000_average",
"pancakenumber_yesterday.102010_average",
"scotchpancakenumber_yesterday.102020_average",
"yorkshirepuddingservings_yesterday.102030_average",
"indiansnacksnumber_yesterday.102040_average",
"croissantnumber_yesterday.102050_average",
"danishpastrynumber_yesterday.102060_average",
"sconenumber_yesterday.102070_average",
"pastry_yesterday_totalaverage",
"instantsoupservings_yesterday.102530_average",
"cannedsoupservings_yesterday.102540_average",
"homemadesoupservings_yesterday.102620_average",
"soup_yesterday_totalaverage",
"cannedtunaservings_yesterday.103150_average",
"oilyfishservings_yesterday.103160_average",
"breadedfishservings_yesterday.103170_average",
"batteredfishservings_yesterday.103180_average",
"whitefishservings_yesterday.103190_average",
"prawnsservings_yesterday.103200_average",
"lobstercrabservings_yesterday.103210_average",
"shellfishservings_yesterday.103220_average",
"otherfishservings_yesterday.103230_average",
"fish_yesterday_totalaverage",
"seafood_yesterday_totalaverage",
"shellfish_yesterday_totalaverage",
"vegburgerservings_yesterday.103260_average",
"tofuservings_yesterday.103270_average",
"quornservings_yesterday.103280_average",
"othervegaltservings_yesterday.103290_average",
"vegalt_yesterday_totalaverage",
"stewedfruitservings_yesterday.104410_average",
"pruneservings_yesterday.104420_average",
"driedfruitservings_yesterday.104430_average",
"mixedfruitservings_yesterday.104440_average",
"appleservings_yesterday.104450_average",
"bananaservings_yesterday.104460_average",
"berryservings_yesterday.104470_average",
"cherryservings_yesterday.104480_average",
"grapefruitservings_yesterday.104490_average",
"grapeservings_yesterday.104500_average",
"mangoservings_yesterday.104510_average",
"melonservings_yesterday.104520_average",
"orangeservings_yesterday.104530_average",
"satsumaservings_yesterday.104540_average",
"peachservings_yesterday.104550_average",
"pearservings_yesterday.104560_average",
"pineappleservings_yesterday.104570_average",
"plumservings_yesterday.104580_average",
"otherfruitservings_yesterday.104590_average",
"fruit_yesterday_totalaverage",
"chocbarservings_yesterday.102260_average",
"whitechocservings_yesterday.102270_average",
"milkchocservings_yesterday.102280_average",
"darkchocservings_yesterday.102290_average",
"choccovraisinsservings_yesterday.102300_average",
"chocsweetsservings_yesterday.102310_average",
"dietsweetsservings_yesterday.102320_average",
"sweetsservings_yesterday.102330_average",
"choccovbiscuitsservings_yesterday.102340_average",
"chocbiscuitsservings_yesterday.102350_average",
"choc_yesterday_totalaverage",
"sweetbiscuitsservings_yesterday.102360_average",
"cerealbarsservings_yesterday.102370_average",
"othersweetssservings_yesterday.102380_average",
"sweets_yesterday_totalaverage",
"waterglasses_yesterday.100150_average",
"dietdrinksglasses_yesterday.100160_average",
"carbonateddrinksglasses_yesterday.100170_average",
"fruitdrinksglasses_yesterday.100180_average",
"orangejuiceglasses_yesterday.100190_average",
"grapefruitjuiceglasses_yesterday.100200_average",
"purefruitveggiejuiceglasses_yesterday.100210_average",
"fruitsmoothieglasses_yesterday.100220_average",
"dairysmoothieglasses_yesterday.100230_average",
"instantcoffeeglasses_yesterday.100250_average",
"filteredcoffeeglasses_yesterday.100270_average",
"cappuccinoglasses_yesterday.100290_average",
"latteglasses_yesterday.100300_average",
"espressoglasses_yesterday.100310_average",
"othercoffeeglasses_yesterday.100330_average",
"milkglasses_yesterday.100520_average",
"flavoredmilkglasses_yesterday.100530_average",
"lowcalhotchocglasses_yesterday.100540_average",
"hotchocglasses_yesterday.100550_average",
"otherdrinkglasses_yesterday.100560_average",
"redwineglasses_yesterday.100590_average",
"rosewineglasses_yesterday.100630_average",
"whitewineglasses_yesterday.100670_average",
"beerciderglasses_yesterday.100710_average",
"fortwineglasses_yesterday.100720_average",
"spiritsglasses_yesterday.100730_average",
"otheralcoholglasses_yesterday.100740_average",
"fruitdrinks_yesterday_totalaverage",
"coffee_yesterday_totalaverage",
"milk_yesterday_totalaverage",
"alcohol_yesterday_totalaverage",
"drinks_yesterday_totalaverage",
"breadslices_yesterday.100950_average",
"baguetteitems_yesterday.101020_average",
"bapitems_yesterday.101090_average",
"rollitems_yesterday.101160_average",
"naanitems_yesterday.101230_average",
"garlicbreaditems_yesterday.101240_average",
"cripsbreaditems_yesterday.101250_average",
"oatcakesitems_yesterday.101260_average",
"otherbreaditems_yesterday.101270_average",
"breadsliceswithbutter_yesterday.101310_average",
"baguetteitemswithbutter_yesterday.101350_average",
"bapitemswithbutter_yesterday.101390_average",
"rollitemswithbutter_yesterday.101430_average",
"crackersitemswithbutter_yesterday.101470_average",
"oatcakesitemswithbutter_yesterday.101510_average",
"otherbreaditemswithbutter_yesterday.101550_average",
"bread_yesterday_totalaverage",
"breadbutter_yesterday_totalaverage",
"standardteacups_yesterday.100400_average",
"rooibosteacups_yesterday.100410_average",
"greenteacups_yesterday.100420_average",
"herbalteacups_yesterday.100430_average",
"otherteacups_yesterday.100440_average",
"tea_yesterday_totalaverage",
"sausageservings_yesterday.103010_average",
"beefservings_yesterday.103020_average",
"porkservings_yesterday.103030_average",
"lambservings_yesterday.103040_average",
"friedpoultryservings_yesterday.103050_average",
"poultryservings_yesterday.103060_average",
"baconservings_yesterday.103070_average",
"hamservings_yesterday.103080_average",
"liverservings_yesterday.103090_average",
"othermeatservings_yesterday.103100_average",
"meat_yesterday_totalaverage",
"bakedbeanservings_yesterday.104000_average",
"beanservings_yesterday.104010_average",
"friedpotatoesservings_yesterday.104020_average",
"bakedpotatoesservings_yesterday.104030_average",
"mashedpotatoservings_yesterday.104050_average",
"mixedvegservings_yesterday.104060_average",
"vegpiecesservings_yesterday.104070_average",
"coleslawservings_yesterday.104080_average",
"sidesaladservings_yesterday.104090_average",
"avocadoservings_yesterday.104100_average",
"broadbeanservings_yesterday.104110_average",
"greenbeanservings_yesterday.104120_average",
"beetrootservings_yesterday.104130_average",
"broccoliservings_yesterday.104140_average",
"butternutservings_yesterday.104150_average",
"cabbageservings_yesterday.104160_average",
"carrotservings_yesterday.104170_average",
"cauliflowerservings_yesterday.104180_average",
"celeryservings_yesterday.104190_average",
"courgetteservings_yesterday.104200_average",
"cucumberservings_yesterday.104210_average",
"garlicservings_yesterday.104220_average",
"leekservings_yesterday.104230_average",
"lettuceservings_yesterday.104240_average",
"mushroomservings_yesterday.104250_average",
"onionservings_yesterday.104260_average",
"parsnipservings_yesterday.104270_average",
"peaservings_yesterday.104280_average",
"sweetpepperservings_yesterday.104290_average",
"spinachservings_yesterday.104300_average",
"sproutsservings_yesterday.104310_average",
"sweetcornservings_yesterday.104320_average",
"sweetpotatoservings_yesterday.104330_average",
"freshtomatoservings_yesterday.104340_average",
"cannedtomatoservings_yesterday.104350_average",
"turnipservings_yesterday.104360_average",
"watercressservings_yesterday.104370_average",
"othervegservings_yesterday.104380_average",
"bean_yesterday_totalaverage",
"potato_yesterday_totalaverage",
"veg_yesterday_totalaverage",
"foodweight_yesterday_total.100001_average",
"energy_yesterday_total.100002_average",
"protein_yesterday_total.100003_average",
"fat_yesterday_total.100004_average",
"carb_yesterday_total.100005_average",
"satfat_yesterday_total.100006_average",
"polyunsatfat_yesterday_total.100007_average",
"sugar_yesterday_total.100008_average",
"englystdietfibre_yesterday_total.100009_average",
"iron_yesterday_total.100011_average",
"vitB6_yesterday_total.100012_average",
"vitB12_yesterday_total.100013_average",
"folate_yesterday_total.100014_average",
"vitC_yesterday_total.100015_average",
"potassium_yesterday_total.100016_average",
"magnesium_yesterday_total.100017_average",
"retinol_yesterday_total.100018_average",
"carotene_yesterday_total.100019_average",
"vitD_yesterday_total.100021_average",
"alcohol_yesterday_total.100022_average",
"starch_yesterday_total.100023_average",
"calcium_yesterday_total.100024_average",
"vitE_yesterday_total.100025_average"
)]

#diet2ever_bin_merged = merge(diet2ever_bin,IDS, by = "ID", all.y = T)

#diet2ever_bin_adj = NULL
#for (i in 4:163)
#	{
#	a = resid(glm(as.numeric(diet2ever_bin_merged[,i])~as.numeric(DIET2_age_months_average) + sex + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = diet2ever_bin_merged, na.action = na.exclude, family = binomial))
#	diet2ever_bin_adj = cbind(diet2ever_bin_adj,a)
#	}
#colnames(diet2ever_bin_adj) = colnames(diet2ever_bin)[4:163]
#diet2ever_bin_adj = cbind(diet2ever_bin_merged$ID, diet2ever_bin_adj)
#colnames(diet2ever_bin_adj)[1] <- "ID"

diet2ever_bin = merge(diet2ever_bin, center, by.x ="ID", by.y = "f.eid")

diet2ever_bin_adj = NULL
for (i in 4:162)
	{
	a = resid(glm(as.numeric(diet2ever_bin[,i])~as.numeric(DIET2_age_months_average) + sex, data = diet2ever_bin, na.action = na.exclude, family = binomial))
	diet2ever_bin_adj = cbind(diet2ever_bin_adj,a)
	}
colnames(diet2ever_bin_adj) = colnames(diet2ever_bin)[4:162]
diet2ever_bin_adj = cbind(diet2ever_bin$ID, diet2ever_bin_adj)
colnames(diet2ever_bin_adj)[1] <- "ID"


#diet2ever_QT_merged = merge(diet2ever_QT,IDS, by = "ID", all.y = T)

#diet2ever_QT_adj = NULL
#for (i in 4:268)
#	{
#	a = resid(lm(as.numeric(diet2ever_QT_merged[,i])~as.numeric(DIET2_age_months_average) + sex + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = diet2ever_QT_merged, na.action = na.exclude))
#	x = qnorm((rank(a,na.last="keep")-0.5)/sum(!is.na(a)))
#	diet2ever_QT_adj = cbind(diet2ever_QT_adj,x)
#	}
#colnames(diet2ever_QT_adj) = colnames(diet2ever_QT)[4:268]
#diet2ever_QT_adj = cbind(diet2ever_QT_merged$ID, diet2ever_QT_adj)
#colnames(diet2ever_QT_adj)[1] <- "ID"

diet2ever_QT = merge(diet2ever_QT, center, by.x ="ID", by.y = "f.eid")

colnames(diet2ever_QT)
diet2ever_QT_adj = NULL
for (i in 4:268)
	{
	a = resid(lm(as.numeric(diet2ever_QT[,i])~as.numeric(DIET2_age_months_average) + sex, data = diet2ever_QT, na.action = na.exclude))
	x = qnorm((rank(a,na.last="keep")-0.5)/sum(!is.na(a)))
	diet2ever_QT_adj = cbind(diet2ever_QT_adj,x)
	}
colnames(diet2ever_QT_adj) = colnames(diet2ever_QT)[4:268]
diet2ever_QT_adj = cbind(diet2ever_QT$ID, diet2ever_QT_adj)
colnames(diet2ever_QT_adj)[1] <- "ID"


### diet 2 proportions
diet2prop$DIET2_age_months_average = rowMeans(subset(diet2prop, select = c(DIET2_age_months.0.0,
							DIET2_age_months.1.0,
							DIET2_age_months.2.0,
							DIET2_age_months.3.0,
							DIET2_age_months.4.0
							)), na.rm = TRUE)


diet2prop_QT = diet2prop[,c(1,166,167,8:165)]
colnames(diet2prop_QT)

diet2prop_QT = merge(diet2prop_QT, center, by.x ="ID", by.y = "f.eid")

diet2prop_QT_adj = NULL
for (i in 4:161)
	{
	a = resid(lm(as.numeric(diet2prop_QT[,i])~as.numeric(DIET2_age_months_average) + sex, data = diet2prop_QT, na.action = na.exclude))
	x = qnorm((rank(a,na.last="keep")-0.5)/sum(!is.na(a)))
	diet2prop_QT_adj = cbind(diet2prop_QT_adj,x)
	}
colnames(diet2prop_QT_adj) = colnames(diet2prop_QT)[4:161]
diet2prop_QT_adj = cbind(diet2prop_QT$ID, diet2prop_QT_adj)
colnames(diet2prop_QT_adj)[1] <- "ID"


### 

tmp = merge(diet1_bin_adj,diet1_QT_adj, by = "ID", all =T) 
tmp1 = merge(tmp, diet2ever_bin_adj, by = "ID", all =T)
tmp2 = merge(tmp1, diet2ever_QT_adj, by = "ID", all =T)
tmp3 = merge(tmp2, diet2prop_QT_adj, by = "ID", all =T)

final = tmp3[,c(1,1:ncol(tmp3))]
colnames(final)[1] <- "FID"
colnames(final)[2] <- "IID"


######### 
#write.table(final, "/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes", 
#	quote = FALSE, row.names = FALSE, sep = " ", eol ="\n", na = "NA", col.names = TRUE)

#write.table(final, "/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03262018_updatedEB", 
#	quote = FALSE, row.names = FALSE, sep = " ", eol ="\n", na = "NA", col.names = TRUE)

#write.table(final, "/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03262018_updatedEB_updated06222018", 
#	quote = FALSE, row.names = FALSE, sep = " ", eol ="\n", na = "NA", col.names = TRUE)

write.table(final, "/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03262018_updatedEB_updated06222018_07062018", 
	quote = FALSE, row.names = FALSE, sep = " ", eol ="\n", na = "NA", col.names = TRUE)


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
################### How to FFQ Diet Phenotypes - Hirschhorn Application
################### STEP3: PCA
########## last updated: November 2018
########## JBC
######################################################################################################################################

##### PCA on diet phenotypes
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
##### some notes on PCA
##### There are two general methods to perform PCA in R :

	##Spectral decomposition which examines the covariances / correlations between variables
	##Singular value decomposition which examines the covariances / correlations between individuals
	##The function princomp() uses the spectral decomposition approach. The functions prcomp() and PCA()[FactoMineR] use the singular value decomposition (SVD).

##According to the R help, SVD has slightly better numerical accuracy. Therefore, the function prcomp() is preferred compared to princomp().

############################################################################################################################################################

## all diet phenotypes, age and sex adjusted, inverse normal tranformed
## thus - do not need to center and scale data for PCA
#data = read.table("/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR433K_phenotypes", header = T)
#data = read.table("/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes", header = T)

#tmp = read.table("/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03262018_updatedEB_updated06222018", header = T)
#tmp2 = read.table("/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03292018_macronutrient_06222018", header = T)
library(data.table)
tmp = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03262018_updatedEB_updated06222018_07062018", data.table=FALSE, verbose = TRUE)
tmp2 = fread("/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_phenotypes_03292018_macronutrient_06222018", data.table=FALSE, verbose = TRUE)
data = merge(tmp, tmp2, by = c("FID","IID") , all.x = T)

################################################ ################# #################   PCA

################# ################# ################# FFQ
###########################################
########################################## 
ffq = data[,c(1:99)]

### remove the depracated alcohol columns
ffq$champagnewhitewine_glassesperweek.1578.average_QT = NULL
ffq$champagnewhitewine_glassespermonth.4418.average_QT= NULL
ffq$redwine_glassesperweek.1568.average_QT= NULL
ffq$redwine_glassespermonth.4407.average_QT= NULL
ffq$beercider_pintsperweek.1588.average_QT= NULL
ffq$beercider_pintspermonth.4429.average_QT= NULL
ffq$spirits_measuresperweek.1598.average_QT= NULL
ffq$spirits_measurespermonth.4440.average_QT= NULL
ffq$fortwine_glassesperweek.1608.average_QT= NULL
ffq$fortwine_glassespermonth.4451.average_QT= NULL
ffq$otheralcohol_glassesperweek.5364.average_QT= NULL
ffq$otheralcohol_glassespermonth.4462.average_QT= NULL

################# ################# #################  remove missing individuals
## remove all rows that have all columns (not including FID and ID) ALL missing.
ffq = ffq[rowSums(is.na(ffq[,3:87])) != ncol(ffq[,3:87]),]

################# ################# Approach 1:  fill in the median
ffq_median = ffq
for(i in 3:ncol(ffq_median)){
ffq_median[is.na(ffq_median[,i]), i] <- median(ffq_median[,i], na.rm = TRUE)
}

ffq_median$FID = NULL
ffq_median$IID = NULL

#check missingness
colnames(ffq_median)[colSums(is.na(ffq_median)) > 0]

ffq_median.pca = prcomp(ffq_median, scale = FALSE)

colnames(ffq_median.pca$x) <- paste0("ffq_median_", colnames(ffq_median.pca$x))
save = cbind(ffq[,1:2], ffq_median.pca$x)

write.table(save, "/broad/jhlab_ukbiobank/users/jcole/Diet/BOLT_UKB_diet_genoQCEUR450K_ffq_median_PC_phenotypes_11212018update", 
	quote = FALSE, row.names = FALSE, sep = " ", eol ="\n", na = "NA", col.names = TRUE)
save(ffq_median.pca,file="/broad/jhlab_ukbiobank/users/jcole/Diet/results/ffq_QT_PCA_paper/ffq_median.pca_11212018update.Rdata")

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


