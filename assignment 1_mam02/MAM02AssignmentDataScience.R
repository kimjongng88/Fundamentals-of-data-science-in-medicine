#############################################
# MAM02 Assignment Data Science
#############################################

#############################################
# Import the data
#############################################

# Set your working directory to change to the folder where you downloaded the data file
# Thus replace the pathname H:/Downloads/ with the folder containing the data file
# Note that in Windows you have to use '/' instead of the normal '\' in the folder pathname
setwd("H:/Downloads/")

# Import the data file
data_diabetic <- read.csv(file="diabetic_data_initial.csv", header=TRUE, sep=",")

# Let's quickly check the first few rows of the data
head(data_diabetic)

# Let's check the names of the variables (i.e., column names)
names(data_diabetic)

# How many rows are there?
dim(data_diabetic)[1]
# your answer: 101766

# Are these all unique encounters?
length(unique(data_diabetic$encounter_id))
# your answer: 101766

# And are they all unique patients?
length(unique(data_diabetic$patient_nbr))
# your answer: 71518

#############################################
# Select data
#############################################

# We have to remove some data according to the paper (read section 2.3 carefully)
# "... used only one encounter per patient ..."
# I found the way to do this on stackoverflow.com (remember this site because you can find a lot of answers on R)
# https://stackoverflow.com/questions/19944334/extract-rows-for-the-first-occurrence-of-a-variable-in-a-data-frame#19944458
data_diabetic_first_encounters <- data_diabetic[match(unique(data_diabetic$patient_nbr), data_diabetic$patient_nbr),]
# "... removed all encounters that resulted in either discharge to a hospice or patient death ..."
# We use the dplyr library to filter the data
library("dplyr")
# The discharge_disposition_id column contains info on the discharge
# In id_mapping.csv we find discharge IDs 11 (Expired), 13 (Hospice / home), 14 (Hospice / medical facility)
names_removed_discharge_disposition_id <- c(11,13,14)
# Thus we remove these rows from the data
data_diabetic_selected = filter(data_diabetic_first_encounters, !data_diabetic_first_encounters$discharge_disposition_id %in% names_removed_discharge_disposition_id)

# Do we now have 69,984 encounters as mentioned in the paper?
dim(data_diabetic_selected)[1]
# your answer: No, 4 are missing

# And let's quickly check if we now only have unique patients
length(unique(data_diabetic_selected$patient_nbr))
# your answer: Yes, all 69980 patients are unique.

#############################################
# Calculate descriptive statistics
#############################################

# Assuming that we have selected the data properly, we will show you the calculation of a single row from Table 3
# We will calculate the numbers for: Gender = Female

# Female: Number of encounters
Female_Number_of_encounters <- table(data_diabetic_selected$gender)[1]

# Female: % of the population
Population_size <- dim(data_diabetic_selected)[1]
Female_Percentage_of_population <- round(Female_Number_of_encounters / Population_size * 100, 1)

# Female Readmitted: Number of encounters
Female_Readmitted_Number_of_encounters <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$gender)[1]

# Female Readmitted: % in group
Percentage_Female_ad <- round(Female_Readmitted_Number_of_encounters / Female_Number_of_encounters * 100, 1)

#############################################
# From here on, you're on your own!
# Complete the source file such that
#############################################

# We will calculate the numbers for: Gender = Male
Male_Number_of_encounters <- table(data_diabetic_selected$gender)[2]
# Number of males = 32743

Male_Percentage_of_population <- round(Male_Number_of_encounters / Population_size * 100, 2)
# Percentage = 46.8%

Male_Readmitted_Number_of_encounters <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$gender)[2]
# Number of reamitted = 2916. Not the same number. Readmitteddiffers from the paper.

Percentage_male_ad <- round(Male_Readmitted_Number_of_encounters / Male_Number_of_encounters * 100, 2)
# % = 8.91, not the same number.

# calculate numbers for: HbA1c
Performed_test <- sum(data_diabetic_selected$A1Cresult == ">7") + sum(data_diabetic_selected$A1Cresult == ">8") + sum(data_diabetic_selected$A1Cresult == "Norm")
Non_performed <- Population_size - Performed_test
No_result_readmitted <- sum(data_diabetic_selected[data_diabetic_selected$A1Cresult == "None",]$readmitted == "<30")
Percentage_No_result <- round(Non_performed / Population_size * 100, 2)
Percentage_No_result_read <- round(No_result_readmitted / Non_performed * 100, 2)

############ Normal results
Normal_results <- sum(data_diabetic_selected$A1Cresult == "Norm") + sum(data_diabetic_selected$A1Cresult == ">7")
# readmitted
Normal_readmitted <- sum(data_diabetic_selected[data_diabetic_selected$A1Cresult == "Norm",]$readmitted == "<30")
Percentage_Normal_result <- round(Normal_results / Population_size * 100, 2)
Percentage_Normal_result_read <- round(Normal_readmitted / Normal_results * 100, 2)



##### High
# High and changed
High_change <- sum(data_diabetic_selected[data_diabetic_selected$A1Cresult == ">8",]$change == "Ch")
High_ch_read <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$A1Cresult[data_diabetic_selected$change == "Ch"])[2]


Percentage_High <- round(High_change / Population_size * 100, 2)
Percentage_High_ch_AD <- round(High_ch_read / High_change * 100, 2)



# High and not changed
High_not_change <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$A1Cresult == ">8")
High_n_read <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$A1Cresult[data_diabetic_selected$change == "No"])[2]
Percentage_High_not <- round(High_not_change / Population_size * 100, 2)
Percentage_High_n_AD <- round(High_n_read / High_not_change * 100, 2)

# calculate numbers for: Discharge disposition
Discharge_Home <- sum(data_diabetic_selected$discharge_disposition_id == "1")
Discharge_Other <- sum(data_diabetic_selected$discharge_disposition_id != "1")
Percentage_Home <- round(Discharge_Home / Population_size * 100, 2)
Percentage_Other <- round(Discharge_Other / Population_size * 100, 2)

Home_readmitted <- sum(data_diabetic_selected[data_diabetic_selected$discharge_disposition_id == "1",]$readmitted == "<30")
Other_readmitted <- sum(data_diabetic_selected[data_diabetic_selected$discharge_disposition_id != "1",]$readmitted == "<30")
Percentage_Home_read <- round(Home_readmitted / Discharge_Home * 100, 2)
Percentage_Other_read <- round(Other_readmitted / Discharge_Other * 100, 2)

###### Admission source
Admission_ER <- sum(data_diabetic_selected$admission_source_id == "7")
Admission_Phy <- sum(data_diabetic_selected$admission_source_id == "1")
Admission_other <- sum(data_diabetic_selected$admission_source_id != "7") - sum(data_diabetic_selected$admission_source_id == "1")
Percentage_ER <- round(Admission_ER / Population_size * 100, 2)
Percentage_P <- round(Admission_Phy / Population_size * 100, 2)
Percentage_O <- round(Admission_other / Population_size * 100, 2)

Admission_ER_AD <- sum(data_diabetic_selected[data_diabetic_selected$admission_source_id == "7",]$readmitted == "<30")
Admission_Phy_AD <- sum(data_diabetic_selected[data_diabetic_selected$admission_source_id == "1",]$readmitted == "<30")
Admission_other_AD <- sum(data_diabetic_selected[data_diabetic_selected$admission_source_id != "7",]$readmitted == "<30") - sum(data_diabetic_selected[data_diabetic_selected$admission_source_id == "1",]$readmitted == "<30")
Percentage_AD_ER <- round(Admission_ER_AD / Admission_ER * 100, 2)
Percentage_AD_P <- round(Admission_Phy_AD / Admission_Phy * 100, 2)
Percentage_AD_O <- round(Admission_other_AD / Admission_other * 100, 2)

#### Specialty
IM <- sum(data_diabetic_selected$medical_specialty == "InternalMedicine")
Cardiology <- sum(data_diabetic_selected$medical_specialty == "Cardiology")
Surgery <- sum(data_diabetic_selected$medical_specialty == "Surgery-General") + sum(data_diabetic_selected$medical_specialty == "Surgery-Cardiovascular/Thoracic")
F_GM <- sum(data_diabetic_selected$medical_specialty == "Family/GeneralPractice")
Unknown <- sum(data_diabetic_selected$medical_specialty == "?")
Other <- Population_size - IM - Cardiology - Surgery - F_GM - Unknown

Percentage_IM <- round(IM/Population_size * 100, 2)
Percentage_C <- round(Cardiology/Population_size * 100, 2)
Percentage_S <- round(Surgery/Population_size * 100, 2)
Percentage_F <- round(F_GM/Population_size * 100, 2)
Percentage_U <- round(Unknown/Population_size * 100, 2)
Percentage_O <- round(Other/Population_size * 100, 2)

#### Specialty, readmission
IM_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$medical_specialty == "InternalMedicine")
Cardiology_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$medical_specialty == "Cardiology")
Surgery_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$medical_specialty == "Surgery-General") + sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$medical_specialty == "Surgery-Cardiovascular/Thoracic")
F_GM_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$medical_specialty == "Family/GeneralPractice")
Unknown_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$medical_specialty == "?")
Other_AD <- sum(data_diabetic_selected$readmitted == "<30") - IM_AD - Cardiology_AD - Surgery_AD - F_GM_AD - Unknown_AD

Percentage_IM_AD <- round(IM_AD/IM * 100, 2)
Percentage_C_AD <- round(Cardiology_AD/Cardiology * 100, 2)
Percentage_S_AD <- round(Surgery_AD/Surgery * 100, 2)
Percentage_F_AD <- round(F_GM_AD/F_GM * 100, 2)
Percentage_U_AD <- round(Unknown_AD/Unknown * 100, 2)
Percentage_O_AD <- round(Other_AD/Other * 100, 2)

#### AGE
Thirty_younger <- sum(data_diabetic_selected$age == "[0-10)") + sum(data_diabetic_selected$age == "[10-20)") + sum(data_diabetic_selected$age == "[20-30)")
Sixty_Thirty <- sum(data_diabetic_selected$age == "[30-40)") + sum(data_diabetic_selected$age == "[40-50)") + sum(data_diabetic_selected$age == "[50-60)")
Older <- Population_size - Thirty_younger - Sixty_Thirty

Percentage_T_Y <- round(Thirty_younger/Population_size * 100, 2)
Percentage_S_T <- round(Sixty_Thirty/Population_size * 100, 2)
Percentage_Old <- round(Older/Population_size * 100, 2)


Thirty_younger_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$age == "[0-10)") + sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$age == "[10-20)") + sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$age == "[20-30)")
Sixty_Thirty_AD <- sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$age == "[30-40)") + sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$age == "[40-50)") + sum(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$age == "[50-60)")
Older_AD <- sum(data_diabetic_selected$readmitted == "<30") - Thirty_younger_AD - Sixty_Thirty_AD

Percentage_T_Y_AD <- round(Thirty_younger_AD/Thirty_younger * 100, 2)
Percentage_S_T_AD <- round(Sixty_Thirty_AD/Sixty_Thirty* 100, 2)
Percentage_Old_AD <- round(Older_AD/Older * 100, 2)

# race
race_table <- table(data_diabetic_selected$race)
af_amer <- table(data_diabetic_selected$race)[2]
asian <- table(data_diabetic_selected$race)[3]
cauc <- table(data_diabetic_selected$race)[4]
hisp <- table(data_diabetic_selected$race)[5]
other <- table(data_diabetic_selected$race)[6] + table(data_diabetic_selected$race)[5] + table(data_diabetic_selected$race)[3]
missing <- table(data_diabetic_selected$race)[1]

#percentage race
percetage_af_amer <- round(af_amer / Population_size * 100, 2)
percetage_cauc <- round(cauc / Population_size * 100, 2)
percetage_other <- round(other / Population_size * 100, 2)
percetage_missing <- round(missing / Population_size * 100, 2)

#race readmission
af_amer_read <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$race)[2]
cauc_read <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$race)[4]
other_read <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$race)[6] + table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$race)[5] + table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$race)[3]  
missing_read <- table(data_diabetic_selected[data_diabetic_selected$readmitted == "<30",]$race)[1] 

percentage_af_amer_read <- round(af_amer_read / af_amer * 100, 2)
percentage_cauc_read <- round(cauc_read / cauc * 100, 2)
percentage_other_read <- round(other_read / other * 100, 2)
percentage_missing_read <- round(missing_read / missing * 100, 2)

#change categorical age column into numeric to obtain average and median age
data_diabetic_selected$age_avg <- data_diabetic_selected$age
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[0-10)"] <- "5"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[10-20)"] <- "15"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[20-30)"] <- "25"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[30-40)"] <- "35"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[40-50)"] <- "45"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[50-60)"] <- "55"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[60-70)"] <- "65"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[70-80)"] <- "75"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[80-90)"] <- "85"
data_diabetic_selected$age_avg[data_diabetic_selected$age_avg == "[90-100)"] <- "95"
data_diabetic_selected$age_avg <- as.numeric(data_diabetic_selected$age_avg)
summary(data_diabetic_selected$age_avg)


#############################################
#TABLE
#############################################
# Data into matrix, data from all variables
data.a <- c(Non_performed, Percentage_No_result, No_result_readmitted, Percentage_No_result_read,
            High_change, Percentage_High, High_ch_read, Percentage_High_ch_AD,
            High_not_change, Percentage_High_not, High_n_read, Percentage_High_n_AD,
            Normal_results,Percentage_Normal_result,Normal_readmitted,Percentage_Normal_result_read,
            Male_Number_of_encounters,Male_Percentage_of_population,Male_Readmitted_Number_of_encounters,Percentage_male_ad,
            Female_Number_of_encounters,Female_Percentage_of_population,Female_Readmitted_Number_of_encounters,Percentage_Female_ad,
            Discharge_Home,Percentage_Home,Home_readmitted,Percentage_Home_read,
            Discharge_Other,Percentage_Other,Other_readmitted,Percentage_Other_read,
            Admission_ER, Percentage_ER, Admission_ER_AD, Percentage_AD_ER,
            Admission_Phy,Percentage_P, Admission_Phy_AD, Percentage_AD_P,
            Admission_other,Percentage_O, Admission_other_AD, Percentage_AD_O,
            IM,Percentage_IM,IM_AD,Percentage_IM_AD,
            Cardiology,Percentage_C,Cardiology_AD,Percentage_C_AD,
            Surgery, Percentage_S,Surgery_AD, Percentage_S_AD,
            F_GM,Percentage_F,F_GM_AD,Percentage_F_AD,
            Unknown, Percentage_U,Unknown_AD,Percentage_U_AD,
            Other, Percentage_O,Other_AD,Percentage_O_AD,
            af_amer,percetage_af_amer,af_amer_read,percentage_af_amer_read,
            cauc,percetage_cauc,cauc_read,percentage_cauc_read,
            other,percetage_other,other_read,percentage_other_read,
            missing,percetage_missing,missing_read,percentage_missing_read,
            Thirty_younger,Percentage_T_Y,Thirty_younger_AD,Percentage_T_Y_AD,
            Sixty_Thirty,Percentage_S_T,Sixty_Thirty_AD,Percentage_S_T_AD,
            Older,Percentage_Old,Older_AD,Percentage_Old_AD
            )


# create matrix with 24 columns and 4 rows
data = matrix(data.a, nrow=24, ncol=4, byrow=TRUE)

# specify the column names and row names of matrix
colnames(data) = c('n ncounters','% of population',' n encounters','% in group')
rownames(data) <- c('No test', 'Result High + medicine changed','Result high + medicine unchanged','Normal result',
                    'male','female',
                    'Discharged to home', 'Otherwise',
                    'Admitted from ER', 'Admitted becuase of referral', 'Otherwise',
                    'Internal Medicine', 'Cardiology', 'Surgery', 'Family/GP', 'Missing or unkown', 'Other',
                    'African American', 'Caucasioan', 'Other', 'Missing',
                    '<30 year', '30-60 years', ">60")

# assign to table
final2=as.table(data)

# display
final2


