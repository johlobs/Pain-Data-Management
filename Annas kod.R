#Här är personnummer delen deletad. Finns i TRE-versionen av dokumentet
---
  title: 'PainSensPROM_Baseline_DataManagement'
output:
  word_document: default
date: "2025-04-04"
---


  ## Cleaning and creating variables

  ### Contens of adjusting, cleaning and adding data into the dataframe
#  All changes should be done in the my_data and that is the one in archive in the end with raw data and all this code as a memory of adjustments and variable creation)

#### In the dataframe my_data you find the code under each relevant code chunk under the title "# ADJUSTING DATA". Below is a list of the variables where an adjustment is present
#- personnr – 260204: DELETED IN THIS VERSION OF THE WORD FILE
#- sex
#- weight
#- mean pain intyensity last 7 days during movement

# ==== MY CODE =====
library(readxl)

path <- "C:/Users/nepet/Documents/Data Management Sahlgrenska Academy"
testfile    <- file.path(path, "Testfile.xlsx")
my_data <- read_excel(testfile, sheet = "Data")

# ===== END OF MY CODE ===
### Creating new variables from raw variables in the file my_data.

#### Variable: age
  #```{r}
# BIN_Persnr = 1 if there is a personal number.
  #my_data$BIN_persnr <- ifelse(my_data$VAR02 < 99999999999, 0, 1)

# Birthyear
  #my_data$birthyear <- my_data$BIN_persnr*(trunc((my_data$BIN_persnr*my_data$VAR02)/100000000)) + (1-my_data$BIN_persnr)*(trunc((my_data$VAR02/10000)))


# ADJUSTING DATA
################
  #Adjustment på personnummer och fördelseår är deleted i denna word-fil av R-kod.
################

#print(my_data$Svarstillfälle[my_data$ID == 40])
  #current_date <- as.POSIXlt(my_data$Svarstillfälle)
  #current_year <- 1900 + current_date$year
#print(current_year)

# Age
  #my_data$age <- current_year - my_data$birthyear
#print(my_data$age)

```

#### Variable: sex and sex_id - sex identity
  # sex: 0 = man, 1 = woman
  # sex_id: 0 = man, 1 = woman, 2 = other
  # ```{r}
# 0 = man, 1 = woman
my_data$sex <- (2 - my_data$VAR04_1) + 2*(2 - my_data$VAR04_2) - 1

# 0 = man, 1 = woman, 2 = other
my_data$sex_id <- ((2 - my_data$VAR05_1) + 2*(2 - my_data$VAR05_2) +  3*(2 - my_data$VAR05_3)) - 1


# ADJUSTING DATA
################

# Have found a few missing in variable SEX. From personnummer they can be imputed.
#Id 207	old_ID 7	Group 2 woman, sex=1
my_data$sex[my_data$ID == 207] <- 1

#Id 281	old_ID 81	Group 2 man, sex=0
my_data$sex[my_data$ID == 281] <- 0

#Id 590	old_ID 90	Group 3 woman, sex=1
my_data$sex[my_data$ID == 590] <- 1

```


#### Variable: length (cm)
```{r}
my_data$length <- my_data$VAR06

```

#### Variable: weigth (kg)
```{r}
my_data$weight <- my_data$VAR07

# ADJUSTING DATA
################

# The weight on ID=7 (old_ID=7 in group=1) had reported VAR07=995. We assume that the correct VAR07=95 kg.
my_data$weight[my_data$ID == 7]  <- 95

################

```

#### Variable: BMI and BMI_cat
Categories according to 
Weir CB, Jan A. BMI Classification Percentile And Cut Off Points. [Updated 2023 Jun 26]. In: StatPearls [Internet]. Treasure Island (FL): SataPearls Publishing; 2024 Jan-. Available from: https://www.ncbi.nlm.nih.gov/book/NBK541070/
  
  BMI_6cat: 1 = underweigth (less or equal to 18.4999), 2 = normal weigth (18.5-24.99), 3 = overweight (25-29.99), 4 = Obesity1 (30-34.99), 5 = Obestity2 (35-39.99), 6 = Obestity3 (40- )

BMI_4cat: 1 = underweigth (less or equal to 18.4999), 2 = normal weigth (18.5-24.99), 3 = overweight (25-29.99), 4 = Obesity (30- )
```{r}
# BMI
my_data$BMI <- my_data$weight / ((my_data$length/100)*(my_data$length/100))


# BMI_6cat
#my_data$BMI_6cat <- my_data$BMI 18.49=1) (18.50 thru 24.99=2) (25.00 thru 29.99=3) (30.00 thru 34.99=4) 
#    (35.00 thru 39.99=5) (40.00 thru Highest=6) INTO BMI_cat

# BMI_4cat
#BMI_4cat: 1 = under weigth (less or equal to 18.4999), 2 = normal weigth #(18.5-24.99), 3 = over weight (25-29.99), 4 = Obesity (30- )
```


#### Variable: edu - Education
1 = Primary/grundskola, 2 = Secondary/Gymnasium eller yrkesutb (minst 2 år), 3 = University edu/Universitet eller högskola
```{r}
# 1 = Primary/grundskola, 2 = Secondary/Gymnasium eller yrkesutb (minst 2 år), 3 = University edu/Universitet eller högskola
my_data$edu <- (2-my_data$VAR08_1) + 2*(2-my_data$VAR08_2) + 3*(2-my_data$VAR08_3)


# Education in two categories for Rasch analysis: 0=gymnasium, grundskola and 1=university, högskola
my_data$edu_BIN <- ((2-my_data$VAR08_1)+(2 - my_data$VAR08_2)+2*(2 - my_data$VAR08_3)) - 1

```

#### Variable: origin - Region of birth
1 = Sverige, 2 = Other Nordic country, 3 = European country, not Nordic, 4 = Country outside Europe

```{r}
# Origin - Birth region in the world
my_data$origin <- (2 -my_data$VAR09_1) + 2*(2 - my_data$VAR09_2) + 3 *(2-my_data$VAR09_3) +4*(2-my_data$VAR09_4)

```

#### Variable: Employment, sickleave, study, pensioner65 (approximation of pnsioner)
employment75 = employment at least 75%
sickleave75 = sickleave at least 75%
student75 = student at least 75%

employment50 = employment at least 50%
sickleave50 = sickleave at least 50%
student50 = student at least 50%

pensioner65 = aged over 65 years old

```{r}

#Employment at least 75%
my_data$employment75[my_data$VAR10_1 < 3] <- 0
my_data$employment75[my_data$VAR10_1 > 2] <- 1
my_data$employment75[is.na(my_data$VAR10_1)] <- 0
summary(my_data$employment75)

#Sickleave at least 75%
my_data$sickleave75[my_data$VAR10_2 < 3] <- 0
my_data$sickleave75[my_data$VAR10_2 > 2] <- 1
my_data$sickleave75[is.na(my_data$VAR10_2)] <- 0
summary(my_data$sickleave75)

#Student at least 75%
my_data$student75[my_data$VAR10_3 < 3] <- 0
my_data$student75[my_data$VAR10_3 > 2] <- 1
my_data$student75[is.na(my_data$VAR10_3)] <- 0
summary(my_data$student75)

#Pensioner65
my_data$pensioner65[my_data$age < 66] <- 0
my_data$pensioner65[my_data$age > 65] <- 1
summary(my_data$pensioner65)

table(my_data$employment75)
table(my_data$sickleave75)
table(my_data$student75)
table(my_data$pensioner65)
my_table <- table(my_data$employment75, my_data$sickleave75)
print(my_table)
my_table <- table(my_data$employment75, my_data$student75)
print(my_table)
my_table <- table(my_data$employment75, my_data$pensioner65)
print(my_table)
my_table <- table(my_data$sickleave75, my_data$student75)
print(my_table)
my_table <- table(my_data$sickleave75, my_data$pensioner65)
print(my_table)


#Employment at least 50%
my_data$employment50[my_data$VAR10_1 < 2] <- 0
my_data$employment50[my_data$VAR10_1 > 1] <- 1
my_data$employment50[is.na(my_data$VAR10_1)] <- 0
summary(my_data$employment50)

#Sickleave at least 50%
my_data$sickleave50[my_data$VAR10_2 < 2] <- 0
my_data$sickleave50[my_data$VAR10_2 > 1] <- 1
my_data$sickleave50[is.na(my_data$VAR10_2)] <- 0
summary(my_data$sickleave50)

#Student at least 50%
my_data$student50[my_data$VAR10_3 < 2] <- 0
my_data$student50[my_data$VAR10_3 > 1] <- 1
my_data$student50[is.na(my_data$VAR10_3)] <- 0
summary(my_data$student50)

table(my_data$employment50)
table(my_data$sickleave50)
table(my_data$student50)
my_table <- table(my_data$employment50, my_data$sickleave50)
print(my_table)
my_table <- table(my_data$employment50, my_data$student50)
print(my_table)
my_table <- table(my_data$employment50, my_data$pensioner65)
print(my_table)
my_table <- table(my_data$sickleave50, my_data$student50)
print(my_table)
my_table <- table(my_data$sickleave50, my_data$pensioner65)
print(my_table)

my_table <- table(my_data$sickleave50[my_data$employment50 == 0], my_data$pensioner65[my_data$employment50 == 0])
print(my_table)
my_table <- table(my_data$sickleave50[my_data$employment50 == 1], my_data$pensioner65[my_data$employment50 == 1])
print(my_table)

my_table <- table(my_data$sickleave50[my_data$employment50 == 0], my_data$student50[my_data$employment50 == 0])
print(my_table)
my_table <- table(my_data$sickleave50[my_data$employment50 == 1], my_data$student50[my_data$employment50 == 1])
print(my_table)

```


### Pain variables

#### pain7days - Pain last 7 days, yes (1) or no (0)

```{r}

my_data$pain7days=(2-my_data$VAR11_1)
summary(my_data$pain7days)

print(my_data$pain7days)

```
#### pain_duration - continues duration, chronic_pain - duration of 3 months or more
pain_duration = 0 --> Less than a month
pain_duration = 1 --> Less than 3 months
pain_duration = 2 --> Less than 12 months
pain_duration = 3 --> Less than 2 years (24 months)
pain_duration = 4 --> More than 2 years

chronic_pain = 0 if pain7days=0 or if pain7days=1 and pain_duration >= 2

```{r}
my_data$pain_duration <- ((2-my_data$VAR17_1)+2*(2-my_data$VAR17_2)+3*(2-my_data$VAR17_3)+4*(2-my_data$VAR17_4)+5*(2-my_data$VAR17_5)) - 1
summary(my_data$pain_duration)

my_data$chronic_pain[is.na(my_data$pain7days)] <- NA
my_data$chronic_pain[my_data$pain7days == 0] <- 0
my_data$chronic_pain[my_data$pain7days == 1 & my_data$pain_duration == 0] <- 0
my_data$chronic_pain[my_data$pain7days == 1 & my_data$pain_duration == 1] <- 0
my_data$chronic_pain[my_data$pain7days == 1 & my_data$pain_duration == 2] <- 1
my_data$chronic_pain[my_data$pain7days == 1 & my_data$pain_duration == 3] <- 1
my_data$chronic_pain[my_data$pain7days == 1 & my_data$pain_duration == 4] <- 1

print(my_data$chronic_pain)

```

#### Pain locations
nr_sites , how many pain sites (0-45). Requires that one said yes on my_data$pain7days ("Pain last 7 day").

```{r}
# nr_sites. Requires that one said yes on my_data$pain7days
my_data$nr_sites <- ifelse(my_data$pain7days == 1, my_data$VAR12_1 + my_data$VAR12_2 + my_data$VAR12_3 + my_data$VAR12_4 + my_data$VAR12_5 + my_data$VAR12_6 + my_data$VAR12_7 + my_data$VAR12_8 + my_data$VAR12_9 + my_data$VAR12_10 + my_data$VAR12_11 + my_data$VAR12_12 + my_data$VAR12_13 + my_data$VAR12_14 + my_data$VAR12_15 + my_data$VAR12_16 + my_data$VAR12_17 + my_data$VAR12_18 + my_data$VAR12_19 + my_data$VAR12_20 + my_data$VAR12_21 + my_data$VAR12_22 + my_data$VAR12_23 + my_data$VAR12_24 + my_data$VAR12_25 + my_data$VAR12_26 + my_data$VAR12_27 + my_data$VAR12_28 + my_data$VAR12_29 + my_data$VAR12_30 + my_data$VAR12_31 + my_data$VAR12_32 + my_data$VAR12_33 + my_data$VAR12_34 + my_data$VAR12_35 + my_data$VAR12_36 + my_data$VAR12_37 + my_data$VAR12_38 + my_data$VAR12_39 + my_data$VAR12_40 + my_data$VAR12_41 + my_data$VAR12_42 + my_data$VAR12_43 + my_data$VAR12_44 + my_data$VAR12_45, NA)

#print(my_data$nr_sites)
```

#### WSP ACR90
ACR90 , widespread pain according to these criterias + chronic pain

```{r}
# Steps to be able to define widespread pain
#Upper (above waist) left quartile = Q1. Yes in at least one of these places >=1. 0=No in all.
my_data$q1 <- (2*8) - (my_data$VAR12_5 + my_data$VAR12_7 + my_data$VAR12_9 + my_data$VAR12_11 + my_data$VAR12_26 + my_data$VAR12_28 + my_data$VAR12_30 + my_data$VAR12_32)

#Upper (above waist) right quartile = Q2. 
my_data$q2 <- (2*8) - (my_data$VAR12_4 + my_data$VAR12_6 + my_data$VAR12_8 + my_data$VAR12_10 + my_data$VAR12_27 + my_data$VAR12_29 + my_data$VAR12_31 + my_data$VAR12_33)

#Lower (below waist) left quartile = Q3.    
my_data$q3 <- (2*7) - (my_data$VAR12_18 + my_data$VAR12_20 + my_data$VAR12_22 + my_data$VAR12_38 + my_data$VAR12_40 + my_data$VAR12_42 + my_data$VAR12_44)

#Lower (below waist) right quartile = Q4. 
my_data$q4 <- (2*7) - (my_data$VAR12_17 + my_data$VAR12_19 + my_data$VAR12_21 + my_data$VAR12_39 + my_data$VAR12_41 + my_data$VAR12_43 + my_data$VAR12_45)

#Axial= Q5 
my_data$q5 <- (2*8) - (my_data$VAR12_3 + my_data$VAR12_12 + my_data$VAR12_13 + my_data$VAR12_25 + my_data$VAR12_34 + my_data$VAR12_35 + my_data$VAR12_36 + my_data$VAR12_37)


#Contralateralt_1 = q1 and q4 and axila
my_data$contralat_1 <- ifelse(my_data$q5 > 0 & my_data$q4 > 0 & my_data$q1 > 0, 1, 0)
print(my_data$contralat_1)

#Contralateralt_2=q2 and q3 and axial
my_data$contralat_2 <- ifelse(my_data$q5 > 0 & my_data$q3 > 0 & my_data$q2 > 0, 1, 0)
print(my_data$contralat_2)

###

#ACR90
my_data$ACR90 <- ifelse(my_data$contralat_1 == 1 | my_data$contralat_2 == 1, 1, 0)

#CP_ACR90 : Chronic pain and ACR90 criteria
my_data$CP_ACR90 <- ifelse(my_data$chronic_pain == 1 & my_data$ACR90 == 1, 1, 0)

###

```

#### WSP Manchester
Manch
CP_Manch
```{r}


```

#### WSP WP2019_AGE
WP2019_AGE
CP_WP2019_AGE
WPI = total number of sites in the different regions (original WPI<=19 (but only 15 locations are used), but AGE WPI<=19)

Region 1 (left upper): jaw/head (2, 23), shoulder girdle (5,26), upper arm (7, 28), lower arm/hand (9, 11, 30, 32)
Region 2 (right upper): jaw/head (1, 24), shoulder girdle (4, 27), upper arm (6, 29), lower arm/hand (8, 10, 31, 33)
Region 3 (left lower): hip/buttock/trochanter (18, 38), upper leg (18, 40), lower leg/feet (20, 22, 42, 44)
Region 4 (right lower):  hip/buttock/trochanter (17, 39), upper leg (17, 41), lower leg/feet (19, 21, 43, 45)
Region 5 (axial): neck(3, 25), upper back(34, 35), lower back (36, 37), chest (12, 13), abdomen (14, 15)

Location 16 is not presented???
  
  Criterias:
  - WPI<8
- Pain in 4 out of the five regions (I include all locations)
- Pain for at least 3 months

```{r}
region1 <- ifelse(my_data$VAR12_2 == 1 | my_data$VAR12_23 == 1 | my_data$VAR12_5 == 1 | my_data$VAR12_26 == 1 | my_data$VAR12_7 == 1 | my_data$VAR12_28 == 1 | my_data$VAR12_9 == 1 | my_data$VAR12_11 == 1 | my_data$VAR12_30 == 1 | my_data$VAR12_32 == 1, 1, 0)

region2 <- ifelse(my_data$VAR12_1 == 1 | my_data$VAR12_24 == 1 | my_data$VAR12_4 == 1 | my_data$VAR12_27 == 1 | my_data$VAR12_6 == 1 | my_data$VAR12_29 == 1 | my_data$VAR12_8 == 1 | my_data$VAR12_10 == 1 | my_data$VAR12_31 == 1 | my_data$VAR12_33 == 1, 1, 0)

region3 <- ifelse(my_data$VAR12_18 == 1 | my_data$VAR12_38 == 1 | my_data$VAR12_40 == 1 | my_data$VAR12_20 == 1 | my_data$VAR12_22 == 1 | my_data$VAR12_42 == 1 | my_data$VAR12_44 == 1, 1, 0)

region4 <- ifelse(my_data$VAR12_17 == 1 | my_data$VAR12_39 == 1 | my_data$VAR12_41 == 1 | my_data$VAR12_19 == 1 | my_data$VAR12_21 == 1 | my_data$VAR12_43 == 1 | my_data$VAR12_45 == 1, 1, 0)

region5 <- ifelse(my_data$VAR12_3 == 1 | my_data$VAR12_25 == 1 | my_data$VAR12_34 == 1 | my_data$VAR12_35 == 1 | my_data$VAR12_36 == 1 | my_data$VAR12_37 == 1 | my_data$VAR12_12 == 1 | my_data$VAR12_13 == 1 | my_data$VAR12_14 == 1 | my_data$VAR12_15 == 1, 1, 0)

nr_regions <- region1 + region2 + region3 + region4 + region5
print(nr_regions)

# WPI_AGE, sum of max 19 points
my_data$WPI_AGE <- (ifelse(my_data$VAR12_2 == 1 | my_data$VAR12_23  == 1 , 1, 0)) + (ifelse(my_data$VAR12_5 == 1 | my_data$VAR12_26  == 1 , 1, 0)) + (ifelse(my_data$VAR12_7 == 1 | my_data$VAR12_28  == 1 , 1, 0)) + (ifelse(my_data$VAR12_9 == 1 | my_data$VAR12_11  == 1  | my_data$VAR12_30  == 1  | my_data$VAR12_32  == 1 , 1, 0)) + (ifelse(my_data$VAR12_1 == 1 | my_data$VAR12_24  == 1 , 1, 0)) + (ifelse(my_data$VAR12_4 == 1 | my_data$VAR12_27  == 1 , 1, 0)) + (ifelse(my_data$VAR12_6 == 1 | my_data$VAR12_29  == 1 , 1, 0)) + (ifelse(my_data$VAR12_8 == 1 | my_data$VAR12_10 == 1 | my_data$VAR12_31 == 1 | my_data$VAR12_33 == 1, 1, 0)) + (ifelse(my_data$VAR12_2 == 18 | my_data$VAR12_38  == 1 , 1, 0)) + (ifelse(my_data$VAR12_18 == 1 | my_data$VAR12_40  == 1 , 1, 0)) + (ifelse(my_data$VAR12_20 == 1 | my_data$VAR12_22  == 1  | my_data$VAR12_42  == 1  | my_data$VAR12_44  == 1 , 1, 0)) +  (ifelse(my_data$VAR12_17 == 1 | my_data$VAR12_39  == 1 , 1, 0)) + (ifelse(my_data$VAR12_17 == 1 | my_data$VAR12_41  == 1 , 1, 0)) +   (ifelse(my_data$VAR12_19 == 1 | my_data$VAR12_21  == 1  | my_data$VAR12_43  == 1  | my_data$VAR12_45  == 1 , 1, 0)) + (ifelse(my_data$VAR12_3 == 1 | my_data$VAR12_25  == 1 , 1, 0)) + (ifelse(my_data$VAR12_34 == 1 | my_data$VAR12_35  == 1 , 1, 0)) + (ifelse(my_data$VAR12_36 == 1 | my_data$VAR12_37  == 1 , 1, 0)) + (ifelse(my_data$VAR12_12 == 1 | my_data$VAR12_13  == 1 , 1, 0)) + (ifelse(my_data$VAR12_14 == 1 | my_data$VAR12_15  == 1 , 1, 0))

print(my_data$WPI_AGE)
# If WPI = NA  it means that they did not answer this question. If it was because they did not have pain last 7 days, pain7days = 0 (VAR11_1 == 1), they should have value 0 on WPI.

# If pain7days = NA, then WPI = NA as we don´t know if they had pain last 7 days. 
my_data$WPI_AGE <- ifelse(is.na(my_data$pain7days),NA, my_data$WPI_AGE)
my_data$WPI_AGE <- ifelse(is.na(my_data$WPI_AGE) & my_data$pain7days == 0, 0, my_data$WPI_AGE)
print(my_data$WPI_AGE)
print(nr_regions)

# WPI_AGE > 6 and nr_regions > 3 and chronic pain
print(my_data$pain7days)
print(my_data$WPI_AGE)
print(nr_regions)
my_data$WP2019_AGE <- ifelse(my_data$WPI_AGE > 6, 1, 0 & nr_regions > 3)
print(my_data$WP2019_AGE)

# CP_WP2019_AGE
my_data$CP_WP2019_AGE <- ifelse(my_data$chronic_pain == 1 & my_data$WP2019_AGE == 1, 1, 0)
print(my_data$CP_WP2019_AGE)

my_table <- table(my_data$CP_ACR90, my_data$CP_WP2019_AGE)
print(my_table)
```


#### Fibromyalgia diagnose
Might be a project specific variable and relation to the CWSP_5cat

```{r}

```


#### PainCategories: Pain3cat_CWSP2019
0 = Not chronic pain (not chronic pain and not Chronic WP2019_AGE)
1 = Chronic pain, but not Chronic WP2019_AGE
2 = Chronic WP2019_AGE

```{r}

temp <- ifelse(my_data$chronic_pain == 1, 1, 0)
my_data$Pain3cat_CWSP2019 <- ifelse(my_data$chronic_pain == 1 & my_data$CP_WP2019_AGE == 1, 2, temp)

my_table <- table(my_data$Pain3cat_CWSP2019, my_data$CP_WP2019_AGE)
print(my_table)

my_table <- table(my_data$Pain3cat_CWSP2019, my_data$CP_ACR90)
print(my_table)

```


#### pain_freq - Pain frequency
Describes how frequent the pain is over time (every day, a couple of days per week, ..., not at all/seldom last 12 months)

pain_freq = 1 --> Every day
pain_freq = 2 --> A couple of days per week (1 day of 2)
pain_freq = 3 --> One day per week
pain_freq = 4 --> A couple of days per month (1 day of 10)
pain_freq = 5 --> Not at all, seldom last 12 months

```{r}
my_data$pain_freq <- (2-my_data$VAR16_1)+2*(2-my_data$VAR16_2)+3*(2-my_data$VAR16_3)+4*(2-my_data$VAR16_4)+5*(2-my_data$VAR16_5)

```


#### persistent_pain - Persistent or periodic pain, yes (1) or no (0)

```{r}
my_data$persistent=(2-my_data$VAR18_2)

```


#### nrs_mean7d - Mean pain intensity last 7 days
VAR13
```{r}
my_data$nrs_mean7d <- (2-my_data$VAR13_1) + 2*(2-my_data$VAR13_2) + 3*(2-my_data$VAR13_3) + 4*(2-my_data$VAR13_4) + 5*(2-my_data$VAR13_5) + 6*(2-my_data$VAR13_6) + 7*(2-my_data$VAR13_7) + 8*(2-my_data$VAR13_8) + 9*(2-my_data$VAR13_9) + 10*(2-my_data$VAR13_10) + 11*(2-my_data$VAR13_11) - 1

print(my_data$nrs_mean7d)
```


#### nrs_rest7d - Mean pain intensity last 7 days during rest
VAR14
```{r}
my_data$nrs_rest7d <- (2-my_data$VAR14_1) + 2*(2-my_data$VAR14_2) + 3*(2-my_data$VAR14_3) + 4*(2-my_data$VAR14_4) + 5*(2-my_data$VAR14_5) + 6*(2-my_data$VAR14_6) + 7*(2-my_data$VAR14_7) + 8*(2-my_data$VAR14_8) + 9*(2-my_data$VAR14_9) + 10*(2-my_data$VAR14_10) + 11*(2-my_data$VAR14_11) - 1

print(my_data$nrs_rest7d)
```


#### nrs_move7d - Mean pain intensity last 7 days during movment
VAR15

```{r}
# ADJUSTING DATA
################

# Mistake in reporting a value from a paper questionnaire.
#ID=2005 (old_ID=4):
#Var15_8 is one of the answer categories for the variable about mean pain intensity last 7 days.
#The responder has answered NO on pain intensity level 7, which means that Var_15_8 = 2.
#Corrected to: ID=2005 (old_ID=4): the variable Var15_8 is now =21, should be 2.

my_data$VAR15_8[my_data$ID == 4]  <- 2

################

my_data$nrs_move7d <- (2-my_data$VAR15_1) + 2*(2-my_data$VAR15_2) + 3*(2-my_data$VAR15_3) + 4*(2-my_data$VAR15_4) + 5*(2-my_data$VAR15_5) + 6*(2-my_data$VAR15_6) + 7*(2-my_data$VAR15_7) + 8*(2-my_data$VAR15_8) + 9*(2-my_data$VAR15_9) + 10*(2-my_data$VAR15_10) + 11*(2-my_data$VAR15_11) - 1

print(my_data$nrs_move7d)

```

#### Pain related pharmacephticals
VAR19_1 - VAR19_10

```{r}
#print(my_data$VAR19_1_1)

```
```{r}
#print(my_data$VAR19_2_1)

```

```{r}


```


#### Undergone major treatment during last 12 months
1 = yes, 0 = no
VAR20_1: Pain rehabilitation program (mmr)
VAR20_2: Neuro modulation, spinal stimulation (neurostim)
VAR20_3: Surgery
(VAR20_4: type of surgery)
VAR20_5: Withdrawl of morphine preparations (opioid)
VAR20_6: Other major treatment (other)
(VAR20_7: type of treatment)

```{r}

my_data$trt_mmr <- 2-my_data$VAR20_1
my_data$trt_mmr[is.na(my_data$VAR20_1)] <- 0

my_data$trt_neurostim <- 2-my_data$VAR20_2
my_data$trt_neurostim[is.na(my_data$VAR20_2)] <- 0

my_data$trt_surgery <- 2-my_data$VAR20_3
my_data$trt_surgery[is.na(my_data$VAR20_3)] <- 0

my_data$trt_opioid <- 2-my_data$VAR20_5
my_data$trt_opioid[is.na(my_data$VAR20_5)] <- 0

my_data$trt_other <- 2-my_data$VAR20_6
my_data$trt_other[is.na(my_data$VAR20_6)] <- 0

summary(my_data$trt_mmr)
summary(my_data$trt_neurostim)
summary(my_data$trt_surgery)
summary(my_data$trt_opioid)
summary(my_data$trt_other)
```

#### PSQ14
PSQ14 items. VAR21_1 - VAR21_4, VAR21_6 - VAR21_8, VAR21_10 - VAR21_12, VAR21_14 - VAR21_17. 

```{r}

my_data$psq14 <- (my_data$VAR21_1+my_data$VAR21_2+my_data$VAR21_3+my_data$VAR21_4+my_data$VAR21_6+my_data$VAR21_7+my_data$VAR21_8+my_data$VAR21_10+my_data$VAR21_11+
                    my_data$VAR21_12+my_data$VAR21_14+my_data$VAR21_15+my_data$VAR21_16+my_data$VAR21_17)/14
summary(my_data$psq14)

```

#### PSQ17
PSQ17 items. VAR21_1 - VAR21_17. 

```{r}

my_data$psq17 <- (my_data$VAR21_1+my_data$VAR21_2+my_data$VAR21_3+my_data$VAR21_4+my_data$VAR21_5+my_data$VAR21_6+my_data$VAR21_7+my_data$VAR21_8+my_data$VAR21_9+my_data$VAR21_10+my_data$VAR21_11+
                    my_data$VAR21_12+my_data$VAR21_13+my_data$VAR21_14+my_data$VAR21_15+my_data$VAR21_16+my_data$VAR21_17)/17
summary(my_data$psq17)

```
## Scatter plot of psq14 and psq17
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r pressure, echo=FALSE}

with(my_data, {plot(psq14, psq17, col = ifelse(my_data$CP_WP2019_AGE == 1 , "red", "black"))
  abline(a=0, b=1, col="red", lty=2)
})

```

#### CSI
CSI items. VAR22_1 - VAR22_25
Part A. All questions need to be changed to 0-4 (now coded as 1-5). Sum all questions
adding the variables together will create NA for the sum where at least one variable is NA.
The anserws are saved as 1-5, but need to be recordet according to the instrument as
0 = Inte alls
1 = Lite grann
2 = Måttligt
3 = Ganska mycket
4 = Väldigt mycket
Hence, subtracting 25 after sumation.

```{r}

my_data$csi <- (my_data$VAR22_1 + my_data$VAR22_2 + my_data$VAR22_3 + my_data$VAR22_4 + my_data$VAR22_5 + my_data$VAR22_6 + my_data$VAR22_7 + my_data$VAR22_8 + my_data$VAR22_9 + my_data$VAR22_10 + my_data$VAR22_11 + my_data$VAR22_12 +  my_data$VAR22_13 + my_data$VAR22_14 + my_data$VAR22_15 + my_data$VAR22_16 + my_data$VAR22_17 + my_data$VAR22_18 + my_data$VAR22_19 + my_data$VAR22_20 + my_data$VAR22_21 + my_data$VAR22_22 + my_data$VAR22_23 + my_data$VAR22_24 + my_data$VAR22_25) - 25
summary(my_data$csi)

```

#### CSI B and Diagnoses
Use CSI B and Diagnoses together to create a list of diagnoses.

##### CSI B
VAR23_1 - VAR23_10 
1 = NO  --> 0
2 = YES --> 1

```{r}
# CSI B
my_data$restlesslegs <- (-1) + my_data$VAR23_1
my_data$chronic_fatigue <- (-1) + my_data$VAR23_2
my_data$fibromyalgia <- (-1) + my_data$VAR23_3
my_data$TMD <- (-1) + my_data$VAR23_4
my_data$headache_migraine <- (-1) + my_data$VAR23_5
my_data$ibs <- (-1) + my_data$VAR23_6
my_data$chemical_sens <- (-1) + my_data$VAR23_7
my_data$neckpain <- (-1) + my_data$VAR23_8
my_data$anxiety <- (-1) + my_data$VAR23_9
my_data$depression <- (-1) + my_data$VAR23_10


```

##### Diagnoses/physicians opinion
VAR24_1 - VAR24_8 
1 = YES --> 1
2 = NO  --> 0

```{r}
# According to your physicians opinion
my_data$RA_arthrosis <- 2 - my_data$VAR24_1
my_data$burnout <- 2 - my_data$VAR24_2
my_data$cardiovasc <- 2 - my_data$VAR24_3
my_data$diabetes12 <- 2 - my_data$VAR24_4
my_data$respiratory <- 2 - my_data$VAR24_5
my_data$dermatol <- 2 - my_data$VAR24_6
my_data$cancer_tumor <- 2 - my_data$VAR24_7
my_data$postcovid <- 2 - my_data$VAR24_8

```


#### HAD 
HAD anxiety and depression
HAD - 14 questions in total, 7 concerning anxiety and 7 concerning depression. The sum for them is counted separately. 
Odd numbers(1, 3, 5 osv.) consider anxiety.  Even numbers (2, 4, 6 osv.) consider depression. 0-3 points for every question.

#####Interpretation of resultats
Anxiety
0-7  points: No indications for anxiety
8-10 points: Possibly anxiety
≥ 11 points: Indications of anxiety

Depression
0-7  points: No indications for depression
8-10 points: Possibly depression
≥ 11 points: Indications of depression

```{r}

my_data$HAD_A1a <- 3*(2-my_data$VAR25_1)
my_data$HAD_A1b <- 2*(2-my_data$VAR25_2)
my_data$HAD_A1c <- 1*(2-my_data$VAR25_3)
my_data$HAD_A1d <- 0*(2-my_data$VAR25_4)

my_data$HAD_D1a <- 3*(2-my_data$VAR26_1)
my_data$HAD_D1b <- 2*(2-my_data$VAR26_2)
my_data$HAD_D1c <- 1*(2-my_data$VAR26_3)
my_data$HAD_D1d <- 0*(2-my_data$VAR26_4)

my_data$HAD_A2a <- 3*(2-my_data$VAR27_1)
my_data$HAD_A2b <- 2*(2-my_data$VAR27_2)
my_data$HAD_A2c <- 1*(2-my_data$VAR27_3)
my_data$HAD_A2d <- 0*(2-my_data$VAR27_4)

my_data$HAD_D2a <- 3*(2-my_data$VAR28_1)
my_data$HAD_D2b <- 2*(2-my_data$VAR28_2)
my_data$HAD_D2c <- 1*(2-my_data$VAR28_3)
my_data$HAD_D2d <- 0*(2-my_data$VAR28_4)

my_data$HAD_A3a <- 3*(2-my_data$VAR29_1)
my_data$HAD_A3b <- 2*(2-my_data$VAR29_2)
my_data$HAD_A3c <- 1*(2-my_data$VAR29_3)
my_data$HAD_A3d <- 0*(2-my_data$VAR29_4)

my_data$HAD_D3a <- 3*(2-my_data$VAR30_1)
my_data$HAD_D3b <- 2*(2-my_data$VAR30_2)
my_data$HAD_D3c <- 1*(2-my_data$VAR30_3)
my_data$HAD_D3d <- 0*(2-my_data$VAR30_4)

my_data$HAD_A4a <- 3*(2-my_data$VAR31_1)
my_data$HAD_A4b <- 2*(2-my_data$VAR31_2)
my_data$HAD_A4c <- 1*(2-my_data$VAR31_3)
my_data$HAD_A4d <- 0*(2-my_data$VAR31_4)

my_data$HAD_D4a <- 3*(2-my_data$VAR32_1)
my_data$HAD_D4b <- 2*(2-my_data$VAR32_2)
my_data$HAD_D4c <- 1*(2-my_data$VAR32_3)
my_data$HAD_D4d <- 0*(2-my_data$VAR32_4)

my_data$HAD_A5a <- 3*(2-my_data$VAR33_1)
my_data$HAD_A5b <- 2*(2-my_data$VAR33_2)
my_data$HAD_A5c <- 1*(2-my_data$VAR33_3)
my_data$HAD_A5d <- 0*(2-my_data$VAR33_4)

my_data$HAD_D5a <- 3*(2-my_data$VAR34_1)
my_data$HAD_D5b <- 2*(2-my_data$VAR34_2)
my_data$HAD_D5c <- 1*(2-my_data$VAR34_3)
my_data$HAD_D5d <- 0*(2-my_data$VAR34_4)

my_data$HAD_A6a <- 3*(2-my_data$VAR35_1)
my_data$HAD_A6b <- 2*(2-my_data$VAR35_2)
my_data$HAD_A6c <- 1*(2-my_data$VAR35_3)
my_data$HAD_A6d <- 0*(2-my_data$VAR35_4)

my_data$HAD_D6a <- 3*(2-my_data$VAR36_1)
my_data$HAD_D6b <- 2*(2-my_data$VAR36_2)
my_data$HAD_D6c <- 1*(2-my_data$VAR36_3)
my_data$HAD_D6d <- 0*(2-my_data$VAR36_4)

my_data$HAD_A7a <- 3*(2-my_data$VAR37_1)
my_data$HAD_A7b <- 2*(2-my_data$VAR37_2)
my_data$HAD_A7c <- 1*(2-my_data$VAR37_3)
my_data$HAD_A7d <- 0*(2-my_data$VAR37_4)

my_data$HAD_D7a <- 3*(2-my_data$VAR38_1)
my_data$HAD_D7b <- 2*(2-my_data$VAR38_2)
my_data$HAD_D7c <- 1*(2-my_data$VAR38_3)
my_data$HAD_D7d <- 0*(2-my_data$VAR38_4)

my_data$had_a <- (my_data$HAD_A1a + my_data$HAD_A1b + my_data$HAD_A1c + my_data$HAD_A1d)  + (my_data$HAD_A2a + my_data$HAD_A2b + my_data$HAD_A2c + my_data$HAD_A2d) + (my_data$HAD_A3a + my_data$HAD_A3b + my_data$HAD_A3c + my_data$HAD_A3d) + (my_data$HAD_A4a + my_data$HAD_A4b + my_data$HAD_A4c + my_data$HAD_A4d) + (my_data$HAD_A5a + my_data$HAD_A5b + my_data$HAD_A5c + my_data$HAD_A5d) + (my_data$HAD_A6a + my_data$HAD_A6b + my_data$HAD_A6c + my_data$HAD_A6d) + (my_data$HAD_A7a + my_data$HAD_A7b + my_data$HAD_A7c + my_data$HAD_A7d)

my_data$had_d <- (my_data$HAD_D1a + my_data$HAD_D1b + my_data$HAD_D1c + my_data$HAD_D1d) + (my_data$HAD_D2a + my_data$HAD_D2b + my_data$HAD_D2c + my_data$HAD_D2d) + (my_data$HAD_D3a + my_data$HAD_D3b + my_data$HAD_D3c + my_data$HAD_D3d) + (my_data$HAD_D4a + my_data$HAD_D4b + my_data$HAD_D4c + my_data$HAD_D4d) + (my_data$HAD_D5a + my_data$HAD_D5b + my_data$HAD_D5c + my_data$HAD_D5d) + (my_data$HAD_D6a + my_data$HAD_D6b + my_data$HAD_D6c + my_data$HAD_D6d) + (my_data$HAD_D7a + my_data$HAD_D7b + my_data$HAD_D7c + my_data$HAD_D7d)

summary(my_data$had_a)
summary(my_data$had_d)
```

#### SCI-93
The anserws are saved as 1-5, but need to be recordet according to the instrument as
0 = Inte alls
1 = Lite grann
2 = Måttligt
3 = Ganska mycket
4 = Väldigt mycket
Hence, subtracting 35 after sumation.

```{r}

my_data$sci93 <- (my_data$VAR39_1 + my_data$VAR39_2 + my_data$VAR39_3 + my_data$VAR39_4 + my_data$VAR39_5 + my_data$VAR39_6 + my_data$VAR39_7 + my_data$VAR39_8 + my_data$VAR39_9 +my_data$VAR39_10 + my_data$VAR39_11 + my_data$VAR39_12 + my_data$VAR39_13 + my_data$VAR39_14 + my_data$VAR39_15 + my_data$VAR39_16 + my_data$VAR39_17 + my_data$VAR39_18 +my_data$VAR39_19 + my_data$VAR39_20 + my_data$VAR39_21 + my_data$VAR39_22 + my_data$VAR39_23 + my_data$VAR39_24 + my_data$VAR39_25 + my_data$VAR39_26 + my_data$VAR39_27 + my_data$VAR39_28 + my_data$VAR39_29 + my_data$VAR39_30 + my_data$VAR39_31 + my_data$VAR39_32 + my_data$VAR39_33 + my_data$VAR39_34 + my_data$VAR39_35) - 35
summary(my_data$sci93)
```
## Scatter plot of CSI and SCI93
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r pressure, echo=FALSE}

plot(my_data$sci93 , my_data$csi, col = ifelse(my_data$CP_WP2019_AGE == 1 , "red", "black"))

```

#### ISI - Insomnia Severity Index
The VAR40_1 to VAR40_3 each have answers 1 to 5,  but the original answerers are coded from 0 to 4. Need to delete 1 from each answer.
VAR41_1 to VAR41_5 each has the answer yes (1) or no (2) and is indicator for the coding of 1 to 5.
VAR41_1-VAR41_5 = ""Hur nöjd/missnöjd är du med ditt nuvarande sömnmönster?"

Yes (1) on VAR41_1 means answer "Mycket nöjd", which originaly should be coded 0.
Yes (1) on VAR41_2 means answer "Nöjd", which originaly should be coded 1.
etc
Yes (1) on VAR41_5 means answer "Mycket missnöjd", which originaly should be coded 4.

Similarly with VAR42_1-Var42_5, ..., VAR44_1-VAR44_5.

```{r}

my_data$ISI <- ( (my_data$VAR40_1-1) + (my_data$VAR40_2-1) + (my_data$VAR40_3-1) + ((2-my_data$VAR41_1)+2*(2-my_data$VAR41_2)+3*(2-my_data$VAR41_3)+4*(2-my_data$VAR41_4)+5*(2-my_data$VAR41_5) - 1) + ((2-my_data$VAR42_1)+2*(2-my_data$VAR42_2)+3*(2-my_data$VAR42_3)+4*(2-my_data$VAR42_4)+5*(2-my_data$VAR42_5) - 1) + ((2-my_data$VAR43_1)+2*(2-my_data$VAR43_2)+3*(2-my_data$VAR43_3)+4*(2-my_data$VAR43_4)+5*(2-my_data$VAR43_5) - 1) + ((2-my_data$VAR44_1)+2*(2-my_data$VAR44_2)+3*(2-my_data$VAR44_3)+4*(2-my_data$VAR44_4)+5*(2-my_data$VAR44_5) - 1) )

summary(my_data$ISI)
```


```{r}

mean_ISI_by_pain <- tapply(my_data$ISI, my_data$pain_duration, mean)
print(mean_ISI_by_pain)

```

### Life style

##### Physical activity
PE_time: VAR45_1 to VAR45_6 - Time for physical excersise (PE) each week
0 = 0 min
1 = mer än 0 men <30 min
2 = 30-60 min
3 = 60-90 min
4 = 90-120 min (1.5-2h)
5 = more than 120 min (>2h)

PE_days: VAR46_1 to VAR46_8 - Days per week of 30 min of exercise
0-7 days

PA_time: VAR47_1 to VAR47_7 - Time for every day exercise (physival cativity PA) (walking, gardening etc) each week
0 = 0 min
1 = mer än 0 men <30 min
2 = 30-60 min
3 = 60-90 min
4 = 90-150 min (1.5-2.5h)
5 = 150-300 min (2.5-5h)
6 = more than 300 min (>5h)

PA_days: VAR48_1 to VAR48_8 - Days per week of 30 min of physical activity (PA)
0-7 days

```{r}
my_data$PE_time <- (-1) + (2-my_data$VAR45_1) + 2*(2-my_data$VAR45_2) + 3*(2-my_data$VAR45_3) + 4*(2-my_data$VAR45_4) + 5*(2-my_data$VAR45_5) + 6*(2-my_data$VAR45_6)

my_data$PE_days <- (-1) + (2-my_data$VAR46_1) + 2*(2-my_data$VAR46_2) + 3*(2-my_data$VAR46_3) + 4*(2-my_data$VAR46_4) + 5*(2-my_data$VAR46_5) + 6*(2-my_data$VAR46_6) + 7*(2-my_data$VAR46_7) + 8*(2-my_data$VAR46_8)


my_data$PA_time <- (-1) + (2-my_data$VAR47_1) + 2*(2-my_data$VAR47_2) + 3*(2-my_data$VAR47_3) + 4*(2-my_data$VAR47_4) + 5*(2-my_data$VAR47_5) + 6*(2-my_data$VAR47_6) + 7*(2-my_data$VAR47_7)

my_data$PA_days <- (-1) + (2-my_data$VAR48_1) + 2*(2-my_data$VAR48_2) + 3*(2-my_data$VAR48_3) + 4*(2-my_data$VAR48_4) + 5*(2-my_data$VAR48_5) + 6*(2-my_data$VAR48_6) + 7*(2-my_data$VAR48_7) + 8*(2-my_data$VAR48_8)

```


#### Smoking
smoke: VAR49_1 to VAR49_4
0 = not smoking or ended more than 6 months ago
1 = not smoking, but ended less than 6 months ago
2 = smoking, but not daily
3 = smoking daily

ant_smoke: VAR50_1 to VAR50_3
0 = if smoke=0 or 1 or 2: 
1 = 1-9 cigarettes/ cigariller per day
2 = 10-19 cigarettes/ cigariller per day
3 = 20 or more cigarettes/ cigariller per day

```{r}
my_data$smoke <- (-1) + (2-my_data$VAR49_1) + 2*(2-my_data$VAR49_2) + 3*(2-my_data$VAR49_3) + 4*(2-my_data$VAR49_4)

my_data$ant_smoke <- (2-my_data$VAR50_1) + 2*(2-my_data$VAR50_2) + 3*(2-my_data$VAR50_3)
my_data$ant_smoke[my_data$smoke < 3] <- 0

my_table <- table(my_data$smoke, my_data$ant_smoke)
print(my_table)

```

#### NOT USEFUL: Snuff - Basically only 8 persons answered the snuff questions
snuff: VAR51_1 to VAR51_4
0 = not snuffing or ended more than 6 months ago
1 = not snuffing, but ended less than 6 months ago
2 = snuffing, but not daily
3 = snuffing daily

ant_snuff: VAR52_1 to VAR52_3
0 = if snuff = 0 or 1 or 2: 
1 = 1-3 boxes/cans per day
2 = 4-6 boxes/cans per day
3 = 7 or more boxes/cans per day

```{r}
my_data$snuff <- (-1) + (2-my_data$VAR51_1) + 2*(2-my_data$VAR51_2) + 3*(2-my_data$VAR51_3) + 4*(2-my_data$VAR51_4)

my_data$ant_snuff <- (2-my_data$VAR52_1) + 2*(2-my_data$VAR52_2) + 3*(2-my_data$VAR52_3)
my_data$ant_snuff[my_data$snuff < 3] <- 0

```
### Now give the file my_data a new name to make sure that the data management is saved

```{r}

R_my_data <- my_data

```

