### Explore EdSurvey
### 5/17/18
### Yuqi Liao

# install.packages("EdSurvey")
# install the latest edsurvey on github (to get the latest feature/bug fixes)
# devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

#library(EdSurvey)
library(dplyr)
library(readr)

##### Download and load the Data #####
# downloadTIMSS(years = 2015, root = "G:/Conference/2019/Data/TIMSS", cache=FALSE)
# downloadTIMSS(years = 2011, root = "G:/Conference/2019/Data/TIMSS", cache=FALSE)
# # Reading in Data
# TIMSS15_G4<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2015", countries = c("*"), gradeLvl = "4")
# TIMSS15_G8<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2015", countries = c("*"), gradeLvl = "8",  verbose = TRUE)
# TIMSS11_G4<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2011", countries = c("*"), gradeLvl = "4", verbose = TRUE)
# TIMSS11_G8<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2011", countries = c("*"), gradeLvl = "8", verbose = TRUE)

# # Save objects in the environment
# save.image(file = "TIMSS_objects.Rdata")
# load(file = "G:/Conference/2019/Git/InternationalAssessment_LanguageProject/TIMSS_objects.Rdata")


##### percent & achievement by ITLANG ##### 
T15_G4_ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS15_G4,
                             jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")

T11_G4_ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS11_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G4_math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS11_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G8_ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS11_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G8_math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS11_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")

##### percent & achievement by Language at home ##### 
T15_G4_ssci_asbg03 <- edsurveyTable(formula = ssci ~ asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_asbg03 <- edsurveyTable(formula = mmat ~ asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_bsbg03 <- edsurveyTable(formula = ssci ~ bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_bsbg03 <- edsurveyTable(formula = mmat ~ bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")

T11_G4_ssci_asbg03 <- edsurveyTable(formula = ssci ~ asbg03, data = TIMSS11_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G4_math_asbg03 <- edsurveyTable(formula = mmat ~ asbg03, data = TIMSS11_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G8_ssci_bsbg03 <- edsurveyTable(formula = ssci ~ bsbg03, data = TIMSS11_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G8_math_bsbg03 <- edsurveyTable(formula = mmat ~ bsbg03, data = TIMSS11_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")


##### percent & achievement by ITLANG & Language at home ##### 
T15_G4_ssci_itlang_asbg03 <- edsurveyTable(formula = ssci ~ itlang + asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_itlang_asbg03 <- edsurveyTable(formula = mmat ~ itlang + asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_itlang_bsbg03 <- edsurveyTable(formula = ssci ~ itlang + bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_itlang_bsbg03 <- edsurveyTable(formula = mmat ~ itlang + bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")

T11_G4_ssci_itlang_asbg03 <- edsurveyTable(formula = ssci ~ itlang + asbg03, data = TIMSS11_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G4_math_itlang_asbg03 <- edsurveyTable(formula = mmat ~ itlang + asbg03, data = TIMSS11_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G8_ssci_itlang_bsbg03 <- edsurveyTable(formula = ssci ~ itlang + bsbg03, data = TIMSS11_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T11_G8_math_itlang_bsbg03 <- edsurveyTable(formula = mmat ~ itlang + bsbg03, data = TIMSS11_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")




### Country Lists #####
### T15 - G4
T15_G4_countryList <- T15_G4_ssci_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)

T15_G8_countryList <- T15_G8_ssci_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)

T11_G4_countryList <- T11_G4_ssci_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)

T11_G8_countryList <- T11_G8_ssci_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)


# Export objects as excels

objectList <- list(T15_G4_ssci_itlang, T15_G4_math_itlang, T15_G8_ssci_itlang, T15_G8_math_itlang, T11_G4_ssci_itlang, T11_G4_math_itlang, T11_G8_ssci_itlang, T11_G8_math_itlang, T15_G4_ssci_asbg03, T15_G4_math_asbg03, T15_G8_ssci_bsbg03, T15_G8_math_bsbg03, T11_G4_ssci_asbg03, T11_G4_math_asbg03, T11_G8_ssci_bsbg03, T11_G8_math_bsbg03, T15_G4_ssci_itlang_asbg03, T15_G4_math_itlang_asbg03, T15_G8_ssci_itlang_bsbg03, T15_G8_math_itlang_bsbg03, T11_G4_ssci_itlang_asbg03, T11_G4_math_itlang_asbg03, T11_G8_ssci_itlang_bsbg03, T11_G8_math_itlang_bsbg03)


names(objectList) <- c("T15_G4_ssci_itlang", "T15_G4_math_itlang", "T15_G8_ssci_itlang", "T15_G8_math_itlang", "T11_G4_ssci_itlang", "T11_G4_math_itlang", "T11_G8_ssci_itlang", "T11_G8_math_itlang", "T15_G4_ssci_asbg03", "T15_G4_math_asbg03", "T15_G8_ssci_bsbg03", "T15_G8_math_bsbg03", "T11_G4_ssci_asbg03", "T11_G4_math_asbg03", "T11_G8_ssci_bsbg03", "T11_G8_math_bsbg03", "T15_G4_ssci_itlang_asbg03", "T15_G4_math_itlang_asbg03", "T15_G8_ssci_itlang_bsbg03", "T15_G8_math_itlang_bsbg03", "T11_G4_ssci_itlang_asbg03", "T11_G4_math_itlang_asbg03", "T11_G8_ssci_itlang_bsbg03", "T11_G8_math_itlang_bsbg03")

i <- 1
for (object in objectList){
  write_csv(object$data, path = paste0(names(objectList)[[i]], ".csv"))
  i <- i + 1
}
rm(i)



countryList <- list(T15_G4_countryList, T15_G8_countryList, T11_G4_countryList, T11_G8_countryList)
names(countryList) <- c("T15_G4_countryList", "T15_G8_countryList", "T11_G4_countryList", "T11_G8_countryList")
i <- 1
for (object in countryList){
  write_csv(object, path = paste0(names(countryList)[[i]], ".csv"))
  i <- i + 1
}
rm(i)

















### TIMSS 2015 #####
# Search for the variable
searchSDF(string = "language", data = TIMSS15_G4, levels = TRUE)
# ITLANG
ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS15_G4,
                     jrrIMax = Inf, varMethod = "jackknife")
math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS15_G4,
                             jrrIMax = Inf, varMethod = "jackknife")
math_asbg03 <- edsurveyTable(formula = mmat ~ asbg03, data = TIMSS15_G4,
                             jrrIMax = Inf, varMethod = "jackknife")
math_itlang_asbg03 <- edsurveyTable(formula = mmat ~ itlang + asbg03, data = TIMSS15_G4,
                             jrrIMax = Inf, varMethod = "jackknife")
math_itlang_asdghrl <- edsurveyTable(formula = mmat ~ itlang + asdghrl, data = TIMSS15_G4,
                             jrrIMax = Inf, varMethod = "jackknife")
#asbg05k: Do you have any of these things at your home? K: <country-specific indicator of wealth>
math_asbg05k <- edsurveyTable(formula = mmat ~ asbg05k, data = TIMSS15_G4,
                                     jrrIMax = Inf, varMethod = "jackknife")
math_itlang_asbg05k <- edsurveyTable(formula = mmat ~ itlang + asbg05k, data = TIMSS15_G4,
                              jrrIMax = Inf, varMethod = "jackknife")

#asbg05i: Do you have any of these things at your home? I: <country-specific indicator of wealth>
math_asbg05i <- edsurveyTable(formula = mmat ~ asbg05i, data = TIMSS15_G4,
                              jrrIMax = Inf, varMethod = "jackknife")
math_itlang_asbg05i <- edsurveyTable(formula = mmat ~ itlang + asbg05i, data = TIMSS15_G4,
                              jrrIMax = Inf, varMethod = "jackknife")


# Language at home

# Regression

lm_math_itlang <- lm.sdf(formula = mmat ~ itlang, data = TIMSS15_G4)
summary(lm_math_itlang[[3]])

#home resource for learning/index
lm_math_itlang_asdghrl <- lm.sdf(formula = mmat ~ itlang + asdghrl, data = TIMSS15_G4)
summary(lm_math_itlang_asdghrl[[3]])

#home resource for learning/scale
lm_math_itlang_asbghrl <- lm.sdf(formula = mmat ~ itlang + asbghrl, data = TIMSS15_G4)
summary(lm_math_itlang_asbghrl[[3]])




### 06/14/18 #####
#Q1: how many education systems administered TIMSS in multiple languages?
table <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS15_G4,
              jrrIMax = Inf, varMethod = "jackknife")

table$data %>% 


























