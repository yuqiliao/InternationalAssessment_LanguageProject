### Explore EdSurvey
### 5/17/18
### Yuqi Liao

# install.packages("EdSurvey")
# install the latest edsurvey on github (to get the latest feature/bug fixes)
# devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

#library(EdSurvey)
library(dplyr)
library(readr)
library(reshape)
library(ggplot2)

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
#omittedLevels = FALSE - the mean and the percentage don't seem to change at all, so no need to do this exercise here
T15_G4_ssci_itlangO <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife", omittedLevels = FALSE)


#keep only countries with more than one itlang
T15_G4_ssci_itlang_shortlist <- T15_G4_ssci_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)
T15_G4_ssci_countryList <- T15_G4_ssci_itlang_shortlist$country %>% unique() #22 countries

#define 22 colors (the first two are for English and Arabic)
color_22 <- c("#2166ac", "#b2182b","#cef2c1","#cef2c2", "#cef2c3", "#cef2c4", "#cef2c5", "#cef2c6", "#cef2c7", "#cef2c8", "#cef2c9", "#cef2d0", "#cef2d1", "#cef2d2", "#cef2d3", "#cef2d4", "#cef2d5", "#cef2d6", "#cef2d7", "#cef2d8", "#cef2d9", "#cef2e0")

T15_G4_ssci_itlang_shortlist_ScorePlot <- T15_G4_ssci_itlang_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()


T15_G4_math_itlang_shortlist <- T15_G4_math_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)
T15_G4_math_countryList <- T15_G4_math_itlang_shortlist$country %>% unique() #23 countries (because South Africa participated in TIMSS numeracy)



T15_G4_math_itlang_shortlist_ScorePlot <- T15_G4_math_itlang_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()



T15_G8_ssci_itlang_shortlist <- T15_G8_ssci_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)
T15_G8_ssci_countryList <- T15_G8_ssci_itlang_shortlist$country %>% unique() #18 countries


T15_G8_ssci_itlang_shortlist_ScorePlot <- T15_G8_ssci_itlang_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()


T15_G8_math_itlang_shortlist <- T15_G8_math_itlang$data %>% 
  group_by(country) %>% 
  mutate(n_itlang = n()) %>% 
  filter(n_itlang > 1)
T15_G8_math_countryList <- T15_G8_math_itlang_shortlist$country %>% unique() #18 countries - same as above



T15_G8_math_itlang_shortlist_ScorePlot <- T15_G8_math_itlang_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()


# T11_G4_ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS11_G4,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G4_math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS11_G4,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G8_ssci_itlang <- edsurveyTable(formula = ssci ~ itlang, data = TIMSS11_G8,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G8_math_itlang <- edsurveyTable(formula = mmat ~ itlang, data = TIMSS11_G8,
#                                     jrrIMax = Inf, varMethod = "jackknife")

##### percent & achievement by Language at home ##### 
T15_G4_ssci_asbg03 <- edsurveyTable(formula = ssci ~ asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_asbg03 <- edsurveyTable(formula = mmat ~ asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_bsbg03 <- edsurveyTable(formula = ssci ~ bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_bsbg03 <- edsurveyTable(formula = mmat ~ bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
#omittedLevels = FALSE (it does not work in edsurvey, see the IDB Analyzer output for now - conclusion: the missing and omitted rate is generally very low, a few cases have more than 5% of the missing rate. (Kuwait in G8_Science.))
T15_G4_ssci_asbg03O <- edsurveyTable(formula = ssci ~ asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife", omittedLevels = FALSE)
T15_G4_math_asbg03O <- edsurveyTable(formula = mmat ~ asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife", omittedLevels = FALSE)
T15_G8_ssci_bsbg03O <- edsurveyTable(formula = ssci ~ bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife", omittedLevels = FALSE)
T15_G8_math_bsbg03O <- edsurveyTable(formula = mmat ~ bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife", omittedLevels = FALSE)

#keep only countries with more than one itlang
T15_G4_ssci_asbg03_shortlist <- T15_G4_ssci_asbg03$data %>% 
  filter(country %in% T15_G4_ssci_countryList) 
T15_G4_math_asbg03_shortlist <- T15_G4_math_asbg03$data %>% 
  filter(country %in% T15_G4_math_countryList) 
T15_G8_ssci_bsbg03_shortlist <- T15_G8_ssci_bsbg03$data %>% 
  filter(country %in% T15_G8_ssci_countryList) 
T15_G8_math_bsbg03_shortlist <- T15_G8_math_bsbg03$data %>% 
  filter(country %in% T15_G8_math_countryList) 

T15_G4_ssci_asbg03_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                            data = T15_G4_ssci_asbg03_shortlist,
                                            LikertVar = "asbg03",
                                            byVar = "country",
                                            pal = c("#DF4949", "#E27A3F","#45B29D", "#334D5C"),
                                            ascending = FALSE)
T15_G4_ssci_asbg03_shortlist_ScorePlot <- T15_G4_ssci_asbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = asbg03)) +
  geom_point(stat = "identity", size = 4) +
  coord_flip()

T15_G4_ssci_asbg03_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                            data = T15_G4_ssci_asbg03_shortlist,
                                            LikertVar = "asbg03",
                                            byVar = "country",
                                            pal = c("#DF4949", "#E27A3F","#45B29D", "#334D5C"),
                                            ascending = FALSE)
T15_G4_math_asbg03_shortlist_ScorePlot <- T15_G4_math_asbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = asbg03)) +
  geom_point(stat = "identity", size = 4) +
  coord_flip()

T15_G8_ssci_bsbg03_shortlist_plot <- likert(sdf = TIMSS15_G8,
                                            data = T15_G8_ssci_bsbg03_shortlist,
                                            LikertVar = "bsbg03",
                                            byVar = "country",
                                            pal = c("#DF4949", "#E27A3F","#45B29D", "#334D5C"),
                                            ascending = FALSE)
T15_G8_ssci_bsbg03_shortlist_ScorePlot <- T15_G8_ssci_bsbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = bsbg03)) +
  geom_point(stat = "identity", size = 4) +
  coord_flip()

T15_G8_math_bsbg03_shortlist_plot <- likert(sdf = TIMSS15_G8,
                                            data = T15_G8_math_bsbg03_shortlist,
                                            LikertVar = "bsbg03",
                                            byVar = "country",
                                            pal = c("#DF4949", "#E27A3F","#45B29D", "#334D5C"),
                                            ascending = FALSE)
T15_G8_math_bsbg03_shortlist_ScorePlot <- T15_G8_math_bsbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = bsbg03)) +
  geom_point(stat = "identity", size = 4) +
  coord_flip()

# T11_G4_ssci_asbg03 <- edsurveyTable(formula = ssci ~ asbg03, data = TIMSS11_G4,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G4_math_asbg03 <- edsurveyTable(formula = mmat ~ asbg03, data = TIMSS11_G4,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G8_ssci_bsbg03 <- edsurveyTable(formula = ssci ~ bsbg03, data = TIMSS11_G8,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G8_math_bsbg03 <- edsurveyTable(formula = mmat ~ bsbg03, data = TIMSS11_G8,
#                                     jrrIMax = Inf, varMethod = "jackknife")


##### percent & achievement by ITLANG & Language at home ##### 
T15_G4_ssci_itlang_asbg03 <- edsurveyTable(formula = ssci ~ itlang + asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_itlang_asbg03 <- edsurveyTable(formula = mmat ~ itlang + asbg03, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_itlang_bsbg03 <- edsurveyTable(formula = ssci ~ itlang + bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_itlang_bsbg03 <- edsurveyTable(formula = mmat ~ itlang + bsbg03, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")


#keep only countries with more than one itlang
T15_G4_ssci_itlang_asbg03_shortlist <- T15_G4_ssci_itlang_asbg03$data %>% 
  filter(country %in% T15_G4_ssci_countryList) 
T15_G4_math_itlang_asbg03_shortlist <- T15_G4_math_itlang_asbg03$data %>% 
  filter(country %in% T15_G4_math_countryList) 
T15_G8_ssci_itlang_bsbg03_shortlist <- T15_G8_ssci_itlang_bsbg03$data %>% 
  filter(country %in% T15_G8_ssci_countryList) 
T15_G8_math_itlang_bsbg03_shortlist <- T15_G8_math_itlang_bsbg03$data %>% 
  filter(country %in% T15_G8_math_countryList) 

T15_G4_ssci_itlang_asbg03_shortlist_ScorePlot <- T15_G4_ssci_itlang_asbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = asbg03)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()

T15_G4_math_itlang_asbg03_shortlist_ScorePlot <- T15_G4_math_itlang_asbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = asbg03)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()

T15_G8_ssci_itlang_bsbg03_shortlist_ScorePlot <- T15_G8_ssci_itlang_bsbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = bsbg03)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()

T15_G8_math_itlang_bsbg03_shortlist_ScorePlot <- T15_G8_math_itlang_bsbg03_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = bsbg03)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()


# T11_G4_ssci_itlang_asbg03 <- edsurveyTable(formula = ssci ~ itlang + asbg03, data = TIMSS11_G4,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G4_math_itlang_asbg03 <- edsurveyTable(formula = mmat ~ itlang + asbg03, data = TIMSS11_G4,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G8_ssci_itlang_bsbg03 <- edsurveyTable(formula = ssci ~ itlang + bsbg03, data = TIMSS11_G8,
#                                     jrrIMax = Inf, varMethod = "jackknife")
# T11_G8_math_itlang_bsbg03 <- edsurveyTable(formula = mmat ~ itlang + bsbg03, data = TIMSS11_G8,
#                                     jrrIMax = Inf, varMethod = "jackknife")




##### percent & achievement by born in the country [student variable] ##### 
T15_G4_ssci_asbg07 <- edsurveyTable(formula = ssci ~ asbg07, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_asbg07 <- edsurveyTable(formula = mmat ~ asbg07, data = TIMSS15_G4,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_bsbg10a <- edsurveyTable(formula = ssci ~ bsbg10a, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_bsbg10a <- edsurveyTable(formula = mmat ~ bsbg10a, data = TIMSS15_G8,
                                    jrrIMax = Inf, varMethod = "jackknife")
#omittedLevels = FALSE (it does not work in edsurvey, see the IDB Analyzer output for now - conclusion: tbd)


#keep only countries with more than one itlang
T15_G4_ssci_asbg07_shortlist <- T15_G4_ssci_asbg07$data %>% 
  filter(country %in% T15_G4_ssci_countryList) 
T15_G4_ssci_asbg07_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                            data = T15_G4_ssci_asbg07_shortlist,
                                            LikertVar = "asbg07",
                                            byVar = "country",
                                            pal = c("#DF4949",  "#334D5C"),
                                            ascending = FALSE)


T15_G4_math_asbg07_shortlist <- T15_G4_math_asbg07$data %>% 
  filter(country %in% T15_G4_math_countryList) 
T15_G4_math_asbg07_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                            data = T15_G4_math_asbg07_shortlist,
                                            LikertVar = "asbg07",
                                            byVar = "country",
                                            pal = c("#DF4949",  "#334D5C"),
                                            ascending = FALSE)

T15_G8_ssci_bsbg10a_shortlist <- T15_G8_ssci_bsbg10a$data %>% 
  filter(country %in% T15_G8_ssci_countryList) 
T15_G8_ssci_bsbg10a_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                            data = T15_G8_ssci_bsbg10a_shortlist,
                                            LikertVar = "bsbg10a",
                                            byVar = "country",
                                            pal = c("#DF4949",  "#334D5C"),
                                            ascending = FALSE)

T15_G8_math_bsbg10a_shortlist <- T15_G8_math_bsbg10a$data %>% 
  filter(country %in% T15_G8_math_countryList) 
T15_G8_math_bsbg10a_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                             data = T15_G8_math_bsbg10a_shortlist,
                                             LikertVar = "bsbg10a",
                                             byVar = "country",
                                             pal = c("#DF4949",  "#334D5C"),
                                             ascending = FALSE)


##### percent & achievement by ITLANG & born in the country [student variable] ##### 
T15_G4_ssci_itlang_asbg07 <- edsurveyTable(formula = ssci ~ itlang + asbg07, data = TIMSS15_G4,
                                           jrrIMax = Inf, varMethod = "jackknife")
T15_G4_math_itlang_asbg07 <- edsurveyTable(formula = mmat ~ itlang + asbg07, data = TIMSS15_G4,
                                           jrrIMax = Inf, varMethod = "jackknife")
T15_G8_ssci_itlang_bsbg10a <- edsurveyTable(formula = ssci ~ itlang + bsbg10a, data = TIMSS15_G8,
                                           jrrIMax = Inf, varMethod = "jackknife")
T15_G8_math_itlang_bsbg10a <- edsurveyTable(formula = mmat ~ itlang + bsbg10a, data = TIMSS15_G8,
                                           jrrIMax = Inf, varMethod = "jackknife")


#keep only countries with more than one itlang
T15_G4_ssci_itlang_asbg07_shortlist <- T15_G4_ssci_itlang_asbg07$data %>% 
  filter(country %in% T15_G4_ssci_countryList) 
T15_G4_math_itlang_asbg07_shortlist <- T15_G4_math_itlang_asbg07$data %>% 
  filter(country %in% T15_G4_math_countryList) 
T15_G8_ssci_itlang_bsbg10a_shortlist <- T15_G8_ssci_itlang_bsbg10a$data %>% 
  filter(country %in% T15_G8_ssci_countryList) 
T15_G8_math_itlang_bsbg10a_shortlist <- T15_G8_math_itlang_bsbg10a$data %>% 
  filter(country %in% T15_G8_math_countryList) 

T15_G4_ssci_itlang_asbg07_shortlist_ScorePlot <- T15_G4_ssci_itlang_asbg07_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = asbg07)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()

T15_G4_math_itlang_asbg07_shortlist_ScorePlot <- T15_G4_math_itlang_asbg07_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = asbg07)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()

T15_G8_ssci_itlang_bsbg10a_shortlist_ScorePlot <- T15_G8_ssci_itlang_bsbg10a_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = bsbg10a)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()

T15_G8_math_itlang_bsbg10a_shortlist_ScorePlot <- T15_G8_math_itlang_bsbg10a_shortlist %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang, shape = bsbg10a)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_22) +
  coord_flip()







# Export objects as excels

objectList <- list(
  #_itlang
  
  #_language at home
  
  #_itlang_lanage at home
  
  #_born in the country (student var)
  
  #_
  T15_G4_ssci_itlang, T15_G4_math_itlang, T15_G8_ssci_itlang, T15_G8_math_itlang, T11_G4_ssci_itlang, T11_G4_math_itlang, T11_G8_ssci_itlang, T11_G8_math_itlang, T15_G4_ssci_asbg03, T15_G4_math_asbg03, T15_G8_ssci_bsbg03, T15_G8_math_bsbg03, T11_G4_ssci_asbg03, T11_G4_math_asbg03, T11_G8_ssci_bsbg03, T11_G8_math_bsbg03, T15_G4_ssci_itlang_asbg03, T15_G4_math_itlang_asbg03, T15_G8_ssci_itlang_bsbg03, T15_G8_math_itlang_bsbg03, T11_G4_ssci_itlang_asbg03, T11_G4_math_itlang_asbg03, T11_G8_ssci_itlang_bsbg03, T11_G8_math_itlang_bsbg03)


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


























