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

T15_G4_math_asbg03_shortlist_plot <- likert(sdf = TIMSS15_G4,
                                            data = T15_G4_math_asbg03_shortlist,
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


### Check if ITLANG is unique for a given CNT and idschool -----
T15_G4_lsdf <- getData(data = TIMSS15_G4, varnames = c("itsex", "itlang", "asbg07", "idcntry", "idschool"),
                 omittedLevels = TRUE, addAttributes = TRUE)

#T15_G4_lsdf is a list by idcntry; now i'm creating a master data frame that has data for all countries.
T15_G4_lsdf_allcountry <- do.call(rbind.light.edsurvey.data.frame, T15_G4_lsdf)

# check if ITLANG is unique
T15_G4_countryList %>% distinct(country) %>% level()
as.numeric(T15_G4_countryList$country)

T15_G4_lsdf_allcountry %>% 
  group_by(idcntry, idschool) %>% 
  summarize(n_unique_itlang  = n_distinct(itlang)) %>% 
  arrange(desc(n_unique_itlang)) %>% 
  View()

glimpse(T15_G4_lsdf_allcountry)
T15_G4_lsdf[[1]] %>% 
  group_by()
  distinct()

attr(T15_G4_lsdf_allcountry)


### Regressions
lm_math_asbg03 <- lm.sdf(formula = mmat ~ asbg03, data = TIMSS15_G4)
lm_math_asbg03_2 <- lm.sdf(formula = mmat ~ asbg03, data = TIMSS15_G4, 
                         recode = list(asbg03 = list(from = c("ALWAYS", "ALMOST ALWAYS"),                                                to = c("ALWAYS/ALMOST ALWAYS")),
                                       asbg03 = list(from = c("SOMETIMES", "NEVER"),                                                     to = c("SOMETIMES/NEVER"))))
summary(lm_math_itlang[[3]])


### modify Trang's code
#pisa2012 <- readPISA(filepath = paste0(datapath, 2012), database = "INT", cognitive = "score", countries = "*")
out <- data.frame("IDCNTRY" = character(0),
                  #"CNTRY.NAME" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  "dof" = numeric(0),
                  "pVal" = numeric(0),
                  "weight" = character(0),
                  "n0" = integer(0),
                  "nUsed" = integer(0)
)
# mmat ~ asbg03
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
dir.create(paste0(functionsavepath,"/rq1"))
#file = file(paste0(functionsavepath,"/rq1/regression_error.txt"), open = "wt")
#sink(file, type = "message", append = T)
library(plyr)
for (ci in 1:length(TIMSS15_G4$data)) {
  cat("\nCountry ")
  cat(labels(TIMSS15_G4$data)[ci])
  lm_temp <- tryCatch(lm.sdf(mmat ~ asbg03, data = TIMSS15_G4$datalist[[ci]], weightVar = 'totwgt', jrrIMax = Inf),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(TIMSS15_G4$datalist[[ci]]$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "mmat"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
  } else {
    out_temp <- data.frame("IDCNTRY" = pisa2012$datalist[[ci]]$country, "YVar" = "mmat")
  }
  out <- rbind.fill(out, out_temp)
}
sink(type = "message")
#dir.create(paste0(functionsavepath,"_rq1"))
write.csv(out, file.path(functionsavepath,"/rq1/T15_G4_lm_mmat_asbg03.csv"), row.names = FALSE)
#readr::write_csv(out, paste0(functionsavepath,"/regression/PISA2012_MATH vs st25q01 n sc03q01 n ic02q07 n ec06q01.csv"))


sink()
sink(type="message")
closeAllConnections()




























### Export objects as excels -----

objectList <- list(
  #_itlang
  T15_G4_ssci_itlang_shortlist, T15_G4_math_itlang_shortlist, T15_G8_ssci_itlang_shortlist, T15_G8_math_itlang_shortlist,
  #_language at home
  T15_G4_ssci_asbg03_shortlist, T15_G4_math_asbg03_shortlist, T15_G8_ssci_bsbg03_shortlist, T15_G8_math_bsbg03_shortlist,
  #_itlang_lanage at home
  T15_G4_ssci_itlang_asbg03_shortlist, T15_G4_math_itlang_asbg03_shortlist, T15_G8_ssci_itlang_bsbg03_shortlist, T15_G8_math_itlang_bsbg03_shortlist,
  #_born in the country (student var)
  T15_G4_ssci_asbg07_shortlist, T15_G4_math_asbg07_shortlist, T15_G8_ssci_bsbg10a_shortlist, T15_G8_math_bsbg10a_shortlist, 
  #_itlang_born in the country
  T15_G4_ssci_itlang_asbg07_shortlist,  T15_G4_math_itlang_asbg07_shortlist, T15_G8_ssci_itlang_bsbg10a_shortlist, T15_G8_math_itlang_bsbg10a_shortlist)


names(objectList) <- c(
  #_itlang
  "T15_G4_ssci_itlang_shortlist", "T15_G4_math_itlang_shortlist", "T15_G8_ssci_itlang_shortlist", "T15_G8_math_itlang_shortlist",
  #_language at home
  "T15_G4_ssci_asbg03_shortlist", "T15_G4_math_asbg03_shortlist", "T15_G8_ssci_bsbg03_shortlist", "T15_G8_math_bsbg03_shortlist",
  #_itlang_lanage at home
  "T15_G4_ssci_itlang_asbg03_shortlist", "T15_G4_math_itlang_asbg03_shortlist", "T15_G8_ssci_itlang_bsbg03_shortlist", "T15_G8_math_itlang_bsbg03_shortlist",
  #_born in the country (student var)
  "T15_G4_ssci_asbg07_shortlist", "T15_G4_math_asbg07_shortlist", "T15_G8_ssci_bsbg10a_shortlist", "T15_G8_math_bsbg10a_shortlist", 
  #_itlang_born in the country
  "T15_G4_ssci_itlang_asbg07_shortlist",  "T15_G4_math_itlang_asbg07_shortlist", "T15_G8_ssci_itlang_bsbg10a_shortlist", "T15_G8_math_itlang_bsbg10a_shortlist")

i <- 1
for (object in objectList){
  write_csv(object, path = paste0(names(objectList)[[i]], ".csv"))
  i <- i + 1
}
rm(i)



PlotList <- list(
  #_itlang
  T15_G4_ssci_asbg03_shortlist_plot, T15_G4_math_itlang_shortlist_ScorePlot, T15_G8_ssci_itlang_shortlist_ScorePlot, T15_G8_math_itlang_shortlist_ScorePlot,
  #_language at home
  T15_G4_ssci_asbg03_shortlist_plot, T15_G4_ssci_asbg03_shortlist_ScorePlot, T15_G4_math_asbg03_shortlist_plot, T15_G4_math_asbg03_shortlist_ScorePlot,T15_G8_ssci_bsbg03_shortlist_plot, T15_G8_ssci_bsbg03_shortlist_ScorePlot, T15_G8_math_bsbg03_shortlist_plot, T15_G8_math_bsbg03_shortlist_ScorePlot,
  #_itlang_lanage at home
  T15_G4_ssci_itlang_asbg03_shortlist_ScorePlot, T15_G4_math_itlang_asbg03_shortlist_ScorePlot, T15_G8_ssci_itlang_bsbg03_shortlist_ScorePlot, T15_G8_math_itlang_bsbg03_shortlist_ScorePlot,
  #_born in the country (student var)
  
  #_itlang_born in the country
  T15_G4_ssci_itlang_asbg07_shortlist_ScorePlot,  T15_G4_math_itlang_asbg07_shortlist_ScorePlot, T15_G8_ssci_itlang_bsbg10a_shortlist_ScorePlot, T15_G8_math_itlang_bsbg10a_shortlist_ScorePlot)


names_of_plots <- c(
  #_itlang
  "T15_G4_ssci_asbg03_shortlist_plot", "T15_G4_math_itlang_shortlist_ScorePlot", "T15_G8_ssci_itlang_shortlist_ScorePlot", "T15_G8_math_itlang_shortlist_ScorePlot",
  #_language at home
  "T15_G4_ssci_asbg03_shortlist_plot", "T15_G4_ssci_asbg03_shortlist_ScorePlot", "T15_G4_math_asbg03_shortlist_plot", "T15_G4_math_asbg03_shortlist_ScorePlot","T15_G8_ssci_bsbg03_shortlist_plot", "T15_G8_ssci_bsbg03_shortlist_ScorePlot", "T15_G8_math_bsbg03_shortlist_plot", "T15_G8_math_bsbg03_shortlist_ScorePlot",
  #_itlang_lanage at home
  "T15_G4_ssci_itlang_asbg03_shortlist_ScorePlot", "T15_G4_math_itlang_asbg03_shortlist_ScorePlot", "T15_G8_ssci_itlang_bsbg03_shortlist_ScorePlot", "T15_G8_math_itlang_bsbg03_shortlist_ScorePlot",
  #_born in the country (student var)
  
  #_itlang_born in the country
  "T15_G4_ssci_itlang_asbg07_shortlist_ScorePlot",  "T15_G4_math_itlang_asbg07_shortlist_ScorePlot", "T15_G8_ssci_itlang_bsbg10a_shortlist_ScorePlot", "T15_G8_math_itlang_bsbg10a_shortlist_ScorePlot")



#one way to export all graphs (not as ideal)
pdf("all.pdf")
invisible(lapply(PlotList, print))
dev.off()

#another way to export all graphs (works great!)
invisible(mapply(ggsave, file=paste0(names_of_plots, ".pdf"), plot=PlotList))





















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


























