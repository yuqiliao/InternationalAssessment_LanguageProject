### Explore EdSurvey
### 5/17/18
### Yuqi Liao

# install.packages("EdSurvey")
# install the latest edsurvey on github (to get the latest feature/bug fixes)
# devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

#library(EdSurvey)
library(dplyr)
library(plyr)
library(readr)
library(reshape)
library(ggplot2)

##### Download and load the Data #####
# downloadTIMSS(years = 2015, root = "G:/Conference/2019/Data/TIMSS", cache=FALSE)
# downloadTIMSS(years = 2011, root = "G:/Conference/2019/Data/TIMSS", cache=FALSE)
# # Reading in Data
# define countryList (all G4 countries that has multiple ITLANG levels)
# countryList <- as.character(T15_G4_math_countryList) 
# [1] "Abu Dhabi, UAE"       "Dubai, UAE"           "United Arab Emirates" "Bahrain"             
# [5] "Canada"               "Ontario, Canada"      "Quebec, Canada"       "Spain"               
# [9] "Finland"              "Hong Kong SAR"        "Croatia"              "Ireland"             
# [13] "Kazakhstan"           "Kuwait"               "Lithuania"            "Northern Ireland"    
# [17] "Norway (4th grade)"   "Norway"               "Oman"                 "Qatar"               
# [21] "Saudi Arabia"         "Slovak Republic"      "South Africa"  
countryList <- c("adu",	"qat",	"are",	"aad",	"kwt",	"bhr",	"omn",	"sau",	"kaz",	"hkg",	"can",	"cot",	"cqu",	"usa",	"esp",	"fin",	"hrv",	"irl",	"ltu",	"nir",	"nor",	"no4",	"svk")

TIMSS15_G4<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2015", countries = c("*"), gradeLvl = "4")
TIMSS15_G4_2<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2015", countries = countryList, gradeLvl = "4")

### Regression: mmat ~ asbg03 (3 levels) -----
# create an empty data frame for looping
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
# The following code is for creating a new variable based on an external cross-walk, commenting out here for records
# read in the cross-walk
# x_walk  <- read_csv(file = 'H:/5.2_main/Conferences/CIES 2019/Language/Output/Regression/rq1/cross_walk.csv')
# temp_lsdf$OffLang <- ifelse(temp_lsdf$itlang %in% x_walk[],1,0)
# temp_lsdf$itlang[1] %in% x_walk[x_walk$cnt == temp_lsdf$idcntry[1], "offlang"]
# 
# grep(temp_lsdf$itlang[1], x_walk[x_walk$cnt == temp_lsdf$idcntry[1], "offlang"])
# looping through each datalist to do data manipulation (adding variables)
for (cntl in TIMSS15_G4_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  #another loop to add the OffLang dummy variable (it looks like i cannot do vector computing anymore, confirm with Trang)
  print(nrow(temp_lsdf))
  # The following code is for creating a new variable based on an external cross-walk, commenting out here for records
  # offLang <- x_walk[x_walk$cnt == temp_lsdf$idcntry[i], "offlang"]
  # print(offLang)
  # temp_lsdf$OffLang <- sapply(temp_lsdf$itlang, function(i) {
  #   ifelse(grepl(i,offLang,ignore.case=TRUE),1,0)
  # })
  
  lm_temp <- tryCatch(lm.sdf(formula = mmat ~ asbg03, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("ALWAYS","ALMOST ALWAYS"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
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
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "mmat")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
#dir.create(paste0(functionsavepath,"/rq1"))
write.csv(out, file.path(functionsavepath,"/regression_output_080718/T15_G4_lm_mmat_asbg03.csv"), row.names = FALSE)




### Regression: mmat ~ asbg03 + itsex + asdage (age) -----
# create an empty data frame for looping
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

for (cntl in TIMSS15_G4_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = mmat ~ asbg03 + itsex + asdage, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("ALWAYS","ALMOST ALWAYS"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
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
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "mmat")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
write.csv(out, file.path(functionsavepath,"/regression_output_080718/T15_G4_lm_mmat_asbg03_itsex_asdage.csv"), row.names = FALSE)



### Regression:mmat ~ asbg03 + itsex + asdage (age) + asbg04 (#books) + asbg07 (student born in the country) -----
# create an empty data frame for looping
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

for (cntl in TIMSS15_G4_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = mmat ~ asbg03 + itsex + asdage + asbg04 + asbg07, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("ALWAYS","ALMOST ALWAYS"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
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
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "mmat")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
write.csv(out, file.path(functionsavepath,"/regression_output_080718/T15_G4_lm_mmat_asbg03_itsex_asdage_asbg04_asbg07.csv"), row.names = FALSE)




# TIMSS15_G8<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2015", countries = c("*"), gradeLvl = "8",  verbose = TRUE)
# TIMSS11_G4<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2011", countries = c("*"), gradeLvl = "4", verbose = TRUE)
# TIMSS11_G8<- readTIMSS("G:/Conference/2019/Data/TIMSS/TIMSS2011", countries = c("*"), gradeLvl = "8", verbose = TRUE)

# # Save objects in the environment
# save.image(file = "TIMSS_objects.Rdata")
# load(file = "G:/Conference/2019/Git/InternationalAssessment_LanguageProject/TIMSS_objects.Rdata")


##### quantile regression ##### 
#install.packages("quantreg")
library(quantreg)

# quantile regression - experienment before looping -----
temp_lsdf <- getData(data = TIMSS15_G4_2$datalist[[1]], varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
                     omittedLevels = FALSE, addAttributes = TRUE)

# for categorical variables selection, mannually changes the omitted levels (such as 'NOT APPLICABLE') into NA
for (i in c( "asbg03",  "itlang", "itsex", "asbg04", "asdhedup", "asbg07")) {
  temp_lsdf[,i] <- factor(temp_lsdf[,i], exclude=EdSurvey:::getAttributes(temp_lsdf,"omittedLevels"))
}


# # create a dummy variable "native" based on "asbg06a", "asbg06b", "asbg07" - it's not creating vary balanced output so i decide to just use asbg07 for now
# # function to make it back to a light.edsurvey.data.frame
# reLight <- function(df, lesdf) {
#   attrList <- attributes(lesdf)
#   attrN <- names(attrList)
#   attrN <- attrN[!attrN %in% c("names", "row.names")]
#   for (a in attrN) {
#     attr(df,a) <- attrList[[a]]  
#   }
#   return(df)
# }
# temp_lsdf_2 <- temp_lsdf %>% 
#   mutate(native = as.factor(ifelse(asbg07 == "NO", 0, ifelse(!(asbg06a == "YES" & asbg06b == "YES"), 0, 1)))) %>% 
#   reLight(temp_lsdf)
# summary(temp_lsdf_2$asbg06a)
# summary(temp_lsdf_2$asbg07)
# summary(temp_lsdf_2$native)


# collapse asbg03 into 3 levels, and make "never" as the reference group
temp_lsdf$asbg03_3l <- ifelse(temp_lsdf$asbg03 == "NEVER", 0, 
                              ifelse(temp_lsdf$asbg03 == "SOMETIMES", 1, 2))

temp_lsdf$asbg03_3l <- factor(temp_lsdf$asbg03_3l,
                        levels = c(0, 1, 2),
                         labels = c("NEVER", "SOMETIMES", "ALWAYS or ALMOST ALWAYS"))
summary(temp_lsdf$asbg03_3l)
summary(temp_lsdf$asbg03)

# quantile regression - using the first PV for now
# # try to run it without the weight, results should not be used
# Qall <- rq(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, tau=c(.1, .25, .5, .75, .9), data=temp_lsdf)
# summary(Qall)
# plot(summary(Qall))

Qall_2<- rq(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, tau=c(.1, .25, .5, .75, .9), data=temp_lsdf, weights = totwgt)
summary(Qall_2)
plot(summary(Qall_2))

lm_temp <- lm.sdf(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, data = temp_lsdf,
                  weightVar = 'totwgt', jrrIMax = Inf)

summary(lm_temp)

rq_temp <- rq.sdf(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, tau=c(.1), data = temp_lsdf,
                  weightVar = 'totwgt', jrrIMax = Inf)
summary(rq_temp)
##yl: somehow it
# [1] "United States"
# [1] 10029
# contrasts can be applied only to factors with 2 or more levels[1] "Spain"
# [1] 7764
# [1] "Finland"
# [1] 5015
# [1] "Croatia"
# [1] 3985
# Error in base::backsolve(r, x, k = k, upper.tri = upper.tri, transpose = transpose,  : 
#                            singular matrix in 'backsolve'. First zero in diagonal [14]
#                          In addition: There were 50 or more warnings (use warnings() to see the first 50)
                        
# quantile regression - loop ------
# create an empty data frame for looping
out <- data.frame("IDCNTRY" = character(0),
                  #"CNTRY.NAME" = character(0),
                  "YVar" = character(0),
                  "EqVar" = character(0),
                  "tau" = character(0),
                  "coef" = numeric(0),
                  "se" = numeric(0),
                  "t" = numeric(0),
                  #"dof" = numeric(0),
                  "pVal" = numeric(0),
                  #"weight" = character(0),
                  #"n0" = integer(0),
                  "nUsed" = integer(0))



for (cntl in TIMSS15_G4_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  # for categorical variables selection, change the omitted levels (such as 'NOT APPLICABLE') into NA
  for (i in c( "asbg03",  "itlang", "itsex", "asbg04", "asdhedup", "asbg07")) {
    temp_lsdf[,i] <- factor(temp_lsdf[,i], exclude=EdSurvey:::getAttributes(temp_lsdf,"omittedLevels"))
  }
  # collapse asbg03 into 3 levels, and make "never" as the reference group (because the rq function can't handle those automatically as it is the case with lm.sdf)
  temp_lsdf$asbg03_3l <- ifelse(temp_lsdf$asbg03 == "NEVER", 0, 
                                ifelse(temp_lsdf$asbg03 == "SOMETIMES", 1, 2))
  
  temp_lsdf$asbg03_3l <- factor(temp_lsdf$asbg03_3l,
                                levels = c(0, 1, 2),
                                labels = c("NEVER", "SOMETIMES", "ALWAYS or ALMOST ALWAYS"))
  print(table(temp_lsdf$asbg03_3l, temp_lsdf$asbg03))
  
  #do quantile regressions (use asmmat01 for now)
  
  rq_temp <- tryCatch(rq(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asbg07, tau=c(.1, .25, .5, .75, .9), data=temp_lsdf, weights = totwgt),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  
  out_cnty <- data.frame("IDCNTRY" = character(0),
                         #"CNTRY.NAME" = character(0),
                         "YVar" = character(0),
                         "EqVar" = character(0),
                         "tau" = character(0),
                         "coef" = numeric(0),
                         "se" = numeric(0),
                         "t" = numeric(0),
                         #"dof" = numeric(0),
                         "pVal" = numeric(0),
                         #"weight" = character(0),
                         #"n0" = integer(0),
                         "nUsed" = integer(0))
  for (tau_ in 1:5) { 
    
  if (length(rq_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(rq_temp$coefficients)))
    out_temp$YVar <- "mmat"
    out_temp$EqVar <- row.names(rq_temp$coefficients)
    out_temp$tau <- summary(rq_temp)[[tau_]][["tau"]]   #ask Trang to see if there is a better way? aka i can't figure out how to do it without summary()
    out_temp$coef <- summary(rq_temp)[[tau_]][["coefficients"]][,"Value"]
    out_temp$se <- summary(rq_temp)[[tau_]][["coefficients"]][,"Std. Error"]
    out_temp$t <- summary(rq_temp)[[tau_]][["coefficients"]][,"t value"]
    out_temp$pVal <- summary(rq_temp)[[tau_]][["coefficients"]][,"Pr(>|t|)"]
    out_temp$nUsed <- summary(rq_temp)[[tau_]][["rdf"]]
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "mmat")
  }
  out_cnty <- rbind.fill(out_cnty, out_temp) 
  }
  out <- rbind.fill(out, out_cnty)
}
  


# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
write.csv(out, file.path(functionsavepath,"/regression_output_080718/T15_G4_QR_mmat_asbg03_itsex_asdage_asbg04_asbg07.csv"), row.names = FALSE)



































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
T15_G4_lsdf <- getData(data = TIMSS15_G4, varnames = c( "mmat" , "asbg03",  "itlang", "idcntry"),
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


### Regressions -----

# lm_math_asbg03 <- lm.sdf(formula = mmat ~ asbg03, data = TIMSS15_G4)
# lm_math_asbg03_2 <- lm.sdf(formula = mmat ~ asbg03, data = TIMSS15_G4, 
#                          recode = list(asbg03 = list(from = c("ALWAYS", "ALMOST ALWAYS"),                                                to = c("ALWAYS/ALMOST ALWAYS")),
#                                        asbg03 = list(from = c("SOMETIMES", "NEVER"),                                                     to = c("SOMETIMES/NEVER"))))

TIMSS15_G4_2_lsdf_allcountry_2
lm_math_asbg03 <- lm.sdf(formula = mmat ~ asbg03, data = TIMSS15_G4_2_lsdf)




### modify Trang's code
#pisa2012 <- readPISA(filepath = paste0(datapath, 2012), database = "INT", cognitive = "score", countries = "*")

# mmat ~ asbg03 (3 levels) -----
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
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
dir.create(paste0(functionsavepath,"/rq1"))
#file = file(paste0(functionsavepath,"/rq1/regression_error.txt"), open = "wt")
#sink(file, type = "message", append = T)
library(plyr)
for (ci in 1:length(TIMSS15_G4$data)) {
  cat("\nCountry ")
  cat(labels(TIMSS15_G4$data)[ci])
  lm_temp <- tryCatch(lm.sdf(mmat ~ asbg03, data = TIMSS15_G4$datalist[[ci]],
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("ALWAYS","ALMOST ALWAYS"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
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
    out_temp <- data.frame("IDCNTRY" = TIMSS15_G4$datalist[[ci]]$country, "YVar" = "mmat")
  }
  out <- rbind.fill(out, out_temp)
}
sink(type = "message")
#dir.create(paste0(functionsavepath,"_rq1"))

#keep rows for the countires of interest
out <- out %>% 
  filter(IDCNTRY %in% T15_G4_math_countryList)


write.csv(out, file.path(functionsavepath,"/rq1/T15_G4_lm_mmat_asbg03_3levels.csv"), row.names = FALSE)
#readr::write_csv(out, paste0(functionsavepath,"/regression/PISA2012_MATH vs st25q01 n sc03q01 n ic02q07 n ec06q01.csv"))
#closeAllConnections()


### more regressions requires working with light.edsurvey.data.frame
TIMSS15_G4_2_lsdf


























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


























