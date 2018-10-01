### Language Project - PIRLS DATA!
### 8/23/18
### Yuqi Liao

devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

#library(EdSurvey)
library(dplyr)
library(plyr)
library(readr)
library(reshape)
library(ggplot2)

##### Download and load the Data #####
#downloadPIRLS(years = 2016, root = "G:/Conference/2019/Data/PIRLS", cache=FALSE)

# # Reading in Data
# define countryList (all countries that has multiple ITLANG levels + the US)
# as.character(P15_read_itlang_shortlist_countryList$country)
# [1] "Abu Dhabi, UAE"       "Dubai, UAE"           "United Arab Emirates" "Azerbaijan"          
# [5] "Bahrain"              "Canada"               "Ontario, Canada"      "Quebec, Canada"      
# [9] "Spain"                "Finland"              "Georgia"              "Israel"              
# [13] "Kazakhstan"           "Kuwait"               "Lithuania"            "Latvia"              
# [17] "Macao SAR"            "Norway (Grade 4)"     "Norway"               "New Zealand"         
# [21] "Oman"                 "Qatar"                "Saudi Arabia"         "Slovak Republic"     
# [25] "Eng Afr Zulu RSA (5)" "South Africa"  

# There are 26 education systems that has more than two itlang, but excluding new zealand here because it doesn't have value for the key independent variable asbg03.
itlang_countryList <- c("aad", "adu", "are", "aze", "bhr", "can", "cot",	"cqu", "esp", "fin", "geo", "isr", "kaz", "kwt", "ltu", "lva", "mac", "no4", "nor", "omn", "qat", "sau", "svk", "za5", "zaf")
usa_countryList <- "usa"

PIRLS15 <- readPIRLS("G:/Conference/2019/Data/PIRLS/PIRLS/2016", countries = c("*"))
PIRLS15_1<- readPIRLS("G:/Conference/2019/Data/PIRLS/PIRLS/2016", countries = itlang_countryList)
PIRLS15_2<- readPIRLS("G:/Conference/2019/Data/PIRLS/PIRLS/2016", countries = c(itlang_countryList, usa_countryList))


### Regression: rrea ~ asbg03 (3 levels) -----
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
                  "nUsed" = integer(0),
                  "r2" = integer(0))
# The following code is for creating a new variable based on an external cross-walk, commenting out here for records
# read in the cross-walk
# x_walk  <- read_csv(file = 'H:/5.2_main/Conferences/CIES 2019/Language/Output/Regression/rq1/cross_walk.csv')
# temp_lsdf$OffLang <- ifelse(temp_lsdf$itlang %in% x_walk[],1,0)
# temp_lsdf$itlang[1] %in% x_walk[x_walk$cnt == temp_lsdf$idcntry[1], "offlang"]
# 
# grep(temp_lsdf$itlang[1], x_walk[x_walk$cnt == temp_lsdf$idcntry[1], "offlang"])
# looping through each datalist to do data manipulation (adding variables)
for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  #another loop to add the OffLang dummy variable (it looks like i cannot do vector computing anymore, confirm with Trang)
  print(nrow(temp_lsdf))
  # The following code is for creating a new variable based on an external cross-walk, commenting out here for records
  # offLang <- x_walk[x_walk$cnt == temp_lsdf$idcntry[i], "offlang"]
  # print(offLang)
  # temp_lsdf$OffLang <- sapply(temp_lsdf$itlang, function(i) {
  #   ifelse(grepl(i,offLang,ignore.case=TRUE),1,0)
  # })
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
#dir.create(paste0(functionsavepath,"/rq1"))
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03.csv"), row.names = FALSE)



### Regression: rrea ~ asbg03 + itlang -----
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
                  "nUsed" = integer(0),
                  "r2" = integer(0))
for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itlang, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}
# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itlang.csv"), row.names = FALSE)


### Regression: rrea ~ asbg03 + itsex + asdage (age) -----
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
                  "nUsed" = integer(0),
                  "r2" = integer(0))
for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itsex + asdage, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}
# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itsex_asdage.csv"), row.names = FALSE)



### Regression: rrea ~ asbg03 + itsex + asdage (age) + asbg04 (#books) + asdg05s (# of home study supports) -----
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
                  "nUsed" = integer(0),
                  "r2" = integer(0))

for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a", "asdg05s"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itsex + asdage + asbg04 + asdg05s, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itsex_asdage_asbg04_asdg05s.csv"), row.names = FALSE)



### Regression: rrea ~ asbg03 + itsex + asdage (age) + asbg04 (#books) + asdg05s (# of home study supports) + asbh14 (# of children't book [parent]) + asdhedup (highest edu [parent]) asdhoccp (highest occupation [parent])-----
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
                  "nUsed" = integer(0),
                  "r2" = integer(0))
for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a", "asdg05s", "asbh14", "asdhedup", "asdhoccp"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itsex + asdage + asbg04 + asdg05s + asbh14 + asdhedup + asdhoccp, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itsex_asdage_asbg04_asdg05s_asbh14_asdhedup_asdhoccp.csv"), row.names = FALSE)


### Regression: rrea ~ asbg03 + itsex + asdage (age) + asbghrl (home resources for learning/scl) -----
#asbghrl is derived from asbg04 (#books) + asdg05s (# of home study supports) + asbh14 (# of children't book [parent]) + asdhedup (highest edu [parent]) + asdhoccp (highest occupation [parent])

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
                  "nUsed" = integer(0),
                  "r2" = integer(0))
for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a", "asdg05s", "asbh14", "asdhedup", "asdhoccp"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itsex + asdage + asbghrl, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itsex_asdage_asbghrl.csv"), row.names = FALSE)


### Regression: rrea ~ asbg03 + itsex + asdage (age) + asdghrl (home resources for learning/idx) -----
#asdghrl is derived from asbg04 (#books) + asdg05s (# of home study supports) + asbh14 (# of children't book [parent]) + asdhedup (highest edu [parent]) asdhoccp (highest occupation [parent])

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
                  "nUsed" = integer(0),
                  "r2" = integer(0))

for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a", "asdg05s", "asbh14", "asdhedup", "asdhoccp", "asdghrl"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itsex + asdage + asdghrl, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itsex_asdage_asdghrl.csv"), row.names = FALSE)




### Regression: rrea ~ asbg03 + itsex + asdage (age) + asbghrl (home resources for learning/scl) + asbh03a (child born in this country[parent]) + itlang -----
#asbghrl is derived from asbg04 (#books) + asdg05s (# of home study supports) + asbh14 (# of children't book [parent]) + asdhedup (highest edu [parent]) asdhoccp (highest occupation [parent])

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
                  "nUsed" = integer(0),
                  "r2" = integer(0))

for (cntl in PIRLS15_2$datalist) {
  print(cntl$country)
  temp_lsdf <- getData(data = cntl, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asbghrl", "asbh03a", "asdg05s", "asbh14", "asdhedup", "asdhoccp", "asdghrl"),
                       omittedLevels = FALSE, addAttributes = TRUE)
  print(nrow(temp_lsdf))
  
  lm_temp <- tryCatch(lm.sdf(formula = rrea ~ asbg03 + itsex + asdage + asbghrl + asbh03a + itlang, data = temp_lsdf,
                             weightVar = 'totwgt', jrrIMax = Inf,
                             recode = list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                                           asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                                           asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS"))),
                             relevels = list(asbg03 = "NEVER")),
                      error = function(cond) {
                        message(cond)
                        return(0)
                      })
  if (length(lm_temp) != 1) {
    out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(lm_temp$coefmat)))
    out_temp$YVar <- "rrea"
    out_temp$EqVar <- row.names(lm_temp$coefmat)
    out_temp$coef <- lm_temp$coefmat$coef
    out_temp$se <- lm_temp$coefmat$se
    out_temp$t <- lm_temp$coefmat$t
    out_temp$dof <- lm_temp$coefmat$dof
    out_temp$pVal <- lm_temp$coefmat$`Pr(>|t|)`
    out_temp$weight <- lm_temp$weight
    out_temp$n0 <- lm_temp$n0
    out_temp$nUsed <- lm_temp$nUsed
    out_temp$r2 <- lm_temp$r.squared
  } else {
    out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "rrea")
  }
  out <- rbind.fill(out, out_temp)
}

# export
functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/Regression'
write.csv(out, file.path(functionsavepath,"P15_lm_rrea_asbg03_itsex_asdage_asbghrl_asbh03a_itlang.csv"), row.names = FALSE)





# # Save objects in the environment
# save.image(file = "TIMSS_objects.Rdata")
# load(file = "G:/Conference/2019/Git/InternationalAssessment_LanguageProject/TIMSS_objects.Rdata")






# commented out because the following code was first used in TIMSS, though the method below didn't use all 5 PVs. I then used STATA to do the quantile regression.

# ##### quantile regression ##### 
# #install.packages("quantreg")
# library(quantreg)
# 
# # quantile regression - experienment before looping -----
# temp_lsdf <- getData(data = TIMSS15_G4_2$datalist[[1]], varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
#                      omittedLevels = FALSE, addAttributes = TRUE)
# 
# # for categorical variables selection, mannually changes the omitted levels (such as 'NOT APPLICABLE') into NA
# for (i in c( "asbg03",  "itlang", "itsex", "asbg04", "asdhedup", "asbg07")) {
#   temp_lsdf[,i] <- factor(temp_lsdf[,i], exclude=EdSurvey:::getAttributes(temp_lsdf,"omittedLevels"))
# }
# 
# 
# # # create a dummy variable "native" based on "asbg06a", "asbg06b", "asbg07" - it's not creating vary balanced output so i decide to just use asbg07 for now
# # # function to make it back to a light.edsurvey.data.frame
# # reLight <- function(df, lesdf) {
# #   attrList <- attributes(lesdf)
# #   attrN <- names(attrList)
# #   attrN <- attrN[!attrN %in% c("names", "row.names")]
# #   for (a in attrN) {
# #     attr(df,a) <- attrList[[a]]  
# #   }
# #   return(df)
# # }
# # temp_lsdf_2 <- temp_lsdf %>% 
# #   mutate(native = as.factor(ifelse(asbg07 == "NO", 0, ifelse(!(asbg06a == "YES" & asbg06b == "YES"), 0, 1)))) %>% 
# #   reLight(temp_lsdf)
# # summary(temp_lsdf_2$asbg06a)
# # summary(temp_lsdf_2$asbg07)
# # summary(temp_lsdf_2$native)
# 
# 
# # collapse asbg03 into 3 levels, and make "never" as the reference group
# temp_lsdf$asbg03_3l <- ifelse(temp_lsdf$asbg03 == "NEVER", 0, 
#                               ifelse(temp_lsdf$asbg03 == "SOMETIMES", 1, 2))
# 
# temp_lsdf$asbg03_3l <- factor(temp_lsdf$asbg03_3l,
#                         levels = c(0, 1, 2),
#                          labels = c("NEVER", "SOMETIMES", "ALWAYS or ALMOST ALWAYS"))
# summary(temp_lsdf$asbg03_3l)
# summary(temp_lsdf$asbg03)
# 
# # quantile regression - using the first PV for now
# # # try to run it without the weight, results should not be used
# # Qall <- rq(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, tau=c(.1, .25, .5, .75, .9), data=temp_lsdf)
# # summary(Qall)
# # plot(summary(Qall))
# 
# Qall_2<- rq(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, tau=c(.1, .25, .5, .75, .9), data=temp_lsdf, weights = totwgt)
# summary(Qall_2)
# plot(summary(Qall_2))
# 
# lm_temp <- lm.sdf(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, data = temp_lsdf,
#                   weightVar = 'totwgt', jrrIMax = Inf)
# 
# summary(lm_temp)
# 
# rq_temp <- rq.sdf(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asdhedup + asbg07, tau=c(.1), data = temp_lsdf,
#                   weightVar = 'totwgt', jrrIMax = Inf)
# summary(rq_temp)
# ##yl: somehow it
# # [1] "United States"
# # [1] 10029
# # contrasts can be applied only to factors with 2 or more levels[1] "Spain"
# # [1] 7764
# # [1] "Finland"
# # [1] 5015
# # [1] "Croatia"
# # [1] 3985
# # Error in base::backsolve(r, x, k = k, upper.tri = upper.tri, transpose = transpose,  : 
# #                            singular matrix in 'backsolve'. First zero in diagonal [14]
# #                          In addition: There were 50 or more warnings (use warnings() to see the first 50)
#                         
# # quantile regression - loop ------
# # create an empty data frame for looping
# out <- data.frame("IDCNTRY" = character(0),
#                   #"CNTRY.NAME" = character(0),
#                   "YVar" = character(0),
#                   "EqVar" = character(0),
#                   "tau" = character(0),
#                   "coef" = numeric(0),
#                   "se" = numeric(0),
#                   "t" = numeric(0),
#                   #"dof" = numeric(0),
#                   "pVal" = numeric(0),
#                   #"weight" = character(0),
#                   #"n0" = integer(0),
#                   "nUsed" = integer(0))
# 
# 
# 
# for (cntl in TIMSS15_G4_2$datalist) {
#   print(cntl$country)
#   temp_lsdf <- getData(data = cntl, varnames = c( "mmat" , "asbg03",  "itlang", "idcntry", "totwgt", "itsex", "asdage", "asbg04", "asdhedup", "asbg07"),
#                        omittedLevels = FALSE, addAttributes = TRUE)
#   print(nrow(temp_lsdf))
#   
#   # for categorical variables selection, change the omitted levels (such as 'NOT APPLICABLE') into NA
#   for (i in c( "asbg03",  "itlang", "itsex", "asbg04", "asdhedup", "asbg07")) {
#     temp_lsdf[,i] <- factor(temp_lsdf[,i], exclude=EdSurvey:::getAttributes(temp_lsdf,"omittedLevels"))
#   }
#   # collapse asbg03 into 3 levels, and make "never" as the reference group (because the rq function can't handle those automatically as it is the case with lm.sdf)
#   temp_lsdf$asbg03_3l <- ifelse(temp_lsdf$asbg03 == "NEVER", 0, 
#                                 ifelse(temp_lsdf$asbg03 == "SOMETIMES", 1, 2))
#   
#   temp_lsdf$asbg03_3l <- factor(temp_lsdf$asbg03_3l,
#                                 levels = c(0, 1, 2),
#                                 labels = c("NEVER", "SOMETIMES", "ALWAYS or ALMOST ALWAYS"))
#   print(table(temp_lsdf$asbg03_3l, temp_lsdf$asbg03))
#   
#   #do quantile regressions (use asmmat01 for now)
#   
#   rq_temp <- tryCatch(rq(formula = asmmat01 ~ asbg03_3l + itsex + asdage + asbg04 + asbg07, tau=c(.1, .25, .5, .75, .9), data=temp_lsdf, weights = totwgt),
#                       error = function(cond) {
#                         message(cond)
#                         return(0)
#                       })
#   
#   out_cnty <- data.frame("IDCNTRY" = character(0),
#                          #"CNTRY.NAME" = character(0),
#                          "YVar" = character(0),
#                          "EqVar" = character(0),
#                          "tau" = character(0),
#                          "coef" = numeric(0),
#                          "se" = numeric(0),
#                          "t" = numeric(0),
#                          #"dof" = numeric(0),
#                          "pVal" = numeric(0),
#                          #"weight" = character(0),
#                          #"n0" = integer(0),
#                          "nUsed" = integer(0))
#   for (tau_ in 1:5) { 
#     
#   if (length(rq_temp) != 1) {
#     out_temp <- data.frame("IDCNTRY" = rep(cntl$country, nrow(rq_temp$coefficients)))
#     out_temp$YVar <- "mmat"
#     out_temp$EqVar <- row.names(rq_temp$coefficients)
#     out_temp$tau <- summary(rq_temp)[[tau_]][["tau"]]   #ask Trang to see if there is a better way? aka i can't figure out how to do it without summary()
#     out_temp$coef <- summary(rq_temp)[[tau_]][["coefficients"]][,"Value"]
#     out_temp$se <- summary(rq_temp)[[tau_]][["coefficients"]][,"Std. Error"]
#     out_temp$t <- summary(rq_temp)[[tau_]][["coefficients"]][,"t value"]
#     out_temp$pVal <- summary(rq_temp)[[tau_]][["coefficients"]][,"Pr(>|t|)"]
#     out_temp$nUsed <- summary(rq_temp)[[tau_]][["rdf"]]
#   } else {
#     out_temp <- data.frame("IDCNTRY" = cntl$country, "YVar" = "mmat")
#   }
#   out_cnty <- rbind.fill(out_cnty, out_temp) 
#   }
#   out <- rbind.fill(out, out_cnty)
# }
#   
# 
# 
# # export
# functionsavepath <- '//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/Regression'
# write.csv(out, file.path(functionsavepath,"/regression_output_080718/T15_G4_QR_mmat_asbg03_itsex_asdage_asbg04_asbg07.csv"), row.names = FALSE)



##### percent & achievement by ITLANG ##### 
P15_read_itlang <- edsurveyTable(formula = rrea ~ itlang, data = PIRLS15_2,
                                    jrrIMax = Inf, varMethod = "jackknife")

P15_allCountries_read_itlang <- edsurveyTable(formula = rrea ~ itlang, data = PIRLS15,
                                 jrrIMax = Inf, varMethod = "jackknife")

# #keep only countries with more than one itlang
# P15_read_itlang_shortlist <- P15_allCountries_read_itlang$data %>% 
#   group_by(country) %>% 
#   dplyr::mutate(n_itlang = n()) %>% 
#   filter(n_itlang > 1) 
# 
# P15_read_itlang_shortlist_countryList <- P15_read_itlang_shortlist %>% 
#   distinct(n_itlang) 
# 
# P15_read_itlang_shortlist_countryList$country  #26 countries

#define 35 colors (the first two are for English and Arabic)
color_35 <- c("#2166ac", "#b2182b",rep("#cef2c1", 33))

P15_read_itlang_ScorePlot <- P15_read_itlang$data %>% 
  ggplot(aes(x = country, y = MEAN, color = itlang)) +
  geom_point(stat = "identity", size = 4) +
  scale_color_manual(values = color_35) +
  coord_flip()


##### percent & achievement by Language at home ##### 
P15_read_asbg03 <- edsurveyTable(formula = rrea ~ asbg03, data = PIRLS15_2,
                                    jrrIMax = Inf, varMethod = "jackknife")

recode_condition <- list(asbg03=list(from=c("I NEVER SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("NEVER")),
                         asbg03=list(from=c("I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME"), to=c("SOMETIMES")),
                         asbg03=list(from=c("I ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME","I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME"), to=c("ALWAYS or ALMOST ALWAYS")))

P15_read_asbg03_3 <- edsurveyTable(formula = rrea ~ asbg03, data = PIRLS15_2,
                                 jrrIMax = Inf, varMethod = "jackknife",
                                 recode = recode_condition)

# export P15_read_asbg03_3 as untidy data (for excel data viz)
N <- P15_read_asbg03_3$data %>% 
  select(country, asbg03, N) %>% 
  spread(asbg03,  N) %>% 
  dplyr::rename(N_N = NEVER,
         N_S = SOMETIMES,
         N_A = `ALWAYS or ALMOST ALWAYS`)
PCT <- P15_read_asbg03_3$data %>% 
  select(country, asbg03, PCT) %>% 
  spread(asbg03,  PCT) %>% 
  dplyr::rename(PCT_N = NEVER,
                PCT_S = SOMETIMES,
                PCT_A = `ALWAYS or ALMOST ALWAYS`)
PCT_SE <- P15_read_asbg03_3$data %>% 
  select(country, asbg03, `SE(PCT)`) %>% 
  spread(asbg03,  `SE(PCT)`) %>% 
  dplyr::rename(PCT_SE_N = NEVER,
                PCT_SE_S = SOMETIMES,
                PCT_SE_A = `ALWAYS or ALMOST ALWAYS`)
MEAN <- P15_read_asbg03_3$data %>% 
  select(country, asbg03, MEAN) %>% 
  spread(asbg03,  MEAN) %>% 
  dplyr::rename(MEAN_N = NEVER,
                MEAN_S = SOMETIMES,
                MEAN_A = `ALWAYS or ALMOST ALWAYS`)
MEAN_SE <- P15_read_asbg03_3$data %>% 
  select(country, asbg03, `SE(MEAN)`) %>% 
  spread(asbg03,  `SE(MEAN)`) %>% 
  dplyr::rename(MEAN_SE_N = NEVER,
                MEAN_SE_S = SOMETIMES,
                MEAN_SE_A = `ALWAYS or ALMOST ALWAYS`)

P15_read_asbg03_3_wide <- bind_cols(N, PCT, PCT_SE, MEAN, MEAN_SE)



P15_read_asbg03_plot <- likert(sdf = PIRLS15,
                                            data = P15_read_asbg03$data,
                                            LikertVar = "asbg03",
                                            byVar = "country",
                                            pal = c("#DF4949", "#E27A3F","#45B29D", "#334D5C"),
                                            ascending = FALSE)

P15_read_asbg03_3_plot <- likert(sdf = PIRLS15_2,
                               data = P15_read_asbg03_3$data,
                               LikertVar = "asbg03",
                               byVar = "country",
                               pal = c("#334D5C","#45B29D", "#DF4949"),
                               ascending = FALSE)

P15_read_asbg03_ScorePlot <- P15_read_asbg03$data %>% 
  ggplot(aes(x = country, y = MEAN, color = asbg03)) +
  geom_point(stat = "identity", size = 4) +
  coord_flip()

P15_read_asbg03_3_ScorePlot <- P15_read_asbg03_3$data %>% 
  ggplot(aes(x = country, y = MEAN, color = asbg03)) +
  geom_point(stat = "identity", size = 4) +
  coord_flip()


##### percent & achievement by ITLANG & Language at home ##### 
P15_read_itlang_asbg03_3 <- edsurveyTable(formula = rrea ~ itlang + asbg03, data = PIRLS15_2,
                                    jrrIMax = Inf, varMethod = "jackknife",
                                    recode = recode_condition)
P15_read_asbg03_3_itlang <- edsurveyTable(formula = rrea ~ asbg03 + itlang, data = PIRLS15_2,
                                        jrrIMax = Inf, varMethod = "jackknife",
                                        recode = recode_condition)


##### percent & achievement by child born in the country (parent item) ##### 
P15_read_itlang_asbh03a <- edsurveyTable(formula = rrea ~ itlang + asbh03a, data = PIRLS15_2,
                                        jrrIMax = Inf, varMethod = "jackknife")

P15_read_asbh03a <- edsurveyTable(formula = rrea ~ asbh03a, data = PIRLS15_2,
                                  jrrIMax = Inf, varMethod = "jackknife")


### Check if ITLANG is unique for a given CNT and idschool -----
P15_lsdf <- getData(data = PIRLS15_2, varnames = c( "rrea" , "asbg03",  "itlang", "idcntry", "idschool"),
                 omittedLevels = TRUE, addAttributes = TRUE)

#P15_lsdf is a list by idcntry; now i'm creating a master data frame that has data for all countries.
P15_lsdf_allcountry <- do.call(rbind.light.edsurvey.data.frame, P15_lsdf)

# check if ITLANG is unique at the idschool level
P15_lsdf_allcountry %>% 
  group_by(idcntry, idschool) %>% 
  dplyr::summarize(n_unique_itlang  = n_distinct(itlang)) %>% 
  arrange(desc(n_unique_itlang)) %>% 
  View()

## only a few schools in a few countries has more than one itlang: idcntry %in% c(31, 268, 246, 398, 428, 440, 578, 710, 724, 5784, 7105)

### Export objects as excels -----

objectList <- list(
  #_itlang
  P15_read_itlang$data, P15_allCountries_read_itlang$data, 
  #_language at home
  P15_read_asbg03$data, P15_read_asbg03_3$data,
  #_itlang_lanage at home
  P15_read_itlang_asbg03$data, P15_read_itlang_asbg03_3$data, P15_read_asbg03_3_itlang$data,
  #_born in the country (parent var)
  P15_read_itlang_asbh03a$data, P15_read_asbh03a$data)


names(objectList) <- c(
  #_itlang
  "P15_read_itlang", "P15_allCountries_read_itlang", 
  #_language at home
  "P15_read_asbg03", "P15_read_asbg03_3",
  #_itlang_lanage at home
  "P15_read_itlang_asbg03",  "P15_read_itlang_asbg03_3", "P15_read_asbg03_3_itlang",
  #_born in the country (parent var)
  "P15_read_itlang_asbh03a", "P15_read_asbh03a")

i <- 1
for (object in objectList){
  write_csv(object, path = paste0("//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/CrossTabs/", names(objectList)[[i]], ".csv"))
  i <- i + 1
}
rm(i)



PlotList <- list(
  #_itlang
  P15_read_itlang_ScorePlot,
  #_language at home
  P15_read_asbg03_plot, P15_read_asbg03_3_plot, P15_read_asbg03_ScorePlot, P15_read_asbg03_3_ScorePlot)


names_of_plots <- c(
  #_itlang
  "P15_read_itlang_ScorePlot",
  #_language at home
  "P15_read_asbg03_plot", "P15_read_asbg03_3_plot", "P15_read_asbg03_ScorePlot", "P15_read_asbg03_3_ScorePlot")

# #one way to export all graphs (not as ideal)
# pdf("all.pdf")
# invisible(lapply(PlotList, print))
# dev.off()

#another way to export all graphs (works great!)
invisible(mapply(ggsave, file=paste0("//dc2fs/dc2work/5.2_main/Conferences/CIES 2019/Language/Output/PIRLS/CrossTabs/", names_of_plots, ".pdf"), plot=PlotList, width = 10, height = 6))



