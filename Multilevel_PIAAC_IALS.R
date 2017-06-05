library(dplyr)
library(car)
library(haven)
library(qlcMatrix)
library(Matrix)
library(data.table)
library(stargazer)
library(lme4)
library(xtable)
library(psych)
library(arm)
library(effects)

setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC")

##### Reading data sets and manipulating #####

## Read PIAAC list object with all datasets
#load("/Users/cimentadaj/Google Drive/Gosta project/PIAAC/countrylist.Rda")

## Read the IALS data frame
ials <- read_sas("/Users/cimentadaj/Google Drive/Gosta project/ials/data/microf.sas7bdat")
ials2 <- tbl_df(ials)

## Binding the piaac datasets into one

#piaactry <- data.frame()
# for (i in 1:length(countrylist)) {
#     piaactry <- rbind(piaactry, countrylist[[i]])
#}
# write.csv(piaactry, "piaactry.csv")
piaac <- fread("piaactry.csv")
piaac <- as.data.frame(piaac)

## lowering variable names for IALS
names(ials2) <- tolower(names(ials2))

## Final datasets: ials2 and piaac

##### Data wrangling #####

## PIAAC
piaac <- rename(piaac, isco=ISCO1C,
                dadedu=J_Q07b,
                momedu=J_Q06b,
                numbooks=J_Q08,
                dadimmigrant=J_Q07a,
                eduattain=B_Q01a,
                cognitive=PVNUM1,
                gender=GENDER_R,
                age=AGE_R,
                age_categories2=AGEG5LFS,
                cntry=CNTRYID,
                bottomthings=I_Q04j,
                differentideas=I_Q04l,
                additinfo=I_Q04m)

vars <- c("isco","dadedu","momedu", "numbooks", "dadimmigrant", "eduattain",
          "cognitive","gender", "age", "age_categories2", "cntry",
          paste0(rep("SPFWT",80),0:80), paste0(rep("PVNUM",9), 2:10),
          "bottomthings", "differentideas", "additinfo")

piaacv1 <- piaac[,vars]
piaacv1$isco <- as.numeric(piaacv1$isco)
piaacv1[] <- lapply(piaacv1, function(x) recode(x,'c(9995,9996,9997,9998,9999)=NA'))
table(piaacv1$isco)

## IALS
ials2 <- rename(ials2,isco=iscor,
                dadedu=c11,
                momedu=c5,
                numbooks2=g6c,
                dadimmigrant=c8,
                eduattain=a8,
                cognitive=pvlit1,
                gender=gender,
                age=age,
                age_categories=ageint,
                cntry=cntrid)

ials2 <- select(ials2, isco, dadedu, momedu, numbooks2, dadimmigrant, eduattain,
                cognitive, gender, age, age_categories, cntry, weight ,starts_with("REPLIC"))

###### Recoding variable #####

## PIAAC

numtrans <- function(x) {
    x<-as.numeric(x)
}
vars <- c("dadedu","momedu","eduattain","dadimmigrant","gender","age","numbooks","bottomthings","differentideas","additinfo")
piaacv1[,vars] <- lapply(piaacv1[vars], numtrans)

factor <- fa(piaacv1[,c("bottomthings","differentideas","additinfo")], nfactors = 1,rotate="none", fm="pa", score=T)
piaacv1$non.cognitive <- as.numeric(factor$scores)

quant <- quantile(scale(piaacv1$cognitive), probs = c(0.25,0.75), na.rm=T)
quant2 <- quantile(scale(piaacv1$non.cognitive), probs = c(0.25,0.75), na.rm=T)


piaacv1$highab <- as.numeric(scale(piaacv1$cognitive) >= quant[2][[1]] & scale(piaacv1$non.cognitive) >= quant2[2][[1]])
piaacv1$lowab <- as.numeric(scale(piaacv1$cognitive) <= quant[1][[1]] & scale(piaacv1$non.cognitive) <= quant2[1][[1]])
piaacv1$midcoghigh <- as.numeric(scale(piaacv1$cognitive) >= quant[2][[1]] & scale(piaacv1$non.cognitive) <= quant2[1][[1]])
piaacv1$midnonhigh <- as.numeric(scale(piaacv1$cognitive) <= quant[1][[1]] & scale(piaacv1$non.cognitive) >= quant2[2][[1]])



piaacv1$isco <- recode(piaacv1$isco, "0=NA")
piaacv1$eduattain <- recode(piaacv1$eduattain, "1:3=1; 4:10=2; 11:16=3")
piaacv1$dadimmigrant <- recode(piaacv1$dadimmigrant, "2=1;1=0")
piaacv1$gender <- recode(piaacv1$gender, "2=0")
piaacv1$highattain <- as.numeric(piaacv1$eduattain == 3)

piaacv1$survey <- "PIAAC"

cntrycode_piaac <- as.numeric(names(table(piaacv1$cntry)))
cntrys_piaac <- c("Austria",
                  "Belgium",
                  "Canada",
                  "Czech Republic",
                  "Denmark",
                  "Estonia",
                  "Finland",
                  "France",
                  "Germany",
                  "Ireland",
                  "Italy",
                  "Japan",
                  "Korea",
                  "Netherlands",
                  "Norway",
                  "Poland",
                  "Russian Federation",
                  "Slovak Republic",
                  "Spain",
                  "Sweden",
                  "United Kingdom",
                  "United States")

piaacv1$cntry <- factor(piaacv1$cntry)
lookup_piaac <- setNames(cntrys_piaac, cntrycode_piaac)
piaacv1$cntry <- lookup_piaac[piaacv1$cntry]; table(piaacv1$cntry)
piaacv2 <- piaacv1[,c(11,ncol(piaacv1),1:10,12:ncol(piaacv1)-1)]

piaacv1$yr_born <- NA;
for (i in unique(piaacv1$cntry)) {
    piaacv1$yr_born[piaacv1$cntry == i & piaacv1$age_categories2 > 3] <- 1964
    piaacv1$yr_born[piaacv1$cntry == i & piaacv1$age_categories2 <= 3] <- 1966
}

## IALS

ials2[] <- lapply(ials2, function(x) recode(x, 'c(97,98,99)=NA'))

parents <- c("momedu","dadedu")
ials2[parents] <- lapply(ials2[parents], function(x) recode(x, "0:2=1; c(3,4)=2; 5:7=3; c(9,10,13)=NA"))

ials2$isco <- recode(ials2$isco, "0=NA")
ials2$eduattain <- recode(ials2$eduattain, "c(0,1,2)=1; 3=2; 5:9=3; c(10)=NA")
ials2$dadimmigrant <- recode(ials2$dadimmigrant, "2=1;1=0; c(7,8,9)=NA")
ials2$gender <- recode(ials2$gender, "2=0; 9=NA")
ials2$age[ials2$age > 65 | ials2$age < 16] <- NA
ials2$highattain <- as.numeric(ials2$eduattain == 3)

## age was not asked in Canada, instead the had ageint which are age intervals
## age categories is already ageint, so here I'm actually recoding ageint. 
## Instead, I'll call this variable age_categories2(which will have Canada missing) and
## use age_categories for Canada
ials2$age_categories2 <- recode(ials2$age, "lo:15=NA;
                               16:19=1; 20:24=2; 25:29=3;
                               30:34=4; 35:39=5; 40:44=6; 
                               45:49=7; 50:54=8;
                               55:59=9; 60:65=10; 66:hi=NA")
ials2$survey <- "IALS"

ials2$cntry <- factor(ials2$cntry)
cntrycode_ials <- as.numeric(names(table(ials2$cntry)))
cntrys_ials <- c("Canada(Eng)",
                 "Canada(Fre)",
                 "Switzerland(Ger)",
                 "Switzerland(Fre)",
                 "Germany",
                 "United States",
                 "Ireland",
                 "Netherlands",
                 "Poland",
                 "Sweden",
                 "New Zealand",
                 "United Kingdom",
                 "Northern Ireland",
                 "Belgium(Fl)",
                 "Italy",
                 "Norway",
                 "Slovenia",
                 "Czech Republic",
                 "Switzerland(Ita)",
                 "Denmark",
                 "Finland",
                 "Hungary",
                 "Chile")

lookup_ials <- setNames(cntrys_ials, cntrycode_ials)
ials2$cntry <- lookup_ials[ials2$cntry]

ials2$cntry[ials2$cntry %in% "Belgium(Fl)"] <- "Belgium"
ials2$cntry[ials2$cntry %in% "Northern Ireland"] <- "United Kingdom"
ials2$cntry[ials2$cntry %in% c("Canada(Eng)","Canada(Fre)")] <- "Canada"
ials2$cntry[ials2$cntry %in% c("Switzerland(Ger)","Switzerland(Fre)","Switzerland(Ita)")] <- "Switzerland"

ials2 <- ials2[,c(11,ncol(ials2),1:10,12:(ncol(ials2)-1))]

ials2$yr_born <- NA;
for (i in unique(ials2$cntry)) {
    if(i == "Canada") {
    ials2$yr_born[ials2$cntry == i & ials2$age_categories > 2] <- 1964
    ials2$yr_born[ials2$cntry == i & ials2$age_categories <= 2] <- 1966    
    } else {
    ials2$yr_born[ials2$cntry == i & ials2$age_categories2 > 3] <- 1964
    ials2$yr_born[ials2$cntry == i & ials2$age_categories2 <= 3] <- 1966
    }
}

###### IGNORE!!!!! Attempt at the Age variable -- IGNORE!!! ###### #####
# year_countries <- c("Germany","Ireland","Netherlands","Poland","Sweden","Switzerland","United States")
# ials2$yr_born[ials2$cntry %in% year_countries] <- 1994-ials2$age
# 
# year_countries <- c("Belgium","United Kingdom","New Zealand")
# ials2$yr_born[ials2$cntry %in% year_countries] <- 1996-ials2$age
# 
# year_countries <- c("Chile","Czech Republic","Denmark","Finland","Hungary","Italy","Norway","Slovenia")
# ials2$yr_born[ials2$cntry %in% year_countries] <- 1998-ials2$age
# 
# ials2$yr_born[ials2$cntry == "Germany"] <- 1994-ials2$age[ials2$cntry=="Germany"]
# ials2$yr_born[ials2$cntry == "Italy"] <- 1994-ials2$age[ials2$cntry=="Italy"]

##### Binding the two datasets and further recoding #####

complete_data <- bind_rows(piaacv1, ials2) ## Complete dataset
complete_data <- as.data.frame(complete_data)

complete_data$highorigin <- as.numeric(complete_data$dadedu==3)
complete_data$loworigin <- as.numeric(complete_data$dadedu==1)
complete_data$serviceclass <- as.numeric(complete_data$isco %in% c(1,2))
complete_data$middleclass <- as.numeric(complete_data$isco %in% c(3,4,5))
complete_data$lowerclass <- as.numeric(complete_data$isco %in% c(6,7,8,9))

complete_data$dadedu[is.na(complete_data$dadedu)] <- 0
complete_data$momedu[is.na(complete_data$momedu)] <- 0

complete_data$highedu <- rowMax(as.matrix(complete_data[c("dadedu","momedu")]))

recodena <- function(var,num) {
    complete_data[,var][complete_data[var]==num] <- NA
    return(complete_data)
}
complete_data <- recodena(c("dadedu","momedu","highedu"),c(0))


complete_data$highisced <- as.numeric(complete_data$highedu == 3)
complete_data$lowisced  <- as.numeric(complete_data$highedu == 1)
complete_data$lowmidisced2 <- as.numeric(complete_data$highedu == 1 | complete_data$highedu == 2) ## For the USA

complete_data$old_vs_young <- as.numeric(complete_data$yr_born < 1965)

anglosaxon <- c("Canada","United Kingdom","Ireland","United States","New Zealand")
asia <- c("Japan","Korea")
chile <- "Chile"
post_communist <- c("Czech Republic","Estonia","Poland","Russian Federation","Slovak Republic",
                    "Slovenia","Hungary")
continental <- c("Austria","Belgium","Germany","France","Switzerland","Netherlands")
mediterran <- c("Spain","Italy")
scandinavia <- c("Sweden","Denmark","Norway","Finland")

l_cr <- list(anglosaxon=anglosaxon,asia=asia,chile=chile,post_communist=post_communist,
             continental=continental,mediterran=mediterran,scandinavia=scandinavia)

for (i in 1:length(l_cr)) {
     complete_data[[paste0(names(l_cr[i]),"_dummy")]] <- as.numeric(complete_data[,"cntry"] %in% l_cr[[i]])
}

## Let's do some checking
lapply(complete_data[1:6], table) ## Everything seems coded correctly

##### Adding the macro indicators ####

## Download the xlsx from here http://www.oecd.org/employment/emp/EPL-timeseries.xlsx
## Convert to CSV and only download the second sheet
epl <- read.csv("/Users/cimentadaj/Downloads/EPL-timeseries - Summary indicators.csv", stringsAsFactors = F)[c(2,3,7)]
epl <- epl[epl$country %in% intersect(unique(complete_data$cntry), unique(epl$country)),]

initial_year_epl <- numeric()
highest_year_epl <- numeric()

for (i in unique(epl$country)) {
    lyear <- min(epl$year[epl$country == i])
    hyear <- max(epl$year[epl$country == i])
    if (i == "Canada") {
    ## Remember that Canada in IALS has the age_categories variable instead of the age_categories2. The equivalent of the 29 threshold in the age_categories2
    ## is the 6 in the age_categories
    complete_data$epl[complete_data$cntry == i & complete_data$yr_born <= 1965 & complete_data$age_categories <= 6 & complete_data$survey == "IALS"] <- epl[epl$country == i & epl$year == lyear,3]
    complete_data$epl[complete_data$cntry == i & complete_data$yr_born <= 1965 & complete_data$age_categories2 <= 10 & complete_data$survey == "PIAAC"] <- epl[epl$country == i & epl$year == lyear,3]
    complete_data$epl[complete_data$cntry == i & complete_data$yr_born > 1965] <- epl[epl$country == i & epl$year == hyear,3]
    } else {
    complete_data$epl[complete_data$cntry == i & complete_data$yr_born <= 1965 & complete_data$age_categories2 <= 10] <- epl[epl$country == i & epl$year == lyear,3]
    complete_data$epl[complete_data$cntry == i & complete_data$yr_born > 1965] <- epl[epl$country == i & epl$year == hyear,3]
  }
    ## Saving this for later
    initial_year_epl <- c(initial_year_epl, lyear)
    highest_year_epl <- c(highest_year_epl, hyear)
}

setNames(initial_year_epl, unique(epl$country)) ## here you can see the initial year for each country; change initial for highest
## to see the highest year

## Cite this
## Original link: http://stats.oecd.org/Index.aspx?datasetcode=SOCX_AGG#
## Set all options to Public, all total and In percentage of GDP, in that order.
# Download as csv and name it oecdsocialspending.csv
oecd <- read.csv("/Users/cimentadaj/Downloads/oecdsocialspending.csv")[c("Country","Year","Value")]

initial_year_welfare <- numeric()
highest_year_welfare <- numeric()

for (i in intersect(unique(complete_data$cntry), unique(oecd$Country))) {
    lyear <- min(oecd$Year[oecd$Country == i])
    hyear <- max(oecd$Year[oecd$Country == i])
    if (i == "Canada") {
        complete_data$welfare[complete_data$cntry == i & complete_data$yr_born <= 1965 & complete_data$age_categories <= 6 & complete_data$survey == "IALS"] <- oecd[oecd$Country == i & oecd$Year == lyear,3]
        complete_data$welfare[complete_data$cntry == i & complete_data$yr_born <= 1965 & complete_data$age_categories2 <= 10 & complete_data$survey == "PIAAC"] <- oecd[oecd$Country == i & oecd$Year == lyear,3]
        complete_data$welfare[complete_data$cntry == i & complete_data$yr_born > 1965] <- oecd[oecd$Country == i & oecd$Year == hyear,3]
    } else {
        complete_data$welfare[complete_data$cntry == i & complete_data$yr_born <= 1965 & complete_data$age_categories2 <= 10] <- oecd[oecd$Country == i & oecd$Year == lyear,3]
        complete_data$welfare[complete_data$cntry == i & complete_data$yr_born > 1965] <- oecd[oecd$Country == i & oecd$Year == hyear,3]
    }
    ## Saving this for later
    initial_year_welfare <- c(initial_year_welfare, lyear)
    highest_year_welfare <- c(highest_year_welfare, hyear)
}

## Russian data was extracted from here: http://data.worldbank.org/indicator/GC.XPN.TOTL.GD.ZS
complete_data$welfare[complete_data$cntry=="Russian Federation"] <- 18.6

setNames(initial_year_welfare, intersect(unique(complete_data$cntry), unique(oecd$Country)))

complete_data$tracking[complete_data$cntry == "Canada" & complete_data$yr_born <= 1965 & complete_data$age_categories <= 6 & complete_data$survey == "IALS"] <- 18
complete_data$tracking[complete_data$cntry == "Canada" & complete_data$yr_born <= 1965 & complete_data$age_categories2 <= 10 & complete_data$survey == "PIAAC"] <- 18 
complete_data$tracking[complete_data$cntry == "Canada" & complete_data$yr_born > 1965] <- 18


trackingdata <- data.frame(
    cntry=c("Austria","Belgium","Canada","Chile","Czech Republic","Denmark","Finland","France","Germany","Hungary",
            "Ireland","Italy","Japan","Korea","Netherlands","New Zealand","Norway","Poland","Russian Federation",
            "Slovak Republic","Slovenia","Spain","Sweden","Switzerland","United Kingdom","United States"),
    "1980"=c(10,12,18,14,15,16,16,16,10,10,12,14,15,14,12,18,16,15,15,10,15,14,16,15.5,16,18),
    "2002"=c(10,12,18,13,11,16,16,15,10,11,15,14,15,14,12,16,16,15,15,11,15,16,16,16,16,18)
)

for (i in 1:nrow(trackingdata)) {
complete_data$tracking[complete_data$cntry == trackingdata$cntry[i] & complete_data$yr_born <= 1965 & complete_data$age_categories2 <= 10] <- trackingdata$X1980[i]
complete_data$tracking[complete_data$cntry == trackingdata$cntry[i] & complete_data$yr_born > 1965] <- trackingdata$X2002[i]
}

anglosaxon <- c("Canada","United Kingdom","Ireland","United States","New Zealand")
asia <- c("Japan","Korea")
chile <- "Chile"
post_communist <- c("Czech Republic","Estonia","Poland","Russian Federation","Slovak Republic",
                    "Slovenia","Hungary")
continental <- c("Austria","Belgium","Germany","France","Switzerland","Netherlands")
mediterran <- c("Spain","Italy")
scandinavia <- c("Sweden","Denmark","Norway","Finland")



group_change <- function(var, variables, grouping) {
  var[complete_data$cntry %in% variables] <- grouping
  var
}

complete_data$countrgroups <- group_change(complete_data$countrgroups, anglosaxon, "Anglosaxon")
complete_data$countrgroups <- group_change(complete_data$countrgroups, asia, "Asia")
complete_data$countrgroups <- group_change(complete_data$countrgroups, post_communist, "post_communist")
complete_data$countrgroups <- group_change(complete_data$countrgroups, continental, "continental")
complete_data$countrgroups <- group_change(complete_data$countrgroups, mediterran, "mediterran")
complete_data$countrgroups <- group_change(complete_data$countrgroups, scandinavia, "scandinavia")

complete_data_list <- split(complete_data[complete_data$survey=="PIAAC", ], complete_data[complete_data$survey=="PIAAC", ]$countrgroups)

# for (i in 1:length(complete_data_list)) {
#     for( j in c(1964,1966)) {
#     m1 <- glm(serviceclass ~ highisced, complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
#     m2 <- glm(serviceclass ~ highisced + scale(cognitive), complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
#     m3 <- glm(serviceclass ~ highisced + scale(cognitive) + non.cognitive, complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
#     m4 <- glm(serviceclass ~ highisced + scale(cognitive) + non.cognitive + tracking, complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
# 
#     stargazer(m1,m2,m3,m4, type="html",title=paste0(names(complete_data_list)[i],"-",j),apply.coef = exp, apply.ci = exp, digits = 2, out=paste0("PIAAC-",names(complete_data_list)[i],"-",j,"-high.html"))
# 
#     m1 <- glm(serviceclass ~ lowisced, complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
#     m2 <- glm(serviceclass ~ lowisced + scale(cognitive), complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
#     m3 <- glm(serviceclass ~ lowisced + scale(cognitive) + non.cognitive, complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
#     m4 <- glm(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + tracking, complete_data_list[[i]], family= binomial(link = "logit"), subset = complete_data_list[[i]]$yr_born == j)
# 
#     stargazer(m1,m2,m3,m4, type="html",title=paste0(names(complete_data_list)[i],"-",j),apply.coef = exp, apply.ci = exp, digits = 2, out=paste0("PIAAC-",names(complete_data_list)[i],"-",j,"-low.html"))
#     }
# }

list_countries <- split(complete_data[complete_data$survey=="PIAAC" & complete_data$cntry %in% c("Denmark","United States"),], complete_data[complete_data$survey=="PIAAC" & complete_data$cntry %in% c("Denmark","United States"),]$cntry)

m1 <- glm(eduattain ~ highisced, data=list_countries[[2]], subset=yr_born==1966 & gender == 1)
m2 <- glm(eduattain ~ highisced + scale(cognitive), data=list_countries[[2]], subset=yr_born==1966 & gender == 1)
m3 <- glm(eduattain ~ highisced + scale(cognitive) + scale(non.cognitive), data=list_countries[[2]], subset=yr_born==1966 & gender == 1)
m4 <- glm(eduattain ~ lowisced, data=list_countries[[2]], subset=yr_born==1966 & gender == 1)
m5 <- glm(eduattain ~ lowisced + scale(cognitive), data=list_countries[[2]], subset=yr_born==1966 & gender == 1)
m6 <- glm(eduattain ~ lowisced + scale(cognitive) + scale(non.cognitive), data=list_countries[[2]], subset=yr_born==1966 & gender == 1)

stargazer(m1,m2,m3,m4,m5,m6, type="html", apply.coef=exp, order = c(1,4),
          out=paste0("US-boys-young-cohort.html"),
          dep.var.labels = "ISCED5-6", digits=2)

# yr <- c(1964,1966)
# old <- c("old","young")
# 
# for (i in 1:length(list_countries)) {
#         for (j in 1:2)  {
#     m1 <- glm(serviceclass ~ highisced, data=list_countries[[i]], subset=yr_born==yr[j])
#     m2 <- glm(serviceclass ~ highisced + scale(cognitive), data=list_countries[[i]], subset=yr_born==yr[j])
#     m3 <- glm(serviceclass ~ highisced + scale(cognitive) + scale(non.cognitive), data=list_countries[[i]], subset=yr_born==yr[j])
#     stargazer(m1,m2,m3, type="html", apply.coef = exp, digits=2, out=paste0("PIAAC-high-",names(list_countries)[i],"-",old[j],".html"))
#             
#     m1 <- glm(serviceclass ~ lowisced, data=list_countries[[i]], subset=yr_born==yr[j])
#     m2 <- glm(serviceclass ~ lowisced + scale(cognitive), data=list_countries[[i]], subset=yr_born==yr[j])
#     m3 <- glm(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive), data=list_countries[[i]], subset=yr_born==yr[j])
#     stargazer(m1,m2,m3, type="html", apply.coef = exp, digits=2, out=paste0("PIAAC-low-",names(list_countries)[i],"-",old[j],".html"))
#   }
# }

##### Analysis #####
# 
# m1 <- glmer(serviceclass ~ highisced +  (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + scale(non.cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m4 <- glmer(serviceclass ~ highisced + scale(cognitive) + scale(non.cognitive) + (1 + highisced | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + scale(non.cognitive) + scale(welfare,scale=F) + (1 + highisced | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + scale(non.cognitive) + scale(welfare,scale=F) + highisced:welfare + (1 + highisced | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# models <- list(m1,m2,m3,m4,m5,m6)
# between_variation <- sapply(models, function(x) round(attributes(summary(x)$varcor$cntry)$stddev[[1]],3))
# betweenisced_variation <- c(0,0,0,sapply(models[4:6], function(x) round(attributes(summary(x)$varcor$cntry)$stddev[[2]],3)))
# 
# stargazer(m1,m2,m3,m4,m5,m6, type="html", apply.coef=exp, digits = 2,
#             add.lines = list(c("Between-country variability", between_variation),
#                              c("ISCED5-6 variability between countries",betweenisced_variation)),
#           out="PIAAC-Multilevel-welfare-high.html")
# 
# #effect("highisced:welfare", m6, list(welfare=c(12,15,18)), grid=TRUE)
# 
# m1 <- glmer(serviceclass ~ lowisced +  (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m4 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + (1 + highisced | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + scale(welfare,scale=F) + (1 + highisced | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + scale(welfare,scale=F) + lowisced:welfare + (1 + highisced | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# models <- list(m1,m2,m3,m4,m5,m6)
# between_variation <- sapply(models, function(x) round(attributes(summary(x)$varcor$cntry)$stddev[[1]],3))
# betweenisced_variation <- c(0,0,0,sapply(models[4:6], function(x) round(attributes(summary(x)$varcor$cntry)$stddev[[2]],3)))
# 
# stargazer(m1,m2,m3,m4,m5,m6, type="html", apply.coef=exp, digits = 2,
#           add.lines = list(c("Between-country variability", between_variation),
#                            c("ISCED5-6 variability between countries",betweenisced_variation)),
#           out="PIAAC-Multilevel-welfare-low.html")
# 


# dotplot(ranef(m4, condVar=TRUE)) ## For when you have varying intercept and varying slope
# plot(effect("highisced:welfare", m5, list(welfare=c(15,18,30)), grid=TRUE))


#highisced=0
# odds intercept == 2.62
# slope == 7.42 -- a unit increase in welfare increases odds of achieving service class of about 7.42

# highisced == 1
# odds intercept == 2.62 - 0.86 == 1.8
# slope == 7.42 - 0.93 == 6.95

# Estimate naked models for each of the macro-level variables
# Add the macro level variable but varying( to see the overall mean effect)
# Add an interaction between highisced and each macro level variable

# plot the varying slope and intercept for each of the macro level variables

# plot an interaction of highisced with each macro level variable 

stargazer(m1,m2,m3,m4,m5, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
          add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
                             summary(m3)$ngrps[[1]],summary(m4)$ngrps[[1]])), out="PIAAC-Multilevel-welfare-high.html")

m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
m4 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + scale(non.cognitive) + welfare + (1 + welfare | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")


stargazer(m1,m2,m3,m4,m5, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
          add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
                             summary(m4)$ngrps[[1]])), out="PIAAC-Multilevel-welfare-low.html")

# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + asia_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + asia_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + asia_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + asia_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-asia-high.html")
# 
# m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + asia_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + asia_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + asia_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + asia_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-asia-hlow.html")
# 
# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + chile_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + chile_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + chile_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + chile_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-chile-high.html")
# 
# m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + chile_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + chile_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + chile_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + chile_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-chile-low.html")
# 
# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + post_communist_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + post_communist_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + post_communist_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + post_communist_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-post_communist-high.html")
# 
# m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + post_communist_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + post_communist_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + post_communist_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + post_communist_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-post_communist-low.html")
# 
# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + continental_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + continental_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + continental_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + continental_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-continental-high.html")
# 
# m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + continental_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + continental_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + continental_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + continental_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-continental-low.html")
# 
# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + mediterran_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + mediterran_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + mediterran_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + mediterran_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-mediterran-high.html")
# 
# m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + mediterran_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + mediterran_dummy + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + mediterran_dummy + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + mediterran_dummy + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-mediterran-low.html")
# 
# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + scan + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + scan + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + scan + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + scale(cognitive) + non.cognitive  + scan + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-scand-high.html")
# 
# m1 <- glmer(serviceclass ~ lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + scan + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + scan + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + scan + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ lowisced + scale(cognitive) + non.cognitive  + scan + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m5,m6,m7,m8, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]], summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]])), out="PIAAC-Multilevel-scand-low.html")
# 
# complete_data$scan <- as.numeric(complete_data$cntry %in% c("Finland","Sweden","Norway","Denmark"))
# complete_data$scan[complete_data$cntry %in% c("Canada","United Kingdom","Ireland","United States","New Zealand")] <- NA

# m1 <- glmer(serviceclass ~ highisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m2 <- glmer(serviceclass ~ highisced + old_vs_young + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m3 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m4 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m5 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + welfare + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m7 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + epl + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m9 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + tracking + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m6 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + welfare + scandinavia_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m8 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + epl + scandinavia_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# m10 <- glmer(serviceclass ~ highisced + old_vs_young + lowisced + scale(cognitive) + tracking + scandinavia_dummy + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")
# 
# stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, type="html",apply.coef = exp, apply.ci = exp, digits = 2,
#           add.lines = list(c("Countries",summary(m1)$ngrps[[1]],summary(m2)$ngrps[[1]],
#                              summary(m3)$ngrps[[1]],summary(m4)$ngrps[[1]],summary(m5)$ngrps[[1]],
#                              summary(m6)$ngrps[[1]],summary(m7)$ngrps[[1]], summary(m8)$ngrps[[1]], summary(m9)$ngrps[[1]],
#                              summary(m10)$ngrps[[1]])), out="scandinavia_multilevel.html")
# 
# newdata <- complete_data[complete_data$survey=="PIAAC",c("cntry","serviceclass","highisced","old_vs_young","lowisced","cognitive")]
# probs <- predict(m4,newdata=newdata, type="response")
# newdata$probs <- probs
# 
# (probtable <- newdata %>%
#     group_by(cntry) %>%
#     summarize("Mean probabilities"=round(mean(probs, na.rm=T),2)))
# 
# probtable$scale <- scale(probtable$`Mean probabilities`)
# 
# 
# probtable$Mobility <- ifelse(probtable$scale > 1, "Highly Mobile",
#        ifelse(probtable$scale < -1, probtable$Mobility <- "Low Mobility", "Normal"))
# 
# probtable$sd <- sd(probtable$`Mean probabilities`)
# 
# probtable <- probtable[c(1:3,5,4)]
# 
# print(xtable(probtable), type="html",file="serviceclasstable.html")


trackingdata <- trackingdata[trackingdata$cntry %in% unique(trackcoef$cntry),]


###############
m4 <- glmer(highattain ~ highisced + old_vs_young + lowisced + scale(cognitive) + (1 | cntry), complete_data, family= binomial(link = "logit"), subset = survey=="PIAAC")

newdata <- complete_data[complete_data$survey=="PIAAC",c("cntry","highattain","highisced","old_vs_young","lowisced","cognitive")]
probs <- predict(m4,newdata=newdata, type="response")
newdata$probs <- probs

(probtable <- newdata %>%
    group_by(cntry) %>%
    summarize("Mean probabilities"=round(mean(probs, na.rm=T),2)))

probtable$scale <- scale(probtable$`Mean probabilities`)


probtable$Mobility <- ifelse(probtable$scale > 1, "Highly Mobile",
                             ifelse(probtable$scale < -1, probtable$Mobility <- "Low Mobility", "Normal"))

print(xtable(probtable), type="html",file="serviceclasstable.html")





