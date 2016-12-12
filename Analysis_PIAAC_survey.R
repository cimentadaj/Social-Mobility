library(survey)
library(stargazer)
library(arm)
###### THIS IS WHERE YOU CHANGE YOUR WORKING DIRECTORY ##############
setwd("/Users/cimentadaj/Downloads/Social_mob_data")

data <- list.files(pattern = ".rda")

for (i in 1:length(data)) {
    load(data[i])
}

ls2 <- c(ls()[grepl("*.design",ls())], "ls2")
# Remove everything that is not in ls2 (so the .design )
rm(list= c(ls()[!ls() %in% ls2]))

countries3 <- list(Austria=prgautp1.design,USA=prgusap1.design,Belgium=prgbelp1.design,
                   Germany=prgdeup1.design,Italy=prgitap1.design,Netherlands=prgnldp1.design,
                   Denmark=prgdnkp1.design,Sweden=prgswep1.design,France=prgfrap1.design,
                   UK=prggbrp1.design,Spain=prgespp1.design,Canada=prgcanp1.design, Czech=prgczep1.design,
                   Estonia=prgestp1.design, Finland=prgfinp1.design, Japan=prgjpnp1.design, Korea=prgkorp1.design,
                   Norway=prgnorp1.design,Poland=prgpolp1.design, Russia=prgrusp1.design, Slovakia=prgsvkp1.design)



### Experimental section ###
### Calculates the share of service class jobs in the PIAAC and the IALS and then the difference ####

# # PIAAC share of service class jobs
# piaac.share <- unlist(lapply(countries3, function(x) with(x,svymean(~serviceclass, na.rm=T,
#                                      rho=NULL,return.replicates=FALSE, deff=FALSE))[[1]][1]))
# names(piaac.share) <- names(countries3)
# 
# # IALS share of service class jobs
# ials <- as.data.frame(read_sas("/Users/cimentadaj/Google Drive/Gosta project/ials/data/microf.sas7bdat"))
# ials$serviceclass <- recode(ials$ISCOR, "c('','.', 0,98,99)=NA; c(1,2)=1; c(3,4,5,6,7,8,9)=0")
# ials$cntrid <- factor(ials$CNTRID, c(1,3,5,6,7,8,9,11,13,14,16,17,18,20,21,23,24,25,29),
#                        labels = c("Canada","Switzerland","Germany","USA","Ireland","Netherlands",
#                                   "Poland","Sweden","New Zealand","UK","Belgium(flanders)",
#                                   "Italy","Norway","Slovenia","Czech","Denmark","Finland","Hungary",
#                                   "Chile"))
# 
# weightsurvey1 <- svrepdesign ( 	
#     weights = ~WEIGHT, 
#     repweights = ials[ ,356:385] ,
#     scale = 1 ,
#     rscales = rep( 1 , 30 ) ,
#     type = 'JKn' ,
#     data = ials ,
#     mse = TRUE
# )
# 
# ials.share <- setNames(as.numeric(svyby(~serviceclass, ~cntrid, weightsurvey1, svymean, na.rm=T,
#               rho=NULL, return.replicates=FALSE, deff=FALSE)[, 2]), na.omit(unique(ials$cntrid)))
# 
# ## Difference between PIAAC and IALS ##
# ## REMEBER BELGIUM IS FLANDERS IN IALS WHEREAS IN PIAAC ITS BELGIUM AS A WHOLE - INCOMPARABLE
# country.names <- intersect(names(ials.share), names(piaac.share))
# piaac.share <- piaac.share[country.names]; ials.share <- ials.share[country.names]
# 
# diff <- piaac.share - ials.share
# 
# 
# # This line is still incomplete. You can't figure out how to add a different number
# # to each list object from the diff vector in line 82(diff=).
# # sapply(names(countries3), function(x) ifelse(x %in% names(diff),
# #                           lapply(countries3, function(x) update(x, diff=)),
# #                           lapply(countries3, function(x) update(x, diff=NA))), simplify = F)

###############

models <- function(dv, covariates, data) {
    dv <- paste(dv, "~ 1")
    combinations <- lapply(1:length(covariates), function(i) seq(1:i))
    formulas <- lapply(combinations, function(p) x <- as.formula(paste(c(dv, covariates[p]), collapse=" + ")))
    results <- lapply(formulas, function(o) with(data, svyglm(o, family = quasibinomial()))[[1]])
    return(results)
}

stargazer2 <- function(model, odd.ratio = F, ...) {
    
    stopifnot(class(model) == "list")
    if (odd.ratio) {
        coefOR2 <- lapply(model, function(x) exp(coef(x)))
        seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
        p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
        stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
        
    } else {
        stargazer(model, ...)
    }
}

digits <- 2

for (i in 1:length(countries3)) {
    
    if (names(countries3[i]) == "USA") {
        #################################### Models for lower class ##############################################
        
        lower1 <- models("lowerclass", c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        lower2 <- models("lowerclass", c("lowmidisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        
        lower.models <- append(lower1, lower2)
        
        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(lower.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-lowerclass"),
                          column.labels = c("1= Lower Class", "1=Lower Class"),
                          column.separate = c(3,3),
                          dep.var.labels.include = FALSE,
                          order = c(1,4),
                          covariate.labels = c("Highest ISCED","DadISCED1-4",
                                               "Cognitivecntrl","Noncognitivecntrl"), digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-lowerclass.html"
                          )
        )
        
        #################################### Models for middle class ######################################################
        
        middle1 <- models("middleclass", c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        middle2 <- models("middleclass", c("lowmidisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        
        middle.models <- append(middle1, middle2)
        
        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(middle.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-middleclass"),
                          column.labels = c("1= Middle Class", "1=Middle Class"),
                          column.separate = c(3,3),
                          dep.var.labels.include = FALSE,
                          order = c(1,4),
                          covariate.labels = c("Highest ISCED","DadISCED1-4",
                                               "Cognitivecntrl","Noncognitivecntrl"), digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-middleclass.html"
                          )
        )
        
        #################################### Models for service class ######################################################
        
        
        high1 <- models("serviceclass",c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        high2 <- models("serviceclass",c("lowmidisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        
        high.models <- append(high1, high2)
        
        
        ## Tables 
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(high.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
                          column.labels = c("1= Service Class", "1=Service Class"),
                          column.separate = c(3,3),
                          dep.var.labels.include = FALSE,
                          order = c(1,4),
                          covariate.labels = c("Highest ISCED","DadISCED1-4",
                                               "Cognitivecntrl","Noncognitivecntrl"), digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-serviceclass.html"
                          )
        )
        
    } else {
        
        #################################### Models for lower class ##############################################
        
        lower1 <- models("lowerclass", c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        lower2 <- models("lowerclass", c("lowmidisced2","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        
        lower.models <- append(lower1, lower2)
        
        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(lower.models, odd.ratio = T,  type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-lowerclass"),
                          column.labels = c("1= Lower Class", "1=Lower Class"),
                          column.separate = c(3,3),
                          dep.var.labels.include = FALSE,
                          order = c(1,4),
                          covariate.labels = c("Highest ISCED","DadISCED1-2",
                                               "Cognitivecntrl","Noncognitivecntrl"), digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-lowerclass.html"
                          )
        )
        
        
        #################################### Models for middle class ######################################################
        
        middle1 <- models("middleclass", c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        middle2 <- models("middleclass", c("lowmidisced2","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        
        middle.models <- append(middle1, middle2)
        
        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(middle.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-middleclass"),
                          column.labels = c("1= Middle Class", "1=Middle Class"),
                          column.separate = c(3,3),
                          dep.var.labels.include = FALSE,
                          order = c(1,4),
                          covariate.labels = c("Highest ISCED","DadISCED1-2",
                                               "Cognitivecntrl","Noncognitivecntrl"), digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-middleclass.html"
                          )
        )
        
        #################################### Models for service class #####
        
        high1 <- models("serviceclass",c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        high2 <- models("serviceclass",c("lowmidisced2","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[i]], gender == 1 & age_categories < 10))
        
        high.models <- append(high1, high2)
        
        
        ## Tables 
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(high.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
                          column.labels = c("1= Service Class", "1=Service Class"),
                          column.separate = c(3,3),
                          dep.var.labels.include = FALSE,
                          order = c(1,4),
                          covariate.labels = c("Highest ISCED","DadISCED1-2",
                                               "Cognitivecntrl","Noncognitivecntrl"), digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-serviceclass.html"
                          )
        )
    }
}

rm(list=c(ls()[!ls() %in% ls2]))