library(survey)
library(stargazer)
###### THIS IS WHERE YOU CHANGE YOUR WORKING DIRECTORY ##############
setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")

data <- grep(".rda", list.files(), value=T)

for (i in 1:length(data)) {
    load(data[i])
}


ls2 <- c(ls()[grepl("*.design",ls())], "ls2")
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



for (i in 1:length(countries3)) {
    
    workdataset3 <- subset(countries3[[1]], age_categories >= 3 & age_categories <= 6 & gender == 1) ## From 25-45 BOYS
    workdataset4 <- subset(countries3[[1]], age_categories >= 7 & age_categories <= 10 & gender == 1) ## From 45-60 BOYS
    
    
    if (names(countries3[i]) == "USA") {
        #################################### Models for lower class ##############################################
        
        #     ## Models for MEN from the 25-45 cohort
        #     m1 <- with(workdataset3, svyglm(lowerclass ~ highisced  , family = quasibinomial()))
        #     #m2 <- with(workdataset3, svyglm(lowerclass ~ highisced  + numbooks, family = quasibinomial()))
        #     m3 <- with(workdataset3, svyglm(lowerclass ~ highisced   + scale(pvnum), family = quasibinomial()))
        #     m3.1 <- with(workdataset3, svyglm(lowerclass ~ highisced   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     m4 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced  , family = quasibinomial()))
        #     #m5 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced  + numbooks , family = quasibinomial()))
        #     m6 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced   + scale(pvnum), family = quasibinomial()))
        #     m6.1 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     ## Models for MEN from 45-65
        #     m7 <- with(workdataset4, svyglm(lowerclass ~ highisced  , family = quasibinomial()))
        #     #m8 <- with(workdataset4, svyglm(lowerclass ~ highisced  + numbooks, family = quasibinomial()))
        #     m9 <- with(workdataset4, svyglm(lowerclass ~ highisced   + scale(pvnum), family = quasibinomial()))
        #     m9.1 <- with(workdataset4, svyglm(lowerclass ~ highisced   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     
        #     m10 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced  , family = quasibinomial()))
        #     #m11 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced  + numbooks , family = quasibinomial()))
        #     m12 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced   + scale(pvnum), family = quasibinomial()))
        #     m12.1 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     
        #     highyoung <- list(m1[[1]],m3[[1]],m3.1[[1]],m4[[1]],m6[[1]],m6.1[[1]])
        #     highold <- list(m7[[1]],m9[[1]],m9.1[[1]],m10[[1]],m12[[1]],m12.1[[1]])
        #     
        #     ## Tables for YOUNG service class BOYS
        #     setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        #     all <- stargazer(highyoung, type = "html", title = paste0(names(countries3[i]),"PIAAC-25-45-sons-lowerclass"),
        #                      column.labels = c("1= Lower Class", "1=Lower Class"),
        #                      column.separate = c(3,3),
        #                      dep.var.labels.include = FALSE,
        #                      order = c(1,4),
        #                      covariate.labels = c("Highest ISCED","DadISCED1-4",
        #                                           "Cognitivecntrl","Noncognitivecntrl"),
        #                      apply.coef = exp,
        #                      apply.ci = exp, digits = 2
        #                      , out = paste0(names(countries3[i]),"PIAAC-25-45-sons-lowerclass.html"
        #                      )
        #     )
        #     
        #     ## Tables for old service class BOYS
        #     all <- stargazer(highold, type = "html", title = paste0(names(countries3[i]),"PIAAC-45-65-sons-lowerclass"),
        #                      column.labels = c("1= Lower Class", "1=Lower Class"),
        #                      column.separate = c(3,3),
        #                      dep.var.labels.include = FALSE,
        #                      order = c(1,4),
        #                      covariate.labels = c("Highest ISCED","DadISCED1-4",
        #                                           "Cognitivecntrl","Noncognitivecntrl"),
        #                      apply.coef = exp,
        #                      apply.ci = exp, digits = 2
        #                      , out = paste0(names(countries3[i]),"PIAAC-45-65-sons-lowerclass.html")
        #     )
        
        #################################### Models for middle class ######################################################
        
        #       ## Models for MEN from the 25-45 cohort
        #       m1 <- with(workdataset3, svyglm(middleclass ~ highisced , family = quasibinomial()))
        #       m2 <- with(workdataset3, svyglm(middleclass ~ highisced  + numbooks, family = quasibinomial()))
        #       m3 <- with(workdataset3, svyglm(middleclass ~ highisced  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       m4 <- with(workdataset3, svyglm(middleclass ~ lowmidisced  , family = quasibinomial()))
        #       m5 <- with(workdataset3, svyglm(middleclass ~ lowmidisced  + numbooks , family = quasibinomial()))
        #       m6 <- with(workdataset3, svyglm(middleclass ~ lowmidisced  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       ## Models for MEN from 45-65
        #       m7 <- with(workdataset4, svyglm(middleclass ~ highisced  , family = quasibinomial()))
        #       m8 <- with(workdataset4, svyglm(middleclass ~ highisced  + numbooks, family = quasibinomial()))
        #       m9 <- with(workdataset4, svyglm(middleclass ~ highisced  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       m10 <- with(workdataset4, svyglm(middleclass ~ lowmidisced  , family = quasibinomial()))
        #       m11 <- with(workdataset4, svyglm(middleclass ~ lowmidisced  + numbooks , family = quasibinomial()))
        #       m12 <- with(workdataset4, svyglm(middleclass ~ lowmidisced  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       middleyoung <- list(m1[[1]],m2[[1]],m3[[1]],m4[[1]],m5[[1]],m6[[1]])
        #       middleold <- list(m7[[1]],m8[[1]],m9[[1]],m10[[1]],m11[[1]],m12[[1]])
        #       
        #       ## Table for YOUNG low class BOYS
        #       setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        #       all <- stargazer(middleyoung, type = "html", title = paste0(names(countries3[i]),"PIAAC-25-45-sons-middleclass"),
        #                        column.labels = c("1= Middle Class", "1= Middle Class"),
        #                        column.separate = c(3,3),
        #                        dep.var.labels.include = FALSE,
        #                        order = c(1,4),
        #                        covariate.labels = c("Highest ISCED","DadISCED1-4",
        #                                             "# of books","Cognitivecntrl"),
        #                        apply.coef = exp,
        #                        apply.ci = exp, digits = 2
        #                        , out = paste0(names(countries3[i]),"PIAAC-25-45-sons-middleclass.html")
        #       )
        #       ## Table for OLD low class BOYS
        #       all <- stargazer(middleold, type = "html", title = paste0(names(countries3[i]),"PIAAC-45-65-sons-middleclass"),
        #                        column.labels = c("1= Middle Class", "1= Middle Class"),
        #                        column.separate = c(3,3),
        #                        dep.var.labels.include = FALSE,
        #                        order = c(1,4),
        #                        covariate.labels = c("Highest ISCED","DadISCED1-4",
        #                                             "# of books","Cognitivecntrl"),
        #                        apply.coef = exp,
        #                        apply.ci = exp, digits = 2
        #                        , out = paste0(names(countries3[i]),"PIAAC-45-65-sons-middleclass.html")
        #       )
        
        #################################### Models for service class ######################################################
        
        
        high1 <- models("serviceclass",c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[1]], gender == 1 & age_categories < 10))
        high2 <- models("serviceclass",c("lowmidisced2","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[1]], gender == 1 & age_categories < 10))
        
        high.models <- append(high1, high2)
        
        
        ## Tables 
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        all <- stargazer(high.models, type = "html", title = paste0(names(countries3[1]),"PIAAC-sons-serviceclass"),
                         column.labels = c("1= Service Class", "1=Service Class"),
                         column.separate = c(3,3),
                         dep.var.labels.include = FALSE,
                         order = c(1,4),
                         covariate.labels = c("Highest ISCED","DadISCED1-2",
                                              "Cognitivecntrl","Noncognitivecntrl"),
                         apply.coef = exp,
                         apply.ci = exp, digits = 2
                         , out = paste0(names(countries3[1]),"-PIAAC-sons-serviceclass.html"
                         )
        
        
        ## Models for MEN from the 25-45 cohort
        models("serviceclass", c("highisced"))
        m1 <- with(workdataset3, svyglm(serviceclass ~ highisced  , family = quasibinomial()))
        m2 <- with(workdataset3, svyglm(serviceclass ~ highisced  + non.cognitive, family = quasibinomial()))
        #m3 <- with(workdataset3, svyglm(serviceclass ~ highisced  + non.cognitive + scale(pvnum), family = quasibinomial()))
        
        m4 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced  , family = quasibinomial()))
        m5 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced  + non.cognitive , family = quasibinomial()))
        #m6 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced   + non.cognitive + scale(pvnum), family = quasibinomial()))
        #m6.1 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced   + non.cognitive + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        
        ## Models for MEN from 45-65
        m7 <- with(workdataset4, svyglm(serviceclass ~ highisced  , family = quasibinomial()))
        m8 <- with(workdataset4, svyglm(serviceclass ~ highisced  + non.cognitive, family = quasibinomial()))
        #m9 <- with(workdataset4, svyglm(serviceclass ~ highisced   + non.cognitive + scale(pvnum), family = quasibinomial()))
        #m9.1 <- with(workdataset4, svyglm(serviceclass ~ highisced  + non.cognitive + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        
        
        m10 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced  , family = quasibinomial()))
        m11 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced  + non.cognitive, family = quasibinomial()))
        #m12 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced  + non.cognitive + scale(pvnum), family = quasibinomial()))
        #m12.1 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced  + non.cognitive + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        
        
        highyoung <- list(m1[[1]],m2[[1]],m4[[1]],m5[[1]])
        highold <- list(m7[[1]],m8[[1]],m10[[1]],m11[[1]])
        
        ## Tables for YOUNG service class BOYS
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        all <- stargazer(highyoung, type = "html", title = paste0(names(countries3[i]),"PIAAC-25-45-sons-serviceclass"),
                         column.labels = c("1= Service Class", "1=Service Class"),
                         column.separate = c(2,2),
                         dep.var.labels.include = FALSE,
                         order = c(1,3),
                         covariate.labels = c("Highest ISCED","DadISCED1-4", "Non.cognitive"),
                         apply.coef = exp,
                         apply.ci = exp, digits = 2
                         , out = paste0(names(countries3[i]),"-PIAAC-25-45-sons-serviceclass.html")
        )
        
        ## Tables for old service class BOYS
        all <- stargazer(highold, type = "html", title = paste0(names(countries3[i]),"PIAAC-45-65-sons-serviceclass"),
                         column.labels = c("1= Service Class", "1=Service Class"),
                         column.separate = c(2,2),
                         dep.var.labels.include = FALSE,
                         order = c(1,3),
                         covariate.labels = c("Highest ISCED","DadISCED1-4", "Non.cognitive"),
                         apply.coef = exp,
                         apply.ci = exp, digits = 2
                         , out = paste0(names(countries3[i]),"-PIAAC-45-65-sons-serviceclass.html")
        )
        
        
        #######o#########
        
    } else {
        
        #################################### Models for lower class ##############################################
        
            lower1 <- models("lowerclass", c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[1]], gender == 1 & age_categories < 10))
            lower2 <- models("lowerclass", c("lowmidisced2","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[1]], gender == 1 & age_categories < 10))
            
            lower.models <- append(lower1, lower2)

            ## Tables
            setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
            all <- stargazer(lower.models, type = "html", title = paste0(names(countries3[1]),"PIAAC-sons-lowerclass"),
                             column.labels = c("1= Lower Class", "1=Lower Class"),
                             column.separate = c(3,3),
                             dep.var.labels.include = FALSE,
                             order = c(1,4),
                             covariate.labels = c("Highest ISCED","DadISCED1-2",
                                                  "Cognitivecntrl","Noncognitivecntrl"),
                             apply.coef = exp,
                             apply.ci = exp, digits = 2
                             , out = paste0(names(countries3[1]),"PIAAC-sons-lowerclass.html"
                             )
            )
        
        
        #################################### Models for middle class ######################################################
        
        #       ## Models for MEN from the 25-45 cohort
        #       m1 <- with(workdataset3, svyglm(middleclass ~ highisced , family = quasibinomial()))
        #       m2 <- with(workdataset3, svyglm(middleclass ~ highisced  + numbooks, family = quasibinomial()))
        #       m3 <- with(workdataset3, svyglm(middleclass ~ highisced  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       m4 <- with(workdataset3, svyglm(middleclass ~ lowmidisced2  , family = quasibinomial()))
        #       m5 <- with(workdataset3, svyglm(middleclass ~ lowmidisced2  + numbooks , family = quasibinomial()))
        #       m6 <- with(workdataset3, svyglm(middleclass ~ lowmidisced2  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       ## Models for MEN from 45-65
        #       m7 <- with(workdataset4, svyglm(middleclass ~ highisced  , family = quasibinomial()))
        #       m8 <- with(workdataset4, svyglm(middleclass ~ highisced  + numbooks, family = quasibinomial()))
        #       m9 <- with(workdataset4, svyglm(middleclass ~ highisced  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       m10 <- with(workdataset4, svyglm(middleclass ~ lowmidisced2  , family = quasibinomial()))
        #       m11 <- with(workdataset4, svyglm(middleclass ~ lowmidisced2  + numbooks , family = quasibinomial()))
        #       m12 <- with(workdataset4, svyglm(middleclass ~ lowmidisced2  + numbooks  + scale(pvnum), family = quasibinomial()))
        #       
        #       middleyoung <- list(m1[[1]],m2[[1]],m3[[1]],m4[[1]],m5[[1]],m6[[1]])
        #       middleold <- list(m7[[1]],m8[[1]],m9[[1]],m10[[1]],m11[[1]],m12[[1]])
        #       
        #       ## Table for YOUNG low class BOYS
        #       setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        #       all <- stargazer(middleyoung, type = "html", title = paste0(names(countries3[i]),"PIAAC-25-45-sons-middleclass"),
        #                        column.labels = c("1= Middle Class", "1= Middle Class"),
        #                        column.separate = c(3,3),
        #                        dep.var.labels.include = FALSE,
        #                        order = c(1,4),
        #                        covariate.labels = c("Highest ISCED","Highest-low ISCED",
        #                                             "# of books","Cognitivecntrl"),
        #                        apply.coef = exp,
        #                        apply.ci = exp, digits = 2
        #                        , out = paste0(names(countries3[i]),"PIAAC-25-45-sons-middleclass.html")
        #       )
        #       ## Table for OLD low class BOYS
        #       all <- stargazer(middleold, type = "html", title = paste0(names(countries3[i]),"PIAAC-45-65-sons-middleclass"),
        #                        column.labels = c("1= Middle Class", "1= Middle Class"),
        #                        column.separate = c(3,3),
        #                        dep.var.labels.include = FALSE,
        #                        order = c(1,4),
        #                        covariate.labels = c("Highest ISCED","Highest-low ISCED",
        #                                             "# of books","Cognitivecntrl"),
        #                        apply.coef = exp,
        #                        apply.ci = exp, digits = 2
        #                        , out = paste0(names(countries3[i]),"PIAAC-45-65-sons-middleclass.html")
        #       )
        
        #################################### Models for service class #####
        
        high1 <- models("serviceclass",c("highisced","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[1]], gender == 1 & age_categories < 10))
        high2 <- models("serviceclass",c("lowmidisced2","scale(pvnum)","scale(non.cognitive)"), subset(countries3[[1]], gender == 1 & age_categories < 10))

        high.models <- append(high1, high2)

        
        ## Tables 
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        all <- stargazer(high.models, type = "html", title = paste0(names(countries3[1]),"PIAAC-sons-serviceclass"),
                         column.labels = c("1= Service Class", "1=Service Class"),
                         column.separate = c(3,3),
                         dep.var.labels.include = FALSE,
                         order = c(1,4),
                         covariate.labels = c("Highest ISCED","DadISCED1-2",
                                              "Cognitivecntrl","Noncognitivecntrl"),
                         apply.coef = exp,
                         apply.ci = exp, digits = 2
                         , out = paste0(names(countries3[1]),"-PIAAC-sons-serviceclass.html"
                         )
        )
    }
}

rm(list=c(ls()[!ls() %in% ls2]))

