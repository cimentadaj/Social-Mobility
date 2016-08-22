library(survey)
library(stargazer)

rm(list=ls()[!ls() %in% c("countrylist","usable.country2")])
setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")

Austria <- load('prgautp1.rda')
USA <- load('prgusap1.rda')
Belgium <- load('prgbelp1.rda')
Germany <- load('prgdeup1.rda')
Canada <- load('prgcanp1.rda')
Italy <- load('prgitap1.rda')
Netherlands <- load('prgnldp1.rda')
Denmark <- load('prgdnkp1.rda')
Sweden <- load('prgswep1.rda')
France <- load('prgfrap1.rda')
UK <- load('prggbrp1.rda')
Spain <- load('prgespp1.rda')
Czech <- load('prgczep1.rda')
Estonia <- load('prgestp1.rda')
Finland <- load('prgfinp1.rda')
Ireland <- load('prgfinp1.rda')
Japan <- load('prgjpnp1.rda')
Korea <- load('prgkorp1.rda')
Norway <- load('prgnorp1.rda')
Poland <- load('prgpolp1.rda')
Russia <- load('prgrusp1.rda')
Slovakia <- load('prgsvkp1.rda')

ls2 <- c(ls()[grepl("*.design",ls())], "countrylist","usable.country2")
rm(list=ls()[!ls() %in% ls2])


countries3 <- list(Austria=prgautp1.design,USA=prgusap1.design,Belgium=prgbelp1.design,
                   Germany=prgdeup1.design,Italy=prgitap1.design,Netherlands=prgnldp1.design,
                   Denmark=prgdnkp1.design,Sweden=prgswep1.design,France=prgfrap1.design,
                   UK=prggbrp1.design,Spain=prgespp1.design,Canada=prgcanp1.design, Czech=prgczep1.design,
                   Estonia=prgestp1.design, Finland=prgfinp1.design, Japan=prgjpnp1.design, Korea=prgkorp1.design,
                   Norway=prgnorp1.design,Poland=prgpolp1.design, Russia=prgrusp1.design, Slovakia=prgsvkp1.design)
p1 <- names(countries3)
p2 <- numeric(0)

for (i in 1:length(countries3)) {
    p2 <- c(p2,with(countries3[[i]],svymean(~serviceclass, na.rm=T, rho=NULL,
                                 return.replicates=FALSE, deff=FALSE))[[1]][1])
}
names(p2) <- 1:length(p2)

for (i in 1:length(countries3)) {
    
    workdataset3 <- subset(countries3[[i]], age_categories >= 3 & age_categories <= 6 & gender == 1) ## From 25-45 BOYS
    workdataset4 <- subset(countries3[[i]], age_categories >= 7 & age_categories <= 10 & gender == 1) ## From 45-60 BOYS
    
    
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
        
        ## Models for MEN from the 25-45 cohort
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
        
        #     ## Models for MEN from the 25-45 cohort
        #     m1 <- with(workdataset3, svyglm(lowerclass ~ highisced  , family = quasibinomial()))
        #     #m2 <- with(workdataset3, svyglm(lowerclass ~ highisced  + numbooks, family = quasibinomial()))
        #     m3 <- with(workdataset3, svyglm(lowerclass ~ highisced   + scale(pvnum), family = quasibinomial()))
        #     m3.1 <- with(workdataset3, svyglm(lowerclass ~ highisced   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     m4 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced2  , family = quasibinomial()))
        #     #m5 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced2  + numbooks , family = quasibinomial()))
        #     m6 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced2   + scale(pvnum), family = quasibinomial()))
        #     m6.1 <- with(workdataset3, svyglm(lowerclass ~ lowmidisced2   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     ## Models for MEN from 45-65
        #     m7 <- with(workdataset4, svyglm(lowerclass ~ highisced  , family = quasibinomial()))
        #     #m8 <- with(workdataset4, svyglm(lowerclass ~ highisced  + numbooks, family = quasibinomial()))
        #     m9 <- with(workdataset4, svyglm(lowerclass ~ highisced   + scale(pvnum), family = quasibinomial()))
        #     m9.1 <- with(workdataset4, svyglm(lowerclass ~ highisced   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        #     
        #     
        #     m10 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced2  , family = quasibinomial()))
        #     #m11 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced2  + numbooks , family = quasibinomial()))
        #     m12 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced2   + scale(pvnum), family = quasibinomial()))
        #     m12.1 <- with(workdataset4, svyglm(lowerclass ~ lowmidisced2   + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
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
        #                      covariate.labels = c("Highest ISCED","DadISCED1-2",
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
        #                      covariate.labels = c("Highest ISCED","DadISCED1-2",
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
        
        ## Models for MEN from the 25-45 cohort
        m1 <- with(workdataset3, svyglm(serviceclass ~ highisced  , family = quasibinomial()))
        m2 <- with(workdataset3, svyglm(serviceclass ~ highisced  + non.cognitive, family = quasibinomial()))
        #m3 <- with(workdataset3, svyglm(serviceclass ~ highisced  + non.cognitive + scale(pvnum), family = quasibinomial()))
        
        m4 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced2  , family = quasibinomial()))
        m5 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced2  + non.cognitive , family = quasibinomial()))
        #m6 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced2   + non.cognitive + scale(pvnum), family = quasibinomial()))
        #m6.1 <- with(workdataset3, svyglm(serviceclass ~ lowmidisced2   + non.cognitive + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        
        ## Models for MEN from 45-65
        m7 <- with(workdataset4, svyglm(serviceclass ~ highisced  , family = quasibinomial()))
        m8 <- with(workdataset4, svyglm(serviceclass ~ highisced  + non.cognitive, family = quasibinomial()))
        #m9 <- with(workdataset4, svyglm(serviceclass ~ highisced   + non.cognitive + scale(pvnum), family = quasibinomial()))
        #m9.1 <- with(workdataset4, svyglm(serviceclass ~ highisced  + non.cognitive + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        
        
        m10 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced2  , family = quasibinomial()))
        m11 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced2  + non.cognitive, family = quasibinomial()))
        #m12 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced2  + non.cognitive + scale(pvnum), family = quasibinomial()))
        #m12.1 <- with(workdataset4, svyglm(serviceclass ~ lowmidisced2  + non.cognitive + scale(pvnum) + scale(non.cognitive), family = quasibinomial()))
        
        
        highyoung <- list(m1[[1]],m2[[1]],m4[[1]],m5[[1]])
        highold <- list(m7[[1]],m8[[1]],m10[[1]],m11[[1]])
        
        ## Tables for YOUNG service class BOYS
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")
        all <- stargazer(highyoung, type = "html", title = paste0(names(countries3[i]),"PIAAC-25-45-sons-serviceclass"),
                         column.labels = c("1= Service Class", "1=Service Class"),
                         column.separate = c(2,2),
                         dep.var.labels.include = FALSE,
                         order = c(1,3),
                         covariate.labels = c("Highest ISCED","DadISCED1-2", "Non.cognitive"),
                         apply.coef = exp,
                         apply.ci = exp, digits = 2
                         , out = paste0(names(countries3[i]),"-PIAAC-25-45-sons-serviceclass.html"
                         )
        )
        
        ## Tables for old service class BOYS
        all <- stargazer(highold, type = "html", title = paste0(names(countries3[i]),"PIAAC-45-65-sons-serviceclass"),
                         column.labels = c("1= Service Class", "1=Service Class"),
                         column.separate = c(2,2),
                         dep.var.labels.include = FALSE,
                         order = c(1,3),
                         covariate.labels = c("Highest ISCED","DadISCED1-2", "Non.cognitive"),
                         apply.coef = exp,
                         apply.ci = exp, digits = 2
                         , out = paste0(names(countries3[i]),"-PIAAC-45-65-sons-serviceclass.html")
        )
    }
}

rm(list=ls()[!ls() %in% ls2])

