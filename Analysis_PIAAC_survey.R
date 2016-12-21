library(survey)
library(car)
library(stargazer)
library(arm)
library(broom)
library(tidyverse)
library(visreg)

###### THIS IS WHERE YOU CHANGE YOUR WORKING DIRECTORY ##############
setwd("/Users/cimentadaj/Downloads/Social_mob_data")

data <- list.files(pattern = ".rda")

for (i in 1:length(data)) {
    load(data[i])
}

ls2 <- c(ls()[grepl("*.design", ls())] , "ls2")
# Remove everything that is not in ls2 (so the .design )
rm(list= c(ls()[!ls() %in% ls2]))

countries3 <- list(Austria=prgautp1.design,
                   USA=prgusap1.design,
                   Belgium=prgbelp1.design,
                   Germany=prgdeup1.design,
                   Italy=prgitap1.design,
                   Netherlands=prgnldp1.design,
                   Denmark=prgdnkp1.design,
                   Sweden=prgswep1.design,
                   France=prgfrap1.design,
                   UK=prggbrp1.design,
                   Spain=prgespp1.design,
                   Canada=prgcanp1.design,
                   Czech=prgczep1.design,
                   Estonia=prgestp1.design,
                   Finland=prgfinp1.design,
                   Japan=prgjpnp1.design,
                   Korea=prgkorp1.design,
                   Norway=prgnorp1.design,
                   Poland=prgpolp1.design,
                   Russia=prgrusp1.design,
                   Slovakia=prgsvkp1.design)

countries3 <- countries3[c("USA", "Germany", "Italy", "Denmark", "Sweden", "Spain")]


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
    results <- lapply(formulas, function(o) with(data, svyglm(o,))[[1]])
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

svy_recode <- function(svy_design, old_varname, new_varname, recode) {
    
    svy_design2 <- lapply(svy_design, function(cnt) {
        for (data in seq_along(cnt$design)) {
            cnt$designs[[data]]$variables[, new_varname] <- car::recode(cnt$designs[[data]]$variables[, old_varname], recode)
        }
        cnt
    })
    
    svy_design2
    
}

######

digits <- 2
all_firstcovariates <- c("highisced", "scale(pvnum)", "scale(non.cognitive)",
                         "age_categories", "scale(pvnum):highisced")

usa_secondcovariates <- c("lowmidisced2", "scale(pvnum)", "scale(non.cognitive)",
                          "age_categories", "scale(pvnum):lowmidisced2")

all_secondcovariates <- c("lowmidisced", "scale(pvnum)", "scale(non.cognitive)",
                          "age_categories", "scale(pvnum):lowmidisced")

covariate_labels <- c("High ISCED", "Low ISCED",
                       "Cognitive", "Non.cognitive",
                       "Age", "Cognitive * High ISCED",
                      "Cognitive * Low ISCED")

countries3 <- svy_recode(countries3, 'isco', 'occupation_cont', '1:2 = 4; 3 = 3; 4:7 = 2; 8:9 = 1')
countries3 <- svy_recode(countries3, 'isco', 'shortupper', "1:5 = 1; NA = NA; else = 0")
countries3 <- svy_recode(countries3, 'isco', 'shortdown', "1:5 = 1; NA = NA; else = 0")

##### Data preparation for interaction visualization #####

# Change data argument
# Specify dataframe
# Specify x and y variables
# Specify interaction separately

# interaction_data <- tidy(mod1[[length(mod1)]])
# intercept <- interaction_data[grep("Intercept", interaction_data), 2] # Intercept for ISCED == 0
# slope <- interaction_data[agrep("^scale(pvnum)$", interaction_data$term)[1], 2]
#
# intercept2 <- intercept + interaction_data[grep("highisced", interaction_data$term)[1], 2]
# slope2 <- slope + interaction_data[grep("highisced", interaction_data$term)[2], 2]
#
# coef_interactions <- data.frame(intercept = c(intercept, intercept2),
#                                 slopes = c(slope, slope2),
#                                 linetypes = c(1, 2),
#                                 category = c("Low ISCED", "High ISCED"))
#
# new_data <- data.frame(highisced = rep(c(3, 1), each = 3),
#                        "pvnum"= rep(c(-0.62, 0.04, 0.70), times = 2),
#                        'non.cognitive' = 0,
#                        age_categories = 0)
#
# predict(mod1[[5]], new_data)
#
# ggplot(mod1[[length(mod1)]]$survey.design$variables, aes(scale(pvnum), occupation_cont)) +
#     geom_point() +
#     geom_abline(data = coef_interactions, aes(intercept = intercept, slope = slope))
###############

svy_data <- lower2[[5]]$model
qua <- quantile(svy_data[, "scale(pvnum)", drop = T], probs = c(0.25, 0.5, 0.75))

df <- data.frame(lowmidisced2 = rep(c(1, 0), each = 3),
                 pvnum = rep(qua, times = 2),
                 non.cognitive = mean(svy_data$`scale(non.cognitive)`[[1]]),
                 age_categories = 5)

contra <- c(lowmidisced2 = 1,
            pvnum = -0.66,
            non.cognitive = 0.03,
            age_categories = 5,
            "lowmidisced2:scale(pvnum)" = 2)
svycontrast(lower2[[5]], contra)

list(avg1 = setNames(as.numeric(df[1, ]), names(df)),
     avg2 = setNames(as.numeric(df[2, ]), names(df)),
     avg3 = setNames(as.numeric(df[3, ]), names(df)),
     avg4 = setNames(as.numeric(df[4, ]), names(df)),
     avg5 = setNames(as.numeric(df[5, ]), names(df)),
     avg6 = setNames(as.numeric(df[6, ]), names(df)))

predict(lower2[[5]],
        newdata = df,
        type = "link",
        se.fit = T,
        vcov = T)

# ##### Data preparation for simulation #######
# empty_data <- data.frame(col1 = rep(NA, 1000))
#
# repeated_data <- setNames(replicate(6, empty_data, simplify = F),
#                           c("lower1", "lower2", "middle1", "middle2", "high1", "high2"))
#
# simulation <- rep(list(repeated_data),
#                   length(countries3))
#
# simulation <- setNames(simulation, names(countries3))
#
# coefi <- data.frame(countries = rep(names(simulation), each = 6),
#                     type_coefs = rep(c("adv", "disadv"), each = 3),
#                     coefs = NA,
#                     type = c(grep("1", names(repeated_data), val = T),
#                              grep("1", names(repeated_data), inv = T, val = T)))
#
# simulated_check <- function(data_simulate, country_list, country_name) {
#
#     for (m in names(data_simulate[[country_name]])) {
#     tidy_stats <- tidy(get(m)[[3]])[-1, ]
#     dist <- rnorm(1000, tidy_stats[1, 2], tidy_stats[1, 3])
#
#     data_simulate[[c(country_name, m)]][,1] <- dist
#     data_simulate[[c(country_name, m)]][,2] <- data_simulate[[c(country_name, m)]][,1] - 2 * sd(data_simulate[[c(country_name, m)]][,1])
#     data_simulate[[c(country_name, m)]][,3] <- data_simulate[[c(country_name, m)]][,1] + 2 * sd(data_simulate[[c(country_name, m)]][,1])
#     }
#
#     data_simulate
# }
#
# ######

for (i in 1:length(countries3)) {

    if (names(countries3[i]) == "USA") {
        #################################### Models for lower class ##############################################

        lower1 <- models("lowerclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        lower2 <- models("lowerclass", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))

        lower.models <- append(lower1, lower2)

        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(lower.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-lowerclass"),
                          column.labels = c("1= Lower Class", "1=Lower Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,7),
                          covariate.labels = covariate_labels, digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-lowerclass.html"
                          )
        )

        #################################### Models for middle class ######################################################

        middle1 <- models("middleclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        middle2 <- models("middleclass", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))

        middle.models <- append(middle1, middle2)

        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(middle.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-middleclass"),
                          column.labels = c("1= Middle Class", "1=Middle Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,7),
                          covariate.labels = covariate_labels, digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-middleclass.html"
                          )
        )

        #################################### Models for service class ######################################################


        high1 <- models("serviceclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        high2 <- models("serviceclass", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))

        high.models <- append(high1, high2)


        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(high.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
                          column.labels = c("1= Service Class", "1=Service Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,7),
                          covariate.labels = covariate_labels, digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-serviceclass.html"
                          )
        )

        # adv <- c(tidy(lower1[[3]])[2, 2], tidy(middle1[[3]])[2, 2], tidy(high1[[3]])[2, 2])
        # disadv <- c(tidy(lower2[[3]])[2, 2], tidy(middle2[[3]])[2, 2], tidy(high2[[3]])[2, 2])
        # 
        # coefi[coefi$countries == names(countries3)[i], 3] <- c(adv, disadv)
        # 
        # simulation <- simulated_check(simulation, countries3, names(countries3)[i])

    } else {

        #################################### Models for lower class ##############################################

        lower1 <- models("lowerclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        lower2 <- models("lowerclass", all_secondcovariates, subset(countries3[[i]], gender == 1 ))

        lower.models <- append(lower1, lower2)

        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(lower.models, odd.ratio = T,  type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-lowerclass"),
                          column.labels = c("1= Lower Class", "1=Lower Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,7),
                          covariate.labels = covariate_labels, digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-lowerclass.html"
                          )
        )


        #################################### Models for middle class ######################################################

        middle1 <- models("middleclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        middle2 <- models("middleclass", all_secondcovariates, subset(countries3[[i]], gender == 1 ))

        middle.models <- append(middle1, middle2)

        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(middle.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-middleclass"),
                          column.labels = c("1= Middle Class", "1=Middle Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,7),
                          covariate.labels = covariate_labels, digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-middleclass.html"
                          )
        )

        #################################### Models for service class #####

        high1 <- models("serviceclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        high2 <- models("serviceclass", all_secondcovariates, subset(countries3[[i]], gender == 1 ))

        high.models <- append(high1, high2)


        ## Tables
        setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        all <- stargazer2(high.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
                          column.labels = c("1= Service Class", "1=Service Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,7),
                          covariate.labels = covariate_labels, digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-serviceclass.html"
                          )
        )

     # adv <- c(tidy(lower1[[3]])[2, 2], tidy(middle1[[3]])[2, 2], tidy(high1[[3]])[2, 2])
     # disadv <- c(tidy(lower2[[3]])[2, 2], tidy(middle2[[3]])[2, 2], tidy(high2[[3]])[2, 2])
     # coefi[coefi$countries == names(countries3)[i], 3] <- c(adv, disadv)
     # 
     # simulation <- simulated_check(simulation, countries3, names(countries3)[i])
    }
}

# 
# ##### Data preparation for simulation #####
# simulated_summary <- lapply(simulation, function(cnt) lapply(cnt, function(vec) exp(colMeans(vec))))
# 
# df <- as.data.frame(do.call(rbind, (lapply(1:6, function(i) do.call(rbind,
#                                                                     lapply(simulated_summary, `[[`, i))))))
# 
# df$country <- row.names(df)
# df <- df[order(row.names(df)), ]
# row.names(df) <- 1:nrow(df)
# df$type <- names(repeated_data)
# 
# lookup_classes <- c(
#              high1 = "Service class - High edu low cogn",
#              high2 = "Service class - Low edu high cogn",
#              middle1 = "Middle class - High edu low cogn",
#              middle2 = "Middle class - Low edu high cogn",
#              lower1 = "Lower class - High edu low cogn",
#              lower2 = "Lower class - Low edu high cogn")
# 
# df$type <- lookup_classes[df$type]
# coefi$type <- lookup_classes[as.character(coefi$type)]
# # Graph of the simulation
# 
# ggplot(df, aes(country, col1)) +
#     geom_hline(yintercept = 1) +
#     geom_point(size = 2, alpha = 0.5, colour = 'black') +
#     geom_errorbar(aes(ymin = V2, ymax = V3)) +
#     facet_wrap(~ type) +
#     geom_point(data = coefi, aes(countries, exp(coefs), colour = "red"), alpha = 0.5, size = 2) +
#     scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
#     ylab("Odds ratios") + xlab("Countries") +
#     guides(colour = F)
# 
# ###################

########
# for (i in 1:length(countries3)) {
#     
#     if (names(countries3[i]) == "USA") {
#         
#         mod1 <- models("occupation_cont", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
#         mod2 <- models("occupation_cont", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))
#         
#         all.models <- append(mod1, mod2)
#         
#         ## Tables
#         setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
#         all <- stargazer2(all.models, odd.ratio = F, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
#                           column.labels = c("1 = Occupation continuous", "1 = Occupation continuous"),
#                           column.separate = rep(length(all_firstcovariates), 2),
#                           dep.var.labels.include = FALSE,
#                           order = c(1,7),
#                           covariate.labels = covariate_labels, digits = digits,
#                           out = paste0(names(countries3[i]),"-PIAAC-sons-occupation_cont.html"
#                           )
#         )
#         
# 
#     } else {
#         
#         mod1 <- models("occupation_cont", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
#         mod2 <- models("occupation_cont", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))
#         
#         all.models <- append(mod1, mod2)
#         
#         ## Tables
#         setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
#         all <- stargazer2(all.models, odd.ratio = F, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
#                           column.labels = c("1 = Occupation continuous", "1 = Occupation continuous"),
#                           column.separate = rep(length(all_firstcovariates), 2),
#                           dep.var.labels.include = FALSE,
#                           order = c(1,7),
#                           covariate.labels = covariate_labels, digits = digits,
#                           out = paste0(names(countries3[i]),"-PIAAC-sons-occupation_cont.html"
#                           )
#         )
#     }
# }
########


rm(list=c(ls()[!ls() %in% ls2]))
