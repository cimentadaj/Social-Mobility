library(survey)
library(car)
library(stargazer)
library(arm)
library(broom)
library(tidyverse)
library(forcats)
library(visreg)
library(cimentadaj)
source("http://peterhaschke.com/Code/multiplot.R")

###### THIS IS WHERE YOU CHANGE YOUR WORKING DIRECTORY ##############
setwd("/Users/cimentadaj/Downloads/Social_mob_data")

# To save tables
directory <- "/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/Social-Mobility/Tables"

walk(list.files(pattern = ".rda"), load, .GlobalEnv)

ls2 <- c(ls()[grepl("*.design", ls())] , "ls2", "directory", "multiplot")
# Remove everything that is not in ls2 (so the .design )
rm(list= c(ls()[!ls() %in% ls2]))

countries3 <- list(# Austria=prgautp1.design,
                   USA=prgusap1.design,
                   # Belgium=prgbelp1.design,
                   Germany=prgdeup1.design,
                   Italy=prgitap1.design,
                   Netherlands=prgnldp1.design,
                   Denmark=prgdnkp1.design,
                   Sweden=prgswep1.design,
                   France=prgfrap1.design,
                   UK=prggbrp1.design,
                   Spain=prgespp1.design
                   # Canada=prgcanp1.design,
                   # Czech=prgczep1.design,
                   # Estonia=prgestp1.design,
                   #Finland=prgfinp1.design,
                   # Japan=prgjpnp1.design,
                   # Korea=prgkorp1.design,
                   # Norway=prgnorp1.design,
                   # Poland=prgpolp1.design,
                   # Russia=prgrusp1.design,
                   # Slovakia=prgsvkp1.design
                   )


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

svy_recode <- function(svy_design, old_varname, new_varname, recode) {
    
    svy_design2 <- lapply(svy_design, function(cnt) {
        for (data in seq_along(cnt$design)) {
            cnt$designs[[data]]$variables[, new_varname] <-
                car::recode(cnt$designs[[data]]$variables[, old_varname], recode)
        }
        cnt
    })
    
    svy_design2
    
}

# New occupation var
countries3 <- svy_recode(countries3, 'isco', 'occupation_recode', '1:2 = 5; 3 = 4; 4:5 = 3; 6:7 = 2; 8:9 = 1')
countries3 <- svy_recode(countries3, 'isco', 'occupation_recode_rev', '1:2 = 1; 3 = 2; 4:5 = 3; 6:7 = 4; 8:9 = 5')

# Long distance variables
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_upward', '5:4 = 4; 3 = 3; 2 = 2; 1 = 1')
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_downward', '1:2 = 1; 3 = 2; 4 = 3; 5 = 4')

##### Model Specification #####
dv <- "occupation_recode"
depvar_title <- "Upward-5 categories (top collapsed)"
out_name <- "-PIAAC-sons-5categories_upward.html"

all_firstcovariates <- c("highisced", "adv","non.cognitive",
                         "age_categories")

usa_secondcovariates <- c("lowmidisced2", "disadv","non.cognitive",
                          "age_categories")

all_secondcovariates <- c("lowisced", "disadv","non.cognitive",
                          "age_categories")

covariate_labels <- c("High ISCED","High ISCED - Low cogn", "Low ISCED - High cogn",
                      "Low ISCED", "Non.cognitive", "Age")
digits <- 2

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


        lower1 <- models("lowerclass", all_firstcovariates, subset(countries3[[1]], gender == 1 ))
        lower2 <- models("lowerclass", usa_secondcovariates, subset(countries3[[1]], gender == 1 ))


        lower.models <- append(lower1, lower2)

        ## Tables
        setwd(directory)
        all <- stargazer2(lower.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-lowerclass"),
                          column.labels = c("1= Lower Class", "1=Lower Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,5),
                          covariate.labels = covariate_labels,
                          digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-lowerclass.html"
                          )
        )

        #################################### Models for middle class ######################################################

        # middle1 <- models("middleclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        # middle2 <- models("middleclass", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))
        # 
        # middle.models <- append(middle1, middle2)
        # 
        # ## Tables
        # setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        # all <- stargazer2(middle.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-middleclass"),
        #                   column.labels = c("1= Middle Class", "1=Middle Class"),
        #                   column.separate = rep(length(all_firstcovariates), 2),
        #                   dep.var.labels.include = FALSE,
        #                   order = c(1,7),
        #                   covariate.labels = covariate_labels, digits = digits,
        #                   out = paste0(names(countries3[i]),"-PIAAC-sons-middleclass.html"
        #                   )
        # )

        #################################### Models for service class ######################################################


        high1 <- models("serviceclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        high2 <- models("serviceclass", usa_secondcovariates, subset(countries3[[i]], gender == 1 ))

        high.models <- append(high1, high2)


        ## Tables
        setwd(directory)
        all <- stargazer2(high.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
                          column.labels = c("1= Service Class", "1=Service Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,5),
                          covariate.labels = covariate_labels,
                          digits = digits,
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
        setwd(directory)
        all <- stargazer2(lower.models, odd.ratio = T,  type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-lowerclass"),
                          column.labels = c("1= Lower Class", "1=Lower Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,5),
                          covariate.labels = covariate_labels,
                          digits = digits,
                          out = paste0(names(countries3[i]),"-PIAAC-sons-lowerclass.html"
                          )
        )


        #################################### Models for middle class ######################################################

        # middle1 <- models("middleclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        # middle2 <- models("middleclass", all_secondcovariates, subset(countries3[[i]], gender == 1 ))
        # 
        # middle.models <- append(middle1, middle2)
        # 
        # ## Tables
        # setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis/Tables")
        # all <- stargazer2(middle.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-middleclass"),
        #                   column.labels = c("1= Middle Class", "1=Middle Class"),
        #                   column.separate = rep(length(all_firstcovariates), 2),
        #                   dep.var.labels.include = FALSE,
        #                   order = c(1,7),
        #                   covariate.labels = covariate_labels, digits = digits,
        #                   out = paste0(names(countries3[i]),"-PIAAC-sons-middleclass.html"
        #                   )
        # )

        #################################### Models for service class #####

        high1 <- models("serviceclass", all_firstcovariates, subset(countries3[[i]], gender == 1 ))
        high2 <- models("serviceclass", all_secondcovariates, subset(countries3[[i]], gender == 1 ))



        high.models <- append(high1, high2)


        ## Tables
        setwd(directory)
        all <- stargazer2(high.models, odd.ratio = T, type = "html", title = paste0(names(countries3[i]),"PIAAC-sons-serviceclass"),
                          column.labels = c("1= Service Class", "1=Service Class"),
                          column.separate = rep(length(all_firstcovariates), 2),
                          dep.var.labels.include = FALSE,
                          order = c(1,5),
                          covariate.labels = covariate_labels,
                          digits = digits,
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

graph_pred <- function(df_list, quant_var, model_to_extract, xvar) {
    
    df <- df_list[[1]]
    quant <- quantile(df$designs[[1]]$variables[, quant_var],
                      na.rm = T,
                      probs = c(0.30, 0.5, 0.70))
    
    plot1 <- visreg(model_to_extract, xvar, quant_var,
                    type = "conditional",
                    overlay = T,
                    plot = F)
    
    plot1$fit[, quant_var] <- as.factor(plot1$fit[, quant_var])
    plot1$fit
}

graph_pred_all <-  function(df, model1, model2, quant_var) {
    
    graph1 <- graph_pred(df, "pvnum", model1[[5]], "highisced")
    xvar <- "highisced"
    
    g1 <- ggplot(graph1, aes_string(xvar, "visregFit", colour = quant_var)) +
        geom_line(size = 1.5, alpha = 0.7) +
        scale_x_continuous(breaks = c(0, 1)) +
        xlab(paste0(ifelse(xvar == "highisced", "High", "Low"), "ISCED = 1")) +
        ylab("Occupation(1 - 4)") +
        ylim(1, 4) +
        labs(title = paste0(names(df))) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_discrete(guide = FALSE)
    
    xvar <- ifelse(names(df) == "USA","lowmidisced2", "lowisced")
    
    graph2 <- graph_pred(df, "pvnum", model2[[5]], xvar)
    
    g2 <- ggplot(graph2, aes_string(xvar, "visregFit", colour = quant_var)) +
        geom_line(size = 1.5, alpha = 0.7) +
        scale_x_continuous(breaks = c(0, 1)) +
        xlab(paste0(ifelse(xvar == "highisced", "High", "Low"), "ISCED = 1")) +
        scale_y_continuous(name = NULL, limits = c(1, 4), labels = NULL) +
        labs(title = paste0(names(df))) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_discrete(name = "Quantile",
                             labels = c("Bottom", "Middle", "High"))
    
    ggsave(paste0(names(df), ".jpeg"), plot = multiplot(g1, g2, cols = 2))
}

modeling_function <- function(df_list,
                              dv,
                              firstcovariates,
                              usa.secondcovariates,
                              secondcovariates,
                              covariate_labels,
                              digits,
                              out_name,
                              dir_tables,
                              depvar_title) {
    
last_models <- rep(list(vector("list", 2)), length(df_list))
names(last_models) <- names(df_list)

for (i in 1:length(df_list)) {
    
    # The low isced variable for USA combines both low and mid isced
    # Whenever the country is USA, use a different set of covariates
    # than with all other countries.
    if (names(df_list[i]) == "USA") {
        secondcovariates <- usa_secondcovariates
    } else {
        secondcovariates <- all_secondcovariates }
    
    mod1 <- models(dv, all_firstcovariates, subset(df_list[[i]], gender == 1 ))
    mod2 <- models(dv, secondcovariates, subset(df_list[[i]], gender == 1 ))
    
    last_models[[i]][[1]] <- tidy(mod1[[length(mod1)]])
    last_models[[i]][[2]] <- tidy(mod2[[length(mod1)]])
            
    # Calculate R squared for each model
    mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
    mod2_r <- paste0(sapply(mod2, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%")
            
    all.models <- append(mod1, mod2)
            
    ## Tables
    setwd(dir_tables)
    stargazer2(all.models, odd.ratio = F, type = "html",
               title = paste0(names(df_list[i]),"-Sons"),
               column.labels = rep(depvar_title, 2),
               column.separate = rep(length(all_firstcovariates), 2),
               dep.var.labels.include = FALSE,
               order = c(1, 2, 5, 6),
               covariate.labels = covariate_labels,
               digits = digits,
               out = paste0(names(df_list[i]), out_name),
               add.lines = list(c(mod1_r, mod2_r))
               )
    # graph_pred_all(df_list[i], mod1, mod2, "pvnum")
    }
  last_models
}

model_lists <-
    modeling_function(
        countries3,
        dv,
        all_firstcovariates,
        usa_secondcovariates,
        all_secondcovariates,
        covariate_labels,
        digits,
        out_name,
        directory,
        depvar_title)

summary_models <-
map(1:length(model_lists), function(country) { # loop through each country in model list
    
    map(model_lists[[country]], function(model) { # The loop through each model inside each country
        ind <- grep("adv|disadv", model$term)
        new_model <- model[ind, c("term", "estimate", "p.value")]
        cbind(new_model, impact = new_model[1, 2] / model[1, 2])
    })
})

# cbind the two models inside each country
merged_models <- map(summary_models, function(model) Reduce(rbind, model))

merged_models <-
    map(1:length(model_lists), function(country_index) {
    merged_models[[country_index]]$country <- names(model_lists)[country_index]
    merged_models[[country_index]]
    })


country_df <-
    merged_models %>%
    do.call("rbind", .) %>%
    mutate(term = rep(c("High ISCED - low cogn", "Low ISCED - High cogn"), nrow(.) / 2),
           p.value = round(p.value, 2),
           estimate = round(estimate, 2),
           impact = round(impact, 2)) %>%
    (function(df) {row.names(df) <- 1:nrow(df); df}) %>%
    arrange(country)

country_df %>%
    ggplot(aes(fct_reorder2(country, term, estimate, .desc = F),
               estimate, shape = term, colour = p.value < 0.05)) +
    geom_hline(yintercept = 0, alpha = 0.4) +
    geom_point(size = 2) +
    coord_flip() +
    scale_shape_discrete(name = NULL) +
    scale_y_continuous(breaks = seq(-2, 2, 0.20)) +
    labs(x = NULL, y = "Estimate")

ggsave("estimates_plot2.png")


country_list <-
    map(countries3, function(country) {
    country$designs[[1]]$variables %>%
    summarise(mean_HighISCEDlowcogn = mean(pvnum[adv == 1], na.rm = T),
              sd_HighISCEDlowcogn = sd(pvnum[adv == 1], na.rm = T),
              mean_LowISCEDhighcogn = mean(pvnum[disadv == 1], na.rm = T),
              sd_LowISCEDhighcogn = sd(pvnum[disadv == 1], na.rm = T)) %>%
    gather(key = name, value = values) %>%
    separate(name, c("metric", "class"), sep = "_") %>%
    select(class, everything())
})

summary_cntlist <-
    country_list %>%
    enframe() %>%
    unnest(value)

summary_cntlist %>%
    ggplot(aes(name, values, fill = metric)) +
    geom_col(position = "dodge") +
    facet_wrap(~ class) +
    coord_flip() +
    labs(x = "Countries", y = "Cognitive value") +
    coord_flip()

library(htmlTable)
summary_cntlist %>%
    spread(metric, values) %>%
    arrange(class) %>%
    (function(df) {
        num_cols <- map_lgl(df, is.numeric)
        df[num_cols] <- map_df(df[num_cols], round, 0)
        df
    }) %>%
    htmlTable()

ggsave("means_sds.png")

rm(list=c(ls()[!ls() %in% ls2]))

# Continous dependent both for positive DV and negative DV
# High and Low ISCED, cognitive, non-cognitive, age and constant

# Long distance variable
# High and Low ISCED, cognitive, non-cognitive, age and constant
# Same model with and withouth the cognitive

# Continous dependent both for positive DV and negative DV
# High and Low dummy, cognitive, non-cognitive, age and constant
# A table for each country with the high and low coefficients and the impact factor(division by constant)


