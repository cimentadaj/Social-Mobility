library(survey)
library(car)
library(stargazer)
library(arm)
library(broom)
library(tidyverse)
library(forcats)
library(visreg)
library(cimentadaj)
library(ggthemes)
library(lme4)
library(MuMIn)
source("http://peterhaschke.com/Code/multiplot.R")

###### THIS IS WHERE YOU CHANGE YOUR WORKING DIRECTORY ##############
old_dir <- getwd()

# To save tables
directory <- "./Tables"

setwd("/Users/cimentadaj/Downloads/Social_mob_data")

walk(list.files(pattern = ".rda"), load, .GlobalEnv)

ls2 <- c(ls()[grepl("*.design", ls())] , "ls2", "directory", "old_dir", "multiplot")
# Remove everything that is not in ls2 (so the .design )
rm(list= c(ls()[!ls() %in% ls2]))

setwd(old_dir)

countries3 <- list(Austria = prgautp1.design,
                   `United States` = prgusap1.design,
                   Belgium = prgbelp1.design,
                   Germany = prgdeup1.design,
                   Italy = prgitap1.design,
                   Netherlands = prgnldp1.design,
                   Denmark = prgdnkp1.design,
                   Sweden = prgswep1.design,
                   France = prgfrap1.design,
                   `United Kingdom` = prggbrp1.design,
                   Spain = prgespp1.design,
                   Canada = prgcanp1.design,
                   `Czech Republic`= prgczep1.design,
                   Estonia = prgestp1.design,
                   Finland = prgfinp1.design,
                   Japan = prgjpnp1.design,
                   Korea = prgkorp1.design,
                   Norway = prgnorp1.design,
                   Poland = prgpolp1.design,
                   `Russian Federation` = prgrusp1.design,
                   `Slovak Republic` = prgsvkp1.design
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
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_downward', '1:2 = 4; 3 = 3; 4 = 2; 5 = 1')

countries3 <- svy_recode(countries3, 'isco', 'lowerclass', '3:9 = 1; 1:2 = 0; else = NA')
countries3 <- svy_recode(countries3, 'age_categories', 'postwelfare', '1:5 = 1; 6:10 = 0; else = NA')

##### Model Specification #####
standard_covariates <- c("scale(pvnum)", "non.cognitive", "age_categories")

all_firstcovariates <- c("highisced", "adv", standard_covariates)
all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])
age <- 1:10
dv <- "long_dist_upward"


dep <- c("long_dist_downward")
title_dep <- c("Continuous working class")
for (index in seq_along(dep)) {

dv <- dep[index]
depvar_title <- title_dep[index]
out_name <- paste0("-PIAAC-sons-", dep[index], ".html")
age <- 1:10
# 6:10 is prewelfare
# 1:5 is postwelfare

covariate_labels <- c("High ISCED","High ISCED - Low cogn", "Low ISCED",
                      "Low ISCED - High cogn", "Cognitive", "Non-cognitive",
                      "Postwelfare", "Age categories")
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

# Function tests the logical statement and if it doesn't equal T, it gives the error_message.
stop_message <- function(logical_statement, error_message) {
    if(logical_statement) stop(error_message, call. = F)
}
warning_message <- function(logical_statement, error_message) {
    if(logical_statement) warning(error_message, call. = F)
}

df_list <- countries3

modeling_function <- function(df_list,
                              dv,
                              firstcovariates,
                              usa_secondcovariates,
                              secondcovariates,
                              age_subset,
                              family_models = "gaussian",
                              covariate_labels,
                              digits,
                              out_name,
                              dir_tables,
                              depvar_title) {
    
    stop_message(length(df_list) < 1, "df_list is empty")
    last_models <- rep(list(vector("list", 2)), length(df_list))
    names(last_models) <- names(df_list)
    
    # Odd ratios or not?
    # This should be done to identify whether DV is a dummy or not
    dv_length_countries <-
        map_dbl(df_list, function(.x) 
            unique(.x$designs[[1]]$variables[, dv]) %>%
            na.omit() %>%
            length())

    # If the number of countries equals 1, bring the only length, if not, sample from all countries
    len <- ifelse(length(dv_length_countries) == 1,
                  dv_length_countries,
                  sample(dv_length_countries, 1))
    
    stop_message(!all(len == dv_length_countries),
                 "The length of the dependent variable differs by country")
    stop_message(!(len >= 2),
                 "DV has length < 2")
    
    odd.ratio <- ifelse(family_models == "gaussian", F,
                        unname(ifelse(sample(dv_length_countries, 1) == 2, T, F)))

for (i in 1:length(df_list)) {
    
    # The low isced variable for USA combines both low and mid isced
    # Whenever the country is USA, use a different set of covariates
    # than with all other countries.
    if (names(df_list[i]) == "USA") {
        secondcovariates <- usa_secondcovariates
    } else {
        secondcovariates <- all_secondcovariates }
    
    mod1 <- models(dv, all_firstcovariates,
                   subset(df_list[[i]], gender == 1 & age_categories %in% age_subset),
                   family_models = family_models)
    mod2 <- models(dv, secondcovariates,
                   subset(df_list[[i]], gender == 1 & age_categories %in% age_subset),
                   family_models = family_models)
    
    last_models[[i]][[1]] <- mod1[[length(mod1)]] # length(mod1) to only get the last (complete model)
    last_models[[i]][[2]] <- mod2[[length(mod1)]]
            
    # Calculate R squared for each model
    mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
    mod2_r <- paste0(sapply(mod2, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%")
            
    all.models <- append(mod1, mod2)
            
    ## Tables
    stargazer2(all.models, odd.ratio, type = "html",
               title = paste0(names(df_list[i])),
               column.labels = rep(depvar_title, 2),
               column.separate = rep(length(all_firstcovariates), 2),
               dep.var.labels.include = FALSE,
               order = c(1, 2, 7, 8),
               covariate.labels = covariate_labels,
               digits = digits,
               out = file.path(dir_tables, paste0(names(df_list[i]), out_name)),
               add.lines = list(c(mod1_r, mod2_r))
               )
    # graph_pred_all(df_list[i], mod1, mod2, "pvnum")
    }
  last_models
}

family_models <- "gaussian"
model_lists <-
    modeling_function(
        df_list = countries3,
        dv = dv,
        firstcovariates = all_firstcovariates,
        usa_secondcovariates = usa_secondcovariates,
        secondcovariates = all_secondcovariates,
        age_subset = age,
        family_models = family_models,
        covariate_labels = covariate_labels,
        digits = digits,
        out_name = out_name,
        dir_tables = directory,
        depvar_title = depvar_title)
}


# To produce regressions for cognitive and non cognitive against
# age variable for different categories.
# For cognitive I ran it for Korea, Italy and Denmark.
# For non.cognitive I ran it for Estonia, Italy and US
# Change the countries inside the loop in the modeling_function call.

ability_vars <- c("non.cognitive")
ability_title <- c("Non cognitive")
index <- 1

ability_list <-
    map(seq_along(ability_vars), function(index) {
        
        dv <- ability_vars[index]
        depvar_title <- ability_title[index]
        out_name <- paste0("ability-PIAAC-sons-", ability_vars[index], ".html")
        age <- 1:10
        # 6:10 is prewelfare
        # 1:5 is postwelfare
        
        standard_covariates <- c("age_categories")
        digits <- 2
        
        # Function tests the logical statement and if it doesn't equal T, it gives the error_message.
        stop_message <- function(logical_statement, error_message) {
            if(logical_statement) stop(error_message, call. = F)
        }
        warning_message <- function(logical_statement, error_message) {
            if(logical_statement) warning(error_message, call. = F)
        }
        
        modeling_function <- function(df_list,
                                      dv,
                                      standard_covariates,
                                      age_subset,
                                      family_models = "gaussian",
                                      digits,
                                      out_name,
                                      dir_tables,
                                      depvar_title) {
            
            stop_message(length(df_list) < 1, "df_list is empty")
            last_models <- rep(list(vector("list", 2)), length(df_list))
            names(last_models) <- names(df_list)
            
            for (i in 1:length(df_list)) {
                
                
                mod1 <- models(dv, standard_covariates,
                               subset(df_list[[i]], gender == 1 &
                                          age_categories %in% age_subset &
                                          postwelfare == 0),
                               family_models = family_models)
                
                last_models[[i]][[1]] <- mod1[[length(mod1)]] # length(mod1) to only get the last (complete model)
                
                # Calculate R squared for each model
                mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
                
                ## Tables
                stargazer2(mod1, odd.ratio = F, type = "html",
                           title = paste0(names(df_list[i])),
                           column.labels = depvar_title,
                           column.separate = length(standard_covariates),
                           dep.var.labels.include = FALSE,
                           digits = digits,
                           out = file.path(dir_tables, paste0(names(df_list[i]), out_name)))
                # add.lines = list(c(mod1_r, mod2_r))
            }
            last_models
        }
        
        family_models <- "gaussian"
        model_lists <-
            modeling_function(
                df_list = countries3[c("Estonia", "Italy", "United States")],
                dv = dv,
                standard_covariates = standard_covariates,
                age_subset = age,
                family_models = family_models,
                digits = digits,
                out_name = out_name,
                dir_tables = directory,
                depvar_title = depvar_title)
    })

# summary_models <-
# map(1:length(model_lists), function(country) { # loop through each country in model list
#     
#     map(model_lists[[country]], function(model) { # The loop through each model inside each country
#         ind <- grep("adv|disadv", model$term)
#         new_model <- model[ind, c("term", "estimate", "p.value")]
#         cbind(new_model, impact = new_model[1, 2] / model[1, 2])
#     })
# })
# 
# # cbind the two models inside each country
# merged_models <- map(summary_models, function(model) Reduce(rbind, model))
# 
# merged_models <-
#     map(1:length(model_lists), function(country_index) {
#     merged_models[[country_index]]$country <- names(model_lists)[country_index]
#     merged_models[[country_index]]
#     })
# 
# 
# country_df <-
#     merged_models %>%
#     do.call("rbind", .) %>%
#     mutate(term = rep(c("High ISCED - low cogn", "Low ISCED - High cogn"), nrow(.) / 2),
#            p.value = round(p.value, 2),
#            estimate = round(estimate, 2),
#            impact = round(impact, 2)) %>%
#     (function(df) {row.names(df) <- 1:nrow(df); df}) %>%
#     arrange(country)
# 
# country_df %>%
#     ggplot(aes(fct_reorder2(country, term, estimate, .desc = F),
#                estimate, shape = term, colour = p.value < 0.05)) +
#     geom_hline(yintercept = 0, alpha = 0.4) +
#     geom_point(size = 2) +
#     coord_flip() +
#     scale_shape_discrete(name = NULL) +
#     scale_y_continuous(breaks = seq(-2, 2, 0.20)) +
#     labs(x = NULL, y = "Estimate")
# 
# ggsave("estimates_plot2.png")

## Download the xlsx from here http://www.oecd.org/employment/emp/EPL-timeseries.xlsx
## Download the second sheet and convert to CSV
epl <- read_csv("./country_level_variables/epl_summary_pagetwo.csv")[c(2, 3, 7)]

latest_epl <-
    epl %>%
    filter(year == 2012)

# Creating 2012 EPL lookup and then creating variable
epl_all_lookup <- setNames(latest_epl$eprc_v1, latest_epl$country)

## EPL is just from 2012 to 1985. A person born in 1967 + 25 years is higher than 1985
## A person born in 1945 + 25 years

## Social Expenditure
## Cite this
## Original link: http://stats.oecd.org/Index.aspx?datasetcode=SOCX_AGG#
## Download as csv and name it oecdsocialspending.csv
## Social Expenditure as % of GDP.
oecd <- read_csv("./country_level_variables/oecdsocialspending.csv")

oecd_tidy <-
    oecd %>%
    filter(Source == "Public" &
           Branch == "Total" &
          `Type of Expenditure` == "Total" &
          `Type of Programme` == "Total" &
           UNIT == "PCT_GDP") %>%
    rename(cntry = Country) %>%
    select(cntry, Year, Value) %>%
    spread(Year, Value) %>%
    map_if(is.numeric, ~ round(.x, 1)) %>%
    as_tibble()

cnts_available_welfare

latest_welfare <-
    oecd_tidy %>%
    gather(year, percentage, -cntry) %>%
    filter(year == 2013)

welfare_lookup <- with(latest_welfare, setNames(percentage, cntry))
# Russian % of GDP still missing.


# Tracking data taken from Checchi paper
# Imputed Estonia which is not there from this
# information: http://www.oecd.org/edu/highlightsEstonia.htm
trackingdata <- tibble(
    cntry = c("Austria","Belgium","Canada","Chile","Czech Republic","Denmark",
              "Finland","France","Germany","Hungary","Ireland","Italy","Japan",
              "Korea","Netherlands","New Zealand","Norway","Poland","Russian Federation",
              "Slovak Republic","Slovenia","Spain","Sweden","Switzerland",
              "United Kingdom","United States", "Estonia"),
    
    `1980` = c(10,12,18,14,15,16,16,16,10,10,12,14,15,14,12,18,16,15,15,10,15,14,16,15.5,16,18,15),
    `2002` = c(10,12,18,13,11,16,16,15,10,11,15,14,15,14,12,16,16,15,15,11,15,16,16,16,16,18,15)
)

latest_tracking <-
    trackingdata %>%
    gather(year, value, -cntry) %>%
    filter(year == 2002)

tracking_lookup <- with(latest_tracking, setNames(value, cntry))

# Share of enrolment by type of institution (early education)
# Download from: http://stats.oecd.org/Index.aspx?datasetcode=SOCX_AGG#

earlyedu_att <- read_csv("./country_level_variables/earlyeducation_attendance.csv")

earlyedu_att_summary <-
    earlyedu_att %>%
    filter(AGE %in% c("Y0T2_OECD", "Y3T4_OECD") & # Early childhood education (ISCED2011 level 0)
           Sex == "Total") %>% # Public institutions
    rename(country = Country, value = Value, age = Age) %>%
    mutate(age = c(`0 to 2 years` = "0_to_2", `3 to 4 years` = "3_to_4")[age]) %>%
    select(country, age, value) %>%
    spread(age, value)

earlyedu_lookup <- with(earlyedu_att_summary, setNames(`3_to_4`, country))

# Share of enrolment by type of institution (early education)
# Download from: http://stats.oecd.org/Index.aspx?datasetcode=SOCX_AGG#

earlyedu_weight <- read_csv("./country_level_variables/earlyeducation_weighted.csv")

names(earlyedu_weight) <-
    c("country", "X2", "participation_rate", "avg_hours_week", "fullt_participation_rate")

earlyedu_att_weight <-
    earlyedu_weight %>%
    select(-X2)

earlyedu_weight_lookup <- with(earlyedu_att_weight, setNames(fullt_participation_rate, country))

names(earlyedu_weight_lookup) <-
    gsub("\\(.*)", "", names(earlyedu_weight_lookup)) %>%
    trimws()

# Share of enrolment by type of institution (early education)
# Download from: http://stats.oecd.org/Index.aspx?datasetcode=SOCX_AGG#

edu_enrollment <- read_csv("./country_level_variables/enrollment_by_edu.csv", skip = 1)
colnames(edu_enrollment) <- c("countries", "overall", "low_edu", "high_edu", "p_val")

edu_enrollment$countries <- trimws(gsub("\\*|\\(.*)", "", edu_enrollment$countries))

lowedu_enrollment_lookup <- with(edu_enrollment, setNames(low_edu, countries))
highedu_enrollment_lookup <- with(edu_enrollment, setNames(high_edu, countries))


# * % employed in public sector
# * proportion of labor force in professional type jobs
# (i.e. in our classes 4 and 5). An alternative is to weight the jobs distribution directly
# 
# * unemployment levels
# * clientelism/corruption index

##### Variables #####
# I will use all of the variables from the first models from above.
cohort <- "young_cohort"

random <- "(1 | country)"
random_two <- "(1 + highisced | country)"
random_three <- "(1 + lowisced | country)"
country_vars <- c("tracking_all") # country-level variables but for all models

# All variables done
# Deleting any scale transformation
# Adding highedu variables for the next two plots
# Adding country variable that I create in the loop
# Adding cohort to identify pre/post welfare

unique_second <- setdiff(all_secondcovariates, all_firstcovariates)

vars_subset <-
    gsub("scale|\\(|\\)", "", c(unique_second, all_firstcovariates)) %>%
    `c`("highedu", "country", "cohort", "gender", "postwelfare", dv)

# Loop through country datasets and names, create a column with that country's name
# and select all variables in vars_subset (which includes the country var)
cnts <- map2(countries3, names(countries3), function(data, names) {
    data$designs[[1]]$variables %>%
        mutate(country = names,
               cohort = ifelse(age_categories <= 5, "post", "pre")) %>%
        select_(.dots = map(vars_subset, as.name))
})

cnt_bind <- Reduce(rbind, cnts)

#### Country level lookups

# EPL
cnt_bind$epl_all <- epl_all_lookup[cnt_bind$country]
# Social expenditure
cnt_bind$welfare_all <- welfare_lookup[cnt_bind$country]
# Tracking
cnt_bind$tracking_all <- tracking_lookup[cnt_bind$country]
# Preschool attendance
cnt_bind$preschool_att <- earlyedu_lookup[cnt_bind$country]
# Preschool attendance weighted by full/part time
cnt_bind$preschool_att_weight <- earlyedu_weight_lookup[cnt_bind$country]
# Enrollment by low edu
cnt_bind$lowedu_enrollment <- lowedu_enrollment_lookup[cnt_bind$country]
# Enrollment by high edu
cnt_bind$highedu_enrollment <- highedu_enrollment_lookup[cnt_bind$country]

# Service class
cnt_countrylevel <- map(countries3, ~ {
    df <- .x$designs[[1]]$variables
    
    perc_industry <-
        df %>%
        mutate(isco_short = recode(isco, `2` = 1, `8` = 9)) %>%
        filter(isco_short %in% c(1, 9)) %>%
        count(postwelfare, isco_short) %>%
        filter(!is.na(isco_short)) %>%
        mutate(
            perc = round(n / sum(n) * 100, 1),
            type_industry = c("perc_service", "perc_manual")
        ) %>%
        select(-n, -isco_short) %>%
        spread(type_industry, perc)
    
    perc_industry
})

cnt_bind <-
    cnt_countrylevel %>%
    enframe() %>%
    unnest(value) %>%
    right_join(cnt_bind, by = c("name" = "country", "postwelfare")) %>%
    rename(country = name)

# ##### Cognitive distribution graphs #####
# cogn <-
#     cnt_bind %>%
#     filter(highedu %in% c(1, 3)) %>%
#     ggplot(aes(pvnum, fill = as.character(highedu))) +
#     geom_density(alpha = 0.3) +
#     labs(x = "Cognitive distribution", y = "Density") +
#     scale_fill_discrete(name = "Social class",
#                         labels = c("Low ISCED", "Higher ISCED")) +
#     scale_y_continuous(labels = round(seq(0, 0.0075, 0.0025), 3),
#                        breaks = round(seq(0, 0.0075, 0.0025), 3)) +
#     theme_minimal()
# 
# non_cogn <-
#     cnt_bind %>%
#     filter(highedu %in% c(1, 3)) %>%
#     ggplot(aes(non.cognitive, fill = as.character(highedu))) +
#     geom_density(alpha = 0.3) +
#     labs(x = "Non-cognitive distribution", y = "Density") +
#     scale_fill_discrete(name = "Social class",
#                         labels = c("Low ISCED", "Higher ISCED")) +
#     theme_minimal()
# #####

## Section on multilevel modeling

# Adding model specific country-variables
# all_firstcovariates <- c(all_firstcovariates, "highedu_enrollment")
# all_secondcovariates <- c(all_secondcovariates, "lowedu_enrollment")

# Loop through each sets of covariates and create a formula containing the DV,
# the covariates and the random component.
# covariate_list <-
#     map(list(all_firstcovariates, all_secondcovariates), function(covariates) {
#         
#     base_model <- paste(covariates, collapse = " + ")
#     extended_model <- paste(base_model, country_vars, sep = " + ")
#     all_models <- c(base_model, extended_model)
#         
#     paste(dv, " ~ ", paste(all_models, random, sep = " + ")) %>%
#     map(as.formula)
# })

rhs_sequence <- function(iv) {
    stop_message(length(iv) < 1, "iv must have length >= 1")
    warning_message(any(is.na(iv)), "NA's found in iv. Removing them.")
    
    non_na_iv <- na.omit(iv)
    model_combination <- map(seq_along(non_na_iv), ~ seq(1:.x))
    rhs <- map(model_combination, ~ paste(non_na_iv[.x], collapse = " + "))
    
    rhs
}
static_formula <- function(dv, rhs, random) {
    new_dv <- paste0(dv, " ~ 1")
    rhs <- paste0(c(rhs, random), collapse = " + ")
    as.formula(paste0(c(new_dv, rhs), collapse = " + "))
}
formula_builder <- function(dv, sequence, fixed) {
    
    if (!missing(fixed)) {
        fixed <- paste0(c(dv, paste(fixed, collapse = " + ")), collapse = " ~ ")
    } else {
        fixed <- paste(dv, "~ 1")
    }
    
    map_chr(rhs_sequence(sequence), ~ paste(c(fixed, .x), collapse = " + "))
}
formula_generator <- function(dv, sequence, fixed) {
    map(formula_builder(dv, sequence, fixed), as.formula)
}
form_random_effects <- function(dv, sequence, random_effects, fixed) {
    random_formulas <- paste(formula_builder(dv, sequence, fixed), random_effects, sep = " + ")
    map(random_formulas, as.formula)
}

# Turn that nested list into a flat list
# covariate_list <- c(covariate_list, recursive = T)

# covariate_list <- c(static_formula(dv, all_firstcovariates, random),
#                     static_formula(dv, c(all_firstcovariates, "highedu_enrollment"), random),
#                     static_formula(dv, all_secondcovariates, random),
#                     static_formula(dv, c(all_secondcovariates, "lowedu_enrollment"), random))

covariate_list <- list(static_formula(dv, all_firstcovariates, random_two),
                       static_formula(dv, all_secondcovariates, random_three))

# If the DV is not binary, run lmer, if it is, then use glmer
type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lmer", "glmer")

multi_fun <-
    switch(type_model,
           lmer = function(formula, data, subset, ...) {
               lmer(formula = formula,
                     data = subset(data, eval(parse(text = subset))),
                    ...)
           },
           glmer = function(formula, data, subset, ...) {
               glmer(formula = formula,
                     data = subset(data, eval(parse(text = subset))),
                     family = "binomial", ...)
        })

# Pass that list to the glmer to run two different models and then show table with stargazer
models_multilevel <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age")
})


stargazer3 <- function(model, odd.ratio = FALSE, ...) {
    
    if (!("list" %in% class(model))) model <- list(model)
    
    if (odd.ratio) {
        # Get coefficients
        coef_table <- purrr::map(model, ~ as.data.frame(summary(.x)$coefficients))
        
        # Estimate odds for all models
        odds <- purrr::map(coef_table, ~ exp(.x[, 1]))
        
        # Loop through odds and SE and multiply them
        oddsSE <- purrr::map2(odds, coef_table, ~ .x * .y[, 2])
        
        # Get p vals from models
        p_vals <- purrr::map(coef_table, ~ .x[, 4])
        
        stargazer::stargazer(model,
                             coef = odds,
                             se = oddsSE,
                             p = p_vals, ...)
        
    } else {
        stargazer::stargazer(model, ...)
    }
    
}

# Remember to finish the stargazer3 in your package (and do it as object-oriented programming).
stargazer_linear <- function(model, covariate_labels, depvar_title, directory, cohort) {
    stargazer(model,
              type = "html",
              digits = 2,
              add.lines = list(c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2)))),
                               # c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
                               # c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
                               # c("Varying", rep("Intercept", length(model)))),
              covariate.labels = covariate_labels,
              dep.var.labels = depvar_title,
              out = file.path(directory, paste0(dv, "_", cohort, "_multilevel_tables.html")))
    
}
stargazer_binomial <- function(model, covariate_labels, depvar_title, directory, cohort) {
    stargazer3(model, odd.ratio = T,
              type = "html",
              digits = 2,
              add.lines = list(c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
                                c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
                                c("Varying", rep("Intercept", length(model)))),
              covariate.labels = covariate_labels,
              dep.var.labels = depvar_title,
              out = file.path(directory, paste0(dv, "_", cohort, "_multilevel_tables.html")))
    
}

stargazer_linear(models_multilevel, covariate_labels, depvar_title, directory, cohort)
# stargazer_binomial(models_multilevel, covariate_labels, depvar_title, directory, cohort)
# 
# countryvars_formula_low <- form_random_effects(dv, country_vars, random, all_secondcovariates)
# countryvars_formula_high <- form_random_effects(dv, country_vars, random, all_firstcovariates)
# 
# all_models <- c(countryvars_formula_high, countryvars_formula_low)
# 
# models <- map(all_models, ~ glmer(.x, subset = age_categories %in% age, data = cnt_bind,
#                                   family = "binomial"))
# 
# 
# stargazer_sequence2(models,
#                    c(covariate_labels,"Tracking",
#                      "% enrollment Lowedu mom",
#                      "% enrollment highedu mom"),
#                    depvar_title,
#                    directory,
#                    cohort)

term_grabber <- function(model, term) {
    df <- tidy(model) 
    df[grep(term, df$term), 1:3]
}

list_model_todf <- function(list, term) {
    map(list, term_grabber, term) %>%
        enframe() %>%
        unnest(value)
}

term <- c("highisced", "lowisced")
breaks_term <- term
term_regex <- paste0("^", term, collapse = "|")
labels_term <- c("High ISCED", "Low ISCED")
order_term <- "lowisced"
term_colour <- c("blue", "blue", "green", "red")

# term <- c("adv", "disadv")
# breaks_term <- term
# labels_term <- c("High ISCED - Low cogn", "Low ISCED - High cogn")
# term_regex <- paste0("^", term, collapse = "|")
# order_term <- term[2]
# term_colour <- c("#00743F", "blue", "blue", "red")

# term <- c("pvnum", "non.cognitive")
# breaks_term <- c(paste0("scale(", term[1], ")"), term[2])
# labels_term <- c("Cognitive", "Non cognitive")
# term_regex <- paste0(term, collapse = "|")
# order_term <- term[2]
# term_colour <- c("blue", "blue", "#00743F", "red")

avg_slopes <-
    list_model_todf(models_multilevel, term_regex)[1:2, ] %>%
    transmute(country = "Average slope",
              term, estimate, std.error) %>%
    mutate(term = c("avg_slope_high", "avg_slope_low"))

cnt_slopes <-
    map(model_lists, ~ {
    list_model_todf(.x, term_regex)[1:2, ]
}) %>%
    enframe(name = 'country') %>%
    unnest(value)

df_countries <-
    cnt_slopes %>%
    select(-name) %>%
    rbind(avg_slopes) %>%
    mutate(lower = estimate - 1.96 * std.error,
           upper = estimate + 1.96 * std.error)

order_cnt <-
    df_countries %>%
    filter(term %in% c(order_term, "avg_slope_low")) %>%
    arrange(desc(estimate)) %>%
    distinct(country) %>%
    .[["country"]]

trans <- 0.5
axis_color <- c(rep("grey36", 10), "blue", rep("grey36", 11))
blue_index <- which(order_cnt == "Average slope")
country_color <-
    c(rep("grey36", blue_index - 1), "blue", rep("grey36", length(order_cnt) - blue_index))

df_countries %>%
    mutate(country = factor(country, levels = order_cnt, ordered = T),
           term = as.factor(term)) %>%
    ggplot(aes(country, estimate, colour = term)) +
    geom_point(alpha = trans) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, alpha = trans - 0.2) +
    scale_colour_manual(name = "Slope",
                      breaks = breaks_term,
                      labels = labels_term,
                      values = term_colour) +
    xlab(NULL) +
    ylab("Estimated slope with 95% uncertainty intervals") +
    ggtitle("Chances of downward mobility") +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y=element_text(colour = country_color))

ggsave("cognitive_noncognitive_downward.png", path = directory)

cnt_slopes %>%
    select(-name, -std.error) %>%
    spread(term, estimate) %>%
    ggplot(aes(highisced, lowisced)) +
    geom_smooth(method = "lm") +
    geom_point(alpha = 0.3) +
    ggrepel::geom_label_repel(aes(label = country)) +
    theme_minimal() +
    xlab("High ISCED Slope") +
    ylab("Low ISCED Slope") +
    ggtitle("Chances of upward mobility - R = -0.65 and -0.41 without Ita, Spa and Ger")

# cnt_slopes %>%
#     select(-name, -std.error) %>%
#     spread(term, estimate) %>%
#     filter(!country %in% c("Spain", "Italy", "Germany")) %>%
#     summarize(cor = cor(highisced, lowisced))

ggsave("cor_high_low_isced_upward.png", path = directory)

# Multilevel models
random <- "(1 | country)"
# country_vars <- c("tracking_all", "epl_all", "welfare_all", "preschool_att", "preschool_att_weight",
#                   "lowedu_enrollment", "highedu_enrollment") # country-level variables but for all models

country_vars <- c("perc_service", "perc_manual")

covariate_list <- 
    map(list(all_firstcovariates, all_secondcovariates), function(.x) {
        map(country_vars, function(.y) static_formula(dv, c(.x, .y), random))
    })

# If the DV is not binary, run lmer, if it is, then use glmer
type_model <- ifelse(length(na.omit(unique(cnt_bind[[dv]]))) > 2, "lmer", "glmer")

multi_fun <-
    switch(type_model,
           lmer = function(formula, data, subset, ...) {
               lmer(formula = formula,
                    data = subset(data, eval(parse(text = subset))),
                    ...)
           },
           glmer = function(formula, data, subset, ...) {
               glmer(formula = formula,
                     data = subset(data, eval(parse(text = subset))),
                     family = "binomial", ...)
           })

# Pass that list to the glmer to run two different models and then show table with stargazer
models_multilevel_one <- map(covariate_list[[1]], function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age & postwelfare == 1")
})

models_multilevel_two <- map(covariate_list[[2]], function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age & postwelfare == 1")
})

adv <- c("High ISCED", "High ISCED - Low cogn")
disadv <- c("Low ISCED", "Low ISCED - High cogn")

covariate_labels_two <- c("Cognitive", "Noncognitive", "Age categories")

covariate_labels_adv <- c(adv, covariate_labels_two, "% Service", "% Manual")
covariate_labels_disadv <- c(disadv, covariate_labels_two, "% Service", "% Manual")
depvar_title <- "Continuous upward"

# model <- models_multilevel_one
# covariate_labels <- c(adv, covariate_labels_two)

stargazer_linear <- function(model, covariate_labels, depvar_title, directory, cohort) {
    stargazer(model,
              type = "html",
              digits = 2,
              add.lines = list(
              c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2))),
              c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
              c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
              c("Varying", rep("Intercept", length(model)))
              ),
              covariate.labels = covariate_labels,
              dep.var.labels = depvar_title,
              out = file.path(directory,
                              paste0(dv, "_", cohort, "_multilevel_tables.html")))
}

stargazer_linear(model = models_multilevel_one,
                 covariate_labels = c(covariate_labels_adv, "Constant"),
                 depvar_title = depvar_title,
                 directory = directory,
                 cohort = paste0(cohort, "_highisced_"))

stargazer_linear(model = models_multilevel_two,
                 covariate_labels = c(covariate_labels_disadv, "Constant"),
                 depvar_title = depvar_title,
                 directory = directory,
                 cohort = paste0(cohort, "_lowisced"))

    
# rm(list=c(ls()[!ls() %in% ls2]))

# Simulation to increase state-level reforms and see how the elasticity of students changes.

# - Check Germany if we have separation dummy between east and west.
# - Maybe conduct analysis leaving out East Germany.

# International data on Germany doesn't have indicator for East/West Germany.
# However, German PIAAC Microdata has state indicators. But this data has to be
# requested + it might be different.

# - Conduct K-nearest neighbor or dendogram
# - And statistical significant difference between each country
# - Plot the scatterplot of countries with CI's and average slope with CI (see plot in the paper).
# - Check the % of women employed in each country.
