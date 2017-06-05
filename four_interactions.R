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
dv <- "lowerclass"
depvar_title <- "Dummy lower class"
out_name <- "-PIAAC-sons-dummylower.html"
age <- 1:10

standard_covariates <- c("scale(pvnum)","non.cognitive", 'postwelfare')

all_firstcovariates <- c("highisced", "adv", standard_covariates)
all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])

covariate_labels <- c("High ISCED","High ISCED - Low cogn", "Low ISCED",
                      "Low ISCED - High cogn", "Cognitive", "Non.cognitive",
                      "Postwelfare")
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

# Function tests the logical statement and if it doesn't equal T, it gives the error_message.
stop_message <- function(logical_statement, error_message) {
    if(logical_statement) stop(error_message, call. = F)
}
warning_message <- function(logical_statement, error_message) {
    if(logical_statement) warning(error_message, call. = F)
}

modeling_function <- function(df_list,
                              dv,
                              firstcovariates,
                              usa_secondcovariates,
                              secondcovariates,
                              age_subset,
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
    len <- ifelse(length(dv_length_countries) == 1, dv_length_countries, sample(dv_length_countries, 1))
    
    stop_message(!all(len == dv_length_countries),
                 "The length of the dependent variable differs by country")
    stop_message(!(len >= 2),
                 "DV has length < 2")
    
    odd.ratio <- unname(ifelse(sample(dv_length_countries, 1) == 2, T, F))
    
    for (i in 1:length(df_list)) {
        
        # The low isced variable for USA combines both low and mid isced
        # Whenever the country is USA, use a different set of covariates
        # than with all other countries.
        if (names(df_list[i]) == "USA") {
            secondcovariates <- usa_secondcovariates
        } else {
            secondcovariates <- all_secondcovariates }
        
        mod1 <- models(dv, all_firstcovariates, subset(df_list[[i]], gender == 1))
        mod2 <- models(dv, secondcovariates, subset(df_list[[i]], gender == 1))
        
        last_models[[i]][[1]] <- tidy(mod1[[length(mod1)]])
        last_models[[i]][[2]] <- tidy(mod2[[length(mod1)]])
        
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
                   order = c(1, 2, 6, 7),
                   covariate.labels = covariate_labels,
                   digits = digits,
                   out = file.path(dir_tables, paste0(names(df_list[i]), out_name)),
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
        age,
        covariate_labels,
        digits,
        out_name,
        directory,
        depvar_title)
beepr::beep()

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

##### Variables #####
dv <- "serviceclass"
depvar_title <- "Service class dummy"
age <- 1:10; cohort <- "fullcohort"

standard_covariates <- c("high_low_cogn" ,"high_low_noncogn",
                         "highcogn", "highnon.cogn", "lowcogn", "lownon.cogn",
                         "age_categories", "postwelfare")

all_firstcovariates <- c(standard_covariates)

all_secondcovariates <- c(standard_covariates)

digits <- 2

# All variables done
# Deleting any scale transformation
# Adding highedu variables for the next two plots
# Adding country variable that I create in the loop
# Adding cohort to identify pre/post welfare

unique_second <- setdiff(all_secondcovariates, all_firstcovariates)

vars_subset <-
    gsub("scale|\\(|\\)", "", c(unique_second, all_firstcovariates)) %>%
    `c`("highedu", "country", "cohort", "gender", dv, "highisced", "lowisced")

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
interaction_vars <- c("highcogn" ,"highnon.cogn", "lowcogn", "lownon.cogn")

x_two <-
    combinations(4,2,interaction_vars) %>%
    as_tibble() %>%
    unite(interaction, V1, V2, sep = "*")

x_two <- x_two[-c(2, 5), ]

standard_covariates <- c(interaction_vars,
                         "age_categories", "postwelfare")

all_firstcovariates <- c(standard_covariates)

# all_secondcovariates <- c("lowisced", standard_covariates)

random <- "(1 | country)"
country_vars <- c("tracking_all") # country-level variables but for all models

covariate_labels <- c("Highcogn", "High noncogn", "Low cogn", "Low noncogn",
                      "Age", "Postwelfare cohort", "High cogn T * Low non.cogn F",
                      "High cogn T * High non.cogn F", "High non.cogn T * High cogn F",
                      "Low cogn T * High non.cogn F")

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

# covariate_list <- list(static_formula(dv, all_firstcovariates, random),
#                        static_formula(dv, all_secondcovariates, random))

covariate_list <-
    map(list(all_firstcovariates), function(iv) {
        map(x_two$interaction, ~ static_formula(dv, c(iv, .x), random))
    }) %>%
    `c`(recursive = T)

# If the DV is not binery, run lmer, if it is, then use glmer
# type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lmer", "glmer")
type_model <- "lmer"

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
models_multilevel1 <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age & highisced == 1")
})
models_multilevel2 <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age & lowisced == 1")
})

models_multilevel <- 
    list(models_multilevel1, models_multilevel2) %>%
    `c`(recursive = T)

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
stargazer_sequence <- function(model, covariate_labels, depvar_title, directory, cohort, ...) {
    stargazer(model,
              type = "html",
              digits = 2,
              add.lines = list(c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2)))),
              # c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
              # c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
              # c("Varying", rep("Intercept", length(model)))),
              covariate.labels = covariate_labels,
              dep.var.labels = depvar_title,
              out = file.path(directory, paste0(dv, "_4interaction_", cohort, "_multilevel_tables.html")),
              ...)
}
stargazer_sequence2 <- function(model, covariate_labels, depvar_title, directory, cohort, ...) {
    stargazer3(model, odd.ratio = T,
               type = "html",
               digits = 2,
               # add.lines = list(c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
               #                  c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
               #                  c("Varying", rep("Intercept", length(model)))),
               covariate.labels = covariate_labels,
               dep.var.labels = depvar_title,
               out = file.path(directory, paste0(dv, "_4interaction_", cohort, "_multilevel_tables.html")),
               ...)
    
}

columns <- rep(c("High isced = 1", "Low isced = 1"), each = 4)

stargazer_sequence(models_multilevel,
                   covariate_labels,
                   depvar_title,
                   directory,
                   cohort,
                   column.labels = columns)

stargazer_sequence2(models_multilevel, covariate_labels, depvar_title, directory, cohort)

countryvars_formula_low <- form_random_effects(dv, country_vars, random, all_secondcovariates)
countryvars_formula_high <- form_random_effects(dv, country_vars, random, all_firstcovariates)

all_models <- c(countryvars_formula_high, countryvars_formula_low)

models <- map(all_models, ~ glmer(.x, subset = age_categories %in% age, data = cnt_bind,
                                  family = "binomial"))


stargazer_sequence2(models,
                    c(covariate_labels,"Tracking",
                      "% enrollment Lowedu mom",
                      "% enrollment highedu mom"),
                    depvar_title,
                    directory,
                    cohort)



rm(list=c(ls()[!ls() %in% ls2]))

# Simulation to increase state-level reforms and see how the elasticity of students changes.
