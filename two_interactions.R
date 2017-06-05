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
dv <- c("long_dist_upward", "long_dist_downward")
title_dep <- c("Continuous upward", "Continuous downward")

countrymod_list <-
    map(seq_along(dep), function(index) {
    
    dv <- dv[index]
    depvar_title <- title_dep[index]
    out_name <- paste0("-PIAAC-sons-", dep[index], "_prewelfare.html")
    age <- 1:10
    # 6:10 is prewelfare
    # 1:5 is postwelfare
    
    standard_covariates <- c("scale(pvnum)", "non.cognitive", "age_categories")
    
    all_firstcovariates <- c("highisced", "adv", standard_covariates)
    all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
    usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])
    
    covariate_labels <- c("High ISCED","High ISCED - Low cogn", "Low ISCED",
                          "Low ISCED - High cogn", "Cognitive", "Non-cognitive",
                          "Age categories")
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
                           subset(df_list[[i]], gender == 1 &
                                      age_categories %in% age_subset &
                                      postwelfare == 0),
                           family_models = family_models)
            mod2 <- models(dv, secondcovariates,
                           subset(df_list[[i]], gender == 1 & age_categories %in% age_subset &
                                      postwelfare == 0),
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
})

# list_broomer <- function(lst) {
#     map(lst, broom::tidy) %>%
#         enframe()
# }
# 
# list_rbind <- function(lst) {
#     map(lst, ~ reduce(.x, rbind))
# }
# 
# 
# summarized_df <-
#     map(countrymod_list, ~ map(.x, list_broomer)) %>%
#     list_rbind() %>%
#     map(~ mutate(.x, country = rep(names(countries3), each = 2))) %>%
#     map(unnest, value) %>%
#     map2(c("upward", "downward"), ~ mutate(.x, dv = .y)) %>%
#     reduce(rbind) %>%
#     select(name, country, term, estimate, dv)
# 
# df_for_cluster <-
#     summarized_df %>%
#     filter(grepl("isced$|pvnum|^non", term)) %>%
#     unite(term, term, dv, name, sep = "_") %>%
#     spread(term, estimate)
# 
# cluster_matrix <-
#     df_for_cluster %>%    
#     select(-country) %>%
#     as.matrix()
# 
# cnt_cluster <- 
#     cluster_matrix %>%
#     kmeans(centers = 5, nstart = 30)
# 
# df_for_cluster <-
#     df_for_cluster %>%
#     mutate(clusters = setNames(cnt_cluster$cluster, NULL)) %>%
#     select(country, clusters)
# 
# cluster_table <-
#     cnt_cluster$centers %>%
#     as_tibble() %>%
#     rownames_to_column(var = "clusters") %>%
#     mutate(clusters = as.integer(clusters)) %>%
#     left_join(df_for_cluster) %>%
#     select(country, clusters, contains("isced")) %>%
#     setNames(gsub("_1|_2", "", names(.)))
# 
# cluster_table <- as_tibble(map_if(cluster_table, is_double, round, 2))
# 
# htmlTable::htmlTableWidget(cluster_table, number_of_entries = 21)
# 
# hc.complete = hclust ( dist(cluster_matrix) , method ="complete")
# plot(hc.complete, main =" Complete Linkage ", xlab ="" , sub ="" ,
#      cex =.9, labels = df_for_cluster$country)

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


##### Variables #####
variables <- c("long_dist_upward", "long_dist_downward")
titles <- c("Continuous upward", "Continuous downward")

interaction_vars <- c(
    "cognitive_top30_bottom30", "noncognitive_top30_bottom30",
    "cognitive_top30_bottom50", "noncognitive_top30_bottom50",
    "cognitive_top20_bottom50", "noncognitive_top20_bottom50",
    "cognitive_top20_bottom40", "noncognitive_top20_bottom40"
)

walk2(variables, titles, function(dv, depvar_title) {
age <- 1:10; cohort <- "fullcohort"

standard_covariates <- c(interaction_vars,
                         "highcogn",
                         "highnon.cogn",
                         "lowcogn",
                         "lownon.cogn",
                         "pvnum",
                         "non.cognitive",
                         "age_categories",
                         "postwelfare",
                         "gender",
                         "highisced",
                         "lowisced",
                         "spfwt0")

all_firstcovariates <- standard_covariates

all_secondcovariates <- standard_covariates

digits <- 2

# All variables done
# Deleting any scale transformation
# Adding highedu variables for the next two plots
# Adding country variable that I create in the loop
# Adding cohort to identify pre/post welfare

vars_subset <- standard_covariates

# Loop through country datasets and names, create a column with that country's name
# and select all variables in vars_subset (which includes the country var)
cnts <- map2(countries3, names(countries3), function(data, names) {
    data$designs[[1]]$variables %>%
        mutate(country = names,
               cohort = ifelse(age_categories <= 5, "post", "pre")) %>%
        select_(.dots = map(c(vars_subset, dv, "country"), as.name))
})

cnt_bind <- Reduce(rbind, cnts)
cnt_bind$pvnum <- scale(cnt_bind$pvnum)
cnt_bind[interaction_vars] <- map_df(cnt_bind[interaction_vars], as.factor)

x_two <-
    tibble(cogn = interaction_vars[seq(1, length(interaction_vars), 2)],
           noncogn = interaction_vars[seq(2, length(interaction_vars), 2)]) %>%
    unite(interaction, cogn, noncogn, sep = "*")

# multilevel models
walk(1:nrow(x_two), ~ {
current_int <- as.character(x_two[.x, ])

standard_covariates <- c(current_int,
                         "pvnum",
                         "non.cognitive",
                         "age_categories", "postwelfare")

all_firstcovariates <- standard_covariates

# all_secondcovariates <- c("lowisced", standard_covariates)

random <- "(1 | country)"

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
        map(current_int, ~ static_formula(dv, c(iv, .x), random))
    }) %>%
    `c`(recursive = T)

# If the DV is not binery, run lmer, if it is, then use glmer
type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lmer", "glmer")

iv <- list(all_firstcovariates)[[1]]

multi_fun <-
    multi_fun <-
    switch(type_model,
           lmer = function(formula, data, subset, ...) {
               m <- match.call()
               m[[1]] <- as.name("lmer")
               eval(m)
           },
           glmer = function(formula, data, subset, ...) {
               m <- match.call()
               m[[1]] <- as.name("glmer")
               eval(m)
           }
    )

# Pass that list to the glmer to run two different models and then show table with stargazer
models_multilevel1 <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = gender == 1 & age_categories %in% age & highisced == 1)
})

models_multilevel2 <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = gender == 1 & age_categories %in% age & lowisced == 1)
})

models_multilevel <- list(models_multilevel1[[1]], models_multilevel2[[1]])

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
              # covariate.labels = covariate_labels,
              dep.var.labels = depvar_title,
              out = file.path(directory,
                              paste0(dv, "_", cohort, .x, "_interaction_multilevel_tables.html")),
              ...)
}

columns <- c("High isced = 1", "Low isced = 1")
stargazer_sequence(models_multilevel,
                   covariate_labels, depvar_title, directory,
                   cohort, column.labels = columns)
})

# lm models
walk(1:nrow(x_two), ~ {
    current_int <- as.character(x_two[.x, ])
    
    standard_covariates <- c("pvnum",
                             "non.cognitive",
                             "age_categories", "postwelfare")

    all_firstcovariates <- standard_covariates
    
    iv <- list(all_firstcovariates)[[1]]
    
    covariate_list <-
        map(list(all_firstcovariates), function(iv) {
            map(current_int, ~ as.formula(paste(dv, paste(c(.x, iv), collapse = " + "), sep = " ~ ")))
        }) %>%
        `c`(recursive = T)
    
    all_firstcovariates <- standard_covariates
    
    # If the DV is not binary, run lm, if it is, then use glm
    type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lm", "glm")
    
    multi_fun <-
        multi_fun <-
        switch(type_model,
               lm = function(formula, data, subset, ...) {
                   m <- match.call()
                   m[[1]] <- as.name("lm")
                   eval(m)
               },
               glmer = function(formula, data, subset, ...) {
                   m <- match.call()
                   m[[1]] <- as.name("glm")
                   eval(m)
               }
        )

    # Pass that list to the glmer to run two different models and then show table with stargazer
    models_multilevel1 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = cnt_bind,
                  subset = gender == 1 & age_categories %in% age & highisced == 1,
                  weight = spfwt0)
    })
    models_multilevel2 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = cnt_bind,
                  subset = gender == 1 & age_categories %in% age & lowisced == 1,
                  weight = spfwt0)
    })
    models_multilevel <- list(models_multilevel1[[1]], models_multilevel2[[1]])
    
# Remember to finish the stargazer3 in your package (and do it as object-oriented programming).
 
stargazer_sequence <- function(model, covariate_labels, depvar_title, directory, cohort, ...) {
        stargazer(model,
                  type = "html",
                  digits = 2,
                  # add.lines = list(c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2)))),
                  # c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
                  # c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
                  # c("Varying", rep("Intercept", length(model)))),
                  # covariate.labels = covariate_labels,
                  dep.var.labels = depvar_title,
                  out = file.path(directory,
                                  paste0(dv, "_", cohort, .x, "_interaction_pooled_linear.html")),
                  ...)
    }
    
    columns <- c("High isced = 1", "Low isced = 1")
    stargazer_sequence(models_multilevel,
                       covariate_labels, depvar_title, directory,
                       cohort, column.labels = columns)
})

# lm models scandinavia
walk(1:nrow(x_two), ~ {
    current_int <- as.character(x_two[.x, ])
    
    standard_covariates <- c(current_int,
                             "pvnum",
                             "non.cognitive",
                             "age_categories", "postwelfare")
    
    all_firstcovariates <- standard_covariates
    
    covariate_list <-
        map(list(all_firstcovariates), function(iv) {
            map(current_int, ~ as.formula(paste(dv, paste(c(.x, iv), collapse = " + "), sep = " ~ ")))
        }) %>%
        `c`(recursive = T)
    
    iv <- list(all_firstcovariates)[[1]]
    
    # If the DV is not binery, run lmer, if it is, then use glmer
    type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lm", "glm")
    
    multi_fun <-
        multi_fun <-
        switch(type_model,
               lm = function(formula, data, subset, ...) {
                   m <- match.call()
                   m[[1]] <- as.name("lm")
                   eval(m)
               },
               glmer = function(formula, data, subset, ...) {
                   m <- match.call()
                   m[[1]] <- as.name("glm")
                   eval(m)
               }
        )
    
    countries <- c("Denmark", "Sweden", "Norway", "Finland")
    # Pass that list to the glmer to run two different models and then show table with stargazer
    models_multilevel1 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = cnt_bind,
                  subset = gender == 1 & age_categories %in% age & highisced == 1 & country %in% countries,
                  weight = spfwt0)
    })
    models_multilevel2 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = cnt_bind,
                  subset = gender == 1 & age_categories %in% age & lowisced == 1 & country %in% countries,
                  weight = spfwt0)
    })
    
    models_multilevel <- list(models_multilevel1[[1]], models_multilevel2[[1]])
    
    # Remember to finish the stargazer3 in your package (and do it as object-oriented programming).
    
    stargazer_sequence <- function(model, covariate_labels, depvar_title, directory, cohort, ...) {
        stargazer(model,
                  type = "html",
                  digits = 2,
                  # add.lines = list(c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2)))),
                  # c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
                  # c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
                  # c("Varying", rep("Intercept", length(model)))),
                  # covariate.labels = covariate_labels,
                  dep.var.labels = depvar_title,
                  out = file.path(directory,
                                  paste0(dv, "_", cohort, .x, "_scandinavia_interaction_linear.html")),
                  ...)
    }
    columns <- c("High isced = 1", "Low isced = 1")
    stargazer_sequence(models_multilevel,
                       covariate_labels, depvar_title, directory,
                       cohort, column.labels = columns)
})
})


rm(list=c(ls()[!ls() %in% ls2]))

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
