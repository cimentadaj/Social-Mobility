#### Packages ####
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
#####

##### Reading data #####
###### THIS IS WHERE YOU CHANGE YOUR WORKING DIRECTORY
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
#####

##### Recoding variables ####
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
#####

##### Model Specification #####

standard_covariates <- c("scale(pvnum)",
                         "non.cognitive",
                         "age_categories",
                         "postwelfare",
                         "dadimmigrant")

# I'm running two models, one with highisced variables and another
# with lowisced variables, that's why I'm creating two separate set of
# independent variables.

# Finally, USA has a different lowisced variable, so I create a separate vector
# for US.
all_firstcovariates <- c("highisced", "adv", standard_covariates)
all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])

# I want all ages (categories from 1:10, not ages 1:10)
age <- 1:10
# 6:10 is prewelfare
# 1:5 is postwelfare


# Change for long_dist_upward and the title to produce models for the other variable
# models
dv <- c("long_dist_downward")
depvar_title <- c("Continuous working class")

#####

##### Modeling ####

out_name <- paste0("-PIAAC-sons-", dv, ".html")

covariate_labels <- c("High ISCED","High ISCED - Low cogn", "Low ISCED",
                      "Low ISCED - High cogn", "Cognitive", "Non-cognitive",
                      "Age categories", "Postwelfare", "Dad immigrant")
digits <- 2


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
               order = c(1, 2, 8, 9),
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


# Descriptives

descriptive_table <-
    countries3 %>%
    enframe(name = "Country")

descriptive_table$descriptive <-
    map(descriptive_table$value, function(.x) {
    .x$designs[[1]]$variables %>%
        group_by(postwelfare) %>%
        summarize(`Sample size` = n(),
                  `% High ISCED` = mean(highisced, na.rm = T) * 100,
                  `% Low ISCED` = mean(lowisced, na.rm = T) * 100,
                  `% Dad immigrant` = mean(dadimmigrant, na.rm = T) * 100,
                  `Avg numeracy skills` = mean(pvnum, na.rm = T)) %>%
        map_if(is_numeric, round, 0) %>%
        as_tibble() %>%
        gather(terms, vals, -postwelfare) %>%
        unite(postwelfare, postwelfare, terms , sep = "_") %>%
        spread(postwelfare, vals)
})

descriptive_table <-
    descriptive_table %>%
    dplyr::select(-value) %>%
    unnest(descriptive) %>%
    setNames(c("Country",
               rep(c("% Dad immigrant",
                     "% High ISCED", "% Low ISCED", "Avg numeracy skills", "Sample size"), 2)))

dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(ReporteRs)

FlexTable(descriptive_table) %>%
    addHeaderRow(text.properties = textBold(),
                 value = c("", "Pre-welfare cohort", "Post-welfare cohort"),
                 colspan = c(1, 5, 5),
                 first = TRUE)

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

##### Variables #####
# I will use all of the variables from the first models from above.
cohort <- "young_cohort"

random <- "(1 | country)"
random_two <- "(1 + highisced | country)"
random_three <- "(1 + lowisced | country)"

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
            type_industry = rep(c("perc_service", "perc_manual"), 2)
        ) %>%
        dplyr::select(-n, -isco_short) %>%
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
