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

# These two variables are recoded wrongly. It should be 1 = 0 and 2 = 1.
countries3 <- svy_recode(countries3, 'momimmigrant', 'momimmigrant', "'2' = 0; '1' = 1; else = NA")
countries3 <- svy_recode(countries3, 'dadimmigrant', 'dadimmigrant', "2 = 0; 1 = 1; else = NA")

#####

##### Variables #####

interaction_vars <- c(
    "cognitive_top20_bottom40", "noncognitive_top20_bottom40",
    "cognitive_top30_bottom30", "noncognitive_top30_bottom30"
)

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
                         "dadimmigrant",
                         "gender",
                         "highisced",
                         "lowisced",
                         "spfwt0",
                         "long_dist_downward",
                         "long_dist_upward")

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
        select_(.dots = map(c(vars_subset, "country"), as.name))
})

cnt_bind <- Reduce(rbind, cnts)
cnt_bind$pvnum <- scale(cnt_bind$pvnum)
cnt_bind[interaction_vars] <- map_df(cnt_bind[interaction_vars], as.factor)

x_two <-
    tibble(cogn = interaction_vars[seq(1, length(interaction_vars), 2)],
           noncogn = interaction_vars[seq(2, length(interaction_vars), 2)]) %>%
    unite(interaction, cogn, noncogn, sep = "*")

#####

##### multilevel models #####
dv <- c("long_dist_downward")
titles <- c("Continuous downward")

current_int <- as.character(x_two[1, ])

standard_covariates <- c(current_int,
                         "pvnum",
                         "non.cognitive",
                         "age_categories",
                         "postwelfare")

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
        map(current_int, ~ static_formula(dv, iv, random))
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
models_multilevel_downward <- list(models_multilevel1[[1]], models_multilevel2[[1]])

values_rbind <-
    map(models_multilevel_downward, ~ {
        n_obs <- nobs(.x)
        rsq <- NA
        rse <- NA
        
        data.frame(term = c("N", "R squared", "Residual s.e"), vals = c(n_obs, rsq, rse))
    }) %>%
    reduce(bind_cols) %>%
    setNames(c("term", "highisced", "", "lowisced")) %>%
    select(-3)

multilevel_downward_table_seven <-
    map(models_multilevel_downward, ~ {
    tidy(.x, conf.int = TRUE) %>%
        .[c(4:5, 2:3, 8, 6:7, 1), c("term", "estimate")]
        }) %>%
    reduce(cbind) %>%
    .[-3] %>%
    setNames(c("term", "highisced", "lowisced")) %>%
    bind_rows(values_rbind) %>%
    mutate_if(is_double, function(x) as.character(round(x, 2)))

multilevel_downward_table_seven

#####

##### lm models_upward_mockup ####
dv <- c("long_dist_upward")
titles <- c("Continuous upward")

current_int <- as.character(x_two[1, ])
    
standard_covariates <- c("pvnum",
                         "non.cognitive",
                         "age_categories",
                         "postwelfare")

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
models_multilevel_upward <- list(models_multilevel1[[1]], models_multilevel2[[1]])

values_rbind <-
    map(models_multilevel_upward, ~ {
    n_obs <- nobs(.x)
    rsq <- summary(.x)$adj.r.squared
    rse <- summary(.x)$sigma
    
    data.frame(term = c("N", "R squared", "Residual s.e"), vals = c(n_obs, rsq, rse))
}) %>%
    reduce(bind_cols) %>%
    setNames(c("term", "highisced", "", "lowisced")) %>%
    select(-3) %>%
    mutate_if(is_double, function(x) as.character(round(x, 2)))

star_paster <- function(df, var_one, var_two) {
    df[[var_two]] <- round(df[[var_two]], 2)
    sapply(1:nrow(df), function(index) {
        
        is_na <- is.na(df[[var_one]][index])
        three_stars <- df[[var_one]][index] < 0.001
        two_stars <- df[[var_one]][index] > 0.001 & df[[var_one]][index] <= 0.01
        one_stars <- df[[var_one]][index] > 0.01 & df[[var_one]][index] <= 0.05
        
        if (is_na) {
            
            NA
            
        } else if (three_stars) {
            
            paste(df[[var_two]][index], "***")
            
        } else if (two_stars) {
            
            paste(df[[var_two]][index], "**")
            
        } else if (one_stars) {
            
            paste(df[[var_two]][index], "*")
            
        } else {
            
            df[[var_two]][index]
        }
    })
    
    
}

multilevel_upward_table_seven <-
    map(models_multilevel_upward, ~ {
        tidy(.x) %>%
            .[c(4:5, 2:3, 8, 6:7, 1), c("term", "estimate")]
    }) %>%
    reduce(cbind) %>%
    .[-3] %>%
    setNames(c("term", "highisced", "lowisced")) %>%
    mutate_if(is_double, function(x) as.character(round(x, 2))) %>%
    bind_rows(values_rbind)

multilevel_upward_table_seven

#####

##### Table seven_mockup! ####

table_seven_mockup <- 
    multilevel_upward_table_seven %>%
    bind_cols(multilevel_downward_table_seven) %>%
    select(-4) %>%
    setNames(c("term", "highisced_upward", "lowisced_upward",
               "highisced_downward", "lowisced_downward"))

dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(ReporteRs)

title_columns <-
    c("", "Upward mobility \n High-SES sons",
      "Upward mobility \n Low-SES sons",
      "Downward mobility \n High-SES sons",
      "Downward mobility \n Low-SES sons")

term_names <- c(
    "Cognitive skills",
    "Non-cognitive skills",
    "Cognitive top + \n Non-cognitive bottom",
    "Cognitive bottom + \n Non-cognitive top",
    "Cognitive top + \n non-cognitive bottom (x)
    cognitive bottom + \nnon-cognitive top",
    "age",
    "cohort",
    "constant",
    "N",
    "R-squared",
    "Residual s.e"
)

table_seven_mockup$term <- term_names

doc <- docx()

doc <- addTitle(doc, "Table 7_mockup")
doc <- addFlexTable(doc,
                    FlexTable(table_seven_mockup, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

#####

##### lm models_upward ####
dv <- c("long_dist_upward")
titles <- c("Continuous upward")

current_int <- as.character(x_two[2, ])

standard_covariates <- c("pvnum",
                         "non.cognitive",
                         "age_categories",
                         "postwelfare",
                         "dadimmigrant")

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
models_multilevel_upward <- list(models_multilevel1[[1]], models_multilevel2[[1]])

values_rbind <-
    map(models_multilevel_upward, ~ {
        n_obs <- nobs(.x)
        rsq <- summary(.x)$adj.r.squared
        rse <- summary(.x)$sigma
        
        data.frame(term = c("N", "R squared", "Residual s.e"), vals = c(n_obs, rsq, rse))
    }) %>%
    reduce(bind_cols) %>%
    setNames(c("term", "highisced", "", "lowisced")) %>%
    select(-3) %>%
    mutate_if(is_double, function(x) as.character(round(x, 2)))

multilevel_upward_table_seven <-
    map(models_multilevel_upward, ~ {
        tidy(.x) %>%
            .[c(4:5, 2:3, 9, 6:8, 1), c("term", "estimate", "p.value")]
    }) %>%
    reduce(cbind) %>%
    .[-4] %>%
    transmute(term,
              estimate = star_paster(., "p.value", "estimate"),
              estimate_one = star_paster(., "p.value.1", "estimate.1")) %>%
    setNames(c("term", "highisced", "lowisced")) %>%
    bind_rows(values_rbind)

multilevel_upward_table_seven

#####

##### lm models_downward ####
dv <- c("long_dist_downward")
titles <- c("Continuous downward")

current_int <- as.character(x_two[2, ])

standard_covariates <- c("pvnum",
                         "non.cognitive",
                         "age_categories",
                         "postwelfare",
                         "dadimmigrant")

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
models_multilevel_downward <- list(models_multilevel1[[1]], models_multilevel2[[1]])

values_rbind <-
    map(models_multilevel_downward, ~ {
        n_obs <- nobs(.x)
        rsq <- summary(.x)$adj.r.squared
        rse <- summary(.x)$sigma
        
        data.frame(term = c("N", "R squared", "Residual s.e"), vals = c(n_obs, rsq, rse))
    }) %>%
    reduce(bind_cols) %>%
    setNames(c("term", "highisced", "", "lowisced")) %>%
    select(-3) %>%
    mutate_if(is_double, function(x) as.character(round(x, 2)))


multilevel_downward_table_seven <-
    map(models_multilevel_downward, ~ {
        tidy(.x, conf.int = TRUE) %>%
            .[c(4:5, 2:3, 9, 6:8, 1), c("term", "estimate", "p.value")]
    }) %>%
    reduce(cbind) %>%
    .[-4] %>%
    transmute(term,
              estimate = star_paster(., "p.value", "estimate"),
              estimate_one = star_paster(., "p.value.1", "estimate.1")) %>%
    setNames(c("term", "highisced", "lowisced")) %>%
    bind_rows(values_rbind)

multilevel_downward_table_seven

#####

##### Table seven_forreal! ####

table_seven <- 
    multilevel_upward_table_seven %>%
    bind_cols(multilevel_downward_table_seven) %>%
    select(-4) %>%
    setNames(c("term", "highisced_upward", "lowisced_upward",
               "highisced_downward", "lowisced_downward")) %>%
    mutate_if(is_double, round, 2)

table_seven$term <- c(
    "Cognitive skills",
    "Non-cognitive skills",
    "Cognitive top + \n Non-cognitive bottom",
    "Cognitive bottom + \n Non-cognitive top",
    "Cognitive top + \n non-cognitive bottom (x)
    cognitive bottom + \nnon-cognitive top",
    "age",
    "cohort",
    "Dad immigrant",
    "constant",
    "N",
    "R-squared",
    "Residual s.e"
)


doc <- addTitle(doc, "Table 7")
doc <- addFlexTable(doc,
                    FlexTable(table_seven, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

writeDoc(doc, file = "./Tables/table_seven.docx")

#####

##### Interaction for table 7 table #####

variables_interaction <- c("pvnum", "non.cognitive",
                           "cognitive_", "noncognitive_",
                           "Intercept")

interaction_table_upwards <- 
    map(models_multilevel_upward, ~ {
        tidy(.x) %>%
            .[c(4:5, 2:3, 9, 6:8, 1), c("term", "estimate", "p.value")]
    }) %>%
    reduce(cbind) %>%
    .[c(1, 2, 5)] %>%
    setNames(c("term", "highisced", "lowisced"))

interaction_table_downwards <- 
    map(models_multilevel_downward, ~ {
        tidy(.x) %>%
            .[c(4:5, 2:3, 9, 6:8, 1), c("term", "estimate", "p.value")]
    }) %>%
    reduce(cbind) %>%
    .[c(1, 2, 5)] %>%
    setNames(c("term", "highisced", "lowisced"))


interaction_calculator <- function(df, vars_interaction) {
    new_inter <-
        df[grepl(paste0(vars_interaction, collapse = "|"),
                 df$term), ]
    
    tibble(status = c("topcogn_topnon",
                      "topcogn_bottnon",
                      "bottcogn_topnon",
                      "bottcogn_bottnon"),
           highisced = c(sum(new_inter[c(6, 1, 2, 5), 2]),
                         sum(new_inter[c(6, 1), 2]),
                         sum(new_inter[c(6, 2), 2]),
                         sum(new_inter[6, 2])),
           lowisced = c(sum(new_inter[c(6, 1, 2, 5), 3]),
                        sum(new_inter[c(6, 1), 3]),
                        sum(new_inter[c(6, 2), 3]),
                        sum(new_inter[6, 3])))
}

interaction_upwards <- interaction_calculator(interaction_table_upwards,
                                              variables_interaction)

interaction_downwards <- interaction_calculator(interaction_table_downwards,
                                                variables_interaction)

interaction_table_ready <-
    left_join(interaction_upwards, interaction_downwards, by = "status") %>%
    mutate(status = c("Top cognitive \n Top non cognitive",
                      "Top cognitive \n Bottom non cognitive",
                      "Bottom cognitive \n Top non cognitive",
                      "Bottom cognitive \n Bottom non cognitive")) %>%
    mutate_if(is_double, round, 2)

doc <- addTitle(doc, "Table interaction")
doc <- addFlexTable(doc,
                    FlexTable(interaction_table_ready, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

writeDoc(doc, file = "./Tables/table_seven.docx")

#####

##### Table 8 ####

#####

##### lm models scandinavia ####
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
######

rm(list=c(ls()[!ls() %in% ls2]))

# There's a mistake. Tabel 7 in the paper, the upward models are written from the pooled linear
# models but the downwrd models are crated from the multilevel models instead.
# You need to change the downward models to the pooled linear models.