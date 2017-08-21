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

dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(ReporteRs)

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

# countries3 <- list(Israel = prgisrp1.design)

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


##### Recoding variables ####

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
cnt_bind$pvnum <- scale(cnt_bind$pvnum) %>% .[, 1]
cnt_bind[interaction_vars] <- map_df(cnt_bind[interaction_vars], as.factor)

x_two <-
    tibble(cogn = interaction_vars[seq(1, length(interaction_vars), 2)],
           noncogn = interaction_vars[seq(2, length(interaction_vars), 2)]) %>%
    unite(interaction, cogn, noncogn, sep = "*")

#####



##### lm models_upward ####
dv <- c("long_dist_upward")
titles <- c("Continuous upward")

current_int <- as.character(x_two[1, ])

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
           glm = function(formula, data, subset, ...) {
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

current_int <- as.character(x_two[1, ])

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
           glm = function(formula, data, subset, ...) {
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

title_columns <- c(
    "",
    "Upward Mobility \n 
    High-SES sons",
    "Upward Mobility \n
    Low-SES sons",
    "Downward mobility \n
    High-SES sons",
    "Downward mobility \n
    Low-SES sons"
)

doc <- docx()
doc <- addTitle(doc, "Table 7")
doc <- addFlexTable(doc,
                    FlexTable(table_seven, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

#####

##### Table 8 = Interaction for table 7 table #####

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

df <- interaction_table_downwards
vars_interaction <- variables_interaction

interaction_calculator <- function(df, vars_interaction) {
    new_inter <-
        df[grepl(paste0(vars_interaction, collapse = "|"),
                 df$term), ]
    
    tibble(status = c("topcogn_topnon",
                      "topcogn_bottnon",
                      "bottcogn_topnon",
                      "bottcogn_bottnon"),
           highisced = c(sum(new_inter[c(6, 1, 2, 5), 2]),
                         sum(new_inter[c(6, 3), 2]),
                         sum(new_inter[c(6, 4), 2]),
                         sum(new_inter[6, 2])),
           lowisced = c(sum(new_inter[c(6, 1, 2, 5), 3]),
                        sum(new_inter[c(6, 3), 3]),
                        sum(new_inter[c(6, 4), 3]),
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

doc <- addTitle(doc, "Table 8 interaction")
doc <- addFlexTable(doc,
                    FlexTable(interaction_table_ready, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

#####



#######################



##### Table 9 ####

interaction_calc <- function(interaction_df, df, dv) {
    
    current_int <- as.character(interaction_df[1, ])
    
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
    type_model <- ifelse(length(na.omit(unique(df[, dv]))) > 2, "lm", "glm")
    
    multi_fun <-
        multi_fun <-
        switch(type_model,
               lm = function(formula, data, subset, ...) {
                   m <- match.call()
                   m[[1]] <- as.name("lm")
                   eval(m)
               },
               glm = function(formula, data, subset, ...) {
                   m <- match.call()
                   m[[1]] <- as.name("glm")
                   eval(m)
               }
        )
    
    # Pass that list to the glmer to run two different models and then show table with stargazer
    models_multilevel1 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = df,
                  subset = gender == 1 & age_categories %in% age & highisced == 1,
                  weight = spfwt0)
    })
    models_multilevel2 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = df,
                  subset = gender == 1 & age_categories %in% age & lowisced == 1,
                  weight = spfwt0)
    })
    
    models_multilevel_upward <- list(models_multilevel1[[1]], models_multilevel2[[1]])
    
    models_multilevel_upward
}

table_eight_calculator <- function(table) {
    
    table_eight <-
        table %>%
        select(1, 2, 4)
    
    tibble("Boosting Upward Moves" = (table_eight[[3, 2]] / table_eight[[4, 2]]) - 1,
           "Averting Downward Moves" = 1 - (table_eight[[3, 3]] / table_eight[[4, 3]])) %>%
        map_df(~ paste0(round(.x, 3) * 100, "%"))
}

all <- cnt_bind$country %>% unique
scandinavia <- c("Denmark", "Sweden", "Norway", "Finland")
czech <- c("Czech Republic", "Italy", "Slovak Republic", "Spain")
aus <- c("Austria", "Belgium", "Estonia", "Korea", "Poland", "United Kingdom")
can <- c("Canada", "France", "Germany", "Japan", "Netherlands", "Sweden", "United States")

all_models <- list(all, scandinavia, czech, aus, can)

table_nine <-
    map(all_models, ~ {
        up <- interaction_calc(x_two, filter(cnt_bind, country %in% .x), "long_dist_upward")[[1]] %>%
            tidy() %>%
            filter(grepl("^noncognitive|Intercept", .$term)) %>%
            select(1, 2) %>%
            spread(term, estimate) %>%
            setNames(c("den", "num")) %>%
            summarize(`Boosting Upward Moves` = round(abs(num) / den, 2))
        
        down <- interaction_calc(x_two, filter(cnt_bind, country %in% .x), "long_dist_downward")[[1]] %>%
            tidy() %>%
            filter(grepl("^noncognitive|Intercept", .$term)) %>%
            select(1, 2) %>%
            spread(term, estimate) %>%
            setNames(c("den", "num")) %>%
            summarize(`Averting Downward Moves` = round(abs(num) / den, 2))
        
        cbind(up, down)
        
}) %>%
    enframe() %>%
    unnest() %>%
    mutate(name = c("All 21 pooled countries",
                    paste(all_models[[2]], collapse = ", "),
                    paste(all_models[[3]], collapse = ", "),
                    paste(all_models[[4]], collapse = ", "),
                    paste(all_models[[5]], collapse = ", ")))

doc <- addTitle(doc, "Table 9")
doc <- addFlexTable(doc, FlexTable(table_nine, header.columns = TRUE))

writeDoc(doc, file = "./Tables/second_batch_tables.docx")

#####