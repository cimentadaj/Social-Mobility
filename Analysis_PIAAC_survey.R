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
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_171/")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(ReporteRs)


##### Reading data #####

# To save tables
directory <- "./Tables"

walk(list.files("./data/", pattern = ".rda", full.names = TRUE), load, .GlobalEnv)

ls2 <- c(ls()[grepl("*.design", ls())] , "ls2", "directory", "old_dir", "multiplot")
# Remove everything that is not in ls2 (so the .design )
rm(list= c(ls()[!ls() %in% ls2]))

star_paster <- function(df, var_one, var_two) {
    df[[var_two]] <- round(df[[var_two]], 2)
    sapply(1:nrow(df), function(index) {
        
        is_na <- is.na(df[[var_one]][index])
        three_stars <- df[[var_one]][index] < 0.001
        two_stars <- df[[var_one]][index] > 0.001 & df[[var_one]][index] <= 0.01

        if (is_na) {
            
            NA
            
        } else if (three_stars) {
            
            paste(df[[var_two]][index], "***")
            
        } else if (two_stars) {
            paste(df[[var_two]][index], "**")
            
        } else {
            
            df[[var_two]][index]
        }
    })
    
    
}


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
                   # Estonia = prgestp1.design,
                   Finland = prgfinp1.design,
                   Japan = prgjpnp1.design,
                   Korea = prgkorp1.design,
                   Norway = prgnorp1.design,
                   Poland = prgpolp1.design,
                   `Russian Federation` = prgrusp1.design,
                   `Slovak Republic` = prgsvkp1.design)

# countries3 <- list(Israel = prgisrp1.design)
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

svy_scale <- function(svy_design, dv) {
    
    svy_design2 <- lapply(svy_design, function(cnt) {
        for (data in seq_along(cnt$design)) {
            cnt$designs[[data]]$variables[, dv] <-
                scale(cnt$designs[[data]]$variables[, dv], center = TRUE,
                      scale = TRUE)[,1]
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

countries3 <- svy_recode(countries3, 'eduattain', "university", "1:11 = 0; c(12, 13, 14, 16) = 1; else = NA")

countries3 <- svy_recode(countries3, "eduattain", "eduattain_reverse",
                         paste0(1:16, " = ", 16:1, collapse = "; "))

#####


##### Table 1 ####
table_one <-
  map(countries3, ~ {
    .x$designs[[1]]$variables %>%
      select(differentideas, bottomthings, additinfo) %>% 
      psy::cronbach() %>% 
      as_tibble() %>% 
      select(alpha)
  }) %>% enframe() %>% unnest()

all_rsq <-
  map(countries3, ~ {
    .x$designs[[1]]$variables %>%
      select(differentideas, bottomthings, additinfo) %>% 
      psych::fa() %>% 
      .[["R2.scores"]] %>% 
      as_tibble() %>% 
      set_names("R-squared")
  }) %>% enframe() %>% unnest()

table_one <- left_join(table_one, all_rsq)

table_one <- mutate_if(table_one, is.numeric, round, 2)

doc <- docx()
doc <- addTitle(doc, "Table 1")
doc <- addFlexTable(doc,
                    FlexTable(table_one, header.columns = FALSE) %>%
                      addHeaderRow(text.properties = textBold(),
                                   value = c("Country", "Internal Consistency", "R-squared of Factor Analysis"),
                                   first = TRUE))

writeDoc(doc, file = "./Tables/tables_one.docx")

#####


##### Model Specification #####

standard_covariates <- c("scale(pvnum)",
                         "non.cognitive",
                         "postwelfare",
                         "dadimmigrant")

# I'm running two models, one with highisced variables and another
# with lowisced variables, that's why I'm creating two separate set of
# independent variables.

# Note that highisced is whther their parents had highsced and same thing
# for low isced.

# Finally, USA has a different lowisced variable, so I create a separate vector
# for US.
all_firstcovariates <- c("highisced", "adv", standard_covariates)
all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])

# I want all ages (categories from 1:10, not ages 1:10)
age <- 1:10
# 6:10 is prewelfare
# 1:5 is postwelfare
#####



##### Modeling for Table 3 and 4 ####

# Change for long_dist_upward/long_dist_downward and the title to produce models for the
# other variable models

dv <- c("long_dist_upward")

# # Standardized DV
# countries3 <- svy_scale(countries3, dv)

depvar_title <- c("Continuous long distance upward")

out_name <- paste0("-PIAAC-sons-", dv, ".html")

covariate_labels <- c("High ISCED",
                      "High ISCED - Low cogn",
                      "Low ISCED",
                      "Low ISCED - High cogn",
                      "Edu attainment (cont)",
                      "Cognitive",
                      "Non-cognitive",
                      "Postwelfare",
                      "Dad immigrant")
digits <- 2

# Function tests the logical statement and if it doesn't equal T, it gives the error_message.
stop_message <- function(logical_statement, error_message) {
    if(logical_statement) stop(error_message, call. = F)
}
warning_message <- function(logical_statement, error_message) {
    if(logical_statement) warning(error_message, call. = F)
}

# Function does all the modeling. It checks the DV is valid,
# whether it's a dummy or not (to produce odd ratios or not)
# and loops through each country and does the modeling.

# df_list = countries3
# dv = "long_dist_downward"
# firstcovariates = all_firstcovariates
# usa_secondcovariates = usa_secondcovariates
# secondcovariates = all_secondcovariates
# age_subset = age
# family_models = family_models
# covariate_labels = covariate_labels
# digits = digits
# out_name = out_name
# dir_tables = directory
# depvar_title = depvar_title

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
    
    # If the number of countries equals 1, bring the only length,
    # if not, sample from all countries
    len <- ifelse(length(dv_length_countries) == 1,
                  dv_length_countries,
                  sample(dv_length_countries, 1))
    
    # stop_message(!all(len == dv_length_countries),
    #              "The length of the dependent variable differs by country")
    # stop_message(!(len >= 2),
    #              "DV has length < 2")
    
    odd.ratio <- ifelse(family_models == "gaussian", F,
                        unname(ifelse(sample(dv_length_countries, 1) == 2, T, F)))
    
    for (i in 1:length(df_list)) {
        print(names(df_list[i]))
        
        # The low isced variable for USA combines both low and mid isced
        # Whenever the country is USA, use a different set of covariates
        # than with all other countries.
        if (names(df_list[i]) == "USA") {
            secondcovariates <- usa_secondcovariates
        }

        mod1 <- models(dv, firstcovariates,
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
                   add.lines = list(c(mod1_r, mod2_r)))
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

dv <- c("long_dist_downward")
depvar_title <- c("Continuous long distance downard")
out_name <- paste0("-PIAAC-sons-", dv, ".html")

model_lists_downward <-
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
#####


##### Table 3 and 4 ####

table_generator <- function(each_model) {
    country_models <-
        lapply(each_model, function(list_data) {
            lapply(list_data, function(each_model) {
                broom::tidy(each_model)[-c(6, 7), c(1, 2, 5)]
            })
        })
    
    country_models <- lapply(country_models, function(x) {
        setNames(x,  c("high", "low")) %>%
            enframe() %>%
            unnest()
    })
    
    all_countries <-
        country_models %>%
        enframe() %>%
        unnest() %>%
        filter(term != "(Intercept)") %>%
        map_if(is_double, round, 2) %>%
        as_tibble() %>%
        mutate(estimate = case_when(p.value < 0.001 ~ paste0(estimate, "***"),
                                    p.value > 0.001 & p.value <= 0.01 ~ paste0(estimate, "**"),
                                    p.value > 0.01 & p.value <= 0.05 ~ paste0(estimate, "*"),
                                    p.value > 0.05 ~ as.character(estimate))) %>%
        select(-p.value)
    
    meritocracy <-
        all_countries %>%
        filter(name1 == "high", term %in% c("highisced", "scale(pvnum)", "non.cognitive")) %>%
        select(-name1) %>%
        mutate(estimate = as.numeric(gsub("\\*", "", estimate))) %>%
        spread(term, estimate) %>%
        transmute(name,
                  cogn_mer = `scale(pvnum)` / highisced,
                  noncogn_mer = non.cognitive / highisced
        ) %>%
        map_if(is_double, round, 1) %>%
        as_tibble()
    
    table_one <-
        all_countries %>%
        filter(term != c("scale(pvnum)", "non.cognitive")) %>%
        select(-name1) %>%
        spread(term, estimate) %>%
        select(name, highisced, adv, lowisced, disadv) %>%
        full_join(meritocracy, key = "name")
    table_one
    
}

table_three <- table_generator(model_lists)
table_four <- table_generator(model_lists_downward)

title_columns <-
    c("","High SES \n origin β \n \n (1)", "High-SES, \n bottom 1/3rd β \n \n (2)",
      "Low SES \n origin β \n \n (3)", "Low-SES \n top 1/3rd β \n \n (4)",
      "Cognitive Meritocracy: \n origin ratio (cogn β:(1))",
      "Non-cognitive meritocracy: \n origin ratio (non-cogn β: (1))")

doc <- docx()
doc <- addTitle(doc, "Table 3")
doc <- addFlexTable(doc,
                    FlexTable(table_three, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

doc <- addTitle(doc, "Table 4")
doc <- addFlexTable(doc,
                    FlexTable(table_four, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

#####


##### Descriptives #####

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
                      `% Dad immigrant` = 100 - (mean(dadimmigrant, na.rm = T) * 100),
                      `Avg numeracy skills` = mean(pvnum, na.rm = T)) %>%
            map_if(is.numeric, round, 0) %>%
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
                     "% High ISCED",
                     "% Low ISCED",
                     "Avg numeracy skills",
                     "Sample size"), 2)))

doc <- addTitle(doc, "Descriptives")
doc <- addFlexTable(doc,
                    FlexTable(descriptive_table) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = c("", "Pre-welfare cohort", "Post-welfare cohort"),
                                     colspan = c(1, 5, 5),
                                     first = TRUE)
)

writeDoc(doc, file = "./Tables/tables.docx")

#####


##### Second descriptives #####

# Same function as before, but commenting out
# the stargazer part in order not to save tables!
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
  
  # If the number of countries equals 1, bring the only length,
  # if not, sample from all countries
  len <- ifelse(length(dv_length_countries) == 1,
                dv_length_countries,
                sample(dv_length_countries, 1))
  
  # stop_message(!all(len == dv_length_countries),
  #              "The length of the dependent variable differs by country")
  # stop_message(!(len >= 2),
  #              "DV has length < 2")
  
  odd.ratio <- ifelse(family_models == "gaussian", F,
                      unname(ifelse(sample(dv_length_countries, 1) == 2, T, F)))
  
  for (i in 1:length(df_list)) {
    print(names(df_list[i]))
    
    # The low isced variable for USA combines both low and mid isced
    # Whenever the country is USA, use a different set of covariates
    # than with all other countries.
    if (names(df_list[i]) == "USA") {
      secondcovariates <- usa_secondcovariates
    }
    
    mod1 <- models(dv, firstcovariates,
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
    
    # ## Tables
    # stargazer2(all.models, odd.ratio, type = "html",
    #            title = paste0(names(df_list[i])),
    #            column.labels = rep(depvar_title, 2),
    #            column.separate = rep(length(all_firstcovariates), 2),
    #            dep.var.labels.include = FALSE,
    #            order = c(1, 2, 7, 8),
    #            covariate.labels = covariate_labels,
    #            digits = digits,
    #            out = file.path(dir_tables, paste0(names(df_list[i]), out_name)),
    #            add.lines = list(c(mod1_r, mod2_r)))
  }
  last_models
}


# I rerun this model only to have pvnum without scaled
# to be able to get descriptives of pvnum unscaled
# as it is in the original descriptives table


models_unscaled <-
  modeling_function(
    df_list = countries3,
    dv = dv,
    firstcovariates = gsub("scale\\(|\\)", "", all_firstcovariates),
    usa_secondcovariates = gsub("scale\\(|\\)", "", usa_secondcovariates),
    secondcovariates = gsub("scale\\(|\\)", "", all_secondcovariates),
    age_subset = age,
    family_models = family_models,
    covariate_labels = covariate_labels,
    digits = digits,
    out_name = out_name,
    dir_tables = directory,
    depvar_title = depvar_title)

missing_descriptives <-
  map(countries3, ~ {
    .x$designs[[1]]$variables %>% select(isco,
                                         lowisced,
                                         pvnum,
                                         non.cognitive,
                                         postwelfare,
                                         dadimmigrant) %>% 
      summarize_all(function(x) mean(is.na(x)))
  }) %>% 
  enframe() %>% 
  unnest() %>% 
  mutate(total_miss = rowSums(select(., -name))) %>% 
  mutate_if(is.numeric, round, 2)


doc <- docx()
doc <- addTitle(doc, "Descriptives of missing values")
doc <- addFlexTable(doc,
                    FlexTable(missing_descriptives,
                              header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = c("Country",
                                               "Parental occupation",
                                               "Low ISCED",
                                               "Numeracy Skill",
                                               "Non cognitive",
                                               "Pre/Post welfare",
                                               "Dad immigrant",
                                               "Total missing value"))
                    )

writeDoc(doc, file = "./Tables/descriptves_missing.docx")


second_descriptives <-
  map(models_unscaled, function(.x) {
    lowisced_table <-
      .x[[2]]$model %>%
      group_by(postwelfare) %>%
      summarize(`% Low ISCED` = mean(lowisced, na.rm = T) * 100) %>%
      mutate_if(is.numeric, round, 0) %>%
      gather(terms, vals, -postwelfare) %>%
      unite(postwelfare, postwelfare, terms , sep = "_") %>%
      spread(postwelfare, vals)
    
    almost_table <-
      .x[[1]]$model %>%
      group_by(postwelfare) %>%
      summarize(`Sample size` = n(),
                `% High ISCED` = mean(highisced, na.rm = T) * 100,
                `% Dad immigrant` = 100 - (mean(dadimmigrant, na.rm = T) * 100),
                `Avg numeracy skills` = mean(pvnum, na.rm = T)) %>%
      map_if(is.numeric, round, 0) %>%
      as_tibble() %>% 
      gather(terms, vals, -postwelfare) %>%
      unite(postwelfare, postwelfare, terms , sep = "_") %>%
      spread(postwelfare, vals) 
    
    final_table <-
      almost_table %>% 
      add_column(`0_% Low ISCED` = lowisced_table[[1]], .after = 2) %>% 
      add_column(`1_% Low ISCED` = lowisced_table[[2]], .after = 7)
    
    final_table
  }) %>% 
  enframe() %>% 
  unnest()

descriptive_table <-
  second_descriptives %>%
  setNames(c("Country",
             rep(c("% Dad immigrant",
                   "% High ISCED",
                   "% Low ISCED",
                   "Avg numeracy skills",
                   "Sample size"), 2)))

doc <- docx()
doc <- addTitle(doc, "Descriptives after sample selection")
doc <- addFlexTable(doc,
                    FlexTable(descriptive_table) %>%
                      addHeaderRow(text.properties = textBold(),
                                   value = c("", "Pre-welfare cohort", "Post-welfare cohort"),
                                   colspan = c(1, 5, 5),
                                   first = TRUE)
)

writeDoc(doc, file = "./Tables/descriptves_finalsample.docx")

#####

##### Preparing second modeling #####
# I will use all of the variables from the first models from above.
dv <- "long_dist_upward"
standard_covariates <- c("scale(pvnum)",
                         "non.cognitive",
                         "postwelfare",
                         "dadimmigrant")

all_firstcovariates <- c("highisced", "adv", standard_covariates)
all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])

cohort <- "young_cohort"

random <- "(1 | country)"
random_two <- "(1 + highisced | country)"
random_three <- "(1 + lowisced | country)"

unique_second <- setdiff(all_secondcovariates, all_firstcovariates)

vars_subset <-
    gsub("scale|\\(|\\)", "", c(unique_second, all_firstcovariates)) %>%
    `c`("highedu", "country", "cohort", "gender", "age_categories",
        "long_dist_upward", "long_dist_downward", "serviceclass", "lowerclass")

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

#####


##### Cognitive distribution graphs #####
cnt_bind %>%
    filter(highedu %in% c(1, 3)) %>%
    ggplot(aes(pvnum, fill = as.character(highedu))) +
    geom_density(alpha = 0.3) +
    labs(x = "Cognitive distribution", y = "Density") +
    scale_fill_colorblind(name = "Social class",
                          labels = c("Low ISCED", "Higher ISCED")) +
    scale_y_continuous(labels = round(seq(0, 0.0075, 0.0025), 3),
                       breaks = round(seq(0, 0.0075, 0.0025), 3)) +
    theme_minimal()

ggsave("cognitive_distribution.png", path = directory)

cnt_bind %>%
    filter(highedu %in% c(1, 3)) %>%
    ggplot(aes(non.cognitive, fill = as.character(highedu))) +
    geom_density(alpha = 0.3) +
    labs(x = "Non-cognitive distribution", y = "Density") +
    scale_fill_colorblind(name = "Social class",
                          labels = c("Low ISCED", "Higher ISCED")) +
    theme_minimal()

ggsave("noncognitive_distribution.png", path = directory)
#####


# ##### Extra AGE analysis ####
# 
# # To produce regressions for cognitive and non cognitive against
# # age variable for different categories.
# # For cognitive I ran it for Korea, Italy and Denmark.
# # For non.cognitive I ran it for Estonia, Italy and US
# # Change the countries inside the loop in the modeling_function call.
# 
# ability_vars <- c("non.cognitive")
# ability_title <- c("Non cognitive")
# 
# ability_list <-
#     map(seq_along(ability_vars), function(index) {
#         
#         dv <- ability_vars[index]
#         depvar_title <- ability_title[index]
#         out_name <- paste0("ability-PIAAC-sons-", ability_vars[index], ".html")
#         age <- 1:10
#         # 6:10 is prewelfare
#         # 1:5 is postwelfare
#         
#         standard_covariates <- c("age_categories")
#         digits <- 2
#         
#         # Function tests the logical statement and if it doesn't equal T, it gives the error_message.
#         stop_message <- function(logical_statement, error_message) {
#             if(logical_statement) stop(error_message, call. = F)
#         }
#         warning_message <- function(logical_statement, error_message) {
#             if(logical_statement) warning(error_message, call. = F)
#         }
#         
#         modeling_function <- function(df_list,
#                                       dv,
#                                       standard_covariates,
#                                       age_subset,
#                                       family_models = "gaussian",
#                                       digits,
#                                       out_name,
#                                       dir_tables,
#                                       depvar_title) {
#             
#             stop_message(length(df_list) < 1, "df_list is empty")
#             last_models <- rep(list(vector("list", 2)), length(df_list))
#             names(last_models) <- names(df_list)
#             
#             for (i in 1:length(df_list)) {
#                 
#                 
#                 mod1 <- models(dv, standard_covariates,
#                                subset(df_list[[i]],
#                                       gender == 1 &
#                                       age_categories %in% age_subset &
#                                       postwelfare == 0),
#                                family_models = family_models)
#                 
#                 last_models[[i]][[1]] <- mod1[[length(mod1)]] # length(mod1) to only get the last (complete model)
#                 
#                 # Calculate R squared for each model
#                 mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
#                 
#                 ## Tables
#                 stargazer2(mod1, odd.ratio = F, type = "html",
#                            title = paste0(names(df_list[i])),
#                            column.labels = depvar_title,
#                            column.separate = length(standard_covariates),
#                            dep.var.labels.include = FALSE,
#                            digits = digits,
#                            out = file.path(dir_tables, paste0(names(df_list[i]), out_name)))
#                 # add.lines = list(c(mod1_r, mod2_r))
#             }
#             last_models
#         }
#         
#         family_models <- "gaussian"
#         model_lists <-
#             modeling_function(
#                 df_list = countries3[c("Italy", "United States")],
#                 dv = dv,
#                 standard_covariates = standard_covariates,
#                 age_subset = age,
#                 family_models = family_models,
#                 digits = digits,
#                 out_name = out_name,
#                 dir_tables = directory,
#                 depvar_title = depvar_title)
#     })
# 
# #####


##### Table 2 ####
# change to lowerclass to get the other table
dv <- "long_dist_upward"
depvar_title <- "Continuous upward mobility"

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

formula_maker <- function(lhs, rhs) {
    as.formula(paste(lhs, " ~ ", paste0(rhs, collapse = " + ")))
}

covariate_list <- list(formula_maker(dv, all_firstcovariates),
                       formula_maker(dv, all_secondcovariates))

# If the DV is not binary, run lmer, if it is, then use glmer
type_model <- ifelse(length(na.omit(unique(cnt_bind[[dv]]))) > 2, "lm", "glm")

multi_fun <-
    switch(type_model,
           lm = function(formula, data, subset, ...) {
               lm(formula = formula,
                  data = subset(data, eval(parse(text = subset))),
                  ...)
           },
           glm = function(formula, data, subset, ...) {
               glm(formula = formula,
                   data = subset(data, eval(parse(text = subset))),
                   family = "gaussian", ...)
           })

# Pass that list to the glmer to run two different models and then show table with stargazer
models_upward <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age")
})

dv <- "long_dist_downward"
depvar_title <- "Continuous downward mobility"

covariate_list <- list(formula_maker(dv, all_firstcovariates),
                       formula_maker(dv, all_secondcovariates))

# Pass that list to the glmer to run two different models and then show table with stargazer
models_lower <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age")
})

table_converter <- function(models) {
    coef_table <- map(models, tidy)
    
    table_one <-
        map(coef_table, ~ select(.x, -std.error, -statistic) %>% filter(!grepl("isced|adv", .x$term))) %>%
        reduce(cbind)
    
    table_one_separate <- map(coef_table, ~ select(.x, -std.error, -statistic) %>% filter(grepl("isced|adv", .x$term)))
    
    names(table_one_separate[[1]])[2:3] <- c("estimate_high", "p.val_high")
    table_one_separate[[1]]$estimate_low <- NA
    table_one_separate[[1]]$p.val_low <- NA
    
    names(table_one_separate[[2]])[2:3] <- c("estimate_low", "p.val_low")
    table_one_separate[[2]]$estimate_high <- NA
    table_one_separate[[2]]$p.val_high <- NA
    
    table_one_separate[[2]] <- table_one_separate[[2]][c(1, 4, 5, 2, 3)]
    
    sample_size <- map_dbl(models, nobs)
    r_sq <- map_dbl(models, ~ summary(.x)$adj.r.squared %>% round(2))
    
    tibble_to_bind <-
        tibble(term = c("R-squared", "Sample size: "),
               estimate_high = as.character(c(r_sq[1], sample_size[1])),
               estimate_low = as.character(c(r_sq[2], sample_size[2])))
    
    table_almost <-
        table_one[-4] %>%
        setNames(c("term", "estimate_high", "p.val_high", "estimate_low", "p.val_low")) %>%
        bind_rows(table_one_separate %>% reduce(bind_rows)) %>%
        as_tibble() %>%
        transmute(term,
                  estimate_high = star_paster(., "p.val_high", "estimate_high"),
                  estimate_low = star_paster(., "p.val_low", "estimate_low")) %>%
        bind_rows(tibble_to_bind)
    
    
    table_ready <-
        table_almost[c(7:10, 2:3, 4:6, 1, 11:12), ] %>%
        mutate(term = recode(term,
                             "highisced" = "High-ISCED origin",
                             "adv" = "High-ISCED origin, \n Bottom 1/3rd cognitive skills",
                             "lowisced" = "Low-ISCED origin",
                             "disadv" = "Low-ISCED origin, \n Top 1/3rd cognitive skills",
                             "scale(pvnum)" = "Cognitive skills",
                             "non.cognitive" = "Social skills",
                             "age_categories" = "Age",
                             "postwelfare" = "Postwelfare (dummy)",
                             "dadimmigrant" = "Dad immigrant",
                             "(Intercept)" = "Constant"))
    table_ready
}

table_two <-
    table_converter(models_upward) %>%
    bind_cols(table_converter(models_lower) %>% select(-term)) %>%
    setNames(c("", rep(c("Service class", "Working class"), each = 2)))

doc <- addTitle(doc, "Table 2")
doc <- addFlexTable(doc, FlexTable(table_two, header.columns = TRUE))

writeDoc(doc, file = "./Tables/tables.docx")
#####

##### Predicted probabilities using the final two models from above #####

subset_df <- cnt_bind %>% filter(gender == 1, age_categories %in% age)

# For predicted values in upward mobility for High and Low SES respectively
# 1)
df_predict <- modelr::data_grid(subset_df, highisced, adv, .model = models_upward[[1]]) %>% drop_na()
broom::augment(models_upward[[1]], newdata = df_predict)

# 2)
df_predict <- modelr::data_grid(subset_df, lowisced, disadv, .model = models_upward[[2]]) %>% drop_na()
broom::augment(models_upward[[2]], newdata = df_predict)

# For predicted values in downward mobility for High and Low SES respectively
# 1)
df_predict <- modelr::data_grid(subset_df, highisced, adv, .model = models_lower[[1]]) %>% drop_na()
broom::augment(models_lower[[1]], newdata = df_predict)

# 2)
df_predict <- modelr::data_grid(subset_df, lowisced, disadv, .model = models_lower[[2]]) %>% drop_na()
broom::augment(models_lower[[2]], newdata = df_predict)


####


##### Modeling figure 1 and 2 ####
# change to lowerclass to get the other table
dv <- "long_dist_upward"
depvar_title <- "Continuous upward mobility"

standard_covariates <- c("scale(pvnum)",
                         "non.cognitive",
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

covariate_list <- list(paste0(dv, " ~ ", paste0(all_firstcovariates, collapse = " + "),
                              " + (highisced | country)"),
                       paste0(dv, " ~ ", paste0(all_secondcovariates, collapse = " + "),
                              " + (lowisced | country)"))

covariate_list <- map(covariate_list, as.formula)

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
                     family = "gaussian", ...)
           })

# Pass that list to the glmer to run two different models and then show table with stargazer
models_multilevel_service <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age")
})

dv <- "long_dist_downward"
depvar_title <- "Continuous downward mobility"

covariate_list <- list(paste0(dv, " ~ ", paste0(all_firstcovariates, collapse = " + "),
                              " + (highisced | country)"),
                       paste0(dv, " ~ ", paste0(all_secondcovariates, collapse = " + "),
                              " + (lowisced | country)"))

covariate_list <- map(covariate_list, as.formula)

# Pass that list to the glmer to run two different models and then show table with stargazer
models_multilevel_lower <- map(covariate_list, function(formula) {
    multi_fun(formula = formula,
              data = cnt_bind,
              subset = "gender == 1 & age_categories %in% age")
})

#####


##### Figure 1 and 2 ####
# Repeat for both dependent variables

term_grabber <- function(model, term) {
    df <- tidy(model) 
    df[grep(term, df$term), 1:3]
}

list_model_todf <- function(list, term) {
    map(list, term_grabber, term) %>%
        enframe() %>%
        unnest(value)
}

plot_generator <- function(multilevel, linear, title_graph) {
    
    term <- c("highisced", "lowisced")
    breaks_term <- term
    term_regex <- paste0("^", term, collapse = "|")
    labels_term <- c("High ISCED", "Low ISCED")
    order_term <- "lowisced"
    term_colour <- c("brown", "black", "brown", "black")
    
    avg_slopes <-
        list_model_todf(multilevel, term_regex)[1:2, ] %>%
        transmute(country = "Average slope",
                  term, estimate, std.error) %>%
        mutate(term = c("avg_slope_high", "avg_slope_low"))
    
    cnt_slopes <-
        map(linear, ~ list_model_todf(.x, term_regex)[1:2, ]) %>%
        enframe(name = 'country') %>%
        unnest(value)
    
    df_countries <-
        cnt_slopes %>%
        dplyr::select(-name) %>%
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
        ggplot(aes(country, estimate, fill = term)) +
        geom_col(alpha = trans) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, alpha = trans - 0.2) +
        scale_fill_manual(name = "Slope",
                          breaks = breaks_term,
                          labels = labels_term,
                          values = term_colour) +
        xlab(NULL) +
        ylab("Estimated slope with 95% uncertainty intervals") +
        ggtitle(title_graph) +
        coord_flip() +
        theme_minimal() +
        theme(axis.text.y=element_text(colour = country_color))
}

plot_generator(models_multilevel_service, model_lists, "Chances of upward mobility")
ggsave("high_low_isced_upward.png", path = directory)

plot_generator(models_multilevel_lower, model_lists_downward, "Chances of downward mobility")
ggsave("high_low_isced_downward.png", path = directory)

#####



##### Table 5 ####

dv <- "serviceclass"
depvar_title <- "Continuous upward class"

standard_covariates <- c("scale(pvnum)",
                         "non.cognitive",
                         "postwelfare",
                         "dadimmigrant")

all_firstcovariates <- c("highisced", "adv", standard_covariates)
all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])

# I want all ages (categories from 1:10, not ages 1:10)
age <- 1:10
# 6:10 is prewelfare
# 1:5 is postwelfare


modeling_function_two <- function(df_list,
                                  dv,
                                  firstcovariates,
                                  usa_secondcovariates,
                                  secondcovariates,
                                  age_subset,
                                  family_models = "gaussian") {
    
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
    
    # If the number of countries equals 1, bring the only length,
    # if not, sample from all countries
    len <- ifelse(length(dv_length_countries) == 1,
                  dv_length_countries,
                  sample(dv_length_countries, 1))
    
    stop_message(!all(len == dv_length_countries),
                 "The length of the dependent variable differs by country")
    stop_message(!(len >= 2),
                 "DV has length < 2")
    
    odd.ratio <- ifelse(family_models == "gaussian", F,
                        unname(ifelse(sample(dv_length_countries, 1) == 2, T, F)))
    
    all.models <- rep(list(list()), length(df_list))
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
        last_models[[i]][[2]] <- mod2[[length(mod2)]]
        
        # Calculate R squared for each model
        # mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
        # mod2_r <- paste0(sapply(mod2, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%")
        
    }
    last_models
}

family_models <- "gaussian"

models_table_service_four <-
    modeling_function_two(
        df_list = countries3,
        dv = dv,
        firstcovariates = all_firstcovariates,
        usa_secondcovariates = usa_secondcovariates,
        secondcovariates = all_secondcovariates,
        age_subset = age,
        family_models = family_models)

dv <- "lowerclass"
depvar_title <- "Continuous downward class"

models_table_downward_four <-
    modeling_function_two(
        df_list = countries3,
        dv = dv,
        firstcovariates = all_firstcovariates,
        usa_secondcovariates = usa_secondcovariates,
        secondcovariates = all_secondcovariates,
        age_subset = age,
        family_models = family_models)

tabler <- function(model, high = "TRUE") {
    
    metadata <- switch(high,
                       "TRUE" = list(-1, c("lowisced", "disadv")),
                       "FALSE" = list(-2, c("highisced", "adv")))
    
    table_ready <-
        map(model, function(country) {
            df_model <- tidy(country[[metadata[[1]]]])
            
            df_model[grepl(paste(metadata[[2]], collapse = "|"), df_model$term), ] %>%
                select(term, estimate, p.value) %>%
                transmute(term,
                          estimate = star_paster(., "p.value", "estimate")) %>%
                spread(term, estimate) %>%
                select(one_of(metadata[[2]]))
        }) %>%
        reduce(rbind) %>%
        mutate(country = names(model)) %>%
        select(one_of(c("country", metadata[[2]])))
    table_ready
}


table_five <-
    tabler(models_table_service_four, "TRUE") %>%
    arrange(country) %>%
    bind_cols(tabler(models_table_downward_four, "FALSE") %>%
                  arrange(country) %>%
                  select(-country))

title_columns <-
    c("Destiny:",
      "Service class \n All low-SES sons",
      "Service class \n Low-SES sons* \n high cognitive \n score",
      "Working class \n All high-SES sons",
      "Working class \n High-SES sons* \n Low cognitive \n score"
    )

doc <- addTitle(doc, "Table 5")
doc <- addFlexTable(doc,
                    FlexTable(table_five, header.columns = FALSE) %>%
                        addHeaderRow(text.properties = textBold(),
                                     value = title_columns,
                                     first = TRUE))

writeDoc(doc, file = "./Tables/tables.docx")


#####



##### Table 7 ####
# 
# dv <- "long_dist_upward"
# standard_covariates <- c("scale(pvnum)",
#                          "non.cognitive",
#                          "dadimmigrant",
#                          "age_categories")
# 
# all_firstcovariates <- c("highisced", "adv", standard_covariates)
# all_secondcovariates <- c("lowisced", "disadv", standard_covariates)
# usa_secondcovariates <- c("lowmidisced2", all_secondcovariates[-1])
# 
# modeling_function_f <- function(df_list,
#                               dv,
#                               firstcovariates,
#                               usa_secondcovariates,
#                               secondcovariates,
#                               age_subset,
#                               family_models = "gaussian") {
#     
#     stop_message(length(df_list) < 1, "df_list is empty")
#     last_models <- rep(list(vector("list", 2)), length(df_list))
#     names(last_models) <- names(df_list)
#     
#     # Odd ratios or not?
#     # This should be done to identify whether DV is a dummy or not
#     dv_length_countries <-
#         map_dbl(df_list, function(.x)
#             unique(.x$designs[[1]]$variables[, dv]) %>%
#                 na.omit() %>%
#                 length())
#     
#     # If the number of countries equals 1, bring the only length,
#     # if not, sample from all countries
#     len <- ifelse(length(dv_length_countries) == 1,
#                   dv_length_countries,
#                   sample(dv_length_countries, 1))
#     
#     stop_message(!all(len == dv_length_countries),
#                  "The length of the dependent variable differs by country")
#     stop_message(!(len >= 2),
#                  "DV has length < 2")
#     
#     odd.ratio <- ifelse(family_models == "gaussian", F,
#                         unname(ifelse(sample(dv_length_countries, 1) == 2, T, F)))
#     
#     for (i in 1:length(df_list)) {
#         
#         # The low isced variable for USA combines both low and mid isced
#         # Whenever the country is USA, use a different set of covariates
#         # than with all other countries.
#         if (names(df_list[i]) == "USA") {
#             secondcovariates <- usa_secondcovariates
#         } else {
#             secondcovariates <- all_secondcovariates }
#         
#         mod1 <- models(dv, all_firstcovariates,
#                        subset(df_list[[i]], gender == 1 &
#                                   postwelfare == 0),
#                        family_models = family_models)
#         
#         mod2 <- models(dv, secondcovariates,
#                        subset(df_list[[i]], gender == 1 &
#                                   postwelfare == 0),
#                        family_models = family_models)
#         
#         last_models[[i]][[1]] <- mod1[[length(mod1)]] # length(mod1) to only get the last (complete model)
#         last_models[[i]][[2]] <- mod2[[length(mod1)]]
#         
#         # Calculate R squared for each model
#         # mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
#         # mod2_r <- paste0(sapply(mod2, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%")
#         
#     }
#     last_models
# }
# 
# modeling_function_t <- function(df_list,
#                               dv,
#                               firstcovariates,
#                               usa_secondcovariates,
#                               secondcovariates,
#                               age_subset,
#                               family_models = "gaussian") {
#     
#     stop_message(length(df_list) < 1, "df_list is empty")
#     last_models <- rep(list(vector("list", 2)), length(df_list))
#     names(last_models) <- names(df_list)
#     
#     # Odd ratios or not?
#     # This should be done to identify whether DV is a dummy or not
#     dv_length_countries <-
#         map_dbl(df_list, function(.x)
#             unique(.x$designs[[1]]$variables[, dv]) %>%
#                 na.omit() %>%
#                 length())
#     
#     # If the number of countries equals 1, bring the only length,
#     # if not, sample from all countries
#     len <- ifelse(length(dv_length_countries) == 1,
#                   dv_length_countries,
#                   sample(dv_length_countries, 1))
#     
#     stop_message(!all(len == dv_length_countries),
#                  "The length of the dependent variable differs by country")
#     stop_message(!(len >= 2),
#                  "DV has length < 2")
#     
#     odd.ratio <- ifelse(family_models == "gaussian", F,
#                         unname(ifelse(sample(dv_length_countries, 1) == 2, T, F)))
#     
#     for (i in 1:length(df_list)) {
#         
#         # The low isced variable for USA combines both low and mid isced
#         # Whenever the country is USA, use a different set of covariates
#         # than with all other countries.
#         if (names(df_list[i]) == "USA") {
#             secondcovariates <- usa_secondcovariates
#         } else {
#             secondcovariates <- all_secondcovariates }
#         
#         mod1 <- models(dv, all_firstcovariates,
#                        subset(df_list[[i]], gender == 1 &
#                                   postwelfare == 1),
#                        family_models = family_models)
#         
#         mod2 <- models(dv, secondcovariates,
#                        subset(df_list[[i]], gender == 1 &
#                                   postwelfare == 1),
#                        family_models = family_models)
#         
#         last_models[[i]][[1]] <- mod1[[length(mod1)]] # length(mod1) to only get the last (complete model)
#         last_models[[i]][[2]] <- mod2[[length(mod1)]]
#         
#         # Calculate R squared for each model
#         # mod1_r <- c("R squared:", paste0(sapply(mod1, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%"))
#         # mod2_r <- paste0(sapply(mod2, function(x) floor((1-x$deviance/x$null.deviance) * 100)), "%")
#         
#     }
#     last_models
# }
# 
# family_models <- "gaussian"
# 
# selected_countries <- c(
#     "Austria",
#     "Belgium",
#     "Denmark",
#     "Finland",
#     "Germany"
# )
# 
# countries_prewelfare <-
#     modeling_function_f(
#         df_list = countries3[selected_countries],
#         dv = dv,
#         firstcovariates = all_firstcovariates,
#         usa_secondcovariates = usa_secondcovariates,
#         secondcovariates = all_secondcovariates,
#         age_subset = age,
#         family_models = family_models)
# 
# countries_postwelfare <-
#     modeling_function_t(
#         df_list = countries3[selected_countries],
#         dv = dv,
#         firstcovariates = all_firstcovariates,
#         usa_secondcovariates = usa_secondcovariates,
#         secondcovariates = all_secondcovariates,
#         age_subset = age,
#         family_models = family_models)
# 
# pre_post_table <- function(pre_mod, post_mod, country, num, den) {
#     pre <- tidy(pre_mod[[country]][[2]]) %>%
#         filter(term %in% c(num, den)) %>%
#         select(term, estimate) %>%
#         spread(term, estimate) %>%
#         setNames(c("num", "den")) %>%
#         summarize(pre = (abs(num) / den))
#     
#     post <- tidy(post_mod[[country]][[2]]) %>%
#         filter(term %in% c(num, den)) %>%
#         select(term, estimate) %>%
#         spread(term, estimate) %>%
#         setNames(c("num", "den")) %>%
#         summarize(post = (abs(num) / den))
# 
#     cbind(post, pre)
# }
# 
# pre_post_table(countries_prewelfare,
#                countries_postwelfare,
#                "Denmark", "scale(pvnum)", "lowisced")
# 
# 
#####