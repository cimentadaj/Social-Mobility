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

variables <- c("long_dist_upward", "long_dist_downward")
titles <- c("Continuous upward", "Continuous downward")

interaction_vars <- c(
    "cognitive_top30_bottom30", "noncognitive_top30_bottom30",
    "cognitive_top30_bottom50", "noncognitive_top30_bottom50",
    "cognitive_top20_bottom50", "noncognitive_top20_bottom50",
    "cognitive_top20_bottom40", "noncognitive_top20_bottom40"
)

dv <- variables[1]
depvar_title <- titles[1]

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

# country_subset <- c("United Kingdom", "Korea", "Estonia", "Austria", "Belgium")
# country_subset <- c("Italy", "Spain")
# country_subset <- c("Slovak Republic", "Czech Republic", "Poland")
# country_subset <- c("United States", "Sweden", "Norway", "Netherlands", "Japan", "Germany", "France",
#                     "Finland", "Canada", "Denmark")

country_subset <- c("United States", "Netherlands", "Japan", "Germany", "France", "Canada")

cnt_bind <- Reduce(rbind, cnts)
cnt_bind$pvnum <- scale(cnt_bind$pvnum)[, 1] # because it returns 1 col matrix
cnt_bind[interaction_vars] <- map_df(cnt_bind[interaction_vars], as.factor)
cnt_bind <- filter(cnt_bind, country %in% country_subset)
    
x_two <-
        tibble(cogn = interaction_vars[seq(1, length(interaction_vars), 2)],
               noncogn = interaction_vars[seq(2, length(interaction_vars), 2)]) %>%
        unite(interaction, cogn, noncogn, sep = "*")

.x <- 1    
    # multilevel models
        current_int <- as.character(x_two[.x, ])
        
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
        
        # stargazer3 <- function(model, odd.ratio = FALSE, ...) {
        #     
        #     if (!("list" %in% class(model))) model <- list(model)
        #     
        #     if (odd.ratio) {
        #         # Get coefficients
        #         coef_table <- purrr::map(model, ~ as.data.frame(summary(.x)$coefficients))
        #         
        #         # Estimate odds for all models
        #         odds <- purrr::map(coef_table, ~ exp(.x[, 1]))
        #         
        #         # Loop through odds and SE and multiply them
        #         oddsSE <- purrr::map2(odds, coef_table, ~ .x * .y[, 2])
        #         
        #         # Get p vals from models
        #         p_vals <- purrr::map(coef_table, ~ .x[, 4])
        #         
        #         stargazer::stargazer(model,
        #                              coef = odds,
        #                              se = oddsSE,
        #                              p = p_vals, ...)
        #         
        #     } else {
        #         stargazer::stargazer(model, ...)
        #     }
        #     
        # }
        # 
        # # Remember to finish the stargazer3 in your package (and do it as object-oriented programming).
        # stargazer_sequence <- function(model, covariate_labels, depvar_title, directory, cohort, ...) {
        #     stargazer(model,
        #               type = "html",
        #               digits = 2,
        #               add.lines = list(c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2)))),
        #               # c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
        #               # c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
        #               # c("Varying", rep("Intercept", length(model)))),
        #               # covariate.labels = covariate_labels,
        #               dep.var.labels = depvar_title,
        #               out = file.path(directory,
        #                               paste0(dv, "_", cohort, .x, "_interaction_multilevel_tables.html")),
        #               ...)
        # }
        # 
        # columns <- c("High isced = 1", "Low isced = 1")
        # stargazer_sequence(models_multilevel,
        #                    covariate_labels, depvar_title, directory,
        #                    cohort, column.labels = columns)
    })
    
# # lm models
# walk(1:nrow(x_two), ~ {
#         current_int <- as.character(x_two[.x, ])
#         
#         standard_covariates <- c("pvnum",
#                                  "non.cognitive",
#                                  "age_categories", "postwelfare")
#         
#         all_firstcovariates <- standard_covariates
#         
#         iv <- list(all_firstcovariates)[[1]]
#         
#         covariate_list <-
#             map(list(all_firstcovariates), function(iv) {
#                 map(current_int, ~ as.formula(paste(dv, paste(c(.x, iv), collapse = " + "), sep = " ~ ")))
#             }) %>%
#             `c`(recursive = T)
#         
#         all_firstcovariates <- standard_covariates
#         
#         # If the DV is not binary, run lm, if it is, then use glm
#         type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lm", "glm")
#         
#         multi_fun <-
#             multi_fun <-
#             switch(type_model,
#                    lm = function(formula, data, subset, ...) {
#                        m <- match.call()
#                        m[[1]] <- as.name("lm")
#                        eval(m)
#                    },
#                    glmer = function(formula, data, subset, ...) {
#                        m <- match.call()
#                        m[[1]] <- as.name("glm")
#                        eval(m)
#                    }
#             )
#         
#         # Pass that list to the glmer to run two different models and then show table with stargazer
#         models_multilevel1 <- map(covariate_list, function(formula) {
#             multi_fun(formula = formula,
#                       data = cnt_bind,
#                       subset = gender == 1 & age_categories %in% age & highisced == 1,
#                       weight = spfwt0)
#         })
#         models_multilevel2 <- map(covariate_list, function(formula) {
#             multi_fun(formula = formula,
#                       data = cnt_bind,
#                       subset = gender == 1 & age_categories %in% age & lowisced == 1,
#                       weight = spfwt0)
#         })
#         models_multilevel <- list(models_multilevel1[[1]], models_multilevel2[[1]])
#         
#         # Remember to finish the stargazer3 in your package (and do it as object-oriented programming).
#         
#         stargazer_sequence <- function(model, covariate_labels, depvar_title, directory, cohort, ...) {
#             stargazer(model,
#                       type = "html",
#                       digits = 2,
#                       # add.lines = list(c("R-sq", map_dbl(model, ~ round(r.squaredGLMM(.x)[1], 2)))),
#                       # c("Between group SD", map_dbl(model, ~ round(.x@theta, 2))),
#                       # c("Number of groups", map_dbl(model, ~ .x@pp$Zt@Dim[1])),
#                       # c("Varying", rep("Intercept", length(model)))),
#                       # covariate.labels = covariate_labels,
#                       dep.var.labels = depvar_title,
#                       out = file.path(directory,
#                                       paste0(dv, "_", cohort, .x, "_interaction_pooled_linear.html")),
#                       ...)
#         }
#         
#         columns <- c("High isced = 1", "Low isced = 1")
#         stargazer_sequence(models_multilevel,
#                            covariate_labels, depvar_title, directory,
#                            cohort, column.labels = columns)
#     })
    

model <- models_multilevel[[1]]

add_predictions_se <- function(data, model) {
    se <- predict(model, newdata = data, se.fit = T, re.form = NA)$se.fit
    data[["se"]] <- se
    data
}


interaction_visual <- function(model) {
    
    title <- ifelse(grepl("upward", as.character(model@call[[2]][[2]])), "upward", "downward")
    
    modelr::data_grid(cnt_bind, cognitive_top30_bottom30, noncognitive_top30_bottom30, .model = model) %>%
    filter(complete.cases(.)) %>%
    modelr::add_predictions(model) %>%
    add_predictions_se(model) %>%
    select(1:2, pred, se) %>%
    mutate(lower = pred - 2 * se,
           upper = pred + 2 * se) %>%
    ggplot(aes(cognitive_top30_bottom30, pred,
               shape = noncognitive_top30_bottom30,
               colour = noncognitive_top30_bottom30)) +
    geom_line(aes(group = noncognitive_top30_bottom30)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05) +
    scale_y_continuous(paste("Predicted", title, "mobility")) +
    scale_x_discrete(name = "Cognitive", labels = c("Bottom 30%", "Top 30%")) +
    scale_color_manual(name = "Non cognitive",
                       labels = c("Bottom 30%", "Top 30%"),
                       values = c("red", "blue")) +
    scale_shape_manual(name = "Non cognitive",
                       labels = c("Bottom 30%", "Top 30%"),
                       values = c(16, 17)) +
    ggtitle(paste("Predicted", title, "mobility for combination of cognitive and non cognitive values")) +
    theme_minimal(base_size = 13)
}

interaction_visual(model)

ggsave(filename = "multilevel_interaction_downward_lowisced.png", path = "./Tables/")

map(models_multilevel, interaction_visual)
