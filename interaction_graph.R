# Script to get the interaction plots (figure 3-6), run this code and change all combinations
# from section: CHANGE

# Another problem. The interaction table 7 was produced with the quantiles 20-40
# for top and bottom cogn/non-cognitive. Here the interaction is calculated with
# the 30-30 quantiles.

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

setwd("./data/")

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

# New occupation var
countries3 <- svy_recode(countries3, 'isco', 'occupation_recode', '1:2 = 5; 3 = 4; 4:5 = 3; 6:7 = 2; 8:9 = 1')
countries3 <- svy_recode(countries3, 'isco', 'occupation_recode_rev', '1:2 = 1; 3 = 2; 4:5 = 3; 6:7 = 4; 8:9 = 5')

# Long distance variables
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_upward', '5:4 = 4; 3 = 3; 2 = 2; 1 = 1')
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_downward', '1:2 = 4; 3 = 3; 4 = 2; 5 = 1')

countries3 <- svy_recode(countries3, 'isco', 'lowerclass', '3:9 = 1; 1:2 = 0; else = NA')
countries3 <- svy_recode(countries3, 'age_categories', 'postwelfare', '1:5 = 1; 6:10 = 0; else = NA')
countries3 <- svy_recode(countries3, 'dadimmigrant', 'dadimmigrant', "2 = 0; 1 = 1; else = NA")

variables <- c("long_dist_upward", "long_dist_downward")
titles <- c("Continuous upward", "Continuous downward")

interaction_vars <- c("cognitive_top30_bottom30", "noncognitive_top30_bottom30")



##### Change #####
# Change to 1 for the upward graphs and 2 for the downward graphs
dv <- variables[2]
depvar_title <- titles[2]

# Change to 1 for the high/low isced specification and 0 for the opposite
high_low_isced <- 0

######

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
                             "dadimmigrant",
                             "spfwt0",
                             "long_dist_upward",
                             "long_dist_downward")
    
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
country_subset <- c("United States", "Sweden", "Norway", "Netherlands", "Japan", "Germany", "France",
                    "Finland", "Canada", "Denmark")
# country_subset <- c("United States", "Netherlands", "Japan", "Germany", "France", "Canada")

cnt_bind <- Reduce(rbind, cnts)
cnt_bind$pvnum <- scale(cnt_bind$pvnum)[, 1] # because it returns 1 col matrix
cnt_bind[interaction_vars] <- map_df(cnt_bind[interaction_vars], as.factor)
# cnt_bind <- filter(cnt_bind, country %in% country_subset)

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
                         "postwelfare",
                         "dadimmigrant")
        
all_firstcovariates <- standard_covariates

covariate_list <- paste(dv, paste0(all_firstcovariates, collapse = " + "), sep = " ~ ")

# If the DV is not binery, run lmer, if it is, then use glmer
type_model <- ifelse(length(na.omit(unique(cnt_bind[, dv]))) > 2, "lm", "glm")

iv <- list(all_firstcovariates)[[1]]
        
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
                      subset = gender == 1 & age_categories %in% age & highisced == high_low_isced)
        })

models_multilevel2 <- map(covariate_list, function(formula) {
            multi_fun(formula = formula,
                      data = cnt_bind,
                      subset = gender == 1 & age_categories %in% age & lowisced == high_low_isced)
        })

models_multilevel <- list(models_multilevel1[[1]], models_multilevel2[[1]])

model <- models_multilevel[[1]]

add_predictions_se <- function(data, model) {
    se <- predict(model, newdata = data, se.fit = T, re.form = NA)$se.fit
    data[["se"]] <- se
    data
}


interaction_visual <- function(model) {
    
    title <- ifelse(grepl("upward", as.character(attr(model$terms, "variables"))[2]),
                    "upward", "downward")
    
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

file_name <- paste0(dv, "_for_", ifelse(high_low_isced, "highisced", "lowisced"), "_interaction.png")

ggsave(filename = file_name,
       path = directory)