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
library(GGally)
library(gridExtra)

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

# New occupation var
countries3 <- svy_recode(countries3, 'isco', 'occupation_recode', '1:2 = 5; 3 = 4; 4:5 = 3; 6:7 = 2; 8:9 = 1')
countries3 <- svy_recode(countries3, 'isco', 'occupation_recode_rev', '1:2 = 1; 3 = 2; 4:5 = 3; 6:7 = 4; 8:9 = 5')

# Long distance variables
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_upward', '5:4 = 4; 3 = 3; 2 = 2; 1 = 1')
countries3 <- svy_recode(countries3, 'occupation_recode', 'long_dist_downward', '1:2 = 4; 3 = 3; 4 = 2; 5 = 1')

countries3 <- svy_recode(countries3, 'isco', 'lowerclass', '3:9 = 1; 1:2 = 0; else = NA')
countries3 <- svy_recode(countries3, 'isco', 'lowerclass_extreme', '8:9 = 1; 1:2 = 0; else = NA')
countries3 <- svy_recode(countries3, 'isco', 'serviceclass_extreme', '8:9 = 0; 1:2 = 1; else = NA')

countries3 <- svy_recode(countries3, 'age_categories', 'postwelfare', '1:5 = 1; 6:10 = 0; else = NA')

##### Model Specification #####
dv <- "serviceclass_extreme"
age <- 1:10
# 6:10 is prewelfare
# 1:5 is postwelfare

age <- 1:10; cohort <- "fullcohort"

standard_covariates <- c("highcogn",
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
                         "spfwt0",
                         "lowerclass_extreme",
                         "serviceclass_extreme")

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
attributes(cnt_bind$pvnum) <- NULL

# multilevel models

standard_covariates <- c("pvnum", "non.cognitive", "age_categories", "postwelfare")
    
all_firstcovariates <- standard_covariates

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
    
covariate_list <-
    map(list(all_firstcovariates), function(iv) static_formula(dv, iv, random)) %>%
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
                  subset = gender == 1 & age_categories %in% age & highisced == 1,
                  weights = cnt_bind$spftw0,
                  family = "binomial")
    })
    models_multilevel2 <- map(covariate_list, function(formula) {
        multi_fun(formula = formula,
                  data = cnt_bind,
                  subset = gender == 1 & age_categories %in% age & lowisced == 1,
                  weights = cnt_bind$spftw0,
                  family = "binomial")
    })
    
models_multilevel <- list(models_multilevel1[[1]], models_multilevel2[[1]])

cnt_lowisced <- subset(cnt_bind, gender == 1 & age_categories %in% age & lowisced == 1)
cnt_highisced <- subset(cnt_bind, gender == 1 & age_categories %in% age & highisced == 1)

cogn_quantile_high <- Hmisc::wtd.quantile(cnt_highisced$pvnum,
                                         weights = cnt_highisced$spfwt0,
                                         probs = seq(0.01, 1, 0.01))

noncogn_quantile_high <- Hmisc::wtd.quantile(cnt_highisced$non.cognitive,
                                            weights = cnt_highisced$spfwt0,
                                            probs = seq(1, 0.01, -0.01))

high_isced <- tibble(pvnum = cogn_quantile_high,
                     non.cognitive = noncogn_quantile_high,
                     age_categories = modelr::typical(cnt_highisced$age_categories),
                     postwelfare = modelr::typical(cnt_highisced$postwelfare))


cogn_quantile_low <- Hmisc::wtd.quantile(cnt_lowisced$pvnum,
                             weights = cnt_lowisced$spfwt0,
                             probs = seq(0.01, 1, 0.01))

noncogn_quantile_low <- Hmisc::wtd.quantile(cnt_lowisced$non.cognitive,
                                     weights = cnt_lowisced$spfwt0,
                                     probs = seq(1, 0.01, -0.01))

low_isced <- tibble(pvnum = cogn_quantile_low,
                   non.cognitive = noncogn_quantile_low,
                   age_categories = modelr::typical(cnt_lowisced$age_categories),
                   postwelfare = modelr::typical(cnt_lowisced$postwelfare))

high_isced$pred <- predict(models_multilevel[[1]], newdata = high_isced, re.form = NA, type = "response")
low_isced$pred <- predict(models_multilevel[[2]], newdata = low_isced, re.form = NA, type = "response")

add_predictions_se <- function(data, model) {
    se <- predict(model, newdata = data, se.fit = T, re.form = NA)$se.fit
    data[["se"]] <- se
    data
}

high_isced <- add_predictions_se(high_isced, models_multilevel[[1]])
low_isced <- add_predictions_se(low_isced, models_multilevel[[2]])



high_isced$isced <- "High ISCED"
low_isced$isced <- "Low ISCED"

high_isced$rank_noncognitive <- 100:1
high_isced$rank_cognitive <- 1:100

low_isced$rank_noncognitive <- 100:1
low_isced$rank_cognitive <- 1:100

prob_isced_data <- bind_rows(high_isced, low_isced)

da <- prob_isced_data %>%
    select(pvnum, non.cognitive, starts_with("rank"), pred, isced, se) %>%
    mutate(pred = pred * 100,
           se = se * 100,
           lower = pred - se,
           upper = pred + se) %>%
    unite(rank_label, rank_noncognitive, rank_cognitive, sep = " - ", remove = F) %>%
    mutate(rank = rep(1:100, 2),
           cogn_noncogn_cat =
               case_when(.$rank_cognitive %in% 1:30 & .$rank_noncognitive %in% 70:100 ~ "Bottomcogn_topnoncogn",
                         .$rank_cognitive %in% 70:100 & .$rank_noncognitive %in% 1:30 ~ "Topcogn_bottomnoncogn")) %>%
    gather(rank_category, value, rank_cognitive, rank_noncognitive)

da_summary <-
    da %>%
    group_by(isced, cogn_noncogn_cat) %>%
    summarize(avg_pred = mean(pred),
              avg_se = mean(se),
              med_pred = median(pred),
              med_se = median(se)) %>%
    mutate(rank = c(17, 87, NA),
           lower_avg = avg_pred - 1.96 * avg_se,
           upper_avg = avg_pred + 1.96 * avg_se,
           lower_med = med_pred - 1.96 * avg_se,
           upper_med = med_pred + 1.96 * avg_se)

rect_df <- tibble(xmin = c(-2, 70), xmax = c(30, 102), ymin = c(-20, -20), ymax = c(125, 125))


da %>%
    ggplot(aes(x = rank, y = pred, colour = isced)) +
    # geom_ribbon(aes(group = isced, ymin = lower, ymax = upper, colour = NULL), fill = "grey70", alpha = 0.3) +
    # geom_errorbar(data = filter(da, rank %in% c(1, 100)), aes(ymin = lower, ymax = upper), width = 2) +
    geom_line(aes(group = isced)) +
    scale_x_continuous(breaks = c(0, 15, 30, 70, 85, 10),
                       labels = c("",
                                  "Top 30% non cognitive \n Bottom 30% cognitive",
                                  "",
                                  "",
                                  "Top 30% cognitive \n Bottom 30% cognitive",
                                  "")) +
    geom_rect(data = rect_df, aes(NULL,
                                  NULL,
                                  xmin = xmin,
                                  xmax = xmax,
                                  ymin = ymin,
                                  ymax = ymax), fill = "azure4", colour = "white", alpha = 0.1) +
    geom_point(data = da_summary, aes(rank, med_pred, fill = isced)) +
    geom_errorbar(data = da_summary, aes(x = rank, y = NULL, ymin = lower_med, ymax = upper_med, fill = isced), width = 2) +
    coord_cartesian(xlim = c(-2, 102), ylim = c(-20, 125), expand = c(0, 0)) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.ticks.x = element_blank())

m <- da %>%
    filter(!is.na(cogn_noncogn_cat)) %>%
    mutate(cogn_label = case_when(.$isced == "High ISCED" & .$rank <= 30 ~ "bottcogn_topnoncogn_high",
                                  .$isced == "High ISCED" & .$rank >= 70 ~ "topcogn_bottncogn_high",
                                  .$isced == "Low ISCED" & .$rank <= 30 ~ "bottcogn_topnoncogn_low",
                                  .$isced == "Low ISCED" & .$rank >= 70 ~ "topcogn_bottnoncogn_low"),
           cogn_label_fac = factor(cogn_label, levels = c("topcogn_bottncogn_high",
                                                      "bottcogn_topnoncogn_high",
                                                      "topcogn_bottnoncogn_low",
                                                      "bottcogn_topnoncogn_low"), ordered = T))

data_m <-
    m %>%
    select(pred, cogn_label_fac, isced) %>%
    mutate(cogn_num = factor(as.numeric(cogn_label_fac), levels = c("4", "3", "2", "1")))

dist_pred <-
    data_m %>%
    ggplot(aes(x = pred, fill = isced)) + 
    geom_histogram(aes(y = ..count..), alpha = .5, bins = 60) + 
    scale_x_continuous(name = NULL, breaks = seq(0, 100, 10)) +
    scale_y_continuous(name = NULL, labels = NULL) +
    scale_fill_discrete(name = "") +
    ggtitle("Probability of achieving service class") +
    theme(legend.position = c(0.95, 0.95),
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, vjust = -5))

(graph_m <-
    data_m %>%
    ggparcoord(columns = c(4, 1),
               groupColumn = 3,
               alphaLines = 0.2))

labels <- c("Top cognitive \n Bottom non cognitive",
            "Bottom cognitive \n Top non cognitive",
            "Top cognitive \n Bottom non cognitive",
            "Bottom cognitive \n Top non cognitive")

x_pos <- rep(0.85, 4)
y_pos <- c(1.35, 0.45, -0.45, -1.35)

cogn_pred <-
    graph_m +
    annotate("text", label = labels , x = x_pos, y = y_pos, size = 4) +
    scale_x_discrete(name = NULL, labels = NULL) +
    scale_y_continuous(name = NULL, labels = NULL) +
    scale_color_discrete(guide = F) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.line.x = element_line(colour = "black")) +
    coord_flip(xlim = c(1.2, 1.40), ylim = c(-1.8, 1.8))

grid.arrange(dist_pred, cogn_pred, ncol=1, nrow=2, widths = 10, heights = c(2, 4))

# You left off here.
# You want to make the bottom plot better.
# Align both plots exactly.
# Maybe change the font
# Get a prettier theme (colors of the plot and so on.)
# Plot looks too white.

# Show what these distributions are? Maybe as the X label of the density
# plot


# If the DV is not binery, run lmer, if it is, then use glmer

add_predictions_se <- function(data, model) {
    se <- predict(model, newdata = data, se.fit = T, re.form = NA)$se.fit
    data[["se"]] <- se
    data
}

cnt_highisced <-
    cnt_bind %>%
    group_by(country) %>%
    filter(gender == 1, age_categories %in% age, highisced == 1) %>%
    split(.$country)

cnt_lowisced <-
    cnt_bind %>%
    group_by(country) %>%
    filter(gender == 1, age_categories %in% age, lowisced == 1) %>%
    split(.$country)


country_predictions <- function(cnt_data, isced_name) {

    cogn_quantile <-
        cnt_data %>%
        {Hmisc::wtd.quantile(.$pvnum, weights = .$spfwt0, probs = seq(0.01, 1, 0.01))}
    
    noncogn_quantile <-
        cnt_data %>%
        {Hmisc::wtd.quantile(.$non.cognitive, weights = .$spfwt0, probs = seq(0.01, 1, 0.01))}
    
    isced <- tibble(pvnum = cogn_quantile,
                         non.cognitive = noncogn_quantile,
                         age_categories = modelr::typical(cnt_data$age_categories),
                         postwelfare = modelr::typical(cnt_data$postwelfare))
    
    mod <- glm(serviceclass_extreme ~ pvnum + non.cognitive + age_categories + postwelfare,
        data = cnt_data,
        family = "binomial")
    
    isced$pred <- predict(mod, newdata = isced, type = "response")
    isced <- add_predictions_se(isced, mod)
    isced$isced <- isced_name
    
    isced$rank_noncognitive <- 100:1
    isced$rank_cognitive <- 1:100
    isced
}

high_isced <-
    map(cnt_highisced, ~ country_predictions(.x, "High ISCED")) %>%
    enframe() %>%
    unnest()

low_isced <-
    map(cnt_lowisced, ~ country_predictions(.x, "Low ISCED")) %>%
    enframe() %>%
    unnest()


prob_isced_data <- bind_rows(high_isced, low_isced)

# you left off here.
da <- prob_isced_data %>%
    select(name, pvnum, non.cognitive, starts_with("rank"), pred, isced, se) %>%
    mutate(pred = pred * 100,
           se = se * 100,
           lower = pred - se,
           upper = pred + se) %>%
    unite(rank_label, rank_noncognitive, rank_cognitive, sep = " - ", remove = F) %>%
    mutate(rank = rep(1:100, 2),
           cogn_noncogn_cat =
               case_when(.$rank_cognitive %in% 1:30 & .$rank_noncognitive %in% 70:100 ~ "Bottomcogn_topnoncogn",
                         .$rank_cognitive %in% 70:100 & .$rank_noncognitive %in% 1:30 ~ "Topcogn_bottomnoncogn")) %>%
    gather(rank_category, value, rank_cognitive, rank_noncognitive)

da_summary <-
    da %>%
    group_by(isced, cogn_noncogn_cat) %>%
    summarize(avg_pred = mean(pred),
              avg_se = mean(se),
              med_pred = median(pred),
              med_se = median(se)) %>%
    mutate(rank = c(17, 87, NA),
           lower_avg = avg_pred - 1.96 * avg_se,
           upper_avg = avg_pred + 1.96 * avg_se,
           lower_med = med_pred - 1.96 * avg_se,
           upper_med = med_pred + 1.96 * avg_se)

rect_df <- tibble(xmin = c(-2, 70), xmax = c(30, 102), ymin = c(-20, -20), ymax = c(125, 125))

