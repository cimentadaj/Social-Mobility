library(car)
library(broom)
library(tidyverse)
library(forcats)
library(cimentadaj)
library(ggthemes)
library(lme4)
library(MuMIn)
library(GGally)
library(gridExtra)
library(artyfarty)

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

countries3 <- svy_recode(countries3, 'isco', 'lowerclass_extreme', '8:9 = 1; 1:2 = 0; else = NA')
countries3 <- svy_recode(countries3, 'isco', 'serviceclass_extreme', '8:9 = 0; 1:2 = 1; else = NA')
countries3 <- svy_recode(countries3, 'age_categories', 'postwelfare', '1:5 = 1; 6:10 = 0; else = NA')

##### Model Specification #####
dv <- "serviceclass_extreme"
age <- 1:10
cohort <- "fullcohort"

all_firstcovariates <- c("pvnum", "non.cognitive", "age_categories", "postwelfare")
vars_to_subset <- c(all_firstcovariates, "gender", "lowisced", "highisced")
digits <- 2

# Loop through country datasets and names, create a column with that country's name
# and select all variables in vars_subset (which includes the country var)
cnts <- map2(countries3, names(countries3), function(data, names) {
    data$designs[[1]]$variables %>%
        mutate(country = names,
               cohort = ifelse(age_categories <= 5, "post", "pre")) %>%
        select_(.dots = map(c(vars_to_subset, dv, "country"), as.name))
})

cnt_bind <- Reduce(rbind, cnts)
cnt_bind$pvnum <- scale(cnt_bind$pvnum)
attributes(cnt_bind$pvnum) <- NULL

rhs_sequence <- function(iv) {
    stop_message(length(iv) < 1, "iv must have length >= 1")
    warning_message(any(is.na(iv)), "NA's found in iv. Removing them.")
    
    non_na_iv <- na.omit(iv)
    model_combination <- map(seq_along(non_na_iv), ~ seq(1:.x))
    rhs <- map(model_combination, ~ paste(non_na_iv[.x], collapse = " + "))
    
    rhs
}
static_formula <- function(dv, rhs) {
    new_dv <- paste0(dv, " ~ 1")
    rhs <- paste0(c(rhs), collapse = " + ")
    as.formula(paste0(c(new_dv, rhs), collapse = " + "))
}

covariate_list <-
    map(list(all_firstcovariates), function(iv) static_formula(dv, iv)) %>%
    `c`(recursive = T)

# Pass that list to the glm to run two different models

cnt_lowisced <- subset(cnt_bind, gender == 1 & age_categories %in% age & lowisced == 1)
cnt_highisced <- subset(cnt_bind, gender == 1 & age_categories %in% age & highisced == 1)

country_lowisced_split <- split(cnt_lowisced, cnt_lowisced$country)
country_highisced_split <- split(cnt_highisced, cnt_highisced$country)

models_low <-
    map(country_lowisced_split, ~ {
        glm(formula = covariate_list[[1]],
            data = .x,
            # weights = .x$spfwt0, # weights not working for some reason
            family = "binomial")
    })

models_high <-
    map(country_highisced_split, ~ {
        glm(formula = covariate_list[[1]],
            data = .x,
            # weights = .x$spfwt0, # weights not working for some reason
            family = "binomial")
    })

quantile_est <- function(df) {
    
    country_list <- map(df, ~ {
        cogn_quantile <- Hmisc::wtd.quantile(.x$pvnum,
                                             weights = .x$spfwt0,
                                             probs = seq(0.01, 1, 0.01))
        
        noncogn_quantile <- Hmisc::wtd.quantile(.x$non.cognitive,
                                                weights = .x$spfwt0,
                                                probs = seq(1, 0.01, -0.01))
        
        isced <- tibble(pvnum = cogn_quantile,
                        non.cognitive = noncogn_quantile,
                        age_categories = modelr::typical(.x$age_categories),
                        postwelfare = modelr::typical(.x$postwelfare))
        isced
    })
    country_list
}

high_isced <- quantile_est(country_highisced_split)
low_isced <- quantile_est(country_lowisced_split)

high_isced <-
    map2(high_isced, models_high, ~ {
        .x$pred <- predict(.y, newdata = .x, type = "response")
        .x
    })

low_isced <-
    map2(low_isced, models_low, ~ {
        .x$pred <- predict(.y, newdata = .x, type = "response")
        .x
    })

add_predictions_se <- function(data, model) {
    se <- predict(model, newdata = data, type = "response", se.fit = T)$se.fit
    data[["se"]] <- se
    data
}

high_isced <- map2(high_isced, models_high, add_predictions_se)
low_isced <- map2(low_isced, models_low, add_predictions_se)

high_isced <- map(high_isced, ~ {
    .x$isced <- "High ISCED"
    .x$rank_noncognitive <- 100:1
    .x$rank_cognitive <- 1:100
    .x
})

low_isced <- map(low_isced, ~ {
    .x$isced <- "Low ISCED"
    .x$rank_noncognitive <- 100:1
    .x$rank_cognitive <- 1:100
    .x
})

high_isced <-
    high_isced %>%
    enframe() %>%
    unnest(value) %>%
    rename(country = name)

low_isced <-
    low_isced %>%
    enframe() %>%
    unnest(value) %>%
    rename(country = name)

prob_isced_data <- bind_rows(high_isced, low_isced)

prob_isced_data <-
    prob_isced_data %>%
    select(-age_categories, -postwelfare) %>%
    mutate(pred = pred * 100,
           se = se * 100,
           lower = pred - se,
           upper = pred + se) %>%
    unite(rank_label, rank_noncognitive, rank_cognitive, sep = " - ", remove = F) %>%
    mutate(rank = rep(1:100, 42), # 42 because it's 21 countries for both low and high isced
           cogn_noncogn_cat =
               case_when(.$rank_cognitive %in% 1:30 & .$rank_noncognitive %in% 70:100 ~ "Bottomcogn_topnoncogn",
                         .$rank_cognitive %in% 70:100 & .$rank_noncognitive %in% 1:30 ~ "Topcogn_bottomnoncogn")) %>%
    gather(rank_category, value, rank_cognitive, rank_noncognitive)

top <- 90
top_b <- 70
bottom_b <- 30
bottom <- 10

data_ready <-
    prob_isced_data %>%
    mutate(cogn_label = case_when(.$isced == "High ISCED" & .$rank <= 30 ~ top_b,
                                  .$isced == "High ISCED" & .$rank >= 70 ~ top,
                                  .$isced == "Low ISCED" & .$rank <= 30 ~ bottom,
                                  .$isced == "Low ISCED" & .$rank >= 70 ~ bottom_b),
           cogn_label_fac = factor(cogn_label,
                                   levels = c(top, top_b, bottom_b, bottom),
                                   labels = c("topcogn_bottncogn_high",
                                              "bottcogn_topnoncogn_high",
                                              "topcogn_bottnoncogn_low",
                                              "bottcogn_topnoncogn_low"), ordered = T))
data_m <-
    data_ready %>%
    mutate(cogn_num = factor(as.numeric(cogn_label_fac), levels = c("4", "3", "2", "1"))) %>%
    select(country, pred, cogn_label_fac, isced, cogn_num, cogn_label) %>%
    map_if(is_double, round, 2) %>%
    as_tibble() %>%
    filter(country == "United States")

dist_pred <-
    data_m %>%
    ggplot(aes(x = pred, fill = isced)) + 
    geom_density(alpha = .5) +
    geom_rug(data = filter(data_m, is.na(cogn_label_fac)), aes(x = pred), color = "black", inherit.aes = FALSE) +
    scale_x_continuous(name = NULL, breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%"),
                       lim = c(0, 100)) +
    scale_y_continuous(name = NULL, labels = NULL) +
    scale_fill_manual(name = "", values = c("red", "blue")) +
    ggtitle("Probability of achieving service class for U.S") +
    coord_cartesian(expand = FALSE) +
    artyfarty::theme_scientific() +
    theme(legend.position = c(0.95, 0.95),
          text = element_text(size = 18))

graph_m <-
    data_m %>%
    ggparcoord(columns = c(6, 2),
               groupColumn = 4,
               alphaLines = 0.2,
               scale = "globalminmax")

labels <- c("Bottom cognitive \n Top non cognitive",
            "Top cognitive \n Bottom non cognitive",
            "Bottom cognitive \n Top non cognitive",
            "Top cognitive \n Bottom non cognitive")

cogn_pred <-
    graph_m +
    scale_x_discrete(name = NULL, labels = NULL) +
    scale_y_continuous(name = NULL,
                       limits = c(0, 100),
                       breaks = c(10, 30, 70, 90),
                       labels = labels) +
    scale_colour_manual(guide = F, values = c("red", "blue")) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.line.x = element_line(colour = "black")) +
    coord_flip(expand = FALSE) +
    artyfarty::theme_scientific() +
    theme(axis.text.x = element_text(size = 16))

grid.arrange(dist_pred, cogn_pred, ncol=1, nrow=2, widths = 10, heights = c(2, 4))

ggsave("US.png", path = "./graphs/")
