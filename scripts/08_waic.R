library(tidyverse)
library(brms)
library(bayesplot)
library(xtable)
library(loo, options(mc.cores = 8))

model_grouped <- readRDS(file = "models/dis_grouped.rds")
model_ungrouped <- readRDS(file = "models/dis_ungrouped.rds")

# loo(model_ungrouped, n_eff=NA)
model_grouped <- add_criterion(model_grouped, c("waic", "loo"))
model_ungrouped <- add_criterion(model_ungrouped, c("waic", "loo"))

waic_comp <- loo_compare(model_grouped, model_ungrouped, criterion = "loo")
print(waic_comp, simplify = F)

table_waic <- as_tibble(waic_comp[, c("elpd_diff", "se_diff", "looic", "se_looic")]) %>% 
  transmute("Model"= c("Grouped model", "Ungrouped Model"),
            "ELPD" = elpd_diff,
            "Standard error of ELPD" = se_diff,
            "LOO-IC"= looic,
            "Standard error of LOO-IC" = se_looic
            ) %>% 
  xtable(caption = "Predictive Power of models measured as WAIC",
         label = "table: loo")
print(xtable(table_waic), include.rownames = FALSE, file="psis-loo.txt")
