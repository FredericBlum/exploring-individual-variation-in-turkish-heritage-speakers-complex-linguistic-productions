library(tidyverse)
library(posterior)
library(brms)
library(bayesplot)
library(loo, options(mc.cores=4))
library(xtable)
library(patchwork)

data <- read_csv('data/data_preprocessed_dis.csv') %>% 
  mutate(Position=as.factor(Position))

model_grouped <- readRDS(file="models/dis_grouped.rds")
model_ungrouped <- readRDS(file="models/dis_ungrouped.rds")
model_samples <- readRDS(file="models/dis_grouped.rds")

if (file.exists("models/grouped_idx.rds")) {
  sim_data <- readRDS(file="models/grouped_idx_predictions.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  sim_data <- posterior_predict(model_grouped, ndraws=2000, cores=getOption("mc.cores", 8))
  saveRDS(sim_data, file="models/grouped_idx_predictions.rds")  
}

lp_grouped <- log_posterior(model_grouped)
np_grouped <- nuts_params(model_grouped)
posterior_grouped <- as_draws_array(model_grouped)

observations <- data %>% .$discourse
group <- data %>% .$Group

stats <- ppc_stat_grouped(observations, sim_data, group=group, binwidth=0.0005)+ 
  scale_x_continuous(breaks=c(0.08, 0.10, 0.12), limits=c(0.07, 0.12)) +
  theme(legend.position="none")

# viol <- ppc_violin_grouped(observations, sim_data, group=group)
dens <- ppc_dens_overlay(observations, sim_data)

dens$scales$scales[[1]]$labels <- c("data", "simulated")

both_pp_checks <- (stats)/(dens)
ggsave("images/pp_checks.png", both_pp_checks, width=2000, height=1400, units="px")

labels <- c(
  `b_PositionFinal` = "Final",
  `b_PositionInitial` = "Initial",
  `b_PositionMedial` = "Medial"
)

# trank plots for Position-values
both_traces <- mcmc_trace(posterior_grouped, regex_pars=c("b_Position*"), 
                          facet_args=list(ncol=3, strip.position="left")) +
  theme(legend.position="none")

both_ranks <- mcmc_rank_overlay(
  posterior_grouped,
  regex_pars=c("b_Position*"),
  facet_args=list(
    ncol=3, strip.position="left"
    # labeller = as_labeller(labels)
    )) +
  coord_cartesian(ylim=c(700, 900)) + 
  theme(legend.position="none")

convergence <- both_traces / both_ranks
ggsave("images/convergence.png", convergence, width=2000, height=1300, units="px")
