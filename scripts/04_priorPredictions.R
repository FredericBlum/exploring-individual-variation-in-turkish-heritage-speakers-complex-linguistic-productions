library(tidyverse)
library(bayesplot)
library(brms)
library(patchwork)
library(tidybayes)
library(viridis)

data <- read_csv('data/data_preprocessed_dis.csv') %>% mutate(Position = as.factor(Position))

model_prior <- 
  brm(data = data, family = bernoulli,
      formula = discourse ~ 0 + Position + Register + z_uttLength + 
        (0 + Position | Group/Speaker),
      prior = c(prior(normal(0, 2), class = b),
                prior(exponential(10), class = sd),
                prior(lkj(12), class = cor)),    
      iter = 5000, warmup = 2000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.98),
      sample_prior = "only",
      file = "models/grouped_samples.rds",
      seed = 42)

color_scheme_set("pink")

para_vals <- posterior_summary(model_prior) %>% 
  data.frame() %>% as_tibble(rownames = "parameter")

hpdi_vals <- posterior_interval(model_prior, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames = "parameter") %>% 
  rename(hpdi_low = X5.5., hpdi_high = X94.5.)

para_vals <- para_vals %>% left_join(hpdi_vals)


raw_markers <- data %>% .$discourse

if (file.exists("data/models/prior_pred.rds")) {
  prior_markers <- readRDS(file = "models/prior_pred.rds")
} else{
  prior_markers <- posterior_predict(model_prior, ndraws = 200, 
                                          cores = getOption("mc.cores", 4))
  saveRDS(prior_markers, file = "models/prior_pred.rds")
} 

prior_overlay <- ppc_dens_overlay(raw_markers, prior_markers, 
                                  alpha = 0.5, size = 0.7, adjust = 1)
prior_overlay$scales$scales[[1]]$labels <- c("data", "prior")

ggsave("images/prior_simData.png", prior_overlay, scale = 1.1,
       width = 2000, height = 1400, units = "px")
