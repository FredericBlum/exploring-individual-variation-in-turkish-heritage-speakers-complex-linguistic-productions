library(tidyverse)
library(ggdist)
#library(patchwork)
library(viridis)

n = 1e5
set.seed(42)


intercepts <- tibble(x = c(rnorm(n, -2.3, 0.3))) %>%
  mutate(group = 'alpha%~% Normal(-3, 1)') %>% 
  ggplot(aes(x = x)) +
  geom_density(aes(fill = group)) +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  scale_y_continuous(breaks = NULL) +
  ylab("Density of values") +
  scale_x_continuous(name = "Intercept values on log-scale", 
                     #limits = c(-1.25, 1.25), 
                     breaks = seq(from = -6, to = 1, by = 0.5)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +
  labs(title = "β ~ Normal(-3, 1)")

#########################################
###     influence of predictors       ###
#########################################
predictors <- tibble(x = c(rnorm(n, 0, 2))) %>%
  mutate(group = 'beta%~% Normal(0, 0.3)') %>% 
  ggplot(aes(x = x)) +
  geom_density(aes(fill = group)) +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  scale_y_continuous(breaks = NULL) +
  ylab("Density of values") +
  scale_x_continuous(name = "Predictor values on log-scale", 
                     #limits = c(-5, 5), 
                     breaks = seq(from = -5, to = 5, by = 0.5)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +
  labs(title = "β ~ Normal(0, 2)")

# exp -5 = -0.006
# exp -2 = 0.135s
# exp 0 = 1

sigma <- rexp(n, rate = 10) %>% 
  tibble() %>% 
  mutate(group = 'sigma%~% exp(10)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill = group)) +
  scale_y_continuous(breaks = NULL,
                     name = "Density of values") +
  scale_x_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2),
                     #limits = c(0, 2),
                     name = "Standard deviation of varying intercepts on log-scale") +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +  
  labs(title = "σ ~ Exp(8)")

#########################################
###     varying slopes matrix         ###
#########################################
lkjcorr <- rlkjcorr_marginal(n, K = 2, eta = 12) %>% tibble(x = .) %>% 
  mutate(group = 'R%~% LKJcorr(12)') %>% 
  ggplot(aes(x = x, fill = group)) + 
  geom_density() +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(name = "Correlation of varying intercepts and slopes",
                     breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +
  ylab("Density of values") +
  labs(title = "R ~ LKJcorr(12)")
