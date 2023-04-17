library(tidyverse)
library(brms)

data <- read_csv('data/data_preprocessed_dis.csv') %>% 
  mutate(Position=as.factor(Position))

BF_discourses <- c()
nruns <- 10

for (i in 1:nruns) {
  print(i)
  # run alternative model
  model_grouped <- brm(
    data=data, family=bernoulli,
    formula=discourse ~ 0 + Position + Register + z_uttLength + 
      (0 + Position | Group/Speaker),
    prior=c(prior(normal(0, 2), class=b),
            prior(exponential(10), class=sd),
            prior(lkj(12), class=cor)), 
    iter=10000, warmup=4000, chains=4, cores=4,
    control=list(adapt_delta=0.98),
    save_pars=save_pars(all=TRUE)
  )
  
  # run bridge sampler
  lml_model_grouped <- bridge_sampler(model_grouped, silent=TRUE)
  rm(model_grouped)
  
  # run null model
  model_ungrouped <- brm(
    data=data, family=bernoulli,
    formula=discourse ~ 0 + Position + Register + z_uttLength + 
      (0 + Position | Speaker),
    prior=c(prior(normal(0, 2), class=b),
            prior(exponential(10), class=sd),
            prior(lkj(12), class=cor)),  
    iter=10000, warmup=4000, chains=4, cores=4,
    control=list(adapt_delta=0.98),
    save_pars=save_pars(all=TRUE)
  )
  
  # run bridge sampler
  lml_model_ungrouped <- bridge_sampler(model_ungrouped, silent=TRUE)
  rm(model_ungrouped)
  # compute Bayes factor
  BF_discourses <- c(BF_discourses, 
                     bayes_factor(lml_model_grouped, lml_model_ungrouped)$bf)
  print(BF_discourses)
}

bf_result <- BF_discourses %>% as_tibble() %>% 
  xtable()

# print(bf_result, file="bayes_factor.txt")

#########################################
# Import BF from cluster run
bf_import <- read_csv("bayes_factor.txt")
bf_summary <- bf_import %>% summarize(
  mean = mean(BF),
  sd = sd(BF)
  )
