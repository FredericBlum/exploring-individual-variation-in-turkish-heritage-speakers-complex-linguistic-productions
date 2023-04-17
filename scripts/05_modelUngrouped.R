library(tidyverse)
library(brms)

data <- read_csv('data/data_preprocessed_dis.csv') %>% 
  mutate(Position=as.factor(Position))

model_ungrouped <- brm(
  data=data, family=bernoulli,
 formula=discourse ~ 0 + Position + Register + z_uttLength +
   (0 + Position | Speaker),
 prior=c(prior(normal(0, 2), class=b),
           prior(exponential(10), class=sd),
           prior(lkj(12), class=cor)),    
 iter=20000, warmup=4000, chains=8, cores=8,
 control=list(adapt_delta=0.98),
 file="models/dis_ungrouped.rds",
 seed=42,
 )
