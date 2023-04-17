library(tidyverse)
library(brms)
library(bayesplot)
library(viridis)
library(xtable)

data <- read_csv('data/data_preprocessed_dis.csv') %>%
  mutate(Position=as.factor(Position))
model_grouped <- readRDS(file="models/dis_grouped.rds")

invlogit <- function(x){
  exp(x) / (1 + exp(x))
}

#########################################
###     Parameters                    ###
#########################################
hpdi_vals89 <- posterior_interval(model_grouped, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_89_low=X5.5., hpdi_89_high=X94.5.)

hpdi_vals99 <- posterior_interval(model_grouped, prob=0.997) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_low=X0.15., hpdi_high=X99.85.)

para_vals <- posterior_summary(model_grouped) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  select(Parameter, Estimate) %>% 
  mutate(Estimate=Estimate) %>% 
  left_join(hpdi_vals89) %>% left_join(hpdi_vals99)

rm(hpdi_vals89, hpdi_vals99)

################################
fix_eff <- para_vals %>% filter(str_detect(Parameter, "b_Position")) %>% 
  mutate(Parameter=str_replace(Parameter, "b_Position", ""))

# gamma=fixef(model_grouped)[1:3, 1]
# disadvantage: no uncertainty involved

group_estimates <- para_vals %>% filter(grepl("^r_Group\\[.*", Parameter)) %>%
  select(Parameter, Estimate, hpdi_89_low, hpdi_89_high, hpdi_high, hpdi_low) %>% 
  mutate(Parameter=gsub("r_Group\\[(.*),(.*)]", "\\1_\\2", Parameter)) %>% 
  separate(sep="_", col=Parameter, into=c("Group", "Parameter")) %>% 
  mutate(Parameter=str_replace(Parameter, "Position", "")) %>% 
  left_join(fix_eff, by="Parameter") %>% 
  transmute(Group=Group,
            Estimate=Estimate.x + Estimate.y,
            hpdi_89_low=hpdi_89_low.x + hpdi_89_low.y,
            hpdi_89_high=hpdi_89_high.x + hpdi_89_high.y,
            hpdi_low=hpdi_low.x + hpdi_low.y,
            hpdi_high=hpdi_high.x + hpdi_high.y,
            Intercept=Estimate.y,
            Parameter=factor(Parameter, c("Initial", "Medial", "Final")))
group_estimates[c(2:7)] <- lapply(group_estimates[c(2:7)], invlogit)

speaker_estimates <- para_vals %>% filter(grepl("^r_Group:Speaker.*", Parameter)) %>%
  select(Parameter, Estimate, hpdi_89_low, hpdi_89_high, hpdi_high, hpdi_low) %>% 
  mutate(Parameter=gsub("r_Group:Speaker\\[(.*),(.*)]", "\\1__\\2", Parameter)) %>% 
  separate(sep="__", col=Parameter, into=c("Speaker", "Parameter")) %>% 
  separate(sep="_", col=Speaker, into=c("Group", "Speaker")) %>% 
  mutate(Parameter=str_replace(Parameter, "Position", "")) # %>%
  
  # Comment out here for speaker deviation plots
#   left_join(group_estimates, by=c("Group", "Parameter")) %>%
#   transmute(Group=Group, Speaker=Speaker, Intercept=Intercept,
#             Estimate=Estimate.x + Estimate.y,
#             hpdi_89_low=hpdi_89_low.x + hpdi_89_low.y,
#             hpdi_89_high=hpdi_89_high.x + hpdi_89_high.y,
#             hpdi_high=hpdi_high.x + hpdi_high.y,
#             hpdi_low=hpdi_low.x + hpdi_low.y,
#             Parameter=factor(Parameter, c("Initial", "Medial", "Final")))
# 
# speaker_estimates[c(3:8)] <- lapply(speaker_estimates[c(3:8)], invlogit)

group_deviations <- group_estimates %>%
  select(Group, Estimate, Parameter) %>%
  group_by(Parameter) %>% summarize(
    sd=round(sd(Estimate, na.rm=TRUE), 3)
  ) %>%
  arrange(Parameter) %>% mutate(Group="Group level") %>%
  pivot_wider(names_from="Parameter", values_from="sd")

speaker_estimates[c(4:8)] <- lapply(speaker_estimates[c(4:8)], invlogit)
speaker_deviations <- speaker_estimates %>%
  pivot_longer(Estimate:hpdi_low, names_to="params", values_to="Value") %>%
  filter(params=="Estimate") %>%
  group_by(Parameter, Group) %>% summarize(
    sd=round(sd(Value, na.rm=TRUE),2)
  ) %>%
  arrange(Parameter, Group) %>% pivot_wider(names_from="Parameter", values_from="sd") %>%
  mutate(Group=paste("Speaker:", Group)) %>%
  rbind(group_deviations) %>%
  xtable(caption="Standard deviation for speaker-groups and the group-level parameter",
         label="table: sd_vars")
print(xtable(speaker_deviations), include.rownames=FALSE)

#########################################
# Check logit conversion for speakers! Has probably been deactivated above
group_plot <- group_estimates %>% 
  ggplot(aes(x=Group, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Group), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(width=0.3, ymin=hpdi_low, ymax=hpdi_high)) +
  geom_hline(aes(yintercept=Intercept), color="red") +
  scale_fill_viridis(discrete =T, begin=0, end=0.7) +
  facet_wrap(~Parameter) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

ggsave("images/group_plot_dis.png", group_plot, scale=1,
       width=2000, height=1200, units="px")
  
speaker_plot <- speaker_estimates %>% 
  ggplot(aes(y=Estimate, x=reorder(Estimate, Speaker))) +
  geom_pointrange(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, color=Group)) +  
  geom_hline(aes(yintercept=Intercept), color="red", size=1) +
  facet_grid(Group~Parameter, scales="free_x") +
  scale_y_continuous(name=NULL) +
  scale_color_viridis(discrete=T, end=0.7) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_alpha(guide="none") +
  labs(fill="") +
  theme(strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        text=element_text(size=20),
        legend.position='bottom')

ggsave("images/speaker_plot_dis.png", speaker_plot, scale=1,
       width=4000, height=4000, units="px")

speaker_ind1 <- speaker_estimates %>% filter(Group == "Turkey") %>% 
  ggplot(aes(y=Estimate, x=Parameter, group=Speaker)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Parameter), 
                size=0.5, width=0.5, fatten=0) +  
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  facet_wrap(~Speaker, scales="free_x", ncol=6)+
  scale_y_continuous(name=NULL, breaks=c(0.0, 0.4, 0.8)) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_fill_viridis(discrete=T, end=0.7) +
  scale_alpha(guide="none") +
  labs(fill="") +
  theme(strip.text.x=element_text(size=18),
        text=element_text(size=16),
        legend.text=element_text(size=22),
        legend.position='bottom')

ggsave("images/speaker_plot_slopes1.png", speaker_ind1, scale=1,
       width=4000, height=5000, units="px")

speaker_ind2 <- speaker_estimates %>% filter(Group == "Germany") %>% 
  ggplot(aes(y=Estimate, x=Parameter, group=Speaker)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Parameter), 
                size=0.5, width=0.5, fatten=0) +  
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  facet_wrap(~Speaker, scales="free_x", ncol=6)+
  scale_y_continuous(name=NULL, breaks=c(0.0, 0.4, 0.8)) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_fill_viridis(discrete=T, end=0.7) +
  scale_alpha(guide="none") +
  labs(fill="") +
  theme(strip.text.x=element_text(size=18),
        text=element_text(size=16),
        legend.text=element_text(size=22),
        legend.position='bottom')

ggsave("images/speaker_plot_slopes2.png", speaker_ind2, scale=1,
       width=4000, height=5000, units="px")

speaker_ind3 <- speaker_estimates %>% filter(Group == "USA") %>% 
  ggplot(aes(y=Estimate, x=Parameter, group=Speaker)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Parameter), 
                size=0.5, width=0.5, fatten=0) +  
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  facet_wrap(~Speaker, scales="free_x", ncol=6) +
  scale_y_continuous(name=NULL, breaks=c(0.0, 0.4, 0.8)) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_fill_viridis(discrete=T, end=0.7) +
  scale_alpha(guide="none") +
  labs(fill="") +
  theme(strip.text.x=element_text(size=18),
        text=element_text(size=16),
        legend.text=element_text(size=22),
        legend.position='bottom')

ggsave("images/speaker_plot_slopes3.png", speaker_ind3, scale=1,
       width=4000, height=5000, units="px")

#########################
pop_level <- c("b_PositionInitial", "b_PositionMedial", "b_PositionFinal", 
               "b_Registerinformal", "b_z_uttLength")

#########################################
###   Tables: Raw values              ###
#########################################
fixed_effects <- para_vals %>% filter(Parameter %in% pop_level) %>% 
  mutate(Estimate=round(invlogit(Estimate), 2),
         Parameter=str_replace(Parameter, "b_Position", ""),
         Parameter=str_replace(Parameter, "b_z_uttLength", "Length of Utterance"),
         Parameter=str_replace(Parameter, "b_Registerinformal", "Informal Register"),
         "89% HPDI"=paste(format(round(invlogit(hpdi_89_low), 2), nsmall=2), "to", 
                              format(round(invlogit(hpdi_89_high), 2), nsmall=2))) %>%
  select(Parameter, Estimate, "89% HPDI") %>% 
  xtable(caption="89% HPDI of the population-level predictors holding all other variables constant",
         label="table: fixed_effects")
print(xtable(fixed_effects), include.rownames=FALSE)

group_results <- group_estimates %>% 
  mutate(Estimate=round(Estimate, 2),
         "89% HPDI"=paste(format(round(hpdi_89_low, 2), nsmall=2), "to", 
                            format(round(hpdi_89_high, 2), nsmall=2))) %>% 
  arrange(Parameter) %>% 
  select(Parameter, Group, Estimate, "89% HPDI") %>% 
  xtable(caption="Probability for discourse markers across groups and positions",
         label="table: slopes_group")
print(xtable(group_results), include.rownames=FALSE)

sd_comp <- para_vals %>% filter(str_detect(para_vals$Parameter, "sd_")) %>% 
  mutate(Parameter=str_replace(Parameter, "sd_(.*)__Position(.*)", "\\1_\\2"),
         Parameter=str_replace(Parameter, "Group:", "")) %>% 
  separate(sep="_", col=Parameter, into=c("Variable", "Position")) %>% 
  mutate("89% HPDI"=paste(format(round(hpdi_89_low, 2), nsmall=2), "to", 
                       format(round(hpdi_89_high, 2), nsmall=2)),
         Estimate=format(round(Estimate, 2), nsmall=2)) %>% 
  select(Variable, Position, Estimate, "89% HPDI") %>% 
  pivot_wider(names_from=Position, values_from=c(Estimate, "89% HPDI")) %>% 
  xtable(caption="Standard deviation for all positions on inverse logit-scale",
         label="table: sd_vars")
print(xtable(sd_comp), include.rownames=FALSE)

#########################################
###    Overall areas                  ###
#########################################
overall_areas <- mcmc_areas(model_grouped, regex_pars=c("^b_"),
                            prob=0.89, prob_outer=0.997, point_est="mean",
                            transformations=invlogit) +
  scale_x_continuous(name="Population-level effects on inverse logit-scale") +
  scale_y_discrete(labels=c("Position: Final", "Position: Initial", 
                              "Position: Medial", "Utterance Length",
                              "Informal Register")) + 
  theme_bw()

ggsave('images/viz_overall.png', overall_areas, scale=1,
       width=2000, height=1200, units="px")
