library(tidyverse)
library(ggdist)
library(gghalves)
library(viridis)
library(xtable)

data <- read_csv('data/data_preprocessed_dis.csv')

data %>% filter(discourse==1) %>% group_by(tok) %>% count()

data %>% group_by(utt_length, Group) %>% count(hesitation) %>% filter(hesitation == 1) %>% 
  ggplot(aes(x = utt_length, y = n, color = Group)) +
    geom_point() +
    scale_color_viridis(discrete = TRUE, end = 0.7)

data %>% ggplot(aes(x = Group, y = utt_length, color = Group, fill = Group)) +
  geom_boxplot(width = .2, fill = "white", size = 1, outlier.shape = NA) +
  geom_half_point(side = "l", range_scale = .25, alpha = .5, size = 0.1) +
  stat_halfeye(adjust = 1, width = .5, color = NA, position = position_nudge(x = .15)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, end = 0.7) +
  scale_color_viridis(discrete = TRUE, end = 0.7)

## dis
data_grouped_dis <- data %>% group_by(Group, Position, Register, discourse) %>% count() %>% 
  pivot_wider(names_from = Position, values_from = n) %>% 
  mutate(discourse = as.character(discourse),
         total = Initial + Medial + Final) %>% 
  arrange(discourse) %>% 
  relocate(discourse, Group, Register,Initial, Medial, Final, total)

count_dis <- data_grouped_dis %>% 
  filter(discourse == 0)
rel_count <- colSums(count_dis[, c(4:7)]) %>% as_tibble()


print(xtable(data_grouped_dis), include.rownames=FALSE)

## hes
data_grouped_hes <- data %>% group_by(Group, Position, hesitation) %>% count() %>% 
  mutate(hesitation = as.character(hesitation)) %>% 
  pivot_wider(names_from = Position, values_from = n)
  
print(xtable(data_grouped_hes), include.rownames=FALSE)
