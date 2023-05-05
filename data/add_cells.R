# Add experimental design

dat_adhd %>% group_by(cell) %>%
  summarise(A1 = unique(A1), R = unique(R), A2 = unique(A2), n = n(), mean_Y = mean(Y2))

# Huge imbalance... is this a problem???
dat_adhd %>% filter(R==0) %>% group_by(A1) %>%
  summarise(mean(A2))
