## ------------------------------------------------------------------------
sens.metrics.vec <- c("rich_toe_family",
                      "rich_ept_macro_genspecies",
                      "rich_genus", 
                      "rich_insecta",
                      "pct_ept",
                      "pct_gammaridae",
                      "pct_physidae")

## ------------------------------------------------------------------------
sub.df <- metrics.long %>% 
  filter(metric %in% sens.metrics.vec) %>% 
  group_by(metric) %>% 
  mutate(min = min(value),
         max = max(value),
         score = (value - min) / (max - min) * 100) %>% 
  ungroup() %>% 
  group_by(bas_loc_rm, site_condition, lg_region) %>% 
  summarise(index_score = mean(score))


## ------------------------------------------------------------------------
sub.df %>% 
  filter(metric == "pct_physidae") %>% 
  ggplot(aes(site_condition, score)) +
  geom_boxplot()

## ------------------------------------------------------------------------
sub.df %>% 
  ggplot(aes(lg_region, index_score)) +
  geom_boxplot()

