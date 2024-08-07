## ------------------------------------------------------------------------
sens.metrics.vec <- sensitivity.df %>% 
  filter(de >= 75) %>% 
  pull(metric)

sens.metrics.vec <- c("pct_rich_toe_family",
                      "pct_rich_ept_macro_genspecies",
                      "pct_rich_genus", 
                      "pct_rich_insecta",
                      "pct_ept",
                      "pct_gammaridae",
                      "pct_physidae")

## ------------------------------------------------------------------------
spearman.df <- metrics.wide %>% 
  select(sens.metrics.vec) %>% 
  cor(method = "spearman") %>% 
  broom::fix_data_frame() %>% 
  gather(metric_y, spearman, -term) %>% 
  rename(metric_x = term) %>% 
  distinct() %>% 
  filter(metric_x != metric_y) %>% 
  mutate(abs_spearman = abs(spearman)) %>% 
  filter(abs_spearman >= 0.7)

## ------------------------------------------------------------------------
sens.sub <- sensitivity.df %>% 
  select(metric, de) %>% 
  filter(metric %in% sens.metrics.vec)

## ------------------------------------------------------------------------
spearman.df <- left_join(spearman.df, sens.sub, by = c("metric_x" = "metric")) %>% 
  rename(de_x = de) %>% 
  left_join(sens.sub, by = c("metric_y" = "metric")) %>% 
  rename(de_y = de)

## ------------------------------------------------------------------------
survival.df <- spearman.df %>% 
  mutate(weakest = case_when(
    de_x < de_y ~ metric_x,
    de_x > de_y ~ metric_y,
    de_x == de_y ~ metric_x,
    TRUE ~ "ERROR"
  ),
  fittest = case_when(
    de_x > de_y ~ metric_x,
    de_x < de_y ~ metric_y,
    de_x == de_y ~ metric_y,
    TRUE ~ "ERROR"
  )) 

test <- unique(survival.df$weakest[!survival.df$weakest %in% survival.df$fittest])
test <- unique(survival.df$fittest[!survival.df$fittest %in% survival.df$weakest])

## ------------------------------------------------------------------------
keep.vec <- spearman.df %>% 
  filter(abs(spearman) < 0.70) %>% 
  pull(metric_x) %>% 
  unique()

## ------------------------------------------------------------------------
test <- purrr::map(unique(spearman.df$metric_x), function(metric.i) {
  sub.df <- spearman.df %>% 
    filter(metric_x == metric.i) 
  
  if(any(sub.df$abs_spearman >= 0.7) ) {
    NA
  } else {
    metric.i
  }
})

