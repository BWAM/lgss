## ------------------------------------------------------------------------
sens.adk.df <- metrics.long %>% 
  filter(lg_region == "Adirondacks") %>% 
  sensitivity(metric.col = metric,
              value.col = value,
              condition.col = site_condition,
              ref.cond = "ref",
              deg.cond = "test")

