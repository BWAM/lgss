## ------------------------------------------------------------------------
sw.sens.df <- metrics.long %>% 
  sensitivity(
    metric.col = metric,
    value.col = value,
    condition.col = site_condition,
    ref.cond = "ref",
    deg.cond = "test")

DT::datatable(sw.sens.df, options = list(scrollX = TRUE))

## ---- fig.width=10, fig.height=4-----------------------------------------
metrics.vec <- c("pct_rich_mollusca_amphipoda_macro_genspecies",
                    "rich_insecta",
                    "pct_rich_cote_family",
                    "rich_intolerant",
                    "pct_rich_amphi_macro_genspecies",
                    "pct_ept")
sw.sens.df %>% 
  filter(metric %in% metrics.vec) %>% 
  inner_join(metrics.long, by = "metric") %>% 
  mutate(metric = factor(metric, levels = unique(metric))) %>% 
  ggplot(aes(site_condition, value)) +
  geom_boxplot() +
  facet_wrap(~metric, ncol = 3, scales = "free")

## ------------------------------------------------------------------------
sens.metrics.vec <- sw.sens.df %>% 
  filter(de >= 70) %>% 
  pull(metric)

sw.metrics.vec <- c("pct_rich_mollusca_amphipoda_macro_genspecies",
                    "rich_insecta",
                    "pct_rich_cote_family",
                    "rich_intolerant",
                    "pct_rich_amphi_macro_genspecies",
                    "pct_ept")

## ------------------------------------------------------------------------
ref.df <- metrics.long %>% 
  filter(metric %in% sw.metrics.vec,
         site_condition == "ref") %>% 
  group_by(metric) %>% 
  summarize(ref_q00 = quantile(value, 0.00),
            ref_q05 = quantile(value, 0.05),
            ref_q25 = quantile(value, 0.25),
            ref_q50 = quantile(value, 0.50),
            ref_q75 = quantile(value, 0.75),
            ref_q95 = quantile(value, 0.95),
            ref_q100 = quantile(value, 1.00))

test.df <- metrics.long %>% 
  filter(metric %in% sw.metrics.vec,
         site_condition == "test") %>% 
  group_by(metric) %>% 
  summarize(test_q00 = quantile(value, 0.00),
            test_q05 = quantile(value, 0.05),
            test_q25 = quantile(value, 0.25),
            test_q50 = quantile(value, 0.50),
            test_q75 = quantile(value, 0.75),
            test_q95 = quantile(value, 0.95),
            test_q100 = quantile(value, 1.00))

## ------------------------------------------------------------------------
sub.df <- metrics.long %>% 
  filter(metric %in% sw.metrics.vec) %>% 
  left_join(ref.df, by = "metric") %>% 
  left_join(test.df, by = "metric") %>% 
  mutate(disturbance = case_when(
    ref_q50 > test_q50 ~ "inc",
    ref_q50 < test_q50 ~ "dec",
    ref_q50 == test_q50  ~ "equ",
    TRUE ~ "ERROR"
  ))

test <- score_metrics(metrics.df = sub.df,
                      scoring.method = "all_gradient_5_95",
                      sensitivity.df = data.frame(sensitivity.df),
                      sensitivity.colname = "de",
                      sensitivity.threshold = 0.7,
                      first.metric = "pct_ept",
                      condition.colname = "site_condition",
                      ref.cond = "ref")

## ------------------------------------------------------------------------
sub.df %>% 
  ggplot(aes(site_condition, index_score)) +
  geom_boxplot() 

## ------------------------------------------------------------------------
sub.df %>% 
  # filter(site_condition == "ref") %>% 
  ggplot(aes(lg_region, index_score, fill = site_condition)) +
  geom_boxplot()

## ------------------------------------------------------------------------
sub.df <- metrics.long %>% 
  filter(metric %in% sw.metrics.vec) %>% 
  group_by(metric) %>% 
  mutate(min = min(value),
         max = max(value),
         score = (value - min) / (max - min) * 100) %>% 
  select(-min, -max, -value) %>% 
  spread(metric, score) #%>% 
  # filter(lg_region == "Adirondacks")

## ------------------------------------------------------------------------
selected.df <- sub.df %>% 
  left_join(condition.df, by = c("bas_loc_rm", "site_condition", "lg_region")) %>% 
  select(site_condition, sw.metrics.vec) %>% 
  mutate(site_condition = if_else(site_condition == "ref", 1, 0))


logistic <- glm(site_condition ~ ., data = selected.df, family = "binomial")

selected.df$predicted <- predict(logistic, newdata = selected.df, type="response")

coef.df <- summary(logistic)$coefficients %>% 
  as_tibble() %>% 
  mutate(metric = rownames(summary(logistic)$coefficients)) %>% 
  filter(`Pr(>|z|)` < 0.1,
         metric != "(Intercept)") %>% 
  pull(metric)


selected.df <- sub.df  %>% 
  left_join(condition.df, by = c("bas_loc_rm", "site_condition", "lg_region")) %>% 
  select(site_condition, coef.df) %>% 
  mutate(site_condition = if_else(site_condition == "ref", 1, 0))


logistic <- glm(site_condition ~ ., data = selected.df, family = "binomial")

selected.df$predicted <- predict(logistic, newdata = selected.df, type="response")


## ---- fig.width=10, fig.height=4-----------------------------------------
sw.sens.df %>% 
  filter(metric %in% coef.df) %>% 
  inner_join(metrics.long, by = "metric") %>% 
  mutate(metric = factor(metric, levels = unique(metric))) %>% 
  ggplot(aes(site_condition, value)) +
  geom_boxplot() +
  facet_wrap(~metric, ncol = 3, scales = "free")

## ---- fig.width=10, fig.height=4-----------------------------------------
sw.sens.df %>% 
  filter(metric %in% coef.df) %>% 
  inner_join(metrics.long, by = "metric") %>% 
  mutate(metric = factor(metric, levels = unique(metric))) %>% 
  ggplot(aes(site_condition, value, fill = lg_region)) +
  geom_boxplot() +
  facet_wrap(~metric, ncol = 3, scales = "free")

## ------------------------------------------------------------------------
tp <- nrow(selected.df[selected.df$site_condition == 1 & selected.df$predicted >= 0.5, ])
ref.n <- nrow(selected.df[selected.df$site_condition == 1, ])
tp.pct <- tp / ref.n * 100

fp <- nrow(selected.df[selected.df$site_condition == 0 & selected.df$predicted < 0.5, ])
test.n <- nrow(selected.df[selected.df$site_condition == 0, ])
fp.pct <- fp / test.n * 100

(tp.pct + fp.pct) / 2

## ------------------------------------------------------------------------
ggplot(selected.df, aes(x=predicted, y=site_condition)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) 

## ------------------------------------------------------------------------
ggplot(selected.df, aes(as.factor(site_condition), predicted)) +
  geom_boxplot() +
  geom_jitter()

