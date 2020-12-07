## ------------------------------------------------------------------------
condition.df <- file.path(root.dir, 
                          "data",
                          "LG_ALL_site_subsetting.csv") %>% 
  # readxl::read_excel(sheet = "FieldData") %>% 
  data.table::fread() %>% 
  rename_all(tolower) %>% 
  select(bas_loc_rm, site_con_1, lg_region) %>% 
  rename(site_condition = site_con_1) 

## ------------------------------------------------------------------------
metrics.long <- tidyr::gather(metrics.wide, metric, value, -bas_loc_rm)  %>% 
  left_join(condition.df, by = "bas_loc_rm")

DT::datatable(head(metrics.long, 500), options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
sensitivity.df <- sensitivity(metrics.long,
                              metric.col = metric,
                              value.col = value,
                              condition.col = site_condition,
                              ref.cond = "ref",
                              deg.cond = "test")

DT::datatable(sensitivity.df, options = list(scrollX = TRUE))

## ------------------------------------------------------------------------
# remove.vec <- c("rich_pterygota", "pct_rich_pterygota", "rich_neoptera",
#                 "rich_holometabola", "pct_rich_eumalacostraca",
#                 "pct_rich_peracarida")
# 
# sensitivity.df  <- sensitivity.df %>% 
#   filter(!metric %in% remove.vec)

## ---- fig.width=8, fig.height=30-----------------------------------------
sensitivity.df %>% 
  filter(barbour == 3) %>% 
  inner_join(metrics.long, by = "metric") %>% 
  mutate(metric = factor(metric, levels = unique(metric))) %>% 
  ggplot(aes(site_condition, value)) +
  geom_boxplot() +
  facet_wrap(~metric, ncol = 4, scales = "free")

## ---- fig.width=10-------------------------------------------------------
sensitivity.df %>% 
  arrange(desc(de)) %>% 
  slice(1:10) %>% 
  inner_join(metrics.long, by = "metric") %>% 
  mutate(metric = factor(metric, levels = unique(metric))) %>% 
  ggplot(aes(site_condition, value)) +
  geom_boxplot() +
  facet_wrap(~metric, ncol = 4, scale = "free")

## ---- fig.width= 10------------------------------------------------------
sensitivity.df %>% 
  arrange(desc(bde)) %>% 
  slice(1:10) %>% 
  inner_join(metrics.long, by = "metric") %>% 
  mutate(metric = factor(metric, levels = unique(metric))) %>% 
  arrange(desc(bde)) %>% 
  ggplot(aes(site_condition, value)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = bde_thresh)) +
  facet_wrap(~metric, ncol = 5, scale = "free")

## ------------------------------------------------------------------------
sensitivity.df %>% 
  filter(metric == "pct_rich_ept_genus") %>% 
  inner_join(metrics.long, by = "metric") %>% 
  arrange(desc(bde)) %>% 
  ggplot(aes(site_condition, value)) +
  geom_jitter()+
  geom_boxplot() +
  geom_hline(aes(yintercept = bde_thresh)) 

## ------------------------------------------------------------------------
sensitivity.df %>% 
  filter(metric == "pct_ept") %>% 
  inner_join(metrics.long, by = "metric") %>% 
  filter(site_condition != "ot") %>% 
  arrange(desc(bde)) %>% 
  ggplot(aes(site_condition, value)) +
  geom_jitter()+
  geom_boxplot() +
  geom_hline(aes(yintercept = bde_thresh)) 

