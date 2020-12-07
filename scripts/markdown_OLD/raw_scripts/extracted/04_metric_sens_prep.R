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

