## ------------------------------------------------------------------------
library(tidyverse)
library(readxl)

## ------------------------------------------------------------------------
root.dir <- rprojroot::find_root("lgss.Rproj")

## ------------------------------------------------------------------------
landcover.path <- file.path(root.dir,
                            "data",
                            "Batch_Land_Cover_Output_12212018.xls")

## ------------------------------------------------------------------------
sheet.vec <- readxl::excel_sheets(landcover.path)

## ------------------------------------------------------------------------
landcover.df <- purrr::map_df(sheet.vec, function(sheet.i) {
  readxl::read_xls(landcover.path,
                   sheet = sheet.i) %>% 
    mutate(site_id = sheet.i,
           site_id = str_replace(site_id, "Basin_", ""))
})

