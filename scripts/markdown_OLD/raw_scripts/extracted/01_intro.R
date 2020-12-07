## ---- eval=FALSE---------------------------------------------------------
## install.packages("tidyverse", "devtools", "plyr", "DT")

## ---- messages=FALSE, warning=FALSE--------------------------------------
suppressPackageStartupMessages(
  library(tidyverse)
)

## ---- eval=FALSE---------------------------------------------------------
## devtools::install_github("zsmith27/mmir", ref = "dev", force = TRUE, quiet = TRUE)
## devtools::install_github("zsmith27/toolbox", force = TRUE, quiet = TRUE)

## ------------------------------------------------------------------------
library(mmir)

## ------------------------------------------------------------------------
root.dir <- rprojroot::find_root("lgss.Rproj")

