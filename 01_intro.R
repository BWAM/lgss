## install.packages("tidyverse", "devtools", "plyr", "DT")
suppressPackageStartupMessages(
  library(tidyverse)
)
## devtools::install_github("zsmith27/mmir", ref = "dev", force = TRUE, quiet = TRUE)
## devtools::install_github("zsmith27/toolbox", force = TRUE, quiet = TRUE)
library(mmir)
root.dir <- rprojroot::find_root("lgss.Rproj")
