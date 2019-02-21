# Extract -----------------------------------------------------------------
sections.path <- "markdown/sections"
extracted.path <- "markdown/raw_scripts/extracted"

extract_code <- function(rmd.path, extracted.path) {
  r.files.vec <- list.files(rmd.path)
  r.files.vec <- r.files.vec[grepl(".Rmd", r.files.vec)]
  
  purrr::map(r.files.vec, function(file.i) {
    file.name <- gsub(".Rmd", "", file.i)
    extracted.file <- paste0(file.name, ".R")
    knitr::purl(file.path(rmd.path, file.i),
                file.path(extracted.path, extracted.file))
  })
  
}
extract_code(sections.path, extracted.path)

# Run ---------------------------------------------------------------------
extracted.path <- c("markdown/raw_scripts/extracted")
source.vec <- c(
  "01_intro.R",
  "02_taxa_prep.R",
  "03_metric_calc.R",
  "04_metric_sens_prep.R"
  # "metric_sensitivity.R"
)

purrr::map(source.vec, function(source.i) {
  source(file.path(extracted.path, source.i))
})
