## ------------------------------------------------------------------------
metrics.key <- taxa.df %>% 
  select(bas_loc_rm) %>% 
  distinct()

## ------------------------------------------------------------------------
hier.vec <-  c("class", "order", "family", "genus", "macro_genspecies")
# taxa.i <- "order"
rich.df <- purrr::map(hier.vec, function(taxa.i) {
  quo.taxa.i <- rlang::quo(taxa.i)
  rich.sub <- dplyr::tibble(
    rich = taxa_rich(long.df = taxa.df,
                           unique.id.col = bas_loc_rm,
                           low.taxa.col = !!quo.taxa.i,
                           taxon = NULL)
  )
  names(rich.sub) <- paste(names(rich.sub), taxa.i, sep = "_")
  return(rich.sub)
  }) %>% 
  bind_cols(metrics.key, .)

DT::datatable(rich.df, options = list(scrollX = TRUE))  

## ------------------------------------------------------------------------
subrich.df <- purrr::map(c("family", "genus", "macro_genspecies"), function(taxa.i) {
  quo.taxa.i <- rlang::enquo(taxa.i)
  subrich.sub <- dplyr::tibble(
    rich_ep = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("ephemeroptera",
                                       "plecoptera")),
    rich_et = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("ephemeroptera",
                                       "trichoptera")),
    rich_pt = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("plecoptera",
                                       "trichoptera")),
    rich_ept = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("ephemeroptera",
                                       "plecoptera",
                                       "trichoptera")),
    rich_toe = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("trichoptera",
                                       "odonata",
                                       "ephemeroptera")),
    rich_cote = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("coleoptera",
                                       "odonata",
                                       "trichoptera",
                                       "ephemeroptera")),
    rich_potec = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("plecoptera",
                                       "odonata",
                                       "trichoptera", 
                                        "ephemeroptera",
                                       "coleoptera")),
    rich_mol = taxa_rich(taxa.df, bas_loc_rm, phylum, !!quo.taxa.i,
                             taxon = c("mollusca")),
    rich_amphi = taxa_rich(taxa.df, bas_loc_rm, order, !!quo.taxa.i,
                             taxon = c("amphipoda")),
    rich_crust = taxa_rich(taxa.df, bas_loc_rm, subphylum, !!quo.taxa.i,
                             taxon = c("crustacea")),
    rich_mollusca_amphipoda = rich_mol + rich_amphi,
    rich_crustacea_mollusca = rich_crust + rich_mol
  ) %>% 
    select(-rich_mol, -rich_amphi, -rich_crust)
  names(subrich.sub) <- paste(names(subrich.sub), taxa.i, sep = "_")
  return(subrich.sub)
  }) %>% 
  bind_cols(metrics.key, .)

DT::datatable(subrich.df, options = list(scrollX = TRUE))  

## ------------------------------------------------------------------------
pct.rich.df <- purrr::map(hier.vec, function(taxa.i) {
  quo.taxa.i <- rlang::enquo(taxa.i)
  pct.rich.sub <- dplyr::tibble(
    pct_rich_ep = taxa_pct_rich(taxa.df,
                                bas_loc_rm,
                                order,
                                !!quo.taxa.i,
                                taxon = c("ephemeroptera", "plecoptera")),
    pct_rich_et = taxa_pct_rich(taxa.df,
                                bas_loc_rm,
                                order,
                                !!quo.taxa.i,
                                taxon = c("ephemeroptera", "trichoptera")),
    pct_rich_pt = taxa_pct_rich(taxa.df,
                                bas_loc_rm,
                                order,
                                !!quo.taxa.i,
                                taxon = c("plecoptera", "trichoptera")),
    pct_rich_ept = taxa_pct_rich(taxa.df,
                                 bas_loc_rm,
                                 order,
                                 !!quo.taxa.i,
                                 taxon = c("ephemeroptera", "plecoptera", "trichoptera")),
    pct_rich_cote = taxa_pct_rich(taxa.df,
                                  bas_loc_rm,
                                  order,
                                  !!quo.taxa.i,
                                  taxon = c("coleoptera", "odonata",
                                            "trichoptera", "ephemeroptera")),
    pct_rich_potec = taxa_pct_rich(taxa.df,
                                   bas_loc_rm,
                                   order, 
                                   !!quo.taxa.i,
                                   taxon = c("plecoptera","odonata", "trichoptera", 
                                             "ephemeroptera", "coleoptera")),
    pct_rich_mol = taxa_pct_rich(taxa.df,
                                   bas_loc_rm,
                                   phylum, 
                                   !!quo.taxa.i,
                                   taxon = "mollusca"),
    pct_rich_amphi = taxa_pct_rich(taxa.df,
                                   bas_loc_rm,
                                   order, 
                                   !!quo.taxa.i,
                                   taxon = "amphipoda"),
    pct_rich_crust = taxa_pct_rich(taxa.df,
                                   bas_loc_rm,
                                   subphylum, 
                                   !!quo.taxa.i,
                                   taxon = "crustacea"),
    pct_rich_mollusca_amphipoda = pct_rich_mol + pct_rich_amphi,
    pct_rich_crustacea_mollusca = pct_rich_crust + pct_rich_mol
  )
  names(pct.rich.sub) <- paste(names(pct.rich.sub), taxa.i, sep = "_")
  return(pct.rich.sub)
  }) %>% 
  bind_cols(metrics.key, .)

DT::datatable(pct.rich.df, options = list(scrollX = TRUE))  

## ------------------------------------------------------------------------
div.df <- purrr::map(c("order", "family", "genus", "macro_genspecies"), function(taxa.i) {
  quo.taxa.i <- rlang::quo(!!rlang::sym(taxa.i))
  purrr::map(c("shannon", "simpson", "margalef", "menhinick", "pielou"), function(job.i) {
    div.sub <- dplyr::tibble(
    temp = taxa_div(long.df = taxa.df,
                       unique.id.col = bas_loc_rm,
                       count.col = count,
                       high.taxa.col = !!quo.taxa.i,
                       job = job.i,
                       low.taxa.col = NULL,
                       taxon = NULL,
                       base.log = 2)
    )
    
    names(div.sub) <- paste(job.i, taxa.i, sep = "_")
    return(div.sub)
  }) %>% 
    dplyr::bind_cols()

  }) %>% 
  bind_cols(metrics.key, .)

DT::datatable(div.df, options = list(scrollX = TRUE))  

## ------------------------------------------------------------------------
pct.df <- metrics.key %>% 
  dplyr::mutate(
    pct_ep = taxa_pct(long.df = taxa.df,
                      unique.id.col = bas_loc_rm,
                      count.col = count,
                      taxon.col = order,
                      taxon = c("ephemeroptera", "plecoptera")),
    pct_et = taxa_pct(long.df = taxa.df,
                      unique.id.col = bas_loc_rm,
                      count.col = count,
                      taxon.col = order,
                      taxon = c("ephemeroptera", "trichoptera")),
    pct_pt = taxa_pct(long.df = taxa.df,
                      unique.id.col = bas_loc_rm,
                      count.col = count,
                      taxon.col = order,
                      taxon = c("plecoptera", "trichoptera")),
    pct_ept = taxa_pct(long.df = taxa.df,
                       unique.id.col = bas_loc_rm,
                       count.col = count,
                       taxon.col = order,
                       taxon = c("ephemeroptera", "plecoptera", "trichoptera")),
    pct_toe = taxa_pct(long.df = taxa.df,
                        unique.id.col = bas_loc_rm,
                        count.col = count,
                        taxon.col = order,
                        taxon = c("trichoptera", "odonata", "ephemeroptera")),
    pct_cote = taxa_pct(long.df = taxa.df,
                        unique.id.col = bas_loc_rm,
                        count.col = count,
                        taxon.col = order,
                        taxon = c("coleoptera", "odonata", "trichoptera", "ephemeroptera")),
    pct_potec = taxa_pct(long.df = taxa.df,
                         unique.id.col = bas_loc_rm,
                         count.col = count,
                         taxon.col = order,
                         taxon = c("plecoptera", "odonata",
                                   "trichoptera","ephemeroptera", "coleoptera")),
    pct_ephemeroptera_no_baetidae = taxa_pct(long.df = taxa.df,
                                             unique.id.col = bas_loc_rm,
                                             count.col = count,
                                             taxon.col = order,
                                             taxon = "ephemeroptera",
                                             exclusion.col = family,
                                             exclusion.vec = "baetidae"),
    pct_ept_no_baetidae = taxa_pct(long.df = taxa.df,
                       unique.id.col = bas_loc_rm,
                       count.col = count,
                       taxon.col = order,
                       taxon = c("ephemeroptera", "plecoptera", "trichoptera"),
                       exclusion.col = family,
                       exclusion.vec = "baetidae"),
    pct_diptera_no_tanytarsini = taxa_pct(long.df = taxa.df,
                                          unique.id.col = bas_loc_rm,
                                          count.col = count,
                                          taxon.col = order,
                                          taxon = "diptera",
                                          exclusion.col = tribe,
                                          exclusion.vec = "tanytarsini"),
    pct_chironomidae_no_tanytarsini = taxa_pct(long.df = taxa.df,
                                          unique.id.col = bas_loc_rm,
                                          count.col = count,
                                          taxon.col = family,
                                          taxon = "chironomidae",
                                          exclusion.col = tribe,
                                          exclusion.vec = "tanytarsini"),
    pct_mol = taxa_pct(long.df = taxa.df,
                       unique.id.col = bas_loc_rm,
                       count.col = count,
                       taxon.col = phylum,
                       taxon = "mollusca"),
    pct_amphi = taxa_pct(long.df = taxa.df,
                         unique.id.col = bas_loc_rm,
                         count.col = count,
                         taxon.col = order,
                         taxon = "amphipoda"),
    pct_mollusca_amphipoda = pct_mol + pct_amphi,
    pct_crust = taxa_pct(long.df = taxa.df,
                         unique.id.col = bas_loc_rm,
                         count.col = count,
                         taxon.col = subphylum,
                         taxon = "crustacea"),
    pct_crustacea_mollusca = pct_crust + pct_mol
  ) %>% 
  select(-pct_mol, - pct_amphi, - pct_crust)

DT::datatable(pct.df, options = list(scrollX = TRUE))  

## ------------------------------------------------------------------------
dom.df <- purrr::map(c("order", "family", "genus", "macro_genspecies"), function(taxa.i) {
  quo.taxa.i <- rlang::quo(!!rlang::sym(taxa.i))
  purrr::map(1:5, function(level.i) {
    dom.sub <- dplyr::tibble(
      dom = taxa_dom(long.df = taxa.df,
                     unique.id.col = bas_loc_rm, 
                     count.col = count, 
                     taxon.col = !!quo.taxa.i,
                     dom.level = level.i)
    )
    
    names(dom.sub) <- paste(paste0("pct_dom", level.i), taxa.i, sep = "_")
    return(dom.sub)
  }) %>% 
    dplyr::bind_cols()

  }) %>% 
  bind_cols(metrics.key, .)

DT::datatable(dom.df, options = list(scrollX = TRUE))  

## ------------------------------------------------------------------------
tol.df <- metrics.key %>% 
  dplyr::mutate(
    hbi_macrogenspecies = taxa_tol_index(long.df = taxa.df,
                            unique.id.col = bas_loc_rm,
                            count.col = count,
                            taxon.col = macro_genspecies,
                            tol.col = tol_int,
                            na.rm = TRUE)
  )

## ------------------------------------------------------------------------
# taxa.cols <- c("class", "subclass", "infraclass",
#                "superorder", "order", "suborder", "infraorder",
#                "superfamily", "family", "subfamily",
#                "tribe", "subtribe",
#                "genus", "subgenus",
#                "species", "subspecies",
#                "tol_char", "ffg")
taxa.cols <- c("class", 
               "order",
               "family", 
               "genus", 
               "species", 
               "tol_char",
               "ffg")

seq.df <- purrr::map(c("rich", "pct_rich", "pct"), function(job.i) {
  # quo.taxa.i <- rlang::quo(!!rlang::sym(taxa.i)) 
  print(job.i)
  seq.sub <- taxa_seq(long.df = taxa.df,
             unique.id.col = bas_loc_rm,
             count.col = count,
             taxa.cols = taxa.cols,
             high.taxa.col = "macro_genspecies",
             job = job.i)
  return(seq.sub)
  }) %>% 
  bind_cols(metrics.key, .)

## ------------------------------------------------------------------------
metrics.wide <- list(rich.df,
                     subrich.df,
                     pct.rich.df,
                     div.df, 
                     pct.df,
                     dom.df,
                     tol.df,
                     seq.df) %>% 
  plyr::join_all(by = c("bas_loc_rm"))

