## Pesticide Analysis for PrestoGP 
# We are using the targets package to develop a pipeline for the analysis of pesticide data.

# First, we load the targets package and the tidyverse package.
# options(tidyverse.quiet = TRUE)
readin <- function(path = "../../../../group/set/Projects/PrestoGP_Pesticides") {
  fst::read_fst(file.path(path, "output/Covariates_Calculated/data_AZO_covariates_zerofill.fst"))
}

subsetting <- function(data) {
  ChmclNm <- NULL
  data %>%
    dplyr::filter(ChmclNm == "Simazine") %>%
    .[, c(22, 23, 3, 4, 5, 10, 25:957)]
}

preppresto <- function(data) {
  dlocs <- data[, 1:3]
  dlocs$Year <- dlocs$Year - 1999
  dlocs[, 1:2] <- dlocs[, 1:2] / 1e6
  dx <- nona(data[, 5:ncol(data)]) %>%
    dplyr::select_if(is.numeric)
  list(
    locs = as.matrix(dlocs),
    Y = as.matrix(data$cncntrt),
    X = as.matrix(dx)
  )
}

nona <- function(data) {
  noncomplete <- sapply(data, function(x) sum(is.na(x)))
  complete <- noncomplete == 0
  complete <- seq_len(ncol(data))[complete]
  datac <- data[, complete]
  datacu <- sapply(datac, function(x) length(unique(x)) >= ceiling(0.6 * length(x)))
  datacu <- datac[, seq_along(datacu)[datacu]]
  return(datacu)
}


init_presto <- function(modeltype = PrestoGP::VecchiaModel(), dat) {

  fit <- PrestoGP::prestogp_fit(
    model = modeltype,
    locs = dat$locs,
    y = dat$Y,
    x = dat$X,
    parallel = FALSE
  )
  fit
}
