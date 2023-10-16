######## pacman initialization ########
library(pacman)
p_load(sf, terra, dplyr, purrr, tidytable, data.table, stringi)
sf_use_s2(F)


## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 4
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/",
        ifelse(COMPUTE_MODE == 4,
          "./input/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )

path_output <- "./output/"

## AZO
azo <- sf::read_sf(paste0(path_base, "data_process/data_AZO_watershed_huc_join.shp"))
azo_s <- azo %>%
  dplyr::select(site_no, Year) %>%
  unique() %>%
  st_transform("EPSG:4326")



nass_yearly <-
  { path_output %s% "NASS/" } %>%
  list.files(path = ., pattern = "csv$", full.names = TRUE) %>%
  split(., rep(2008:2022, each = 3)) %>%
  lapply(function(x) {
    lapply(x, data.table::fread) %>%
    data.table::rbindlist(., use.names = TRUE, fill = TRUE)
  }) %>%
  mapply(function(x, y) {
    x %>% mutate(year = y)
  }, ., seq(2008, 2022) %>% split(.,.), SIMPLIFY = FALSE) %>%
  data.table::rbindlist(., use.names = TRUE, fill = TRUE)

nass_yearly_k <- copy(nass_yearly)
names(nass_yearly)
# columns with trailing whitespace (actually duplicated) fix
nass_yearly_k <- nass_yearly_k %>%
  mutate(
    Pears = ifelse(is.na(Pears) & !is.na(`Pears `), `Pears `, Pears),
    `Perennial Ice/Snow` = ifelse(is.na(`Perennial Ice/Snow`) & !is.na(`Perennial Ice/Snow `), `Perennial Ice/Snow `, `Perennial Ice/Snow`)) %>%
  tidytable::select(-layer, -zone, -`Perennial Ice/Snow `, -`Pears `)

nass_names <- names(nass_yearly_k)
nass_names <- gsub("/", "_", nass_names)
nass_names <- gsub("[[:punct:]]", "_", nass_names)
nass_names <- trimws(nass_names)
nass_names <- gsub("(Dbl Crop )", "dbl_", nass_names)
nass_names <- gsub(" ", "", nass_names)
nass_names <- tolower(nass_names)
hucyear_index <- grep("(huc|year)", nass_names)
(nass_names)

colnames(nass_yearly_k) <- nass_names

nass_yearly_clean <- nass_yearly_k %>%
  tidytable::select(huc, year, 2:38, 40:119, 121:ncol(.)) %>%
  tidytable::mutate(across(everything(), ~ifelse(is.na(.), 0L, .)))

nass_yearly_clean_val <- nass_yearly_clean[, -1:-2] %>%
  as.matrix() %>%
  {. / rowSums(.) }

nass_yearly_clean[, 3:129] <- data.table(nass_yearly_clean_val)
nass_yearly_clean <- nass_yearly_clean %>%
  mutate(huclevel = cut(nchar(as.character(huc)),
    c(6, 8, 10, 12), 
    labels = c("HUC08", "HUC10", "HUC12")))
nass_yearly_cleanl <- nass_yearly_clean %>%
  split(., unlist(.$huclevel))

nass_huc08 <- nass_yearly_cleanl$HUC08
nass_huc10 <- nass_yearly_cleanl$HUC10
nass_huc12 <- nass_yearly_cleanl$HUC12


## code sort: HUC02
nass_huc08 <- nass_yearly_cleanl$HUC08 |>
  mutate(
    huce = sprintf("%08d", as.integer(huc)),
         huce02 = stri_sub(huce, 1, 2))
nass_huc10 <- nass_yearly_cleanl$HUC10 |>
  mutate(huce = sprintf("%010d", as.integer(huc)),
         huce02 = stri_sub(huce, 1, 2))
nass_huc12 <- nass_yearly_cleanl$HUC12 |>
  mutate(huce02 = ifelse(stri_length(huc) == 11, stri_sub(huc, 1, 1), stri_sub(huc, 1, 2))) |>
  mutate(huce02 = sprintf("%02d", as.integer(huce02)))


## gg
gen_temporal_consistency <- function(dat, split = "huce02", pick = 100L) {
    library(ggplot2)
    dat_long <- dat %>%
        pivot_longer(cols = seq(3, ncol(.) - 3))
    ggplot(data = dat_long |> as_tibble(),
           mapping = aes(x = year, y = value, group = interaction(name, huc))) +
        facet_wrap(~huce02) +
        #geom_line(linewidth = 0.1, col = 'grey', alpha = 0.2) +
        stat_summary(mapping = aes(x = year, y = value, group = name), fun = 'median', geom = 'line', col = 'red', linewidth = 0.66) +
        theme_minimal() +
        theme(
            text = element_text(size = 14)
        )
}

nass_huc08 %>%
    # filter(huce02 == "02") %>%
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
    gen_temporal_consistency(., pick = nrow(.))

nass_huc10 %>%
    # filter(huce02 == "15") %>%
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
    gen_temporal_consistency(., pick = nrow(.))

nass_huc12 %>%
    # filter(huce02 == "15") %>%
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
    gen_temporal_consistency(., pick = nrow(.))


## rank proportions of each crop class
nass_huc08 %>%
    pivot_longer(cols = seq(3, ncol(.) - 3)) %>%
    group_by(huce02, name, year) %>%
    summarize(p_median = median(value)) %>%
    ungroup() %>%
    pivot_wider(values_from = p_median, names_from = year) %>%
    filter(huce02 == "10") %>%
    arrange(-`2010`)

nass_huc08 %>%
    pivot_longer(cols = seq(3, ncol(.) - 3)) %>%
    group_by(huce02, name, year) %>%
    summarize(p_median = median(value)) %>%
    ungroup() %>%
    pivot_wider(values_from = p_median, names_from = year) %>%
    filter(huce02 == "06") %>%
    arrange(-`2018`) %>%
    select(1:2, 12:ncol(.))

nass_huc08 %>%
    tidytable::mutate(tidytable::across(3:(ncol(.) - 3), ~ifelse(is.na(.), 0, .))) %>%
    pivot_longer(cols = seq(3, ncol(.) - 3)) %>%
    group_by(huce02, name, year) %>%
    summarize(p_median = median(value)) %>%
    ungroup() %>%
    pivot_wider(values_from = p_median, names_from = year) %>%
    tidytable::filter(huce02 == "14") %>%
    tidytable::arrange(-`2015`) %>%
    tidytable::select(1:2, 8:ncol(.))



# conclusion: calculate spatially representative fractions
# (remove temporal info)



