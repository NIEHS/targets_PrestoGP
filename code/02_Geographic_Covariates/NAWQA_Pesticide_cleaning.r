######## pacman initialization ########
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(data.table, tidytable, tigris, sf, terra, ggplot2, ggrepel, skimr)
sf_use_s2(F)

Sys.getenv("TIGRIS_CACHE_DIR")
# tigris::tigris_cache_dir("__cache__dir__")
`%s%` <- function(x, y) paste0(x, y)

## data path determination
## compute_mode 1 (wine mount), compute_mode 2 (hpc compute node), 3 (container internal)
COMPUTE_MODE <- 1
path_base <-
  ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
    ifelse(COMPUTE_MODE == 2,
      "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/",
      ifelse(COMPUTE_MODE == 3,
        "/opt/",
        ifelse(COMPUTE_MODE == 4,
          "/tmp/AZO/",
          stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
        )
      )
    )
  )


## pesticides county data
download_txt <- function(years = seq(2000, 2012)) {
  targ_dir <- paste0(path_base, "NAWQA_Pesticides_County/")
  if (!dir.exists(targ_dir)) {
    dir.create(targ_dir)
  }
  for (year in years) {
    filename <- sprintf("EPest.county.estimates.%d.txt", year)
    filename_full <- paste0(targ_dir, filename)
    if (!file.exists(filename_full))
      url_front <- "https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/PesticideUseEstimates/"
      download.file(paste0(url_front, filename),
                    filename_full)
      Sys.sleep(1)
  }

}

# download_txt()

home_dir <- paste0(path_base, "NAWQA_Pesticides/")


states <- tigris::states()
statesdf <- states %>%
  select(3, 6, NAME) %>%
  st_drop_geometry()
statesdf <- statesdf %>% filter(!STUSPS %in% c("HI", "VI", "GU", "AK", "PR", "MP"))
# dim(statesdf)


pestdir <- home_dir
txts <- list.files(path = pestdir, pattern = "*.txt$", full.names = TRUE)
txts_20c <- grepl("(19)[0-9]+{2,2}", txts)
txts <- txts[!txts_20c]
txtsr <- gsub("\\.20", "_20", txts)
txtsr <- gsub("EPest\\.county\\.est", "EPest_county_est", txtsr)
txtsr <- order(txtsr)

pests <- lapply(txts[txtsr], fread)
pests <- lapply(
  pests,
  function(x) {
    x %>% mutate(GEOID = sprintf("%02d%03d", STATE_FIPS_CODE, COUNTY_FIPS_CODE))
  }
)

pests_df <- rbindlist(pests)
pests_df <- pests_df %>%
  arrange(GEOID, YEAR, COMPOUND) %>%
  select(GEOID, YEAR, COMPOUND, starts_with("EPEST"))
pests_df <- pests_df %>%
  filter(YEAR >= 2000) |>
  mutate(GEOID = plyr::mapvalues(GEOID, c("12025", "46113"), c("12086", "46102")))
# saveRDS(pests_df, paste0(home_dir, "county_pesticides_2000_2019.rds"))


## chemical information
length(unique(pests_df$COMPOUND))
chemlist <- readxl::read_excel("./input/ChemicalList_EPAOPPIN.xlsx")
chemlist <- chemlist %>% mutate(COMPOUND = toupper(`PREFERRED NAME`))

# naive literal join
pests_chem <- left_join(pests_df, chemlist %>% select(COMPOUND, SMILES))



# stringdist join
pests_chem_remain <- pests_chem %>%
  filter(is.na(SMILES)) %>%
  select(-SMILES)
write.csv(pests_chem_remain %>% select(COMPOUND) %>% unique(),
  "./input/NAWQA_nonjoined_compounds.csv", row.names = FALSE)
write.csv(pests_df %>% select(COMPOUND) %>% unique(),
  "./input/NAWQA_all_compounds.csv", row.names = FALSE)

pests_compound_names <- pests_df %>%
  select(COMPOUND) %>%
  unique() %>%
  mutate(compid = seq(1, nrow(.))) 


## Join based on string distance
pests_chem_f <- fuzzyjoin::stringdist_left_join(pests_chem_remain, chemlist %>% select(COMPOUND, SMILES))

# get nonjoined elements
pests_chem_f %>% filter(is.na(SMILES)) %>% select(COMPOUND.x) %>% unique
chemlist %>% filter(grepl("DICHLOROPHENOXY)ACETIC", COMPOUND)) %>% .[[2]]
chemlist %>% filter(grepl("MANCOZEB", COMPOUND)) %>% .[[2]]
chemlist %>% filter(grepl("CARBAMATE", COMPOUND)) %>% .[[2]]
chemlist %>% filter(grepl("INDOLE-", COMPOUND)) %>% .[[2]]
chemlist %>% filter(grepl("BUTYRIC", COMPOUND)) %>% .[[2]]



nrow(pests_chem_remain)
nrow(pests_chem_f)

pests_chem_f %>%
  group_by(COMPOUND)

# wide frame
pests_df_geoid <- pests_df %>%
  select(1:2) %>%
  unique() %>%
  mutate(valfill = TRUE) %>%
  pivot_wider(names_from = YEAR, values_from = valfill)

# Which counties are partially present during the study period?
pests_df_geoid_nas <- pests_df_geoid %>%
  filter(!complete.cases(.)) %>%
  pivot_longer(cols = 2:ncol(.)) %>%
  transmute(
    GEOID = GEOID,
    year = name,
    value = ifelse(is.na(value), FALSE, value)
  )
pests_df_geoid_nas

## Pesticide selection
## [Atr|Prop|Sim]azine
## KeyID starts with DTXCID... DSSTox Identifiers
## CASRN and InChI lookup (as of 2018)
sim_chem <- list.files(path = "./input/chemical_similarity/",
  pattern = "*PhysChem.csv$", full.names = TRUE)
sim_tox <- list.files(path = "./input/chemical_similarity/",
  pattern = "*ToxRef.csv$", full.names = TRUE)
azine_names <- c("atr", "prop", "sim")
sim_chem_df <- sim_chem %>%
  lapply(\(x) read.csv(x, header = FALSE)) %>%
  lapply(\(x) {
    xx <- x[-1,]
    names(xx) <- x[1,]
    xx <- t(xx)[,2:4]
    return(xx)
  }) %>%
  lapply(\(x) x %>% select(-ends_with("_units")))
sim_tox_df <- sim_tox %>%
  lapply(\(x) read.csv(x, header = FALSE)) %>%
  lapply(\(x) {
    xx <- x[-1,]
    names(xx) <- x[1,]
    xx <- t(xx)[,2:4]
    return(xx)
  }) %>%
  lapply(\(x) x %>% select(-ends_with("_units")))

# assign names
names(sim_chem_df) <- azine_names
names(sim_tox_df) <- azine_names
# dsstox_sid, cid
sim_chem_df_c <- rbindlist(sim_chem_df, idcol = "azine") %>%
  rename(compound = `3`,
         dsstox_sid = `4`,
         dsstox_cid = `5`) %>%
  filter(compound != "" & compound != "preferred name") %>%
  mutate(type = "chemical")
sim_tox_df_c <- rbindlist(sim_tox_df, idcol = "azine") %>%
  rename(compound = `3`,
         dsstox_sid = `4`,
         dsstox_cid = `5`) %>%
  filter(compound != "" & compound != "preferred name") %>%
  mutate(type = "toxic")

sim_chemtox_df_c <- rbind(sim_chem_df_c, sim_tox_df_c)
write.csv(sim_chemtox_df_c, "./input/GenRA_Azines_sim.csv", row.names = FALSE)

## strategy: similarity tables... find synonyms; use another synonym 
# to match USGS common names
dsstox_syn <- fread("./input/DSSTox_Synonyms.csv")
dsstox_syn <- dsstox_syn %>%
  filter(startsWith(`Synonym Type`, "Synonym")) %>%
  separate(Synonyms, sprintf("synonym_%d", 1:8), "|") %>%
  select(1,3,4,8:15) %>%
  pivot_longer(cols = 4:ncol(.)) %>%
  select(-name) %>%
  rename(synonyms = value) %>%
  filter(!is.na(synonyms))

## join all
sim_dsstox_synonym <-
  sim_chemtox_df_c %>%
  left_join(dsstox_syn, by = c("dsstox_sid" = "DTXSID")) %>%
  mutate(synonymscap = toupper(synonyms)) %>%
  left_join(pests_compound_names, by = c("synonymscap" = "COMPOUND")) %>%
  mutate(compoundcap = toupper(compound)) %>%
  left_join(pests_compound_names, by = c("compoundcap" = "COMPOUND"))
write.csv(sim_dsstox_synonym, "./input/sim_dsstox_synonym.csv", row.names = FALSE)

sim_chemtox_df_c %>%
  stringdist_left_join(dsstox_syn, by = c("compound" = "synonyms"))


## lookup table
dss_cas_lookup <- readxl::read_excel("./input/DSSTox_Identifiers_and_CASRN_with_InChIs_0927_2018.xlsx") %>%
  select(1:3)
## look at dsstox_substance_id


#### Missing data visualization ####
## Plots for the NA values
cnty00 <- tigris::counties(year = 2000, cb = TRUE)
cnty15 <- tigris::counties(year = 2015, cb = TRUE)

cnty00s <- cnty00 %>%
  dplyr::filter(STATE %in% statesdf$STATEFP) %>%
  dplyr::mutate(
    GEOID = paste0(STATEFP, COUNTYFP),
    GEOID_lab = ifelse(GEOID %in% unique(pests_df_geoid_nas$GEOID), GEOID, NA)
  ) %>%
  st_buffer(0)
cnty15s <- cnty15 %>%
  dplyr::filter(STATEFP %in% statesdf$STATEFP) %>%
  dplyr::mutate(
    GEOID = paste0(STATEFP, COUNTYFP),
    GEOID_lab = ifelse(GEOID %in% unique(pests_df_geoid_nas$GEOID), GEOID, NA)
  ) %>%
  st_buffer(0)
checkbox <- st_as_sfc(st_bbox(c(xmin = -130, ymin = 20, xmax = -60, ymax = 62), crs = st_crs(cnty15s)))
cnty15s <- cnty15s[checkbox, ]

g1 <-
  ggplot(
    data = pests_df_geoid_nas,
    mapping = aes(x = year, y = GEOID, fill = value)
  ) +
  geom_tile() +
  scale_fill_discrete(type = c("#E69F00", "#56B4E9")) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 1, angle = 30),
    legend.direction = "horizontal"
  ) +
  labs(fill = NULL)
g2 <-
  ggplot(data = cnty00s) +
  geom_sf(aes(fill = !is.na(GEOID_lab))) +
  # geom_sf_label(aes(label = GEOID_lab)) +
  ggrepel::geom_label_repel(
    aes(label = GEOID_lab, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 3,
    label.size = 0.2,
    segment.size = 0.125,
    box.padding = 0.05,
    label.padding = 0.1,
    point.padding = 0,
    max.iter = 1e5,
    force = 1
  ) +
  scale_fill_discrete(type = c("grey", "#D05050")) +
  # xlim(c(-130, -60)) +
  # ylim(c(20, 55)) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    text = element_text(size = 10)
  )
# g2

g3 <-
  ggplot(data = cnty15s) +
  geom_sf(aes(fill = !is.na(GEOID_lab))) +
  # geom_sf_label(aes(label = GEOID_lab)) +
  ggrepel::geom_label_repel(
    aes(label = GEOID_lab, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 3,
    label.size = 0.2,
    segment.size = 0.125,
    box.padding = 0.05,
    label.padding = 0.1,
    point.padding = 0,
    max.iter = 1e5,
    force = 1
  ) +
  scale_fill_discrete(type = c("grey", "#D05050")) +
  # xlim(c(-130, -60)) +
  # ylim(c(20, 55)) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    text = element_text(size = 10)
  )

# gridExtra::grid.arrange(g1, g2, g3, layout_matrix = matrix(c(1,2,2,1,3,3), byrow = T, nrow = 2))

gwrapped <-
  patchwork::wrap_plots(
    A = g1, B = g2, C = g3,
    widths = c(0.3, 0.7),
    design = "AABBB
              AABBB
              AACCC
              AACCC"
  )
gwrapped

ggsave(filename = "./output/pesticides_county_nonpresence.png", plot = gwrapped, width = 15, height = 9, units = "in", dpi = 300)

# How many GEOIDs are present each year?
pests_df_geoid <- pests_df %>%
  select(1:2) %>%
  unique() %>%
  group_by(YEAR) %>%
  summarize(N = n()) %>%
  ungroup()


## [Census Bureau](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2020.html)
## check if the county-equivalent changes are reflected
pests_df_geoid %>%
  filter(GEOID == "51700")
# Dade County -> Miami-Dade County (07/22/1997)
pests_df_geoid %>%
  filter(GEOID == "12025")
# Miami-Dade County
pests_df_geoid %>%
  filter(GEOID == "12086")
pests_df_geoid %>%
  filter(GEOID == "51153")
# Franklin city (12/31/1995)
pests_df_geoid %>%
  filter(GEOID == "51620")
# Southampton County (12/31/1995)
pests_df_geoid %>%
  filter(GEOID == "51175")
# Waynesboro city (07/01/1994)
pests_df_geoid %>%
  filter(GEOID == "51820")
# Adams County, CO (part taken to Broomfield County, 11/15/2001)
pests_df_geoid %>%
  filter(GEOID == "08001")
# Broomfield county (11/15/2001)
pests_df_geoid %>%
  filter(GEOID == "08014")
# Bedford (independent) city, VA (07/01/2013)
pests_df_geoid %>%
  filter(GEOID == "51515")
# Bedford County, VA (07/01/2013)
pests_df_geoid %>%
  filter(GEOID == "51019")
# Oglala Lakota County (05/01/2015)
pests_df_geoid %>%
  filter(GEOID == "46102")
# Shannon County (old Oglala Lakota) (05/01/2015)
pests_df_geoid %>%
  filter(GEOID == "46113")
## Conclusion: name changes -> recode; establishment -> use the shapes in each year

## WBD
wbdpath <- "../../WBD_National_GDB.gdb"
huc08 <- vect(wbdpath, layer = "WBDHU8")
huc10 <- vect(wbdpath, layer = "WBDHU10")
huc12 <- vect(wbdpath, layer = "WBDHU12")


cntys <-
  seq(2000, 2021) %>%
  split(., .) %>%
  lapply(function(x) tigris::counties(cb = TRUE, year = x))

cnty00 <- tigris::counties(year = 2000, cb = TRUE)
cnty07 <- tigris::counties(year = 2007, cb = TRUE)
cnty10 <- tigris::counties(year = 2010, cb = TRUE)
cnty20 <- tigris::counties(year = 2020, cb = TRUE)

cnty14 <- tigris::counties(year = 2014, cb = TRUE)


cnty00 %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  filter(GEOID == "08014")
cnty10 %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  filter(GEOID == "08014")


cnty00 %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  filter(GEOID == "25019")
# ... terra::intersect()
# ... sf::st_interpolate_aw()



## reflecting GEOID changes
# temporal trend
geoid_na_6pyears <- 
  pests_df_geoid_nas |>
  group_by(GEOID) |>
  filter(sum(!value) >= 6) |>
  ungroup() |>
  _[["GEOID"]] |>
  unique()
pests_df |>
  filter(GEOID %in% geoid_na_6pyears) |>
  filter(grepl("AZINE", COMPOUND)) |>
  ggplot(data = _, mapping = aes(x = YEAR)) +
  geom_errorbar(mapping = aes(x = YEAR, ymin = EPEST_LOW_KG, ymax = EPEST_HIGH_KG)) +
  facet_grid(GEOID ~ COMPOUND, scale = "free_y") +
  ylab("Total usage (kg, high/low)") +
  theme(text = element_text(size = 15))



### Cleaning and subsetting workflow: ####
## use sim_dsstox_synonym.xlsx Final_name's unique values
library(readxl)

dsstox <- read_excel("./input/sim_dsstox_synonym.xlsx")
dsstox_unames <- unique(dsstox$Final_name)
dsstox_unames <- dsstox_unames[!is.na(dsstox_unames)]

dsstox_unames

in_data_unames <- pests_df %>%
  filter(COMPOUND %in% dsstox_unames) %>%
  .$COMPOUND %>%
  unique
# FLUVALINATE-TAU and FLUVALINATE TAU coexist
dsstox_unames[!dsstox_unames %in% in_data_unames]
unique(pests_df$COMPOUND) %>% sort


pests_df_sub <-
  pests_df %>%
  filter(COMPOUND %in% dsstox_unames) %>%
  mutate(COMPOUND = plyr::mapvalues(COMPOUND,
    c("FLUVALINATE TAU"), c("FLUVALINATE-TAU")))

pests_df_sub_w <-
  pests_df_sub %>%
  pivot_wider(names_from = COMPOUND,
              values_from = c(EPEST_LOW_KG, EPEST_HIGH_KG))

pests_df_sub_fullyear <-
  expand.grid(GEOID = unique(pests_df_sub$GEOID),
    YEAR = seq(2000, 2019)) %>%
  left_join(., pests_df_sub_w)
  

skimr::skim(pests_df_sub_fullyear)


pests_df_widecomp <-
  pests_df_sub %>%
  rename(low = EPEST_LOW_KG,
    high = EPEST_HIGH_KG) %>%
  pivot_wider(values_from = c(low, high),
    names_from = c(COMPOUND))

pests_df_wideyear <-
  pests_df_sub %>%
  rename(low = EPEST_LOW_KG,
    high = EPEST_HIGH_KG) %>%
  pivot_wider(values_from = c(low, high),
    names_from = c(YEAR))

pests_df_sub_filled <- pests_df_widecomp[,-1:-2] %>%
  missRanger::missRanger(maxiter = 20L)

pests_df_sub_filled2 <- cbind(pests_df_widecomp[,1:2], pests_df_sub_filled)
pests_df_sub_filled2l <- pests_df_sub_filled2 %>%
  pivot_longer(cols = 3:ncol(.))
pests_df_sub_filled2l %>%
  filter(name == "high_2,4-D")
