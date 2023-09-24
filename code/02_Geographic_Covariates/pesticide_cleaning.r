######## pacman initialization ########
if (!require(pacman)) {
	install.packages('pacman')
    library(pacman)
    }

p_load(data.table, tidytable, tigris, sf, terra)

Sys.getenv('TIGRIS_CACHE_DIR')
# tigris::tigris_cache_dir("__cache__dir__")

states = tigris::states()
statesdf = states %>%
    select(3, 6, NAME) %>%
    st_drop_geometry
statesdf %>% filter(STUSPS %in% c("HI", "VI", "GU", "AK", "PR", "MP"))
dim(statesdf)

pestdir = "../../pesticides/"
txts = list.files(path = pestdir, pattern = "*.txt$", full.names = TRUE)

pests = lapply(txts, fread)
pests = lapply(pests,
    function(x) {
        x %>% mutate(GEOID = sprintf("%02d%03d", STATE_FIPS_CODE, COUNTY_FIPS_CODE))
    })

pests_df = rbindlist(pests)
pests_df = pests_df %>%
    arrange(GEOID, YEAR, COMPOUND) %>%
    select(GEOID, YEAR, COMPOUND, starts_with("EPEST"))
pests_df = pests_df %>%
    filter(YEAR >= 2000)

# wide frame
pests_df_geoid = pests_df %>%
    select(1:2) %>%
    unique() %>%
    mutate(valfill = TRUE) %>%
    pivot_wider(names_from = YEAR, values_from = valfill)

# Which counties are partially present during the study period?
pests_df_geoid_nas = pests_df_geoid %>%
    filter(!complete.cases(.))
pests_df_geoid_nas

# How many GEOIDs are present each year?
pests_df_geoid = pests_df %>%
    select(1:2) %>%
    unique() %>%
    group_by(YEAR) %>%
    summarize(N = n()) %>%
    ungroup


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
wbdpath = "../../WBD_National_GDB.gdb"
huc08 = vect(wbdpath, layer = "WBDHU8")
huc10 = vect(wbdpath, layer = "WBDHU10")
huc12 = vect(wbdpath, layer = "WBDHU12")


cntys = 
    seq(2000, 2021) %>%
        split(.,.) %>%
        lapply(function(x) tigris::counties(cb = TRUE, year = x))

cnty00 = tigris::counties(year = 2000, cb = TRUE)
cnty07 = tigris::counties(year = 2007, cb = TRUE)
cnty10 = tigris::counties(year = 2010, cb = TRUE)
cnty20 = tigris::counties(year = 2020, cb = TRUE)

cnty14 = tigris::counties(year = 2014, cb = TRUE)


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