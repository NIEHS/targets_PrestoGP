######## pacman initialization ########
if (!require(pacman)) {
	install.packages('pacman')
    library(pacman)
    }

p_load(data.table, tidytable, tigris, sf, terra, ggplot2, ggrepel)
sf_use_s2(F)
setDTthreads(percent = 80)

Sys.getenv('TIGRIS_CACHE_DIR')
# tigris::tigris_cache_dir("__cache__dir__")

states = tigris::states()
statesdf = states %>%
    select(3, 6, NAME) %>%
    st_drop_geometry
statesdf = statesdf %>% filter(!STUSPS %in% c("HI", "VI", "GU", "AK", "PR", "MP"))



## AZO 
load("./input/data_process/data_AZO_year_avg.RData")
azo = sf::read_sf("./input/data_process/data_AZO_watershed_huc_join.shp")

azo

azo_s = azo %>%
    dplyr::select(site_no, Year) %>%
    unique %>%
    st_transform("EPSG:4326")

write_sf(azo_s, "~/Documents/AZO.shp")
azo_st = azo_s %>%
    bind_cols(data.frame(st_coordinates(.))) %>%
    st_drop_geometry
write.csv(azo_st, "~/Documents/AZO.csv", row.names = F)
