# source("../00_Load_Packages/Load_Packages.r")

pkgs = c("sf", "stars", "terra", "dplyr", "tidytable", "future", "future.apply", "scomps")
invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
sf_use_s2(FALSE)
# 1 -- localmount: mounted volume
# 2 -- computenode: HPC job submission
COMPUTE_MODE = 2 # Sys.getenv("COMPUTE_MODE")

datadir = ifelse(COMPUTE_MODE == 1,
    "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
    "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/")
# option 2: apptainer mount point
datadir = "/opt/"

# datadir = "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/"
wbddir = paste0(datadir, "WBD-National/")
wbdpath = paste0(wbddir, "WBD_National_GDB.gdb")
# wbd_list = sf::st_layers(wbdpath)

list_hucs = c("WBDHU8", "WBDHU10", "WBDHU12")
# c("WBDHU2", "WBDHU4", "WBDHU6", "WBDHU8", "WBDHU10", "WBDHU12")
# list_hucs_ex = split(list_hucs, list_hucs) |>
#     lapply(function(x) sf::read_sf(wbdpath, layer = x, stringsAsFactors = TRUE))

list_nass = list.files(path = paste0(datadir, "USDA_NASS"), pattern = "*.tif$", full.names = TRUE)

ext_mainland = c(xmin = -125, xmax = -62, ymin = 24, ymax = 61)
ext_mainland = terra::ext(ext_mainland)

# query = sprintf("select * from WBDHU%d where states not in ")
# list_hucs_tr = split(list_hucs, list_hucs) |>
#     lapply(function(x) terra::vect(wbdpath, layer = x)) |>
#     lapply(function(x) x[ext_mainland,])

# plot(list_hucs_tr[[1]] %>% .[ext_mainland,])
# plot(list_hucs_tr[[3]] %>% .[ext_mainland,])

future::plan(multicore, workers = 15L)

# list_hucs_tr = split(list_hucs, list_hucs) |>
#     lapply(function(x) terra::vect(wbdpath, layer = x)) |>
#     lapply(function(x) x[ext_mainland,]) |>
#     lapply(function(x) {
#         this_name = grep("^huc", names(x), value = TRUE)
#         #this_name = gsub("\\d{1,2}", "", this_name)
#         x$HUC = x[[this_name]]
#         return(x[,"HUC"])
#     })
# list_hucs_trc = Reduce(rbind, list_hucs_tr)


# double-loop (even though it is not a good practice, for efficiency)
# for (huc in list_hucs) {
#     target_huc = terra::vect(wbdpath, layer = huc)
#     target_huc = target_huc[ext_mainland,]
#     # split by huc2
#     this_name = gsub("WBDHU", "huc", huc)
#     target_huc$huc_split = substr(target_huc[[this_name]], 1, 2)
#     target_huc_list = split(target_huc, target_huc$huc_split)
#     future.apply::future_lapply(target_huc_list,
#         FUN = scomps::extract_with(id = this_name)
#     )
#     scomps::extract_with()

# }

## file based strategy

# terra::vect(wbdpath, layer = "WBDHU8")


# test: 17:41 09/20/2023
split(list_nass, list_nass) %>%
    future_lapply(., function(x) { 
        nass = terra::rast(x)
        # print(nass)
        huc = terra::vect(wbdpath, layer = "WBDHU8")
        huc = huc[ext_mainland,]
        huc = terra::project(huc, "EPSG:5070")
        huc = huc[,'huc8']
        hucfrq = terra::freq(nass, zones=huc, wide = TRUE)
        fname = gsub("\\.tif", "_huc8_ext.csv", x)
        write.csv(hucfrq, fname, row.names = FALSE)
        # for (huc in list_hucs) {
        #     target_huc = terra::vect(wbdpath, layer = huc)
        #     target_huc = target_huc[ext_mainland,]
        #     # split by huc2
        #     this_name = gsub("WBDHU", "huc", huc)
        #     target_huc$huc_split = substr(target_huc[[this_name]], 1, 2)
        #     target_huc_list = split(target_huc, target_huc$huc_split)
        #     future.apply::future_lapply(target_huc_list,
        #         FUN = scomps::extract_with(id = this_name)
        #     )
        #     scomps::extract_with()        
    })


# for (nass in list_nass) {
#     target_huc = terra::vect(wbdpath, layer = huc)
#     target_huc = target_huc[ext_mainland,]
#     # split by huc2
#     this_name = gsub("WBDHU", "huc", huc)
#     target_huc$huc_split = substr(target_huc[[this_name]], 1, 2)
#     target_huc_list = split(target_huc, target_huc$huc_split)
#     future.apply::future_lapply(target_huc_list,
#         FUN = scomps::extract_with(id = this_name)
#     )
#     scomps::extract_with()

# }



# test= terra::rast(paste0(datadir, "USDA_NASS/2010_30m_cdls.tif"))
# wbdhucs2 = terra::vect(wbdpath, layer = "WBDHU2")
# wbdhucs2 = wbdhucs2[ext_mainland,]
# wbdhucs2 = terra::project(wbdhucs2, "EPSG:5070")
# testext = scomps::extract_with(test, wbdhucs2[1,] |> terra::project("EPSG:5070"), "huc2", func = mean)

# wbdhucs8 = terra::vect(wbdpath, layer = "WBDHU8")
# wbdhucs8 = wbdhucs8[ext_mainland,]
# wbdhucs8 = terra::project(wbdhucs8, "EPSG:5070")
# testkk = terra::rast(paste0(datadir, "USDA_NASS/2010_30m_cdls.tif"), win = wbdhucs2[2,])


# library(tictoc)

# tic()
# testext1 = scomps::extract_with(test, wbdhucs2[1,] |> terra::project("EPSG:5070"), "huc2", func = mean)
# toc()

# tic()
# testext2 = terra::rast(paste0(datadir, "USDA_NASS/2010_30m_cdls.tif"), win = wbdhucs8[2,]) |>
#     terra::crop(wbdhucs8[2,]) |>
#     terra::freq()
# toc()

# tic()
# testext2 = terra::rast(paste0(datadir, "USDA_NASS/2010_30m_cdls.tif"), win = wbdhucs2[2,]) |>
#     terra::crop(wbdhucs2[2,]) |>
#     terra::freq()
# toc()