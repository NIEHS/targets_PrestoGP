# source("../00_Load_Packages/Load_Packages.r")

pkgs = c("sf", "stars", "terra", "dplyr", "tidytable", "future", "future.apply", "scomps")
invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
sf_use_s2(FALSE)
# 1 -- localmount: mounted volume
# 2 -- computenode: HPC job submission
COMPUTE_MODE = 1 # Sys.getenv("COMPUTE_MODE")

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
        # print(nass)
        huc08 = terra::vect(wbdpath, layer = "WBDHU8")
        huc08 = huc08[ext_mainland,]
        huc08 = terra::project(huc08, "EPSG:5070")
        huc08 = huc08[,'huc8']
        huc08$huc_split = substr(huc08$huc8, 1, 4)
        # subset
        huc08 = huc08[which(huc08$huc_split %in% unique(huc08$huc_split)),]

        fname = gsub("\\.tif", "_huc08_ext.csv", x)
        fname = gsub("/opt/USDA_NASS", "/mnt", fname)
        split(huc08, huc08$huc_split) %>%
            lapply(function(k) {
                nass = terra::rast(x, win = terra::ext(k))
                nass = terra::crop(nass, terra::ext(k))
                hucfrq = terra::freq(nass, zones=k, wide = TRUE)
                hucfrq$HUC = k$huc8[hucfrq$zone]
                return(hucfrq)
            }) %>%
            do.call(dplyr::bind_rows, .) %>%
            write.csv(., fname, row.names = FALSE)
        gc()

        huc10 = terra::vect(wbdpath, layer = "WBDHU10")
        huc10 = huc10[ext_mainland,]
        huc10 = terra::project(huc10, "EPSG:5070")
        huc10 = huc10[,'huc10']
        huc10$huc_split = substr(huc10$huc10, 1, 4)
        # subset
        huc10 = huc10[which(huc10$huc_split %in% unique(huc10$huc_split)),]

        fname = gsub("\\.tif", "_huc10_ext.csv", x)
        fname = gsub("/opt/USDA_NASS", "/mnt", fname)
        split(huc10, huc10$huc_split) %>%
            lapply(function(k) {
                nass = terra::rast(x, win = terra::ext(k))
                nass = terra::crop(nass, terra::ext(k))
                hucfrq = terra::freq(nass, zones=k, wide = TRUE)
                hucfrq$HUC = k$huc10[hucfrq$zone]
                return(hucfrq)
            }) %>%
            do.call(dplyr::bind_rows, .) %>%
            write.csv(., fname, row.names = FALSE)
        gc()

        huc12 = terra::vect(wbdpath, layer = "WBDHU12")
        huc12 = huc12[ext_mainland,]
        huc12 = terra::project(huc12, "EPSG:5070")
        huc12 = huc12[,'huc12']
        huc12$huc_split = substr(huc12$huc12, 1, 6)
        # subset
        huc12 = huc12[which(huc12$huc_split %in% unique(huc12$huc_split)),]

        fname = gsub("\\.tif", "_huc12_ext.csv", x)
        fname = gsub("/opt/USDA_NASS", "/mnt", fname)
        split(huc12, huc12$huc_split) %>%
            lapply(function(k) {
                nass = terra::rast(x, win = terra::ext(k))
                nass = terra::crop(nass, terra::ext(k))
                hucfrq = terra::freq(nass, zones=k, wide = TRUE)
                hucfrq$HUC = k$huc12[hucfrq$zone]
                return(hucfrq)
            }) %>%
            do.call(dplyr::bind_rows, .) %>%
            write.csv(., fname, row.names = FALSE)

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


# csv_compar = list(
#     d1 = read.csv("./output/2018_30m_cdls_huc10_ext.csv"),
#     d2 = read.csv("./output/2008_30m_cdls_huc10_ext.csv")
# )

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