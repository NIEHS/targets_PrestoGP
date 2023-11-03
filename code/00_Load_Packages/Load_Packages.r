pkgs <- c("sf", "stars", "terra", "dplyr", "tidytable", "tigris", "future", "future.apply", "scomps")
invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))

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
