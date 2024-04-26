calc_olm <- function(data.AZO, wbd_data, hucunit, bulk_density, clay, oc, pH, sand, soil_order, texture){

  layername <- paste0("WBD", toupper(gsub("c", "", hucunit)))
  
  
  # # For efficient extraction, we just need the geometry
  AZO.geometry <- sf::st_geometry(data.AZO)
  

  # # NHD WBD Layer names
  WBD <- sf::st_read(wbd_data, layer = layername) %>%
    st_transform("EPSG:4326")
  
  
  
  # Combine the rasters in which we are calculating a mean value into one raster stack with
  # many layers
  OLM.stack.values <- c(
    bulk_density, clay, oc, pH, sand, soil_order
  )
  
  OLM.stack.classes <- texture
  
  
  # The soil texture needs updating of its class names
  
  texture_classes <- data.frame("classes" = c(
    "clay", "silty_clay", "sandy_clay",
    "clay_loam", "silty_clay_loam", "sandy_clay_loam", "loam",
    "silt_loam", "sandy_loam", "silt",
    "loamy_sand", "sand"
  ), "value" = 1:12)
  
  for (i in 1:6) {
    levels(OLM.stack.classes[[i]]) <- list(data.frame(
      ID = 1:12,
      textures = texture_classes$classes
    ))
  }
  
  names(OLM.stack.classes) <- c(
    "Texture_000cm", "Texture_010cm", "Texture_030cm",
    "Texture_060cm", "Texture_100cm", "Texture_200cm"
  )
  
  
  ## ----Calculate exact grid points ,echo=TRUE-----------------------------------
  

  huc_polygon <- WBD

  HUC.nulltable <- data.table()
  HUC.rast.vals <- HUC.nulltable[, (hucunit) := unlist(huc_polygon[[hucunit]])]
  

  HUC.rast.vals[, paste0(hucunit, ".", names(OLM.stack.values)) := NA_real_]
  
  HUC.rast.class <- HUC.nulltable[, (hucunit) := unlist(huc_polygon[[hucunit]])]
  
  class.df <- expand.grid(levels(OLM.stack.classes)[[1]]$ID, c(
    "Texture_000cm", "Texture_010cm", "Texture_030cm",
    "Texture_060cm", "Texture_100cm", "Texture_200cm"
  ))
  class.df <- class.df[order(class.df$Var1), ]
  
  class.possible.names <- paste0("frac_", class.df$Var1, ".", class.df$Var2)
  
  
  HUC.rast.class[, paste0(hucunit, ".", class.possible.names) := 0]
  
  # for (i in 1:length(huc08.unique)) {
  #   print(i)
  
  # get index - for rows of output - where data match the given HUC08
  # idx.row <- data.AZO$huc08 == huc08.unique[i]
  
  
  # calculate the mean raster values in the HUC
  huc08.val <- exact_extract(OLM.stack.values, st_geometry(huc_polygon), fun = "mean", stack_apply = TRUE)
  
  # Assign the extracted values to the appropriate location in the output
  HUC.rast.vals[, 2:ncol(HUC.rast.vals)] <- huc08.val
  
  # calculate the fraction of each raster class in the HUC
  extract.raster.classes <- exact_extract(OLM.stack.classes, st_geometry(huc_polygon), fun = "frac", stack_apply = TRUE)
  
  # # get indexs - for columns of outout - where classes match the output
  idx.col <- which(class.possible.names %in% colnames(extract.raster.classes)) + 1
  
  # Assign the extracted class fractions to the appropriate location in the output
  HUC.rast.class[, idx.col] <- extract.raster.classes
  
  
  # Correct some column names that will be problematic later
  
  HUC.rast.vals <- HUC.rast.vals %>%
    rename_with(function(x) str_replace_all(x, "[.]", "_")) %>%
    rename_with(function(x) str_replace_all(x, "_{2}", "_")) %>%
    rename_with(function(x) str_replace_all(x, "sol_order_usda_soiltax_", "")) %>%
    rename_with(function(x) str_replace_all(x, "_1950_2017_v0_1", "")) %>%
    select(-!!sym(hucunit))
  
  # Classes column names updates
  HUC.rast.class <- HUC.rast.class %>%
    rename_with(function(x) str_replace_all(x, "[.]", "_")) %>%
    select(-!!sym(hucunit))
  
  # Rename the number with the class name - ugly but it works
  HUC.rast.class <- dplyr::rename_with(HUC.rast.class, ~ gsub("frac_1_", paste0("frac_", texture_classes$classes[1], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_2_", paste0("frac_", texture_classes$classes[2], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_3_", paste0("frac_", texture_classes$classes[3], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_4_", paste0("frac_", texture_classes$classes[4], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_5_", paste0("frac_", texture_classes$classes[5], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_6_", paste0("frac_", texture_classes$classes[6], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_7_", paste0("frac_", texture_classes$classes[7], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_8_", paste0("frac_", texture_classes$classes[8], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_9_", paste0("frac_", texture_classes$classes[9], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_10_", paste0("frac_", texture_classes$classes[10], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_11_", paste0("frac_", texture_classes$classes[11], "_"), .x, fixed = TRUE)) %>%
    dplyr::rename_with(~ gsub("frac_12_", paste0("frac_", texture_classes$classes[12], "_"), .x, fixed = TRUE))
  
  
  ## ----Save the data to a geopackage (OSG open source format) ,echo=TRUE--------
  data.AZO.HUC08.OLM <- cbind(HUC08=unlist(huc_polygon[[hucunit]]), HUC.rast.vals, HUC.rast.class)
  names(data.AZO.HUC08.OLM)[1] <- hucunit

  return(data.AZO.HUC08.OLM)

    
}

olm_read_crop <- function(olm_data){
  # # US bounding box
  US.bb <- terra::ext(c(-124.7844079, -66.9513812, 24.7433195, 49.3457868))
  stack <- terra::rast(olm_data) %>% terra::crop(US.bb)
  return(stack)
}
