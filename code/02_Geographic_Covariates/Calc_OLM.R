calc_olm_huc <- function(data.AZO, olm_raster, wbd_data, hucunit){

  # olm_raster <- olm_raster[[1]]
  
  layername <- paste0("WBD", toupper(gsub("c", "", hucunit)))

  # # NHD WBD Layer names
  WBD <- sf::st_read(wbd_data, layer = layername) %>%
    st_transform("EPSG:4326")

  # Update hucunit variable - Pad zeros for single digit hucs to be 2 digits (i.e. huc8 -> huc08)
  if (nchar(hucunit) == 4) {
    colnames(WBD)[colnames(WBD) == hucunit] <- paste0("huc0", str_sub(hucunit,4))
    hucunit <- paste0("huc0", str_sub(hucunit,4))
  } 

  # Get the unique HUCs
  huc_unique <- unique(data.AZO[[hucunit]])
  

  


  
  
  if (any(grepl("TextureClass",names(olm_raster)))) {
    print("Yes, it is Texture")
    
    olm_raster <- terra::as.factor(olm_raster)
    
    texture_classes <- data.frame("classes" = c(
      "clay", "silty_clay", "sandy_clay",
      "clay_loam", "silty_clay_loam", "sandy_clay_loam", "loam",
      "silt_loam", "sandy_loam", "silt",
      "loamy_sand", "sand"
    ), "value" = 1:12)
    
    for (i in 1:6) {
      levels(olm_raster[[i]]) <- list(data.frame(
        ID = 1:12,
        textures = texture_classes$classes
      ))
    }
    
    names(olm_raster) <- c(
      "Texture_0cm", "Texture_10cm", "Texture_30cm",
      "Texture_60cm", "Texture_100cm", "Texture_200cm"
    )
    
    
    
    class.df <- expand.grid(levels(olm_raster)[[1]]$ID, c(
      "Texture_0cm", "Texture_10cm", "Texture_30cm",
      "Texture_60cm", "Texture_100cm", "Texture_200cm"
    ))
    
    olm_values <- data.table(hucunit = data.AZO[[hucunit]]) |>
      setnames(hucunit)
    
    class.df <- class.df[order(class.df$Var1), ]
    
    class.possible.names <- paste0("frac_", class.df$Var1, ".", class.df$Var2)
    
    olm_values[, paste0(hucunit, ".", class.possible.names) := 0]
    
    
    for (i in 1:length(huc_unique)) {

      # get index - for rows of output - where data match the given HUC08
      idx.row <- data.AZO[[hucunit]] == huc_unique[i]

      # get the given HUC08 geometry from the WBD data
      huc_polygon <- WBD[WBD[[hucunit]] == huc_unique[i],]

      # calculate the mean raster values in the HUC
      huc_val <- exact_extract(olm_raster, st_geometry(huc_polygon), fun = "frac", stack_apply = TRUE)
      # get indexs - for columns of outout - where classes match the output
      idx.col <- which(class.possible.names %in% colnames(huc_val)) + 1

      # Assign the extracted class fractions to the appropriate location in the output
      olm_values[idx.row, idx.col] <- huc_val
    }
    
    

    # Classes column names updates
    olm_values <- olm_values %>%
      rename_with(function(x) str_replace_all(x, "[.]", "_")) 
    # Rename the number with the class name - ugly but it works
    olm_values <- dplyr::rename_with(olm_values, ~ gsub("frac_1_", paste0("frac_", texture_classes$classes[1], "_"), .x, fixed = TRUE)) %>%
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
    
    
  } else { 
    ### continuous value rasters
    
    # Make the data tables
    olm_values <- data.table(hucunit = data.AZO[[hucunit]]) |>
      setnames(hucunit)
    
    olm_values[, paste0(hucunit, "_", names(olm_raster)) := NA_real_]
    

    for (i in 1:length(huc_unique)) {
      
      # get index - for rows of output - where data match the given HUC08
      idx.row <- data.AZO[[hucunit]] == huc_unique[i]
      
      # get the given HUC geometry from the WBD data
      huc_polygon <- WBD[WBD[[hucunit]] == huc_unique[i],]
      
      # calculate the mean raster values in the HUC
      huc_val <- exact_extract(olm_raster, st_geometry(huc_polygon), fun = "mean", stack_apply = TRUE)
      
      # Assign the extracted values to the appropriate location in the output
      olm_values[idx.row, 2:ncol(olm_values)] <- huc_val
 
    }
    
    
    olm_values <- olm_values %>%
      rename_with(function(x) str_replace_all(x, "[.]", "_")) %>%
      rename_with(function(x) str_replace_all(x, "_{2}", "_")) %>%
      rename_with(function(x) str_replace_all(x, "sol_order_usda_soiltax_", "")) %>%
      rename_with(function(x) str_replace_all(x, "_1950_2017_v0_1", ""))
    
  }
  
  olm_values <- olm_values %>%
    select(-hucunit)
  
  return(olm_values)
  
}



calc_olm_point <- function(data.AZO, olm_raster) {
  
  # olm_raster <- olm_raster[[1]]
  
  # # For efficient extraction, we just need the geometry
  AZO.geometry <- sf::st_geometry(data.AZO)
  
  if (any(grepl("TextureClass",names(olm_raster)))) {
    print("Yes, it is Texture")
    
    olm_raster <- terra::as.factor(olm_raster)
    
    texture_classes <- data.frame("classes" = c(
      "clay", "silty_clay", "sandy_clay",
      "clay_loam", "silty_clay_loam", "sandy_clay_loam", "loam",
      "silt_loam", "sandy_loam", "silt",
      "loamy_sand", "sand"
    ), "value" = 1:12)
    
    for (i in 1:6) {
      levels(olm_raster[[i]]) <- list(data.frame(
        ID = 1:12,
        textures = texture_classes$classes
      ))
    }
    
    names(olm_raster) <- c(
      "Texture_0cm", "Texture_10cm", "Texture_30cm",
      "Texture_60cm", "Texture_100cm", "Texture_200cm"
    )
    
    olm_values <- exact_extract(olm_raster, AZO.geometry, fun = "frac", stack_apply = TRUE)
    # Classes column names updates
    olm_values <- olm_values %>%
      rename_with(function(x) str_replace_all(x, "[.]", "_")) 
    
    # Rename the number with the class name - ugly but it works
    olm_values <- dplyr::rename_with(olm_values, ~ gsub("frac_1_", paste0("frac_", texture_classes$classes[1], "_"), .x, fixed = TRUE)) %>%
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
    
    
    
  } else {
    olm_values <- exact_extract(olm_raster, AZO.geometry, fun = "mean", stack_apply = TRUE)
    
    olm_values <- olm_values %>%
      rename_with(function(x) str_replace_all(x, "[.]", "_")) %>%
      rename_with(function(x) str_replace_all(x, "_{2}", "_")) %>%
      rename_with(function(x) str_replace_all(x, "sol_order_usda_soiltax_", "")) %>%
      rename_with(function(x) str_replace_all(x, "_1950_2017_v0_1", ""))
    
  }

  return(olm_values)
  
}


