calc_TWI_huc <- function(data.AZO, raster, wbd_data, hucunit){


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
  
# get the row index for unique HUC values
 



    ### continuous value rasters
    
    # Make the data tables
    ras_vals <- data.table(hucunit = data.AZO[[hucunit]]) |>
      setnames(hucunit)
    
    field_name <- paste0(tc_names, "_", hucunit)
    
    ras_vals[, {field_name} := NA_real_]
    
    AZO.index <- data.AZO$Year - 2000 + 1
    

    for (i in 1:length(huc_unique)) {
      
      # get index - for rows of output - where data match the given HUC08
      idx.row <- data.AZO[[hucunit]] == huc_unique[i]
      
      # get the given HUC geometry from the WBD data
      huc_polygon <- WBD[WBD[[hucunit]] == huc_unique[i],]
      
      # calculate the mean raster values in the HUC
      huc_val <- exact_extract(raster, st_geometry(huc_polygon), fun = "mean", stack_apply = TRUE)
      

      # Assign the extracted values to the appropriate location in the output
      ras_vals[idx.row, {field_name}] <- huc_val[AZO.index[idx.row]]

    
    }
    

    ras_vals <- ras_vals %>%
      select(-hucunit)
  
  return(ras_vals)
  
}



calc_TWI_point <- function(data.AZO, raster, tc_names) {
  
  
  # Match the year field in data.AZO with the year field in the raster

  
  # # For efficient extraction, we just need the geometry
  AZO.geometry <- sf::st_geometry(data.AZO)
  
  # Column index corresponding to year
  AZO.index <- data.AZO$Year - 2000 + 1
 
    ras_vals <- exact_extract(raster, AZO.geometry, fun = "mean", stack_apply = TRUE, 
                              force_df = TRUE, progress = TRUE)
    
    ras_out <- ras_vals[cbind(1:nrow(ras_vals), AZO.index)] |> as.data.table()
    
    names(ras_out) <- paste0(tc_names, "_buffer")

  return(ras_out)
  
}


