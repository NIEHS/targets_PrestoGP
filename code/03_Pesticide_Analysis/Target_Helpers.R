# TARGETS help functions for PrestoGP Pesticide Pipeline Analysis 

list_raster <- function(base_path, sub_path, recursive = TRUE) {
  # Combine base_path and all subpaths
  full_path <- file.path(base_path, sub_path)
  
  # List all the rasters in the directory
  rasters <- list.files(full_path, pattern = ".tif$", full.names = TRUE, recursive = recursive)
  
  return(rasters)
}


join_pesticide_huc <- function(points, wbd_huc){
  
  sf_use_s2(FALSE)
  
  HUC12 <- sf::st_read(wbd_huc, layer = "WBDHU12")
  
  # Convert both the AZO points and HUC to Albers Equal Area projected coordinate system
#   AZO.t <- sf::st_transform(points, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  AZO.t <- sf::st_transform(points, crs = st_crs(HUC12))
  

  
  AZO.HUC.join <- sf::st_join(AZO.t, HUC12)
  
  AZO.HUC.join <- sf::st_transform(AZO.HUC.join, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  AZO.HUC.join$huc10 <- str_sub(AZO.HUC.join$huc12, 1, 10)
  AZO.HUC.join$huc08 <- str_sub(AZO.HUC.join$huc12, 1, 8)
  AZO.HUC.join$huc06 <- str_sub(AZO.HUC.join$huc12, 1, 6)
  AZO.HUC.join$huc04 <- str_sub(AZO.HUC.join$huc12, 1, 4)
  AZO.HUC.join$huc02 <- str_sub(AZO.HUC.join$huc12, 1, 2)
  
  return(AZO.HUC.join)
  
}



partition_datasets <- function(data_sf) {
  
  # drop the soil chemistry covariates (not needed and too difficult to work with)
  data_sf <- data_sf %>% 
    select(-starts_with("soilchem"))
  
  
  # Partition the data into (1) outcome + covariates/features (2) ancillary info
  outcome <- data_sf |> 
    dplyr::select("id","ChmclNm","Year","cncntrt","lft_cns",
                  starts_with("nass"),
                  starts_with("olm"),
                  starts_with("aquifer"),
                  starts_with("tclim"),
                  starts_with("prism"),
                  contains("geology_unit_type"),
                  starts_with("pest")) 
  

  
  ancillary <- data_sf |> 
    dplyr::select("id","site_no","parm_cd","Units","ContyNm","StateNm","wll_dpt",
                  "nsampls","objectid","tnmid","metasource","sourcedata","sourceorig",
                  "sourcefeat","loaddate","referenceg","areaacres","areasqkm",
                  "states","huc12","huc10","huc08","huc06","huc04","huc02",
                  "name","hutype","humod","tohuc","noncontrib","noncontr_1","globalid",
                  "shape_Leng","shape_Area")
  
  return(list(outcome, ancillary))
}


#  Covariate preparation includes dropping continuous covariates that don't have enough unique values - and creating dummy variables for categorical covariates
#' covariate_prep
#'
#' @param data Partitioned data with 2 lists: main data and ancillary
#' @param threshold Threshold for the number of unique values in a numeric covariate
#' @return
#' @export
#'
#' @examples
covariate_prep <- function(data, threshold = 1e-5){
  
    outcome <- data[[1]] |>
      dplyr::select(c("id","ChmclNm","Year","cncntrt","lft_cns"))

    data_covariates <- data[[1]] |>
      dplyr::select(c("id"),
                    starts_with("nass"),
                    starts_with("olm"),
                    starts_with("aquifer"),
                    starts_with("tclim"),
                    starts_with("prism"),
                    contains("geology_unit_type"),
                    starts_with("pest")
      )



  # Get the numeric covariates
  covariates_num <- dplyr::select_if(data_covariates, is.numeric)

  # Get the factor type covariates and drop aquifer_ROCK_NAME
  covariates_factor <- dplyr::select_if(data_covariates, is.factor) |> 
    select(-"aquifer_ROCK_NAME")

  covariates_factor$aquifer_AQ_NAME <- fct_explicit_na(covariates_factor$aquifer_AQ_NAME, "unknown")
  
  

  # Calculate unique values for each numeric covariate
  n_unique <- covariates_num |> 
    dplyr::summarise_all(n_distinct) |> 
    dplyr::mutate_all(~ . / nrow(covariates_num) * 100) |>
    as.numeric() |>
    t() 

  # Filter the numeric covariates by the threshold
  covariates_num_filter <- covariates_num |>
    select(which(n_unique > threshold)) 

  
  
  # Create dummy variables for the factor covariates
  covariates_factor_dummy <- dummy_cols(covariates_factor, 
                                        c("geology_unit_type", 
                                          "aquifer_AQ_NAME"), 
                                        remove_selected_columns = TRUE, 
                                        ignore_na = FALSE) |>
                                        st_as_sf() 

  
  # Combine the numeric and factor covariates
  data_processed <- outcome |> 
    cbind(covariates_num_filter) |>
    cbind(covariates_factor_dummy) |>
    select(-"geometry.1",-"geometry.2",-"id.1") 

  
  return(list(data_processed, data[[2]]))
}



#' plot_cv_map
#'
#' @param pesticide 
#'
#' @return p ggplot object
#' @export
#'
#' @examples
plot_cv_map <- function(pesticide) {
  
  # Create a ggplot object
  p <- ggplot() +
    geom_sf(data = pesticide[[1]], aes(color = kfolds )) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
  
}


pivot_covariates <- function(data) {
  
  # Variables to pivot 
  
  
  vars_to_plot <- c("id","olm_huc12_frac_clay_loam_Texture_010cm","olm_huc12_Sand_200cm","olm_huc08_frac_clay_Texture_030cm","nass_huc08_potatoes","nass_huc08_developed_highintensity","nass_huc12_woodywetlands",
    "pest_low_2000_2019_INDOXACARB","pest_low_2000_2019_ATRAZINE","pest_high_2000_2019_SIMAZINE","geology_unit_type_Baltimore.Gneiss","aquifer_AQ_NAME_Ordovician.aquifers",
    "geology_unit_type_Ultramafic.rocks..chiefly.Mesozoic..unit.2..Western.Sierra.Nevada.and.Klamath.Mountains.","prism_huc12_solslope","tclim_sum_soil_huc08",
    "aquifer_AQ_NAME_New.York.and.New.England.carbonate.rock.aquifers")
  
  data_temp <- data |>
    select(all_of(vars_to_plot))
  
  # Pivot the covariates
  data_pivot <- data_temp |>
    pivot_longer(cols = c(-"id", -"geometry"), names_to = "covariate", values_to = "value")
  
  return(data_pivot)
  
}


#' fit_lasso
#'
#' @param data assume [[1]] is the outcome sf, [[2]] is the covariates sf
#'
#' @return
#' @export
#'
#' @examples
fit_lasso <- function(data) {
  

  
   
  # Fit using the linear_model function in Parsnip
  lasso <- linear_reg(penalty = 1) %>%
    set_engine("glmnet", family = "gaussian") %>%
    set_mode("regression") %>%
    fit(log(cncntrt) ~ ., data = data)

  return(lasso)
  
}


prepare_pesticide_for_fit <- function(data) {
  data_outcome <- data[[1]]
  
  data_covariates <- data[[2]]
  
  # Put together the final dataframe
  
  data_outcome_to_join <- data_outcome |>
    st_drop_geometry() |>
    select(c("cncntrt","id")) |>
    as.data.frame() |>
    mutate(id = row_number()) 
  
  
  data_covariates_to_join <- data_covariates |>
    st_drop_geometry() |>
    as.data.frame() |>
    mutate(id = row_number())
  
  data_fit <- left_join(data_outcome_to_join, data_covariates_to_join, by = "id") |>
    select(-"id")
  
  return(data_fit)
}

## splits will be the `rsplit` object with the 90/10 partition
#' lasso_spatial_kfold_fit
#'
#' @param splits 
#' @param ... a formula object
#'
#' @return
#' @export
#'
#' @examples
  lasso_spatial_kfold_fit <- function(splits, formula) {
  
  # Fit the model to the 90%
  data_analysis <- splits |> 
    analysis() |>
    sf::st_drop_geometry()
    
    
  # Fit using the linear_model function in Parsnip
  lasso <- linear_reg(penalty = 1) %>%
    set_engine("glmnet", family = "gaussian") %>%
    set_mode("regression") %>%
    fit(formula, data = data_analysis)
  
  # Save the 10%
  holdout <-  splits |> 
    assessment() |>
    sf::st_drop_geometry()
  # `augment` will save the predictions with the holdout data set
  res <- broom::augment(lasso, new_data = holdout)
  return(res)
}


#' plot_exploratory_covariates
#'
#' @param data 
#'pl
#' @return
#' @export
#'
#' @examples
plot_exploratory_covariates <- function(data) {
  
  if (length(unique(data$value)) == 2) { # Discrete/Binary covariate
    g <- ggplot() +
      geom_sf(data = data, aes(color = as.factor(value))) +
      theme_minimal() +
      scale_color_viridis_d() +
      ggtitle(data$covariate) +
      theme(legend.position = "right")
    
    
  } else { # Continuous covariate
    g <- ggplot() +
      geom_sf(data = data, aes(color = value )) +
      theme_minimal() +
      scale_color_viridis_c(option = "C",  trans = scales::pseudo_log_trans(sigma = 0.001)) +
      ggtitle(data$covariate) +
      theme(legend.position = "right")
    
    
  }

  return(g)
}



plot_pesticide_ridges <- function(data) {
 
  df <- as.data.frame(data)
  df$cncntrt[df$cncntrt == 0] <- 0.0001
  
  sample_size = df %>% group_by(ChmclNm) %>% summarize(num = n(), bd = sum(lft_cns)/length(lft_cns) * 100)
 
  p <- 
  df |>
    left_join(sample_size) %>%
    mutate(myaxis = paste0(ChmclNm, "\n", "n=", num," ","percentBD=",round(bd,1))) %>%
    group_by(ChmclNm) %>% 
    ggplot() +
    geom_density_ridges(aes(x = cncntrt,y = as.factor(myaxis), fill = as.factor(lft_cns))) +
     scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    scale_fill_viridis_d(option = "D") +
    labs(y = "Chemical",x = "Concentration (ug/L)",fill = "Censored")
  
  return(p)
  
}



plot_outcome_map <- function(data) {
  
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

  df_obs <- data |> filter(lft_cns == 0)
  df_cens <- data |> filter(lft_cns == 1)
  
  p <- ggplot() +
    geom_sf(data = df_cens, size = 0.5) +
    geom_sf(data = df_obs, aes(color = cncntrt)) +
    # facet_wrap(~ Year) +
    scale_color_viridis_c(option = "A", trans = scales::pseudo_log_trans(sigma = 0.005)) +
    geom_sf(data = states, fill = NA, size=0.15) +
    theme_minimal() +
    theme(legend.position = "right") +
    ggtitle(data$ChmclNm) 
  
  return(p)
  
}



fit_MV_Vecchia <- function(splits) {
  
  # Fit the model to the training
  data_analysis <- splits |> 
    analysis() |>
    sf::st_drop_geometry()
  
  # Save the assessment split
  holdout <-  splits |> 
    assessment() |>
    sf::st_drop_geometry()

  
  # 1) A List of Y's, each element is a vector of the response variable
  # 1a) Y normal vs log-transform?
  # 2) A list of LOD, each element is a vector of the limit of detection
  # 2a) If Y is log-transfor, then LOD is log-transformed
  # 3) Impute.y = TRUE, impute the missing values in Y
  # 4) A list of X's, each element is a matrix of the covariates
  # 5) A list of locs, each element is a matrix of the locations
  # 6) Apanasovich is TRUE
  # 7) scaling = c(1,1,2) -> maybe needs to be list? could also do c(1,2,3) 
  
  # 1) A List of Y's, each element is a vector of the response variable
  # use dplyr to create a separate list by each ChmlNm and extract only the cncntrt column
  # If the variable lft_cns is 0, then the cncntrt is the value, otherwise we convert it to a 
  # small value of 1e-9
  pesticide_outcomes <- data_analysis |> 
    group_by(ChmclNm) |> 
    nest() |>
    mutate(data = map(data, ~ ifelse(.x$lft_cns == 0, .x$cncntrt, 1e-9))) |>
    pull(data)
  
  # 2) A list of LOD, each element is a vector of the limit of detection
  # use dplyr to create a separate list by each ChmlNm and create an LOD. The 
  # LOD should be the cncntrt
  pesticide_lod <- data_analysis |> 
    group_by(ChmclNm) |> 
    nest() |>
    mutate(data = map(data, ~ .x$cncntrt)) |>
    pull(data)

  #2a) Create a list of the of the lft_cns variable by ChmclNm
  pesticide_lft_cns <- data_analysis |> 
    group_by(ChmclNm) |> 
    nest() |>
    mutate(data = map(data, ~ .x$lft_cns)) |>
    pull(data)
  
  # Comment for Sciome - I think a more standard or straight forward approach
  # to implement an LOD would be to have 1 value for the cnct and then a binary
  # for whether it is observed or a limit of detection. 
  
  # 3) Impute.y = TRUE, impute the missing values in Y
  Impute.y = TRUE
  
  # 4) A list of X's, each element is a matrix of the covariates
  # For now, we will use the covariates in the data_analysis
  # Convert each list element to a matrix (i.e. is.matrix = TRUE)
  pesticide_covariates <- data_analysis |> 
    group_by(ChmclNm) |> 
    nest() |>
    mutate(data = map(data, ~ .x %>% select(-cncntrt, -lft_cns, -id, -id.1, -Year))) |>
    pull(data)
  
  # convert to matrix, but retain dimensions
  pesticide_covariates <- pesticide_covariates |> 
    map(~ as.matrix(.x, is.matrix = TRUE))
  
  # 5) A list of locs, each element is a matrix of the locations

  locs <- data_analysis |> 
    group_by(ChmclNm) |> 
    nest() |>
    mutate(data = map(data, ~ .x %>% select(id, Year))) |>
    pull(data)
  
  
  # Update the locs list
  locs <- locs |> 
    map(~ {
      coords <- st_coordinates(.x)
      mat <- as.matrix(data.frame(
        X = coords[, "X"],
        Y = coords[, "Y"],
        Year = .x$Year
      ))
      return(mat)
    })
  

  # 6) Apanasovich is TRUE
  Apanasovich = TRUE
  
  # 7) scaling = c(1,1,2) -> maybe needs to be list? could also do c(1,2,3)
  scaling = c(1,1,2)
  
  # Fit the model
  
  pesticide_mvm <-  new("MultivariateVecchiaModel", n_neighbors = 10)
  pesticide_mvm <- prestogp_fit(pesticide_mvm, Y = pesticide_outcomes, lod = pesticide_lod, 
                           impute.y = Impute.y, X = pesticide_covariates, locs = locs, 
                           apanasovich = Apanasovich, scaling = scaling)
  return(soil.mvm)
  
}



#' Calculate NASS data for specified HUC levels
#'
#' This function calculates NASS (National Agricultural Statistics Service) data for specified HUC (Hydrologic Unit Code) levels.
#'
#' @param nass_path The path to the NASS data directory.
#' @param wbd_path The path to the WBD (Watershed Boundary Dataset) data directory.
#' @param huc_level A numeric vector specifying the HUC levels to calculate data for.
#' One of 8, 10, or 12.
#' @param n_cores The number of CPU cores to use for parallel processing.
#'
#' @returns A list of extracted NASS data for each HUC level.
#'
#' @examples
#' calc_nass(nass_path = "input/USDA_NASS",
#'           wbd_path = "input/WBD",
#'           huc_level = c(8, 10, 12),
#'           out_path = "output",
#'           n_cores = 1)
calc_nass <- function(
  nass_path = "input/USDA_NASS",
  wbd_path = "input/WBD-National/WBD_National_GDB.gdb",
  huc_level = 12,
  n_cores = 1
) {
  huc_level <- match.arg(as.character(huc_level), c("8", "10", "12"))
  # read in the NASS data
  list_nass <- list_raster(nass_path, recursive = TRUE, full.names = TRUE)
  # read in the WBD data
  wbdpath <- file.path(wbd_path)
  # file based strategy
  ext_mainland <- sf::st_as_text(
    sf::st_as_sfc(sf::st_bbox(c(xmin=-126, xmax=-66, ymin=22, ymax=52)))
  )
  sf::sf_use_s2(FALSE)
  layer_name <- sprintf("WBDHU%s", huc_level)
  field_name <- sprintf("huc%s", huc_level)

  huc <- sf::st_read(
    wbdpath, layer = layer_name, wkt_filter = ext_mainland
  )
  huc <- sf::st_transform(huc, "EPSG:5070") |>
    dplyr::select(dplyr::all_of(field_name))
  huc$huc_split <- substr(unlist(huc[[field_name]]), 1, 4)

  future::plan(future::multisession, workers = n_cores)

  # approach 1: Map par_hierarchy
  # extracted <-
  #   Map(
  #     function(x) {
  #       chopin::par_hierarchy(
  #         regions = huc,
  #         split_level = "huc_split",
  #         fun_dist = chopin::extract_at_poly,
  #         polys = huc,
  #         surf = x,
  #         id = field_name,
  #         func = "frac",
  #         max_cells = 3e+07
  #       )
  #     },
  #     list_nass
  #   )
  # approach 2: file based
  extracted <-
    chopin::par_multirasters(
      filenames = list_nass,
      fun_dist = chopin::extract_at_poly,
      polys = huc,
      id = field_name,
      func = "frac",
      max_cells = 3e+07
    )
  future::plan(future::sequential)
  return(extracted)
}


#' Calculate TWI data for specified HUC levels
#'  
#' This function calculates TWI (Topographic Wetness Index) data for specified HUC (Hydrologic Unit Code) levels.
#'  
#' @param twi_file The path to the TWI TIFF data.
#' @param wbd_path The path to the WBD (Watershed Boundary Dataset) data directory.
#' @param huc_level A numeric vector specifying the HUC levels to calculate data for.
#' One of 8, 10, or 12.
#' @param n_cores The number of CPU cores to use for parallel processing.
#'  
#' @returns A list of extracted TWI data for each HUC level.
#'  
#'  
#' @examples
#' calc_twi(twi_path = "input/TWI",
#'         wbd_path = "input/WBD",
#'        huc_level = c(8, 10, 12),
#'       out_path = "output",
#'     n_cores = 1)
#' 
#' 
calc_twi <- function(
  twi_file = "input/TWI/CONUS_TWI_epsg5072_30m_unmasked.tif",
  wbd_path = "input/WBD-National/WBD_National_GDB.gdb",
  huc_level = 12,
  n_cores = 1
) {
  huc_level <- match.arg(as.character(huc_level), c("8", "10", "12"))
  # read in the WBD data
  wbdpath <- file.path(wbd_path)
  # file based strategy
  ext_mainland <- sf::st_as_text(
    sf::st_as_sfc(sf::st_bbox(c(xmin=-126, xmax=-66, ymin=22, ymax=52)))
  )
  sf::sf_use_s2(FALSE)
  layer_name <- sprintf("WBDHU%s", huc_level)
  field_name <- sprintf("huc%s", huc_level)

  huc <- sf::st_read(
    wbdpath, layer = layer_name, wkt_filter = ext_mainland
  )
  huc <- sf::st_transform(huc, "EPSG:5070") |>
    dplyr::select(dplyr::all_of(field_name))
  huc$huc_split <- substr(unlist(huc[[field_name]]), 1, 4)

  future::plan(future::multisession, workers = n_cores)

  # approach 1: Map par_hierarchy
  extracted <-
    chopin::par_hierarchy(
      regions = huc,
      split_level = "huc_split",
      fun_dist = chopin::extract_at_poly,
      polys = huc,
      surf = twi_file,
      id = field_name,
      func = "mean",
      max_cells = 3e+07
    )
  future::plan(future::sequential)
  return(extracted)
}



