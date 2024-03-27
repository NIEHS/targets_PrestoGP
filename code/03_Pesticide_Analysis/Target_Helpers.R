# TARGETS help functions for PrestoGP Pesticide Pipeline Analysis 




#' set_local_data_path
#'
#' @param COMPUTE_MODE 
#'
#' @return
#' @export
#'
#' @examples
set_local_data_path <- function(COMPUTE_MODE = 1){
  
    ifelse(COMPUTE_MODE == 1,
           "/Volumes/SET/Projects/PrestoGP_Pesticides/output/",
           ifelse(COMPUTE_MODE == 2,
                  "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/output/",
                  ifelse(COMPUTE_MODE == 3,
                         "/opt/",
                         ifelse(COMPUTE_MODE == 4,
                                "/tmp/AZO/",
                                stop("COMPUTE_MODE should be one of 1, 2, 3, or 4.\n")
                         )
                  )
           )
    )
}


#' read_data
#'
#' @param path 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
read_data <- function(path, pesticide_data = pesticide_data){
  
  # Read in the data
  data <- qs::qread(paste0(path, pesticide_data, ".qs"))
  
  # Return the data
  return(data)
}



partition_datasets <- function(data_sf) {
  
  # drop the soil chemistry covariates (not needed and too difficult to work with)
  data_sf <- data_sf %>% 
    select(-starts_with("soilchem"))
  
  
  # Partition the data into (1) outcome (2) covariates/features (3) ancillary info
  outcome <- data_sf |> 
    dplyr::select(c("id","ChmclNm","Year","cncntrt","lft_cns")) 
  
  covariates <- data_sf |> 
    dplyr::select(c("id"),
                  starts_with("nass"),
                  starts_with("olm"),
                  starts_with("aquifer"),
                  starts_with("tclim"),
                  starts_with("prism"),
                  contains("geology_unit_type"),
                  starts_with("pest")
    )
  
  ancillary <- data_sf |> 
    dplyr::select("id","site_no","parm_cd","Units","ContyNm","StateNm","wll_dpt",
                  "nsampls","objectid","tnmid","metasource","sourcedata","sourceorig",
                  "sourcefeat","loaddate","referenceg","areaacres","areasqkm",
                  "states","huc12","huc10","huc08","huc06","huc04","huc02",
                  "name","hutype","humod","tohuc","noncontrib","noncontr_1","globalid",
                  "shape_Leng","shape_Area")
  
  return(list(outcome, covariates, ancillary))
}



#  Covariate preparation includes dropping continuous covariates that don't have enough unique values - and creating dummy variables for categorical covariates
#' covariate_prep
#'
#' @param data Partitioned data with 3 lists: outcome, covariates, ancillary
#' @param threshold Threshold for the number of unique values in a numeric covariate
#' @return
#' @export
#'
#' @examples
covariate_prep <- function(data, threshold = 1e-5){
  
  # Get the covariates from the partitioned list
  data_covariates <- data[[2]]

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
  covariates_processed <- covariates_num_filter |>
    cbind(covariates_factor_dummy) |>
    select(-"geometry.1") 

  
  return(list(data[[1]], covariates_processed, data[[3]]))
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


#' plot_cv_map
#'
#' @param pesticide 
#'
#' @return p ggplot object
#' @export
#'
#' @examples
plot_single_map <- function(pesticide) {
  

  # Create a ggplot object
  p <- ggplot() +
    geom_sf(data = pesticide) +
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
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
fit_lasso <- function(data) {
  
  # Subset the outcome and covariates from the data
  pest_data <- data |>
    select(cncntrt, 40:ncol(data) -1)
  
  pest_data <- st_drop_geometry(pest_data)
  
  # Fit using the linear_model function in Parsnip
  lasso <- linear_reg() %>%
    set_engine("glmnet") %>%
    set_mode("regression") %>%
    fit(log10(cncntrt) ~ ., data = pest_data)

  return(lasso)
  
}





#' plot_exploratory_covariates
#'
#' @param data 
#'
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
    facet_wrap(~ Year) +
    scale_color_viridis_c(option = "A", trans = scales::pseudo_log_trans(sigma = 0.005)) +
    geom_sf(data = states, fill = NA, size=0.15) +
    theme_minimal() +
    theme(legend.position = "right") +
    ggtitle(data$ChmclNm) 
  
  return(p)
  
}