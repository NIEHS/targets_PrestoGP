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





#' filter_NA
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
filter_NA <- function(data){
  
  
  data <- data %>% 
    select(-starts_with("soilchem"))
  

}


# Create a function for exploratory analysis of the covariates
#' exploratory_analysis
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
unique_vals <- function(data){
  
# Create a logical index for all columns after the 41st column
  
  idx <- which(colnames(data) %in% colnames(data)[41:ncol(data)])
  
  n_unique <- data |> 
    dplyr::select(idx) |>
    dplyr::summarise_all(n_distinct) |> 
    dplyr::mutate_all(~ . / nrow(data) * 100)  
  
  # Add a generic value for the first 41 columns and prepend it to the data frame
  nu <- c(rep(100,41), t(n_unique))

  return(n_unique)
}


drop_bad_cols <- function(data, idx, threshold){
  # Subset `data` by column where idx > threshold
  data <- data |>
    select(which(idx > threshold)) 
  

  
  return(data)
}


#' plot_cv_map
#'
#' @param pesticide 
#' @param kfold_cv 
#'
#' @return p ggplot object
#' @export
#'
#' @examples
plot_cv_map <- function(pesticide) {
  
  # Create a ggplot object
  p <- ggplot() +
    geom_sf(data = pesticide, aes(color = kfolds )) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
  
}
