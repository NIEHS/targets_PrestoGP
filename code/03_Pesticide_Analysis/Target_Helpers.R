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


# Save data as a qs file
#' save_as_qs
#'
#' @param dataRDS 
#'
#' @return
#' @export
#'
#' @examples
save_as_qs <- function(dataRDS){
  
  # Save the data as a qs file
  qs::qsave(dataRDS, "myfile.qs")

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


# Extract the covariates

get_covariates <- function(data){
  
  # Extract the covariates
  covariates <- data %>% 
    select(-c("pesticide", "date", "site", "lat", "long", "elev", "soiltype", "soilchem"))
  
  # Return the covariates
  return(covariates)
  
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
exploratory_analysis <- function(data){
  
  # Create a correlation matrix
  cor_matrix <- cor(data)
  
  # Create a scatterplot matrix
  scatterplot_matrix <- ggplot2::ggpairs(data)
  
  # Return the correlation matrix and scatterplot matrix
  return(list(cor_matrix, scatterplot_matrix))
  
}


read_pesticide_data <- function(COMPUTE_MODE = 1){
  
  path_base <-
    ifelse(COMPUTE_MODE == 1,
           "/Volumes/SET/Projects/PrestoGP_Pesticides/input/",
           ifelse(COMPUTE_MODE == 2,
                  "/ddn/gs1/group/set/Projects/PrestoGP_Pesticides/input/",
                  ifelse(COMPUTE_MODE == 3,
                         "/opt/", stop("COMPUTE_MODE should be one of 1, 2, or 3.\n")
                  )
           )
    )
  
  azo <- sf::read_sf(paste0(path_base, "data_process/data_AZO_watershed_huc_join.shp"))

  
  # Return the data
  return(azo)
  
}
