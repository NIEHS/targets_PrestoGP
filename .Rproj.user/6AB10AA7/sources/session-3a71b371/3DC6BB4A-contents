# Name, NWIS Code, CASN
# CHLOROTRIAZINES
# Atrazine, 39632
# Desethylatrazine, 04040, 6190-65-4
# Desisopropylatrazine, 04038, 1007-28-9
# Diaminochloroatrazine (2-Chloro-4,6-diamino-s-triazine),04039 , 3397-62-4 [ZERO]
# Propazine,38535, 139-40-2. [ZERO ]
# Simazine, 04035, 122-34-9



# CHLOROACETANLIDES
# Alachlor, 46342, 15972-60-8 [ZERO]
# Alachlor Sulfonic Acid (ESA), 50009, 142363-53-9
# Alachlor oxanilic acid (OA), 61031, 	171262-17-2
# Acetochlor, 49260, 34256-82-1 [ZERO]
# Acetochlor sulfonic acid, 61029, 187022-11-3
# Acetochlor oxanilic acid, 61030, 194992-44-4
# Butachlor, 04026, 23184-66-9
# Butachlor sulfonic acid (ESA), 68171, 187022-12-4
# Metolachlor, 39415 (alt. 50289), 51218-45-2
# Metolachlor OA, 61031, 171262-17-2
# Metolachlor ESA, 61043, 171118-09-5

library(dataRetrieval)
library(dplyr)
library(beepr)
library(lubridate)
library(tidyverse)
library(data.table)
param.chlorotriazines <- c("39632","04040","04038","04039","38535","04035")
startDate <- "2000-01-01"
endDate <- "2020-12-31"

state.list <- state.abb[c(-2,-11)]

state.fun.AZO <- function(x){ 
  print(x)
  temp <- whatWQPdata(statecode = state.list[x], 
                parameterCd=param.chlorotriazines) 
  
  temp <- temp[temp$MonitoringLocationTypeName=="Well",]
  
  if (nrow(temp)>0){
  site.info <- str_subset(temp$MonitoringLocationIdentifier,"(?<=USGS-)\\d+") %>% 
          str_extract("(?<=USGS-)\\d+")   %>% 
    readNWISsite() %>% 
    select(c(site_no,well_depth_va)) 
  site.info$MonitoringLocationIdentifier <- paste0("USGS-",site.info$site_no)
  data.chlorotriazines <- readWQPqw(temp$MonitoringLocationIdentifier, param.chlorotriazines, 
                                    startDate = "2000-01-01", endDate = "2020-12-31") 
  
  result <- left_join(data.chlorotriazines,temp,by = "MonitoringLocationIdentifier") %>% 
    left_join(site.info,by = "MonitoringLocationIdentifier")
  
  return(result) 
  }
}

data.AZO <- lapply(1:48, state.fun.AZO)
beep(sound = 2)

data.AZO.final<- do.call(rbind.data.frame, data.AZO)
beep(sound = 2)

censored_text <- c("Not Detected",
                   "Non-Detect",
                   "Non Detect",
                   "Detected Not Quantified",
                   "Below Quantification Limit")

data.AZO.censored <- data.AZO.final %>% 
  mutate(left_censored = grepl(paste(censored_text, collapse="|"), 
                               ResultDetectionConditionText,
                               ignore.case = TRUE),
         Year = lubridate::year(ActivityStartDate)) %>%
  select(site_no = MonitoringLocationIdentifier,
         startDate = ActivityStartDate,
         Year,
         parm_cd = USGSPCode,
         ChemicalName = CharacteristicName,
         left_censored,
         lon,lat,well_depth = well_depth_va,
         CountyName,StateName,
         Units = ResultMeasure.MeasureUnitCode,
         result_va = ResultMeasureValue,
         detection_level = DetectionQuantitationLimitMeasure.MeasureValue,
         dl_units = DetectionQuantitationLimitMeasure.MeasureUnitCode) %>% 
  arrange(startDate, parm_cd)


# We first need to resolve the replicates on the same day
# Then we calculate averages for each year
# Concentration average- max if all censored, mean if all observed, 
# mean of observed if mix
# left-censoring flag - TRUE if all TRUE, FALSE if all FALSE, FALSE if mix
# Other columns are the first value (shouldn't matter what the function is)
data.AZO.daily.avg <- data.AZO.censored %>% 
  group_by(ChemicalName,site_no,startDate) %>%
  mutate("id" = cur_group_id(),"nsamples" = n()) %>% 
  summarise(concentration = fifelse(all(left_censored),max(detection_level),
                                    fifelse(all(!left_censored),mean(result_va),
                                            mean(result_va[!left_censored]),
                                    )),
            left_censored = fifelse(all(left_censored),TRUE,
                                    fifelse(all(!left_censored),FALSE,
                                            FALSE,
                                    )),
            Longitude = first(lon),
            Latitude= first(lat),
            ChemicalName = first(ChemicalName),
            site_no = first(site_no),
            parm_cd = first(parm_cd),
            Units = first(coalesce(Units,dl_units)),
            CountyName = first(CountyName),
            StateName = first(StateName),
            well_depth = first(well_depth),
            Year = first(Year),
            id = first(id),
            nsamples = sum(nsamples))

# Second,  we average by year

data.AZO.year.avg <- data.AZO.daily.avg %>% 
  group_by(ChemicalName,site_no,Year) %>%
  mutate("id" = cur_group_id()) %>% 
  summarise(concentration = fifelse(all(left_censored),max(concentration),
                                    fifelse(all(!left_censored),mean(concentration),
                                            mean(concentration[!left_censored]),
                                    )),
            left_censored = fifelse(all(left_censored),TRUE,
                                    fifelse(all(!left_censored),FALSE,
                                            FALSE,
                                    )),
            Longitude = first(Longitude),
            Latitude= first(Latitude),
            ChemicalName = first(ChemicalName),
            site_no = first(site_no),
            parm_cd = first(parm_cd),
            Units = first(Units),
            CountyName = first(CountyName),
            StateName = first(StateName),
            well_depth = first(well_depth),
            Year = first(Year),
            id = first(id),
            nsamples = sum(nsamples)) %>% 
  arrange(Year, site_no,parm_cd)

save(data.AZO.year.avg,file = "/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_AZO_year_avg_20221120.RData")

# End AZO

# Begin ACE
param.chloroacetanilides <- c("46342","50009","61031","49260","61029","61030","04026","68171","39415","61043")

state.fun.ACE <- function(x){ 
  print(x)
  temp <- whatWQPdata(statecode = state.list[x], 
                      parameterCd=param.chloroacetanilides) 
  
  temp <- temp[temp$MonitoringLocationTypeName=="Well",]
  
  if (nrow(temp)>0){
    site.info <- str_subset(temp$MonitoringLocationIdentifier,"(?<=USGS-)\\d+") %>% 
      str_extract("(?<=USGS-)\\d+")   %>% 
      readNWISsite() %>% 
      select(c(site_no,well_depth_va)) 
    site.info$MonitoringLocationIdentifier <- paste0("USGS-",site.info$site_no)
    data.chloroacetanilides <- readWQPqw(temp$MonitoringLocationIdentifier, param.chloroacetanilides, 
                                      startDate = "2000-01-01", endDate = "2020-12-31") 
    
    result <- left_join(data.chloroacetanilides,temp,by = "MonitoringLocationIdentifier") %>% 
      left_join(site.info,by = "MonitoringLocationIdentifier")
    
    return(result) 
  }
}

data.ACE <- lapply(1:48, state.fun.ACE)
beep(sound = 2)

data.ACE.final<- do.call(rbind.data.frame, data.ACE)
beep(sound = 2)

censored_text <- c("Not Detected",
                   "Non-Detect",
                   "Non Detect",
                   "Detected Not Quantified",
                   "Below Quantification Limit")

data.ACE.censored <- data.ACE.final %>% 
  mutate(left_censored = grepl(paste(censored_text, collapse="|"), 
                               ResultDetectionConditionText,
                               ignore.case = TRUE),
         Year = lubridate::year(ActivityStartDate)) %>%
  select(site_no = MonitoringLocationIdentifier,
         startDate = ActivityStartDate,
         Year,
         parm_cd = USGSPCode,
         ChemicalName = CharacteristicName,
         left_censored,
         lon,lat,well_depth = well_depth_va,
         CountyName,StateName,
         Units = ResultMeasure.MeasureUnitCode,
         result_va = ResultMeasureValue,
         detection_level = DetectionQuantitationLimitMeasure.MeasureValue,
         dl_units = DetectionQuantitationLimitMeasure.MeasureUnitCode) %>% 
  arrange(startDate, parm_cd)


# We first need to resolve the replicates on the same day
# Then we calculate averages for each year
# Concentration average- max if all censored, mean if all observed, 
# mean of observed if mix
# left-censoring flag - TRUE if all TRUE, FALSE if all FALSE, FALSE if mix
# Other columns are the first value (shouldn't matter what the function is)
data.ACE.daily.avg <- data.ACE.censored %>% 
  group_by(ChemicalName,site_no,startDate) %>%
  mutate("id" = cur_group_id(),"nsamples" = n()) %>% 
  summarise(concentration = fifelse(all(left_censored),max(detection_level),
                                    fifelse(all(!left_censored),mean(result_va),
                                            mean(result_va[!left_censored]),
                                    )),
            left_censored = fifelse(all(left_censored),TRUE,
                                    fifelse(all(!left_censored),FALSE,
                                            FALSE,
                                    )),
            Longitude = first(lon),
            Latitude= first(lat),
            ChemicalName = first(ChemicalName),
            site_no = first(site_no),
            parm_cd = first(parm_cd),
            Units = first(coalesce(Units,dl_units)),
            CountyName = first(CountyName),
            StateName = first(StateName),
            well_depth = first(well_depth),
            Year = first(Year),
            id = first(id),
            nsamples = sum(nsamples))

# Second,  we average by year

data.ACE.year.avg <- data.ACE.daily.avg %>% 
  group_by(ChemicalName,site_no,Year) %>%
  mutate("id" = cur_group_id()) %>% 
  summarise(concentration = fifelse(all(left_censored),max(concentration),
                                    fifelse(all(!left_censored),mean(concentration),
                                            mean(concentration[!left_censored]),
                                    )),
            left_censored = fifelse(all(left_censored),TRUE,
                                    fifelse(all(!left_censored),FALSE,
                                            FALSE,
                                    )),
            Longitude = first(Longitude),
            Latitude= first(Latitude),
            ChemicalName = first(ChemicalName),
            site_no = first(site_no),
            parm_cd = first(parm_cd),
            Units = first(Units),
            CountyName = first(CountyName),
            StateName = first(StateName),
            well_depth = first(well_depth),
            Year = first(Year),
            id = first(id),
            nsamples = sum(nsamples)) %>% 
  arrange(Year, site_no,parm_cd)

save(data.ACE.year.avg,file = "/Volumes/messierkp/Projects/CensoredGP/WaterData-Pesticides/nwis_triazines_alachlors/data_ACE_year_avg_20221120.RData")

# End ACE
# 1) Determine final detection status
# 2) Calculate annual averages
# 3) Plot percent detect by year


                                                                                                           


