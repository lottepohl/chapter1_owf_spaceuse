
# description -------------------------------------------------------------

# script to query acoustic detection data and save some of them

# To Do -------------------------------------------------------------------



# workspace ---------------------------------------------------------------

library(dplyr)
# library(EMODnetWFS)
# library(emodnet.wfs)
library(pak)
# pak::pak("inbo/etn")
library(etn)
library(sf)
library(lubridate)

source("R/helpers.R")


# paths -------------------------------------------------------------------

path_spatial <- "data/spatial"
path_data_raw <- "data_raw"
path_data <- "data"
path_plots <- "docs/plots"

# load_data ---------------------------------------------------------------

BENL <- load_data("BENL", path_spatial)

# deployments -------------------------------------------------------------

deployments_raw <-  etn::get_acoustic_deployments()
save_data(deployments_raw, folder = path_data_raw)

deployments <-
  deployments_raw %>%
  # remove deployments with missing lat or lon
  dplyr::filter(!is.na(deploy_latitude) | !is.na(deploy_longitude)) %>%
  # transform lat/lon into geom
  dplyr::mutate(lat = deploy_latitude,
                lon = deploy_longitude) %>%
  sf::st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
  # make new col with boolean that is TRUE if inside OWF
  dplyr::mutate(inside_OWF = lengths(sf::st_within(geometry, windfarms_polygons_prod)) > 0) %>%
  sf::st_join(windfarms_polygons_prod, join = sf::st_within) %>% 
  # remove duplicated deployments
  dplyr::distinct(deployment_id, .keep_all = T) %>%
  # fill recover_date_time with current date 
  dplyr::mutate(recover_date_time = dplyr::if_else(recover_date_time %>% is.na(), Sys.time(), recover_date_time))

save_data(deployments, folder = path_data)

# make summary per receiver station/deployment location
deployments_station_sum <-
  deployments %>%
  # get the duration of each deployment
  dplyr::mutate(deploy_duration = difftime(recover_date_time, deploy_date_time, units = "days")) %>%
  # sf::st_drop_geometry() %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(deploy_latitude = mean(deploy_latitude),
                   deploy_longitude = mean(deploy_longitude),
                   geometry = st_centroid(st_combine(geometry)),
                   deploy_duration_total = sum(deploy_duration),
                   deployment_ids = paste0(deployment_id, collapse = ", "),
                   inside_OWF = paste0(inside_OWF %>% unique(), collapse = ", "),
                   country = paste0(country %>% unique(), collapse = ", "))
save_data(deployments_station_sum, folder = path_data)

# get station names from inside Belgian OWF
deployment_stations_BE_OWF <-
  deployments_station_sum %>%
  # sf::st_drop_geometry() %>%
  dplyr::filter(country == "Belgium" & inside_OWF == TRUE) %>%
  # median deploy duration
  dplyr::mutate(median_deploy_duration = median(deploy_duration_total),
  # distance to coast
                dist_to_coast_km = as.numeric(sf::st_distance(geometry, BENL)[, 1]) / 1000 )

save_data(deployment_stations_BE_OWF, path_data)


# animal projects ---------------------------------------------------------

etn_animal_proj <- etn::get_animal_projects()

animal_proj_keywords <- c("elasmo","elasmobranch", "seabass", "mackerel", "OWF", "VLIZ", "shark", "ray")

etn_anim_proj_key <- 
  etn_animal_proj %>%
  dplyr::filter(grepl(paste(animal_proj_keywords, collapse = "|"), etn_animal_proj$project_code, ignore.case = T))

save_data(etn_anim_proj_key, path_data)


# animals -----------------------------------------------------------------

animals_anim_proj <-
  etn::get_animals(animal_project_code = etn_anim_proj_key$project_code)

save_data(animals_anim_proj, path_data)

# detections --------------------------------------------------------------


## from 10 longest deployed stations ---------------------------------------

# detections from the 10 stations that had the longest deploy period
detections_OWF_longestdeployments <-
  etn::get_acoustic_detections(station_name = deployment_stations_BE_OWF %>% st_drop_geometry() %>% slice_max(n = 10, order_by = deploy_duration_total) %>% dplyr::select(station_name) %>% pull())

save_data(detections_OWF_longestdeployments, path_data)

detections_BE_OWF_10longestdeployments_sum <-
detections_OWF_longestdeployments %>%
  dplyr::filter(!scientific_name %in% c("Built-in", NA)) %>%
  dplyr::mutate(month = floor_date(date_time, unit = "month")) %>%
  dplyr::group_by(animal_id, station_name, month) %>%
  dplyr::summarise(n_det = dplyr::n(),
                   scientific_name = scientific_name %>% unique(),
                   animal_proj_code = paste0(animal_project_code %>% unique(), collapse = ",")) %>%
  dplyr::left_join(deployment_stations_BE_OWF %>% dplyr::select(station_name, dist_to_coast_km, deploy_duration_total))
save_data(detections_BE_OWF_10longestdeployments_sum, path_data)

detections_BE_OWF_10longestdeployments_sum_animal <-
  detections_OWF_longestdeployments %>%
  dplyr::filter(!scientific_name %in% c(NA, "Built-in")) %>%
  dplyr::group_by(scientific_name, animal_id) %>%
  dplyr::summarise(n_det = dplyr::n(),
                   first_det = min(date_time),
                   last_det = max(date_time),
                   n_stations = station_name %>% unique() %>% length(),
                   stations = paste0(station_name %>% unique(), collapse = ","))
save_data(detections_BE_OWF_10longestdeployments_sum_animal, path_data)

## smoothhound -------------------------------------------------------------

detections_BE_OWF_smoothhound <- etn::get_acoustic_detections(scientific_name = "Mustelus asterias",
                                                              station_name = deployment_stations_BE_OWF$station_name)
save_data(detections_BE_OWF_smoothhound, path_data)

detections_BE_OWF_smoothhound_sum <-
  detections_BE_OWF_smoothhound %>%
  dplyr::mutate(month = floor_date(date_time, unit = "month")) %>%
  dplyr::group_by(animal_id, station_name, month) %>%
  dplyr::summarise(n_det = dplyr::n(),
                   animal_proj_code = paste0(animal_project_code %>% unique(), collapse = ",")) %>%
  dplyr::left_join(deployment_stations_BE_OWF %>% dplyr::select(station_name, dist_to_coast_km, deploy_duration_total))

save_data(detections_BE_OWF_smoothhound_sum, path_data)

## seabass -------------------------------------------------------------
detections_BE_OWF_seabass <- etn::get_acoustic_detections(scientific_name = "Dicentrarchus labrax",
                                                          station_name = deployment_stations_BE_OWF$station_name)
save_data(detections_BE_OWF_seabass, path_data)

detections_BE_OWF_seabass_sum <-
  detections_BE_OWF_seabass %>%
  dplyr::mutate(month = floor_date(date_time, unit = "month")) %>%
    dplyr::group_by(animal_id, station_name, month) %>%
    dplyr::summarise(n_det = dplyr::n(),
                     animal_proj_code = paste0(animal_project_code %>% unique(), collapse = ",")) %>%
    dplyr::left_join(deployment_stations_BE_OWF %>% dplyr::select(station_name, dist_to_coast_km, deploy_duration_total))

save_data(detections_BE_OWF_seabass_sum, path_data)

## catshark -------------------------------------------------------------

detections_BE_OWF_catshark <- etn::get_acoustic_detections(scientific_name = "Scyliorhinus canicula",
                                                           station_name = deployment_stations_BE_OWF$station_name)
save_data(detections_BE_OWF_catshark, path_data)


## mackerel -------------------------------------------------------------

detections_BE_OWF_mackerel <- etn::get_acoustic_detections(scientific_name = "Scomber scombrus",
                                                           station_name = deployment_stations_BE_OWF$station_name)
save_data(detections_BE_OWF_mackerel, path_data)


## thornback -------------------------------------------------------------

detections_BE_OWF_thornbackray <- etn::get_acoustic_detections(scientific_name = "Raja clavata",
                                                               station_name = deployment_stations_BE_OWF$station_name)
save_data(detections_BE_OWF_thornbackray, path_data)

detections_BE_OWF_thornbackray_sum <-
  detections_BE_OWF_thornbackray %>%
  dplyr::mutate(month = floor_date(date_time, unit = "month")) %>%
  dplyr::group_by(animal_id, station_name, month) %>%
  dplyr::summarise(n_det = dplyr::n(),
                   animal_proj_code = paste0(animal_project_code %>% unique(), collapse = ",")) %>%
  dplyr::left_join(deployment_stations_BE_OWF %>% dplyr::select(station_name, dist_to_coast_km, deploy_duration_total))

save_data(detections_BE_OWF_thornbackray_sum, path_data)

## cod -------------------------------------------------------------

detections_BE_OWF_cod <- etn::get_acoustic_detections(scientific_name = "Gadus morhua",
                                                      station_name = deployment_stations_BE_OWF$station_name)
# save_data(detections_BE_OWF_cod, path_data)

detections_BE_OWF_cod_sum <-
  detections_BE_OWF_cod %>%
  dplyr::mutate(month = floor_date(date_time, unit = "month")) %>%
  dplyr::group_by(animal_id, station_name, month) %>%
  dplyr::summarise(n_det = dplyr::n(),
                   animal_proj_code = paste0(animal_project_code %>% unique(), collapse = ",")) %>%
  dplyr::left_join(deployment_stations_BE_OWF %>% dplyr::select(station_name, dist_to_coast_km, deploy_duration_total))

save_data(detections_BE_OWF_cod_sum, path_data)



