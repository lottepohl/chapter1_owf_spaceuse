# script to check fish detections around OWF receivers in the Belgian North Sea
# 2026-03-03

# workspace ---------------------------------------------------------------

library(dplyr)
library(magrittr)
library(sf)
library(purrr)
library(lubridate)
library(mregions2)
library(tidyr)
library(mapview)
library(ggplot2)
library(plotly)
library(giscoR)
library(leaflet)
library(mgcv)
# library(EMODnetWFS)
# library(emodnet.wfs)
# pak::pak("inbo/etn")
library(etn)

## connect to ETN R package
if(packageVersion('etn') >= "2.3.0"){
  # if 'new' version of etn R package: read .Renviron directly
  readRenviron("../.Renviron")
  Sys.getenv("ETN_USER")
  Sys.getenv("ETN_PWD")
}else{
  # if 'old' version: create con object
  con <- etn::connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))}

spatial_layer_path <- file.path(getwd(), "02_data/01_spatial_layers")

# get metadata ------------------------------------------------------------


# ## spatial objects
# BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry() # Belgian EEZ
# Scheldt_Estuary <-  mregions2::gaz_search(4812) %>% mregions2::gaz_geometry()
# BENL <- mregions2::gaz_search(c(14, 15)) %>% mregions2::gaz_geometry()
# #BE_OWF <- sf::st_read(dsn = file.path(getwd(), "etn-projects/01_maps/layers/OWF_BE_safetyzones.gpkg")) %>% sf::st_transform(4326)
# countries <- giscoR::gisco_get_countries(year = "2024", resolution = 03, region = "Europe")
# 
# ## OWF boundaries
# wfs_human <- emodnet_init_wfs_client(service = "human_activities")
# #wfs_human %>% emodnet_get_wfs_info() %>% View()
# layers_windfarms <- wfs_human %>% emodnet_get_layers(layers = c("windfarms", "windfarmspoly"), crs = 4326) 
# windfarms_polygons <- layers_windfarms %>% purrr::pluck("windfarmspoly")
# windfarms_points <- layers_windfarms %>% purrr::pluck("windfarms")
# 
# shipwrecks <- 
#   readr::read_delim(file.path(spatial_layer_path,"wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
#   dplyr::select(Easting, Northing, `Gezonken op`, Code, Bouwjaar) %>%
#   sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) %>%
#   sf::st_transform(4326) %>%
#   dplyr::bind_cols(
#     readr::read_delim(file.path(spatial_layer_path,"wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
#       dplyr::select(Easting, Northing) %>%
#       sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) %>%
#       sf::st_transform(4326) %>%
#       sf::st_coordinates() %>% # Extract coordinates as a matrix %>%
#       tidyr::as_tibble() %>% # Convert coordinates matrix to tibble
#       dplyr::rename(lon = X, lat = Y))

# export_cables <- read_sf(file.path(spatial_layer_path, "Export_Cables.shp"))
# # https://spatial.naturalsciences.be/geoserver/od_nature/wms?service=WMS&version=1.1.0&request=GetMap&layers=od_nature%3AMUMM_windmill_locations_ETRS89&bbox=3817777.419943257%2C3176768.2180007333%2C3841725.2737183524%2C3205763.9329526643&width=634&height=768&srs=EPSG%3A3035&styles=&format=application/openlayers
# sf::write_sf(export_cables, file.path(spatial_layer_path, "export_cables.gpkg"))

# load metadata --------------------------------------------------
## to work in a spatial context, transform (back) to https://epsg.io/32631 for a meter-based crs
# TODO: load all spatial layers in one go
shipwrecks <- sf::read_sf(file.path(spatial_layer_path, "shipwrecks.gpkg"))
windfarms_polygons <- sf::read_sf(file.path(spatial_layer_path, "windfarms_polygons.gpkg"))
windfarms_points <- sf::read_sf(file.path(spatial_layer_path, "windfarms_points.gpkg"))
substations <- 
  sf::read_sf(file.path(spatial_layer_path, "Substations.gpkg")) %>%
  sf::st_transform(crs = 4326)
wtg_BE <- sf::read_sf(file.path(spatial_layer_path, "windfarms_points_BE.gpkg")) %>%
  sf::st_transform(crs = 4326)
infield_cables <- 
  sf::read_sf(file.path(spatial_layer_path, "infield_cables.gpkg")) %>%
  sf::st_transform(crs = 4326)
export_cables <- 
  sf::read_sf(file.path(spatial_layer_path, "export_cables.gpkg")) %>%
  sf::st_transform(crs = 4326)

## leaflet map (interactive)
map_leaflet <-
leaflet() %>%
  setView(lng = 3, lat = 51.5, zoom = 7) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://ows.emodnet-bathymetry.eu/wms",
    layers  = "emodnet:contours",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>%
  addWMSTiles(
    baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
    layers = "eez",   # replace with your layer name
    options = WMSTileOptions(
      format = "image/png",
      styles = "Polygons_greyoutline",
      transparent = TRUE)
  ) %>%
  addPolylines(data = infield_cables, color = "black", weight = 1.5) %>%
  addPolylines(data = export_cables, color = "black", weight = 3) %>%
  addCircleMarkers(data = shipwrecks, color = "brown", fillOpacity = 1, radius = 2, weight = 0, opacity = 0.5) %>%
  addPolygons(data = windfarms_polygons,
              fillOpacity = 0, 
              color = "red",
              weight = 1,
              popup = ~paste0("status: ", status)) %>%
  addCircles(data = wtg_BE, color = "darkblue", fillOpacity = 1, radius = 50, weight = 0, opacity = 0.75,
                   label = ~paste0("Concession: ", concession, ", Code: ", code)) %>%
  addCircles(data = substations, color = "darkblue", fillOpacity = 0, radius = 75, weight = 0.75, opacity = 1,
                   label = ~paste0("Concession: ", concession)) %>%
  addScaleBar(position = "bottomright")
  
map_leaflet
## ggplot map (static)


map <-
  ggplot() +
  geom_sf(data = BPNS, colour = "darkgrey", fill = "transparent", linewidth = 0.3) +
  geom_sf(data = shipwrecks, fill = "brown", colour = 'brown', alpha = 1, size = 1) +
  geom_sf(data = countries, fill = "lightgrey", colour = 'darkgrey', alpha = 1, linewidth = 0.15) +
  geom_sf(data = wtg_BE, fill = "darkblue", colour = 'darkblue', alpha = 1, size = 5) +
  geom_sf(data = windfarms_polygons, fill = "transparent", colour = 'red', alpha = 1, linewidth = 0.15) +
  coord_sf(xlim = c(1.5, 4), ylim = c(51, 52), expand = FALSE)+
  theme(
    axis.text = element_blank(),   # removes axis text (labels)
    axis.ticks = element_blank(),   # removes tick marks
    panel.background = element_rect(fill = "white") #'#edebeb'
  )

map

# etn data ----------------------------------------------------------------


## animal projects
all_animal_proj <- etn::list_animal_project_codes()
animal_proj <- c("sharks_vliz", "thornback_vliz")

## receiver deployments
deployments <- etn::get_acoustic_deployments(open_only = T)
stations <- 
  deployments %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(deploy_latitude = mean(deploy_latitude, na.rm = T),
                     deploy_longitude = mean(deploy_longitude, na.rm = T))

## tags
tags <- etn::get_tags()
tags_VLIZ <- tags %>%
  dplyr::filter(owner_organization == "VLIZ")

## VLIZ-tagged animals
animals_VLIZ <- 
  etn::get_animals(tag_serial_number = tags_VLIZ$tag_serial_number)

## detections for VLIZ-tagged animals

dets_VLIZ <-
  etn::get_acoustic_detections(acoustic_tag_id = tags_VLIZ$acacoustic_tag_id)



# maps with deployments ---------------------------------------------------

map_leaflet_d <-
  map_leaflet %>%
  addCircleMarkers(data = deployments,
                   lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   radius = 3,
                   fillOpacity = 1,
                   weight = 0,
                   color = "blue",
                   label = ~paste0("station: ", station_name),
                   popup = ~paste0("station: ", station_name)) 

map_leaflet_d


# ETN data ----------------------------------------------------------------

# 00. first checking for animal projects that might have data that can be used
etn_proj <- etn::get_animal_projects()
etn_proj_owf <- 
  etn_proj %>%
    dplyr::filter(grepl("OWF", etn_proj$project_code))

OWF_proj_ani <- 
  etn::get_animals(animal_project_code = etn_proj_owf$project_code) %>% 
  dplyr::filter(tag_subtype == "animal")

OWF_proj_det <- 
  etn::get_acoustic_detections(acoustic_project_code = "FISHOWF")

# OLD ---------------------------------------------------------------------

# plot_path <- "etn-projects/01_maps/graphics/SI2026_elasmos_vliz"

# # define colors
# colors <- 
#   c("m" = "#91C1CD", 
#     "f" = "#B06258",
#     "Estuary" = "#E0C143", 
#     "Coast" = "#6D9D49", 
#     "Wind Farm" = "darkorange",
#     "Offshore" = "#266CA9",
#     "other" = "grey") 
# 
# color_df <- tibble(
#   name = names(colors),
#   color = unname(colors)) 
# 
# ## sex symbols
# sex_symbols <- tibble(sex = c("f", "m"),
#                       symbol = c("\u2640", "\u2642"))

# Now we set the bounding box and the start and end data for our datasets
# lon_min <- 1.7
# lon_max <- 4.3
# lat_min <- 51
# lat_max <- 52.2


## animals
elasmos <- 
  etn::get_animals(animal_project_code = animal_proj) %>%
  dplyr::mutate(sex = 
                  ifelse(sex %in% c('f', 'F', 'female', 'Female', 'FEMALE'), 'f', 'm'),
                month = lubridate::month(release_date_time),
                animal_id = factor(animal_id, levels = unique(animal_id)),
                year = lubridate::year(release_date_time)) %>%
  dplyr::left_join(seasons, join_by(between(month, start, end))) %>%
  dplyr::left_join(tags %>% dplyr::select(tag_serial_number, status, owner_organization, activation_date, battery_estimated_end_date)) %>%
  dplyr::mutate(release_date = lubridate::date(release_date_time),
                battery_end_date = lubridate::date(battery_estimated_end_date)) %>%
  dplyr::left_join(sex_symbols) %>%
  dplyr::select(-(length2_type:length4_unit), -(sedative_concentration:holding_temperature)) %>%
  sf::st_as_sf(coords = c("release_longitude", "release_latitude"), crs = 4326) %>%
  dplyr::rename(release_geom = geometry)

### inspection
elasmos %>% 
  dplyr::filter(scientific_name != "Rhincodon typus") %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::count() %>%
  sf::st_drop_geometry()

## seasons
seasons <-
  tibble(start = c(03, 06, 09, 12, 01),
         end = c(05, 08, 11, 12, 02),
         season = c("spring", "summer", "autumn", "winter", "winter")) %>%
  dplyr::mutate(season = factor(season, levels = c("spring", "summer", "autumn", "winter")))

seasons_start_end <-
  seasons %>%
  tidyr::expand_grid(year = (elasmos$release_date_time %>% lubridate::year() %>% min()):(elasmos$release_date_time %>% lubridate::year() %>% max())) %>%
  dplyr::mutate(start_date = paste0(year, "-", sprintf("%02d", start), "-01") %>% as.Date(),
                end_date = paste0(year, "-", sprintf("%02d", end), "-28") %>% as.Date()) %>%
  dplyr::select(-c(start, end))

## number of individuals tagged (per sex and species)
elasmos_ind_tagged <-
  elasmos %>%
  dplyr::group_by(scientific_name, sex) %>%
  dplyr::summarise(n_ind_tagged = dplyr::n()) %>%
  sf::st_drop_geometry()

## stations
all_deployments <- 
  etn::get_acoustic_deployments(open_only = F) %>%
  dplyr::mutate(deploy_year = lubridate::year(deploy_date_time),
                lat = deploy_latitude, lon = deploy_longitude) %>%
  dplyr::filter(!is.na(deploy_longitude) & !is.na(deploy_latitude),
                deploy_year > 2016) %>%
  dplyr::mutate(open = ifelse((deploy_year > 2023) & is.na(recover_date_time), T, F))


stations_raw <-
  all_deployments %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(acoustic_project_code = first(acoustic_project_code),
                   open = ifelse(T %in% unique(open), T, F),
                   deploy_latitude = mean(deploy_latitude, na.rm = T),
                   deploy_longitude = mean(deploy_longitude, na.rm = T),
                   lat = deploy_latitude,
                   lon = deploy_longitude) %>%
  dplyr::filter(deploy_latitude %>% dplyr::between(lat_min, lat_max),
                deploy_longitude %>% dplyr::between(lon_min, lon_max)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  # transform into CRS based on m unit and calc min distance to the BENL geom
  sf::st_transform(32632) %>%
  dplyr::mutate(lon_UTM = st_coordinates(geometry)[, 1], # get lat and lon in UTM projection to use in model
                lat_UTM = st_coordinates(geometry)[, 2],
                dist_to_coast_km = apply(st_distance(geometry, BENL %>% sf::st_transform(32632)), 1, min) / 1000,
                dist_to_shipwreck_km =  apply(st_distance(geometry, shipwrecks %>% sf::st_transform(32632)), 1, min) / 1000) %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(row_id = dplyr::row_number(),
                within_BPNS = sf::st_within(geometry, BPNS) %>% lengths() > 0 | acoustic_project_code %in% c("cpodnetwork", "bpns"),
                within_Scheldt = sf::st_within(geometry, Scheldt_Estuary) %>% lengths() > 0,
                within_BE_OWF = sf::st_within(geometry, BE_OWF) %>% lengths() > 0,
                station_cat = 
                  ifelse(within_Scheldt == T, "Estuary",
                         ifelse(dist_to_coast_km < 20 & (within_BPNS == T), "Coast",
                                ifelse(dist_to_coast_km > 20 & (within_BPNS == T) & (within_BE_OWF == F), "Offshore",
                                       ifelse(within_BE_OWF == T, "Wind Farm",
                                              "other"))))
  ) %>%
  dplyr::mutate(station_cat = factor(station_cat, levels = c("Estuary", "Coast", "Offshore", "Wind Farm", "other")))


### inspection
mapview(stations_raw)
stations_raw %>% dplyr::filter(within_BE_OWF)
leaflet() %>%
  addTiles() %>%
  setView(lng = 3, lat = 51, zoom = 8) %>%
  addCircles(data = stations,
             radius = 100,
             weight = 0,
             fillColor = "grey",
             fillOpacity = 1) %>%
  addLabelOnlyMarkers(data = stations %>% dplyr::filter(station_cat != "other"),
                      label = ~station_name,
                      labelOptions = labelOptions(
                        noHide = TRUE,
                        textsize = "12px",
                        direction = "auto"))

## bathymetry
bathy_rast <- terra::rast(file.path(processed_dir, "bathy_rast.nc"))

#### Extract raster values and keep the ID
bathy_vals <- terra::extract(bathy_rast, terra::vect(stations_raw)) %>%
  rename(row_id = ID) %>%  # terra returns "ID" col
  group_by(row_id) %>%
  # there are several rows per ID, so we need to summarise them: calc mean
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")


## seabed habitats
habitats_rast <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))

# Extract habitat raster values
habitats_vals <- 
  terra::extract(habitats_rast, terra::vect(stations_raw)) %>%
  rename(row_id = ID) %>% 
  distinct(row_id, .keep_all = TRUE)

## complete stations with env data
stations <-
  stations_raw %>%
  dplyr::left_join(habitats_vals, by = "row_id") %>%
  dplyr::left_join(bathy_vals, by = "row_id")

rm(bathy_vals)
rm(habitats_vals)

## detections
det_raw <-
  etn::get_acoustic_detections(animal_project_code = animal_proj)

### per stationxanimal combination, calculate the distance to the animal's release location
stations_release_locations <-
  det_raw %>%
  dplyr::mutate(animal_id = factor(animal_id, levels = unique(animal_id))) %>%
  dplyr::group_by(animal_id, station_name) %>%
  dplyr::summarise(lat = mean(deploy_latitude, na.rm = T),
                   lon = mean(deploy_longitude, na.rm = T)
                   ,release_geom = elasmos %>%
                     filter(animal_id == animal_id) %>%
                     pull(release_geom) %>%
                     .[1]
  ) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  dplyr::mutate(distance_to_release_km = (sf::st_distance(geometry, release_geom, by_element = TRUE) %>% as.numeric() / 1000) %>% round())


det_elasmos <- 
  det_raw %>% 
  dplyr::mutate(animal_id = factor(animal_id, levels = unique(animal_id))) %>%
  dplyr::left_join(elasmos %>% dplyr::select(animal_id, sex, release_date_time) %>% sf::st_drop_geometry(), by = 'animal_id') %>%
  dplyr::mutate(month = lubridate::month(date_time),
                year = lubridate::year(date_time),
                hour = lubridate::hour(date_time),
                release_date = release_date_time %>% lubridate::date(),
                lat = deploy_latitude,
                lon = deploy_longitude) %>%
  dplyr::left_join(seasons, join_by(between(month, start, end))) %>%
  dplyr::left_join(stations_release_locations) %>%
  dplyr::left_join(elasmos_ind_tagged) %>%
  # dplyr::left_join(colors) %>%
  dplyr::left_join(sex_symbols) %>%
  dplyr::left_join(stations %>% dplyr::select(station_name, dist_to_coast_km, dist_to_shipwreck_km, station_cat, habitat, elevation, lat_UTM, lon_UTM) %>% sf::st_drop_geometry()) %>%
  dplyr::left_join(color_df, by = join_by(station_cat == name)) %>%
  dplyr::mutate(color = color %>% as.factor(),
                station_cat = factor(station_cat, levels = c("Estuary", "Coast", "Offshore", "Wind Farm", "other"))) %>%
  dplyr::add_count(station_name, name = "n_det_station") %>% #only keep stations with >1 detection
  dplyr::arrange(desc(release_date_time)) %>%
  dplyr::mutate(animal_id = factor(animal_id, levels = unique(animal_id)),
                # scale continuous variables
                elevation_scaled = base::scale(elevation)[,1],
                dist_to_shipwreck_km_scaled = base::scale(dist_to_shipwreck_km)[,1],
                dist_to_coast_km_scaled = base::scale(dist_to_coast_km)[,1],
                dist_to_release_km_scaled = base::scale(distance_to_release_km)[,1]) %>%
  filter(n_det_station > 1,
         !acoustic_project_code %in% c("2024_bovenschelde", "BTN-IMEDEA", "SW_Oude_Ijssel"), #filter out false dets
         lat > 0) %>% # -> remove detections from west of BPNS
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) 

write.csv(det_elasmos, file.path(plot_path, "elasmos_det_until_20251105.csv"))

### inspection
det_elasmos %>%
  dplyr::filter(scientific_name == "Mustelus asterias", season == "winter") %>%
  View()

det_elasmos %>% group_by(scientific_name) %>% summarise(n = animal_id %>% unique() %>% length()) %>% st_drop_geometry()

det_elasmos %>%
  dplyr::group_by(scientific_name, season) %>%
  dplyr::summarise(mean_dist_to_coast_km = mean(dist_to_coast_km),
                   sd_dist_to_coast_km = sd(dist_to_coast_km),
                   mean_dist_to_shipwreck_km = mean(dist_to_shipwreck_km),
                   sd_dist_to_shipwreck_km = sd(dist_to_shipwreck_km)) %>%
  sf::st_drop_geometry()

det_elasmos %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::count() %>%
  sf::st_drop_geometry()

# summarising detections --------------------------------------------------

## per individual
det_elasmos_ind <-
  det_elasmos %>%
  dplyr::mutate(month = lubridate::floor_date(date_time, unit = "month")) %>%
  dplyr::group_by(animal_id, month, station_name) %>%
  dplyr::summarise(n_detections = dplyr::n(),
                   scientific_name = first(scientific_name),
                   distance_to_release_km = first(distance_to_release_km)) %>%
  dplyr::left_join(stations %>% dplyr::select(station_name, station_cat) %>% sf::st_drop_geometry())

## per station
det_elasmos_station <-
  det_elasmos %>%
  dplyr::mutate(month = lubridate::floor_date(date_time, unit = "month")) %>%
  dplyr::group_by(station_name, month, scientific_name) %>%
  dplyr::summarise(n_individuals = animal_id %>% unique() %>% length(),
                   n_species = scientific_name %>% unique() %>% length()) %>%
  dplyr::left_join(stations %>% dplyr::select(station_name, station_cat) %>% sf::st_drop_geometry())

## per season, station and, sex species
det_elasmos_station_season_sex_species <-
  det_proc %>%
  dplyr::group_by(station_name, season, sex, scientific_name) %>%
  dplyr::summarise(n_detections = dplyr::n(),
                   n_individuals = animal_id %>% unique() %>% length(),
                   station_cat = first(station_cat)) 

# plot detections ---------------------------------------------------------

## per time interval (month)
abacus_ind <-
  det_elasmos_ind %>%
  ggplot(aes(y = animal_id, x = month, color = station_cat, size = n_detections, shape = scientific_name, alpha = distance_to_release_km)) +
  geom_point() +
  theme_minimal()

abacus_ind #%>% ggplotly()

ggsave(file.path(plot_path, "abacus_ind.png"), abacus_ind , height = 25, width = 15, units = 'cm') 
ggsave(file.path(plot_path, "abacus_ind.svg"), abacus_ind , height = 25, width = 15, units = 'cm') 

## per station
abacus_station <-
  det_elasmos_station %>%
  ggplot(aes(y = station_name, x = month, color = station_cat, size = n_individuals, shape = scientific_name)) +
  geom_point() +
  theme_minimal()

abacus_station %>% ggplotly()

ggsave(file.path(plot_path, "abacus_station.png"), abacus_station , height = 25, width = 15, units = 'cm') 
ggsave(file.path(plot_path, "abacus_station.svg"), abacus_station , height = 25, width = 15, units = 'cm') 

# map detections ----------------------------------------------------------

map_n_ind_season <-
  basemap +
  geom_sf(data = stations, aes(fill = station_cat), color = "transparent", shape = 21, stroke = 0, size = 2, alpha = 0.55) + #deployments
  geom_sf(data = det_elasmos_station_season_sex_species, aes(size = n_individuals, fill = sex),
          color = "black",     # <-- outline color
          shape = 21,          # <-- ensures points can have separate fill + outline
          stroke = 0.5         # <-- thickness of the outline (adjust as needed)
  ) +
  scale_fill_manual(values = colors) +
  scale_size_continuous(
    breaks = seq(0, max(det_elasmos_station_season_sex_species$n_individuals, na.rm = TRUE), by = 2)
  ) +
  facet_wrap(c("scientific_name", "season")) +
  coord_sf(xlim = c(2, 4), ylim = c(51, 52), expand = FALSE, label_axes = "----") + #bbox
  labs(size = "# individuals", fill = "Location", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks   = element_blank()) +
  theme_minimal() +
  guides(size = "none")

map_n_ind_season

ggsave(file.path(plot_path, "map_n_ind_season.png"), map_n_ind_season , height = 25, width = 25, units = 'cm') 
ggsave(file.path(plot_path, "map_n_ind_season.svg"), map_n_ind_season , height = 25, width = 25, units = 'cm') 


# individual inspection ---------------------------------------------------

s_canicula <-  99926 # 72846 #

det_elasmos %>%
  dplyr::filter(animal_id == s_canicula) %>%
  ggplot(aes(y = distance_to_release_km, shape = station_cat, x = date_time, color = station_name)) +
  geom_point() +
  theme_minimal()

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = elasmos %>%
                     dplyr::filter(animal_id == s_canicula),
                   label = ~paste0("release time: ", release_date_time)) %>%
  addCircleMarkers(data = det_elasmos %>%
                     dplyr::filter(animal_id == s_canicula) %>%
                     dplyr::select(-release_geom),
                   label = ~paste0("det time: ", date_time))

# formulate GAMs ----------------------------------------------------------

## S. canicula

s_canicula_det <-
  det_elasmos %>%
  dplyr::filter(scientific_name == "Scyliorhinus canicula") %>%
  dplyr::mutate(animal_id = factor(animal_id, levels = unique(animal_id)),
                # scale continuous variables again -> only for this species
                elevation_scaled = base::scale(elevation)[,1],
                dist_to_shipwreck_km_scaled = base::scale(dist_to_shipwreck_km)[,1],
                dist_to_coast_km_scaled = base::scale(dist_to_coast_km)[,1],
                dist_to_release_km_scaled = base::scale(distance_to_release_km)[,1]) %>% 
  sf::st_drop_geometry()

s_canicula_det %>% head()

# which response var makes sense?
## idea: distance to release_km

s_canicula_gam1 <- mgcv::gam(distance_to_release_km ~ 
                               s(dist_to_coast_km_scaled, k = 10, bs = "tp") +
                               s(dist_to_shipwreck_km_scaled, k = 10, bs = "tp") +
                               sex + season + #habitat +
                               s(animal_id, bs = "re") +
                               offset(log(n_ind_tagged)),
                             family = Gamma(link = "log"),
                             method = "REML",
                             data = s_canicula_det
)

summary(s_canicula_gam1)


gam.check(s_canicula_gam1)
plot(s_canicula_gam1)

s_canicula_gam2 <- mgcv::gam(distance_to_release_km ~ 
                               s(dist_to_coast_km_scaled, k = 10, bs = "tp") +
                               s(dist_to_shipwreck_km_scaled, k = 10, bs = "tp") +
                               season +# station_cat + #habitat +
                               s(animal_id, bs = "re") +
                               offset(log(n_ind_tagged)),
                             family = Gamma(link = "log"),
                             method = "REML",
                             data = s_canicula_det
)

summary(s_canicula_gam2)


gam.check(s_canicula_gam2)
plot(s_canicula_gam2)

summary(s_canicula_gam2)$s.table


## all species

species_model_df <-
  det_elasmos %>%
  dplyr::mutate(week = lubridate::floor_date(date_time, unit = "weeks")) %>%
  dplyr::group_by(animal_id, station_name, week) %>%
  dplyr::summarise(time_difference_days = (base::difftime(max(date_time), min(date_time), units = "days") %>% as.numeric())  + 1e-6,
                   dist_to_coast_km_scaled = dist_to_coast_km_scaled %>% first(),
                   dist_to_shipwreck_km_scaled = dist_to_shipwreck_km_scaled %>% first(),
                   distance_to_release_km_scaled = dist_to_release_km_scaled %>% first(),
                   n_detections = dplyr::n(),
                   scientific_name = unique(scientific_name),
                   station_cat = unique(station_cat),
                   sex = sex %>% first(),
                   season = season %>% first(),
                   n_ind_tagged = n_ind_tagged %>% max()) %>% 
  sf::st_drop_geometry()

all_species_gam1 <- mgcv::gam(time_difference_days ~ 
                                s(dist_to_coast_km_scaled, k = 10, bs = "tp") +
                                s(distance_to_release_km_scaled, k = 10, bs = "tp") +
                                # s(dist_to_shipwreck_km_scaled, k = 20, bs = "tp") +
                                sex + season + scientific_name +
                                s(animal_id, bs = "re") +
                                offset(log(n_ind_tagged)),
                              family =  tw(link="log"), #Gamma(link = "log"),
                              method = "REML",
                              data = species_model_df
)

summary(all_species_gam1)

gam.check(all_species_gam1)
plot(all_species_gam1)

## Mustelus asterias

m_asterias_det <-
  det_elasmos %>%
  dplyr::filter(scientific_name == "Mustelus asterias") %>%
  dplyr::mutate(animal_id = factor(animal_id, levels = unique(animal_id)),
                # scale continuous variables again -> only for this species
                elevation_scaled = base::scale(elevation)[,1],
                dist_to_shipwreck_km_scaled = base::scale(dist_to_shipwreck_km)[,1],
                dist_to_coast_km_scaled = base::scale(dist_to_coast_km)[,1],
                dist_to_release_km_scaled = base::scale(distance_to_release_km)[,1]) %>% 
  sf::st_drop_geometry()

m_asterias_gam1 <- mgcv::gam(distance_to_release_km ~ 
                               # s(dist_to_coast_km_scaled, k = 20, bs = "tp") +
                               s(dist_to_shipwreck_km_scaled, k = 20, bs = "tp") +
                               sex + season + station_cat +
                               s(animal_id, bs = "re") +
                               offset(log(n_ind_tagged)),
                             family = Gamma(link = "log"),
                             method = "REML",
                             data = m_asterias_det
)

summary(m_asterias_gam1)


gam.check(m_asterias_gam1)
plot(m_asterias_gam1)

# plots -------------------------------------------------------------------

## boxplots
s_canicula_det %>%
  ggplot(x = sex, y = distance_from_release_km, fill = sex) +
  geom_boxplot() +
  theme_minimal()
