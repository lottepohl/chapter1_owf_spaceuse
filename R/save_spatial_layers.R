
# description -------------------------------------------------------------

# helper script to query and save spatial layers from marineregions.org and 
# emodnet Human Activities, and the Belgian wrakkendatabank.


# To Do -------------------------------------------------------------------

# 1. clean up loading the layers
# 2. saving every in ./data/spatial 

# workspace ---------------------------------------------------------------

library(dplyr)
library(sf)
library(giscoR)
# library(EMODnetWFS)
# library(emodnet.wfs)
library(pak)
# pak::pak("lifewatch/mregions2")
library(mregions2)


# paths -------------------------------------------------------------------

path_spatial <- "data/spatial"
path_data_raw <- "data_raw"
path_data <- "data"
path_plots <- "docs/plots"


# spatial objects ---------------------------------------------------------

BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry() # Belgian EEZ
Scheldt_Estuary <-  mregions2::gaz_search(4812) %>% mregions2::gaz_geometry()
BENL <- mregions2::gaz_search(c(14, 15)) %>% mregions2::gaz_geometry()
sf::write_sf(BENL, file.path(path_spatial, "BENL.gpkg"))
#BE_OWF <- sf::st_read(dsn = file.path(getwd(), "etn-projects/01_maps/layers/OWF_BE_safetyzones.gpkg")) %>% sf::st_transform(4326)
countries <- giscoR::gisco_get_countries(year = "2024", resolution = 03, region = "Europe")

## OWF boundaries
wfs_human <- emodnet_init_wfs_client(service = "human_activities")
#wfs_human %>% emodnet_get_wfs_info() %>% View()
layers_windfarms <- wfs_human %>% emodnet_get_layers(layers = c("windfarms", "windfarmspoly"), crs = 4326)
windfarms_polygons <- layers_windfarms %>% purrr::pluck("windfarmspoly")
windfarms_points <- layers_windfarms %>% purrr::pluck("windfarms")

shipwrecks <-
  readr::read_delim(file.path(path_spatial,"wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(Easting, Northing, `Gezonken op`, Code, Bouwjaar) %>%
  sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) %>%
  sf::st_transform(4326) %>%
  dplyr::bind_cols(
    readr::read_delim(file.path(path_spatial,"wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
      dplyr::select(Easting, Northing) %>%
      sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>% # Extract coordinates as a matrix %>%
      tidyr::as_tibble() %>% # Convert coordinates matrix to tibble
      dplyr::rename(lon = X, lat = Y))

# export_cables <- read_sf(file.path(path_spatial, "Export_Cables.shp"))
# # https://spatial.naturalsciences.be/geoserver/od_nature/wms?service=WMS&version=1.1.0&request=GetMap&layers=od_nature%3AMUMM_windmill_locations_ETRS89&bbox=3817777.419943257%2C3176768.2180007333%2C3841725.2737183524%2C3205763.9329526643&width=634&height=768&srs=EPSG%3A3035&styles=&format=application/openlayers
# sf::write_sf(export_cables, file.path(path_spatial, "export_cables.gpkg"))

# concession zones
concession_zones_BE <- read_sf(file.path(path_spatial, "Concession zones.shp"))
sf::write_sf(concession_zones_BE, file.path(path_spatial, "concession_zones_BE.gpkg"))
concession_zones_BE <- concession_zones_BE %>% sf::st_transform(4326)
