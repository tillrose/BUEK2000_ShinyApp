
getPointCoordinates <- function(geoLaenge, geoBreite, crs = "EPSG:25832") {
  
  data.frame("geoLaenge" = geoLaenge, "geoBreite" = geoBreite) %>% 
    st_as_sf(coords = c("geoLaenge", "geoBreite")) %>% 
    st_set_crs(value = "+proj=longlat +datum=WGS84") %>% 
    st_transform(crs = crs) %>% 
    mutate("geoLaenge" = geoLaenge, "geoBreite" = geoBreite)
  
}


getSoilMap <- function(point_coordinates, BUEK2000_Kartenblaetter, BUEK2000_path) {
  
  ## Select map sheet and import
  Kartenblatt <- point_coordinates %>% 
    st_join(BUEK2000_Kartenblaetter, join = st_covered_by) %>% 
    pull(Kartenblatt) %>% 
    first()
  
  File_string <- paste0(BUEK2000_path, Kartenblatt)
  
  Kartenblatt_spatial <- read_rds(File_string) %>% 
    dplyr::select(TKLE_NR)
  
  Kartenblatt_spatial
  
}


getSoilPolygon <- function(point_coordinates, Kartenblatt_spatial, BUEK2000_code) {
  
  ## Extract point values
  Boden_All <- point_coordinates %>% 
    st_join(Kartenblatt_spatial, join = st_nearest_feature) %>%
    left_join(BUEK2000_code, multiple = "all", by = "TKLE_NR") 
  
  Boden_All
  
}


getSoilTexture <- function(soil_polygon, short = TRUE, fill_NA = TRUE) {
  
  Boden_All <- soil_polygon %>% 
    `st_geometry<-`(NULL)
  
  ## Set texture to the value óf the upper horizon if texture is na
  if(fill_NA) {
    
    Boden_All <- Boden_All %>% 
      mutate(BOART = ifelse(is.na(BOART), lag(BOART), BOART))
    
  }
  
  ## Reduce data to the basic information if short == TRUE
  if(short) {
    
    Boden_All <-  Boden_All %>% 
      filter(STATUS == "Leitboden") %>% 
      dplyr::select(geoLaenge, geoBreite, HOR_NR, OTIEF, UTIEF, BOART, LE_TXT) %>% 
      mutate(OTIEF = as.integer(OTIEF*10),
             UTIEF = as.integer(UTIEF*10))
    
  }
  
  Boden_All
  
}




# getSoilTexture <- function(geoLaenge, geoBreite, BUEK2000_shape, BUEK2000_code, short = TRUE, fill_NA = TRUE) {
#   
#   ## Transform numerical Long/Lat-input into sf coordinates with the right crs
#   point_coordinates <- data.frame("geoLaenge" = geoLaenge, "geoBreite" = geoBreite) %>% 
#     st_as_sf(coords = c("geoLaenge", "geoBreite")) %>% 
#     st_set_crs(value = "+proj=longlat +datum=WGS84") %>% 
#     st_transform(crs = st_crs(BUEK2000_shape)) %>% 
#     mutate("geoLaenge" = geoLaenge, "geoBreite" = geoBreite)
#   
#   ## Extract point values
#   Boden_All <- point_coordinates %>% 
#     st_join(BUEK2000_shape, join = st_nearest_feature) %>%
#     left_join(BUEK2000_code, multiple = "all", by = "TKLE_NR") %>% 
#     dplyr::select(-Symbol, -Shape_Area, -Shape_Leng) %>% 
#     `st_geometry<-`(NULL)
#   
#   ## Set texture to the value óf the upper horizon if texture is na
#   if(fill_NA) {
#     
#     Boden_All <- Boden_All %>% 
#       mutate(BOART = ifelse(is.na(BOART), lag(BOART), BOART))
#     
#   }
#   
#   ## Reduce data to the basic information if short == TRUE
#   if(short) {
#     
#     Boden_All <-  Boden_All %>% 
#       filter(STATUS == "Leitboden") %>% 
#       dplyr::select(geoLaenge, geoBreite, HOR_NR, OTIEF, UTIEF, BOART) %>% 
#       mutate(OTIEF = as.integer(OTIEF*10),
#              UTIEF = as.integer(UTIEF*10))
#     
#   }
#   
#   Boden_All
#   
# }