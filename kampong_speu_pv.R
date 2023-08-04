pacman::p_load(raster, 
               tmap, 
               tidyverse)


cambodia_province <-  getData('GADM', country='KHM', level=1)

cambodia_nation <-  getData('GADM', country='KHM', level=0) %>% 
  st_as_sf()

kampong <- st_as_sf(cambodia_province)%>% 
  filter(NAME_1 =="Kâmpóng Spœ")

path <- "C:/Users/omp2/OneDrive - CDC/Cambodia_03_25_19/analysis/cambodia_g6pd/2019_Global_Pv_Incidence_2017.tif"
# this raster was downloaded from MAP; can be pulled directly with malariaAtlas package

pv <- raster(path)


crop_pv <- crop(pv, cambodia_nation)
mask_pv <- mask(crop_pv, cambodia_nation)

mask_pv@data@names <- "Pv Incidence"

breaks <- seq(from = 0, to = 0.3, by = 0.02)
cambodia_nation <- st_as_sf(cambodia_nation)
kampong <- st_as_sf(kampong)

diff <- st_difference(cambodia_nation, kampong) 

data("World")
#world <- World 

#st_crs(world) <- 4326

diff2 <- st_difference(World, kampong)


map1 <- tm_shape(mask_pv)+
  tm_raster(palette = "YlOrBr", 
            breaks = breaks)+
  tm_shape(cambodia_nation)+
  tm_borders(col = "black")+
  tm_layout(frame = FALSE, legend.show = FALSE)

map2 <-tm_shape(mask_pv)+
  tm_raster(palette = "YlOrBr", 
            breaks = breaks)+
  tm_shape(cambodia_nation)+
  tm_borders(col = "black")+
  tm_shape(diff)+
  tm_fill("azure3", alpha = 0.5)+
  tm_shape(kampong)+
  tm_borders(col = "azure4", lwd = 4)+
  tm_layout(frame = FALSE, legend.show = FALSE)

raster_leg <- tm_shape(mask_pv)+
  tm_raster(palette = "YlOrBr", 
            breaks = breaks)+
  tm_shape(cambodia_nation)+
  tm_borders(col = "black")+
  tm_layout(frame = FALSE, legend.only = TRUE)

map_list <- list(map1, map2, raster_leg) %>% 
  set_names("pv_inc", "pv_ks_inc", "pv_inc_leg")

walk2(map_list, names(map_list), ~tmap_save(.x, 
                                            filename = paste(here::here(.y), 
                                                             ".png"), 
                                            dpi = 700))
      