pacman::p_load(sf, 
               tmap, 
               tidyverse)

data("World")

pts <- tribble(
  ~lat,      ~long, 
  41.293995, -101.670111, 
  -9.119160, -52.055926, 
  16.472194, 101.137433,
  23.648822, 90.195050,
  53.333674, -1.870379)
# pulled above from Google Maps

pt_sf <- st_as_sf(pts, coords = c("long", "lat"), 
                  crs = 4326)

prior_study <- tm_shape(World)+
  tm_borders()+
  tm_shape(pt_sf)+
  tm_markers()+
  tm_layout(frame = FALSE)

tmap_save(tm = prior_study, 
          filename = "prior_g6pd.png", 
          dpi = 700)