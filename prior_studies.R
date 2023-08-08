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


pts2 <- tribble(
  ~lat,      ~long,      ~ part,
  41.293995, -101.670111, 183,  #US
  -9.119160, -52.055926, 1736,  #Brasil
  16.472194, 101.137433, 150,   #Thailand
  23.648822, 90.195050,  997,  #Bangladesh
  53.333674, -1.870379,  167,   #UK
  12.454958, 104.943110, 1967)  #Cambodia
# pulled above from Google Maps

pt_sf <- st_as_sf(pts2, coords = c("long", "lat"), 
                  crs = 4326)
tmap_mode("plot")
prior_study2 <- tm_shape(World)+
  tm_borders()+
  tm_shape(pt_sf)+
  tm_markers(size = "part")+
  tm_layout(frame = FALSE, legend.show = FALSE)

tmap_save(tm = prior_study2, 
          filename = "prior_g6pd_2.png", 
          dpi = 700)
