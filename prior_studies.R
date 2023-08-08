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
  ~lat,      ~long,      ~ part, ~ sensitivity, ~ci_low_sens, ~ci_hi_sens, ~place,
  41.293995, -101.670111, 100,        100,           83.2,        100,      "US",
  -9.119160, -52.055926, 1736,        100,          93.6,        100,       "Brasil",
  16.472194, 101.137433, 150,         100,           93.4,        100,      "Thailand",
  23.648822, 90.195050,  102,         100,           88,          100,      "Bangladesh",
  53.333674, -1.870379,  167,         100,           92.3,        100,      "UK",
  12.454958, 104.943110, 1967,        93.1,          87.7,        96.6,     "Cambodia")

# pulled above from Google Maps

pt_sf <- st_as_sf(pts2, coords = c("long", "lat"), 
                  crs = 4326)
tmap_mode("plot")
prior_study2 <- tm_shape(World)+
  tm_borders()+
  tm_shape(pt_sf)+
  tm_markers(size = "part", 
             scale = 1.3)+
  tm_layout(frame = FALSE, legend.show = FALSE)

tmap_save(tm = prior_study2, 
          filename = "prior_g6pd_2.png", 
          dpi = 700)

comp <- ggplot(pts2)+
  geom_col(aes(x = place, y = sensitivity))+
  geom_errorbar(aes(x = place, ymin = ci_low_sens, ymax = ci_hi_sens), width = 0.2)+
  theme_minimal()+
  labs(x = "Location", 
       y = "Sensitivity for <30% AMM G6PD Activity")

ggsave(comp, filename = "study_comparison.png", 
       width = 8, 
       height = 6, 
       units = "in")
