library(janitor)
library(linelist)

# Sorry this one is sort of a mess; cobbled together a few data sources that I was too lazy to change 
# (I would have if this were to be used for anything else. In fact I can't imagine that anyone is reading this now.)

annual <- rio::import("mme_epi.xlsx") %>%
  clean_data(guess_dates = F) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(1) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname),
         rowname = as.Date(rowname, origin = "1899-12-30"), 
         year = lubridate::year(rowname)) %>% 
  mutate_at(2:4, as.numeric) %>% 
  group_by(year) %>% 
  summarise(pf = sum(p_faliciparum_cases, na.rm = T), 
            pv = sum(p_vivax_cases, na.rm = T), 
            mix = sum(mixed_cases, na.rm = T)) %>% 
  pivot_longer(!year, names_to = "species", values_to = "count") %>% 
  mutate(species = forcats::as_factor(forcats::fct_relevel(species, c("pv", "pf", "mix"))))%>% 
  mutate(year = as.character(year))

annual_add_on <- rio::import("cambodia_21_23_incidence.xlsx") %>% 
  pivot_longer(!`...1`, names_to = "year", values_to = "count") %>% 
  rename(species = 1) 

annual <- bind_rows(annual, annual_add_on) %>% 
  mutate(species = factor(species, levels = c("pv", "pf", "mix")))

pal <- c("#7d3834", "grey50", "black")

by_year <- ggplot(annual)+
  geom_bar(aes(x = year, y = count, fill = species), stat = "identity", 
           color = "black", 
           position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = pal)+
  theme_minimal()+
  labs(x = "Year", 
       y = "Cases")+
  theme(legend.position = "none")

a <- ggplot(annual)+
  geom_bar(aes(x = year, y = count, fill = species), stat = "identity", 
           color = "black", 
           position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = pal, name = "Species", 
                    labels = c("P. vivax", "P. falciparum",
                                             "Mixed"))+
  theme_minimal()+
  labs(x = "Year", 
       y = "Cases")

b <- cowplot::get_legend(a)

ggsave(b, filename = "legend_camb_time.png", dpi = 500)

ggsave(by_year, filename = "camb_by_year.png", dpi = 500)

library(tmap)
library(raster)
library(tidyverse)

gms_count <- c("VNM", "THA", "MMR", "LAO", "CHN", "KHM")
maps <- list()
for (i in 1:length(gms_count))
  maps[i] <- raster::getData("GADM", country = gms_count[i], level = 0)


maps1 <- list()
for (i in 1:length(gms_count))
  maps1[[i]] <- maps[[i]] %>% 
  sf::st_as_sf()


gms <- bind_rows(maps1)
gms <- gms %>% 
  mutate(counts = case_when(GID_0 == "VNM" ~ 1421, 
                   GID_0 == "THA" ~ 4004, 
                   GID_0 == "MMR" ~ 58836, 
                   GID_0 == "LAO" ~ 3504, 
                   GID_0 == "CHN" ~ 137, 
                   TRUE ~ 9411), 
         log_counts = log10(counts)) %>% 
  sf::st_as_sf()
#list2env(maps, envir = .GlobalEnv)


box <- tmaptools::bb(xlim=c(91.421536, 110.669583), 
                     ylim = c(5.232734, 29.544392))

gms_map <- tm_shape(gms, bbox = box)+
  tm_fill("log_counts", style = "cont", palette = c("white", "black"))+
  tm_borders(col = "black")+
  tm_layout(frame = F, legend.show = F, bg.color = "transparent")
 
tmap_save(tm = gms_map, filename = "gms_counts.png", dpi = 500, bg="transparent")


box2 <- tmaptools::bb(xlim=c(101.640254, 108.259793), 
                      ylim = c(9.862451, 14.867329))

cambodia_nation <- maps1[[6]] %>% 
  sf::st_as_sf()

ks <- getData('GADM', country = 'KHM', level = 1) %>% 
  sf::st_as_sf() %>%
  filter(VARNAME_1 == "Kampong Speu|Kompong Speu|Kompong Spueu") %>% 
  sf::st_as_sf()

diff <- sf::st_difference(cambodia_nation, gms) 

pp <- tibble(place = "pp", lat = 11.574521, long = 104.884649)
pp_sf <- sf::st_as_sf(pp, coords = c("long", "lat"), 
         crs = 4326)

kampong_map <- tm_shape(gms, bbox = box2)+
  tm_borders(col = "black")+
  tm_shape(mask_pv)+
  tm_raster(palette = "YlOrBr", 
            breaks = breaks)+
  tm_shape(cambodia_nation)+
  tm_borders(col = "black")+
  tm_shape(diff)+
  tm_fill("azure3", alpha = 0.5)+
  tm_shape(kampong)+
  tm_borders(col = "azure4", lwd = 4)+
  tm_shape(pp_sf)+
  tm_markers(shape = marker_icon(), size = 1)+
  tm_layout(frame = F, legend.show = F)

kampong_leg <- tm_shape(mask_pv)+
  tm_raster(palette = "YlOrBr", 
            breaks = breaks)+
  tm_shape(cambodia_nation)+
  tm_borders(col = "black")+
  tm_shape(diff)+
  tm_fill("azure3", alpha = 0.5)+
  tm_shape(kampong)+
  tm_borders(col = "azure4", lwd = 4)+
  tm_shape(pp_sf)+
  tm_markers(shape = marker_icon(), size = 1)+
  tm_layout(frame = F, legend.only = T)  
tmap_save(tm = kampong_leg, filename = "kampong_leg.png", dpi = 500)  

  tm_shape(ks)+
  tm_fill("grey50")+
  tm_borders()+
  tm_layout(frame = F)+
  tm_shape(pp_sf)+
  tm_markers(shape = marker_icon(), size = 1)+
  tm_layout(frame = F)
  
tmap_save(tm = kampong_map, filename = "kampong.png", dpi = 500)

tm_shape(ks)+
  tm_borders()

gms1 <- gms %>% 
  sf::st_drop_geometry()

gms1 <- gms1 %>% 
  arrange(desc(counts))

cases_20 <- ggplot(gms1)+
  geom_bar(aes(x = reorder(NAME_0, -counts), y = counts), stat = "identity", color = "grey60")+
  theme_minimal()+
  labs(x = "", 
       y = "Cases")+
  coord_flip()

ggsave(cases_20, filename = "gms_cases.png", dpi = 500)

data(World)

world <- World %>% 
  mutate(sd = ifelse(iso_a3 %in% c("BRA", "BGD", "USA", "THA"), 1, 0)) %>% 
  sf::st_as_sf()


sd_only <- world %>% 
  filter(sd == 1) %>% 
  sf::st_as_sf() %>% 
  sf::st_centroid()

world <- tm_shape(world)+
 # tm_fill(col = "sd", palette = c("white", "black"))+
  tm_borders()+
  tm_shape(sd_only)+
  tm_markers(shape = marker_icon(), size = 2)+
  tm_layout(frame = F)

tmap_save(tm = world, filename = "world.png", dpi = 
            500)
