pacman::p_load(malariaAtlas, 
               raster, 
               sf, 
               tmap, 
               tidyverse)

g6pd_global <- getRaster(surface = "G6PD Deficiency Allele Frequency")
pf <- getRaster(surface = "Plasmodium  falciparum Incidence version 2020", year = 2001)
pv <- getRaster(surface = "Plasmodium vivax Incidence version 2020", year = 2001)

pv1 <- pv
pv1[pv < 0] <- 0


cols <- colorRampPalette(c("white", "#7d3834"), bias = 10)(5)
cols_mal <- colorRampPalette(c("white", "#6F5E33"), bias = 10)(5)


g6pd_globe <- tm_shape(World)+
  tm_fill(col = "white")+
  tm_shape(g6pd_global)+
  tm_raster(style = "cont", palette = cols)+
  tm_shape(World)+
  tm_borders()+
  tm_layout(frame = FALSE, 
            bg.color = "#3C4A5A",
            legend.show = FALSE)

g6pd_globe_leg <- tm_shape(g6pd_global)+
  tm_raster(style = "cont", palette = cols)+
  tm_layout(frame = FALSE, 
            legend.only = TRUE)

data("World")

pf_globe <- tm_shape(pf)+
  tm_raster(style = "cont", palette = cols_mal)+
  tm_layout(frame = FALSE, 
            legend.show = FALSE)

pf_globe_leg <- tm_shape(pf)+
  tm_raster(style = "cont", palette = cols_mal)+
  tm_layout(frame = FALSE, 
            legend.only = TRUE)


pv_globe <- tm_shape(pv1)+
  tm_raster(style = "cont", palette = cols_mal)+
  tm_layout(frame = FALSE, 
            legend.show = FALSE)

pv_globe_leg <- tm_shape(pv1)+
  tm_raster(style = "cont", palette = cols_mal)+
  tm_layout(frame = FALSE, 
            legend.only = TRUE)

figs <- list(g6pd_globe, g6pd_globe_leg, 
             pf_globe, pf_globe_leg, 
             pv_globe, pv_globe_leg) %>% 
  set_names(c("g6pd_globe", "g6pd_globe_leg",
              "pf_globe", "pf_globe_leg",
              "pv_globe", "pv_globe_leg"))

walk2(figs, names(figs), ~tmap_save(tm = .x, filename = paste0(getwd(), "/", .y, 
                                                               ".png"),
                                    dpi = 700))

