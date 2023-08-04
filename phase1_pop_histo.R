a <- psi_c %>% 
  dplyr::select(id, sex) %>% 
  left_join(ipc_c, by = c("id"="id")) %>% 
  mutate(outlier_hb = ifelse(hb_ipc_fbc < hb_low|hb_ipc_fbc > hb_high, 1, 0)) %>% 
  dplyr::filter(sex == "male" & !is.na(g6pd_ipc_spectro) & outlier_hb==0)
# here, standards for diagnosis are set leaving out all hemoglobin outliners

b <- psi_c %>% 
  dplyr::select(id, sex) %>% 
  left_join(ipc_c, by = "id") %>% 
  drop_na(g6pd_ipc_spectro) %>% 
  mutate(outlier_hb = ifelse(hb_biosensor_onsite < 7, 1, 0)) %>% 
  dplyr::filter(!is.na(g6pd_ipc_spectro))
# only those with BIOSENSOR hb >=7 are included


range_g6pd <- c(min(b$g6pd_ipc_spectro), max(b$g6pd_ipc_spectro))

dummy <- hist(b$g6pd_ipc_spectro, 
              breaks =  seq(0, max(b$g6pd_ipc_spectro)+1, by = 1), 
              plot = FALSE)

#dummy[["breaks"]] <- seq(0, max(b$g6pd_ipc_spectro)+0.5, by = 0.5)
ymax <- max(dummy[["counts"]])+20

med_spec <- median(a$g6pd_ipc_spectro)       
cut_spec <- 0.1*med_spec                       
amm_spec <- median(a$g6pd_ipc_spectro[a$g6pd_ipc_spectro > cut_spec])

b1 <- b %>% 
  mutate(frac_spec = g6pd_ipc_spectro/amm_spec) %>% 
  filter(frac_spec <3)  # filters out outliers greater than 3x amm

b1_men <- b1 %>% 
  filter(sex == "male")

b1_female <- b1 %>% 
  filter(sex == "female")



hist_breaks <- seq(0, 2.5, by = 0.1)

hist_tot <- ggplot(b1)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 140, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 140, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=frac_spec),color = "white", breaks = hist_breaks)+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 125))+
  labs(x = "Proportion of Median\n G6PD Activity", 
       y = "Count")

hist_f <- ggplot(b1_female)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 100, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 100, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=frac_spec),color = "white", breaks = hist_breaks)+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 90))+
  labs(x = "Proportion of Median\n G6PD Activity", 
       y = "Count")

hist_m <- ggplot(b1_men)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 100, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 100, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=frac_spec),color = "white", breaks = hist_breaks)+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 90))+
  labs(x = "Proportion of Median\n G6PD Activity", 
       y = "Count")


histo_1 <- list(hist_tot, hist_m, hist_f) %>% 
  purrr::set_names("total_1", "men_1", "women_1")

walk2(histo_1, names(histo_1), 
      ~ggsave(.x, filename = paste0(here::here(.y), ".png"), dpi = 700))
