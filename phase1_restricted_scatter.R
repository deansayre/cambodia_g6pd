test <- rio::import("ipc_data/update final data 28 10 2020.xlsx") %>% 
  Rtesunate::act_clean() %>% 
  mutate(id = as.double(id))
  
b <- psi_c %>% 
    dplyr::select(id, sex, date) %>% 
  mutate(id = factor(id)) %>% 
    left_join(test, by = "id") %>% 
    drop_na(g6pd_ipc_spectro) %>% 
    mutate(outlier_hb = ifelse(hb_biosensor_onsite < 7, 1, 0), 
           lag = as.numeric(date_g6pd_spectro_ipc - date)) %>% 
    dplyr::filter(!is.na(g6pd_ipc_spectro))

Hmisc::describe(b$lag)

b1 <- b %>% 
  mutate(frac_spec = g6pd_ipc_spectro/amm_spec) %>% 
  filter(frac_spec <3 & lag < 4) 

b_f <- b1 %>% 
  filter(sex == "female") 

scatter <- function(x) {0}
scatter_dual <- function(x) {0}
insertSource(here::here("phase1_scatter.R"), functions=c("scatter", "scatter_dual")) 
scatter
scatter_dual

scatter5 <- scatter_dual(b1, 0.3, poc_thresh = 4, xcol = TRUE, ycol = TRUE)
scatter6 <- scatter_dual(b1, 0.3, poc_thresh = 6, xcol = TRUE, ycol = TRUE)
scatter7 <- scatter_dual(b_f, 0.7, poc_thresh = 6, xcol = TRUE, ycol = TRUE)

scatter_list <- list(scatter5, scatter6, scatter7)


walk2(scatter_list, list("scatter5", "scatter6", "scatter7"), 
      ~ggsave(filename = paste0(here::here(.y), ".png"), .x, 
              width = 6,
              height = 6,
              units = "in"))

scatter6_df <- tibble::enframe(scatter(b1, 0.3, poc_thresh = 6, 
                                       xcol = TRUE, ycol = TRUE)[[2]]) %>% 
  mutate(ci_low = as.numeric(ifelse(str_detect(value, "-"), word(value, 1, sep = "-"), NA)), 
         ci_high= as.numeric(ifelse(str_detect(value, "-"), word(value, 2, sep = "-"), NA)), 
         ci_low = lead(ci_low), 
         ci_high = lead(ci_high), 
         value = as.numeric(value), 
         name = factor(name, levels = c("Sensitivity", 
                                        "Specificity", 
                                        "PPV", 
                                        "NPV"))) %>% 
  drop_na(ci_low)

phase1_all <- ggplot(scatter6_df)+
  geom_col(aes(x = name, y = value))+
  geom_errorbar(aes(x = name, group = name, ymin = ci_low, ymax = ci_high), 
               width = 0.2)+
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()+
  labs(x = element_blank(), 
       y = element_blank())

ggsave(phase1_all, filename = "phase1_histo_all.png",
              width = 6,
              height = 6,
              units = "in")

scatter7_df <- tibble::enframe(scatter(b_f, 0.7, poc_thresh = 6, 
                                       xcol = TRUE, ycol = TRUE)[[2]]) %>% 
  mutate(ci_low = as.numeric(ifelse(str_detect(value, "-"), word(value, 1, sep = "-"), NA)), 
         ci_high= as.numeric(ifelse(str_detect(value, "-"), word(value, 2, sep = "-"), NA)), 
         ci_low = lead(ci_low), 
         ci_high = lead(ci_high), 
         value = as.numeric(value), 
         name = factor(name, levels = c("Sensitivity", 
                                        "Specificity", 
                                        "PPV", 
                                        "NPV"))) %>% 
  drop_na(ci_low)

phase1_f_inter <- ggplot(scatter7_df)+
  geom_col(aes(x = name, y = value))+
  geom_errorbar(aes(x = name, group = name, ymin = ci_low, ymax = ci_high), 
                width = 0.2)+
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()+
  labs(x = element_blank(), 
       y = element_blank())

ggsave(phase1_f_inter, filename = "phase1_histo_women.png",
       width = 6,
       height = 6,
       units = "in")
