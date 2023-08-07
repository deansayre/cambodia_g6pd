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

scatter <- function(x) {0}
scatter_dual <- function(x) {0}
insertSource(here::here("phase1_scatter.R"), functions=c("scatter", "scatter_dual") 
scatter
scatter_dual

scatter5 <- scatter_dual(b1, 0.3, poc_thresh = 4, xcol = TRUE, ycol = TRUE)
scatter6 <- scatter_dual(b1, 0.3, poc_thresh = 6, xcol = TRUE, ycol = TRUE)
scatter7 <- scatter_dual(b1, 0.7, poc_thresh = 6, xcol = TRUE, ycol = TRUE)

scatter_list <- list(scatter5, scatter6, scatter7)


walk2(scatter_list, list("scatter5", "scatter6", "scatter7"), 
      ~ggsave(filename = paste0(here::here(.y), ".png"), .x, 
              width = 6,
              height = 6,
              units = "in"))