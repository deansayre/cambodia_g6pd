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
  filter(frac_spec <3) 


scatter <- function(df, spec_thresh, poc_thresh, xcol, ycol, 
                    metric){
  
df1 <- df %>% 
  mutate(dx_spec = factor(ifelse(frac_spec < spec_thresh, 1, 0)), 
         dx_poc = factor(ifelse(g6pd_results_biosensor_onsite < poc_thresh, 
                                1, 0)))

if(ycol == TRUE){
scatter <- ggplot(df1)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 50, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 50, 
           fill = "grey50", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0, xmax = 2.5, ymin = 0, ymax = poc_thresh, 
           fill = "#423400", alpha = 0.5)+
  geom_point(aes(x = frac_spec, y = g6pd_results_biosensor_onsite))+
  theme_minimal()+
  scale_x_continuous(limits = c(0, 2.5))+
  coord_cartesian(ylim = c(0,22))+
  labs(x = "Proportion of Median G6PD Activity", 
       y = "POC G6PD Activity")
} else{
  scatter <- ggplot(df1)+
    annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 50, 
             fill = "#7d3834", alpha = 0.5)+
    annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 50, 
             fill = "grey50", alpha = 0.5)+
    geom_point(aes(x = frac_spec, y = g6pd_results_biosensor_onsite))+
    theme_minimal()+
    scale_x_continuous(limits = c(0, 2.5))+
    coord_cartesian(ylim = c(0,22))+
    labs(x = "Proportion of Median G6PD Activity", 
         y = "POC G6PD Activity")
}

if(ycol == TRUE){
p_x <- ggplot(df1)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 120, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 120, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=frac_spec, fill = as.factor(dx_poc)), color = "black")+
  scale_fill_manual(values = c("grey50", "#423400"))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0, 2.5))+
  labs (x = element_blank(),
        y = element_blank())+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
} else(
  p_x <- ggplot(df1)+
    annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 120, 
             fill = "#7d3834", alpha = 0.5)+
    annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 120, 
             fill = "grey50", alpha = 0.5)+
    geom_histogram(aes(x=frac_spec), 
                   fill = "grey50",
                   color = "black")+
    theme_minimal()+
    theme(legend.position = "none")+
    scale_x_continuous(limits = c(0, 2.5))+
    labs (x = element_blank(),
          y = element_blank())+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
)
if(ycol == TRUE){
p_y <- ggplot(df1)+
  annotate(geom = "rect", xmin = 0, xmax = poc_thresh, 
           ymin = 0, ymax = 120, 
           fill = "#423400", alpha = 0.5)+
  geom_histogram(aes(x = g6pd_results_biosensor_onsite, 
                     fill = as.factor(dx_spec)), color = "black")+
  scale_fill_manual(values = c("grey50", "#7d3834"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs (x = element_blank(),
        y = element_blank())+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())+
  coord_flip(xlim = c(0,22), ylim = c(0,110))
} else {
  if(xcol == TRUE){
  p_y <- ggplot(df1)+
    geom_histogram(aes(x = g6pd_results_biosensor_onsite, 
                       fill = as.factor(dx_spec)), color = "black")+
    scale_fill_manual(values = c("grey50", "#7d3834"))+
    theme_minimal()+
    theme(legend.position = "none")+
    labs (x = element_blank(),
          y = element_blank())+
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank())+
    coord_flip(xlim = c(0,22), ylim = c(0,110))
  } else {
    p_y <- ggplot(df1)+
      geom_histogram(aes(x = g6pd_results_biosensor_onsite), 
                     fill = "grey50", 
                     color = "black")+
      theme_minimal()+
      theme(legend.position = "none")+
      labs (x = element_blank(),
            y = element_blank())+
      theme(axis.text.x = element_blank(), 
            axis.text.y = element_blank())+
      coord_flip(xlim = c(0,22), ylim = c(0,110))
  }
}

graph <- cowplot::plot_grid(p_x, NULL, NULL, 
                   NULL, NULL, NULL,
                   scatter, NULL, p_y, 
                   align = "hv", axis = "tbrl",
                   rel_widths = c(3,-0.2, 1), 
                   rel_heights = c(1,-0.2, 3))
metrics <- setNames(c(round(sum(df1$dx_poc == 1 & df1$dx_spec == 1, na.rm = TRUE)/
               sum(df1$dx_spec == 1, na.rm = TRUE), 3),
               paste0(round(confintr::ci_proportion(sum(df1$dx_poc == 1 & df1$dx_spec == 1, na.rm = TRUE),
                                       sum(df1$dx_spec == 1, na.rm = TRUE))[["interval"]][[1]], 3),
                      "-", 
                      round(confintr::ci_proportion(sum(df1$dx_poc == 1 & df1$dx_spec == 1, na.rm = TRUE),
                                              sum(df1$dx_spec == 1, na.rm = TRUE))[["interval"]][[2]],3)
                      ),
             round(sum(df1$dx_poc == 0 & df1$dx_spec == 0, na.rm = TRUE)/
               sum(df1$dx_spec == 0, na.rm = TRUE), 3),
             paste0(round(confintr::ci_proportion(sum(df1$dx_poc == 0 & df1$dx_spec == 0, na.rm = TRUE),
                                                  sum(df1$dx_spec == 0, na.rm = TRUE))[["interval"]][[1]], 3),
                    "-", 
                    round(confintr::ci_proportion(sum(df1$dx_poc == 0 & df1$dx_spec == 0, na.rm = TRUE),
                                                  sum(df1$dx_spec == 0, na.rm = TRUE))[["interval"]][[2]],3)
             ),
             round(sum(df1$dx_poc == 1 & df1$dx_spec == 1, na.rm = TRUE)/
               sum(df1$dx_poc == 1, na.rm = TRUE), 3), 
             paste0(round(confintr::ci_proportion(sum(df1$dx_poc == 1 & df1$dx_spec == 1, na.rm = TRUE),
                                                  sum(df1$dx_poc == 1, na.rm = TRUE))[["interval"]][[1]], 3),
                    "-", 
                    round(confintr::ci_proportion(sum(df1$dx_poc == 1 & df1$dx_spec == 1, na.rm = TRUE),
                                                  sum(df1$dx_poc == 1, na.rm = TRUE))[["interval"]][[2]],3)
             ),
             round(sum(df1$dx_poc == 0 & df1$dx_spec == 0, na.rm = TRUE)/
               sum(df1$dx_poc == 0, na.rm = TRUE),3),
             paste0(round(confintr::ci_proportion(sum(df1$dx_poc == 0 & df1$dx_spec == 0, na.rm = TRUE),
                                                                                      sum(df1$dx_poc == 0, na.rm = TRUE))[["interval"]][[1]], 3),
                                                        "-", 
                                                        round(confintr::ci_proportion(sum(df1$dx_poc == 0 & df1$dx_spec == 0, na.rm = TRUE),
                                                                                      sum(df1$dx_poc == 0, na.rm = TRUE))[["interval"]][[2]],3)
               )), 
             c("Sensitivity", "Sens CI",
               "Specificity", "Spec CI",
               "PPV", "PPV CI",
               "NPV", "NPV CI"))

return(list(graph, metrics))

}

scatter_dual <- function(df, spec_thresh, poc_thresh, xcol, ycol, 
         metric){
  l <- suppressMessages(suppressWarnings(scatter(df, spec_thresh, poc_thresh, xcol, ycol, 
               metric)))
  print(l[[2]])
  return(l[[1]])
}

scatter1 <- scatter_dual(b1, 0.3, poc_thresh = 4, xcol = FALSE, ycol = FALSE)
scatter2 <- scatter_dual(b1, 0.3, poc_thresh = 4, xcol = FALSE, ycol = TRUE)
scatter3 <- scatter_dual(b1, 0.3, poc_thresh = 6, xcol = FALSE, ycol = TRUE)
scatter4 <- scatter_dual(b1, 0.7, poc_thresh = 6, xcol = FALSE, ycol = TRUE)

scatter_list <- list(scatter1, scatter2, scatter3, scatter4)


walk2(scatter_list, list("scatter1", "scatter2", "scatter3", "scatter4"), 
      ~ggsave(filename = paste0(here::here(.y), ".png"), .x, 
              width = 12,
              height = 6.25,
              units = "in"))

