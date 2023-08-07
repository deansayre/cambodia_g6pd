source("cambodia_import.R")


phase2_men <- phase2 %>%    # Spec cutoffs uses all recorded spectrophotometry measurements  
  filter(sex == 1 & !is.na(g6pd_ven_spect))

med_spec_2 <- median(phase2_men$g6pd_ven_spect, na.rm = T)       
cut_spec_2 <- 0.1*med_spec_2                       
amm_spec_2 <- median(phase2_men$g6pd_ven_spect[phase2_men$g6pd_ven_spect > 
                                                 cut_spec_2], na.rm = T)

phase2_valid <- phase2 %>% 
  mutate(g6pd_cap_field = ifelse(hb_cap_field <= 7, NA, g6pd_cap_field), 
         g6pd_ven_field = ifelse(hb_ven_field <= 7, NA, g6pd_ven_field), 
         g6pd_ven_poc_lab = ifelse(hb_ven_poc_lab <= 7, NA, g6pd_ven_poc_lab))



phase2_r_dx <- phase2_valid %>% 
  mutate(spec_frac = g6pd_ven_spect/amm_spec_2, 
         spec_dx = factor(ifelse(is.na(g6pd_ven_spect), NA_character_, 
                                 case_when(spec_frac < 0.3 ~ "1", 
                                           spec_frac <= 0.7 ~ "2", 
                                           TRUE ~ "98"))),
         g6pd_cap_field_dx = case_when(is.na(g6pd_cap_field) ~ NA_character_,
                                       sex == 1 & g6pd_cap_field <= 4 ~ "def", 
                                       sex == 1 ~ "norm", 
                                       sex == 2 & g6pd_cap_field <= 4 ~ "sev", 
                                       sex == 2 & g6pd_cap_field <= 6 ~ "int",
                                       sex == 2 ~ "norm", 
                                       TRUE ~ "error"), 
         g6pd_ven_field_dx = case_when(is.na(g6pd_ven_field) ~ NA_character_,
                                       sex == 1 & g6pd_ven_field <= 4 ~ "def", 
                                       sex == 1 ~ "norm", 
                                       sex == 2 & g6pd_ven_field <= 4 ~ "sev", 
                                       sex == 2 & g6pd_ven_field <= 6 ~ "int",
                                       sex == 2 ~ "norm", 
                                       TRUE ~ "error"), 
         g6pd_ven_poc_lab_dx = case_when(is.na(g6pd_ven_poc_lab) ~ NA_character_,
                                         sex == 1 & g6pd_ven_poc_lab <= 4 ~ "def", 
                                         sex == 1 ~ "norm", 
                                         sex == 2 & g6pd_ven_poc_lab <= 4 ~ "sev", 
                                         sex == 2 & g6pd_ven_poc_lab <= 6 ~ "int",
                                         sex == 2 ~ "norm", 
                                         TRUE ~ "error"), 
         spec_dx = case_when(is.na(spec_dx) ~ NA_character_,
                             spec_dx == 1 & sex == 1 ~ "def", 
                             sex == 1 & (spec_dx == 98 | spec_dx == 2)  ~ "norm", 
                             sex == 2 & spec_dx == 1 ~ "sev", 
                             sex == 2 & spec_dx == 2 ~ "int", 
                             sex == 2 & spec_dx == 98 ~ "norm", 
                             TRUE ~ "error"), 
         g6pd_cap_field_dx_kappa = case_when(is.na(g6pd_cap_field_dx) ~ NA_real_,
                                             sex == 1 & g6pd_cap_field_dx == "def" ~ 2, 
                                             sex == 1 & g6pd_cap_field_dx == "norm" ~ 1, 
                                             sex == 2 & g6pd_cap_field_dx ==  "sev" ~ 3, 
                                             sex == 2 & g6pd_cap_field_dx ==  "int" ~ 2,
                                             sex == 2 & g6pd_cap_field_dx == "norm" ~ 1, 
                                             TRUE ~ 98), 
         g6pd_ven_field_dx_kappa = case_when(is.na(g6pd_ven_field_dx) ~ NA_real_,
                                             sex == 1 & g6pd_ven_field_dx == "def" ~ 2, 
                                             sex == 1 & g6pd_ven_field_dx == "norm" ~ 1, 
                                             sex == 2 & g6pd_ven_field_dx == "sev" ~ 3, 
                                             sex == 2 & g6pd_ven_field_dx == "int" ~ 2,
                                             sex == 2 & g6pd_ven_field_dx == "norm" ~ 1, 
                                             TRUE ~ 98), 
         g6pd_ven_poc_lab_dx_kappa = case_when(is.na(g6pd_ven_poc_lab_dx) ~ NA_real_,
                                               sex == 1 & g6pd_ven_poc_lab_dx == "def" ~ 2, 
                                               sex == 1 & g6pd_ven_poc_lab_dx ==  "norm" ~ 1, 
                                               sex == 2 & g6pd_ven_poc_lab_dx ==  "sev" ~ 3, 
                                               sex == 2 & g6pd_ven_poc_lab_dx ==  "int" ~ 2,
                                               sex == 2 & g6pd_ven_poc_lab_dx ==  "norm" ~ 1,
                                               TRUE ~ 98), 
         spec_dx_kappa = case_when(is.na(spec_dx) ~ NA_real_,
                                   sex == 1 & spec_dx == "def" ~ 2, 
                                   sex == 1 & spec_dx == "norm" ~ 1, 
                                   sex == 2 & spec_dx == "sev" ~ 3, 
                                   sex == 2 & spec_dx == "int" ~ 2, 
                                   sex == 2 & spec_dx == "norm" ~ 1, 
                                   TRUE ~ 98))



# for analyses requiring numeric values (i.e., cannot use 'high')
phase2_r_comp <- phase2_r_dx %>%   
  mutate(across(c(g6pd_cap_field, g6pd_ven_field, g6pd_ven_poc_lab), 
                ~ifelse(.x > 20, NA_real_, .x))) %>% 
  mutate(log_g6pd_ven_spect = log10(g6pd_ven_spect), 
         log_g6pd_cap_field = log10(g6pd_cap_field), 
         diff_from_ref = g6pd_cap_field - g6pd_ven_spect, 
         perc_diff_from_ref = 100*(g6pd_cap_field - g6pd_ven_spect)/g6pd_ven_spect)




hist_breaks <- seq(0, 2.5, by = 0.1)

phase2_m <- phase2_r_dx %>% 
  filter(sex == 1)

phase2_f <- phase2_r_dx %>% 
  filter(sex == 2)


hist_tot_2 <- ggplot(phase2_r_dx)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 170, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 170, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=spec_frac),color = "white", breaks = hist_breaks)+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 160))+
  labs(x = "Proportion of Median\n G6PD Activity", 
       y = "Count")

sum(phase2_r_dx$spec_frac < 0.3, na.rm = TRUE)/sum(!is.na(phase2_r_dx$spec_frac))
sum(phase2_r_dx$spec_frac >= 0.3 & phase2_r_dx$spec_frac <0.7 , na.rm = TRUE)/
  sum(!is.na(phase2_r_dx$spec_frac))

hist_m_2 <- ggplot(phase2_m)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 140, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 140, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=spec_frac),color = "white", breaks = hist_breaks)+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 120))+
  labs(x = "Proportion of Median\n G6PD Activity", 
       y = "Count")

sum(phase2_m$spec_frac < 0.3, na.rm = TRUE)/sum(!is.na(phase2_m$spec_frac))
sum(phase2_m$spec_frac >= 0.3 & phase2_m$spec_frac <0.7 , na.rm = TRUE)/
  sum(!is.na(phase2_m$spec_frac))

hist_f_2 <- ggplot(phase2_f)+
  annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 140, 
           fill = "#7d3834", alpha = 0.5)+
  annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 140, 
           fill = "grey50", alpha = 0.5)+
  geom_histogram(aes(x=spec_frac),color = "white", breaks = hist_breaks)+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 120))+
  labs(x = "Proportion of Median\n G6PD Activity", 
       y = "Count")

sum(phase2_f$spec_frac < 0.3, na.rm = TRUE)/sum(!is.na(phase2_f$spec_frac))
sum(phase2_f$spec_frac >= 0.3 & phase2_f$spec_frac <0.7 , na.rm = TRUE)/
  sum(!is.na(phase2_f$spec_frac))

histo_2 <- list(hist_tot_2, hist_m_2, hist_f_2) %>% 
  purrr::set_names("total_2", "men_2", "women_2")

walk2(histo_2, names(histo_2), 
      ~ggsave(.x, filename = paste0(here::here(.y), ".png"), dpi = 700))


phase2_r_dx <- phase2_r_dx %>% 
  mutate(g6pd_cap_field = as.numeric(g6pd_cap_field)) %>% 
  filter(g6pd_cap_field < 50)

scatter <- function(df, spec_thresh, poc_thresh, xcol, ycol, 
                    metric){
  
  df1 <- df %>% 
    mutate(dx_spec = factor(ifelse(spec_frac < spec_thresh, 1, 0)), 
           dx_poc = factor(ifelse(g6pd_cap_field < poc_thresh, 
                                  1, 0)))
  
  if(ycol == TRUE){
    scatter <- ggplot(df1)+
      annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 50, 
               fill = "#7d3834", alpha = 0.5)+
      annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 50, 
               fill = "grey50", alpha = 0.5)+
      annotate(geom = "rect", xmin = 0, xmax = 2.5, ymin = 0, ymax = poc_thresh, 
               fill = "#423400", alpha = 0.5)+
      geom_point(aes(x = spec_frac, y = g6pd_cap_field))+
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
      geom_point(aes(x = spec_frac, y = g6pd_cap_field))+
      theme_minimal()+
      scale_x_continuous(limits = c(0, 2.5))+
      coord_cartesian(ylim = c(0,22))+
      labs(x = "Proportion of Median G6PD Activity", 
           y = "POC G6PD Activity")
  }
  
  if(ycol == TRUE){
    p_x <- ggplot(df1)+
      annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 130, 
               fill = "#7d3834", alpha = 0.5)+
      annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 130, 
               fill = "grey50", alpha = 0.5)+
      geom_histogram(aes(x=spec_frac, fill = as.factor(dx_poc)), color = "black")+
      scale_fill_manual(values = c("grey50", "#423400"))+
      theme_minimal()+
      theme(legend.position = "none")+
      scale_x_continuous(limits = c(0, 2.5))+
      labs (x = element_blank(),
            y = element_blank())+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
  } else{
    p_x <- ggplot(df1)+
      annotate(geom = "rect", xmin = 0, xmax = 0.3, ymin = 0, ymax = 130, 
               fill = "#7d3834", alpha = 0.5)+
      annotate(geom = "rect", xmin = 0.3, xmax = 0.7, ymin = 0, ymax = 130, 
               fill = "grey50", alpha = 0.5)+
      geom_histogram(aes(x=spec_frac), 
                     fill = "grey50",
                     color = "black")+
      theme_minimal()+
      theme(legend.position = "none")+
      scale_x_continuous(limits = c(0, 2.5))+
      labs (x = element_blank(),
            y = element_blank())+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  if(ycol == TRUE){
    p_y <- ggplot(df1)+
      annotate(geom = "rect", xmin = 0, xmax = poc_thresh, 
               ymin = 0, ymax = 120, 
               fill = "#423400", alpha = 0.5)+
      geom_histogram(aes(x = g6pd_cap_field, 
                         fill = as.factor(dx_spec)), color = "black")+
      scale_fill_manual(values = c("grey50", "#7d3834"))+
      theme_minimal()+
      theme(legend.position = "none")+
      labs (x = element_blank(),
            y = element_blank())+
      theme(axis.text.x = element_blank(), 
            axis.text.y = element_blank()
          )+
      coord_flip(xlim = c(0,22), ylim = c(0,110))
  } else {
    if(xcol == TRUE){
      p_y <- ggplot(df1)+
        geom_histogram(aes(x = g6pd_cap_field, 
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
        geom_histogram(aes(x = g6pd_cap_field), 
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

test_f <- test %>% 
  filter(sex == 2)


scatter2_1 <- scatter_dual(df = test, 0.3, poc_thresh = 4, 
                           xcol = TRUE, 
                           ycol = TRUE)
scatter2_2 <- scatter_dual(df = test, 0.3, poc_thresh = 6, 
                           xcol = TRUE, 
                           ycol = TRUE)
scatter2_3 <- scatter_dual(df = test_f, 0.7, poc_thresh = 6, 
                           xcol = TRUE, 
                           ycol = TRUE)


scatter2_2_df <- tibble::enframe(scatter(test, 0.3, poc_thresh = 6, 
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

scatter2_3_df <- tibble::enframe(scatter(test_f, 0.7, poc_thresh = 6, 
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

phase2_all <- ggplot(scatter2_2_df)+
  geom_col(aes(x = name, y = value))+
  geom_errorbar(aes(x = name, group = name, ymin = ci_low, ymax = ci_high), 
                width = 0.2)+
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()+
  labs(x = element_blank(), 
       y = element_blank())

phase2_inter_f <- ggplot(scatter2_3_df)+
  geom_col(aes(x = name, y = value))+
  geom_errorbar(aes(x = name, group = name, ymin = ci_low, ymax = ci_high), 
                width = 0.2)+
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()+
  labs(x = element_blank(), 
       y = element_blank())

scatter_list <- list(scatter2_1, scatter2_2, scatter2_3, phase2_all, phase2_inter_f)


walk2(scatter_list, list("scatter2_1", "scatter2_2", "scatter2_3", "bar_all", "bar_inter_f"), 
      ~ggsave(filename = paste0(here::here(.y), ".png"), .x, 
              width = 6,
              height = 6,
              units = "in"))

