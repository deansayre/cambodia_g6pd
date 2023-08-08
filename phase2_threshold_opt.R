pacman::p_load(cutpointr,
               plotROC,
               pROC)

pal_9 <- c("#d3d3d3", "#b6aa8c", "#998246", "#c098b9", "#a57a7b", "#8b5e3d", "#ad5b9c", "#954968", "#7d3834")
pal_9a <- c("#d3d3d3", "#C3AE7B", "#6F5E33", 
            "#C16F6B", "#d3d3d3", "#C3AE7B", 
            "#7d3834", "#C16F6B", "#d3d3d3")

phase2_r_male_bin <- test %>% 
  filter(sex == 1) %>% 
  mutate(spec_dx_def_bin = factor(ifelse(is.na(spec_frac), NA_character_,
                                         ifelse(spec_frac <0.3, "1", "0"))), 
         spec_dx_inter_bin = factor(ifelse(is.na(spec_frac), NA_character_, 
                                           ifelse(spec_frac <=0.7, "1", "0"))))


dummy <- cutpointr(phase2_r_male_bin, g6pd_cap_field, spec_dx_def_bin, direction = "<=", 
                   method = maximize_metric, metric = youden, na.rm = TRUE) %>% 
  as_tibble()
dummy1 <- pull(dummy,2)
dummy2 <- cutpointr(phase2_r_male_bin, g6pd_cap_field, spec_dx_inter_bin, direction = "<=", 
                    method = maximize_metric, metric = youden, na.rm = TRUE)
dummy2 <- pull(dummy2, 2)

opt_men_df <- tibble::enframe(scatter(phase2_r_male_bin, 0.3, poc_thresh = 4.5, 
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

# Hmisc::describe(phase2_r_male_bin$g6pd_cap_field)

phase2_r_male_severe_spec <-  test %>% 
  filter(sex == 1) %>% 
  filter(spec_frac < 0.3)

phase2_r_male_inter_spec <-  test %>% 
  filter(sex == 1) %>% 
  filter(spec_frac >= 0.3, 
         spec_frac <= 0.7)

phase2_r_male_norm_spec <-  test %>% 
  filter(sex == 1) %>% 
  filter(spec_frac > 0.7)

a <- ggplot(phase2_r_male_norm_spec)+
  geom_histogram(aes(x = g6pd_cap_field), fill = "white", color = "black", 
                 binwidth=0.5, boundary = 0,closed = "left")+
  theme_minimal()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank())+
  coord_cartesian(xlim = c(0, 25), ylim = c(0,50))+
  labs(x = "", 
       y = "")


b <- ggplot(phase2_r_male_inter_spec)+
  geom_histogram(aes(x = g6pd_cap_field), fill = "grey50", alpha = 0.5,
                 color = "black", 
                 binwidth=0.5, boundary = 0,closed = "left")+
  theme_minimal()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank())+
  coord_cartesian(xlim = c(0, 25), ylim = c(0,50))+
  labs(x = "", 
       y = "")

c <- ggplot(phase2_r_male_severe_spec)+
  geom_histogram(aes(x = g6pd_cap_field), fill = pal_9[9], color = "black", 
                 binwidth=0.5, boundary = 0,closed = "left")+
  theme_minimal()+
  coord_cartesian(xlim = c(0, 25), ylim = c(0,50))+
  labs(x = "G6PD Activity by POC assay (U/g Hg)", 
       y = "")

men_strt <- cowplot::plot_grid(a,b,c, ncol = 1, align = "v")

ggsave(men_strt, filename = "phase2_men_strat.png", 
       width = 12,
       height = 6,
       units = "in")




phase2_r_f_severe_spec <-  test %>% 
  filter(sex == 2) %>% 
  filter(spec_frac < 0.3)

phase2_r_f_inter_spec <-  test %>% 
  filter(sex == 2) %>% 
  filter(spec_frac >= 0.3, 
         spec_frac <= 0.7)

phase2_r_f_norm_spec <-  test %>% 
  filter(sex == 2) %>% 
  filter(spec_frac > 0.7)

a <- ggplot(phase2_r_f_norm_spec)+
  geom_histogram(aes(x = g6pd_cap_field), fill = "white", color = "black", 
                 binwidth=0.5, boundary = 0,closed = "left")+
  theme_minimal()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank())+
  coord_cartesian(xlim = c(0, 25), ylim = c(0,50))+
  labs(x = "", 
       y = "")


b <- ggplot(phase2_r_f_inter_spec)+
  geom_histogram(aes(x = g6pd_cap_field), fill = "grey50", alpha = 0.5,
                 color = "black", 
                 binwidth=0.5, boundary = 0,closed = "left")+
  theme_minimal()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank())+
  coord_cartesian(xlim = c(0, 25), ylim = c(0,50))+
  labs(x = "", 
       y = "")

c <- ggplot(phase2_r_f_severe_spec)+
  geom_histogram(aes(x = g6pd_cap_field), fill = pal_9[9], color = "black", 
                 binwidth=0.5, boundary = 0,closed = "left")+
  theme_minimal()+
  coord_cartesian(xlim = c(0, 25), ylim = c(0,50))+
  labs(x = "G6PD Activity by POC assay (U/g Hg)", 
       y = "")

women_strt <- cowplot::plot_grid(a,b,c, ncol = 1, align = "v")

ggsave(women_strt, filename = "phase2_women_strat.png", 
       width = 12,
       height = 6,
       units = "in")


phase2_r_f_bin <- test %>% 
  filter(sex == 2) %>% 
  mutate(spec_dx_def_bin = factor(ifelse(is.na(spec_frac), NA_character_,
                                         ifelse(spec_frac <0.3, "1", "0"))), 
         spec_dx_inter_bin = factor(ifelse(is.na(spec_frac), NA_character_, 
                                           ifelse(spec_frac <=0.7, "1", "0"))))


dummy <- cutpointr(phase2_r_f_bin, g6pd_cap_field, spec_dx_def_bin, direction = "<=", 
                   method = maximize_metric, metric = youden, na.rm = TRUE) %>% 
  as_tibble()
dummy1 <- pull(dummy,2)
dummy2 <- cutpointr(phase2_r_f_bin, g6pd_cap_field, spec_dx_inter_bin, direction = "<=", 
                    method = maximize_metric, metric = youden, na.rm = TRUE)
dummy2 <- pull(dummy2, 2)

opt_f_df <- tibble::enframe(scatter(phase2_r_f_bin, 0.3, poc_thresh = 4.4, 
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

opt_f_df_2 <- tibble::enframe(scatter(phase2_r_f_bin, 0.7, poc_thresh = 6, 
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


phase2_r_roc <- phase2_r_comp %>% 
  drop_na(spec_dx) %>% 
  mutate(dx_severe = ifelse(spec_dx == "sev" | spec_dx == "def", 1,0), 
         dx_inter_severe = ifelse(spec_dx == "sev" | spec_dx == "def" | spec_dx == "int",
                                  1, 0))

true_neg <- phase2_r_roc %>% 
  filter(dx_severe == 0) %>% 
  summarise(true_negs = n(), 
            test_negs = sum(g6pd_cap_field >= 4, na.rm = TRUE))

spec_severe <- 1-true_neg$test_negs/true_neg$true_negs

severe_roc <- ggplot(phase2_r_roc)+
  geom_vline(xintercept = spec_severe, color = "#7d3834")+
  geom_roc(aes(d = dx_severe, m = g6pd_cap_field), 
           increasing = FALSE, n.cuts = 0)+ 
  style_roc(theme = theme_minimal, 
            xlab = "1 - Specificity", 
            ylab = "Sensitivity")

ggsave(severe_roc, filename = "roc_severe.png", 
       width = 6,
       height = 6,
       units = "in")

roc_severe <- phase2_r_roc %>% 
  pROC::roc(dx_severe, g6pd_cap_field)
auc_severe <- pROC::auc(roc_severe)
ci <- ci.auc(roc_severe, conf.level=0.95)


women_true_neg <- phase2_r_roc %>% 
  filter(sex == 2 & dx_inter_severe == 0)%>% 
  summarise(true_negs = n(), 
            test_negs = sum(g6pd_cap_field >= 7, na.rm = TRUE))

women_inter <- phase2_r_roc %>% 
  filter(sex == 2) 

spec_inger <- 1-women_true_neg$test_negs/women_true_neg$true_negs

inter_roc <- ggplot(women_inter)+
  geom_vline(xintercept = spec_inger, color = "#7d3834")+
  geom_roc(aes(d = dx_inter_severe, m = g6pd_cap_field), 
           increasing = FALSE, n.cuts = 0)+ 
  style_roc(theme = theme_minimal, 
            xlab = "1 - Specificity", 
            ylab = "Sensitivity")

ggsave(inter_roc, filename = "roc_inter.png", 
       width = 6,
       height = 6,
       units = "in")

roc_inter <- women_inter %>% 
  pROC::roc(dx_inter_severe, g6pd_cap_field)
auc_inter <- pROC::auc(roc_inter)
ci <- ci.auc(roc_inter, conf.level=0.95)

