pacman::p_load(here, 
               tidyverse)

set.seed(45616)

abn <- rnorm(n = 2000, mean = 0.3, sd = 0.5) %>% 
  as_tibble() %>% 
  filter(value >= 0)

abn_f <- rnorm(n = 1000, mean = 0.3, sd = 3) %>% 
  as_tibble() %>% 
  filter(value >= 0)

abn_f_2 <- runif(n = 1000, min = 0, max = 8) %>% 
  as_tibble()  


hb_dist <- rnorm(n = 10000, mean = 12, sd = 2) %>% 
  as_tibble() %>% 
  bind_rows(abn) %>% 
  mutate(norm = factor(ifelse(value <= 8 | value >= 16, 1, 0)))

dist_f <- rnorm(n = 10000, mean = 12, sd = 2) %>% 
  as_tibble() %>% 
  bind_rows(abn_f) %>% 
  bind_rows(abn_f_2)

med <- median(hb_dist$value)

hb_dist1 <- filter(hb_dist, value > 0.1*med)

amm <- median(hb_dist1$value)

primaquine <- 0.3*amm

taf <- 0.7*amm

pal <- c("grey", "#7d3834")

dist <- ggplot(hb_dist)+
  geom_bar(aes(x=value), 
           fill = "grey",
           color = "black")+
  scale_x_binned(breaks = seq(0,20, 1), 
                 limits = c(0, 20),
                 show.limits = TRUE)+
  coord_cartesian(xlim = c(0, 20))+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

abnormal <- ggplot(hb_dist)+
  geom_bar(aes(x=value, fill = norm), 
                 color = "black")+
  scale_x_binned(breaks = seq(0,20, 1), 
                 limits = c(0, 20),
                 show.limits = TRUE)+
  scale_fill_manual(values = pal)+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

abnormal_f <- ggplot(dist_f)+
  geom_bar(aes(x=value, fill = "grey"), 
           color = "black")+
  scale_x_binned(breaks = seq(0,20, 1), 
                 limits = c(0, 20),
                 show.limits = TRUE)+
  scale_fill_manual(values = pal)+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(dist, filename = here::here("generic_distribution.png"), 
       dpi = 700)
ggsave(abnormal, filename = here::here("abnormal_distribution.png"), 
       dpi = 700)

ggsave(abnormal_f, filename = here::here("abnormal_distribution_f.png"), 
       dpi = 700)


