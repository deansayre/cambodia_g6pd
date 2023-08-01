pacman::p_load(here, 
               tidyverse)

set.seed(45616)

hb_dist <- rnorm(n = 10000, mean = 16, sd = 2) %>% 
  as_tibble() %>% 
  mutate(norm = factor(ifelse(value <= 20 & value >= 12, 1, 0)))

amm <- median(hb_dist$value)
primaquine <- 0.3*amm

taf <- 0.7*amm

pal <- c("#7d3834", "grey")

dist <- ggplot(hb_dist)+
  geom_bar(aes(x=value), 
           fill = "grey",
           color = "black")+
  scale_x_binned(breaks = seq(0,24, 1), 
                 limits = c(0, 24),
                 show.limits = TRUE)+
  coord_cartesian(xlim = c(0, 24))+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90))

abnormal <- ggplot(hb_dist)+
  geom_bar(aes(x=value, fill = norm), 
                 color = "black")+
  scale_x_binned(breaks = seq(0,24, 1), 
                 limits = c(0, 24),
                 show.limits = TRUE)+
  scale_fill_manual(values = pal)+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90))

ggsave(dist, filename = here::here("generic_distribution.png"), 
       dpi = 700)
ggsave(abnormal, filename = here::here("abnormal_distribution.png"), 
       dpi = 700)

