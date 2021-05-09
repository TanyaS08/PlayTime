devtools::install_github("djnavarro/flametree")
library(flametree)
library(tidyverse)

set.seed(3)

layout <- "
ABDE
ACCE
"

#frog
p1 = flametree_grow(time = 13,
                    scale = c(0.0468, 0.675, 0.730, 0.461),
                    angle = c(1, -32, 41, 22)) %>% 
  flametree_plot(palette = c(
    "#65b4e6", 
    "#f29472"  
  ), 
  style = "wisp")

#panama
p2 = flametree_grow(time = 12,
                    scale = c(1.03, 0.622, 0.711, 0.409),
                    angle = c(1, 32, -83, 31)) %>% 
  flametree_plot(palette = c(
    "#f235b0", 
    "#65b4e6" 
  ), 
  style = "wisp")

#plank
p3 = flametree_grow(trees = 15,
                    scale = c(5.71, 1.47, 1.51, 0.655),
                    angle = c(211, 277, -203, -22)) %>% 
  flametree_plot(palette = c(
    "#d4f294", 
    "#f235b0" 
  ), 
  style = "nativeflora")

p3w = flametree_grow(trees = 15,
                    scale = c(5.71, 1.47, 1.51, 0.655),
                    angle = c(211, 277, -203, -22)) %>% 
  flametree_plot(palette = c(
    "#d4f294", 
    "#f235b0" 
  ), 
   style = "whisp")

#az
p4 = flametree_grow(time = 15,
                    scale = c(0.206, 0.9, 0.506, 1.26),
                    angle = c(-1, -69, -16, 72)) %>% 
  flametree_plot(palette = c(
    "#d4f294", 
    "#65b4e6" 
  ), 
  style = "wisp")

#herb
p5 = flametree_grow(time = 14,
                    scale = c(0.409, 0.711, 0.622, 1.03),
                    angle = c(32, 32, 83, 1)) %>% 
  flametree_plot(palette = c(
    "#d4f294", 
    "#f29472"  
  ), 
  style = "wisp")

p =
  p1 + p2 + p3 + p4 + p5 +
  plot_layout(design = layout) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "black", colour = NA),
    panel.background = element_rect(fill = "black", colour = NA)))

beepr::beep(sound = 8)

ggsave("flametree1.png", p1, width = 7, height = 10, units = "in")
ggsave("flametree2.png", p2, width = 30, height = 10, units = "in")
ggsave("flametree3.png", p3, width = 30, height = 10, units = "in")
ggsave("flametree3wisp.png", p3w, width = 30, height = 10, units = "in")
ggsave("flametree4.png", p4, width = 30, height = 10, units = "in")
ggsave("flametree5.png", p5, width = 30, height = 10, units = "in")
ggsave("flametree_all.png", p, width = 30, height = 10, units = "in")



