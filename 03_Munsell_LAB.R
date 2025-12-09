# Test munsell dimensions vs LAB

library(magrittr)
library(dplyr)
library(tidyr)
library(munsell)
library(munsellinterpol)
library(spacesXYZ)
library(circular)
library(ggplot2)
library(spacesRGB)
library(ggrepel)
library(extras)
library(ggforce)
library(autoimage) # For rotating coordinates
library(tibble)
library(pcds)  # For intersecting lines and circles
library(colorscience)
library(colorspace)

?MunsellToLab

munsell_grid <- expand.grid(
  H = c(25, 50, 100),
  V = c(3, 5, 7),
  C = c(0, 2.5, 5)
) 

LAB_grid <- munsell_grid %>%
  as.matrix() %>%
  MunsellToLab() %>%
  as.data.frame() %>%
  rownames_to_column()

combined_grid <- bind_cols(munsell_grid, LAB_grid) %>%
  mutate(
    HC_x = tan(H*pi*2/100)*C,
    lab_intensity = sqrt(a^2 + b^2)
  )

combined_grid %>%
  group_by(C) %>%
  summarise(
    mean_intensity = mean(lab_intensity)
    ) %>%
  ungroup() %>%
  mutate(
    internsity_per_C = mean_intensity / C
  )

# L == Value*10 (approx)
# LAB intensity == C*5.65

10 / 5.65
# [1] 1.769912

# END