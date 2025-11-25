# Calculation and illustration of MUnsell colors for DMC floss

# To do:
# General:
# - Fix color labels/text positions [ok].
# - Make translations for Danish.
# - Indicate extent of color space on charts [?].
# Value slices:
# - Add hue labels to value slices ("radial" plots).
# - Add color ring to value slices.
# - Add darkest/lightest color slices.
# Hue slices:
# - Fix aspect ratio for hue slices [ok].
# Chroma slices:
# - Create chroma slices.
# - Figure for neutral colors.
# - Figure for most intense colors.

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
library(sf) # For minimum bounding circle
library(ggforce)
library(autoimage) # For rotating coordinates
library(tibble)
library(pcds)  # For intersecting lines and circles
library(shadowtext)  # For text outline
library(colorscience)
library(colorspace)

getwd()

dir <- getwd()

dir_results <- dir %>%
  dirname() %>%
  paste0(., "/DMC_results/") %T>%
  dir.create()

my_colors <- dir %>%
  paste0(., "/Floss_colors.txt") %>%
  read.table(
    dec = ",", 
    sep = "\t", 
    header = TRUE, 
    comment.char = ""
    ) %>%
  arrange(DMC)

head(my_colors)

discontinued <- c(504, 731, 776, 781, 806, 868, 971, 3773)

discontinued[!discontinued %in% my_colors$DMC]
# [1] 868

my_colors %<>% filter(
  !(DMC %in% discontinued)
)

color_ref <- dir %>%
  paste0(., "/Color_ref.txt") %>%
  read.table(
    dec = ",", 
    sep = "\t", 
    header = TRUE, 
    comment.char = ""
  )

# Standardize color references

color_ref %<>%
  mutate(
    R = case_when(
      !is.na(Hex) ~ col2rgb(Hex)[1,],
      .default = R
    ),
    G = case_when(
      !is.na(Hex) ~ col2rgb(Hex)[2,],
      .default = G
    ),
    B = case_when(
      !is.na(Hex) ~ col2rgb(Hex)[3,],
      .default = B
    )
  )

color_ref

cref_mean <- color_ref %>%
  group_by(Objekt, Reference) %>%
  summarise(
    R = mean(R),
    G = mean(G),
    B = mean(B)
  )

cref_mean

cref_dif <- cref_mean[1:4, -1:-2] / cref_mean[5:8, -1:-2]

cref_dif

cref_sum <- apply(cref_dif[1:3,], 2, mean)

cref_sum

cref_rgb <- 255*cref_sum/max(cref_sum)

cref_rgb

paper_rgb <- cref_mean[4, -1:-2] %>% unlist()

paper_rgb

cref_total <- (cref_rgb + paper_rgb)/2

cref_total

# Use Whibal rgb for standardization

Whibal_rgb <- c(192, 192, 192)

Whibal_rgb_logodds <- log_odds(Whibal_rgb/255)

Whibal_rgb_logodds

Whibal_rgb_cg <- unlist(cref_mean[8, -1:-2])

Whibal_rgb_cg

Whibal_rgb_cg_logodds <- log_odds(Whibal_rgb_cg/255)

Whibal_rgb_cg_logodds

Whibal_rgb_diff_linear <- Whibal_rgb_cg - Whibal_rgb

Whibal_rgb_diff_linear

Whibal_rgb_diff_logodds <- Whibal_rgb_cg_logodds - Whibal_rgb_logodds

Whibal_rgb_diff_logodds

# Standardize my own color measurements

my_colors$Hex_Ego %>%
  col2rgb() %>%
  t() %>%
  head()

my_colors$Hex_Ego %>%
  col2rgb() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select(red) %>%
  unlist() %>%
  hist()

my_colors$Hex_Ego %>%
  col2rgb() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select(green) %>%
  unlist() %>%
  hist()

my_colors$Hex_Ego %>%
  col2rgb() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select(blue) %>%
  unlist() %>%
  hist()

# RGB_ego_cal <- my_colors$Hex_Ego %>%
#   col2rgb() %>%
#   multiply_by(255) %>%
#   divide_by(cref_total) %>%
#   t()
 
# Use logit (log odds) standardization instead

RGB_ego_cal <- my_colors$Hex_Ego %>%
  col2rgb() %>%
  divide_by(255) %>%
  log_odds() %>%
  subtract(Whibal_rgb_diff_logodds) %>%
  inv_logit() %>%
  t() %>%
  multiply_by(255)

head(RGB_ego_cal)

RGB_ego_cal %>%
  as.data.frame() %>%
  filter(red > 255 | green > 255 | blue > 255)

RGB_ego_cal[, 1] %>% hist()
RGB_ego_cal[, 2] %>% hist()
RGB_ego_cal[, 3] %>% hist()

RGB_ego_cal_max <- apply(RGB_ego_cal, 2, max)

RGB_ego_cal_max

RGB_ego_cal_max[RGB_ego_cal_max < 255] <- 255

RGB_ego_cal2 <- RGB_ego_cal %>%
  t() %>%
  multiply_by(255) %>%
  divide_by(RGB_ego_cal_max) %>%
  # divide_by(max(RGB_ego_cal_max)) %>%
  t()

# RGB_ego_cal2 <- RGB_ego_cal

RGB_ego_cal2[RGB_ego_cal2 > 255] <- 255

head(RGB_ego_cal2)

plot(data.frame(RGB_ego_cal2))

HEX_ego_cal <- rgb(RGB_ego_cal2/255 - 0.00001)

my_colors$HEX_ego_cal <- HEX_ego_cal

head(my_colors)

# SP colors (kind of drab)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, fill = I(Hex_SP))) +
  geom_raster() +
  ggtitle("Hex_SP") +
  scale_y_reverse()

# SF colors (ok)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, fill = I(Hex_SF))) +
  geom_raster() +
  ggtitle("Hex_SF") +
  scale_y_reverse()

# CAZ colors (also kind of drab)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, fill = I(Hex_CAZ))) +
  geom_raster() +
  ggtitle("Hex_CAZ") +
  scale_y_reverse()

# My colors (most vivid)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, fill = I(HEX_ego_cal))) +
  geom_raster() +
  ggtitle("HEX_ego_cal") +
  scale_y_reverse()

# I first thought that I should only use SF and my own colors, as the other
# ones were too grayish. However, using all the colors gives a better alignment
# with the relative differences between them when calculating the average.

# Pivot longer

my_colors_RGB <- my_colors %>%
  filter(!DMC %in% discontinued) %>% 
  dplyr::select(
    DMC, Floss.Name, Kolonne, Række, 
    Hex_SP,
    Hex_SF, 
    Hex_CAZ,
    HEX_ego_cal
  ) %>%
  pivot_longer(
    cols = c(
      Hex_SP,
      Hex_SF,
      Hex_CAZ,
      HEX_ego_cal
      ),
    names_to = "Source",
    values_to = "HEX"
  ) %>%
  mutate(
    R = col2rgb(HEX)[1,],
    G = col2rgb(HEX)[2,],
    B = col2rgb(HEX)[3,]
  )

head(my_colors_RGB)

# Check data for consistency

my_colors_RGB_mean <- my_colors_RGB %>%
  group_by(DMC) %>%
  summarise(
    R_mean = mean(R),
    G_mean = mean(G),
    B_mean = mean(B)
  )

left_join(
  my_colors_RGB,
  my_colors_RGB_mean,
  "DMC"
  ) %>%
  mutate(
    R_dev = abs(R - R_mean)^2,
    G_dev = abs(G - G_mean)^2,
    B_dev = abs(B - B_mean)^2
    ) %>%
  group_by(Source) %>%
  summarise(
    R_dev = sqrt(mean(R_dev)),
    G_dev = sqrt(mean(G_dev)),
    B_dev = sqrt(mean(B_dev))
  )

# SF and CAZ have the largest deviations from the mean values.

# my_colors_RGB %<>%
#   filter(
#     Source != "Hex_CAZ",
#     Source != "Hex_SP"
#   )

# Convert to LAB

my_colors_LAB <- my_colors_RGB %>%
  dplyr::select(R, G, B) %>%
  as.matrix() %>% 
  divide_by(255) %>%
  sRGB() %>%
  as("LAB") %>% 
  slot("coords") %>%
  as.data.frame()

# plot(my_colors_LAB)

my_colors_LAB_mean <- my_colors_RGB %>%
  select(DMC) %>%
  bind_cols(my_colors_LAB) %>%
  group_by(DMC) %>%
  summarise(
    L = mean(L),
    A = mean(A),
    B = mean(B)
  )

plot(my_colors_LAB_mean)

# Calculate HEX for mean values

HEX_mean <- my_colors_LAB_mean %>%
  dplyr::select(-DMC) %>%
  as.matrix() %>%
  LAB() %>%
  as("sRGB") %>%
  slot("coords") %>%
  replace(. > 1, 1) %>%
  as.data.frame() %>%
  mutate(
    HEX = rgb(R, G, B, maxColorValue = 1)
  ) %>%
  dplyr::select(HEX) %>%
  unlist() %>%
  unname()

# Plot average colors

my_colors$HEX_mean <- HEX_mean

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, fill = I(HEX_mean))) +
  geom_raster() +
  ggtitle("HEX_mean") +
  scale_y_reverse()

# Convert LAB to Munsell

my_colors_Munsell <- my_colors_LAB_mean %>%
  dplyr::select(-DMC) %>%
  as.matrix() %>%
  LabtoMunsell() %>%
  as.data.frame() %>%
  rownames_to_column()

my_colors <- bind_cols(my_colors, my_colors_Munsell)

head(my_colors)

# Write results to file

saveRDS(
  my_colors,
  paste0(dir_results, "/my_colors.Rds")
)

# END