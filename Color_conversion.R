# Calculation and illustration of MUnsell colors for DMC floss

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

getwd()

dir <- getwd()

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
  select(red) %>%
  unlist() %>%
  hist()

my_colors$Hex_Ego %>%
  col2rgb() %>%
  t() %>%
  as.data.frame() %>%
  select(green) %>%
  unlist() %>%
  hist()

my_colors$Hex_Ego %>%
  col2rgb() %>%
  t() %>%
  as.data.frame() %>%
  select(blue) %>%
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

# I first thought that I should only use SF and my own colors, as tthe other
# ones were too greyish. However, using all the colors gives a better alignment
# with the relative differences between them when calculating the average.

# Pivot longer

my_colors_RGB <- my_colors %>%
  filter(!DMC %in% discontinued) %>% 
  select(
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

# Convert to XYZ 

my_colors_XYZ <- my_colors_RGB %>%
  select(R, G, B) %>%
  as.matrix() %>%
  XYZfromRGB(maxSignal = 255) %>%
  extract2(2)

# plot(as.data.frame(my_colors_XYZ))

my_colors_XYZ <- bind_cols(my_colors_RGB, my_colors_XYZ)

head(my_colors_XYZ)

my_colors_XYZ_mean <- my_colors_XYZ %>%
  group_by(DMC) %>%
  summarise(
    X = mean(X),
    Y = mean(Y),
    Z = mean(Z)
  )

# plot(my_colors_XYZ_mean)

# Calculate HEX for mean values

HEX_mean <- my_colors_XYZ_mean %>%
  select(-DMC) %>%
  as.matrix() %>%
  RGBfromXYZ(maxSignal = 255) %>%
  extract2(1) %>%
  as.data.frame() %>%
  mutate(
    HEX = rgb(R, G, B, maxColorValue = 255)
  ) %>%
  select(HEX) %>%
  unlist() %>%
  unname()

my_colors_XYZ_mean$HEX <- HEX_mean

# Plot average colors

my_colors$HEX_mean <- HEX_mean

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, fill = I(HEX_mean))) +
  geom_raster() +
  ggtitle("HEX_mean") +
  scale_y_reverse()

# Convert HEX to Munsell

my_colors_Munsell <- HEX_mean %>%
  col2rgb() %>%
  t() %>%
  RGBtoMunsell() %>%
  as.data.frame()

my_colors <- bind_cols(my_colors, my_colors_Munsell)

head(my_colors)

# Preliminary plots

my_colors %>%
  ggplot(aes(x = C, y = V, color = I(HEX_mean))) +
  geom_point() +
  coord_equal()

my_colors %>%
  ggplot(aes(x = H, y = V, color = I(HEX_mean))) +
  geom_point()

my_colors %>%
  ggplot(aes(x = H, y = C, color = I(HEX_mean))) +
  geom_point()

# Hue/Chroma coordinates

my_colors %<>%
  mutate(
    HCx = cos(H*2*pi/100)*C,
    HCy = sin(H*2*pi/100)*C,
  )

# Label color

my_colors %<>%
  mutate(
    labcol = case_when(
      V > 5 ~ "black",
      .default = "white"
    )
  )

# my_colors %>%
#   ggplot(aes(x = HCx, y = HCy, color = I(HEX_mean))) +
#   geom_point() +
#   coord_equal()

# my_colors %>%
#   filter(
#     V > 4.5,
#     V < 5.5
#   ) %>%
#   ggplot(aes(x = HCx, y = HCy, color = I(HEX_mean))) +
#   geom_point() +
#   coord_equal()

my_colors %>%
  ggplot(aes(x = H, y = C, color = I(HEX_mean))) +
  geom_point() +
  coord_radial(expand = FALSE) +
  xlim(c(0, 100))

my_colors %>%
  filter(
    V > 4.5,
    V < 5.5
  ) %>%
  ggplot(aes(x = H, y = C, color = I(HEX_mean))) +
  geom_point() +
  coord_radial(expand = FALSE) +
  xlim(c(0, 100))

my_colors %>%
  filter(H < 2.5) %>%
  ggplot(
    aes(
      x = C,
      y = V, 
      fill = I(HEX_mean), 
      # color = I(labcol), 
      label = DMC)
    ) +
  geom_point(size = 10, shape = 21, color = "black") +
  geom_text_repel(box.padding = 1, max.overlaps = Inf) +
  ylim(c(0, 10))

hist(my_colors$H)
hist(my_colors$V)
hist(my_colors$C)

my_colors %>%
  select(H, V, C) %>%
  as.matrix() %>%
  MunsellNameFromHVC()

# Hues for plotting

Huestrings <- HueStringFromNumber( seq( 2.5, 100, by=2.5 ) )

Hue_angles <- seq( 2.5, 50, by = 2.5 )*pi/50

my_colors %<>%
  mutate(
    H_string = case_when(
        C >= 0.5 ~ HueStringFromNumber(round(H/2.5)*2.5),
        .default = "N"
        )
  )

i <- 5

# Pseudochroma for N

psC <- function(x, y, R) {
  out <- sqrt(x^2 + y^2) * cos(R - atan(y/x))
  return(out)
}

# Distance from Hue angle

get_distH <- function(x, y, R) {
  out <- sqrt(x^2 + y^2) * sin(R - atan(y/x))
  return(out)
}

hstrings_i <- c(Huestrings[i], Huestrings[i + 20], "N")

my_colors %>%
  mutate(distH = get_distH(x = HCx, y = HCy, R = Hue_angles[i])) %>%
  filter(
    H_string %in% hstrings_i | (distH < 0.5 & distH > -0.5)
    ) %>%
  mutate(
    distC = psC(x = HCx, y = HCy, R = Hue_angles[i]),
    C = case_when(
      H_string == hstrings_i[2] ~ C*(-1),
      !(H_string %in% hstrings_i[1:2]) ~ distC,
      .default = C
      )
  ) %>%
  ggplot(
    aes(
      x = C,
      y = V, 
      fill = I(HEX_mean), 
      # color = I(labcol), 
      label = DMC)
  ) +
  geom_point(size = 10, shape = 21, color = "black") +
  geom_text_repel(box.padding = 1, max.overlaps = Inf) +
  ylim(c(0, 10))

# Old code
# 
# Convert to Munsell values
# 
# hues <- mnsl_hues()
# 
# my_colors_HVC <- my_colors_RGB %>%
#   mutate(
#     H = sRGBtoMunsell(c(R, G, B))[, 1],
#     V = sRGBtoMunsell(c(R, G, B))[, 2],
#     C = sRGBtoMunsell(c(R, G, B))[, 3],
#     H_rad = H*2*pi/100
#     )
# 
# head(my_colors_HVC)
# 
# my_colors_HVC_xy <- my_colors_HVC %>%
#   mutate(
#     x = cos(H_rad)*C,
#     y = sin(H_rad)*C
#   )
# 
# head(my_colors_HVC_xy)
# 
# my_colors_HVC_xy_sum <- my_colors_HVC_xy %>%
#   group_by(DMC) %>%
#   summarise(
#     H_rad = mean.circular(H_rad),
#     V = mean(V),
#     C = mean(C),
#     x = mean(x),
#     y = mean(y),
#   ) %>%
#   mutate(
#     # C = sqrt(x^2 + y^2),
#     # H_rad = atan2(y, x),
#     H_rad = case_when(
#       H_rad < 0 ~ H_rad + pi*2,
#       .default = H_rad
#     ),
#     H = as.numeric(H_rad*100/(2*pi))
#   )
# 
# my_colors_HVC_xy_sum %>%
#   as.data.frame(
#   )
# 
# my_hexes <- my_colors_HVC_xy_sum %>%
#   select(H, V, C) %>%
#   as.matrix() %>%
#   MunsellToRGB() %>%
#   extract2(3) %>%
#   as.data.frame() %>%
#   select(R, G, B) %>%
#   mutate(
#     hex = rgb(R, G, B, maxColorValue = 255)
#   )
# 
# my_colors_HVC_xy_sum$hex <- my_hexes$hex
# 
# my_colors_HVC_xy_sum %>%
#   ggplot(aes(x = C, y = V, color = I(hex))) +
#   geom_point() +
#   coord_equal()
# 
# my_colors_HVC_xy_sum %>%
#   ggplot(aes(x = H, y = V, color = I(hex))) +
#   geom_point()
# 
# my_colors_HVC_xy_sum %>%
#   ggplot(aes(x = H, y = C, color = I(hex))) +
#   geom_point()
# 
# hist(my_colors_HVC$H_rad)
# hist(my_colors_HVC$C)
# 
# my_colors_HVC %>% 
#   select(H, V, C) %>%
#   plot()
# 
# my_colors_HVC %>%
#   apply(2, max)

# END