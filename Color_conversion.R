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

cref_mean <- color_ref %>%
  group_by(Objekt, Reference) %>%
  summarise(
    R = mean(R),
    G = mean(G),
    B = mean(B)
  )

cref_dif <- cref_mean[1:4, -1:-2] / cref_mean[5:8, -1:-2]

cref_sum <- apply(cref_dif, 2, mean)

paper_rgb <- cref_mean[4, -1:-2] %>% unlist()

# Standardize my own color measurements

RGB_ego_cal <- my_colors$Hex_Ego %>%
  col2rgb() %>%
  multiply_by(255) %>%
  divide_by(paper_rgb) %>%
  t()
 
head(RGB_ego_cal)

RGB_ego_cal_max <- apply(RGB_ego_cal, 2, max)

RGB_ego_cal_max

RGB_ego_cal2 <- RGB_ego_cal %>%
  t() %>%
  multiply_by(255) %>%
  divide_by(RGB_ego_cal_max) %>%
  t()

head(RGB_ego_cal2)

plot(data.frame(RGB_ego_cal2))

HEX_ego_cal <- rgb(RGB_ego_cal2/255 - 0.00001)

my_colors$HEX_ego_cal <- HEX_ego_cal

head(my_colors)

# SP colors (kind of drab)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, color = I(Hex_SP))) +
  geom_point() +
  ggtitle("Hex_SP") +
  scale_y_reverse()

# SF colors (ok)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, color = I(Hex_SF))) +
  geom_point() +
  ggtitle("Hex_SF") +
  scale_y_reverse()

# CAZ colors (also kind of drab)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, color = I(Hex_CAZ))) +
  geom_point() +
  ggtitle("Hex_CAZ") +
  scale_y_reverse()

# My colors (most vivid)

my_colors %>%
  ggplot(aes(x = Kolonne, y = Række, color = I(HEX_ego_cal))) +
  geom_point() +
  ggtitle("HEX_ego_cal") +
  scale_y_reverse()

# I should only use SF and my own colors. The other ones are too greyish.

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

my_colors_RGB %<>%
  filter(
    Source != "Hex_CAZ",
    Source != "Hex_SP"
  )

# Convert to XYZ 

my_colors_XYZ <- my_colors_RGB %>%
  select(R, G, B) %>%
  as.matrix() %>%
  XYZfromRGB(maxSignal = 255) %>%
  extract2(2)

plot(as.data.frame(my_colors_XYZ))

my_colors_XYZ <- bind_cols(my_colors_RGB, my_colors_XYZ)

head(my_colors_XYZ)

my_colors_XYZ_mean <- my_colors_XYZ %>%
  group_by(DMC) %>%
  summarise(
    X = mean(X),
    Y = mean(Y),
    Z = mean(Z)
  )

plot(my_colors_XYZ_mean)

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
  ggplot(aes(x = Kolonne, y = Række, color = I(HEX_mean))) +
  geom_point() +
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

my_colors %>%
  ggplot(aes(x = HCx, y = HCy, color = I(HEX_mean))) +
  geom_point() +
  coord_equal()

my_colors %>%
  filter(
    V > 4,
    V < 6
  ) %>%
  ggplot(aes(x = HCx, y = HCy, color = I(HEX_mean))) +
  geom_point() +
  coord_equal()

my_colors %>%
  filter(H < 10) %>%
  ggplot(aes(x = C, y = V, color = I(HEX_mean))) +
  geom_point()

hist(my_colors$H)

# Old code

# Convert to Munsell values

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