# Calculation and illustration of MUnsell colors for DMC floss

# To do:
# - Fix color labels/text positions
# - Add hue labels to value slices ("radial" plots)
# - Fix aspect ratio for hue slices.
# - Create chroma slices

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

# I first thought that I should only use SF and my own colors, as tthe other
# ones were too greyish. However, using all the colors gives a better alignment
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

# Convert to XYZ 

my_colors_XYZ <- my_colors_RGB %>%
  dplyr::select(R, G, B) %>%
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
  dplyr::select(-DMC) %>%
  as.matrix() %>%
  RGBfromXYZ(maxSignal = 255) %>%
  extract2(1) %>%
  as.data.frame() %>%
  mutate(
    HEX = rgb(R, G, B, maxColorValue = 255)
  ) %>%
  dplyr::select(HEX) %>%
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
  as.data.frame() %>%
  rownames_to_column()

my_colors <- bind_cols(my_colors, my_colors_Munsell)

head(my_colors)

# Preliminary plots

# Value and chroma

my_colors %>%
  ggplot(aes(x = C, y = V, color = I(HEX_mean))) +
  geom_point() +
  coord_equal()

# Hue and value

my_colors %>%
  ggplot(aes(x = H, y = V, color = I(HEX_mean))) +
  geom_point()

# Hue and chroma

my_colors %>%
  ggplot(aes(x = H, y = C, color = I(HEX_mean))) +
  geom_point()

# Hue/Chroma coordinates

Hue_to_radians <- function(Hue) {
  radians <- Hue %>%
    multiply_by(-1) %>%
    # add(100) %>%
    add(30) %>%
    divide_by(100) %>%
    multiply_by(2*pi)
  radians[radians > pi] <- radians[radians > pi] - 2*pi
  radians[radians < -pi] <- radians[radians < -pi] + 2*pi
  return(radians)
}

Hue_to_radians(seq(0, 100, 10)) %>% plot()

my_colors %<>%
  mutate(
    H_rad = Hue_to_radians(H),
    HCx = cos(H_rad)*C,
    HCy = sin(H_rad)*C,
  )

# Minimum bounding circle

circle_min <- my_colors %>%
  dplyr::select(HCx, HCy) %>%
  as.matrix() %>%
  st_multipoint() %>%
  st_minimum_bounding_circle()

plot(circle_min)
points(my_colors$HCx, my_colors$HCy)

circle_min_center <- circle_min %>% 
  st_centroid() %>%
  as.vector() %>%
  round() %>%
  t() %>%
  as.data.frame() %>%
  set_colnames(c("HCx", "HCy"))

circle_min_radius <- circle_min %>%
  st_area() %>%
  divide_by(pi) %>%
  sqrt() %>%
  ceiling()

my_xlims <- circle_min_center[, 1] %>%
  unlist() %>%
  add(c(-circle_min_radius - 2, circle_min_radius + 2))

my_ylims <- circle_min_center[, 2] %>%
  unlist()  %>%
  add(c(-circle_min_radius - 2, circle_min_radius + 2))

# Label color

my_colors %<>%
  mutate(
    labcol = case_when(
      V > 5 ~ "black",
      .default = "white"
    )
  )

# Radial coordinates

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

# Preliminary hue slice

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
  dplyr::select(H, V, C) %>%
  as.matrix() %>%
  MunsellNameFromHVC()

# Hues for plotting

Huestrings <- HueStringFromNumber(seq(2.5, 100, by = 2.5))

Huestrings

Hue_angles <- Hue_to_radians(seq(2.5, 50, by = 2.5))

Hue_angles

my_colors %<>%
  mutate(
    H_string = case_when(
        C >= 0.5 ~ HueStringFromNumber(round(H/2.5)*2.5),
        .default = "N"
        )
  )

my_colors %>%
  dplyr::select(HCx, HCy) %>% head()

my_colors %>%
  dplyr::select(HCx, HCy) %>%
  as.matrix() %>%
  rotate(theta = pi/2, pivot = c(0,0)) %>% head()


i <- 8

hstrings_i <- c(Huestrings[i], Huestrings[i + 20], "N")

HC_coords_i <- my_colors %>%
  dplyr::select(HCx, HCy) %>%
  as.matrix() %>%
  rotate(theta = -Hue_angles[i], pivot = c(0,0)) %>%
  set_colnames(c("x_i", "y_i"))

my_colors %>%
  bind_cols(HC_coords_i) %>%
  mutate(
    distH = y_i,
    distC = x_i,
    C = case_when(
      H_string == hstrings_i[2] ~ C*(-1),
      !(H_string %in% hstrings_i[1:2]) ~ distC,
      .default = C
    )
  ) %>%
  filter(
    H_string %in% hstrings_i | (distH < 0.5 & distH > -0.5)
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

# XY coordinates

# Points for hue labels

hue_lab_pts <- Hue_to_radians(seq(5, 55, by = 10)) %>%
  magrittr::extract(-length(.)) %>%
  lapply(
    function(x) {
      line_pts <- rotate(matrix(c(100, -100, 0, 0), ncol = 2), x) 
      out <- intersect.line.circle(
          p1 = line_pts[1, ], 
          p2 = line_pts[2, ], 
          cent = unlist(circle_min_center), 
          rad =circle_min_radius + 1
        ) %>%
        as.data.frame() %>%
        set_colnames(c("HCx", "HCy"))
      return(out)
    }
  ) %>%
  bind_rows() %>%
  rowid_to_column() %>%
  mutate(
    hue_lab = c("R", "BG", "YR", "B", "Y", "PB", "GY", "P", "G", "RP")
  )

hue_lab_pts



# Circles for chroma scale

extra_cirlces <- data_frame(
  HCx = 0,
  HCy = 0,
  r = seq(2, 18, 2)
)

# Points for chroma scale

chroma_text_pos <- data.frame(
  HCx = 0,
  HCy = seq(2, 18, 2)
)

my_colors %>%
  ggplot(
    aes(x = HCx, 
        y = HCy, 
        # color = I(HEX_mean), 
        # fill = I(HEX_mean)
        )
    ) +
  geom_circle(
    data = circle_min_center,
    color = NA,
    fill = "grey92",
    aes(x0 = HCx, y0 = HCy, r = circle_min_radius)
  ) +
  geom_circle(
    data = extra_cirlces,
    color = "white",
    fill = NA,
    aes(x0 = HCx, y0 = HCy, r = r)
  ) +
  geom_hline(color = "white", yintercept = 0) +
  geom_abline(intercept = 0, slope = tan(pi*-2.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*-1.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*-0.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*0.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*1.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*2.5/5), color = "white") +
  geom_point(
    data = chroma_text_pos,
    size = 10, 
    shape = 21, 
    color = "white",
    fill = "grey92"
  ) +
  geom_text(
    data = chroma_text_pos,
    col = "grey",
    fill = NA,
    aes(label = HCy)
  ) +
  geom_text(
    data = hue_lab_pts,
    col = "black",
    aes(label = hue_lab)) +
  geom_point(shape = 21, color = "black", size = 3, aes(fill = I(HEX_mean))) +
  coord_equal(xlim = my_xlims, ylim = my_ylims) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

# Value slice

my_colors %>%
  filter(
    V > 4.5,
    V < 5.5
  ) %>%
  ggplot(
    aes(x = HCx, 
        y = HCy, 
        color = I(HEX_mean), 
        
        )
    ) +
  geom_circle(
    data = circle_min_center,
    color = NA,
    fill = "grey92",
    aes(x0 = HCx, y0 = HCy, r = circle_min_radius, label = NA)
  ) +
  geom_circle(
    data = extra_cirlces,
    color = "white",
    fill = NA,
    aes(x0 = HCx, y0 = HCy, r = r, label = NA)
  ) +
  geom_hline(color = "white", yintercept = 0) +
  geom_abline(intercept = 0, slope = tan(pi*-2.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*-1.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*-0.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*0.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*1.5/5), color = "white") +
  geom_abline(intercept = 0, slope = tan(pi*2.5/5), color = "white") +
  geom_point(
    data = chroma_text_pos,
    size = 10, 
    shape = 21, 
    color = "white",
    fill = "grey92",
    label = NA
  ) +
  geom_text(
    data = value_text_pos,
    col = "grey",
    aes(label = HCy)
  ) +
  geom_text(
    data = hue_lab_pts,
    col = "black",
    aes(label = hue_lab)) +
  geom_point(size = 10, shape = 21, color = "black", label = NA, aes(fill = I(HEX_mean))) +
  geom_text_repel(box.padding = 1, max.overlaps = Inf, color = "black",
                  aes(label = DMC, color = "black")) +
  coord_equal(xlim = my_xlims, ylim = my_ylims) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )


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