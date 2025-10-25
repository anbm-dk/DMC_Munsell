# Calculation and illustration of MUnsell colors for DMC floss

library(magrittr)
library(dplyr)
library(tidyr)

getwd()

dir <- getwd()

my_colors <- dir %>%
  paste0(., "/Floss_colors.txt") %>%
  read.table(
    dec = ",", 
    sep = "\t", 
    header = TRUE, 
    comment.char = ""
    )

discontinued <- c(504, 731, 776, 781, 806, 868, 971, 3773)

discontinued[!discontinued %in% my_colors$DMC]
# [1] 868

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

my_colors

# Aggregate color values

my_colors_RGB <- my_colors %>%
  filter(!DMC %in% discontinued) %>% 
  select(
    DMC, Floss.Name, Kolonne, Række, Hex_SP, Hex_SF, Hex_CAZ, HEX_ego_cal
  ) %>%
  pivot_longer(
    cols = c(Hex_SP, Hex_SF, Hex_CAZ, HEX_ego_cal),
    names_to = "Source",
    values_to = "HEX"
  ) %>%
  mutate(
    R = col2rgb(HEX)[1,],
    G = col2rgb(HEX)[2,],
    B = col2rgb(HEX)[3,]
  )

my_colors_RGB_sum <- my_colors_RGB %>%
  group_by(DMC) %>%
  summarise(
    R_sd = sd(R),
    G_sd = sd(G),
    B_sd = sd(B),
    R = mean(R),
    G = mean(G),
    B = mean(B)
  )

my_colors_RGB_sum

plot(my_colors_RGB_sum$R, my_colors_RGB_sum$R_sd)
plot(my_colors_RGB_sum$G, my_colors_RGB_sum$G_sd)
plot(my_colors_RGB_sum$B, my_colors_RGB_sum$B_sd)

plot(my_colors_RGB_sum[, 5:7])

# Convert to Munsell values



# END