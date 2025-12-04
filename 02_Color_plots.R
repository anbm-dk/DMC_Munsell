# Color plots

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

source("funs.R")

getwd()

dir <- getwd()

dir_results <- dir %>%
  dirname() %>%
  paste0(., "/DMC_results/") %T>%
  dir.create()

my_colors <- readRDS(
  paste0(dir_results, "/my_colors.Rds")
  )

head(my_colors)

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

# Hue_to_radians(seq(0, 100, 10)) %>% plot()

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
  # round() %>%
  t() %>%
  as.data.frame() %>%
  set_colnames(c("HCx", "HCy"))

circle_min_radius <- circle_min %>%
  st_area() %>%
  divide_by(pi) %>%
  sqrt() %>%
  # ceiling() %>%
  add(1.1)

my_xlims <- circle_min_center[, 1] %>%
  unlist() %>%
  add(c(-circle_min_radius - 2, circle_min_radius + 2))

my_ylims <- circle_min_center[, 2] %>%
  unlist()  %>%
  add(c(-circle_min_radius - 2, circle_min_radius + 2))

# Hues for plotting

Huestrings <- HueStringFromNumber(seq(2.5, 100, by = 2.5))

Huestrings

Hue_angles <- Hue_to_radians(seq(2.5, 50, by = 2.5))

Hue_angles

# Test color circle wedges

V_colorcircle <- 9

C_colorcircle <- MaxChromasForStandardMunsellHuesAndValues %>%
  filter(V == V_colorcircle) %>%
  summarise(
    min_C = min(MaximumChroma)
  ) %>%
  floor()

circle_colors <- data.frame(
  H = seq(2.5, 100, by = 2.5),
  V = V_colorcircle,
  C = C_colorcircle
) %>%
  as.matrix() %>%
  MunsellToRGB() %>% 
  extract2(3) %>%
  rgb(maxColorValue = 255)

color_circle_angles <- data.frame(
  n = rep(1, length(circle_colors)),
  start = -1*(c(Hue_angles, Hue_angles + pi) - pi/2 - pi/40),
  end = -1*(c(Hue_angles, Hue_angles + pi) - pi/2 + pi/40)
)

color_circle_angles %>%
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0, r0 = 0, r = 1,
      fill = I(circle_colors),
      start = start,
      end = end
    ),
    color = NA
  ) +
  coord_equal()

# Histograms of Munsell dimensions

hist(my_colors$H)
hist(my_colors$V)
hist(my_colors$C)

# Munsell names

my_colors %>%
  dplyr::select(H, V, C) %>%
  as.matrix() %>%
  MunsellNameFromHVC()

# Hue strings in data frame

my_colors %<>%
  mutate(
    H_string = case_when(
      C >= 0.5 ~ HueStringFromNumber(round(H/2.5)*2.5),
      .default = "N"
    )
  )

# Test hue/chroma coordinate rotations

my_colors %>%
  dplyr::select(HCx, HCy) %>% head()

my_colors %>%
  dplyr::select(HCx, HCy) %>%
  as.matrix() %>%
  rotate(theta = pi/2, pivot = c(0,0)) %>% head()

# Hue slices

xlims_hslice <- matrix(numeric(), ncol = 2, nrow = length(Hue_angles))
hue_slices <- list()

for (i in 1:length(Hue_angles)) {
  # i <- 8
  
  hstrings_i <- c(Huestrings[i], Huestrings[i + 20], "N")
  
  HC_coords_i <- my_colors %>%
    dplyr::select(HCx, HCy) %>%
    as.matrix() %>%
    rotate(theta = -Hue_angles[i], pivot = c(0,0)) %>%
    set_colnames(c("x_i", "y_i"))
  
  plot_colors_i <- my_colors %>%
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
      H_string %in% hstrings_i | abs(distH) < 0.5,
      abs(distH) <= abs(distC)
    ) %>%
    group_by(round(V), round(C)) %>%
    mutate(
      rank = rank(abs(distH))
    ) %>%
    ungroup() %>%
    filter(H_string %in% hstrings_i | rank == 1)
  
  xlims_hslice[i, 1] <- min(plot_colors_i$C, na.rm = TRUE)
  xlims_hslice[i, 2] <- max(plot_colors_i$C, na.rm = TRUE)
  
  x_middle <- (xlims_hslice[i, 2] + xlims_hslice[i, 1])/2
  xlims_i <- c(
    x_middle - 11.37237 - 0.55,
    x_middle + 11.37237 + 0.55
  )
  
  hue_slices[[i]] <- plot_colors_i %>%
    ggplot(
      aes(
        x = C,
        y = V, 
        fill = I(HEX_mean), 
        # color = I(labcol), 
        label = DMC,
        group = -1L
      )
    ) +
    geom_voronoi_tile(
      aes(fill = I(HEX_mean)),
      color = 'black',
      max.radius = 0.5,
      linewidth = 1/4
    ) +
    geom_text_repel(
      data = plot_colors_i,
      aes(
        label = DMC
      ),
      box.padding = 0.15,
      max.overlaps = Inf,
      point.padding = NA,
      size = 2.5,
      min.segment.length = 0.3,
      bg.r = 0.15,          # shadow radius
      point.size = NA,
      color = "white",
      bg.color = "black"
    ) +
    ylim(c(-0.55, 10.55)) +
    xlim(
      # c(min(plot_colors_i$C) - 0.55, max(plot_colors_i$C) + 0.55)
      xlims_i
      ) +
    coord_fixed(1, expand = FALSE) +
    ggtitle(
      paste0("Hues ", hstrings_i[2], " (left) and ", hstrings_i[1], " (right)")
    )
}

max(xlims_hslice[, 2] - xlims_hslice[, 1]) / 2
# [1] 11.37237

hue_slices[[8]]

pdf(
  file = paste0(dir_results, "Hue_slice_test.pdf"),
  height = 10/2.54,
  width = 16/2.54
)

hue_slices[[8]]

try(dev.off())
try(dev.off())

tiff(
  file = paste0(dir_results, "Hue_slice_test.tif"),
  height = 10/2.54,
  width = 16/2.54,
  units = "in",
  res = 300
)

hue_slices[[8]]

try(dev.off())
try(dev.off())

pdf(
  file = paste0(dir_results, "Hue_slices_all.pdf"),
  height = 10/2.54,
  width = 16/2.54
  )

hue_slices

try(dev.off())
try(dev.off())

# Preparation for Value slices

# Endpoints for radial lines

hue_lines_pts <- Hue_to_radians(seq(5, 55, by = 10)) %>%
  magrittr::extract(-length(.)) %>%
  lapply(
    function(x) {
      line_pts <- rotate(matrix(c(100, -100, 0, 0), ncol = 2), x) 
      out <- intersect.line.circle(
        p1 = line_pts[1, ], 
        p2 = line_pts[2, ], 
        cent = unlist(circle_min_center), 
        rad = circle_min_radius
      ) %>%
        as.data.frame() %>%
        set_colnames(c("HCx", "HCy"))
      return(out)
    }
  ) %>%
  bind_rows() %>%
  mutate(number = rep(c(1:5), each = 2))

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
        rad = circle_min_radius + 1
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

extra_circles <- base::data.frame(
  HCx = 0,
  HCy = 0,
  r = seq(2, 18, 2)
)

# Make arcs instead

arcs_from_to <- apply(
  extra_circles,
  1,
  FUN = function(i) {
    points_int <- intersect_circles(
      x = c(i[1], circle_min_center$HCx),
      y = c(i[2], circle_min_center$HCy),
      r = c(i[3], circle_min_radius)
    )
    if (is.null(points_int)) {
      out <- c(0, 2*pi)
    } else {
      out <- atan2(points_int[, 1], points_int[, 2])
    }
    return(out)
  }
) %>%
  t() %>%
  as.data.frame() %>%
  set_colnames(c("from", "to"))

extra_arcs <- extra_circles %>%
  bind_cols(arcs_from_to)

# Points for chroma scale

chroma_text_pos <- data.frame(
  HCx = 0,
  HCy = seq(0, 18, 2)
)

# Plot of Hue/Chroma for all colors

my_plot_HC_all <- my_colors %>%
  arrange(V) %>%
  ggplot(
    aes(x = HCx, 
        y = HCy, 
        # color = I(HEX_mean), 
        # fill = I(HEX_mean)
    ),
    gr
  ) +
  geom_arc_bar(
    data = color_circle_angles,
    aes(
      x0 = 0, y0 = 0, r0 = 0, r = 30,
      fill = I(circle_colors),
      start = start,
      end = end
    ),
    color = NA,
    inherit.aes = FALSE
  ) +
  geom_arc_bar(
    data = circle_min_center,
    aes(
      x0 = HCx,
      y0 = HCy,
      r0 = circle_min_radius + 2, 
      r = 30,
      
      start = 0,
      end = 2*pi
    ),
    color = "white",
    fill = "white",
    inherit.aes = FALSE
  ) +
  geom_circle(
    data = circle_min_center,
    color = "white",
    fill = "grey92",
    aes(x0 = HCx, y0 = HCy, r = circle_min_radius)
  ) +
  geom_arc(
    data = extra_arcs,
    aes(x0 = HCx, y0 = HCy, r = r, start = to, end = from),
    col = "white",
    inherit.aes = FALSE
  ) +
  geom_line(
    data = hue_lines_pts,
    aes(group = number),
    color = "white"
  ) +
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
    aes(label = HCy)
  ) +
  geom_shadowtext(
    data = hue_lab_pts,
    col = "black",
    aes(label = hue_lab),
    bg.color = "white", # shadow color
    size = 4,
    bg.r = 0.15   
  ) +
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

my_plot_HC_all


# Value slices

mindist_HC <- 1.2

my_colors %>%
  mutate(
    v_round = round(V)
    ) %>%
  pull(v_round) %>%
  unique()

V_rounded_unique <- my_colors %>%
  mutate(
    v_round = round(V*2)/2
  ) %>%
  pull(v_round) %>%
  unique() %>%
  sort()

value_slices <- list()

for(i in 3:length(V_rounded_unique)) {
  
  # i <- 5
  V_rounded_i <- V_rounded_unique[i]
  
  my_colors_plot_i <- my_colors %>%
    mutate(
      # V_round = round(V)
      v_round = round(V*2)/2
      ) %>%
    filter(
      v_round == V_rounded_i
    )
  
  # mindist_i <- my_colors_plot_i %>%
  #   dplyr::select(HCx, HCy) %>%
  #   mutate(
  #     HCx = HCx*0.9,
  #     HCy = HCy*1.8
  #   ) %>%
  #   dist(
  #     diag = FALSE,
  #     upper = TRUE
  #   ) %>%
  #   as.matrix()
  # 
  # diag(mindist_i) <- NA
  # 
  # mindist_i %<>%
  #   apply(1, function(x) {min(x, na.rm = TRUE)})
  # 
  # my_colors_plot_i_text <- my_colors_plot_i %>%
  #   mutate(
  #     mindist_i = mindist_i
  #   ) %>%
  #   filter(mindist_i > 1)
  # 
  # my_colors_plot_i_label <- my_colors_plot_i %>%
  #   mutate(
  #     mindist_i = mindist_i,
  #     DMC = case_when(
  #       mindist_i > 1 ~ "",
  #       .default = DMC
  #     )
  #   )
  
  value_slices[[i]] <- my_colors_plot_i %>%
    ggplot(
      aes(x = HCx, 
          y = HCy,
          group = -1L
      )
    ) +
    geom_arc_bar(
      data = color_circle_angles,
      aes(
        x0 = 0, 
        y0 = 0, 
        r0 = 0, 
        r = 30,
        fill = I(circle_colors),
        start = start,
        end = end
      ),
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_arc_bar(
      data = circle_min_center,
      aes(
        x0 = HCx,
        y0 = HCy,
        r0 = circle_min_radius + 2, 
        r = 30,
        start = 0,
        end = 2*pi
      ),
      color = "white",
      fill = "white",
      inherit.aes = FALSE
    ) +
    geom_circle(
      data = circle_min_center,
      color = "white",
      fill = "grey92",
      aes(x0 = HCx, y0 = HCy, r = circle_min_radius)
    ) +
    geom_arc(
      data = extra_arcs,
      aes(x0 = HCx, y0 = HCy, r = r, start = to, end = from),
      col = "white",
      inherit.aes = FALSE
    ) +
    geom_line(
      data = hue_lines_pts,
      aes(group = number),
      color = "white"
    ) +
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
      aes(label = HCy)
    ) +
    geom_shadowtext(
      data = hue_lab_pts,
      col = "black",
      aes(label = hue_lab),
      bg.color = "white", # shadow color
      size = 4,
      bg.r = 0.15   
    ) +
    geom_voronoi_tile(
      aes(fill = I(HEX_mean)),
      color = 'black',
      max.radius = 1,
      linewidth = 1/4
    ) +
    geom_text_repel(
      data = my_colors_plot_i,
      aes(
        label = DMC
      ),
      box.padding = 0.15,
      max.overlaps = Inf,
      point.padding = NA,
      size = 3,
      min.segment.length = 0.3,
      bg.r = 0.075,          # shadow radius
      point.size = NA,
      color = "white",
      bg.color = "grey20"
    ) +
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
    ) +
    ggtitle(paste0("Value = ", V_rounded_i))
}


value_slices[[10]]

pdf(
  file = paste0(dir_results, "HC_slice_test.pdf"),
  height = 17/2.54,
  width = 16/2.54
)

value_slices[[10]]

try(dev.off())
try(dev.off())

tiff(
  file = paste0(dir_results, "HC_slice_test.tif"),
  height = 17/2.54,
  width = 16/2.54,
  units = "in",
  res = 300
)

value_slices[[10]]

try(dev.off())
try(dev.off())

pdf(
  file = paste0(dir_results, "Value_slices_all.pdf"),
  height = 17/2.54,
  width = 16/2.54
)

value_slices

try(dev.off())
try(dev.off())


# END