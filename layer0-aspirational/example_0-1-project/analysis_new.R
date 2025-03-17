library('dplyr')
library('eurostat')
library('lubridate')
library('readr')
library('sf')
library('stringi')
library('tidyr')
library('yaml')
library('ggplot2')
library('rnaturalearth')
library('rnaturalearthdata')
library('rnaturalearthhires')
library('ggtern')
library('scales')

euro_geo_nuts2 <-
  get_eurostat_geospatial(output_class = 'sf',
                          resolution = '10', nuts_level = 2, year = 2016) %>%
  filter(!(stri_detect(geo, regex = '^AL') | stri_detect(geo, regex = '^LI') | geo == 'FI20')) %>%
  st_transform(crs = 3035) %>%
  st_buffer(0) %>%
  st_crop(xmin = 25.0e5,
          xmax = 75.0e5,
          ymin = 13.5e5,
          ymax = 54.5e5) %>%
  mutate(
    name = stri_trans_general(NUTS_NAME, id = 'any-latin; latin-ascii')
  ) %>%
  select(id, name, geometry)

educ <- get_eurostat('edat_lfse_04')

euro_education <-
  educ %>%
  mutate(year = lubridate::year(TIME_PERIOD),
         id = as.character(geo)) %>%
  filter(year == 2016,
         stri_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T',
         age == 'Y25-64') %>%
  mutate(values = values/100) %>%
  spread(isced11, values) %>%
  select(id, ed_0to2 = `ED0-2`, ed_3to4 = `ED3_4`, ed_5to8 = `ED5-8`) %>%
  drop_na()


lf <- get_eurostat('lfst_r_lfe2en2')

euro_sectors <-
  lf %>%
  mutate(
    year = as.integer(lubridate::year(TIME_PERIOD)),
    geo = as.character(geo)
  ) %>%
  filter(
    age == 'Y_GE15',
    stri_length(geo) == 4,
    year == 2016
  ) %>%
  complete(nace_r2, geo, year, fill = list(values = 0)) %>%
  mutate(
    sector = recode(as.character(nace_r2),
                    `A` = 'primary',
                    `B-E` = 'secondary',
                    `F` = 'secondary'),
    sector = ifelse(!sector %in% c('primary', 'secondary', 'TOTAL'),
                    'tertiary',
                    sector)
  ) %>%
  group_by(year, geo, sector) %>%
  summarise(N = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(sector, N) %>%
  mutate_at(vars(primary, secondary, tertiary), .funs = ~ ./TOTAL) %>%
  select(id = geo, lf_pri = primary, lf_sec = secondary, lf_ter = tertiary) %>%
  drop_na()


eura_sf <-
  ne_countries(continent = c('europe', 'asia', 'africa'),
               returnclass = 'sf', scale = 10) %>%
  st_transform(crs = 3035) %>%
  st_union(by_feature = FALSE) %>%
  st_crop(xmin = 25.0e5,
          xmax = 75.0e5,
          ymin = 13.5e5,
          ymax = 54.5e5)

euro_basemap <-
  ggplot(eura_sf) +
  geom_sf(
    aes(geometry = geometry),
    color = NA, fill = 'grey90'
  ) +
  coord_sf(expand = FALSE, datum = NA) +
  theme_void()


TernaryMeshCentroids <- function (k) {
  K = k^2; id = 1:K
  g <- floor(sqrt(K-id)); gsq <- g^2
  c1 <- (((-K + id + g*(g+2) + 1) %% 2) - 3*gsq - 3*id + 3*K + 1) / (6*k)
  c2 <- -(((-K + gsq + id + 2*g + 1) %% 2) + 3*g - 3*k + 1) / (3*k)
  c3 <- (((-K + gsq + id + 2*g + 1) %% 2) + 3*gsq + 6*g + 3*id - 3*K + 1) / (6*k)

  return(cbind(id = id, p1 = c1, p2 = c2, p3 = c3))
}

TernaryMeshVertices <- function (C) {
  k <- sqrt(nrow(C))
  j <- k - floor(sqrt(k^2-C[,1]))
  i <- C[,1] - (j-1)*(2*k-j+1)
  term1 <- ((-1)^(i %% 2) * 2) / (3*k)
  term2 <- ((-1)^(i %% 2)) / (3*k)

  v1 <- cbind(C[,2] - term1, C[,3] + term2, C[,4] + term2)
  v2 <- cbind(C[,2] + term2, C[,3] - term1, C[,4] + term2)
  v3 <- cbind(C[,2] + term2, C[,3] + term2, C[,4] - term1)

  V <- cbind(C[,1], rep(1:3, each = nrow(C)), rbind(v1, v2, v3))
  colnames(V) <- c('id', 'vertex', 'p1', 'p2', 'p3')

  return(V)
}

Perturbate <- function (P, c = rep(1/3, 3)) {
  return(prop.table(t(t(P)*c), margin = 1))
}

ColorMap <- function (P, h_, c_, l_, contrast, center) {

  phi <- (h_*0.0174 + c(0, 2.09, 4.19)) %% 6.28

  P <- P_raw <- prop.table(P, margin = 1)
  P <- Perturbate(P, 1/center)

  C <- P*c_

  Z <-
    matrix(
      complex(
        argument = phi, modulus = c(t(C))
      ),
      ncol = 3, byrow = TRUE
    )

  z <- rowSums(Z)
  M <- cbind(h = (Arg(z)*57.3)%%360, c = Mod(z), l = l_)

  cfactor <- M[,2]*contrast/c_ + 1-contrast
  M[,3] <- cfactor*M[,3]
  M[,2] <- cfactor*M[,2]

  hexsrgb <- hcl(h = M[,1], c = M[,2], l = M[,3],
                 alpha = 1, fixup = TRUE)

  result <-
    data.frame(
      P_raw, P, M[,1], M[,2], M[,3], hexsrgb,
      row.names = NULL, check.rows = FALSE,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  colnames(result) <-
    c('p1', 'p2', 'p3', 'cp1', 'cp2', 'cp3', 'h', 'c', 'l', 'hexsrgb')
  return(result)
}


ColorKey <- function (h_, c_, l_, contrast, center) {

  k = 100
  C <- TernaryMeshCentroids(k)
  V <- TernaryMeshVertices(C)
  rgbs <- ColorMap(P = C[,-1], h_, c_, l_, contrast, center)[['hexsrgb']]
  legend_surface <- data.frame(V, rgb = rep(rgbs, 3))

  legend <-
    ggtern(
      legend_surface,
      aes_string(x = 'p1', y = 'p2', z = 'p3')
    ) +
    geom_polygon(
      aes_string(group = 'id', fill = 'rgb', color = 'rgb'),
      lwd = 1
    ) +
    geom_mask() +
    scale_color_identity(guide = FALSE) +
    scale_fill_identity(guide = FALSE) +
    theme_classic() +
    theme(
      tern.axis.title.L =
        element_text(hjust = 0.2, vjust = 1, angle = -60),
      tern.axis.title.R =
        element_text(hjust = 0.8, vjust = 0.6, angle = 60)
    )

  return(legend)
}


plot_euro_education_key <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) +
  lline(
    Lintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  tline(
    Tintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  rline(
    Rintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  geom_point(
    aes(x = ed_0to2, y = ed_3to4, z = ed_5to8),
    shape = 21,
    data = euro_education
  ) +
  labs(
    L = '% Lower secondary\nor less',
    T = '% Upper secondary',
    R = '% Tertiary',
    title = 'A'
  ) +
  theme_arrowlarge()

plot_euro_education_key

ggsave(
  filename = './euro_education_key.pdf',
  plot = plot_euro_education_key,
  width = 6, height = 6
)


euro_education_colors <-
  ColorMap(
    as.matrix(euro_education[,2:4]),
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) %>%
  mutate(
    id = euro_education$id
  )

euro_education_map <-
  left_join(euro_geo_nuts2, euro_education_colors, by = 'id')

plot_euro_education_map <-
  euro_basemap +
  geom_sf(
    aes(
      geometry = geometry,
      fill = hexsrgb
    ),
    color = NA,
    data = euro_education_map
  ) +
  scale_fill_identity() +
  labs(title = 'B')

plot_euro_education_map

ggsave(
  filename = './euro_education_map.pdf',
  plot = plot_euro_education_map,
  width = 6, height = 6
)

GeometricMean <- function (x, na.rm = TRUE, zero.rm = TRUE) {
  if (zero.rm) { x <- x[x!=0] }
  return(exp(mean(log(x), na.rm = na.rm)))
}

Centre <- function (P) {
  g <- apply(P, MARGIN = 2, FUN = GeometricMean)
  return(g/sum(g))
}

eu_center <- Centre(euro_sectors[,2:4])

euro_sectors_colors_noncentered <-
  ColorMap(
    as.matrix(euro_sectors[,2:4]),
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) %>%
  mutate(id = euro_sectors$id)

plot_euro_sectors_key <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) +
  lline(
    Lintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  tline(
    Tintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  rline(
    Rintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  lline(
    Lintercept = eu_center[1],
    size = 0.1*5,
    color = 'grey5'
  ) +
  tline(
    Tintercept = eu_center[2],
    size = 0.1*5,
    color = 'grey5'
  ) +
  rline(
    Rintercept = eu_center[3],
    size = 0.1*5,
    color = 'grey5'
  ) +
  geom_point(
    aes(x = p1, y = p2, z = p3),
    alpha = 0.2, size = 0.1,
    data = euro_sectors_colors_noncentered
  ) +
  labs(
    L = '% Primary', T = '% Secondary', R = '% Tertiary'
  ) +
  theme(text = element_text(size = 7))

plot_euro_sectors_key


euro_sectors_map <-
  left_join(euro_geo_nuts2, euro_sectors_colors_noncentered, by = 'id')

plot_euro_sectors_map <-
  euro_basemap +
  geom_sf(
    aes(
      geometry = geometry,
      fill = hexsrgb
    ),
    color = NA,
    data = euro_sectors_map
  ) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      plot_euro_sectors_key +
        theme(plot.background = element_rect(fill = NA, color = NA))
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(title = 'A')

plot_euro_sectors_map


ggsave(
  filename = 'euro_sectors_map.pdf',
  plot = plot_euro_sectors_map,
  width = 6, height = 6
)

saveRDS(
  euro_sectors_colors_noncentered,
  './euro_sectors_colors_noncentered.rds'
)


TernaryGridCentered <- function (center, N = 10, relative_grid = TRUE) {

  if (isTRUE(relative_grid)) {
    prop_diff = seq(-1, 1, by = 1/N)
    labels = data.frame(
      L = prop_diff[prop_diff >= -center[1]][1:N],
      T = prop_diff[prop_diff >= -center[2]][1:N],
      R = prop_diff[prop_diff >= -center[3]][1:N]
    )
    non_centered_breaks = data.frame(
      L = labels$L + center[1],
      T = labels$T + center[2],
      R = labels$R + center[3]
    )
  }

  if (!isTRUE(relative_grid)) {
    prop = seq(0, 1, by = 1/N)
    labels = data.frame(
      L = unique(sort(c(prop, center[1]))),
      T = unique(sort(c(prop, center[2]))),
      R = unique(sort(c(prop, center[3])))
    )
    non_centered_breaks = labels
  }

  gridL =
    data.frame(
      scale = 'L',
      center = ifelse(non_centered_breaks$L == center[1], TRUE, FALSE),
      L_from = non_centered_breaks$L,
      T_from = 1-non_centered_breaks$L,
      R_from = 0,
      L_to = non_centered_breaks$L,
      T_to = 0,
      R_to = 1-non_centered_breaks$L
    )

  gridT =
    data.frame(
      scale = 'T',
      center = ifelse(non_centered_breaks$T == center[2], TRUE, FALSE),
      L_from = 0,
      T_from = non_centered_breaks$T,
      R_from = 1-non_centered_breaks$T,
      L_to = 1-non_centered_breaks$T,
      T_to = non_centered_breaks$T,
      R_to = 0
    )

  gridR =
    data.frame(
      scale = 'R',
      center = ifelse(non_centered_breaks$R == center[3], TRUE, FALSE),
      L_from = 1-non_centered_breaks$R,
      T_from = 0,
      R_from = non_centered_breaks$R,
      L_to = 0,
      T_to = 1-non_centered_breaks$R,
      R_to = non_centered_breaks$R
    )

  non_centered_grid = rbind(gridL, gridT, gridR)

  centered_grid = data.frame(
    non_centered_grid[,1:2],
    prop.table(t(t(non_centered_grid[,3:5])*(1/center)), margin = 1),
    prop.table(t(t(non_centered_grid[,6:8])*(1/center)), margin = 1)
  )

  centered_breaks =
    data.frame(
      L = centered_grid[centered_grid$scale == 'L', 'L_from'],
      T = centered_grid[centered_grid$scale == 'T', 'T_from'],
      R = centered_grid[centered_grid$scale == 'R', 'R_from']
    )

  list(
    non_centered_grid = non_centered_grid,
    centered_grid = centered_grid,
    non_centered_breaks = non_centered_breaks,
    centered_breaks = centered_breaks,
    labels = labels
  )

}


gridlines_pct <-
  TernaryGridCentered(
    eu_center, N = 5, relative_grid = FALSE
  )

euro_sectors_colors_centered <-
  ColorMap(
    as.matrix(euro_sectors[,2:4]),
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = eu_center
  ) %>%
  mutate(id = euro_sectors$id)


saveRDS(
  euro_sectors_colors_centered,
  './euro_sectors_colors_centered.rds'
)

plot_euro_sectors_key_centered <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) +
  geom_segment(
    aes(
      x = L_from, xend = L_to,
      y = T_from, yend = T_to,
      z = R_from, zend = R_to,
      alpha = center
    ),
    show.legend = FALSE,
    data = gridlines_pct$centered_grid
  ) +
  geom_point(
    aes(x = cp1, y = cp2, z = cp3),
    alpha = 0.2, size = 0.1,
    data = euro_sectors_colors_centered
  ) +
  scale_L_continuous(
    breaks = gridlines_pct$centered_breaks$L,
    labels = formatC(gridlines_pct$labels$L, format = 'f', digits = 2)
  ) +
  scale_T_continuous(
    breaks = gridlines_pct$centered_breaks$T,
    labels = formatC(gridlines_pct$labels$T, format = 'f', digits = 2)
  ) +
  scale_R_continuous(
    breaks = gridlines_pct$centered_breaks$R,
    labels = formatC(gridlines_pct$labels$R, format = 'f', digits = 2)
  ) +
  labs(
    L = '% Primary', T = '% Secondary', R = '% Tertiary'
  ) +
  theme(text = element_text(size = 7))

plot_euro_sectors_key_centered


euro_sectors_map_centered <-
  left_join(euro_geo_nuts2, euro_sectors_colors_centered, by = 'id')

plot_euro_sectors_map_centered <-
  euro_basemap +
  theme_void() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = hexsrgb
    ),
    color = NA,
    data = euro_sectors_map_centered
  ) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      plot_euro_sectors_key_centered +
        theme(plot.background = element_rect(fill = NA, color = NA))
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(title = 'B')

plot_euro_sectors_map_centered

ggsave(
  filename = './euro_sectors_map_centered.pdf',
  plot = plot_euro_sectors_map_centered,
  width = 6, height = 6
)

legend_style_a <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) +
  # grid
  lline(
    Lintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  tline(
    Tintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  rline(
    Rintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  # center
  lline(Lintercept = eu_center[1]) +
  tline(Tintercept = eu_center[2]) +
  rline(Rintercept = eu_center[3]) +
  geom_point(
    aes(x = p1, y = p2, z = p3),
    alpha = 0.2, size = 1,
    data = euro_sectors_colors_noncentered
  ) +
  labs(x = '% primary', y = '% secondary', z = '% ternary')

legend_style_a


ggsave(
  filename = './legend_style_a.pdf',
  plot = legend_style_a,
  width = 6, height = 6
)


legend_style_b <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) +
  lline(
    Lintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  tline(
    Tintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  rline(
    Rintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  geom_point(
    aes(x = cp1, y = cp2, z = cp3),
    alpha = 0.2, size = 1,
    data = euro_sectors_colors_centered
  ) +
  labs(x = '% yellow', y = '% cyan', z = '% magenta')

legend_style_b


ggsave(
  filename = './legend_style_b.pdf',
  plot = legend_style_b,
  width = 6, height = 6
)


# transformed grid
gridlines_pct_diff <- TernaryGridCentered(eu_center, N = 10)

legend_style_c <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = c(1/3, 1/3, 1/3)
  ) +
  geom_segment(
    aes(
      x = L_from, xend = L_to,
      y = T_from, yend = T_to,
      z = R_from, zend = R_to,
      alpha = center
    ),
    show.legend = FALSE,
    data = gridlines_pct_diff$centered_grid
  ) +
  geom_point(aes(x = cp1, y = cp2, z = cp3),
             alpha = 0.2, size = 1,
             data = euro_sectors_colors_centered) +
  scale_L_continuous(
    breaks =
      gridlines_pct_diff$centered_breaks$L,
    labels =
      paste0(formatC(gridlines_pct_diff$labels$L*100, format = 'd', flag = '+'),
             ' (', round(gridlines_pct_diff$non_centered_breaks$L*100, 0), ')')
  ) +
  scale_T_continuous(
    breaks =
      gridlines_pct_diff$centered_breaks$T,
    labels =
      paste0(formatC(gridlines_pct_diff$labels$T*100, format = 'd', flag = '+'),
             ' (', round(gridlines_pct_diff$non_centered_breaks$T*100, 0), ')')
  ) +
  scale_R_continuous(
    breaks =
      gridlines_pct_diff$centered_breaks$R,
    labels =
      paste0(formatC(gridlines_pct_diff$labels$R*100, format = 'd', flag = '+'),
             ' (', round(gridlines_pct_diff$non_centered_breaks$R*100, 0), ')')
  ) +
  scale_alpha_manual(values = c(`FALSE` = 0.2, `TRUE` = 1)) +
  labs(
    x = '%pt. diff. primary (%)',
    y = '%pt. diff. secondary (%)',
    z = '%pt. diff. ternary (%)'
  )

legend_style_c

legend_style_d <-
  ColorKey(
    h_ = 80,
    c_ = 170,
    l_ = 90,
    contrast = 0.7,
    center = eu_center
  ) +
  # grid
  lline(
    Lintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  tline(
    Tintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  rline(
    Rintercept = seq(0.1, 0.9, 0.1),
    size = 0.1,
    color = 'grey5'
  ) +
  # center
  lline(Lintercept = eu_center[1]) +
  tline(Tintercept = eu_center[2]) +
  rline(Rintercept = eu_center[3]) +
  geom_point(aes(x = cp1, y = cp2, z = cp3),
             alpha = 0.2, size = 1,
             data = euro_sectors_colors_noncentered) +
  labs(x = '% primary', y = '% secondary', z = '% ternary')

legend_style_d

AlrTransform <- function (P) {
  K <- NCOL(P)
  N <- NROW(P)
  logpk <- log(P[,K])
  X <- matrix(0, N, K-1)
  for (j in 1:(K-1)) {
    X[,j] <- log(P[,j]) - logpk
  }
  return(X)
}
InvAlrTransform <- function (P) {
  K <- NCOL(P)
  N <- NROW(P)
  X <- matrix(1, N, K+1)
  for (j in 1:K) {
    X[,j] <- exp(P[,j])
  }
  prop.table(X, margin = 1)
}


euro_sectors_colors_alr_transformed <-
  AlrTransform(
    as.matrix(euro_sectors_colors_centered[,4:6])
  )
colnames(euro_sectors_colors_alr_transformed) <- c('x', 'y')
euro_sectors_colors_centered <-
  bind_cols(euro_sectors_colors_centered,
            as.data.frame(euro_sectors_colors_alr_transformed))

legend_style_e <-
  euro_sectors_colors_centered %>%
  filter(is.finite(x)) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(
    aes(x = x, y = y, fill = hexsrgb),
    shape = 21, color = 'black',
    size = 3
  ) +
  scale_x_continuous(
    expression(log(p[pri]/p[ter])-log(c[pri]/c[ter])),
    breaks = scales::breaks_width(1)
  ) +
  scale_y_continuous(
    expression(log(p[sec]/p[ter])-log(c[sec]/c[ter])),
    breaks = scales::breaks_width(1)
  ) +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    panel.grid.minor =
      element_blank(),
    panel.grid.major =
      element_line(color = 'black', size = 0.2)
  ) +
  theme(
    panel.background = element_rect(fill = 'grey30', color = NA)
  ) +
  coord_equal(clip = 'off')

legend_style_e
