# Init --------------------------------------------------------------------

library(ggtern)

# Function ----------------------------------------------------------------

#' Geometric Mean
#'
#' Calculate the geometric mean for a numeric vector.
#'
#' @param x Numeric vector.
#' @param na.rm Should NAs be removed? (default=TRUE)
#' @param zero.rm Should zeros be removed? (default=TRUE)
#'
#' @return The geometric mean as numeric scalar.
#'
#' @examples
#' tricolore:::GeometricMean(0:100)
#' tricolore:::GeometricMean(0:100, zero.rm = FALSE)
#'
#' @keywords internal
GeometricMean <- function (x, na.rm = TRUE, zero.rm = TRUE) {
  # The geometric mean can't really deal with elements equal to 0.
  # This option removes 0 elements from the vector.
  if (zero.rm) { x = x[x!=0] }
  return(exp(mean(log(x), na.rm = na.rm)))
}

#' Center Composition
#'
#' Center a compositional data set around its mean.
#'
#' @param P n by m matrix of compositions {p1, ..., pm}_i for
#'          i=1,...,n.
#'
#' @return n by m matrix of centered compositions.
#'
#' @examples
#' P <- prop.table(matrix(runif(300), 100), margin = 1)
#' tricolore:::Centre(P)
#'
#' @references Von Eynatten, H., Pawlowsky-Glahn, V., & Egozcue, J. J. (2002).
#' Understanding perturbation on the simplex: A simple method to better
#' visualize and interpret compositional data in ternary diagrams.
#' Mathematical Geology, 34(3), 249-257.
#'
#' @keywords internal
Centre <- function (P) {
  # calculate the geometric mean for each element of the composition
  g = apply(P, MARGIN = 2, FUN = GeometricMean)
  # the closed vector of geometric means is the mean (centroid)
  # of the compositional data set
  centre = g/sum(g)
  # perturbating the original composition by the inverse
  # centroid centers the composition around the centroid
  return(prop.table(t(t(P)*(1/centre)), margin = 1))
}

#' Coordinates and Labels for the Centered Gridlines of a Ternary Diagram
TernaryCentroidGrid <- function (centroid) {

  # centroid percent difference labels
  labels = seq(-1, 1, 0.1)
  labels = data.frame(
    L = labels[labels >= -centroid[1]][1:10],
    T = labels[labels >= -centroid[2]][1:10],
    R = labels[labels >= -centroid[3]][1:10]
  )

  # breaks of uncentered grid
  breaks = data.frame(
    L = labels$L + centroid[1],
    T = labels$T + centroid[2],
    R = labels$R + centroid[3]
  )

  # grid L
  gridL =
    data.frame(
      scale = 'L',
      centroid = ifelse(breaks$L == centroid[1], TRUE, FALSE),
      L_from = breaks$L,
      T_from = 1-breaks$L,
      R_from = 0,
      L_to = breaks$L,
      T_to = 0,
      R_to = 1-breaks$L
    )

  # grid T
  gridT =
    data.frame(
      scale = 'T',
      centroid = ifelse(breaks$T == centroid[2], TRUE, FALSE),
      L_from = 0,
      T_from = breaks$T,
      R_from = 1-breaks$T,
      L_to = 1-breaks$T,
      T_to = breaks$T,
      R_to = 0
    )

  # grid R
  gridR =
    data.frame(
      scale = 'R',
      centroid = ifelse(breaks$R == centroid[3], TRUE, FALSE),
      L_from = 1-breaks$R,
      T_from = 0,
      R_from = breaks$R,
      L_to = 0,
      T_to = 1-breaks$R,
      R_to = breaks$R
    )

  # grid line coordinates of uncentered grid
  grid = rbind(gridL, gridT, gridR)

  # grid line coordinates of centered grid
  cgrid = data.frame(
    grid[,1:2],
    prop.table(t(t(grid[,3:5])*(1/centroid)), margin = 1),
    prop.table(t(t(grid[,6:8])*(1/centroid)), margin = 1)
  )

  # breaks of centered grid
  cbreaks = data.frame(L = cgrid[cgrid$scale == 'L', 'L_from'],
                       T = cgrid[cgrid$scale == 'T', 'T_from'],
                       R = cgrid[cgrid$scale == 'R', 'R_from'])

  list(grid = grid, cgrid = cgrid,
       breaks = breaks, cbreaks = cbreaks, labels = labels)

}

# Data --------------------------------------------------------------------

# raw proportions
P = as.data.frame(
  prop.table(
    cbind(runif(30)+2, runif(30)+1, runif(30)),
    margin = 1
  )
)

# compositional center/mean
g = apply(P, MARGIN = 2, GeometricMean)
center = g/sum(g)
center

# differences from center
diff_from_center = t(t(P)-center)
diff_from_center
rowSums(diff_from_center)

# centered proportions
cP = as.data.frame(Centre(P))

# raw and centered ternary grid-lines
grids = TernaryCentroidGrid(center)

# Raw, proportion labels --------------------------------------------------

p1 <-
  ggtern(grids$grid) +
  geom_segment(aes(x = L_from, xend = L_to,
                   y = T_from, yend = T_to,
                   z = R_from, zend = R_to,
                   color = centroid),
               show.legend = FALSE) +
  geom_point(aes(x = V1, y = V2, z = V3), data = P) +
  scale_L_continuous(breaks = grids$breaks$L,
                     labels = round(grids$breaks$L, 2)) +
  scale_T_continuous(breaks = grids$breaks$T,
                     labels = round(grids$breaks$T, 2)) +
  scale_R_continuous(breaks = grids$breaks$R,
                     labels = round(grids$breaks$R, 2)) +
  scale_color_manual(values = c('grey', 'red')) +
  labs(x = 'L', y = 'T', z = 'R') +
  theme_bw() +
  theme_nogrid() +
  ggtitle('Proportions',
          subtitle = paste0('Center: ',
                            paste0(round(center, 2),
                                   collapse = ', ')))

# Raw, difference labels --------------------------------------------------

p2 <-
  ggtern(grids$grid) +
  geom_segment(aes(x = L_from, xend = L_to,
                   y = T_from, yend = T_to,
                   z = R_from, zend = R_to,
                   color = centroid),
               show.legend = FALSE) +
  geom_point(aes(x = V1, y = V2, z = V3), data = P) +
  scale_L_continuous(breaks = grids$breaks$L,
                     labels = grids$labels$L) +
  scale_T_continuous(breaks = grids$breaks$T,
                     labels = grids$labels$T) +
  scale_R_continuous(breaks = grids$breaks$R,
                     labels = grids$labels$R) +
  scale_color_manual(values = c('grey', 'red')) +
  labs(x = 'L', y = 'T', z = 'R') +
  theme_bw() +
  theme_nogrid() +
  ggtitle('Percent-point differences from center')

# Centered difference labels ----------------------------------------------

p3 <-
  ggtern(grids$cgrid) +
  geom_segment(aes(x = L_from, xend = L_to,
                   y = T_from, yend = T_to,
                   z = R_from, zend = R_to,
                   color = centroid),
               show.legend = FALSE) +
  geom_point(aes(x = V1, y = V2, z = V3), data = cP) +
  scale_L_continuous(breaks = grids$cbreaks$L,
                     labels = grids$labels$L) +
  scale_T_continuous(breaks = grids$cbreaks$T,
                     labels = grids$labels$T) +
  scale_R_continuous(breaks = grids$cbreaks$R,
                     labels = grids$labels$R) +
  scale_color_manual(values = c('grey', 'red')) +
  labs(x = 'L', y = 'T', z = 'R') +
  theme_bw() +
  theme_nogrid() +
  ggtitle('Percent-point differences from center (centered)')

# Centered, proportion labels ---------------------------------------------

p4 <-
  ggtern(grids$cgrid) +
  geom_segment(aes(x = L_from, xend = L_to,
                   y = T_from, yend = T_to,
                   z = R_from, zend = R_to,
                   color = centroid),
               show.legend = FALSE) +
  geom_point(aes(x = V1, y = V2, z = V3), data = cP) +
  scale_L_continuous(breaks = grids$cbreaks$L,
                     labels = round(grids$breaks$L, 2)) +
  scale_T_continuous(breaks = grids$cbreaks$T,
                     labels = round(grids$breaks$T, 2)) +
  scale_R_continuous(breaks = grids$cbreaks$R,
                     labels = round(grids$breaks$R, 2)) +
  scale_color_manual(values = c('grey', 'red')) +
  labs(x = 'L', y = 'T', z = 'R') +
  theme_bw() +
  theme_nogrid() +
  ggtitle('Proportions (centered)')

grid.arrange(p1, p3, p2, p4)
