# Functions for the centered ternary balance scheme
#
# The functions defined below are simplified versions of those
# implemented in the R package "tricolore".

# Compositional data analysis -------------------------------------

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
#' GeometricMean(0:100)
#' GeometricMean(0:100, zero.rm = FALSE)
GeometricMean <- function (x, na.rm = TRUE, zero.rm = TRUE) {
  # the geometric mean can't really deal with elements equal to 0
  # this option removes 0 elements from the vector
  if (zero.rm) { x <- x[x!=0] }
  return(exp(mean(log(x), na.rm = na.rm)))
}

#' Compositional Centre
#'
#' Calculate the centre of a compositional data set.
#'
#' @param P n by m matrix of compositions
#'          {p1, ..., pm}_i for i=1,...,n.
#'
#' @return The centre of P as an m element numeric vector.
#'
#' @examples
#' P <- prop.table(matrix(runif(300), 100), margin = 1)
#' Centre(P)
#'
#' @references
#' Von Eynatten, H., Pawlowsky-Glahn, V., & Egozcue, J. J. (2002).
#' Understanding perturbation on the simplex: A simple method to
#' better visualize and interpret compositional data in ternary
#' diagrams. Mathematical Geology, 34(3), 249-257.
#'
#' Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosana-Delgado, R.
#' (2007). Lecture Notes on Compositional Data Analysis.
#' Retrieved from
#' https://dugi-doc.udg.edu/bitstream/handle/10256/297/CoDa-book.pdf
Centre <- function (P) {
  # calculate the geometric mean for each element
  # of the composition
  g <- apply(P, MARGIN = 2, FUN = GeometricMean)
  # the closed vector of geometric means is the mean (centre)
  # of the compositional data set
  return(g/sum(g))
}

#' Compositional Perturbation
#'
#' Perturbate a compositional data set by a compositional vector.
#'
#' @param P n by m matrix of compositions
#'         {p1, ..., pm}_i for i=1,...,n.
#' @param c Compositional perturbation vector {c1, ..., cm}.
#'
#' @return n by m matrix of perturbed compositions.
#'
#' @examples
#' P <- prop.table(matrix(runif(12), 4), margin = 1)
#' cP <- Perturbate(P, 1/Centre(P))
#' Perturbate(cP, Centre(P)) - P
#'
#' @references
#' Von Eynatten, H., Pawlowsky-Glahn, V., & Egozcue, J. J. (2002).
#' Understanding perturbation on the simplex: A simple method to
#' better visualize and interpret compositional data in ternary
#' diagrams. Mathematical Geology, 34(3), 249-257.
#'
#' Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosana-Delgado, R.
#' (2007). Lecture Notes on Compositional Data Analysis.
#' Retrieved from
#' https://dugi-doc.udg.edu/bitstream/handle/10256/297/CoDa-book.pdf
Perturbate <- function (P, c = rep(1/3, 3)) {
  return(prop.table(t(t(P)*c), margin = 1))
}

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

# Ternary geometry ------------------------------------------------

# T(K=k^2):   Equilateral triangle subdivided into K equilateral
#             sub-triangles. Each side of T is divided into k
#             intervals of equal length.
# (p1,p2,p3): Barycentric coordinates wrt. T(K).
# id:         One-dimensional index of sub-triangles in T(K).
#
#                  p2           id index
#                  /\               9
#                 /  \            6 7 8
#                /____\         1 2 3 4 5
#              p1      p3

#' Centroid Coordinates of Sub-Triangles in Segmented Equilateral Triangle
#'
#' Segment an equilateral triangle into k^2 equilateral
#' sub-triangles and return the barycentric centroid
#' coordinates of each sub-triangle.
#'
#' @param k Number of rows in the segmented equilateral triangle.
#'
#' @return A matrix of barycentric centroid coordinates of regions
#'         id=1,...,k^2.
#'
#' @references
#' S. H. Derakhshan and C. V. Deutsch (2009): A Color Scale for
#' Ternary Mixtures.
#'
#' @examples
#' TernaryMeshCentroids(1)
#' TernaryMeshCentroids(2)
#' TernaryMeshCentroids(3)
TernaryMeshCentroids <- function (k) {
  # total number of centroids and centroid id
  K = k^2; id = 1:K
  
  # centroid coordinates as function of K and id
  g <- floor(sqrt(K-id)); gsq <- g^2
  c1 <- (((-K + id + g*(g+2) + 1) %% 2) - 3*gsq - 3*id + 3*K + 1) / (6*k)
  c2 <- -(((-K + gsq + id + 2*g + 1) %% 2) + 3*g - 3*k + 1) / (3*k)
  c3 <- (((-K + gsq + id + 2*g + 1) %% 2) + 3*gsq + 6*g + 3*id - 3*K + 1) / (6*k)
  
  return(cbind(id = id, p1 = c1, p2 = c2, p3 = c3))
}

#' Vertex Coordinates of Sub-Triangles in Segmented Equilateral Triangle
#'
#' Given the barycentric centroid coordinates of the sub-triangles in
#' an equilateral triangle subdivided into k^2 equilateral
#' sub-triangles, return the barycentric vertex coordinates of each
#' sub-triangle.
#'
#' @param C n by 4 matrix of barycentric centroid coordinates of
#'          n=k^2 sub-triangles. Column order: id, p1, p2, p3 with
#'          id=1,...,k^2.
#'
#' @return Index, vertex id and barycentric vertex coordinates for
#'         each of the k^2 sub-triangles.
#'
#' @examples
#' k = 2
#' C <- TernaryMeshCentroids(k)
#' TernaryMeshVertices(C)
#'
#' @references
#' S. H. Derakhshan and C. V. Deutsch (2009): A Color Scale for Ternary Mixtures.
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

#' Coordinates and Labels for a Centered Ternary Diagram Grid
#'
#' @param center
#'   Composition that is to be shifted to the center.
#' @param N
#'   Number of grid lines to draw.
#' @param relative_grid
#'   Draw regularly spaced grid-lines relative to center?
TernaryGridCentered <- function (center, N = 10, relative_grid = TRUE) {
  
  # regularly spaced grid-lines relative to center
  if (isTRUE(relative_grid)) {
    # labels mark absolute difference from center coordinates
    prop_diff = seq(-1, 1, by = 1/N)
    labels = data.frame(
      L = prop_diff[prop_diff >= -center[1]][1:N],
      T = prop_diff[prop_diff >= -center[2]][1:N],
      R = prop_diff[prop_diff >= -center[3]][1:N]
    )
    # breaks of non-centered grid
    non_centered_breaks = data.frame(
      L = labels$L + center[1],
      T = labels$T + center[2],
      R = labels$R + center[3]
    )
  }
  
  # regularly spaced grid-lines over whole ternary surface
  if (!isTRUE(relative_grid)) {
    # labels mark proportions
    prop = seq(0, 1, by = 1/N)
    labels = data.frame(
      L = unique(sort(c(prop, center[1]))),
      T = unique(sort(c(prop, center[2]))),
      R = unique(sort(c(prop, center[3])))
    )
    # breaks of non-centered grid
    non_centered_breaks = labels
  }
  
  # grid L
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
  
  # grid T
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
  
  # grid R
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
  
  # grid line coordinates of non-centered grid
  non_centered_grid = rbind(gridL, gridT, gridR)
  
  # grid line coordinates of centered grid
  centered_grid = data.frame(
    # scale, center
    non_centered_grid[,1:2],
    # from
    prop.table(t(t(non_centered_grid[,3:5])*(1/center)), margin = 1),
    # to
    prop.table(t(t(non_centered_grid[,6:8])*(1/center)), margin = 1)
  )
  
  # breaks of centered grid
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

# Ternary color scale ---------------------------------------------

#' RGB Mixture of Ternary Composition
#'
#' Return the ternary balance scheme colors for a matrix of
#' ternary compositions.
#'
#' @param P n by 3 matrix of ternary compositions {p1, p2, p3}_i
#'            for i=1, ..., n.
#' @param h_ Primary hue of the first ternary element in angular
#'           degrees [0, 360].
#' @param c_ Maximum possible chroma of mixed colors [0, 200].
#' @param l_ Lightness of mixed colors [0, 100].
#' @param contrast Lightness contrast of the color scale [0, 1).
#' @param center Ternary coordinates of the grey-point.
#'
#' @return An n row data frame giving, for each row of the input P,
#'         the input proportions (p1, p2, p3), parameters of the
#'         color mixture (h, c, l) and the hexsrgb string of the mixed
#'         colors.
#'
#' @examples
#' P <- prop.table(matrix(runif(9), ncol = 3), 1)
#' ColorMap(P, h_ = 80, c_ = 170, l_ = 80,
#'          contrast = 0.6, center = rep(1/3, 3))
ColorMap <- function (P, h_, c_, l_, contrast, center) {
  
  # generate primary colors starting with a hue value in [0, 360) and then
  # picking two equidistant points on the circumference of the color wheel.
  # input hue in degrees, all further calculations in radians.
  phi <- (h_*0.0174 + c(0, 2.09, 4.19)) %% 6.28
  
  # closing
  P <- P_raw <- prop.table(P, margin = 1)
  # centering
  P <- Perturbate(P, 1/center)
  
  # calculate the chroma matrix C by scaling the row proportions
  # of the input matrix P by the maximum chroma parameter
  C <- P*c_
  
  # the complex matrix Z represents each case (i) and
  # group (j=1,2,3) specific color in complex polar form
  # with hue as angle and chroma as radius
  Z <-
    matrix(
      complex(
        argument = phi, modulus = c(t(C))
      ),
      ncol = 3, byrow = TRUE
    )
  
  # adding up the rows gives the CIE-Lab (cartesian) coordinates
  # of the convex color mixture in complex form
  z <- rowSums(Z)
  # convert the cartesian CIE-Lab coordinates to polar
  # CIE-Lch coordinates and add lightness level
  M <- cbind(h = (Arg(z)*57.3)%%360, c = Mod(z), l = l_)
  
  # decrease lightness and chroma towards the center of the color scale
  cfactor <- M[,2]*contrast/c_ + 1-contrast
  M[,3] <- cfactor*M[,3]
  M[,2] <- cfactor*M[,2]
  
  # convert the complex representation of the color mixture to
  # hex-srgb representation via the hcl (CIE-Lch) color space
  hexsrgb <- hcl(h = M[,1], c = M[,2], l = M[,3],
                 alpha = 1, fixup = TRUE)
  
  # original compositions, Perturbated compositions, hcl values of mixtures and hexsrgb code
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

# Ternary Balance Scheme Legend
#
# Plot a ternary balance scheme legend.
#
# @inheritParams ColorMap
#
# @examples
# ColorKey(h_ = 0, c_ = 140, l_ = 70, contrast = 0.5,
#          center = rep(1/3, 3))
ColorKey <- function (h_, c_, l_, contrast, center) {
  
  # partition the ternary legend into k^2 equilateral sub-triangles
  # calculate ternary vertex coordinates and fill color for each sub-triangle.
  k = 100
  C <- TernaryMeshCentroids(k)
  V <- TernaryMeshVertices(C)
  rgbs <- ColorMap(P = C[,-1], h_, c_, l_, contrast, center)[['hexsrgb']]
  legend_surface <- data.frame(V, rgb = rep(rgbs, 3))
  
  # plot the legend
  legend <-
    # basic legend
    ggtern(
      legend_surface,
      aes_string(x = 'p1', y = 'p2', z = 'p3')
    ) +
    geom_polygon(
      aes_string(group = 'id', fill = 'rgb', color = 'rgb'),
      lwd = 1
    ) +
    geom_mask() +
    # rgb color input
    scale_color_identity(guide = FALSE) +
    scale_fill_identity(guide = FALSE) +
    # theme
    theme_classic() +
    theme(
      tern.axis.title.L =
        element_text(hjust = 0.2, vjust = 1, angle = -60),
      tern.axis.title.R =
        element_text(hjust = 0.8, vjust = 0.6, angle = 60)
    )
  
  return(legend)
}
