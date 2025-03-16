center = c(0.2, 0.5, 0.3)

C <- tricolore:::GetCentroids(10)
V <- tricolore:::GetVertices(C)

library(ggtern)

ggtern(as.data.frame(V)) +
  geom_polygon(aes(x = p1, y = p2, z = p3, group = id),
               fill = NA, color = 'black') +
  geom_point(aes(x = p1, y = p2, z = p3), data = as.data.frame(C)) +
  annotate('point', x = center[1], y = center[2], z = center[3], color = 'red') +
  tern_limit(T = 0.9, L = 0.8, R = 0.85) +
  theme_nogrid()
