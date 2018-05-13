# GeomPirate <- ggproto("GeomOurate", Geom,
#                       setup_data = function(data, params) {
#                         data$width <- data$width %||%
#                           params$width %||% (resolution(data$x, FALSE) * 0.9)
#
#                         # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
#                         plyr::ddply(data, "group", transform,
#                                     xmin = x - width / 2,
#                                     xmax = x + width / 2
#                         )
#                       },
#                                required_aes = c("x", "y"),
#                       default_aes = aes(
#                         shape = 19, colour = "black", size = 1.5, fill = "white",
#                         alpha = NA, stroke = 0.5
#                       ),
#                                draw_key = draw_key_point,
#
#                       draw_group = function(data,
#                                             panel_scales,
#                                             coord,
#                                             point.alpha = NA,
#                                             point.col = NA,
#                                             x.jitter = .1,
#                                             y.jitter = 0,
#                                             width.max = .07) {
#                                  ## Transform the data first
#
#                                  coords <- coord$transform(data, panel_scales)
#
#                                  newdata <- data <- data %>% arrange(x, y)
#
#                                  # Get unique values of x
#                                  x_unique <- sort(unique(coords$x))
#
#                                  # Set some defaults (should be based on data later)
#                                  # width.max <- .05 #data$width[2] / length(x_unique) / 2
#                                  dens.n <- 100
#
#                                  ## Calculate density
# #
#                                  # coords <- data.frame(x = rep(1:2, each = 10),
#                                  #                      y = rep(1:10, times =2))
# #                                  #
#
#                                  densities <- lapply(unique(coords$x), FUN = function(x) {
#
#                                    vec <- coords$y[coords$x == x]
#
#                                    if(length(vec) <= 4) {
#
#                                      NULL
#
#                                    } else {
#
#                                    density(vec, n = dens.n)
#
#                                    }
#
#                                  })
#
#                                  density.max <- max(unlist(lapply(1:length(densities),
#                                                                   FUN = function(x) {
#
#                                                                     if(is.null(densities[[x]]) == FALSE) {
#                                                                     max(densities[[x]]$y)
#                                                                     }
#
#                                                                     })))
#
#                                  df <- lapply(1:length(densities), FUN = function(x) {
#
#                                    if(is.null(densities[[x]]) == FALSE) {
#
#                                    xmin <- data$xmin[data$x == x_unique[x]][1]
#                                    xmax <- data$xmax[data$x == x_unique[x]][1]
#
#                                    out <- data.frame(x = c(x_unique[x] - (densities[[x]]$y / density.max) * width.max,
#                                                            rev(x_unique[x] + (densities[[x]]$y / density.max) * width.max)),
#                                                      y = c(densities[[x]]$x, rev(densities[[x]]$x)),
#                                                      group = x_unique[x],
#                                                      side = rep(c("l", "r"), each = length(densities[[x]]$y)))
#
#                                    out <- out[-nrow(out),]
#
#
#                                    # out <- out[out$y > min(coords$y[coords$y == x_unique[x]]),]
#                                    # out <- out[out$y < max(coords$y[coords$y == x_unique[x]]),]
#
#                                    return(out)
#
#                                    }
#                                  })
#
#
#
#                                  df <- do.call(rbind, df)
#                                  df <- df %>% arrange(group, side)
#
#
#
#                                  if(is.na(point.col)) {
#
#                                    point.col = coords$colour
#
#                                  }
#
#                                  if(is.na(point.alpha)) {
#
#
#                                    point.alpha = coords$alpha
#                                  }
#
#                                  point.col = alpha(point.col, point.alpha)
#
#
#
#                                  ### Add voilin
#                                  grobTree(
#
#                                  grid::polygonGrob(
#                                    x = df$x,
#                                    y = df$y,
#                                    id = df$group,
#                                    gp = gpar(
#                                      col = alpha(coords$colour, coords$alpha),
#                                      fill = alpha(coords$fill, coords$alpha),
#                                      # Stroke is added around the outside of the point
#                                      fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
#                                      lwd = coords$stroke * .stroke
#                                    )
#                                  ),
#
#                                  grid::pointsGrob(
#                                    x = coords$x + rnorm(length(coords$x), mean = 0, sd = x.jitter * width.max),
#                                    y = coords$y,
#                                    pch = coords$shape,
#                                    gp = gpar(
#                                      col = point.col,
#                                       fill = alpha(coords$fill, coords$alpha),
#                                      fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
#                                      lwd = coords$stroke * .stroke / 2
#                                    )
#                                  ))
#
#
#
#
#                                })
#
# geom_pirate <- function(mapping = NULL, data = NULL, stat = "identity",
#                              position = "identity", na.rm = FALSE,
#                              show.legend = NA, inherit.aes = TRUE, ...) {
#   ggplot2::layer(
#     geom = GeomPirate, mapping = mapping,
#     data = data, stat = stat, position = position,
#     show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
#
# }
#
# set.seed(100)
# mydata <- data.frame(x = rep(1:2, each = 100),
#                      y = c(rnorm(100, 10, 1), rnorm(100, 5, 1)))
#
# coords <- mydata
#
# ggplot(data = mpg, aes(factor(class), cty, fill = factor(year)))  + geom_pirate(point.alpha = .3)
#
