#' Map theme
#'
#' @description Creates a standard theme for all my maps
#'
#' @param base_size Base size for the fonts
#'
#' @importFrom ggplot2 theme element_text rel element_blank element_rect element_line
#'
#' @export
#'
ggtheme_map <- function(base_size = 9) {

  theme(text = element_text(color = "gray30",
                            size = base_size),
        plot.title = element_text(size = rel(1.25),
                              hjust = 0,
                              face = "bold"),
        plot.title.position = "plot",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(size = base_size),
        axis.title = element_blank(),
        legend.text = element_text(size = base_size),
        legend.text.align = 0,
        legend.position = "bottom",
        legend.background = element_blank(),
        plot.background = element_blank(),
        plot.caption = element_text(hjust = 0.7),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = NA, fill = NA)
        )
}


#' Plot theme
#'
#' @description Creates a standard theme for all my plots
#'
#' @param font_size The size of the font for \code{element_text()}. Defaults to \code{font_size = 10}.
#' @param font_family The font family
#' @param line_size The size of the lines
#'
#' @importFrom ggplot2 %+replace% theme_gray theme element_rect element_text margin rel element_line element_blank unit
#' @export
#'
ggtheme_plot <- function (font_size = 10, font_family = "", line_size = 0.5) {

  half_line <- font_size / 2

  theme_gray(base_size = font_size, base_family = font_family) %+replace%
    theme(rect = element_rect(fill = "transparent",
                              colour = NA,
                              color = NA,
                              size = 0,
                              linetype = 0),
          text = element_text(family = font_family,
                              face = "plain",
                              colour = "black",
                              size = font_size,
                              hjust = 0.5,
                              vjust = 0.5,
                              angle = 0,
                              lineheight = 0.9,
                              margin = margin(),
                              debug = FALSE),
          axis.text = element_text(colour = "black",
                                   size = rel(0.8)),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black",
                                   size = line_size,
                                   lineend = "square"),
          axis.line.x = element_line(colour = "black",
                                     size = line_size,
                                     lineend = "square"),
          axis.line.y = element_line(colour = "black",
                                     size = line_size,
                                     lineend = "square"),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(1, "lines"),
          legend.spacing = unit(0.4, "cm"),
          legend.text = element_text(size = rel(0.8)),
          legend.justification = c("left", "center"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = rel(0.8)),
          strip.background = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(face = "bold",
                                    size = font_size,
                                    margin = margin(b = half_line)),
          complete = TRUE
    )
}

