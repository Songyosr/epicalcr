# Colors schemes -----------------------------------------------------------
## Preparation -------------------------------------------------------------

# Define Epi_color
epi_colors <- c(
  "blue" = "#003C71",
  "red" = "#DF2935",
  "yellow" = "#FDCA40",
  "green" = "#44AF69",
  "light blue" = "#7EBDC2",
  "purple" = "#7F055F",
  "grey" = "#676569",
  "NA grey" = "#BBB5BD",
  "light" = "#FFFBFE",
  "dark grey" = "#131515",
  "black" = "black",
  "white" = "white"
)

# Define [descrete palette function]
#' @export
epi_pal <- function(primary = "blue",
                    other = "red",
                    direction = 1) {
  stopifnot(primary %in% names(epi_colors))

  function(n = NULL) {
    if (is.null(n)) {
      return(epi_colors[primary])
    }
    if (n > 7) warning("Epi Color Palette only has 7 colors.")

    if (n == 2) {
      other <- if (!other %in% names(epi_colors)) {
        other
      } else {
        epi_colors[other]
      }
      color_list <- c(other, epi_colors[primary])
    } else {
      color_list <- epi_colors[1:n]
    }

    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}

# Example
# epi_pal(other = "red")(3)
# epi_pal("NA grey")()



## Building GGPLOT scales --------------------------------------------------


### Discrete Scales ---------------------------------------------------------

# scale_color
#' @export
scale_colour_epi <- function(primary = "blue", other = "light blue",
                             direction = 1, na.value = epi_pal("NA grey")(), ...) {
  ggplot2::discrete_scale(
    "colour", "epi",
    epi_pal(primary, other, direction),
    na.value = na.value,
    ...
  )
}

#' @export
scale_color_epi <- scale_colour_epi

# scale_fill
#' @export
scale_fill_epi <- function(primary = "blue", other = "light blue",
                           direction = 1, na.value = epi_pal("NA grey")(), ...) {
  ggplot2::discrete_scale(
    "fill", "epi",
    epi_pal(primary, other, direction),
    na.value = na.value,
    ...
  )
}

# Examples
# BP %>%
#   ggplot(aes(sbp, dbp, color = sex))+
#   geom_point(size = 3)+
#   theme_epi()+
#   scale_color_epi()


# Theme -------------------------------------------------------------------
element_blank <- ggplot2::element_blank()
element_line <- ggplot2::element_line()
element_rect <- ggplot2::element_rect()
element_text <- ggplot2::element_text()

#' @export
theme_epi <- function(discretes = TRUE, ...) {
  ggplot2::theme_minimal(base_family = "Ubuntu Condensed") +
    ggplot2::theme(
      text = element_text(color = epi_pal("black")()),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", size = 0.3, color = epi_pal("dark grey")()),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 25, margin = margin(b = 10), hjust = 0.030),
      plot.subtitle = element_text(size = 15, margin = margin(b = 20), hjust = 0.030),
      plot.caption = element_text(size = 15, margin = margin(b = 0, t = 10), color = epi_pal("grey")()),
      axis.text.y = element_text(size = 18, color = epi_pal("grey")()),
      axis.text.x = element_text(size = 18, color = epi_pal("dark grey")()), # element_blank(),
      strip.text = element_text(color = epi_pal("dark grey")(), size = 20, face = "bold", hjust = 0.030),
      plot.background = element_rect(fill = epi_pal("white")(), color = NA),
      plot.margin = unit(c(2, 2, 2, 2) / 2.5, "cm"),
      # legend.key = element_rect(colour = "transparent", fill = "white"),
      # legend.background = element_rect(alpha = 0),
      legend.position = "top",
      legend.spacing = unit(0.1, "lines"),
      legend.title = element_text(family = "Ubuntu Condensed", size = 20),
      legend.text = element_text(family = "Ubuntu Condensed", size = 18),
      legend.text.align = 0
    ) +
    theme(...)
}
