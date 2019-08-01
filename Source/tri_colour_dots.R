# Author: Stuart Morrison
# Date: 28 May 2019
library(tidyverse, quietly = T)
library(magrittr, quietly = T)
library(scales, quietly = T)

"###############################################################################
#### Tri-colour dots ####
###############################################################################"

################################################################################
#### Function to make segments ####
################################################################################

    dot_maker <- function(x, y, size_force) {



        temp <- tibble(x = x + 1 / (2 + sin(pi / 3)) * cos(2 * pi * (1:3) / 3 + pi / 2),
                       y = y + 1 / (2 + sin(pi / 3)) * sin(2 * pi * (1:3) / 3 + pi / 2),
                       colour = letters[1:3],
                       size = size_force)

        return(temp)
    }

    size_force_maker <- function(x_mid, y_mid, radius,
                                 x, y, severity = 6, regular = 3) {

        if (sqrt((x_mid - x) ^ 2 + (y_mid - y) ^ 2) <= radius) {
            degree <- runif(n = 1, min = 0.5, max = severity)
            return(degree)
        } else {
            return(regular)
        }

    }

################################################################################
#### Make data ####
################################################################################

    x_res <- 100
    y_res <- 56
    dots <- tibble()

    for (xx in 1:x_res) {
        for (yy in 1:y_res) {
            temp_size <- size_force_maker(x_mid = x_res / 2, y_mid = y_res / 2,
                                          radius = min(x_res / 4, y_res / 4),
                                          x = xx, y = yy)

            dots %<>% bind_rows(dot_maker(x = xx, y = yy + 0.5 * (xx %% 2),
                                          size_force = temp_size))
        }
    }

################################################################################
#### Plot ####
################################################################################

    g <- ggplot() +
            coord_fixed() +
            geom_point(data = dots,
                        aes(x = x, y = y,
                            col = colour),
                      size = dots$size,
                      alpha = 1) +
            scale_color_manual(name = "",
                               values = c("#b5eef1", "#e4b8f5", "#A52A2A")) +
            theme_void() +
            theme(plot.background = element_rect(fill = "#fffff0",
                                                 colour = NA)) +
            guides(col = FALSE)
    ggsave(filename = "Charts/tri_colour_dots.png", plot = g,
           width = 26.7, height = 15, units = "in", bg = "#eee8dc")

################################################################################
#### Plot smaller version ####
################################################################################

    ggsave(filename = "Charts/tri_colour_dots_lo_res.png", plot = g,
           width = 13.35, height = 7.5, units = "in", bg = "#000000")
