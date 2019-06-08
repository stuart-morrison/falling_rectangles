# Author: Stuart Morrison
# Date: 28 May 2019
library(tidyverse, quietly = T)
library(magrittr, quietly = T)
library(scales, quietly = T)

"###############################################################################
#### Diagonal lines ####
###############################################################################"

################################################################################
#### Function to make segments ####
################################################################################

    segment_maker <- function(x, y, group,
                              normal_force) {

        temp <- tibble(x_0 = x, y_0 = y, group = group,
                       xx = x + (normal_force / sqrt(2)),
                       yy = y + (normal_force / sqrt(2)))

        return(temp)

    }

    normal_force_maker <- function(x_mid, y_mid, radius,
                                   x, y, severity = 0.5) {

        if (sqrt((x_mid - x) ^ 2 + (y_mid - y) ^ 2) <= radius) {
            degree <- runif(n = 1, min = -severity, max = severity)
            return(degree)
        } else {
            return(0)
        }

    }

################################################################################
#### Make data ####
################################################################################

    x_res <- 50
    y_res <- 28
    group <- 0
    lines <- tibble()

    for (y_int in (1 - x_res):(y_res - 1)) {
        group <- group + 1
        for (xx in 1:x_res) {
            if (((y_int + xx) >= 1) & ((y_int + xx) <= y_res)) {
                temp_normal <- normal_force_maker(x_mid = x_res / 2, y_mid = y_res / 2,
                                                  radius = min(x_res / 4, y_res / 4),
                                                  x = xx, y = (y_int + xx))

                lines %<>% bind_rows(tibble(x = xx + temp_normal / sqrt(2),
                                            y = (y_int + xx) - temp_normal / sqrt(2),
                                            group = y_int))

            }


        }

    }

################################################################################
#### Interpolate lines ####
################################################################################

    splines <- tibble()

    for (i in unique(lines$group)) {

        temp_data <- lines %>%
                        filter(group == i)

        splines %<>% bind_rows(as_tibble(spline(temp_data$x, temp_data$y, method = "natural", n = 9 * nrow(temp_data))) %>%
                                   mutate(group = i))

    }

################################################################################
#### Plot ####
################################################################################

    g <- ggplot() +
            coord_fixed() +
            geom_path(data = splines,
                        aes(x = x, y = -y, group = group),
                        col = "#FFFFFF") +
            theme_void() +
            theme(plot.background = element_rect(fill = "#000000",
                                                 colour = NA))
    ggsave(filename = "Charts/diagonal_lines.png", plot = g,
           width = 26.7, height = 15, units = "in", bg = "#000000")

################################################################################
#### Plot smaller version ####
################################################################################

    ggsave(filename = "Charts/diagonal_lines_lo_res.png", plot = g,
           width = 13.35, height = 7.5, units = "in", bg = "#000000")

################################################################################
#### Plot in cream ####
################################################################################

    g <- ggplot() +
            coord_fixed() +
            geom_path(data = splines,
                      aes(x = x, y = -y, group = group),
                      col = "#182E3A",
                      size = 2) +
            theme_void() +
            theme(plot.background = element_rect(fill = "#fffff0",
                                                 colour = NA))
    ggsave(filename = "Charts/diagonal_lines_cream.png", plot = g,
           width = 26.7, height = 15, units = "in", bg = "#fffff0")

