source("//HKDC01/Shared Folders/Company/ggplot_theme/HK_header.r")
# Author: Stuart Morrison
# Date: 28 May 2019
rm(list = ls())
library(tidyverse, quietly = T)
library(magrittr, quietly = T)
library(scales, quietly = T)

# Import the HoustonKemp ggplot theme
source("//HKDC01/Shared Folders/Company/ggplot_theme/houstonkemp_theme.r")
source("//HKDC01/Shared Folders/Company/ggplot_theme/custom_functions.r")

"###############################################################################
#### Falling rectangles ####
###############################################################################"

################################################################################
#### Function to make segments ####
################################################################################

    segment_maker <- function(x, y, rotation) {

        rotation_matrix <- matrix(c(cos(rotation), -sin(rotation),
                                    sin(rotation), cos(rotation)),
                                  byrow = TRUE, nrow = 2)

        a <- rotation_matrix %*% matrix(c(0.5, -0.5), nrow = 2)
        b <- rotation_matrix %*% matrix(c(0.5, 0.5), nrow = 2)
        c <- rotation_matrix %*% matrix(c(-0.5, 0.5), nrow = 2)
        d <- rotation_matrix %*% matrix(c(-0.5, -0.5), nrow = 2)

        temp <- tibble(x_0 = x, y_0 = y, point = paste(x, y, sep = "-"),
                       x = c(a[1, 1], b[1, 1], c[1, 1], d[1, 1]),
                       xend = c(b[1, 1], c[1, 1], d[1, 1], a[1, 1]),
                       y = c(a[2, 1], b[2, 1], c[2, 1], d[2, 1]),
                       yend = c(b[2, 1], c[2, 1], d[2, 1], a[2, 1]))


    }

################################################################################
#### Make data ####
################################################################################

    rectangles <- tibble()
    for (xx in 1:25) {
        for (yy in 1:14) {
            temp_rotation <- case_when(yy > 3 ~ runif(n = 1,
                                                      min = -pi / 3.5 * yy / 25,
                                                      max = pi / 3.5 * yy / 25),
                                       TRUE ~ 0)

            rectangles %<>% bind_rows(segment_maker(x = xx,
                                                    y = yy,
                                                    rotation = temp_rotation))
        }
    }

################################################################################
#### Plot ####
################################################################################

    g <- ggplot() +
            coord_fixed() +
            geom_segment(data = rectangles,
                         aes(x = x + x_0, y = -y - y_0,
                             xend = xend + x_0, yend = -yend - y_0,
                             group = point),
                         col = "#FFFFFF") +
            theme_void() +
            theme(plot.background = element_rect(fill = "#000000",
                                                 colour = NA))
    ggsave(filename = "Charts/falling_rectangles.png", plot = g,
           width = 26.7, height = 15, units = "in", bg = "#000000")