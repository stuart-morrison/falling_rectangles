library(tidyverse)
library(mvtnorm)
library(magrittr)

squiggle_line <- function(xoffset, yoffset) {

    # xoffset <- 0
    # yoffset <- 0

    vert <- tibble(
        x = c(0, 4, 8, 12) + xoffset, 
        xend = c(0, 4, 8, 12) + xoffset, 
        y = c(0, 0, 0, 0 ) + yoffset, 
        yend = c(2, 2, 2, 2) + yoffset
        )

    diagon <- tibble(
        x = c(1, 1, 2, 2) + xoffset, 
        xend = c(1, 2, 2, 3) + xoffset, 
        y = c(0, 1, 1, 2) + yoffset, 
        yend = c(1, 1, 2, 2) + yoffset
        ) %>% 
        bind_rows(
            tibble(
                x = c(5, 5, 6, 6) + xoffset, 
                xend = c(5, 6, 6, 7) + xoffset, 
                y = c(2, 1, 1, 0) + yoffset, 
                yend = c(1, 1, 0, 0) + yoffset
                )   
        ) %>% 
        bind_rows(
            tibble(
                x = c(9, 10, 10, 11) + xoffset, 
                xend = c(10, 10, 11, 11) + xoffset, 
                y = c(0, 0, 1, 1) + yoffset, 
                yend = c(0, 1, 1, 2) + yoffset
                )   
        ) %>% 
        bind_rows(
            tibble(
                x = c(13, 14, 14, 15) + xoffset, 
                xend = c(14, 14, 15, 15 ) + xoffset, 
                y = c(2, 2, 1, 1) + yoffset, 
                yend = c(2, 1, 1, 0) + yoffset
                )   
        )

    strike <- tibble(
        x = c(2, 6, 9, 13) + xoffset, 
        xend = c(3, 7, 10, 14) + xoffset, 
        y = c(0, 2, 1, 1) + yoffset, 
        yend = c(1, 1, 2, 0) + yoffset
    )

    return(bind_rows(vert, diagon, strike))

}

squiggle_points <- function(xoffset, yoffset) {
    point <- tibble(
        x = c(1, 3, 5, 7, 9, 11, 13, 15) + xoffset,
        y = c(2, 0, 0, 2, 2, 0, 0, 2) + yoffset
    )
}


sin_field <- function(x, y, alpha, offset, scale) {
    p = cos(alpha) * x + sin(alpha) * y
    return(sin((p + offset) / scale))
}

generate_one_grid <- function(alpha, offset, scale, x, y) {
    gather <- sin_field(x, y,  alpha, offset, scale)

    gather_table <- tibble(x = x, y = y, value = gather)

    return(gather_table)

}

random_draw <- function(n, width, height) {

    x <- 0:width

    y <- 0:height

    this_data <- crossing(tibble(x = x), tibble(y = y))

    this_alpha <- runif(1) * pi * 2

    this_offset <- runif(1) * pi * 2

    this_scale <- 
        sample(
            c(
                rexp(n = 1, rate = 1 / (width * 10)),
                rexp(n = 1, rate = 1 / (width * 5)),
                rexp(n = 1, rate = 1 / (width / 3)),
                rexp(n = 1, rate = 1 / (width / 10)),
                rt(n = 1, df = 1) * width,
                width / 5
            ),
            1            
        )
        

    this_grid_data <- generate_one_grid(this_alpha, this_offset, this_scale, this_data$x, this_data$y)

    return(this_grid_data)

}


random_contour <- function(n, width, height) {

    contour_data <- map_df(.x = 0:n, random_draw, width = width * 1.1, height = height)

    contour_data %<>% 
                    group_by(x, y) %>% 
                        summarise(value = sum(value)) %>% 
                        ungroup()

    return(contour_data)

}


width <- 200
height <- 200

offset_points <- crossing(x = ((-1 * (width %/% 16)) - 10):((width %/% 16) + 3) * 16, y = 0:(height %/% 3) * 3)
offset_points %<>% mutate(x = x + 5 * (y %/% 3))

lines_ <- pmap_df(
            .l = list(
                xoffset = offset_points$x,
                yoffset = offset_points$y
            ),
            .f = squiggle_line
)

points_ <- pmap_df(
            .l = list(
                xoffset = offset_points$x,
                yoffset = offset_points$y
            ),
            .f = squiggle_points
)

for (i in 1:350) {

    these_colours <- random_contour(10, width, height)

    g <- ggplot() +
        geom_point(aes(x = x, y = y, col = value), size = 1.2,
        data = points_  %>% inner_join(these_colours, by = c("x", "y"))) +
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend, col = value),
        data = lines_ %>% inner_join(these_colours, by = c("xend" = "x", "y")),
        size = 1.2) +
        theme_void() +
        scale_colour_gradient2(
            low = "#ba7c03", 
            mid = "#ca9aaa", 
            high = "#033a3e", 
            midpoint = 0
        ) +
        theme(legend.position="none", panel.background = element_rect(fill = "#f2e5d9")) + 
        coord_fixed()

    ggsave(filename = paste0("Charts/contours/", i, ".png"), plot = g,
    width = 18, height = 18, unit = "in", limitsize = FALSE)

}

