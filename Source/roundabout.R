# Author: Stuart Morrison
# Date: 28 May 2019
library(tidyverse)
library(magrittr)
library(gifski)
library(schemr)

# Function to make position
position <- function(theta, tick_total, radius = 1) {

    x <- cos(theta + (0:(tick_total - 1) / tick_total) * 2 * pi) * radius * cos(theta)
    y <- cos(theta + (0:(tick_total - 1) / tick_total) * 2 * pi) * radius * sin(theta)

    temp <- tibble(
        theta = theta,
        tick = 0:(tick_total - 1),
        x = x,
        y = y
    )

    return(temp)

}

segment_maker <- function(theta, radius = 1) {

    x_start <- 0
    x_end <- radius * cos(theta)
    y_start <- 0
    y_end <- radius * sin(theta)

    temp <- tibble(
        theta = theta,
        x_start = x_start,
        x_end = x_end,
        y_start = y_start,
        y_end = y_end

    )

    return(temp)

}

number_theta <- 8
ticks <- 75

# Create data
position_data <- map_df(
    .x = 1:number_theta / number_theta * pi,
    .f = position,
    tick_total = ticks
)

segment_data <- map_df(
    .x = 1:(number_theta * 2) / (number_theta * 2) * 2 * pi,
    .f = segment_maker
)

ratio <- 2532 / 1170

colour_1 <- hex_to_rgb("#ffa7a6")
colour_2 <- hex_to_rgb("#ffab6b")

colours <- tibble()

position_data %<>%
    bind_rows(position_data %>%
                  mutate(tick = tick + ticks))

red_step <- (colour_2$red - colour_1$red) / ticks
green_step <- (colour_2$green - colour_1$green) / ticks
blue_step <- (colour_2$blue - colour_1$blue) / ticks

for (i in unique(position_data$tick)) {

    if (i >= ticks) {
        signsign <- -1
        step <- ticks
        red_ <- colour_2$red
        green_ <- colour_2$green
        blue_ <- colour_2$blue


    } else {
        signsign <- 1
        red_ <- colour_1$red
        green_ <- colour_1$green
        blue_ <- colour_1$blue
        step <- 0
    }

    temp <-
        tibble(
            tick = i,
            red = red_ + red_step * (i - step) * signsign,
            green = green_ + green_step * (i - step) * signsign,
            blue = blue_ + blue_step * (i - step) * signsign
        )

    colours %<>% bind_rows(temp)

}

colours_hex <- rgb_to_hex(colours %>% select(-tick))


for (i in unique(position_data$tick)) {
    g <-
    ggplot() +
        geom_path(
            data =
                tibble(x = cos(0:300 / 300 * 2 * pi),
                       y = sin(0:300 / 300 * 2 * pi)),
            aes(x = x, y = y),
            col = "#000000",
            size = 1
        ) +
        geom_segment(
            data = segment_data,
            aes(x = x_start, xend = x_end,
                y = y_start, yend = y_end),
            size = 1,
            col = "#000000"
        ) +
        geom_point(
            data = position_data %>%
                filter(tick == i),
            aes(x = x, y = y),
            size = 16, col = "#FFFFFF"
        ) +
        theme_void() +
        theme(panel.background = element_rect(fill = colours_hex[i + 1]),
              panel.border = element_blank()) +
        coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-2 * 1.3, 2 * 1.3))
    ggsave(filename = paste0("Charts/circles/", i, ".png"), plot = g,
           width = 6, height = 12, units = "in")
}

frames <- paste0("Charts/circles/", unique(position_data$tick), ".png")

gifski(png_files = frames, gif_file = "Charts/circle.gif", width = 1080, height = floor(1080 * 2), loop = TRUE, delay = 1 / 24)
