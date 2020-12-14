input_file <- "day3/input/real"

library(purrr)
library(stringr)
map_raw <-readLines(input_file)

width <- nchar(map_raw[1])

my_map <- paste0(map_raw, collapse = "") %>%
    {map_chr(seq_len(nchar(.)), .f = function(x) str_sub(., x, x))} %>%
    matrix( ncol = width, byrow = TRUE)


depth <- nrow(my_map)

reps <- ceiling(depth * 3 / width)

full_map <- my_map

for (i in 2:reps) {
    full_map <- cbind(full_map, my_map)
}

trees <- 0

for (i in seq_len(depth)) {
    if (full_map[i, 3*(i - 1) + 1] == "#") {
        trees <- trees + 1
    }
}

usethis::ui_done("I found {trees} trees.")