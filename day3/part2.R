input_file <- "day3/input/real"

steps_right <- c(1, 3, 5, 7, 1)
steps_down <- c(1, 1, 1, 1, 2)
arboreal_stops <- integer(length(steps_right))


library(purrr)
library(stringr)
map_raw <-readLines(input_file)

width <- nchar(map_raw[1])

my_map <- paste0(map_raw, collapse = "") %>%
    {map_chr(seq_len(nchar(.)), .f = function(x) str_sub(., x, x))} %>%
    matrix( ncol = width, byrow = TRUE)


depth <- nrow(my_map)

reps <- ceiling(depth * max(steps_right) / width)

full_map <- my_map

for (i in 2:reps) {
    full_map <- cbind(full_map, my_map)
}


for (j in seq_along(steps_right)) {
    
    trees <- 0
    for (i in seq_len(depth)) {
        if (steps_down[j] * (i - 1) + 1 > depth) {
            break()
        }
        if (full_map[steps_down[j]*(i - 1) + 1, steps_right[j]*(i - 1) + 1] == "#") {
            trees <- trees + 1
        }
    }
    usethis::ui_info("For slope [Right: {steps_right[j]}, Down: {steps_down[j]}] I found {trees} trees.")
    arboreal_stops[j] <- trees
}

usethis::ui_done("The product of slopes is {prod(arboreal_stops)}.")