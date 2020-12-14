library(purrr)
library(stringr)
library(tibble)
library(dplyr)
library(tidyr)

input_file <- "day11/input/test_start"

import_map <- function(input_file) {
    lines <- readLines(input_file)
    
    cols <- nchar(lines[1])
    rows <- length(lines)
    
    map_data <- matrix(rep("", cols * rows), nrow = rows)
    
    for (i in seq_along(lines)) {
        map_data[i, ] <- map_chr(seq_len(cols), ~ str_sub(lines[i], .x, .x))
    }
    map_data
}

map_data <- import_map(input_file)

get_num_adjacent_occupied <- function(i, j, map_data) {
    rows <- (i - 1):(i + 1)
    cols <- (j - 1):(j + 1)
    tmp <- crossing(ii = rows, jj = cols) %>%
        filter(ii >= 1 & ii <= nrow(map_data) & jj >= 1 & jj <= ncol(map_data)) %>%
        filter(!(ii == i & jj == j)) %>%
        mutate(seat = map_data[ii, jj]) %>%
        filter(seat == "#")
    
    nrow(tmp)
    
}

apply_rules <- function(i, j, map_data) {
    if (map_data[i, j] == ".") {
        return(".")
    }
    num_occ <- get_num_adjacent_occupied(i, j, map_data)
    
    if (map_data[i, j] == "L" & num_occ == 0) {
        return("#")
    }
    
    if (map_data[i, j] == "#" & num_occ >= 4) {
        return("L")
    }
    
    map_data[i, j]
    
}

update_map <- function(map_data) {
    new_map_data <- map_data
    for(i in seq_len(nrow(map_data))) {
        for(j in seq_len(ncol(map_data))) {
            new_map_data[i, j] <- apply_rules(i, j, map_data)
        }
    }
    new_map_data
}

round1_data <- update_map(map_data)
round1_check <- import_map("day11/input/test_round1")

if(all(round1_data == round1_check)) {
    usethis::ui_done("Round 1 checks out.")
}

round2_data <- update_map(round1_data)
round2_check <- import_map("day11/input/test_round2")


if(all(round2_data == round2_check)) {
    usethis::ui_done("Round 2 checks out.")
}

# is_static <- FALSE
# prior_map_data <- map_data
# cntr <- 1
# while (!is_static) {
#     usethis::ui_info("Round: {cntr}")
#     map_data <- update_map(prior_map_data)
#     if (map_data == prior_map_data) {
#         is_static <- TRUE
#     }
#     cntr <- cntr + 1
#     prior_map_data <- map_data
# }