
input_file <- "day7/input/example2"

raw_data <- readLines(input_file)
library(purrr)
library(stringr)

clean_data <- function(x) {
    all_data <- str_remove_all(x, pattern = "bags?") %>%
        str_split(pattern = "( contain |, )") %>%
        map( ~ trimws(str_remove_all(.x, pattern = "\\.")))
    
    all_data
}

parsed_data <- clean_data(raw_data)

extract_bag_labels <- map_chr(parsed_data, pluck,  1)
num_bags <- length(extract_bag_labels)
moves_mat <- matrix(
    rep(0, num_bags^2), 
    nrow = num_bags, 
    dimnames = list(extract_bag_labels, extract_bag_labels)
)

for (i in seq_along(parsed_data)) {
    row_label <- parsed_data[[i]][1]
    for(j in 2:length(parsed_data[[i]])){
        if (parsed_data[[i]][j] == "no other") {
            col_label <- row_label
            moves_mat[row_label, col_label] <- 1
            break()
        }
        vals <- str_split(parsed_data[[i]][j], "(?<=[0-9]) (?=[a-z])") %>%
            flatten_chr()
        col_label <- vals[2]
        moves_mat[row_label, col_label] <- as.numeric(vals[1])
    }
    row_sum <- sum(moves_mat[row_label, ])
    moves_mat[row_label, ] <- moves_mat[row_label, ] / row_sum 
}

num_starting_points <- 0
for (i in seq_len(num_bags)) {
    starting_point <- rep(0, num_bags)
    starting_point[i] <- 1
    usethis::ui_info("Testing {extract_bag_labels[i]}.")
    test_mat <- matrix(starting_point, nrow = 1)
    keep_going <- TRUE
    while (keep_going) {
        prior_mat <- test_mat
        test_mat <- test_mat %*% moves_mat
        if (test_mat[1, "shiny gold"] > 0) {
            num_starting_points <- num_starting_points + 1
            usethis::ui_info("Found a gold bag!!!")
            keep_going <- FALSE
        }
        if (sum(abs(test_mat - prior_mat)) < 0.0001) {
            keep_going <- FALSE
        }
    }
}

usethis::ui_done("I found {num_starting_points} places to start")
