
input_file <- "day7/input/real"

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
            moves_mat[row_label, col_label] <- 0
            break()
        }
        vals <- str_split(parsed_data[[i]][j], "(?<=[0-9]) (?=[a-z])") %>%
            flatten_chr()
        col_label <- vals[2]
        moves_mat[row_label, col_label] <- as.numeric(vals[1])
    }
    #row_sum <- sum(moves_mat[row_label, ])
    #moves_mat[row_label, ] <- moves_mat[row_label, ] / row_sum 
}

gold_bag <- rep(0, num_bags)
names(gold_bag) <- extract_bag_labels
gold_bag["shiny gold"] <- 1
gold_bag <- t(as.matrix(gold_bag))

total_bags <- 0

keep_going <- TRUE
prior_bags <- gold_bag
while (keep_going) {
    all_bags <- prior_bags %*% moves_mat
    if (all(all_bags == prior_bags)) {
        keep_going <- FALSE
    } else {
        print(all_bags[1, all_bags > 0]) 
        total_bags <- total_bags + sum(all_bags)
        usethis::ui_info("Found {sum(all_bags)} more bags; total: {total_bags}")
        prior_bags <- all_bags
    }
}

usethis::ui_done("Total Number of bags: {total_bags}")
