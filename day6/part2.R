input_file <- "day6/input/real"

library(purrr)
library(stringr)
raw_data <- readLines(input_file)

groups <- cumsum(raw_data == "") + 1
group_size <- table(groups)
group_size[-1] <- group_size[-1] -1

group_responses <- map_chr(seq_len(max(groups)), ~ trimws(paste0(raw_data[groups == .x], collapse = "")))



num_responses <- group_responses %>% 
    str_extract_all(pattern = "[a-z]") %>%
    map( ~ table(.x))

ans <- map2_int(num_responses, group_size, ~ sum(.x == .y)) %>%
    sum()

usethis::ui_done("The answer is {ans}")
