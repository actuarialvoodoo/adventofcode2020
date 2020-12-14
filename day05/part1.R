
library(stringr)
library(purrr)
library(dplyr)

decode_pattern <- function(pattern, code) {
    
    value <- 0
    pattern_length <- nchar(pattern) 
    for (i in seq_len(pattern_length)) {
        if (!str_sub(pattern, i, i) %in% code) {
            usethis::ui_oops("Invalid pattern {pattern}. ", 
            "Valid codes are ({paste0(code, collapse = ', ')})")
            stop(call. = FALSE)
        }
        
        if (str_sub(pattern, i, i) == code[2]) {
            value <- value + 2^(pattern_length - i)
        }
        
    }
    value
}

test_data <- tibble::tribble(
    ~seat_code, ~seat_row, ~seat_col, ~seat_id,
    "FBFBBFFRLR", 44, 5, 357,
    "BFFFBBFRRR", 70, 7, 567,
    "FFFBBBFRRR", 14, 7, 119,
    "BBFFBBFRLL",102, 4, 820
)

decode_seat_code <- function(x) {
    row <- decode_pattern(str_sub(x, 1, 7), code = c("F", "B"))
    col <- decode_pattern(str_sub(x, 8, 10), code = c("L", "R"))
    
    list(seat_row = row, seat_col = col)    
}

check_test_data <- purrr::map_dfr(
    test_data$seat_code, 
    ~ tibble::as_tibble(decode_seat_code(.x))) %>% 
    dplyr::mutate(seat_id = seat_row * 8 + seat_col)

if(assertthat::are_equal(test_data[, 2:4], check_test_data)) {
    usethis::ui_done("All test checks have passed")
}

seat_codes <- readLines("day5/input/real")

seat_data <- map_dfr(seat_codes, ~ tibble::as_tibble(decode_seat_code(.x))) %>%
    mutate(seat_id = seat_row * 8 + seat_col)

max_seat_id <- max(seat_data$seat_id)

usethis::ui_done("Max seat id is {max_seat_id}")

# Let's find the missing seat



gap_in_seats <- seat_data %>% 
    arrange(seat_id) %>% 
    mutate(diff = c(diff(seat_id), NA)) %>% 
    filter(diff > 1)


if (nrow(gap_in_seats) == 0) {
    usethis::ui_oops("No gap found")
    stop(call. = FALSE)
}

if (gap_in_seats$seat_col == 7) {
    your_seat_row <- gap_in_seats$seat_row + 1
    your_seat_col <- 0
} else {
    your_seat_row <- gap_in_seats$seat_row
    your_seat_col <- gap_in_seats$seat_col + 1
}

your_seat_id <- your_seat_row * 8 + your_seat_col
usethis::ui_done("Your seat is row {your_seat_row}, column {your_seat_col}; seat_id: {your_seat_id}")





