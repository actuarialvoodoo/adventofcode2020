
library(dplyr)

input_file <- "day1/input/part1_real"

entries <- readLines(input_file) %>% as.integer() 

sorted_entries <- sort(entries)

assertthat::assert_that(max(sorted_entries) <= 2020 && min(sorted_entries) >= 0)

head_ptr <- 1
tail_ptr <- length(sorted_entries)

sol_found <- FALSE

while(head_ptr < tail_ptr && !sol_found) {
    
    test_sum <- sorted_entries[head_ptr] + sorted_entries[tail_ptr] 
    if (test_sum == 2020L) {
        sol_found <- TRUE
    }else if (test_sum < 2020L) {
        head_ptr <- head_ptr + 1
    }else {
        tail_ptr <- tail_ptr - 1
    }
}

if (!sol_found) {
    stop("No solution was found", call. = FALSE)
}


usethis::ui_info("Solution Found: {sorted_entries[head_ptr]} + {sorted_entries[tail_ptr]} = {test_sum}")
sol <- sorted_entries[head_ptr] * sorted_entries[tail_ptr]
usethis::ui_done("Ans: {sorted_entries[head_ptr]} * {sorted_entries[tail_ptr]} = {sol}")