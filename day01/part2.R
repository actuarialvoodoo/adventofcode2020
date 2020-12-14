
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
    usethis::ui_info("testsum: {sorted_entries[head_ptr]} + {sorted_entries[tail_ptr]} = {test_sum}")
    if (test_sum >= 2020L) {
        tail_ptr <- tail_ptr - 1
        usethis::ui_info("greater than 2020 reducing head")
        next()
    }
    
    candidates <- sorted_entries[(head_ptr + 1):(tail_ptr - 1)]
    
    test_sum3 <- candidates + test_sum
    if (all(test_sum3 > 2020L)) {
        tail_ptr <- tail_ptr - 1
        next()
    }
    pos <- which(test_sum3 == 2020L)
    
    if (length(pos) == 1) {
        sol_found <- TRUE
        break()
    }
    
    head_ptr <- head_ptr + 1
}

if (!sol_found) {
    stop("No solution was found", call. = FALSE)
}

third_val <- sorted_entries[head_ptr + pos]
usethis::ui_info("Solution Found: {sorted_entries[head_ptr]} + {sorted_entries[tail_ptr]} + {third_val} = {test_sum + third_val}")
sol <- sorted_entries[head_ptr] * sorted_entries[tail_ptr] * third_val
usethis::ui_done("Ans: {sorted_entries[head_ptr]} * {sorted_entries[tail_ptr]} * {third_val}  = {sol}")