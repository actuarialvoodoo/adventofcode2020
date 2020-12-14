
input_file <- "day9/input/real"

raw_data <- as.numeric(readLines(input_file))

preamble <- 25

check_value <- function(x, inputs) {
    
    sorted_entries <- sort(inputs)
    head_ptr <- 1
    tail_ptr <- length(sorted_entries)
    
    sol_found <- FALSE
    
    while(head_ptr < tail_ptr && !sol_found) {
        
        test_sum <- sorted_entries[head_ptr] + sorted_entries[tail_ptr] 
        if (test_sum == x) {
            sol_found <- TRUE
        }else if (test_sum < x) {
            head_ptr <- head_ptr + 1
        }else {
            tail_ptr <- tail_ptr - 1
        }
    }
    sol_found
}

for (i in seq_along(raw_data)[-seq_len(preamble)]) {
    if (!check_value(raw_data[i], raw_data[i - seq_len(preamble)])) {
        usethis::ui_done("raw_data[{i}]: {raw_data[i]} is NOT valid")
        break()
    }
    usethis::ui_info("raw_data[{i}]: {raw_data[i]} is valid")
    
}

