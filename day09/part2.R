library(purrr)
input_file <- "day9/input/real"

raw_data <- as.numeric(readLines(input_file))

preamble <- 25

invalid_idx <- 634

invalid_value <- raw_data[invalid_idx]
invalid_inputs <- raw_data[invalid_idx - seq_len(preamble)]

# are there values greater than invalid_value

too_big <- raw_data >= invalid_value

possible_contiguous_runs <- which(!too_big)

contig_diffs <- diff(possible_contiguous_runs)

contig_starts <- c(1, seq_along(possible_contiguous_runs)[contig_diffs > 1] + 1)
contig_ends <- c(seq_along(possible_contiguous_runs)[contig_diffs > 1], length(possible_contiguous_runs))

check_sum <- function(target, inputs) {
    
    stop_idx <- NULL
    num_inputs <- length(inputs)
    for (start_idx in seq_len(num_inputs - 1)) {
        usethis::ui_info("Checking {start_idx} to {num_inputs} for {target}")
        tmp <- cumsum(inputs[start_idx:num_inputs])
        #print(tmp)
        if(any(tmp == target)) {
            stop_idx <- which(tmp == target)
            usethis::ui_info("Potential Solution Found:")
            if (sum(inputs[start_idx + (seq_len(stop_idx) - 1)]) != target) {
                stop("Somthing went wrong")
            }
            break()
        }
    }
    
    if (is.null(stop_idx)) {
        return(NULL)
    }
    
    return(inputs[start_idx + (seq_len(stop_idx) - 1)])
    
}

ans <- map2(contig_starts, contig_ends, ~ check_sum(invalid_value, raw_data[.x:.y]))

is_sol <- !map_lgl(ans, is.null)
if(any(is_sol)) {
    min_val <- min(ans[is_sol][[1]])
    max_val <- max(ans[is_sol][[1]])
    usethis::ui_done("min + max = {min_val} + {max_val} = {min_val + max_val}")
}