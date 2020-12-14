input_file <- "day10/input/real"

jolts <- as.integer(readLines(input_file))
sorted_jolts <- sort(jolts)

gaps <- c(diff(c(0, sorted_jolts)), 3)

hist_gaps <- table(gaps)
    
gaps_produc <- hist_gaps[1] * hist_gaps[2]