library(stringr)
library(purrr)
input_file <- "day13/input/real"

notes <- readLines(input_file)

start_time <- as.integer(notes[1])
bus_ids <- notes[2] %>% 
    str_remove_all(pattern = "x,?") %>%
    str_split(pattern = ",") %>%
    flatten_chr() %>%
    as.integer()

smallest_multiple_xs <- ceiling(start_time / bus_ids)

first_bus_time <- smallest_multiple_xs * bus_ids

first_bus_idx <- which.min(first_bus_time)

bus_time <- first_bus_time[first_bus_idx]

first_bus <- bus_ids[first_bus_idx]

wait_time <- bus_time - start_time

usethis::ui_done("Take bus {first_bus} @ {bus_time}. This is a wait time of {wait_time}. Ans: {first_bus * wait_time}")

