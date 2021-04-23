library(stringr)
library(purrr)
library(bit64)
input_file <- "day13/input/real"

notes <- readLines(input_file)

start_time <- as.integer(notes[1])
bus_ids <- notes[2] %>% 
    str_replace_all(pattern = "x", replacement = "0") %>%
    str_split(pattern = ",") %>%
    flatten_chr() %>%
    as.integer64()

offsets <- as.integer64(seq_along(bus_ids))

idx <- bus_ids > as.integer64(0)
bus_ids <- bus_ids[idx]
offsets <- offsets[idx]
max_bus_idx <- which.max(bus_ids)
max_bus_id <- bus_ids[max_bus_idx]

offsets <- offsets - offsets[max_bus_idx]

keep_going <- TRUE
start_time <- as.integer64("100000000000000")

k <- start_time %/% max_bus_id 
while(keep_going) {
    targets <- k * max_bus_id + offsets
    
    if (k %% as.integer64(1000) == as.integer64(0)) {
        usethis::ui_info("Trying bus_id {max_bus_id} @ {k * max_bus_id}")
    }
    if (all(targets %% bus_ids == as.integer64(0))) {
        keep_going <- FALSE
        break()   
    }
    
    #if (k * max_bus_id > 755000) {
    #    usethis::ui_oops("We went too far")
    #    break()
    #}
    k <- k + 1
}

usethis::ui_done("golden start time: {k * max_bus_id + offsets[1]}")