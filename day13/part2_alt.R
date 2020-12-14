library(stringr)
library(purrr)
library(bit64)
library(lpSolve)
input_file <- "day13/input/test"

notes <- readLines(input_file)

start_time <- as.integer(notes[1])
bus_ids <- notes[2] %>% 
    str_replace_all(pattern = "x", replacement = "0") %>%
    str_split(pattern = ",") %>%
    flatten_chr() %>%
    as.integer()

offsets <- seq_along(bus_ids)

idx <- bus_ids > 0L
bus_ids <- bus_ids[idx]
offsets <- offsets[idx]
max_bus_idx <- which.max(bus_ids)
max_bus_id <- bus_ids[max_bus_idx]

offsets <- offsets - offsets[max_bus_idx]

keep_going <- TRUE

#f.obj <- ifelse(sign(offsets) == 0, 1, -sign(offsets)) * bus_ids
f.obj <- bus_ids

A <- diag(-bus_ids)

const.mat <- A[-max_bus_idx, ] - matrix(rep(A[max_bus_idx, ], length(bus_ids) - 1), nrow = length(bus_ids) - 1, byrow = TRUE)

const.dir <- rep("==", nrow(const.mat))

const.rhs <- offsets[-max_bus_idx]

res <- lp(
    objective.in = f.obj, 
    const.mat = const.mat, 
    const.dir = const.dir, 
    const.rhs = const.rhs, 
    all.int = TRUE,
    scale = 0
    )
