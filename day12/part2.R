library(stringr)
input_file <- "day12/input/real"

nav_steps <- readLines(input_file)

# cardinal direction is encoded as unit vector, 
# E: (-1,  0)
# W: ( 1,  0)
# N: ( 0,  1)
# S: ( 0, -1) 
direction <- matrix(c(-1, 0), nrow = 1)
position <- matrix(c(0, 0), nrow = 1)
way_point <- matrix(c(-10, 1), nrow = 1)

make_rotation_matrix <- function(action, value) {
    
    rads <- pi/180 * value * ifelse(action == "L", 1, -1)
    
    round(matrix(c(cos(rads), -sin(rads), sin(rads), cos(rads)), nrow = 2, byrow = TRUE))
}

for (step in nav_steps) {
    action <- str_extract(step, "^[A-Z]")
    value <- as.integer(str_extract(step, "-?[0-9]+$"))
    
    if (action == "N") {
        way_point <- way_point + c(0, value)
    }else if(action == "S") {
        way_point <- way_point + c(0, -value)
    }else if(action == "E") {
        way_point <- way_point + c(-value, 0)
    }else if(action == "W") {
        way_point <- way_point + c(value, 0)
    }else if(action %in% c("L", "R")) {
        way_point <- way_point %*% make_rotation_matrix(action, value)
    
    }else if(action == "F") {
        position <- position + value * way_point
    }else{
        stop(c("invalid action: ", action), call. = FALSE)
        
    }
    usethis::ui_info("Current position is ({position[1]}, {position[2]}).")
}
mandist <- abs(position[1]) + abs(position[2])
usethis::ui_done("Final position was ({position[1]}, {position[2]}): dist {mandist}")