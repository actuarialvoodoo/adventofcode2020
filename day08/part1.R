library(stringr)
program_file <- "day8/input/real"

program <- readLines(program_file)

accumulator <- 0

lines <- rep(0, length(program))

idx <- 1

while (idx <= length(lines)) {
    lines[idx] <- lines[idx] + 1
    if (lines[idx] > 1) {
        break()
    }
    
    op <- str_extract(program[idx], "^[a-z]{3}")
    val <- as.integer(str_extract(program[idx], "-?[0-9]+$"))
    usethis::ui_info("op: {op} val: {val}")
    if (op == "acc") {
        
        accumulator <- accumulator + val
        idx <- idx + 1
    }else if(op == "jmp") {
        idx <- idx + val
    }else if(op == "nop") {
        idx <- idx + 1
    }else{
        stop(paste0("invalid operation ", op, " found."), call. = FALSE)
    }
}

usethis::ui_done("the accumlator value before entering infinite loop: {accumulator}")