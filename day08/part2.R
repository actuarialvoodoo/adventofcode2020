library(stringr)
program_file <- "day8/input/real"

program <- readLines(program_file)

find_nops <- which(str_detect(program, "^nop"))

for (chg_ins in find_nops) {
    
    modified_program <- program
    modified_program[chg_ins] <- str_replace(modified_program[chg_ins], "nop", "jmp")
    accumulator <- 0
    
    lines <- rep(0, length(program))
    
    idx <- 1
    
    while (idx <= length(lines)) {
        lines[idx] <- lines[idx] + 1
        if (lines[idx] > 1) {
            break()
        }
        
        op <- str_extract(modified_program[idx], "^[a-z]{3}")
        val <- as.integer(str_extract(modified_program[idx], "-?[0-9]+$"))
        #usethis::ui_info("op: {op} val: {val}")
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
    
    if(all(lines <= 1)) {
        usethis::ui_done("Program successfully terminates")
        break()
    }
    
    if (max(lines) > 1) {
        usethis::ui_oops("Modified instruction {chg_ins} but still a inf loop")
    }
    
    
}


if (idx == length(lines) + 1) {
    usethis::ui_done("the accumlator value at end of program: {accumulator}")
} else {
    usethis::ui_oops("replacing nops with jmp is unsuccessful")
}

find_nops <- which(str_detect(program, "^jmp"))

for (chg_ins in find_nops) {
    
    modified_program <- program
    modified_program[chg_ins] <- str_replace(modified_program[chg_ins], "jmp", "nop")
    accumulator <- 0
    
    lines <- rep(0, length(program))
    
    idx <- 1
    
    while (idx <= length(lines)) {
        lines[idx] <- lines[idx] + 1
        if (lines[idx] > 1) {
            break()
        }
        
        op <- str_extract(modified_program[idx], "^[a-z]{3}")
        val <- as.integer(str_extract(modified_program[idx], "-?[0-9]+$"))
        #usethis::ui_info("op: {op} val: {val}")
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
    
    if(all(lines <= 1)) {
        usethis::ui_done("Program successfully terminates")
        break()
    }
    
    if (max(lines) > 1) {
        usethis::ui_oops("Modified instruction {chg_ins} but still a inf loop")
    }
    
    
}


if (idx == length(lines) + 1) {
    usethis::ui_done("the accumlator value at end of program: {accumulator}")
} else {
    usethis::ui_oops("replacing jmp with nop is unsuccessful")
}