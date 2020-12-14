input_file <- "day4/input/real"

req_fields <- c(
    "byr", # (Birth Year)
    "iyr", # (Issue Year)
    "eyr", # (Expiration Year)
    "hgt", # (Height)
    "hcl", # (Hair Color)
    "ecl", # (Eye Color)
    "pid", # (Passport ID)
    "cid"  # (Country ID)
) 


library(purrr)
library(stringr)
raw_input <- readLines(input_file)

record_idx <- 1 + cumsum(raw_input == "")

records <- map_chr(
    seq_len(max(record_idx)), 
    ~ trimws(paste0(raw_input[record_idx == .x], collapse = " "))
)

record_fields <- str_extract_all(records, pattern = "[a-z]{3}(?=:)")

validate_passport <- function(x) {
    check <- req_fields %in% x
    all(check[-length(check)])
}

num_valid_passports <- sum(map_lgl(record_fields, validate_passport))

usethis::ui_done("Valid Passports: {num_valid_passports}")