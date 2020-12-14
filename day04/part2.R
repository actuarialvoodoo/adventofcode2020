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
library(tibble)
library(dplyr)

raw_input <- readLines(input_file)

record_idx <- 1 + cumsum(raw_input == "")

records <- map_chr(
    seq_len(max(record_idx)), 
    ~ trimws(paste0(raw_input[record_idx == .x], collapse = " "))
)

record_fields <- str_extract_all(records, pattern = "[a-z]{3}(?=:)")
record_values <- str_extract_all(records, pattern = "(?<=:)[^ ]+")

#record_tbl <- map(seq_along(record_values), ~ set_names(record_values[[.x]], record_fields[[.x]]))
record_tbl <- map_dfr(seq_along(record_values),
                      ~ tibble::as_tibble(
                          set_names(
                              as.list(record_values[[.x]]), 
                              record_fields[[.x]]
                              )
                          )
                      )

validate_byr <- function(x) {
    str_detect(x, "^(19[2-9][0-9]|200[0-2])$")
}
validate_iyr <- function(x) {
    str_detect(x, "^(201[0-9]|2020)$")
}
validate_eyr <- function(x) {
    str_detect(x, "^(202[0-9]|2030)$")
}
validate_hgt <- function(x) {
    str_detect(x, "^(1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in)$")
}
validate_hcl <- function(x) {
    str_detect(x, "^#[0-9a-f]{6}$")
}
validate_ecl <- function(x) {
    str_detect(x, "^(amb|blu|brn|gry|grn|hzl|oth)$")
}
validate_pid <- function(x) {
    str_detect(x, "^[0-9]{9}$")
}
validate_passport <- function(x) {
    check <- req_fields %in% x
    all(check[-length(check)])
}
validate_cid <- function(x) {
    TRUE
}

valid_passport_info <- record_tbl %>%
    filter(validate_byr(byr), 
           validate_iyr(iyr),
           validate_eyr(eyr),
           validate_hgt(hgt),
           validate_hcl(hcl),
           validate_ecl(ecl),
           validate_pid(pid)
           )

num_valid_passports <- nrow(valid_passport_info)
usethis::ui_done("Valid Passports: {num_valid_passports}")