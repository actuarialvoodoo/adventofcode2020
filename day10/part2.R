input_file <- "day10/input/real"

jolts <- as.integer(readLines(input_file))

sorted_jolts <- c(0L, sort(jolts))

diff_tbl <- tibble::tibble(
    diff = diff(sorted_jolts), 
    grp = cumsum(diff == 3L)
    ) %>%
    dplyr::filter(diff == 1L) %>% 
    dplyr::count(grp) %>%
    dplyr::mutate(paths = dplyr::case_when(
        n == 4L ~ 7L,
        n == 3L ~ 4L,
        n == 2L ~ 2L,
        n == 1L ~ 1L
    ))

num_paths <- prod(diff_tbl$paths)

usethis::ui_done("The number of arrangments is: {num_paths}")



