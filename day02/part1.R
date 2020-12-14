input_file <- "day2/input/day2_real"

passwd <-readLines(input_file)


library(stringr)
library(purrr)

policy_passwd <- str_split(passwd, pattern = ":")

policies <- map_chr(policy_passwd, pluck, 1)
passwd <- map_chr(policy_passwd, pluck, 2)

chr <- str_split(policies, pattern = " ")
    
rng <- map_chr(chr, pluck, 1) %>%
    str_split(pattern = "-") %>%
    map(as.integer)
    
ltr <- map_chr(chr, pluck, 2)
    
cnts <- str_count(passwd, pattern = ltr)

valid_pwds <- rep(0L, length(rng))
for( i in seq_along(rng)) {
    valid_pwds[i] <- as.integer(cnts[i] >= rng[[i]][1] && cnts[i] <= rng[[i]][2])
}

print(sum(valid_pwds))
        
    
