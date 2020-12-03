input_file <- "day2/input/day2_real"

passwd <-readLines(input_file)


library(stringr)
library(purrr)

policy_passwd <- str_split(passwd, pattern = ": ")

policies <- map_chr(policy_passwd, pluck, 1)
passwd <- map_chr(policy_passwd, pluck, 2)

chr <- str_split(policies, pattern = " ")
    
rng <- map_chr(chr, pluck, 1) %>%
    str_split(pattern = "-") %>%
    map(as.integer)
    
ltr <- map_chr(chr, pluck, 2)
    

valid_pwds <- rep(0L, length(rng))
for( i in seq_along(rng)) {
    pos_1 <- str_sub(passwd[i], start = rng[[i]][1], end = rng[[i]][1])
    pos_2 <- str_sub(passwd[i], start = rng[[i]][2], end = rng[[i]][2])
    cond1 <- pos_1 == ltr[i] 
    cond2 <- pos_2 == ltr[i]
    
    valid_pwds[i] <- as.integer(xor(cond1, cond2))
    usethis::ui_info("checking {passwd[i]} against {policies[i]}({pos_1}-{pos_2}): {cond1} {cond2}")
    
}

print(sum(valid_pwds))
        
    
