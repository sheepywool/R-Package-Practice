set.seed(1)
x = rbinom(100, 1, 0.3)
y = rbinom(100, 1, 0.7)
binom_sample = data.frame(x,y)

usethis::use_data(binom_sample, compress = "gzip")
