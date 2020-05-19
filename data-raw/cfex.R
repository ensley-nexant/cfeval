library(dplyr)

n <- 2000; p <- 10; set.seed(123)

cfex <- matrix(rnorm(n * p), n, p) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(fct = sample(letters[1:3], n, replace = T),
         W = rbinom(n, 1, 0.4 + 0.2 * (V1 > 0)),
         Y = pmax(V1, 0) * W + V2 + pmin(V3, 0) + rnorm(n))


write.csv(cfex, file = 'data-raw/cfex.csv', row.names = FALSE)
usethis::use_data(cfex, overwrite = TRUE)
