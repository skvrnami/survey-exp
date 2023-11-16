library(dplyr)

N <- 200
DIFF <- 1

set.seed(1234)

# Vygeneruj kompletně fake data
data <- tibble(
  # Exp. skupina
  skupina = rep(c(0, 1), times = N), 
  # Hodnocení kandidáta
  eval = rnorm(N * 2, 0, 2), 
  gender = sample(c("man", "woman"), size = N * 2, 
               replace = TRUE), 
  age = floor(runif(N * 2, 18, 80))
) %>% 
  mutate(
    eval = if_else(skupina == 1, eval + DIFF, eval) %>% 
      round(., 0)
  )

head(data)

# Summary stats
data %>% 
  group_by(skupina) %>% 
  summarise(across(where(is.numeric), ~mean(.x)), 
            gender_men = mean(gender == "man"))

# T-test
(t1 <- t.test(eval ~ skupina, data = data))

# Regrese
lm(eval ~ skupina + age + gender, data = data) %>% 
  summary()
