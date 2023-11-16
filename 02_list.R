library(list)
library(ggplot2)

data("affirm")

head(affirm)

table(affirm$y, affirm$treat)

affirm %>% 
  count(treat, y) %>% 
  group_by(treat) %>% 
  mutate(share = n / sum(n)) %>% 
  ggplot(., aes(x = y, y = share, fill = factor(treat))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal()

affirm %>% 
  group_by(treat) %>% 
  summarise(mean_items = mean(y))

t2 <- t.test(y ~ treat, data = affirm)
unname(t2$estimate[2]) - unname(t2$estimate[1])

mod0_lm <- ictreg(y ~ 1, data = affirm, treat = "treat", J = 3, method = "lm")
summary(mod0_lm)

# Test design efektů
ict.test(affirm$y, affirm$treat, J = 3)

# Odhad rozdílu (ML estimator)
mod0 <- ictreg(y ~ 1, data = affirm, treat = "treat", J = 3, method = "ml")
summary(mod0)

# Regrese
mod1 <- ictreg(y ~ south + age + male + college, data = affirm, 
               treat = "treat", J=3, method = "ml")

summary(mod1)

# Regrese s odhadem floor a ceiling efektů
mod2 <- ictreg(y ~ age + college + male + south, treat = "treat", 
               J = 3, data = affirm, method = "ml", 
               floor = TRUE,
               floor.fit = "bayesglm",
               floor.formula = ~ age + college + male + south,
               ceiling = TRUE,
               ceiling.fit = "bayesglm",
               ceiling.formula = ~ age + college + male + south)

summary(mod2, boundary.proportions = T)
