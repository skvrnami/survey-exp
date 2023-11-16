library(cregg)

voters <- readRDS("data/voters.rds") 

head(voters)

# AMCE 
cj1 <- cj(voters, winner ~ gender + experience + spouse + 
            occupation + children + age, 
          id = ~responseid)

plot(cj1)

# AMCEs pro podskupiny
cj2 <- cj(voters, winner ~ gender + experience + spouse + occupation + children + age, 
          by = ~respondent_party_id, 
          id = ~responseid) 

plot(cj2, group = "respondent_party_id")

# DIY
legislators <- readRDS("data/legislators.rds")
