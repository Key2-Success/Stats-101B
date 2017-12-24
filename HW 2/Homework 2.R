drug <- read.csv(file = "Kitu/College/Junior Year/Winter Quarter/Stats 101B/wk2-2.csv")
drug1 <- drug[, 1:2]
conduct <- drug[, 4:5]
cata <- drug[, 7:8]

# 2
fit <- aov(Observation~Level, data = drug1) # fit anova test
summary(fit) # shows anova test
plot(fit) # will show residual

# 3
fit2 <- aov(Conductivity~Coating, data = conduct) # fit anova test
summary(fit2)
plot(fit2)

# 4
fit3 <- aov(Concentration~Catalyst, data = cata) # fit anova test
summary(fit3)
plot(fit3)
