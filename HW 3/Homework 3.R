sales1 <- read.csv(file = "Kitu/College/Junior Year/Winter Quarter/Stats 101B/wk3.csv")
sales <- sales1[, 1:3]
batch <- sales1[, 5:8]
chem <- sales1[1:20, 10:12]


# 4.3 - anova
fit4 <- aov(Strength~factor(Chemical) + factor(Bolt), data = chem)
summary(fit4)
plot(fit4)

# 4.8a - anova
fit <- aov(Responses~factor(Region)+factor(Design), data = sales)
summary(fit)

sales$Region <- as.factor(sales$Region)

# 4.8b - tukey method
tukey <- TukeyHSD(fit)
tukey

# 4.8c - plot residuals
plot(fit)

# 4.22 - anova
fit2 <- aov(Time~factor(Treatment)+factor(Batch)+factor(Day), data = batch)
summary(fit2)
plot(fit2) # residuals

# 4.35 - anova

yield = c(26, 16, 19, 16, 13, 18, 21, 18, 11, 21, 20, 12, 16, 25, 13, 15, 15, 22, 14, 17, 10, 24, 17, 17, 14)
acid = c(rep(c(1, 2, 3, 4, 5), times = 5))
batch = c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5))
conc = c("alpha", "beta", "gamma", "delta", "epsilon", 
              "gamma", "delta", "epsilon", "alpha", "beta",
              "epsilon", "alpha", "beta", "gamma", "delta", 
              "beta", "gamma", "delta", "epsilon", "alpha",
              "delta", "epsilon", "alpha", "beta", "gamma")
method = c("A", "B", "C", "D", "E",
           "B", "C", "D", "E", "A", 
           "C", "D", "E", "A", "B",
           "D", "E", "A", "B", "C",
           "E", "A", "B", "C", "D")

assemblyDF = data.frame(acid, batch, method,
                        conc, yield)

a2.aov = aov(yield ~ factor(method) + factor(conc) +  factor(acid) + factor(batch), 
             data = assemblyDF)
summary(a2.aov) # anova table
qf(0.95, df1 = 4-1, df2 = (4-2)*(4-1))

plot(a2.aov) # residuals


