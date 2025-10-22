#Example 1: 2 by 2 Qualitative factorial example
# Study on weight gain of rats fed four different diets, 
# where there were two levels of protein (high or low) and 
# two sources of protein (beef or cereal).  
# This gives 2 x 2 treatment combinations: 
# high/beef (HB), high/cereal (HC), low/beef (LB), low/cereal (LC). 
# Ten rats were in each of the four treatment groups.  


library(emmeans)

#Creating the data
source <- rep(c("beef", "beef", "cereal", "cereal"), each = 10)
amount <- rep(c("low", "high", "low", "high"), each = 10)
rat <- rep(1:10, times = 4)
weight <- c(
  90, 76, 90, 64, 86, 51, 72, 90, 95, 78,             # beef low
  73, 102, 118, 104, 81, 107, 100, 87, 117, 111,       # beef high
  107, 95, 97, 80, 98, 74, 74, 67, 89, 58,             # cereal low
  98, 74, 56, 111, 95, 88, 82, 77, 86, 92              # cereal high
)


# Combine into a data frame
rats <- data.frame(source, amount, rat, weight)
str(rats)

#Convert to factor variables
rats$source <- as.factor(rats$source)
rats$amount <- as.factor(rats$amount)
str(rats)
head(rats)

#Fit the model:
mod.fit<- lm(weight~ source + amount + source:amount, data=rats)
summary(aov(mod.fit)) #Type 3 Analysis of variance 

#Interaction plot:
interaction.plot(rats$amount,rats$source, rats$weight
                 , xlab = "Amount", ylab= "Weight",
                 trace.label = "Source", main="Interaction Plot")


#LSMEANS
library(emmeans)
emmeans(mod.fit, ~source, data=rats)
emmeans(mod.fit, ~amount, data=rats)
emmeans(mod.fit, ~source*amount, data=rats)

#Pairwise comparisons
emmeans(mod.fit, pairwise~ source)
emmeans(mod.fit, pairwise ~amount)
emmeans(mod.fit, pairwise ~source*amount, adjust="none") #adjust=TUKEY by default

emmeans(mod.fit, pairwise ~source*amount) 

#Confidence interval for differences
result <- emmeans(mod.fit, pairwise ~ source * amount, adjust = "none")
summary(result$contrasts, infer = c(TRUE, TRUE), level=0.95)  # infer = c(confidence, test)


