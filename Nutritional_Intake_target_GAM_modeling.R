rm(list=ls())

library(tidyverse) 
library(mgcv)
library(MuMIn)
library(multcomp)

dat <- read.csv("") #Replace with wherever you put your data
names(dat)

#Check distribution of IT length in each level of Pop
dat %>%
  group_by(Pop) %>%
  count((IT.length))

#Creating new columns of adjust macronutrients consumed divided by length of IT

dat2 <- dat %>%
  mutate(adjusted.carbs = total.carb.consumed..g./IT.length,
         adjusted.protein = total.protein.consumed..g./IT.length)

#unadjusted raw data 

ggplot(dat2,aes(x=(total.protein.consumed..g.),y=(total.carb.consumed..g.),color=Pop)) + geom_point() +
  coord_equal(ratio=1) + xlim(0,.2) + ylim(0,.2)

#adjusted raw data

ggplot(dat2,aes(x=(adjusted.protein),y=(adjusted.carbs),color=Pop)) + geom_point() +
  coord_equal(ratio=1)  + xlim(0,.1) + ylim(0,.1)

#filtering out Lab ITs only keeping lab cycle 1

dat3 <- dat2 %>%
  filter(Pop != "Lab")

# non-linear relationship with initial GH mass
mod1 <- gam(list( 
  adjusted.carbs ~ Pop + diet.pair + sex + s(locust.initial.mass..g.),
  adjusted.protein ~ Pop + diet.pair + sex + s(locust.initial.mass..g.)),
  family=mvn(d=2),select=TRUE, data=dat2
)

summary(mod1) #checking results
plot(mod1, all.terms=TRUE) #plotting ALL graphs graphs with a '.1' signify protein consumption
k.check(mod1) #These p-values should NOT be significant

#linear relationship with initial GH mass
mod2 <- gam(list(
  adjusted.carbs ~ Pop + diet.pair + sex + (locust.initial.mass..g.),
  adjusted.protein ~ Pop + diet.pair + sex + (locust.initial.mass..g.) ),
  family=mvn(d=2),select=TRUE, data=dat2
)

summary(mod2)
plot(mod2, all.terms=TRUE)

#model without initial mass
mod3 <- gam(list(
  adjusted.carbs ~ Pop + diet.pair + sex,
  adjusted.protein ~ Pop + diet.pair + sex),
  family=mvn(d=2),select=TRUE, data=dat2
)

summary(mod3)
plot(mod3, all.terms=TRUE)

#NULL model (this should be the worst model)
mod4 <- gam(list(
  adjusted.carbs ~ 1,
  adjusted.protein ~ 1),
  family=mvn(d=2),select=TRUE, data=dat2
)

summary(mod4)
plot(mod4, all.terms=TRUE)

#Below are three ways to select the models (see here to understand the difference: https://stats.stackexchange.com/questions/577/is-there-any-reason-to-prefer-the-aic-or-bic-over-the-other)
BIC(mod1,mod2,mod3,mod4) 
AIC(mod1,mod2,mod3,mod4)
AICc(mod1,mod2,mod3,mod4) #this is AIC adjusted for small sample size.

#Multiple comparisons
## this gets a little hairy as we have to code a work around to get a GAM to work with this function

#this basically creates a matrix to show what comparisons to test
contr <- matrix(0, nrow = 3, ncol = length(coef(mod2))) #change 'mod2' to what ever model you want to look at
colnames(contr) <- names(coef(mod2)) #change 'mod2' to what ever model you want to look at
rownames(contr) <- c("1 - 2", "1 - lab.cycle.1", "2 - lab.cycle.1") #these are the comparisons we are trying to test
contr[, 2:3] <- rbind(c(1, 0), c(0, 1), c(-1, 1))

multicomp <- glht(mod2, linfct = contr) #This is the actual command that does the test. Again, change 'mod2' to what model you want
summary(multicomp)



