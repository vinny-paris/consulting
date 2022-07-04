library(emmeans)
library(multcomp)

#if you don't have this package run the code
# install.packages('readxl')
library(readxl)
library(stringr)


#packages for mixed models
library(lme4)
library(car)

#Read in first sheet entitiled "Measurements"
my_data <- read_xlsx('Greenhouse measurements.xlsx', sheet = 1)

#Removing all 0 observation????????
my_data <- my_data[-13,]

#For each character string in the week 6 ht data
#Split the character string along the double space
my_height <- lapply(my_data[,4], function(x) (str_split(x, '  ')))
my_height2 <-lapply(my_height, function(x) lapply(x, function(y) str_split(x[[y]], ',')))

#Make the character numeric
#change the data class from list to vector
#Take every other observation (listed average)
my_height2 <- (unlist(my_height)[(2*(1:35) - 1)])
my_height3 <- lapply(my_height2, function(x) str_split(x, ','))            
my_height4 <- as.numeric(unlist(my_height3))

#code in fert
fert <- c(rep('fe', 8), rep('blm', 7), rep('bom',8),
          rep('fm', 8), rep('control', 4))


#expanding to label each plant
fert_mixed <- lapply(fert,  function(x) rep(x, 4))
fert_mixed <- unlist(fert_mixed)

#coding in (categorical!!!) values for high (4) and low (2).
#Control was recorded as a 0
levs <- c(kronecker(rep(c(2,4), 4), c(1,1,1,1)), 0, 0, 0, 0)

#Removing the all 0 day
levs <- levs[-13]


#expand for the multiple plants per exp. unit
levs_mixed <- kronecker(levs, c(1,1,1,1))

#make sure levs is dealt with as a factor, not numeric
levs <- as.factor(levs)


#locations, similar to ht stuff
loc <- str_split(my_data$`Treatment/location...1`,
                 pattern = fixed('('))
loc2 <- unlist(loc)[2*(1:35)]
loc3 <- str_split(loc2, '0')
loc4 <- unlist(loc3)[(2*(1:35)) - 1]
loc5 <- kronecker(as.numeric(loc4), c(1,1,1,1))

#identify the exp. units
exp_unit <- kronecker(1:35, c(1,1,1,1))
exp_unit <- as.factor(exp_unit)

#collect the data into one data frame
data <- cbind(my_height4, fert_mixed, levs_mixed, loc5, exp_unit)
data <- as.data.frame(data)
#rename cols 
colnames(data)[1:4] <- c('ht', 'fert', 'levs', 'location')

data$ht <- as.numeric(data$ht)
head(data)



mixed_mod <- lmer(ht ~ location + fert*levs + (1|exp_unit),
                  data = data)

#these are some nice residuals
#I like these residuals
plot(mixed_mod)

#note this is a type 3 deviance table
#(which reads like a type 3 anova table)
#Please, please, please try to read about
#what type 1 vs 2 vs 3 anova's are; it's 
#important for this problem. 
Anova(mixed_mod, type = 3)


summary(mixed_mod)


#again, cell means is easier in this situation so
#following the same as from the previous ht example
my_trt2 <- unlist(lapply(my_trt, function(x) rep(x, 4)))
data$trt <- my_trt2

mixed_mod2 <- lmer(ht ~ location + trt + (1|exp_unit),
                  data = data)



#calculate the marginal means of the interaction 
#of fert and levs (the interaction is relabled trt)
my_means <- emmeans(mixed_mod2, ~trt)
my_means

#connected letters and pairwise comparisons
cld(my_means)
pairs(my_means)

#look at means
plot(my_means)

#general scatterings
ggplot(data, aes(fert, ht)) +
  geom_point(aes(col = levs))


#ht is the response
#Each boxplot is an experimental unit made
#of four plants
ggplot(data, aes(fert, ht)) +
  geom_boxplot(aes(col = levs, group = exp_unit)) +
  facet_grid(~fert)
