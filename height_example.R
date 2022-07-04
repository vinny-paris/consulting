library(emmeans)
library(multcomp)

#if you don't have this package run the code
# install.packages('readxl')
library(readxl)
library(stringr)

#Read in first sheet entitiled "Measurements"
my_data <- read_xlsx('Greenhouse measurements.xlsx', sheet = 1)

#Removing all 0 observation????????
my_data <- my_data[-13,]

#For each character string in the week 6 ht data
#Split the character string along the double space
my_height <- lapply(my_data[,4], function(x) (str_split(x, '  ')))

#Make the character numeric
#change the data class from list to vector
#Take every other observation (listed average)
my_height2 <- (unlist(my_height)[(2*(1:35) - 1)])
my_height3 <- lapply(my_height2, function(x) str_split(x, ','))            
my_height4 <- as.numeric(unlist(my_height3))

my_ht_mean <- as.numeric(unlist(my_height))[(2*(1:35))]

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


exp_unit <- kronecker(1:35, c(1,1,1,1))
exp_unit <- as.factor(exp_unit)






















#############################################
#Linear Model Time!
#############################################


#fit a basic linear model
my_mod <- lm(my_ht_mean ~ fert*levs + loc4)

#change pane layout
par(mfrow = c(2,2))

#plot the residuals
plot(my_mod)
#We are looking for...
#1) constant variance
#2) a random normal scattering/no structure
#(we also need independence of the data but that's hard
#to do with just graphics)
#
#So for this plot I think we are fine. Obs.s 30, 14,
#and 29 are annoyingly high but not terrible.

#Let's see the anova table
anova(my_mod)
#NOTE: levs only has 1 df, not 2, because levs = 0
#is the exact same obs. as fert = control. It is
#unestimable so it doesn't require a df to estimate




##############################################
#Estimation Stuff
##############################################
#Easier to work with cell means model
#than the seperate factors imo so I'll
#use your trt encodings


#breaking apart your trt string
my_trt <- lapply(my_data[,1], function(x) (str_split(x, ' ')))

#unlist and choose every third object (fert:level)
my_trt <- (unlist(my_trt)[(3*(1:35)) - 1])


#fit the SAME model, just different parameterization
my_mod <- lm(my_height ~ my_trt)

#Note the error terms are identical between the anovas
#Again, it's the same model, just different parameters
anova(my_mod)

#let's fit and see some means
my_means <- emmeans(my_mod, ~my_trt)
my_means

#t-tests to see if the means are 0. Not sure if this is 
#interesting but there you go
contrast(my_means)


#connecting letter reports, similar to the paper you
#sent me
cld(my_means)





#############################################
#Graphics
#############################################

#read in and clean week 4 and 5 ht data
#code works the same as for the week 6 case
my_height_wk4 <- lapply(my_data[,2], function(x) (str_split(x, '  ')))
my_height_wk4 <- as.numeric(unlist(my_height_wk4)[2*(1:35)])
my_height_wk4 


my_height_wk5 <- lapply(my_data[,3], function(x) (str_split(x, '  ')))
my_height_wk5 <- as.numeric(unlist(my_height_wk5)[2*(1:35)])
my_height_wk5


#Collect locations
my_loc <- lapply(my_data[,1], function(x) (str_split(x, ' ')))
#unlist and choose every third object (field location)
my_loc <- (unlist(my_loc)[(3*(1:35))])
row.names(my_loc) <- NULL


#make a new data frame with the three heights over time
my_ht_over_time <- cbind(my_height_wk4, my_height_wk5, my_height)
colnames(my_ht_over_time) <- c('Week 4', 'Week 5', 'Week 6')

#Plot the three wweeks on the same graph
#Green triangles are levs = 4
#Red Circles are levs = 2
#Black Squares are control
pairs(my_ht_over_time, col = as.numeric(levs), pch = as.numeric(levs) + 14)


