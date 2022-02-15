#setting parameters

p <- c(.5, .55, .65, .7, .75, .8, .85, .9, .95) 
reps <- 4
n <- 25
sim <- 10000

#loop
holding <- NULL
for(i in 1:reps){
  block_error <- rnorm(1, 0, .02)
  holding[[i]] <- apply(as.data.frame(p + block_error), 1, function(x) rbinom(sim, n, x))  
}

simls <-  t(do.call(cbind, holding))
simls_var <- cbind(simls, as.factor(1:length(p)))


my_ests <- cbind(aggregate(simls_var[,1:sim], list(simls_var[,(sim + 1)]), mean)/(n), 1:length(p)) 
colnames(my_ests)[c(1, (sim+2))] <- c('p', 'trts')
my_ests$p <- p
obs <- melt(my_ests, id.vars = c(1, (sim+2)))
colnames(obs)[3:4] <- c('sim_num', 'obs_prop')


ggplot(obs, aes(obs_prop)) + 
  geom_histogram() + 
  facet_wrap(p) +
  ggtitle('Simulation Means: Cell Means Model', '25 Seeds, 4 Reps')+
  xlim(.35, 1.1)

