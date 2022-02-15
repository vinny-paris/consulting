


soil <- c(.5, .6, .7, .8, .9)
seed <- c(.0, .05)
reps <- 5
n <- 25
sim <- 10000
block_noise <- .02



#building the data frame
soil_rep <- rep(soil, length(seed))
seed_rep <- kronecker(seed, rep(1, length(soil)))
p <- soil_rep + seed_rep 



#loop
holding <- NULL
for(i in 1:reps){
  block_error <- kronecker(rnorm(length(seed), 0, block_noise), rep(1, length(soil)))
  holding[[i]] <- apply(as.data.frame(p) + block_error, 1, function(x) rbinom(sim, n, x))  
}

simls <-  t(do.call(cbind, holding))
simls_var <- cbind(simls, as.factor(1:length(p)))

simls_var2 <- cbind(simls_var, soil_rep, seed_rep)


soil_repped <- rep(soil_rep, reps)
seed_repped <- rep(seed_rep, reps)
rep_repped <- rep(1:reps, length(seed_rep)*length(soil_rep))

my_sim_ranges <- apply(simls, 2, function(x){
  mod <- lm(x ~ as.factor(soil_repped) + as.factor(seed_repped))
  mod_ests <- coef(mod)
  return(mod_ests)
})



my_ests <- t(my_sim_ranges)/n

colnames(my_ests) <- c('Intercept_.5', 'Soil_.1', 'Soil_.2',
                                      'Soil_.3', 'Soil_.4', 'Seed_.05')
obs <- melt(my_ests)
colnames(obs) <- c('sim_num', 'exp_var', 'obs_prop')

ggplot(obs, aes(obs_prop)) + 
  geom_histogram() + 
  facet_wrap(obs$exp_var) +
  ggtitle('Simulation Means: Linear Model', '25 Seeds, 5 Reps') +
  xlim(-.1, .8)
