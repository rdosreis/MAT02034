model {
  for (j in 1:J) {
    y[j] ~ dnorm (theta[j], tau.y[j]) 
    theta[j] ~ dnorm (mu.theta, tau.theta)
    tau.y[j] <- pow(sigma.y[j], -2)
  } 
  mu.theta ~ dnorm (0.0, 1.0E-6)
  tau.theta <- pow(sigma.theta, -2)
  sigma.theta ~ dunif (0, 1000)
}


J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list("J", "y", "sigma.y")

inits <- function(){
  list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100),
       sigma.theta = runif(1, 0, 100))
}

schools.sim <- bugs(data, inits,
                    model.file = here::here("material_de_aula", "OpenBugsExemplos", "schools.txt"),
                    parameters = c("theta", "mu.theta", "sigma.theta"),
                    n.chains = 3, n.iter = 1000, codaPkg = TRUE)

print(schools.sim)

library(coda)
codaobject <- read.bugs(schools.sim)
plot(codaobject)



model
{
  for (i in 1:N) {
    mu[i] <- beta0 + beta1 * xcent[i]
    y[i] ~ dnorm( mu[i], tausq )
  }
  beta0 ~ dflat()
  beta1 ~ dflat()
  tausq ~ dgamma( 0.001, 0.001)
  sigma <- 1/sqrt(tausq) # regression standard deviation
}

N <- nrow(mtcars)
y <- mtcars$mpg
xcent <- mtcars$hp - mean(mtcars$hp)

data <- list("N", "y", "xcent")

inits <- function(){
  list(beta0 = rnorm(1, 0, 100), beta1 = rnorm(1, 0, 100),
       tausq = 5)
}

rlb.sim <- bugs(data, inits,
                    model.file = here::here("material_de_aula", "OpenBugsExemplos", "rlb.txt"),
                    parameters = c("beta0", "beta1", "sigma"),
                    n.chains = 3, n.iter = 10000)

print(rlb.sim)
# library(coda)
codaobject <- read.bugs(rlb.sim)
plot(codaobject)



library(dplyr)

sim.resumo <- rlb.sim$sims.matrix %>% 
  as.data.frame() %>% 
  select(beta0, beta1) %>% 
  summarise_all(list(mean, quantile), probs = c(.025, .975))

plot(mtcars$hp - mean(mtcars$hp), mtcars$mpg, pch = 16, col = "steelblue",
     xlab = "Potência do motor (centralizada)",
     ylab = "Consumo de combustível (MPG)")

xcent <- mtcars$hp - mean(mtcars$hp)
xcent <- c(min(xcent) - 10, sort(xcent), max(xcent) + 10)

y1 <- sim.resumo$beta0_fn2[1] +  sim.resumo$beta1_fn2[1] * xcent
y2 <- sim.resumo$beta0_fn2[2] +  sim.resumo$beta1_fn2[2] * xcent

# Fill area between lines
polygon(c(xcent, rev(xcent)), c(y2, rev(y1)),
        col = rgb(red = 255, green = 160, blue = 122, alpha = round(0.3*255), max = 255), border =  F)

abline(a = sim.resumo$beta0_fn1[1],
       b = sim.resumo$beta1_fn1[1],
       col = "red", lwd = 2)

