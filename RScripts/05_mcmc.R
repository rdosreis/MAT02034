## ----metropolis-hastings, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-----------------------

# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 1000
theta <- vector(length = n)



## ----metropolis-hastings1, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------------

# Alogritmo M-H

theta[1] <- 0.3 # theta_0 = 0.3

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta[t])),
             1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}



## ----metropolis-hastings2, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "Trajetória da cadeia (1.000 passos)")



## ----metropolis-hastings3, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

theta2 <- vector(length = n)

theta2[1] <- 0.7 # theta_0 = 0.7

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta2[t])),
             1)
  
  theta2[(t+1)] <- sample(c(theta_cand, theta2[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "Duas trajetórias da cadeia (1.000 passos)",
     ylim = c(0,0.1))
lines(theta2, col = "red")



## ----metropolis-hastings4, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

n <- 20000
burnin <- 10000
theta3 <- vector(length = n)

theta3[1] <- 0.5 # theta_0 = 0.5

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta3[t])),
             1)
  
  theta3[(t+1)] <- sample(c(theta_cand, theta3[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

plot(theta3[(burnin+1):n], type = "l",
     col = "blue",
     ylab = expression(theta), xlab = "t",
     main = "Trajetória da cadeia (descartando 10.000 primeiras observações)",
     ylim = c(0,0.1))



## ----metropolis-hastings5, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="50%", fig.align='center'----

acf(theta3[(burnin+1):n])



## ----metropolis-hastings6, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

# Configuração

n <- 10000
burnin <- 10000
k <- 10

theta <- vector(length = n)

# Período de burnin

theta_b <- 0.9

for (t in 1:(burnin-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta_b)),
             1)
  
  theta_b <- sample(c(theta_cand, theta_b),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

theta_n <- theta_b

for (t in 1:( (k*n)) ){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta_n)),
             1)
  
  theta_n <- sample(c(theta_cand, theta_n),
                         size = 1,
                         prob = c(rho, 1 - rho))
  if (t %% k == 0) {
    theta[(t/k)] <- theta_n
  }
}

plot(theta, type = "l",
     col = "blue",
     ylab = expression(theta), xlab = "t",
     main = "Trajetória da cadeia (descartando 10.000 primeiras observações, saltando de 10 em 10)",
     ylim = c(0,0.1))



## ----metropolis-hastings7, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

acf(theta)



## ----metropolis-hastings8, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

hist(theta, breaks = 40,
     probability = TRUE, border = "white",
     xlab = expression(theta), ylab = "Densidade",
     main = "Amostra da dist. a posteriori")



## ----metropolis-hastings9, eval=TRUE, echo=FALSE, results='hide', warning=FALSE, message=FALSE-----

mean(theta)
median(theta)

mean(theta > 0.05)

quantile(theta, c(0.025, 0.975))

library(HDInterval)
hdi(theta, credMass = 0.95)



## ----gibbs, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-------------------------------------

# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 1000
theta <- vector(length = n)
Y <- vector(length = n)



## ----gibbs1, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE------------------------------------

# Amostrador de Gibbs

theta[1] <- 0.3 # theta_0 = 0.3

for (t in 1:(n-1)){
  
  Y[t] <- x + rpois(n = 1,
                    lambda = lambda * (1 - theta[t]))
  
  theta[(t+1)] <- rbeta(n = 1,
                        shape1 = alpha + x,
                        shape2 = gamma + Y[t] - x)
  
}

Y[n] <- x + rpois(n = 1,
                    lambda = lambda * (1 - theta[n]))



## ----gibbs2, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

plot(Y[1:5], theta[1:5], type = "l",
     ylab = expression(theta), xlab = "Y",
     main = "Trajetória da cadeia (5 primeiros passos)")



## ----gibbs3, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

par(mfrow = c(1,2))

plot(Y[1:20], theta[1:20], type = "l",
     ylab = expression(theta), xlab = "Y",
     ylim = c(0, 0.06), xlim = c(80, 120),
     main = "Trajetória da cadeia (20 primeiros passos)")

plot(Y, theta, type = "l",
     ylab = expression(theta), xlab = "Y",
     ylim = c(0, 0.08), xlim = c(70, 130),
     main = "Trajetória da cadeia (1.000 passos)")



## ----gibbs4, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

par(mfrow = c(2,1))

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     ylim = c(0, 0.10),
     main = "Trajetória da cadeia (1.000 passos)")

plot(Y, type = "l",
     ylab = "Y", xlab = "t",
     main = "Trajetória da cadeia (1.000 passos)")



## ----gibbs-burnin, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE------------------------------

# Configuração

n <- 10000
burnin <- 10000
k <- 10

theta <- vector(length = n)
Y <- vector(length = n)



## ----gibbs-burnin1, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-----------------------------

# Amostrador de Gibbs

# Período de burn-in

theta_b <- 0.3 # theta_0 = 0.3

Y_b <- x + rpois(n = 1,
                 lambda = lambda * (1 - theta_b))

for (t in 1:(burnin-1)){
  
  theta_b <- rbeta(n = 1,
                   shape1 = alpha + x,
                   shape2 = gamma + Y_b - x)
  
  Y_b <- x + rpois(n = 1,
                   lambda = lambda * (1 - theta_b))
  
}



## ----gibbs-burnin2, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-----------------------------

# Amostrador de Gibbs

# Período pós burn-in

theta_n <- theta_b
Y_n <- Y_b

for (t in 1:(k*n) ){
  
  theta_n <- rbeta(n = 1,
                   shape1 = alpha + x,
                   shape2 = gamma + Y_n - x)
  
  Y_n <- x + rpois(n = 1,
                   lambda = lambda * (1 - theta_n))
  
  if (t %% k == 0) {
    theta[(t/k)] <- theta_n
    Y[(t/k)] <- Y_n
  }
}



## ----gibbs5, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="80%", fig.align='center'----

par(mfrow = c(2,1))

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "")

plot(Y, type = "l",
     ylab = "Y", xlab = "t",
     main = "")



## ----gibbs6, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

library(ggplot2)

df <- data.frame(theta, Y)

p <- ggplot(data = df,
            mapping = aes(y = Y, x = theta)) +
  geom_point() +
  geom_density2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.25, colour = "black") +
  labs(y = "Y", x = expression(theta), title = "Amostra e densidade (estimada) conjunta da distribuição a posteriori")

p



## ----gibbs7, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

p <- ggplot(data = df,
            mapping = aes(x = theta)) +
  geom_histogram(aes(y = ..density..), binwidth = density(df$theta)$bw,
                 colour = "white") +
  geom_density(colour = "red", size = 1) +
  theme_bw() +
  labs(x = expression(theta), y = "Densidade")
  
p



## ----gibbs8, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE------------------------------------

# média _a posteriori_
mean(theta)

# mediana _a posteriori_
median(theta)

# proabilidade _a posteriori_ theta >= 0.07
mean(theta >= 0.07)

# intervalos de credibilidade (quantílico)
quantile(theta, c(0.025, 0.975))

# intervalos de credibilidade (HPD)
library(HDInterval)
hdi(theta, credMass = 0.95)



## ----mcmc_converge, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="90%"----

# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 1000
theta <- vector(length = n)

# Alogritmo M-H

theta[1] <- 0.01

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta[t])),
             1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

theta2 <- vector(length = n)

theta2[1] <- 0.3

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta2[t])),
             1)
  
  theta2[(t+1)] <- sample(c(theta_cand, theta2[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

theta3 <- vector(length = n)

theta3[1] <- 0.7

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta3[t])),
             1)
  
  theta3[(t+1)] <- sample(c(theta_cand, theta3[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "",
     ylim = c(0,0.1))
lines(theta2, col = "red")
lines(theta3, col = "blue")
legend("topright",
       legend = c(expression(theta[0]==0.01),
                  expression(theta[0]==0.3),
                  expression(theta[0]==0.7)),
       col = c("black", "red", "blue"), bty = "n", lty = 1)



## ----mcmc_converge2, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="70%"----

# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 1000
theta4 <- vector(length = n)

# Alogritmo M-H

theta4[1] <- 0.3 # theta_0 = 0.3

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = 1, shape2 = 1)
  
  rho <- min( ( exp(-lambda * theta_cand) * theta_cand^(x+alpha-1) * (1 - theta_cand)^(gamma-1) ) / ( exp(-lambda * theta4[t]) * theta4[t]^(x+alpha-1) * (1 - theta4[t])^(gamma-1) ),
              1)
  
  theta4[(t+1)] <- sample(c(theta_cand, theta4[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

plot(theta4, type = "l", col = "steelblue",
     ylab = expression(theta), xlab = "t",
     main = "")



## ----mcmc_converge3, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

par(mfrow = c(1,2))
acf(theta, main = expression("Proposta Beta" (x + alpha, gamma) ))
acf(theta4, main = expression("Proposta Beta" (1, 1) ))



## ----mcmc_converge4, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="90%"----

# Configuração

x <- 1
alpha <- 1
gamma <- 49
lambda <- 100

n <- 10000
theta <- vector(length = n)

# Alogritmo M-H

theta[1] <- 0.01

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta[t])),
             1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                         size = 1,
                         prob = c(rho, 1 - rho))
  
}

theta2 <- vector(length = n)

theta2[1] <- 0.3

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta2[t])),
             1)
  
  theta2[(t+1)] <- sample(c(theta_cand, theta2[t]),
                          size = 1,
                          prob = c(rho, 1 - rho))
  
}

theta3 <- vector(length = n)

theta3[1] <- 0.7

for (t in 1:(n-1)){
  
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda * (theta_cand - theta3[t])),
             1)
  
  theta3[(t+1)] <- sample(c(theta_cand, theta3[t]),
                          size = 1,
                          prob = c(rho, 1 - rho))
  
}

plot(theta, type = "l",
     ylab = expression(theta), xlab = "t",
     main = "",
     ylim = c(0,0.1))
lines(theta2, col = "red")
lines(theta3, col = "blue")
legend("topright",
       legend = c(expression(theta[0]==0.01),
                  expression(theta[0]==0.3),
                  expression(theta[0]==0.7)),
       col = c("black", "red", "blue"), bty = "n", lty = 1)


library(coda)

theta.coda <- mcmc.list(mcmc(theta),
                        mcmc(theta2), 
                        mcmc(theta3))

traceplot(theta.coda, ylim = c(0, 0.1))
plot(theta.coda)
summary(theta.coda)

gelman.plot(theta.coda,
            col = c("steelblue", "black"))

gelman.diag(theta.coda)


## ----mcmc_converge5, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------------------

# Configuração

x <- 1
alpha <- 1
gamma <- 49
# lambda <- 100

n <- 10000
theta <- vector(length = n)
lambda <- vector(length = n)



## ----mcmc_converge6, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------------------

# Amostrador de Gibbs
theta[1] <- 0.3 # theta_0 = 0.3

for (t in 1:(n-1)){
  lambda[t] <- rgamma(n = 1, shape = x + 1, rate = theta[t])
  
  # Passo de M-H
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
  
  rho <- min(exp(-lambda[t] * (theta_cand - theta[t])), 1)
  
  theta[(t+1)] <- sample(c(theta_cand, theta[t]),
                          size = 1,
                          prob = c(rho, 1 - rho))
}
lambda[n] <- rgamma(n = 1, shape = x + 1, rate = theta[n])



## ----mcmc_converge7, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

par(mfrow = c(2,1))

plot(theta, type = "l", col = "steelblue",
     ylab = expression(theta), xlab = "t",
     main = "")

plot(lambda, type = "l", col = "steelblue",
     ylab = expression(lambda), xlab = "t",
     main = "")


