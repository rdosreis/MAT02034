## ----carrega-dados, echo=TRUE, message=FALSE, warning=FALSE-------------------------

library(readr)
library(dplyr)

taxasMiss <- read_delim(
  file = here::here("dados",
                    "Tsutakawa1985.txt"),
  delim = "\t", escape_double = FALSE,
  trim_ws = TRUE)

taxasMiss



## ----gibbs, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------------

library(invgamma)

# Configuração

mu_0 <- 0
sigma2_mu <- 1000
a <- 5
b <- 1
c_T <- 1.5

n <- length(taxasMiss$y)
M <- 10000
burnin <- 10000
k <- 10

cadeia_post <- matrix(0, nrow = M, ncol = n + 2)
colnames(cadeia_post) <- c(paste0("theta_", 1:n), "mu", "tau2")

# Valores iniciais
theta <- log( (taxasMiss$y + 1/2)/(taxasMiss$N - taxasMiss$y + 1/2) )
mu <- 0
tau2 <- 1

cadeia_post[1,] <- c(theta, mu, tau2)



## ----gibbs1, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE---------------------

# Amostrador de Gibbs (burnin)

for (t in 1:burnin){

  # Passos de M-H passeio aleatório
  theta_cand <- theta + 
    rnorm(n = n, mean = rep(0, n), sd = rep(c_T, n))
  
  razao_a <- ( dbinom(x = taxasMiss$y, size = taxasMiss$N,
    prob = (exp(theta_cand)/(1 + exp(theta_cand))) ) * 
    dnorm(x = theta_cand, mean = mu, sd = sqrt(tau2)) )/
    ( dbinom(x = taxasMiss$y, size = taxasMiss$N,
    prob = (exp(theta)/(1 + exp(theta))) ) * 
    dnorm(x = theta, mean = mu, sd = sqrt(tau2)) )
  
  rho <- apply(X = data.frame(razao_a),
               MARGIN = 1,
               FUN = function(x){min(x, 1)})
  u <- runif(n = n, min = 0, max = 1)
  theta <- ifelse(u <= rho, theta_cand, theta)
  
  mu <- rnorm(n = 1,
              mean = (sigma2_mu/(sigma2_mu + tau2)) * mean(theta) +
                (sigma2_mu/(sigma2_mu + tau2)) * mu_0,
              sd = sqrt( (sigma2_mu*tau2)/(sigma2_mu + tau2) ) )
  
  tau2 <- rinvgamma(n = 1,
                    shape = a + n/2,
                    rate = b + (1/2) * sum( (theta - mu)^2 ))
}



## ----gibbs1.2, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-------------------

for (t in 1:(k*M)){
  # Passos de M-H passeio aleatório
  theta_cand <- theta + 
    rnorm(n = n, mean = rep(0, n), sd = rep(c_T, n))
  
  razao_a <- ( dbinom(x = taxasMiss$y, size = taxasMiss$N,
    prob = (exp(theta_cand)/(1 + exp(theta_cand))) ) * 
    dnorm(x = theta_cand, mean = mu, sd = sqrt(tau2)) )/
    ( dbinom(x = taxasMiss$y, size = taxasMiss$N,
    prob = (exp(theta)/(1 + exp(theta))) ) * 
    dnorm(x = theta, mean = mu, sd = sqrt(tau2)) )
  
  rho <- apply(X = data.frame(razao_a),
               MARGIN = 1,
               FUN = function(x){min(x, 1)})
  u <- runif(n = n, min = 0, max = 1)
  theta <- ifelse(u <= rho, theta_cand, theta)
  
  mu <- rnorm(n = 1,
              mean = (sigma2_mu/(sigma2_mu + tau2)) * mean(theta) +
                (sigma2_mu/(sigma2_mu + tau2)) * mu_0,
              sd = sqrt( (sigma2_mu*tau2)/(sigma2_mu + tau2) ) )
  
  tau2 <- rinvgamma(n = 1,
                    shape = a + n/2,
                    rate = b + (1/2) * sum( (theta - mu)^2 ))
  
  if (t %% k == 0) {cadeia_post[(t/k),] <- c(theta, mu, tau2)}
}



## ----mcmc_converge, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

par(mfrow = c(3,2))

plot(cadeia_post[,"mu"], type = "l", col = "steelblue",
     ylab = expression(mu), xlab = "t",
     main = "")

acf(cadeia_post[,"mu"], main = expression(mu) )

plot(cadeia_post[,"tau2"], type = "l", col = "steelblue",
     ylab = expression(tau^2), xlab = "t",
     main = "")

acf(cadeia_post[,"tau2"], main = expression(tau^2))

plot(cadeia_post[,"theta_1"], type = "l", col = "steelblue",
     ylab = expression(theta[1]), xlab = "t",
     main = "")

acf(cadeia_post[,"theta_1"], main = expression(theta[1]))



## ----inferencia, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

par(mfrow = c(1,2))

hist(cadeia_post[,"mu"], probability = TRUE,
     breaks = 40,
     col = "steelblue",
     border = "white",
     xlab = expression(mu),
     ylab = "Densidade",
     main = "")

hist(cadeia_post[,"tau2"], probability = TRUE,
     breaks = 40,
     col = "steelblue",
     border = "white",
     xlab = expression(tau^2),
     ylab = "Densidade",
     main = "")



## ----inferencia2, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="90%"----

par(mfrow = c(1,2))

hist(cadeia_post[,"theta_1"], probability = TRUE,
     col = "steelblue",
     border = "white",
     xlab = expression(theta[1]),
     ylab = "Densidade",
     main = "")

hist(cadeia_post[,"theta_84"], probability = TRUE,
     col = "steelblue",
     border = "white",
     xlab = expression(theta[84]),
     ylab = "Densidade",
     main = "")



## ----inferencia3, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="90%"----

par(mfrow = c(1,2))

hist(x = exp(cadeia_post[,"theta_1"])/(1 + exp(cadeia_post[,"theta_1"])),
     probability = TRUE,
     col = "lightsalmon",
     border = "white",
     xlab = expression(p[1]),
     ylab = "Densidade",
     main = "")

hist(x = exp(cadeia_post[,"theta_84"])/(1 + exp(cadeia_post[,"theta_84"])),
     probability = TRUE,
     col = "lightsalmon",
     border = "white",
     xlab = expression(p[84]),
     ylab = "Densidade",
     main = "")



## ----inferencia4, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE----------------

# Resumo da distribuição 
# a posteriori das taxas de scram

p_post <- data.frame(
  cidade = factor(taxasMiss$Cidade),
  y = taxasMiss$y,
  N = taxasMiss$N,
  EMV = taxasMiss$y/taxasMiss$N,
  media = colMeans( exp(cadeia_post[,1:n])/(1 + exp(cadeia_post[,1:n])) ) )

p_post$li95 <- apply(X = exp(cadeia_post[,1:n])/(1 + exp(cadeia_post[,1:n])),
                          MARGIN = 2,
                          FUN = quantile, probs = 0.025)

p_post$ls95 <- apply(X = exp(cadeia_post[,1:n])/(1 + exp(cadeia_post[,1:n])),
                          MARGIN = 2,
                          FUN = quantile, probs = 0.975)

head(p_post)



## ----inferencia5, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="95%"----

library(ggplot2)

p <- ggplot(data = p_post) +
  geom_point(mapping = 
               aes(x = cidade, #reorder(cidade, desc(media)), 
                   y = media),
             color = "steelblue") +
  geom_point(mapping = 
               aes(x = cidade, #reorder(cidade, desc(media)),
                   y = EMV),
             shape = 23,
             fill = "lightsalmon", color = "lightsalmon") +
  geom_errorbar(mapping = 
                  aes(x = cidade, #reorder(cidade, desc(media)),
                      ymin = li95, ymax = ls95),
                width=.2, position = position_dodge(.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Cidade", y = "Taxa de óbito", title = "Estimativas de Taxa de óbito por câncer de estômago em homens com idades\n entre 45 e 64 anos das 84 maiores cidades do Missouri (média a posteriori e\n intervalo de credibilidade de 95%; *EMV em vermelho)")

p


