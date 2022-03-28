## ----metodo-rejeicao, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE--------------------

metodo_rejeicao <- function(x, alpha, gamma, lambda, j = 0){
  repeat{
    j <- j + 1
# 1. Gerar de theta candidado de p e u de U(0,1)
  u <- runif(n = 1, 0, 1)
  theta_cand <- rbeta(
    n = 1, shape1 = x + alpha, shape2 = gamma)
# 2. Aceita theta candidato se u menor ou igual g/cp  
    if (u <= exp(-lambda * theta_cand))
      return(c(theta_cand, j))
  }
}



## ----metodo-rejeicao2, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-------------------

M_alvo <- 10000 # Número de amostras
theta <- c()
M_gerados <- c()

# Loop
for (i in 1:M_alvo){
  aux <- metodo_rejeicao(x = 1, alpha = 1,
                         gamma = 49, lambda = 100)
  theta[i] <- aux[1]
  M_gerados[i] <- aux[2]
}

taxa_aceita <- M_alvo/sum(M_gerados)
taxa_aceita



## ----metodo-rejeicao3, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

hist(theta, breaks = 40,
     probability = TRUE, border = "white",
     xlab = expression(theta), ylab = "Densidade",
     main = "Amostra da dist. a posteriori")

mean(theta)
mean(theta > 0.05)
quantile(theta, probs = c(0.025, 0.975))

## ----amostragem-importancia, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE-------------

amostragem_importancia <- function(x, alpha, gamma, lambda, M = 1000){

  # 1. Gerar de theta de p
  theta <- rbeta(n = M, shape1 = x + alpha, shape2 = gamma)
  # 2. Calcula pesos de importância
  w.theta <- exp(-lambda * theta)
  # 3. Estimativa da média a posteriori
  media_post <- weighted.mean(x = theta, w = w.theta)
  return(media_post)
}

amostragem_importancia(x = 1, alpha = 1,
                       gamma = 49, lambda = 100, M = 10000)


## ----sir, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE--------------------------------

amostragem_sir <- function(x, alpha, gamma, lambda, M = 1000){

  # 1. Gerar de theta de p(theta)
  theta <- rbeta(n = M, shape1 = x + alpha, shape2 = gamma)
  # 2. Calcula pesos de importância
  w.theta <- exp(-lambda * theta)
  # 3. Converte pesos em probabilidades
  p <- w.theta/sum(w.theta)
  # 4. Reamostra theta de acordo com p
  ind <- sample(x = 1:M, size = M, replace = TRUE, prob = p)
  theta_s <- theta[ind]
  return(theta_s)
}

theta_estrela <- amostragem_sir(x = 1, alpha = 1,
                     gamma = 49, lambda = 100, M = 10000)


## ----sir2, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="95%", fig.align='center'----

hist(theta_estrela, breaks = 40,
     probability = TRUE, border = "white",
     xlab = expression(theta), ylab = "Densidade",
     main = "Amostra da dist. a posteriori")


