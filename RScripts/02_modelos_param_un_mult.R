## ----carrega-dados, echo=TRUE, message=FALSE, warning=FALSE---------------------------------

library(dplyr)

scram <- readr::read_table2(
  file = here::here("dados", "table76.txt"))

scram <- scram %>% 
  filter(Year == 1) %>% 
  select(-X5, -Year) %>% 
  rename(x = y) %>% 
  mutate(T = T/1000)

scram



## ----emv-lambda, echo=TRUE, message=FALSE, warning=FALSE------------------------------------

soma.x <- sum(scram$x)
soma.T <- sum(scram$T)
lambda.chap <- soma.x/soma.T
soma.x
soma.T
lambda.chap



## ----priori-gama, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

x <- seq(0, 5, length = 1000)

plot(x, dgamma(x, shape = 0.5, rate = 0.5),
     ylab = 'Densidade', xlab = expression(lambda),
     type = 'l', col = 'purple', lwd = 2, ylim = c(0,1.5))
lines(x, dgamma(x, shape = 0.5, rate = 1), col = 'red', lwd = 2) 
lines(x, dgamma(x, shape = 1.5, rate = 1), col = 'blue', lwd = 2)
lines(x, dgamma(x, shape = 2, rate = 3), col = 'orange', lwd = 2)
lines(x, dgamma(x, shape = 3, rate = 2), col = 'green', lwd = 2)

legend('topright',
       legend = c('Gama(0.5, 0.5)','Gama(0.5, 1)','Gama(1.5,1)', 'Gama(2,3)', 'Gama(3,2)'),
       lty = c(1,1,1,1,1), , lwd = c(2,2,2,2,2),
       col=c('purple', 'red', 'blue', 'orange', 'green'),
       bty = "n")





## ----aprox-grade, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------

# Passo 1: criar a grade

alpha_grade <- seq(from = 0.01, to = 4, length = 200)
beta_grade <- seq(from = 0.01, to = 4, length = 200)

grade_df <- expand.grid(alpha_grade, beta_grade)
names(grade_df) <- c("alpha_g", "beta_g")


## ----aprox-grade2, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

# Passos 2: avaliar a priori e vero. em cada ponto da grade

priori_ab <- function(a, b){
  a^(-1)*(b + 1)^(-2)
}

veros_ab <- function(a, b, dados){
  x <- dados$x
  t <- dados$T
  
  prod( (gamma(a + x) / (gamma(x + 1) * gamma(a))) 
        * ((b/(b + t))^a) * ((t/(b + t))^x) )
}


## ----aprox-grade3, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

grade_df$priori <- 0
grade_df$veros <- 0

for(i in 1:length(grade_df$priori)){
  
  grade_df$priori[i] <- priori_ab(a = grade_df$alpha_g[i],
                                  b = grade_df$beta_g[i])
  grade_df$veros[i] = veros_ab(a = grade_df$alpha_g[i],
                               b = grade_df$beta_g[i],
                               dados = scram)

}


## ----aprox-grade4, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

# Passo 3: aproximar a posteriori

library(dplyr)

grade_df <- grade_df %>% 
  mutate(post_naonorm = priori * veros,
         posteriori = post_naonorm/sum(post_naonorm))

# Confirmando que a posteriori aproximada soma 1

grade_df %>% 
  summarize(sum(post_naonorm), sum(posteriori))


## ----aprox-grade5, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

# Passo 4: amostrar da posteriori discretizada

post_amostra <- sample_n(grade_df, size = 1000, 
                        weight = posteriori,
                        replace = TRUE)


## ----grafico-post, echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%'----

# Gráfico da amostra a posteriori

library(ggplot2)

p <- ggplot(data = post_amostra,
            mapping = aes(x = alpha_g, y = beta_g)) +
  geom_point() +
  labs(x = expression(alpha), y = expression(beta))

p



## ----aprox-grade-est, echo=TRUE, message=FALSE, warning=FALSE-------------------------------

mean(post_amostra$alpha_g)
sd(post_amostra$alpha_g)

mean(post_amostra$beta_g)
sd(post_amostra$beta_g)


