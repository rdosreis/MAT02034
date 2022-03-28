## ----ex1, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="90%"-----

library(dplyr)
library(ggplot2)

set.seed(666)
M = 1000 # número de iterações

df = tibble(t = 1:M,
            x = runif(length(t),-1, 1),
            y = runif(length(t),-1, 1)) %>%
  mutate(
    Circ = ifelse(x ^ 2 + y ^ 2 <= 1, 1, 0),
    pi_est = round(4 * cumsum(Circ) / t, 4),
    erro = round(abs(pi - pi_est), 4),
    erro_est = round(sqrt((
      cumsum(16 * Circ) / t - pi_est ^ 2
    ) / t), 4)
  )

p <- ggplot() + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "black") +
  geom_rect(aes(
    xmin = -1,
    ymin = -1,
    xmax = 1,
    ymax = 1
  ),
  color = "black",
  alpha = 0) +
  guides(color = FALSE)

p + labs(title = expression(paste(
  "Método de Monte Carlo para a estimação de ", pi
)))



## ----ex1-chuva, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="90%"----

p +
  geom_point(data = df, aes(x = x, y = y, colour = factor(Circ)), size = 3) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = expression(paste("Método de Monte Carlo para a estimação de ",pi)),
        subtitle = paste("M = ",df$t[M],"  ;   pi_est = 4*(",cumsum(df$Circ)[M],"/",df$t[M],") = ",df$pi_est[M],"  ;   erro = ",df$erro[M],"  ;   erro_est = ",df$erro_est[M]))



## ----ex1-chuva2, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="90%"----


# Estimativas com o aumento do tamanho amostral
df %>%
  ggplot(aes(x = t, y = pi_est)) + theme_bw() +
  geom_line() +
  geom_hline(yintercept = pi, linetype = "longdash") +
  geom_ribbon(aes(ymin = pi_est - erro_est, ymax = pi_est + erro_est), alpha =
                0.3) +
  xlab("M") +
  ylab(expression(paste("Estimativa do ", pi)))



## ----ex2, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="90%"-----

set.seed(666)
M = 1000 # número de iterações
theta = 74046 - 27240 * exp(1)
df = tibble(t = 1:M,
            u = runif(length(t), 0, 1),
            y = rbeta(length(t), 4, 6)) %>%
  mutate(
    est_u = cumsum(exp(u + 3 * log(u) + 5 * log(1 - u))) / t,
    est_y = cumsum(exp(lbeta(4, 6) + y)) / t,
    erro_u = abs(theta - est_u),
    erro_y = abs(theta - est_y),
    erro_est_u = sqrt((cumsum(exp(
      2 * (u + 3 * log(u) + 5 * log(1 - u))
    )) / t - est_u ^ 2) / t),
    erro_est_y = sqrt((cumsum(exp(
      2 * (lbeta(4, 6) + y)
    )) / t - est_y ^ 2) / t)
  )
# Estimativas com o aumento do tamanho amostral
df %>%
  ggplot() + theme_bw() +
  geom_hline(yintercept = theta, linetype = "longdash") +
  geom_line(aes(x = t, y = est_u, colour = "Uniforme")) +
  geom_ribbon(
    aes(
      x = t,
      y = est_u,
      ymin = est_u - erro_est_u,
      ymax = est_u + erro_est_u,
      fill = "Uniforme"
    ),
    alpha = 0.4
  ) +
  geom_line(aes(x = t, y = est_y, colour = "Beta")) +
  geom_ribbon(
    aes(
      x = t,
      y = est_y,
      ymin = est_y - erro_est_y,
      ymax = est_y + erro_est_y,
      fill = "Beta"
    ),
    alpha = 0.4
  ) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + xlab("M") + ylab("Estimativa") +
  labs(fill = "Estratégia") + guides(colour = FALSE)


