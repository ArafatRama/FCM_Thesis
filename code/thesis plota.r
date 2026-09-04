# thesis plots
#wang transform distributional results 
library(ggplot2)

# ---- parameters (adjust to taste) ----
mu    <- 0
sigma <- 1
alpha <- 0.5          # distortion parameter; shift = alpha*sigma

x <- seq(-4, 5, length.out = 400)

# ---- NORMAL CASE ----
S_orig <- 1 - pnorm(x, mean = mu,               sd = sigma)   # S_X
S_dist <- 1 - pnorm(x, mean = mu + alpha*sigma, sd = sigma)   # S_{X*}

df_norm <- data.frame(
  x = rep(x, 2),
  S = c(S_orig, S_dist),
  Distribution = rep(c("Original",
                       "Distorted"), each = length(x))
)

p_norm <- ggplot(df_norm, aes(x, S, colour = Distribution, linetype = Distribution)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Original" = "grey40",
                                 "Distorted" = "#C0392B")) +
  scale_linetype_manual(values = c("Original" = "solid",
                                   "Distorted" = "22")) +
  labs(x = "x", y = expression(S(x)),
       title = "Wang Transform: Normal Survival Function") +
  theme_minimal(base_size = 11) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("wang_normal.pdf", p_norm, width = 5.5, height = 3.6)

# ---- LOGNORMAL CASE ----
# ln(Y) ~ N(mu, sigma^2); distorted ln(Y*) ~ N(mu + alpha*sigma, sigma^2)
y <- seq(0.01, 8, length.out = 400)
S_orig_ln <- 1 - plnorm(y, meanlog = mu,               sdlog = sigma)
S_dist_ln <- 1 - plnorm(y, meanlog = mu + alpha*sigma, sdlog = sigma)

df_ln <- data.frame(
  y = rep(y, 2),
  S = c(S_orig_ln, S_dist_ln),
  Distribution = rep(c("Original",
                       "Distorted"), each = length(y))
)

p_ln <- ggplot(df_ln, aes(y, S, colour = Distribution, linetype = Distribution)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Original"  = "grey40",
                                 "Distorted" = "red")) +
  scale_linetype_manual(values = c("Original"  = "solid",
                                   "Distorted" = "22")) +
  labs(x = "y", y = expression(S(y)),
       title = "Wang Transform: Lognormal Survival Function") +
  theme_minimal(base_size = 11) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))


ggsave("wang_lognormal.pdf", p_ln, width = 5.5, height = 3.6)


#lexis diagram 
par(mfrow = c(1, 1))
a <- 3
t <- 6

x_min <- t - a - 2
x_max <- t + 2
y_min <- 0
y_max <- a + 3

old_par <- par(
  mar = c(5, 5, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

plot(
  NA,
  xlim = c(x_min - 0.4, x_max + 0.2),
  ylim = c(-0.35, y_max + 0.2),
  axes = FALSE,
  xlab = "",
  ylab = "",
  asp = 1
)

rect(
  xleft = t,
  ybottom = a,
  xright = t + 1,
  ytop = a + 1,
  col = "grey75",
  border = NA
)

segments(
  x0 = x_min,
  y0 = 0,
  x1 = x_max,
  y1 = 0,
  lwd = 1.5,
  xpd = NA
)

segments(
  x0 = x_min,
  y0 = 0,
  x1 = x_min,
  y1 = y_max,
  lwd = 1.5,
  xpd = NA
)

segments(
  x0 = x_min,
  y0 = a,
  x1 = x_max,
  y1 = a,
  lwd = 1.2
)

segments(
  x0 = x_min,
  y0 = a + 1,
  x1 = x_max,
  y1 = a + 1,
  lwd = 1.2
)

segments(
  x0 = t,
  y0 = 0,
  x1 = t,
  y1 = y_max,
  lwd = 1.2
)

segments(
  x0 = t + 1,
  y0 = 0,
  x1 = t + 1,
  y1 = y_max,
  lwd = 1.2
)

cohorts <- c(
  t - a - 1,
  t - a,
  t - a + 1
)

for (cohort in cohorts) {
  abline(
    a = -cohort,
    b = 1,
    lwd = 1.2
  )
}

birth_year <- t - a

person_ages <- 0:(a + 2)
person_years <- birth_year + person_ages

shade_width <- 0.08

polygon(
  x = c(
    person_years,
    rev(person_years)
  ),
  y = c(
    person_ages - shade_width,
    rev(person_ages + shade_width)
  ),
  col = adjustcolor(
    "darkblue",
    alpha.f = 0.35
  ),
  border = NA
)

lines(
  person_years,
  person_ages,
  lwd = 2.5
)

points(
  person_years,
  person_ages,
  pch = 16,
  cex = 0.7
)

text(
  birth_year,
  0,
  labels = "Birth",
  pos = 1,
  offset = 0.7,
  xpd = TRUE,
  cex = 0.9
)

x_ticks <- c(
  t - a - 1,
  t - a,
  t - a + 1,
  t,
  t + 1
)

segments(
  x0 = x_ticks,
  y0 = 0,
  x1 = x_ticks,
  y1 = -0.12,
  lwd = 1.2,
  xpd = NA
)

segments(
  x0 = x_max,
  y0 = 0,
  x1 = x_max,
  y1 = -0.12,
  lwd = 1.2,
  xpd = NA
)

segments(
  x0 = x_min - 0.12,
  y0 = y_max,
  x1 = x_min,
  y1 = y_max,
  lwd = 1.2,
  xpd = NA
)

axis(
  side = 1,
  at = c(
    t - a - 1,
    t - a,
    t - a + 1,
    t,
    t + 1
  ),
  labels = c(
    expression(t - a - 1),
    expression(t - a),
    expression(t - a + 1),
    expression(t),
    expression(t + 1)
  ),
  tick = FALSE,
  line = 0.5,
  cex.axis = 1.1
)

axis(
  side = 2,
  at = c(a, a + 1),
  labels = c(
    expression(a),
    expression(a + 1)
  ),
  las = 1,
  tick = FALSE,
  line = 0.5,
  cex.axis = 1.1
)

mtext(
  "Year",
  side = 1,
  line = 3.5,
  cex = 1.3
)

mtext(
  "Age",
  side = 2,
  line = 3.5,
  cex = 1.3
)

par(old_par)

pdf("activation_functions.pdf", width = 9, height = 3)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
z <- seq(-4, 4, 0.01)
plot(z, 1/(1 + exp(-z)), type = "l", lwd = 2, ylab = expression(sigma(z)), main = "Sigmoid")
abline(h = c(0, 1), lty = 3)
plot(z, tanh(z), type = "l", lwd = 2, ylab = "tanh(z)", main = "Hyperbolic tangent")
abline(h = c(-1, 1), lty = 3)
plot(z, pmax(0, z), type = "l", lwd = 2, ylab = "ReLU(z)", main = "ReLU")
dev.off()
