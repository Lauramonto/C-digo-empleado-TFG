library(coda)
library(dplyr)
library(gplot2)

load("var_select_reduce_cauchy_model.rda")
View(mcmc.output)

subset_mcmc_list <- mcmc.list(
  as.mcmc(mcmc.output$samples$chain1[, 1:1000]),
  as.mcmc(mcmc.output$samples$chain2[, 1:1000])
)

#  //////////////// R-HAT //////////////////

# que tan diferentes son las cadenas
gelman_results <- gelman.diag(subset_mcmc_list)
print(gelman_results)
which(gelman_results$psrf[,1] > 1.1)



# --- Análisis para cada parámetro

subset_mcmc_list <- mcmc.list(
  as.mcmc(mcmc.output$samples$chain1),
  as.mcmc(mcmc.output$samples$chain2)
)


# -------------- Beta ----------------------------------------------------------

beta_cols <- grep("^beta\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
mcmc_beta_only <- lapply(subset_mcmc_list, function(chain) chain[, beta_cols])
class(mcmc_beta_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_beta <- gelman.diag(mcmc_beta_only, multivariate = TRUE)
gelman_results_beta
# Ver los que tienen R-hat > 1.1
rhat_univ <- gelman_results_beta$psrf[, 1]
rhat_problematicos = rhat_univ[rhat_univ > 1.1]


# Gráfico

rhat_values <- gelman_results_beta$psrf[, 1]

df_rhat <- data.frame(Rhat = rhat_values)

p <- ggplot(df_rhat, aes(x = Rhat)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 1.1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "R-hat", y = "Número de parámetros") +
  theme_minimal()
p
ggsave("rhat_beta.png", plot = p, width = 6, height = 6, dpi = 300)

# -------------- Gamma ---------------------------------------------------------


gamma_cols <- grep("^gamma\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
mcmc_gamma_only <- lapply(subset_mcmc_list, function(chain) chain[, gamma_cols])
class(mcmc_gamma_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_gamma <- gelman.diag(mcmc_gamma_only, multivariate = TRUE)

# Mostrar los que tienen R-hat > 1.1
rhat_gamma <- gelman_results_gamma$psrf[, 1]
rhat_gamma_problematicos <- rhat_gamma[rhat_gamma > 1.1]
print(rhat_gamma_problematicos)

autocorr.plot(subset_mcmc_list[, "gamma[6]"])
traceplot(subset_mcmc_list[,"gamma[6]"])
effectiveSize(subset_mcmc_list[,"gamma[6]"])
dnorm(mcmc.output$samples$chain1[,"gamma[6]"])


#Gráfico
df_gamma <- data.frame(Rhat = rhat_gamma)

g <- ggplot(df_gamma, aes(x = Rhat)) +
  geom_histogram(binwidth = 0.025, fill = "#a1d99b", color = "black", boundary = 0) +
  geom_vline(xintercept = 1.1, color = "red", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(floor(min(df_gamma$Rhat)*10)/10, ceiling(max(df_gamma$Rhat)*10)/10, by = 0.1)) +
  labs(x = "R-hat", y = "Número de parámetros") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggsave("traceplot.png", plot = traceplot(subset_mcmc_list[,"gamma[6]"]), width = 6, height = 6, dpi = 300)
ggsave("rhatgamma.png", plot = g, width = 6, height = 6, dpi = 300)

# -------------- Pi ------------------------------------------------------------

subset_mcmc_list <- mcmc.list(
  as.mcmc(mcmc.output$samples$chain1),
  as.mcmc(mcmc.output$samples$chain2)
)

# Selecciona 1500 nombres aleatorios

pi_cols <- grep("^pi\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
set.seed(1234)  # para reproducibilidad
pi_sample_cols <- sample(pi_cols, 1500)
mcmc_pi_only <- lapply(subset_mcmc_list, function(chain) chain[, pi_sample_cols])
class(mcmc_pi_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_pi <- gelman.diag(mcmc_pi_only, multivariate = TRUE)

# Mostrar los que tienen R-hat > 1.1
rhat_pi <- gelman_results_pi$psrf[, 1]
rhat_pi_problematicos <- rhat_pi[rhat_pi > 1.1]


#Gráfico
df_pi <- data.frame(Rhat = rhat_pi)
pi <- ggplot(df_pi, aes(x = Rhat)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 1.1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "R-hat", y = "Número de parámetros") +
  theme_minimal()


# Resumen

media_rhat <- mean(rhat_pi)
max_rhat <- max(rhat_pi)
min_rhat <- min(rhat_pi)
porcentaje_mayor_1_1 <- mean(rhat_pi > 1.1) * 100

resumen <- data.frame(
  Estadístico = c("Media R-hat", "Máximo R-hat", "Mínimo R-hat", "% R-hat > 1.1"),
  Valor = c(round(media_rhat, 4), round(max_rhat, 4), round(min_rhat, 4), round(porcentaje_mayor_1_1, 2))
)

print(resumen)

ggsave("rhatpi.png", plot = pi, width = 6, height = 6, dpi = 300)

# -------------- Ef. Espaciales (u) --------------------------------------------

# Selecciona 1500 nombres aleatorios
u_cols <- grep("^u\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
set.seed(123)  # para reproducibilidad
u_sample_cols <- sample(u_cols, 1500)
mcmc_u_only <- lapply(subset_mcmc_list, function(chain) chain[, u_sample_cols])
class(mcmc_u_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_u <- gelman.diag(mcmc_u_only, multivariate = TRUE)

# Mostrar los que tienen R-hat > 1.1
rhat_u <- gelman_results_u$psrf[, 1]
rhat_u_problematicos <- rhat_u[rhat_u > 1.1]
print(rhat_u_problematicos)

#Gráfico
df_u <- data.frame(Rhat = rhat_u)
u <- ggplot(df_u, aes(x = Rhat)) +
  geom_histogram(binwidth = 0.01, fill = "orange", color = "black") +
  geom_vline(xintercept = 1.1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "R-hat", y = "Número de parámetros") +
  theme_minimal()

# Resumen
media_rhat_u <- mean(rhat_u)
max_rhat_u <- max(rhat_u)
min_rhat_u <- min(rhat_u)
porcentaje_mayor_1_1_u <- mean(rhat_u > 1.1) * 100

resumen_u <- data.frame(
  Estadístico = c("Media R-hat", "Máximo R-hat", "Mínimo R-hat", "% R-hat > 1.1"),
  Valor = c(round(media_rhat_u, 4), round(max_rhat_u, 4), round(min_rhat_u, 4), round(porcentaje_mayor_1_1_u, 2))
)

print(resumen_u)

ggsave("rhatu.png", plot = u, width = 6, height = 6, dpi = 300)

# -------------- Ef. Aleatorios (v) --------------------------------------------

# Selecciona 1500 nombres aleatorios
v_cols <- grep("^v\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
set.seed(1234)  # para reproducibilidad
v_sample_cols <- sample(v_cols, 1500)
mcmc_v_only <- lapply(subset_mcmc_list, function(chain) chain[, v_sample_cols])
class(mcmc_v_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_v <- gelman.diag(mcmc_v_only, multivariate = TRUE)

# Mostrar los que tienen R-hat > 1.1
rhat_v <- gelman_results_v$psrf[, 1]
rhat_v_problematicos <- rhat_v[rhat_v > 1.1]
print(rhat_v_problematicos)

# Gráafico

df_v <- data.frame(Rhat = rhat_v)
v <- ggplot(df_v, aes(x = Rhat)) +
  geom_histogram(binwidth = 0.01, fill = "salmon", color = "black") +
  geom_vline(xintercept = 1.1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "R-hat", y = "Número de parámetros") +
  theme_minimal()


# Resumen

media_rhat_v <- mean(rhat_v)
max_rhat_v <- max(rhat_v)
min_rhat_v <- min(rhat_v)
porcentaje_mayor_1_1_v <- mean(rhat_v > 1.1) * 100

resumen_v <- data.frame(
  Estadístico = c("Media R-hat", "Máximo R-hat", "Mínimo R-hat", "% R-hat > 1.1"),
  Valor = c(round(media_rhat_v, 4), round(max_rhat_v, 4), round(min_rhat_v, 4), round(porcentaje_mayor_1_1_v, 2))
)

print(resumen_v)

ggsave("rhatv.png", plot = v, width = 6, height = 6, dpi = 300)

# -------------- Varianza u -----------------------------------------------------
  
sigma_cols <- grep("^sigma_u\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
mcmc_sigma_only <- lapply(subset_mcmc_list, function(chain) chain[, sigma_cols])
class(mcmc_sigma_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_sigma <- gelman.diag(mcmc_sigma_only, multivariate = TRUE)

# Mostrar valores con R-hat > 1.1
rhat_sigma <- gelman_results_sigma$psrf[, 1]
rhat_sigma_problematicos <- rhat_sigma[rhat_sigma > 1.1]
print(rhat_sigma_problematicos)

media_rhat_sigma <- mean(rhat_sigma)
max_rhat_sigma <- max(rhat_sigma)
min_rhat_sigma <- min(rhat_sigma)
porcentaje_mayor_1_1_sigma <- mean(rhat_sigma > 1.1) * 100

resumen_sigma <- data.frame(
  Estadístico = c("Media R-hat", "Máximo R-hat", "Mínimo R-hat", "% R-hat > 1.1"),
  Valor = c(round(media_rhat_sigma, 4), round(max_rhat_sigma, 4), round(min_rhat_sigma, 4), round(porcentaje_mayor_1_1_sigma, 2))
)

print(resumen_sigma)

# Densidades
df_sigma <- data.frame(Rhat = rhat_sigma)

sigma1u <- as.numeric(subset_mcmc_list[[1]][, "sigma_u[1]"])
sigma2u <- as.numeric(subset_mcmc_list[[1]][, "sigma_u[2]"])
sigma3u <- as.numeric(subset_mcmc_list[[1]][, "sigma_u[3]"])
sigma4u <- as.numeric(subset_mcmc_list[[1]][, "sigma_u[4]"])
sigma5u <- as.numeric(subset_mcmc_list[[1]][, "sigma_u[5]"])
sigma6u <- as.numeric(subset_mcmc_list[[1]][, "sigma_u[6]"])


df_sigma <- data.frame(
  sigma = c(sigma1u, sigma2u, sigma3u, sigma4u, sigma5u, sigma6u),
  grupo = rep(paste0("sigma_u", 1:6), each = length(sigma1u))
)

ggplot(df_sigma, aes(x = sigma, color = grupo, fill = grupo)) +
  geom_density(alpha = 0.3, linewidth = 0.9) +
  labs(x = expression(sigma[u][j]),
       y = "Densidad",
       color = "Parámetro",
       fill = "Parámetro") +
  theme_minimal() +
  theme(
    legend.position = c(0.97, 0.97),  # Posición relativa (x, y)
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# -------------- Varianza v -----------------------------------------------------

# Selecciona columnas que comienzan por "sigma"
sigma_cols <- grep("^sigma_v\\[", colnames(subset_mcmc_list[[1]]), value = TRUE)
mcmc_sigma_only <- lapply(subset_mcmc_list, function(chain) chain[, sigma_cols])
class(mcmc_sigma_only) <- "mcmc.list"

# Diagnóstico de Gelman-Rubin
gelman_results_sigma <- gelman.diag(mcmc_sigma_only, multivariate = TRUE)
print(gelman_results_sigma)

# Mostrar valores con R-hat > 1.1
rhat_sigma <- gelman_results_sigma$psrf[, 1]
rhat_sigma_problematicos <- rhat_sigma[rhat_sigma > 1.1]
print(rhat_sigma_problematicos)

media_rhat_sigma <- mean(rhat_sigma)
max_rhat_sigma <- max(rhat_sigma)
min_rhat_sigma <- min(rhat_sigma)
porcentaje_mayor_1_1_sigma <- mean(rhat_sigma > 1.1) * 100

resumen_sigma <- data.frame(
  Estadístico = c("Media R-hat", "Máximo R-hat", "Mínimo R-hat", "% R-hat > 1.1"),
  Valor = c(round(media_rhat_sigma, 4), round(max_rhat_sigma, 4), round(min_rhat_sigma, 4), round(porcentaje_mayor_1_1_sigma, 2))
)

print(resumen_sigma)

# Densidad
sigma1v <- as.numeric(subset_mcmc_list[[1]][, "sigma_v[1]"])
sigma2v <- as.numeric(subset_mcmc_list[[1]][, "sigma_v[2]"])
sigma3v <- as.numeric(subset_mcmc_list[[1]][, "sigma_v[3]"])
sigma4v <- as.numeric(subset_mcmc_list[[1]][, "sigma_v[4]"])
sigma5v <- as.numeric(subset_mcmc_list[[1]][, "sigma_v[5]"])
sigma6v <- as.numeric(subset_mcmc_list[[1]][, "sigma_v[6]"])


df_sigma <- data.frame(
  sigma = c(sigma1v, sigma2v, sigma3v, sigma4v, sigma5v, sigma6v),
  grupo = rep(paste0("sigma_v", 1:6), each = length(sigma1))
)

ggplot(df_sigma, aes(x = sigma, color = grupo, fill = grupo)) +
  geom_density(alpha = 0.3, linewidth = 0.9) +
  labs(x = expression(sigma[u][j]),
       y = "Densidad",
       color = "Parámetro",
       fill = "Parámetro") +
  theme_minimal() +
  theme(
    legend.position = c(0.97, 0.97),  # Posición relativa (x, y)
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
# -------------- Lambda --------------------------------------------------------

# Resumen
summary_df <- mcmc.output[["summary"]][["chain1"]]
lambda_summary <- summary_df[grep("lambda", rownames(summary_df)), ]
View(lambda_summary)

summary_df <- mcmc.output[["summary"]][["chain1"]] %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("parametro") %>% 
  filter(grepl("lambda", parametro))
summary_df


# Densidad por cadena
subset_mcmc_list <- mcmc.list(
  as.mcmc(mcmc.output$samples$chain1),
  as.mcmc(mcmc.output$samples$chain2)
)

#plot
cadena1 <- as.numeric(subset_mcmc_list[[1]][, "lambda"])
cadena2 <- as.numeric(subset_mcmc_list[[2]][, "lambda"])

plot(density(cadena1), col = "blue", lwd = 2, main = "Densidad por cadena", xlab = "lambda")
lines(density(cadena2), col = "red", lwd = 2)
legend("topright", legend = c("Cadena 1", "Cadena 2"), col = c("blue", "red"), lwd = 2)

# ggplot
df_lambda <- data.frame(
  lambda = c(cadena1, cadena2),
  cadena = factor(rep(c("Cadena 1", "Cadena 2"), each = length(cadena1)))
)

# Gráfico de densidades
ggplot(df_lambda, aes(x = lambda, fill = cadena, color = cadena)) +
  geom_density(alpha = 0.4, linewidth = 0.9) +
  labs(x = expression(lambda),
       y = "Densidad",
       fill = "Cadena",
       color = "Cadena") +
  theme_minimal() +
  theme(
    legend.position = c(0.97, 0.97),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# gelman
lambda_mcmc_list <- mcmc.list(
  as.mcmc(mcmc.output$samples$chain1[, grep("lambda", colnames(mcmc.output$samples$chain1))]),
  as.mcmc(mcmc.output$samples$chain2[, grep("lambda", colnames(mcmc.output$samples$chain2))])
)
gelman_results_lambda <- gelman.diag(lambda_mcmc_list, multivariate = FALSE)

gelman_results$psrf  

# traceplot
traceplot(lambda_mcmc_list)

