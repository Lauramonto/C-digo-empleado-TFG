
load("var_select_reduce_cauchy_model.rda")


View(mcmc.output)
View(mcmc.output[["samples"]][["chain1"]])
View(mcmc.output[["summary"]][["chain1"]])


# //////////////// PI ////////////////////

df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_pi <- df %>% filter(grepl("^pi", segment)) 
View(df_pi)

# Significativos
pi_intervalos_positivos <- df_pi %>%
  filter(`95%CI_low` > 0 & `95%CI_upp` > 0)

n_positivos <- nrow(pi_intervalos_positivos)
total_pi <- nrow(df_pi)
porcentaje_positivos <- (n_positivos / total_pi) * 100
cat("Porcentaje:", round(porcentaje_positivos, 2), "%\n")

# //////////////// BETA //////////////////

df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_beta <- df %>% filter(grepl("^beta", segment)) 
View(df_beta)

# Significativos
beta_intervalos_positivos <- df_beta %>%
  filter(`95%CI_low` > 0 & `95%CI_upp` > 0 | `95%CI_low` < 0 & `95%CI_upp` < 0)  

n_positivos <- nrow(beta_intervalos_positivos)
total_beta <- nrow(df_beta)
porcentaje_positivos <- (n_positivos / total_beta) * 100
cat("Porcentaje:", round(porcentaje_positivos, 2), "%\n")


# //////////////// GAMMA //////////////////

df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_gam <- df %>% filter(grepl("^gamma", segment))
View(df_gam)

# Significativos
gam_intervalos_positivos <- df_gam %>%
  filter(`95%CI_low` > 0 & `95%CI_upp` > 0 | `95%CI_low` < 0 & `95%CI_upp` < 0)  

n_positivos <- nrow(gam_intervalos_positivos)
total_gam <- nrow(df_gam)
porcentaje_positivos <- (n_positivos / total_gam) * 100
cat("Porcentaje:", round(porcentaje_positivos, 2), "%\n")


# //////////////// EFECTOS ESPACIALES (u) //////////////////
  
df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_u <- df %>% filter(grepl("^u", segment))
View(df_u)

# Significativos
u_intervalos_positivos <- df_u %>%
  filter((`95%CI_low` > 0 & `95%CI_upp` > 0) | 
           (`95%CI_low` < 0 & `95%CI_upp` < 0)) 


n_positivos <- nrow(u_intervalos_positivos)
total_u <- nrow(df_u)
porcentaje_positivos <- (n_positivos / total_u) * 100
cat("Porcentaje:", round(porcentaje_positivos, 2), "%\n")


# //////////////// EFECTOS ALEATORIOS (v) //////////////////

  
df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_v <- df %>% filter(grepl("^v", segment))
View(df_v)

# Significativos
v_intervalos_positivos <- df_v %>%
  filter((`95%CI_low` > 0 & `95%CI_upp` > 0) | 
           (`95%CI_low` < 0 & `95%CI_upp` < 0)) 

n_positivos <- nrow(v_intervalos_positivos)
total_v <- nrow(df_v)
porcentaje_positivos <- (n_positivos / total_v) * 100
cat("Porcentaje:", round(porcentaje_positivos, 2), "%\n")

# //////////////// VARIANZA EF. ESPACIALES //////////////////
  
df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_u <- df %>% filter(grepl("^sigma_u", segment))
View(df_u)

# //////////////// VARIANZA EF. ALEATORIOS //////////////////

df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_v <- df %>% filter(grepl("^sigma_v", segment))
View(df_v)

# //////////////// LAMBDA //////////////////
  
df <- as.data.frame(mcmc.output[["summary"]][["chain1"]]) 
df <- rownames_to_column(df, var = "segment") 
df_lambda <- df %>% filter(grepl("^lambda", segment))
View(df_lambda)
