# MixNorm-Verteilung

f_LogS_MixNorm<-function(y, a, mean, sd) {
  if (a < 0 || a > 1) {
    stop(" a muss im Intervall [0, 1] liegen.")
  }
  
  a=pmin(1-10^-15,a)
  
  if (y == 0) {
    return(-log(a))
  } 
  else if (y != 0) {
    return(-log((1 - a)*dnorm(y, mean = mean, sd=sd)))
  } 
}


# MixNorm-Verteilungskomponenten

f_LogS_Bernoulli<-function(x, a) {
  if (a < 0 || a > 1) {
    stop(" a muss im Intervall [0, 1] liegen.")
  }
  
  a=pmin(1-10^-15,a)
  
  if (x != 0 & x != 1)
    stop(" x muss 0 oder 1 sein.")
  
  else if (x==0) {
    return(-log(a))
  } 
  else if (x==1) {
    return(-log(1-a))
  } 
}


f_LogS_MixNorm_stetig<-function(y, mean, sd) {
  if (y == 0) {
    stop(" y muss > 0 sein.")
  } 
  else if (y != 0) {
    return(-log(dnorm(y, mean = mean, sd=sd)))
  } 
}

# Exzessmodelle (# y sind die Excesse auf der Ursprungsskala)

# Exzess-MixNorm


f_LogS_MixNorm_Exz<-function(y, a, mean, sd, quantile) {
  if (quantile<1) {
    stop(" quantile sollte >=1 sein")
  }
  if (a < 0 || a > 1) {
    stop(" a muss im Intervall [0, 1] liegen.")
  }
  
  a=pmin(1-10^-15,a)
  
  if (y== 0) {
    return(-log(a+(1-a)*pnorm(log10(y+quantile), mean = mean, sd=sd)))
  } 
  
  else if (y!= 0) {
    return(-log((1-a)/((y+quantile)*log(10))*(dnorm(log10(y+quantile), mean = mean, sd=sd))))
  } 
}



f_LogS_Bernoulli_Exz<-function(x, a, mean, sd, u) {
  if (a < 0 || a > 1) {
    stop(" a muss im Intervall [0, 1] liegen.")
  }
  
  a=pmin(1-10^-15,a)
  
  if (x != 0 & x != 1)
    stop(" x muss 0 oder 1 sein.")
  
  else if (x==0) {
    return(-log(a + (1-a)*pnorm(log10(u), mean=mean, sd=sd)))
  } 
  else if (x==1) {
    return(-log(1-(a + (1-a)*pnorm(log10(u), mean=mean, sd=sd))))
  } 
}

f_LogS_MixNorm_Exz_stetig<-function(y, mean, sd, u) { 
  if (u<1) {
    stop(" quantile sollte >=1 sein")
  }
  
  if (y<=0){
    stop("y (Excess) muss >0 sein")
  }
  else if (y> 0) {
    return(-log(1/((y+u)*log(10))*dnorm(log10(y+u), mean = mean, sd=sd)/(1-pnorm(log10(u),mean=mean, sd=sd)))
    )
  } 
}

#MixGPD

f_LogS_MixGPD<-function(y, a, sigmau, xi) {
  if (a < 0 || a > 1) {
    stop(" a muss im Intervall [0, 1] liegen.")
  }
  
  a=pmin(1-10^-15,a)
  
  if(y<0){
    stop("y muss >=0 sein")
  }
  
  else if (y == 0) {
    return(-log(a))
    
  } 
  else if (y > 0) {
    return(-log((1-a)*reservr::dgpd(y,u = 0, sigmau = sigmau, xi = xi)))
  } 
}


f_LogS_MixGPD_stetig <- function(y, sigmau, xi) {
  
  if(y<=0){
    stop("y muss >0 sein")
  }
  else if (y > 0) {
    return(-log(reservr::dgpd(y,u = 0, sigmau = sigmau, xi = xi)))
  } 
}


# Beispiel mit simulierten Daten

data_predictions <- test_data %>%
  mutate(x_lw= ifelse(y_lw==0, 0, 1),
         x_sturm= ifelse(y_sturm==0, 0, 1),
         x_feuer= ifelse(y_feuer==0, 0, 1),
         x_el= ifelse(y_el==0, 0, 1),
         
         mean_nn_lw= pred_params_nn_lw$dists[[2]]$mean,
         sd_nn_lw= pred_params_nn_lw$dists[[2]]$sd,
         a_nn_lw = pred_params_nn_lw$probs[[1]],
         mean_gf_lw = global_fit_lw$params$dists[[2]]$mean,
         sd_gf_lw = global_fit_lw$params$dists[[2]]$sd,
         a_gf_lw =  global_fit_lw$params$probs[[1]],
         mean_llm_lw = mu_llm_lw,
         a_llm_lw= p0_llm_lw)



data_excess_predictions <- test_data_extremes %>%
  mutate(x_lw= ifelse(excess_lw==0, 0, 1),
         x_sturm= ifelse(excess_sturm==0, 0, 1),
         x_feuer= ifelse(excess_feuer==0, 0, 1),
         x_el= ifelse(excess_el==0, 0, 1),
         
         mean_nn_lw= pred_params_nn_lw$dists[[2]]$mean,
         sd_nn_lw= pred_params_nn_lw$dists[[2]]$sd,
         a_nn_lw = pred_params_nn_lw$probs[[1]],
         mean_gf_lw = global_fit_lw$params$dists[[2]]$mean,
         sd_gf_lw = global_fit_lw$params$dists[[2]]$sd,
         a_gf_lw =  global_fit_lw$params$probs[[1]],
         mean_llm_lw = mu_llm_lw,
         a_llm_lw= p0_llm_lw,
         a_nn_gpd_lw = pred_params_nn_mixgpd_lw$probs[[1]],
         a_gf_gpd_lw = global_fit_mixgpd_lw$params$probs[[1]],
         xi_gf_lw = fit_gpd_lw$params$xi,
         sigmau_nn_gpd_lw =  pred_params_nn_mixgpd_lw$dists[[2]]$sigmau,
         sigmau_gf_gpd_lw = global_fit_mixgpd_lw$params$dists[[2]]$sigmau
  )


# MixNorm
LogS_Mixnorm_NN_lw <- mean(trim=1/nrow(data_predictions), x= mapply(f_LogS_MixNorm,
                                                                    data_predictions$y_lw,
                                                                    data_predictions$a_nn_lw, 
                                                                    data_predictions$mean_nn_lw, 
                                                                    data_predictions$sd_nn_lw))
LogS_Mixnorm_LLM_lw  <- mean(trim=1/nrow(test_features), x= mapply(f_LogS_MixNorm,
                                                                   data_predictions$y_lw,
                                                                   data_predictions$a_llm_lw, 
                                                                   data_predictions$mean_llm_lw, 
                                                                   data_predictions$sd_gf_lw))
LogS_Mixnorm_GF_lw  <- mean(trim=1/nrow(test_features), x= mapply(f_LogS_MixNorm,
                                                                  data_predictions$y_lw,
                                                                  data_predictions$a_gf_lw, 
                                                                  data_predictions$mean_gf_lw, 
                                                                  data_predictions$sd_gf_lw))







# MixNorm Komponenten


LogS_Bernoulli_NN_MixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli,
                                                                          data_predictions$x_lw,
                                                                          data_predictions$a_nn_lw 
))

LogS_Bernoulli_GF_MixNorm_lw  <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli,
                                                                           data_predictions$x_lw,
                                                                           data_predictions$a_gf_lw
))
LogS_Bernoulli_LLM_MixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli,
                                                                           data_predictions$x_lw,
                                                                           data_predictions$a_llm_lw
))


LogS_Stetig_NN_MixNorm_lw  <- mean(trim=1/nrow(data_predictions %>%filter(y_lw !=0)), x= mapply(f_LogS_MixNorm_stetig,
                                                                                                data_predictions %>%filter(y_lw !=0) %>%pull(y_lw),
                                                                                                (data_predictions %>%filter(y_lw !=0))$mean_nn_lw,
                                                                                                (data_predictions %>%filter(y_lw !=0))$sd_nn_lw
))

LogS_Stetig_GF_MixNorm_lw <- mean(trim=1/nrow(data_predictions %>%filter(y_lw !=0)), x=mapply(f_LogS_MixNorm_stetig,
                                                                                              data_predictions %>%filter(y_lw !=0) %>%pull(y_lw),
                                                                                              (data_predictions %>%filter(y_lw !=0))$mean_gf_lw,
                                                                                              (data_predictions %>%filter(y_lw !=0))$sd_gf_lw
))

LogS_Stetig_LLM_MixNorm_lw <- mean(trim=1/nrow(data_predictions %>%filter(y_lw !=0)), x=mapply(f_LogS_MixNorm_stetig,
                                                                                               data_predictions %>%filter(y_lw !=0) %>%pull(y_lw),
                                                                                               (data_predictions %>%filter(y_lw !=0))$mean_llm_lw,
                                                                                               (data_predictions %>%filter(y_lw !=0))$sd_gf_lw
))


# Exzesse

# MixGPD
LogS_MixGPD_NN_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_MixGPD,
                                                               data_excess_predictions$excess_lw,
                                                               data_excess_predictions$a_nn_gpd_lw,
                                                               data_excess_predictions$sigmau_nn_gpd_lw,
                                                               data_excess_predictions$xi_gf_lw))

LogS_MixGPD_GF_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_MixGPD,
                                                               data_excess_predictions$excess_lw,
                                                               data_excess_predictions$a_gf_gpd_lw,
                                                               data_excess_predictions$sigmau_gf_gpd_lw,
                                                               data_excess_predictions$xi_gf_lw))

# MixGPD Komponenten
LogS_Bernoulli_NN_MixGPD_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli,
                                                                         data_excess_predictions%>%pull(x_lw),
                                                                         (data_excess_predictions)$a_nn_gpd_lw
))


LogS_Bernoulli_GF_MixGPD_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli,
                                                                         data_excess_predictions$x_lw,
                                                                         data_excess_predictions$a_gf_gpd_lw
))


LogS_Stetig_NN_MixGPD_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_LogS_MixGPD_stetig,
                                                                                                         data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$sigmau_nn_gpd_lw,
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$xi_gf_lw
))

LogS_Stetig_GF_MixGPD_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_LogS_MixGPD_stetig,
                                                                                                         data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$sigmau_gf_gpd_lw,
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$xi_gf_lw
))



# Exzess-MixNorm


LogS_ExzMixNorm_NN_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_MixNorm_Exz,
                                                                   data_excess_predictions$excess_lw,
                                                                   data_excess_predictions$a_nn_lw,
                                                                   data_excess_predictions$mean_nn_lw,
                                                                   data_excess_predictions$sd_nn_lw,
                                                                   10^q90_lw))

LogS_ExzMixNorm_LLM_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_MixNorm_Exz,
                                                                   data_excess_predictions$excess_lw,
                                                                   data_excess_predictions$a_llm_lw,
                                                                   data_excess_predictions$mean_llm_lw,
                                                                   data_excess_predictions$sd_gf_lw,
                                                                   10^q90_lw))

LogS_ExzMixNorm_GF_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_MixNorm_Exz,
                                                                   data_excess_predictions$excess_lw,
                                                                   data_excess_predictions$a_llm_lw,
                                                                   data_excess_predictions$mean_llm_lw,
                                                                   data_excess_predictions$sd_gf_lw,
                                                                   10^q90_lw))


# Exzess-MixNorm Komponenten
LogS_Bernoulli_NN_ExzMixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli_Exz,
                                                                             data_excess_predictions$x_lw,
                                                                             data_excess_predictions$a_nn_lw,
                                                                             data_excess_predictions$mean_nn_lw,
                                                                             data_excess_predictions$sd_nn_lw,
                                                                             10^q90_lw
                                                                             
))

LogS_Bernoulli_LLM_ExzMixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_LogS_Bernoulli_Exz,
                                                                              data_excess_predictions$x_lw,
                                                                              data_excess_predictions$a_llm_lw,
                                                                              data_excess_predictions$mean_llm_lw,
                                                                              data_excess_predictions$sd_gf_lw,
                                                                              10^q90_lw
                                                                              
))

LogS_Stetig_NN_ExzMixNorm_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_LogS_MixNorm_Exz_stetig,
                                                                                                             data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                             (data_excess_predictions %>%filter(excess_lw !=0))$mean_nn_lw,
                                                                                                             (data_excess_predictions %>%filter(excess_lw !=0))$sd_nn_lw,
                                                                                                             10^q90_lw
))
LogS_Stetig_LLM_ExzMixNorm_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_LogS_MixNorm_Exz_stetig,
                                                                                                              data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                              (data_excess_predictions %>%filter(excess_lw !=0))$mean_llm_lw,
                                                                                                              (data_excess_predictions %>%filter(excess_lw !=0))$sd_gf_lw,
                                                                                                              10^q90_lw
))


# Ergebnisse mit Vergleich zum globalen Fit: Je niedriger, desto besser. Werte < 1 deuten auf bessere Vorhersagen als die konstanten Parameter des globalen Fits hin.

logscore_ergebnisse <- tibble(
  Modell = c(
    # MixNormmodelle
    "MixNorm - NN", 
    "Bernoulli - NN",
    "Stetig - NN",
    
    "MixNorm - LLM",
    "Bernoulli - LLM",
    "Stetig - LLM ",
    
    # Exzessmodelle
    "MixGPD - NN",
    "Bernoulli - NN (MixGPD)",
    "Stetig - NN (MixGPD)",

    "ExzMixNorm - NN", 
    "Bernoulli - NN (ExzMixNorm)", 
    "Stetig - NN (ExzMixNorm)",
    
    "ExzMixNorm - LLM",
    "Bernoulli - LLM (ExzMixNorm)",
    "Stetig - LLM (ExzMixNorm)"
  ),
 
  LogS_Wert = c(
    # MixNormmodelle
    LogS_Mixnorm_NN_lw/LogS_Mixnorm_GF_lw,
    LogS_Bernoulli_NN_MixNorm_lw/LogS_Bernoulli_GF_MixNorm_lw,
    LogS_Stetig_NN_MixNorm_lw/LogS_Stetig_GF_MixNorm_lw, 
    
    LogS_Mixnorm_LLM_lw/LogS_Mixnorm_GF_lw,
    LogS_Bernoulli_LLM_MixNorm_lw/LogS_Bernoulli_GF_MixNorm_lw,
    LogS_Stetig_LLM_MixNorm_lw/LogS_Stetig_GF_MixNorm_lw,
    
    # Exzessmodelle
    LogS_MixGPD_NN_lw/ LogS_MixGPD_GF_lw,
    LogS_Bernoulli_NN_MixGPD_lw/LogS_Bernoulli_GF_MixGPD_lw,
    LogS_Stetig_NN_MixGPD_lw/LogS_Stetig_GF_MixGPD_lw,
  
    LogS_ExzMixNorm_NN_lw/LogS_ExzMixNorm_GF_lw,
    LogS_Bernoulli_NN_ExzMixNorm_lw/LogS_Bernoulli_GF_MixGPD_lw, 
    LogS_Stetig_NN_ExzMixNorm_lw/LogS_Stetig_GF_MixGPD_lw,
    
    LogS_ExzMixNorm_LLM_lw/LogS_ExzMixNorm_GF_lw,
    LogS_Bernoulli_LLM_ExzMixNorm_lw/LogS_Bernoulli_GF_MixGPD_lw,
    LogS_Stetig_LLM_ExzMixNorm_lw/LogS_Stetig_GF_MixGPD_lw
  )
)

print(logscore_ergebnisse)

# Ergebnisse LogS und CRPS zusammengefasst
left_join(logscore_ergebnisse, crpscore_ergebnisse, by = "Modell")
