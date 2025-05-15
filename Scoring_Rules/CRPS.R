
# Integralform für ExzessMixNorm
f_CRPS_MixNorm_Exz<- function(y, a, mean, sd, quantile) {
  if (quantile<1) {
    stop(" quantile sollte >=1 sein")
  }
  if (a < 0 || a > 1) {
    stop(" a muss im Intervall [0, 1] liegen.")
  }
  
  a=pmin(1-10^-15,a)
  
  if (y < 0) {
    stop(" y muss >= 0 sein.") 
    
  } else if (y == 0) {
    term2 <- function(x) (a+(1-a)*pnorm(log10(quantile+x), mean=mean, sd=sd)-1)^2
    int2 <-unlist(integrate(term2, lower=0, upper=Inf, stop.on.error =FALSE))
    return(int2$value)
    
  }  else if (y > 0) {
    term1 <- function(x) (a+(1-a)*pnorm(log10(quantile+x), mean=mean, sd=sd))^2
    term2 <- function(x) (a+(1-a)*pnorm(log10(quantile+x), mean=mean, sd=sd)-1)^2
    int1 <-unlist(integrate(term1, lower=0, upper=y, stop.on.error =FALSE))
    int2 <-unlist(integrate(term2, lower=y, upper=Inf,  stop.on.error =FALSE))
    
    return( int1$value+ int2$value) 
  } 
}

f_CRPS_MixNorm_Exz_stetig <- function(y, mean, sd, quantile) {
  if (quantile<1) {
    stop(" quantile sollte >=1 sein")
  }
  
  
  if (y <= 0) {
    stop(" y muss > 0 sein.") 
    
  }  else if (y > 0) {
    term1 <- function(x) ((pnorm(log10(quantile+x), mean=mean, sd=sd)-pnorm(log10(quantile), mean=mean, sd=sd))/(1-pnorm(log10(quantile), mean=mean, sd=sd)))^2
    term2 <- function(x) ((pnorm(log10(quantile+x), mean=mean, sd=sd)-pnorm(log10(quantile), mean=mean, sd=sd))/(1-pnorm(log10(quantile), mean=mean, sd=sd))-1)^2
    int1 <-unlist(integrate(term1, lower=0, upper=y, stop.on.error =FALSE))
    int2 <-unlist(integrate(term2, lower=y, upper=Inf,  stop.on.error =FALSE))
    
    return( int1$value+ int2$value) 
  } 
}


# geschlossene Formeln für CRPS

# MixNorm
f_CRPS_MixNorm<-function(y, a, mean, sd) {
  a*abs(y) +(1-a)*sd*( (y-mean)/sd * (2*pnorm((y-mean)/sd)-1) + 2* dnorm((y-mean)/sd) )-
    a*(1-a)* ( 2*sd*dnorm(mean/sd) + mean*(2*pnorm(mean/sd)-1) )-
    (1-a)^2*sd / sqrt(pi)
}

f_CRPS_MixNorm_stetig <-function(y, mean, sd) { #CRPS für Normalverteilung
  u=(y-mean)/sd
  return(sd*(u*(2* pnorm(u,mean=0, sd=1) -1) + 2*dnorm(u, mean=0, sd=1) - 1/sqrt(pi)))
}

# MixGPD
f_CRPS_MixGPD_stetig<-function(y, sigma, xi) { #M = 0
  
  return(sigma*(abs(y)/sigma - 2/(1-xi) *(1-(1-reservr::pgpd(y/sigma, u=0, sigmau=1, xi=xi))^(1-xi))+1/(2-xi)))
}

f_CRPS_MixGPD <- function(y, a, sigma, xi) {
  return(sigma*(abs(y)/sigma - 2*(1-a)/(1-xi) *(1-(1-reservr::pgpd(y/sigma, u=0, sigmau=1, xi=xi))^(1-xi))+(1-a)^2/(2-xi)))
}

# Bernoulli
f_CRPS_Bernoulli<-function(y,a) {
  a*y + (1-y)*(1-a) - a*(1-a)
}

f_CRPS_Bernoulli_MixNorm_Exz <-function(y,a,mean, sd, u) {
  (a + (1-a)*pnorm(log10(u), mean=mean, sd=sd))*y + 
    (1-y)*(1-(a + (1-a)*pnorm(log10(u), mean=mean, sd=sd))) -
    (a + (1-a)*pnorm(log10(u), mean=mean, sd=sd))*(1-(a + (1-a)*pnorm(log10(u), mean=mean, sd=sd)))
}



#Beispiel mit Gefahr Leitungswasser 

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
CRPS_Mixnorm_NN_lw <- mean(trim=1/nrow(data_predictions), x= mapply(f_CRPS_MixNorm,
                                                                    data_predictions$y_lw,
                                                                    data_predictions$a_nn_lw, 
                                                                    data_predictions$mean_nn_lw, 
                                                                    data_predictions$sd_nn_lw))
CRPS_Mixnorm_LLM_lw  <- mean(trim=1/nrow(test_features), x= mapply(f_CRPS_MixNorm,
                                                                   data_predictions$y_lw,
                                                                   data_predictions$a_llm_lw, 
                                                                   data_predictions$mean_llm_lw, 
                                                                   data_predictions$sd_gf_lw))
CRPS_Mixnorm_GF_lw  <- mean(trim=1/nrow(test_features), x= mapply(f_CRPS_MixNorm,
                                                                  data_predictions$y_lw,
                                                                  data_predictions$a_gf_lw, 
                                                                  data_predictions$mean_gf_lw, 
                                                                  data_predictions$sd_gf_lw))







# MixNorm Komponenten


CRPS_Bernoulli_NN_MixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli,
                                                                          data_predictions$x_lw,
                                                                          data_predictions$a_nn_lw 
))

CRPS_Bernoulli_GF_MixNorm_lw  <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli,
                                                                           data_predictions$x_lw,
                                                                           data_predictions$a_gf_lw
))
CRPS_Bernoulli_LLM_MixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli,
                                                                           data_predictions$x_lw,
                                                                           data_predictions$a_llm_lw
))


CRPS_Stetig_NN_MixNorm_lw  <- mean(trim=1/nrow(data_predictions %>%filter(y_lw !=0)), x= mapply(f_CRPS_MixNorm_stetig,
                                                                                                data_predictions %>%filter(y_lw !=0) %>%pull(y_lw),
                                                                                                (data_predictions %>%filter(y_lw !=0))$mean_nn_lw,
                                                                                                (data_predictions %>%filter(y_lw !=0))$sd_nn_lw
))

CRPS_Stetig_GF_MixNorm_lw <- mean(trim=1/nrow(data_predictions %>%filter(y_lw !=0)), x=mapply(f_CRPS_MixNorm_stetig,
                                                                                              data_predictions %>%filter(y_lw !=0) %>%pull(y_lw),
                                                                                              (data_predictions %>%filter(y_lw !=0))$mean_gf_lw,
                                                                                              (data_predictions %>%filter(y_lw !=0))$sd_gf_lw
))

CRPS_Stetig_LLM_MixNorm_lw <- mean(trim=1/nrow(data_predictions %>%filter(y_lw !=0)), x=mapply(f_CRPS_MixNorm_stetig,
                                                                                               data_predictions %>%filter(y_lw !=0) %>%pull(y_lw),
                                                                                               (data_predictions %>%filter(y_lw !=0))$mean_llm_lw,
                                                                                               (data_predictions %>%filter(y_lw !=0))$sd_gf_lw
))


# Exzesse

# MixGPD
CRPS_MixGPD_NN_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_MixGPD,
                                                               data_excess_predictions$excess_lw,
                                                               data_excess_predictions$a_nn_gpd_lw,
                                                               data_excess_predictions$sigmau_nn_gpd_lw,
                                                               data_excess_predictions$xi_gf_lw))

CRPS_MixGPD_GF_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_MixGPD,
                                                               data_excess_predictions$excess_lw,
                                                               data_excess_predictions$a_gf_gpd_lw,
                                                               data_excess_predictions$sigmau_gf_gpd_lw,
                                                               data_excess_predictions$xi_gf_lw))

# MixGPD Komponenten
CRPS_Bernoulli_NN_MixGPD_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli,
                                                                         data_excess_predictions%>%pull(x_lw),
                                                                         (data_excess_predictions)$a_nn_gpd_lw
))


CRPS_Bernoulli_GF_MixGPD_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli,
                                                                         data_excess_predictions$x_lw,
                                                                         data_excess_predictions$a_gf_gpd_lw
))


CRPS_Stetig_NN_MixGPD_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_CRPS_MixGPD_stetig,
                                                                                                         data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$sigmau_nn_gpd_lw,
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$xi_gf_lw
))

CRPS_Stetig_GF_MixGPD_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_CRPS_MixGPD_stetig,
                                                                                                         data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$sigmau_gf_gpd_lw,
                                                                                                         (data_excess_predictions %>%filter(excess_lw !=0))$xi_gf_lw
))



# Exzess-MixNorm


CRPS_ExzMixNorm_NN_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_MixNorm_Exz,
                                                                   data_excess_predictions$excess_lw,
                                                                   data_excess_predictions$a_nn_lw,
                                                                   data_excess_predictions$mean_nn_lw,
                                                                   data_excess_predictions$sd_nn_lw,
                                                                   10^q90_lw))

CRPS_ExzMixNorm_GF_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_MixNorm_Exz,
                                                                   data_excess_predictions$excess_lw,
                                                                   data_excess_predictions$a_llm_lw,
                                                                   data_excess_predictions$mean_llm_lw,
                                                                   data_excess_predictions$sd_gf_lw,
                                                                   10^q90_lw))


# Exzess-MixNorm Komponenten
CRPS_Bernoulli_NN_ExzMixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli_MixNorm_Exz,
                                                                             data_excess_predictions$x_lw,
                                                                             data_excess_predictions$a_nn_lw,
                                                                             data_excess_predictions$mean_nn_lw,
                                                                             data_excess_predictions$sd_nn_lw,
                                                                             10^q90_lw
                                                                             
))

CRPS_Bernoulli_LLM_ExzMixNorm_lw <- mean(trim=1/nrow(test_features), x=mapply(f_CRPS_Bernoulli_MixNorm_Exz,
                                                                              data_excess_predictions$x_lw,
                                                                              data_excess_predictions$a_llm_lw,
                                                                              data_excess_predictions$mean_llm_lw,
                                                                              data_excess_predictions$sd_gf_lw,
                                                                              10^q90_lw
                                                                              
))

CRPS_Stetig_NN_ExzMixNorm_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_CRPS_MixNorm_Exz_stetig,
                                                                                                             data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                             (data_excess_predictions %>%filter(excess_lw !=0))$mean_nn_lw,
                                                                                                             (data_excess_predictions %>%filter(excess_lw !=0))$sd_nn_lw,
                                                                                                             10^q90_lw
))
CRPS_Stetig_LLM_ExzMixNorm_lw <- mean(trim=1/nrow(data_excess_predictions %>%filter(excess_lw !=0)), x=mapply(f_CRPS_MixNorm_Exz_stetig,
                                                                                                              data_excess_predictions %>%filter(excess_lw !=0) %>%pull(excess_lw),
                                                                                                              (data_excess_predictions %>%filter(excess_lw !=0))$mean_llm_lw,
                                                                                                              (data_excess_predictions %>%filter(excess_lw !=0))$sd_gf_lw,
                                                                                                              10^q90_lw
))
