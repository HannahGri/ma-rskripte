# pmin() und pmax() sorgen dafür, extrem kleine Dichtewerte zu begrenzen, damit die Verlustwerte keine undefinierten Werte annehmen
loss_orig_nn_lw <- - mean(log(pmin(1-10^-15,pred_params_nn_lw$probs[[1]])*ifelse(test_Y$y_lw==0,1,0)+
                       ifelse(test_Y$y_lw==0,0,1)*(1-pmin(1-10^-15,pred_params_nn_lw$probs[[1]]))*pmax(10^-15,dnorm(test_Y$y_lw, 
                                                                                                        mean = pred_params_nn_lw$dists[[2]]$mean,
                                                                                                        sd=pred_params_nn_lw$dists[[2]]$sd))))
                                                                              
loss_orig_llm_lw<- - mean(log(pmin(1-10^-15,p0_llm_lw)*ifelse(test_Y$y_lw==0,1,0)+
                                ifelse(test_Y$y_lw==0,0,1)*(1-pmin(1-10^-15,p0_llm_lw))*pmax(10^-15,dnorm(test_Y$y_lw, 
                                                                                              mean = mu_llm_lw ,
                                                                                              sd = global_fit_lw$params$dists[[2]]$sd))))



f_pfi_nn_lw <- function(i, n_perm) {
  set.seed(159)
  
  mse_diff_sum<-0
  loss_diff_sum<-0
  
  for (n in 1:n_perm) {
    test_features_perm <-  test_features
    test_features_perm[, i] <- sample(test_features[, i, drop=TRUE]) 
    
    input_test_perm <- list(
      Betr = k_constant(unclass(test_features_perm$Betr)-1L),
      Jahr = k_constant(unclass(test_features_perm$Jahr)),
      HGK = k_constant(unclass(test_features_perm$HGK)-1L),
      SGK = k_constant(unclass(test_features_perm$SGK)-1L),
      Alter_kat = k_constant(unclass(test_features_perm$Alter_kat)),
      qm_kat = k_constant(unclass(test_features_perm$qm_kat)),
      VS_kat = k_constant(unclass(test_features_perm$VS_kat)),
      Vorschaden_feuer= k_constant(unclass(test_features_perm$Vorschaden_feuer)-1L),
      Vorschaden_lw= k_constant(unclass(test_features_perm$Vorschaden_lw)-1L),
      Vorschaden_sturm= k_constant(unclass(test_features_perm$Vorschaden_sturm)-1L),
      Vorschaden_el= k_constant(unclass(test_features_perm$Vorschaden_el)-1L),
      BAKL =k_constant(unclass(test_features_perm$BAKL)-1L),
      AVB =k_constant(unclass(test_features_perm$AVB)-1L),
      UVVZ =k_constant(unclass(test_features_perm$UVVZ)-1L),
      Rohbau =k_constant(unclass(test_features_perm$Rohbau)-1L),
      Feu =k_constant(unclass(test_features_perm$Feu)-1L),
      LW =k_constant(unclass(test_features_perm$LW)-1L),
      ST =k_constant(unclass(test_features_perm$ST)-1L),
      EL =k_constant(unclass(test_features_perm$EL)-1L),
      gew =k_constant(unclass(test_features_perm$gew)-1L)
    )
    #Vorhersagen auf permutierte Testdaten
    pred_params_perm <- predict(nn_model_lw, input_test_perm)

    loss_perm <- - mean(log(pmin(1-10^-15,pred_params_perm$probs[[1]])*ifelse(test_Y$y_lw==0,1,0)+
                                            ifelse(test_Y$y_lw==0,0,1)*(1-pmin(1-10^-15,pred_params_perm$probs[[1]]))*
                                            pmax(10^-15,dnorm(test_Y$y_lw, 
                                                  mean = pred_params_perm$dists[[2]]$mean,
                                                  sd=pred_params_perm$dists[[2]]$sd))))

    loss_diff_sum <- loss_diff_sum + (loss_perm - loss_orig_nn_lw)
  }
 
  mean_loss_diff <- loss_diff_sum / n_perm
  
  
  return(c(Feature = i, mean_loss_diff= mean_loss_diff))
  
}


f_pfi_llm_lw <- function(i, n_perm) {
  set.seed(159)
  
  mse_diff_sum<-0
  loss_diff_sum<-0
  
  #feature_name <- colnames(test_features_bestand_gefahr)[i]
  
  for (n in 1:n_perm) {
    test_features_perm <-  test_features
    test_features_perm[, i] <- sample(test_features[, i, drop=TRUE])  # Feature permutieren
    
    
    #Vorhersagen auf permutierte Testdaten
    pred_p0_perm <- predict(logit_lw, newdata=test_features_perm, type="response")
    mu_perm <-  predict(lm_lw, newdata= as.data.frame(
      cbind( model.matrix(~ . - 1, 
                          data = test_features_perm %>%
                            select(-c(Alter_kat, VS_kat, qm_kat, Jahr))), 
             test_features_perm %>% 
               select(Jahr, VS_kat, Alter_kat, qm_kat))))
    
    loss_perm <- - mean(log(pmin(1-10^-15,pred_p0_perm)*ifelse(test_Y$y_lw==0,1,0)+
                              ifelse(test_Y$y_lw==0,0,1)*(1-pmin(1-10^-15,pred_p0_perm))*
                              pmax(10^-15, dnorm(test_Y$y_lw, 
                                                 mean = mu_perm,
                                                 sd=global_fit_lw$params$dists[[2]]$sd))))
    loss_diff_sum <- loss_diff_sum + (loss_perm - loss_orig_llm_lw)
  }
  
  mean_loss_diff <- loss_diff_sum / n_perm
  
  
  return(c(Feature = i,  mean_loss_diff= (mean_loss_diff)))
  
}


# Berechnung mit 5 Permutationen
PFI_nn_lw <- do.call(rbind,lapply(1:20, f_pfi_nn_lw, n_perm=5))

PFI_llm_lw <- do.call(rbind,lapply(1:20, f_pfi_llm_lw, n_perm=5))

PFI_mixnorm <- left_join(data.frame(PFI_nn_lw), data.frame(PFI_llm_lw), by="Feature") %>% mutate(Feature = names(test_features))

ggplot(PFI_mixnorm  %>%
         pivot_longer(cols = c(mean_loss_diff.x, mean_loss_diff.y), 
                                    names_to = "Modell", values_to = "Wert")%>%
         mutate(Modell = recode(Modell,
                               "mean_loss_diff.x" = "NN",
                               "mean_loss_diff.y" = "LLM"))%>%
         mutate(Feature= factor(Feature,  levels = PFI_mixnorm$Feature[order(PFI_mixnorm$mean_loss_diff.x)]),
                Modell = factor(Modell, levels = c("LLM", "NN"))), aes(x = Feature, y = Wert, fill = Modell)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("NN" = "#3182BD", "LLM" = "#FF6666")) +
  labs(title= "Leitungswasser", x = NULL, y = "Mean loss difference", fill = "Modell") +
  theme_minimal()
