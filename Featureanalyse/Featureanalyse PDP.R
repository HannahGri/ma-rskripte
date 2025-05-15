
f_pd <- function(model, data, feature) {

  x <- data[[feature]]
  feature_levels <- sort(unique(x))
  
  pdp_values <- lapply(feature_levels, function(c) {
    
    newdata <- data
    
    # das gewählte Feature erhält die konstante Ausprägung x für alle Datensätze
    newdata[[feature]] <- c
    
    # Input für das neuronale Netz anpassen
    input <- list(
      Betr = k_constant(unclass(newdata$Betr)-1L),
      Jahr = k_constant(unclass(newdata$Jahr)),
      HGK = k_constant(unclass(newdata$HGK)-1L),
      SGK = k_constant(unclass(newdata$SGK)-1L),
      Alter_kat = k_constant(unclass(newdata$Alter_kat)),
      qm_kat = k_constant(unclass(newdata$qm_kat)),
      VS_kat = k_constant(unclass(newdata$VS_kat)),
      Vorschaden_feuer = k_constant(unclass(newdata$Vorschaden_feuer)-1L),
      Vorschaden_lw = k_constant(unclass(newdata$Vorschaden_lw)-1L),
      Vorschaden_sturm = k_constant(unclass(newdata$Vorschaden_sturm)-1L),
      Vorschaden_el = k_constant(unclass(newdata$Vorschaden_el)-1L),
      BAKL = k_constant(unclass(newdata$BAKL)-1L),
      AVB = k_constant(unclass(newdata$AVB)-1L),
      UVVZ = k_constant(unclass(newdata$UVVZ)-1L),
      Rohbau = k_constant(unclass(newdata$Rohbau)-1L),
      Feu = k_constant(unclass(newdata$Feu)-1L),
      LW = k_constant(unclass(newdata$LW)-1L),
      ST = k_constant(unclass(newdata$ST)-1L),
      EL = k_constant(unclass(newdata$EL)-1L),
      gew = k_constant(unclass(newdata$gew)-1L)
    )
    

    pred_params_new <- predict(model, input)
    
    # Vorhersagen für neuen Datensatz, für alle möglichen Ausprägungen x
    c(mean(pred_params_new$probs[[1]]), #a
      mean(pred_params_new$dists[[2]]$mean), #mu
      mean(pred_params_new$dists[[2]]$sd)) #sigma
    
  })
  
  pdp_matrix <- do.call(rbind, pdp_values)
  
  pdp_data <- data.frame(
    feature_value = feature_levels,
    a = pdp_matrix[, 1],
    mean = pdp_matrix[, 2],
    sd = pdp_matrix[, 3]
  )
  
  return(pdp_data)
}




pd_betr_lw <- f_pd(nn_model_lw, train_features, feature="Betr")

pd_Jahr_lw <- f_pd(nn_model_lw, train_features, feature="Jahr")

pd_VS_lw <- f_pd(nn_model_lw, train_features, feature="VS_kat")



