# Erstellung der ALE-Plots mit Paket iml

# VS, Alter, Fläche, HGK, SGK werden in der Reihenfolge sortiert
predictor_lw <- Predictor$new(model=nn_model_lw$model, 
                              data = train_features %>% mutate(VS_kat=factor(VS_kat,ordered=TRUE),
                                     Alter_kat=factor(Alter_kat,ordered=TRUE),
                                     qm_kat=factor(qm_kat,ordered=TRUE),
                                     Jahr=factor(Jahr,ordered = TRUE),
                                     HGK=factor(HGK,ordered = TRUE),
                                     SGK=factor(SGK,ordered = TRUE)),
                           y=train_Y$y_lw)

FI_ALE_Betr_lw<- FeatureEffect$new(predictor_lw, feature="Betr", method="ale")
FI_ALE_Jahr_lw<- FeatureEffect$new(predictor_lw, feature="Jahr", method="ale")
FI_ALE_VS_lw<- FeatureEffect$new(predictor_lw, feature="VS_kat", method="ale")

# Plot Betriebsart
ggplot(data=  FI_ALE_Betr_lw$results %>% filter(.class == 1), aes(x = factor(Betr), y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = NULL, x = NULL, y = NULL) +
  
ggplot(data=   FI_ALE_Betr_lw$results %>% filter(.class == 3) , aes(x = factor(Betr), y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = NULL, x = NULL, y = NULL) +
  
ggplot(data=   FI_ALE_Betr_lw$results %>% filter(.class == 4) , aes(x = factor(Betr), y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = NULL, x = "Betriebsart", y = NULL) +
  plot_layout(ncol=1)


# Plot Jahr
ggplot(data=  FI_ALE_Jahr_lw$results %>% filter(.class == 1), aes(x =Jahr, y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(labels= 2014:2023)+
  labs(title = NULL, x = NULL, y = NULL) +
  
ggplot(data=   FI_ALE_Jahr_lw$results %>% filter(.class == 3) , aes(x = Jahr, y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(labels= 2014:2023)+
  labs(title = NULL, x = NULL, y = NULL) +
  
ggplot(data=   FI_ALE_Jahr_lw$results %>% filter(.class == 4) , aes(x =Jahr, y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(labels= 2014:2023)+
  labs(title = NULL, x = "Jahr", y = NULL) +
plot_layout(ncol=1)

# Plot Versicherungssumme
ggplot(data=  FI_ALE_VS_lw$results %>% filter(.class == 1), aes(x = VS_kat, y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(breaks= seq(0,53, by=5))+
  labs(title = NULL, x = NULL, y = NULL) +
  
ggplot(data=   FI_ALE_VS_lw$results %>% filter(.class == 3) , aes(x = VS_kat, y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(breaks= seq(0,53, by=5))+
  labs(title = NULL, x = NULL, y = NULL) +
  
ggplot(data=   FI_ALE_VS_lw$results %>% filter(.class == 4) , aes(x = VS_kat, y = .value, group=1)) +
  geom_point()+
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = NULL, x = "VS (kateg.)", y = NULL) +
  scale_x_discrete(breaks= seq(0,53, by=5))+
plot_layout(ncol=1)


