
library(iml)

# VS, Alter, Fläche, HGK, SGK werden in der Reihenfolge sortiert

predictor_lw <- Predictor$new(model=nn_model_lw$model, 
                              data = train_features %>%mutate(VS_kat=factor(VS_kat,ordered=TRUE),
                                     Alter_kat=factor(Alter_kat,ordered=TRUE),
                                     qm_kat=factor(qm_kat,ordered=TRUE),
                                     HGK=factor(HGK,ordered = TRUE),
                                     SGK=factor(SGK,ordered = TRUE)),
                           y=train_Y$y_lw)


FI_ALE_Jahr_lw<- FeatureEffect$new(predictor_lw, feature="Jahr", method="ale")
FI_ALE_VS_lw<- FeatureEffect$new(predictor_lw, feature="VS_kat", method="ale")

# Plot 1: Jahr

ggplot(data=FI_ALE_Jahr_lw$results%>% filter(.class==1))+
  geom_col(aes(x=(factor(Jahr) 
  ),
  y= .value), fill="skyblue3",alpha=0.6)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(name=NULL ,breaks=2014:2023)+
  labs(y=NULL,
       title=expression(alpha))+
ggplot(data=FI_ALE_Jahr_lw$results%>% filter(.class==3))+
  geom_col(aes(x=factor(Jahr 
  ),
  y= .value), fill="skyblue3", alpha=0.6)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(name=NULL ,breaks=2014:2023)+
  labs( y=NULL,
        title= expression(mu))+
ggplot(data=FI_ALE_Jahr_lw$results%>% filter(.class==4))+
  geom_col(aes(x=factor(Jahr 
  ),
  y= .value), fill="skyblue3", alpha=0.6)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(name="Jahr" ,breaks=2014:2023)+
  labs( y=NULL,
        title= expression(sigma))+
  plot_layout(ncol=1)

# Plot 2: Verischerungssumme (kategorisiert)

ggplot(data=FI_ALE_VS_lw$results%>% filter(.class==1))+
  geom_col(aes(x=(factor(VS_kat) 
  ),
  y= .value), fill="skyblue3",alpha=0.6)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(name=NULL ,breaks=c(seq(0,55, by=5)))+
  labs(x="", y="ALE",
       title=expression(alpha))+
ggplot(data=FI_ALE_VS_lw$results%>% filter(.class==3))+
  geom_col(aes(x=(VS_kat 
  ),
  y= .value), fill="skyblue3", alpha=0.6)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(name=NULL ,breaks=c(seq(0,55, by=5)))+
  labs(x="", y="ALE",
       title= expression(mu))+
ggplot(data=FI_ALE_VS_lw$results%>% filter(.class==4))+
  geom_col(aes(x=(VS_kat 
  ),
  y= .value), fill="skyblue3", alpha=0.6)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(name="Versicherungssumme (kateg.)", breaks=c(seq(0,55, by=5)))+
  labs(x=NULL, , y="ALE",
       title= expression(sigma))+
  plot_layout(ncol=1)

