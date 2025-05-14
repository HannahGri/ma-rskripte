#data_sim wie im MixNorm-Netz

q90_lw <-quantile(data_sim%>% filter(y_lw>0)%>% pull(y_lw), 0.9)
q90_sturm <-quantile(data_sim%>% filter(y_sturm>0)%>% pull(y_sturm), 0.9)
q90_feuer <-quantile(data_sim%>% filter(y_feuer>0)%>% pull(y_feuer), 0.9)
q90_el <-quantile(data_sim%>% filter(y_el>0)%>% pull(y_el), 0.9)


data_extremes <- data_sim%>% mutate(excess_feuer=ifelse(y_feuer>q90_feuer, 10^y_feuer-10^q90_feuer,0),
                                                excess_lw=ifelse(y_lw>q90_lw, 10^y_lw-10^q90_lw,0),
                                                excess_el=ifelse(y_el>q90_el, 10^y_el-10^q90_el,0),
                                                excess_sturm=ifelse(y_sturm>q90_sturm, 10^y_sturm-10^q90_sturm,0)) %>%
  select(-c(y_feuer, y_lw, y_sturm, y_el))



set.seed(80)
tf$random$set_seed(80)
py_set_seed(80)

test_data_extremes<- data_extremes %>%
  group_by(Jahr,qm_kat, VS_kat) %>%
  sample_frac(0.2) %>%
  ungroup()

test_features_extremes<- test_data_extremes%>% select(-c(excess_feuer,excess_lw,excess_el,excess_sturm,SchNr))
test_Y_extremes<- test_data_extremes%>% select(c(excess_feuer,excess_lw,excess_el,excess_sturm))


train_data_extremes<- data_extremes %>% anti_join(test_data_extremes) 

train_features_extremes<- train_data_extremes%>% select(-c(excess_feuer,excess_lw,excess_el,excess_sturm, SchNr))
train_Y_extremes<- train_data_extremes%>% select(c(excess_feuer,excess_lw,excess_el,excess_sturm)) 

# globaler fit der GPD-Verteilung, um xi festzulegen
fit_gpd_lw <-fit(dist_genpareto1(u=0),train_data_extremes%>%filter(excess_lw>0)%>% pull(excess_lw) )
fit_gpd_sturm <-fit(dist_genpareto1(u=0),train_data_extremes%>%filter(excess_sturm>0)%>% pull(excess_sturm) )
fit_gpd_feuer <-fit(dist_genpareto1(u=0),train_data_extremes%>%filter(excess_feuer>0)%>% pull(excess_feuer) )
fit_gpd_el <-fit(dist_genpareto1(u=0),train_data_extremes%>%filter(excess_el>0)%>% pull(excess_el) )

#Mischverteilung mit konstantem xi definieren
dist_dir_gpd_lw<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_lw$params$xi)))
dist_dir_gpd_sturm<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_sturm$params$xi)))
dist_dir_gpd_feuer<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_feuer$params$xi)))
dist_dir_gpd_el<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_el$params$xi)))

global_fit_mixgpd_lw <- fit_dist(dist_dir_gpd_lw, train_Y_extremes$excess_lw)
global_fit_mixgpd_sturm <- fit_dist(dist_dir_gpd_sturm, train_Y_extremes$excess_sturm)
global_fit_mixgpd_feuer <- fit_dist(dist_dir_gpd_feuer, train_Y_extremes$excess_feuer)
global_fit_mixgpd_el <- fit_dist(dist_dir_gpd_el, train_Y_extremes$excess_el)

input_train_mixgpd <- list(
  Betr = k_constant(unclass(train_features_extremes$Betr)-1L),
  Jahr = k_constant(unclass(train_features_extremes$Jahr)),
  HGK = k_constant(unclass(train_features_extremes$HGK)-1L),
  SGK = k_constant(unclass(train_features_extremes$SGK)-1L),
  Alter_kat = k_constant(unclass(train_features_extremes$Alter_kat)),
  qm_kat = k_constant(unclass(train_features_extremes$qm_kat)),
  VS_kat = k_constant(unclass(train_features_extremes$VS_kat)),
  Vorschaden_feuer= k_constant(unclass(train_features_extremes$Vorschaden_feuer)-1L),
  Vorschaden_lw= k_constant(unclass(train_features_extremes$Vorschaden_lw)-1L),
  Vorschaden_sturm= k_constant(unclass(train_features_extremes$Vorschaden_sturm)-1L),
  Vorschaden_el= k_constant(unclass(train_features_extremes$Vorschaden_el)-1L),
  BAKL =k_constant(unclass(train_features_extremes$BAKL)-1L),
  AVB =k_constant(unclass(train_features_extremes$AVB)-1L),
  UVVZ =k_constant(unclass(train_features_extremes$UVVZ)-1L),
  Rohbau =k_constant(unclass(train_features_extremes$Rohbau)-1L),
  Feu =k_constant(unclass(train_features_extremes$Feu)-1L),
  LW =k_constant(unclass(train_features_extremes$LW)-1L),
  ST =k_constant(unclass(train_features_extremes$ST)-1L),
  EL =k_constant(unclass(train_features_extremes$EL)-1L),
  gew =k_constant(unclass(train_features_extremes$gew)-1L)
)

input_test_mixgpd <- list(
  Betr = k_constant(unclass(test_features_extremes$Betr)-1L),
  Jahr = k_constant(unclass(test_features_extremes$Jahr)),
  HGK = k_constant(unclass(test_features_extremes$HGK)-1L),
  SGK = k_constant(unclass(test_features_extremes$SGK)-1L),
  Alter_kat = k_constant(unclass(test_features_extremes$Alter_kat)),
  qm_kat = k_constant(unclass(test_features_extremes$qm_kat)),
  VS_kat = k_constant(unclass(test_features_extremes$VS_kat)),
  Vorschaden_feuer= k_constant(unclass(test_features_extremes$Vorschaden_feuer)-1L),
  Vorschaden_lw= k_constant(unclass(test_features_extremes$Vorschaden_lw)-1L),
  Vorschaden_sturm= k_constant(unclass(test_features_extremes$Vorschaden_sturm)-1L),
  Vorschaden_el= k_constant(unclass(test_features_extremes$Vorschaden_el)-1L),
  BAKL =k_constant(unclass(test_features_extremes$BAKL)-1L),
  AVB =k_constant(unclass(test_features_extremes$AVB)-1L),
  UVVZ =k_constant(unclass(test_features_extremes$UVVZ)-1L),
  Rohbau =k_constant(unclass(test_features_extremes$Rohbau)-1L),
  Feu =k_constant(unclass(test_features_extremes$Feu)-1L),
  LW =k_constant(unclass(test_features_extremes$LW)-1L),
  ST =k_constant(unclass(test_features_extremes$ST)-1L),
  EL =k_constant(unclass(test_features_extremes$EL)-1L),
  gew =k_constant(unclass(test_features_extremes$gew)-1L)
)




set.seed(11)
tf$random$set_seed(11)
py_set_seed(11)


inputs <- list(
  Betr = layer_input(name = "Betr", dtype = "int32", shape = 1L),
  Jahr = layer_input(name = "Jahr", dtype = "int32", shape = 1L),
  HGK = layer_input(name = "HGK", dtype = "int32", shape = 1L),
  SGK = layer_input(name = "SGK", dtype = "int32", shape = 1L),
  Alter_kat = layer_input(name = "Alter_kat", dtype = "int32", shape = 1L),
  qm_kat = layer_input(name = "qm_kat", dtype = "int32", shape = 1L),  
  VS_kat = layer_input(name = "VS_kat", dtype = "int32", shape = 1L),
  Vorschaden_feuer = layer_input(name = "Vorschaden_feuer", dtype = "int32", shape = 1L),
  Vorschaden_lw = layer_input(name = "Vorschaden_lw", dtype = "int32", shape = 1L),
  Vorschaden_sturm = layer_input(name = "Vorschaden_sturm", dtype = "int32", shape = 1L),
  Vorschaden_el = layer_input(name = "Vorschaden_el", dtype = "int32", shape = 1L),
  BAKL = layer_input(name = "BAKL", dtype = "int32", shape = 1L),
  AVB = layer_input(name = "AVB", dtype = "int32", shape = 1L),
  UVVZ = layer_input(name = "UVVZ", dtype = "int32", shape = 1L),
  Rohbau = layer_input(name = "Rohbau", dtype = "int32", shape = 1L),
  Feu = layer_input(name = "Feu", dtype = "int32", shape = 1L),
  LW = layer_input(name = "LW", dtype = "int32", shape = 1L),
  ST = layer_input(name = "ST", dtype = "int32", shape = 1L),
  EL = layer_input(name = "EL", dtype = "int32", shape = 1L),
  gew = layer_input(name = "gew", dtype = "int32", shape = 1L)
)


embedding <- list(
  layer_embedding(inputs$Betr, input_dim = nlevels(data_extremes$Betr), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Jahr, input_dim = max(data_extremes$Jahr)+1, output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$HGK, input_dim = nlevels(data_extremes$HGK), output_dim = 1) %>%   
    layer_flatten(),   
  layer_embedding(inputs$SGK, input_dim = nlevels(data_extremes$SGK), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$Alter_kat, input_dim = max(data_extremes$Alter_kat)+1, output_dim = 1) %>%   
    layer_flatten(),   
  layer_embedding(inputs$VS_kat, input_dim = max(data_extremes$VS_kat)+1, output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$qm_kat, input_dim = max(data_extremes$qm_kat)+1, output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_feuer, input_dim = nlevels(data_extremes$Vorschaden_feuer), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_lw, input_dim = nlevels(data_extremes$Vorschaden_lw), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_sturm, input_dim = nlevels(data_extremes$Vorschaden_sturm), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_el, input_dim = nlevels(data_extremes$Vorschaden_el), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$BAKL, input_dim = nlevels(data_extremes$BAKL), output_dim = 1) %>%  
    layer_flatten(),   
  layer_embedding(inputs$AVB, input_dim = nlevels(data_extremes$AVB), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$UVVZ, input_dim = nlevels(data_extremes$UVVZ), output_dim = 1) %>%  
    layer_flatten(),   
  layer_embedding(inputs$Rohbau, input_dim = nlevels(data_extremes$Rohbau), output_dim = 1) %>%   
    layer_flatten(),
  layer_embedding(inputs$Feu, input_dim = nlevels(data_extremes$Feu), output_dim = 1) %>%   
    layer_flatten(),   
  layer_embedding(inputs$LW, input_dim = nlevels(data_extremes$LW), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$ST, input_dim = nlevels(data_extremes$ST), output_dim = 1) %>%  
    layer_flatten(),   
  layer_embedding(inputs$EL, input_dim = nlevels(data_extremes$EL), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$gew, input_dim = nlevels(data_extremes$gew), output_dim = 1) %>%  
    layer_flatten()
)




combined <- layer_concatenate(embedding) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 3, name = "parameter_output")


nn_output <- combined

optimizer <- optimizer_adam(learning_rate=5*10^-4)

nn_mixgpd_model_lw  <- tf_compile_model(
  inputs = inputs,
  intermediate_output = nn_output,
  dist = dist_dir_gpd_lw,
  optimizer = optimizer,
  censoring = FALSE, 
  truncation = FALSE
)


fit_nn_mixgpd_lw  <- fit(nn_pareto_lw_num$model,
                                x = input_train_mixgpd,
                                y = k_matrix(as_trunc_obs(as.numeric(unlist(train_Y_extremes$excess_lw)))),
                                epochs = 10L,
                                batch_size = round(nrow(train_features_extremes)/100),
                                shuffle = FALSE,
                                validation_split=0.2,
                                view_metrics=FALSE)

plot_fit_nn_mixgpd_lw  <- plot(fit_nn_mixgpd_lw)

pred_params_pareto_lw_num_lr1000 <- predict(nn_pareto_lw_num, input_test_mixgpd)

