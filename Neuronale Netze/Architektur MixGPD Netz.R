q_num_lw <-quantile(df_data%>% filter(y_lw>0)%>% pull(y_lw), 0.9)
q_num_sturm <-quantile(df_data%>% filter(y_sturm>0)%>% pull(y_sturm), 0.9)
q_num_feuer <-quantile(df_data%>% filter(y_feuer>0)%>% pull(y_feuer), 0.9)
q_num_el <-quantile(df_data%>% filter(y_el>0)%>% pull(y_el), 0.9)


extremes_num <- df_data%>% mutate(excess_feuer=ifelse(y_feuer>q_num_feuer, 10^y_feuer-10^q_num_feuer,0),
                                                excess_lw=ifelse(y_lw>q_num_lw, 10^y_lw-10^q_num_lw,0),
                                                excess_el=ifelse(y_el>q_num_el, 10^y_el-10^q_num_el,0),
                                                excess_sturm=ifelse(y_sturm>q_num_sturm, 10^y_sturm-10^q_num_sturm,0)) %>%
  select(-c(y_feuer, y_lw, y_sturm, y_el))



tf <- import("tensorflow")
set.seed(80)
tf$random$set_seed(80)
py_set_seed(80)

test_data_gpd<- extremes_num %>%
  group_by(Jahr,qm_kat, VS_kat) %>%
  sample_frac(0.2) %>%
  ungroup()

test_features_gpd<- test_data_gpd%>% select(-c(excess_feuer,excess_lw,excess_el,excess_sturm,SchNr))
test_Y_gpd<- test_data_gpd%>% select(c(excess_feuer,excess_lw,excess_el,excess_sturm))


train_data_gpd<- extremes_num %>% anti_join(test_data_gpd) 

train_features_gpd<- train_data_gpd%>% select(-c(excess_feuer,excess_lw,excess_el,excess_sturm, SchNr))
train_Y_gpd<- train_data_gpd%>% select(c(excess_feuer,excess_lw,excess_el,excess_sturm)) 


fit_gpd_lw <-fit(dist_genpareto1(u=0),train_data_gpd%>%filter(excess_lw>0)%>% pull(excess_lw) )
fit_gpd_sturm <-fit(dist_genpareto1(u=0),train_data_gpd%>%filter(excess_sturm>0)%>% pull(excess_sturm) )
fit_gpd_feuer <-fit(dist_genpareto1(u=0),train_data_gpd%>%filter(excess_feuer>0)%>% pull(excess_feuer) )
fit_gpd_el <-fit(dist_genpareto1(u=0),train_data_gpd%>%filter(excess_el>0)%>% pull(excess_el) )


dist_dir_gpd_lw<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_lw$params$xi)))
dist_dir_gpd_sturm<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_sturm$params$xi)))
dist_dir_gpd_feuer<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_feuer$params$xi)))
dist_dir_gpd_el<-dist_mixture(dists = list(dist_dirac(0), dist_genpareto1(u=0, sigma=NULL, xi=fit_gpd_el$params$xi)))

global_fit_mixgpd_lw <- fit_dist(dist_dir_gpd_lw, train_Y_gpd$excess_lw)
global_fit_mixgpd_sturm <- fit_dist(dist_dir_gpd_sturm, train_Y_gpd$excess_sturm)
global_fit_mixgpd_feuer <- fit_dist(dist_dir_gpd_feuer, train_Y_gpd$excess_feuer)
global_fit_mixgpd_el <- fit_dist(dist_dir_gpd_el, train_Y_gpd$excess_el)

input_train_mixgpd <- list(
  Betr = k_constant(unclass(train_features_gpd$Betr)-1L),
  Jahr = k_constant(unclass(train_features_gpd$Jahr)),
  HGK = k_constant(unclass(train_features_gpd$HGK)-1L),
  SGK = k_constant(unclass(train_features_gpd$SGK)-1L),
  Alter_kat = k_constant(unclass(train_features_gpd$Alter_kat)),
  qm_kat = k_constant(unclass(train_features_gpd$qm_kat)),
  VS_kat = k_constant(unclass(train_features_gpd$VS_kat)),
  Vorschaden_feuer= k_constant(unclass(train_features_gpd$Vorschaden_feuer)-1L),
  Vorschaden_lw= k_constant(unclass(train_features_gpd$Vorschaden_lw)-1L),
  Vorschaden_sturm= k_constant(unclass(train_features_gpd$Vorschaden_sturm)-1L),
  Vorschaden_el= k_constant(unclass(train_features_gpd$Vorschaden_el)-1L),
  BAKL =k_constant(unclass(train_features_gpd$BAKL)-1L),
  AVB =k_constant(unclass(train_features_gpd$AVB)-1L),
  UVVZ =k_constant(unclass(train_features_gpd$UVVZ)-1L),
  Rohbau =k_constant(unclass(train_features_gpd$Rohbau)-1L),
  Feu =k_constant(unclass(train_features_gpd$Feu)-1L),
  LW =k_constant(unclass(train_features_gpd$LW)-1L),
  ST =k_constant(unclass(train_features_gpd$ST)-1L),
  EL =k_constant(unclass(train_features_gpd$EL)-1L),
  gew =k_constant(unclass(train_features_gpd$gew)-1L)
)

input_test_mixgpd <- list(
  Betr = k_constant(unclass(test_features_gpd$Betr)-1L),
  Jahr = k_constant(unclass(test_features_gpd$Jahr)),
  HGK = k_constant(unclass(test_features_gpd$HGK)-1L),
  SGK = k_constant(unclass(test_features_gpd$SGK)-1L),
  Alter_kat = k_constant(unclass(test_features_gpd$Alter_kat)),
  qm_kat = k_constant(unclass(test_features_gpd$qm_kat)),
  VS_kat = k_constant(unclass(test_features_gpd$VS_kat)),
  Vorschaden_feuer= k_constant(unclass(test_features_gpd$Vorschaden_feuer)-1L),
  Vorschaden_lw= k_constant(unclass(test_features_gpd$Vorschaden_lw)-1L),
  Vorschaden_sturm= k_constant(unclass(test_features_gpd$Vorschaden_sturm)-1L),
  Vorschaden_el= k_constant(unclass(test_features_gpd$Vorschaden_el)-1L),
  BAKL =k_constant(unclass(test_features_gpd$BAKL)-1L),
  AVB =k_constant(unclass(test_features_gpd$AVB)-1L),
  UVVZ =k_constant(unclass(test_features_gpd$UVVZ)-1L),
  Rohbau =k_constant(unclass(test_features_gpd$Rohbau)-1L),
  Feu =k_constant(unclass(test_features_gpd$Feu)-1L),
  LW =k_constant(unclass(test_features_gpd$LW)-1L),
  ST =k_constant(unclass(test_features_gpd$ST)-1L),
  EL =k_constant(unclass(test_features_gpd$EL)-1L),
  gew =k_constant(unclass(test_features_gpd$gew)-1L)
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
  layer_embedding(inputs$Betr, input_dim = nlevels(df_data$Betr), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Jahr, input_dim = max(df_data$Jahr)+1, output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$HGK, input_dim = nlevels(df_data$HGK), output_dim = 1) %>%   
    layer_flatten(),   
  layer_embedding(inputs$SGK, input_dim = nlevels(df_data$SGK), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$Alter_kat, input_dim = max(df_data$Alter_kat)+1, output_dim = 1) %>%   
    layer_flatten(),   
  layer_embedding(inputs$VS_kat, input_dim = max(df_data$VS_kat)+1, output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$qm_kat, input_dim = max(df_data$qm_kat)+1, output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_feuer, input_dim = nlevels(df_data$Vorschaden_feuer), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_lw, input_dim = nlevels(df_data$Vorschaden_lw), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_sturm, input_dim = nlevels(df_data$Vorschaden_sturm), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$Vorschaden_el, input_dim = nlevels(df_data$Vorschaden_el), output_dim = 1) %>%  
    layer_flatten(),
  layer_embedding(inputs$BAKL, input_dim = nlevels(df_data$BAKL), output_dim = 1) %>%  
    layer_flatten(),   
  layer_embedding(inputs$AVB, input_dim = nlevels(df_data$AVB), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$UVVZ, input_dim = nlevels(df_data$UVVZ), output_dim = 1) %>%  
    layer_flatten(),   
  layer_embedding(inputs$Rohbau, input_dim = nlevels(df_data$Rohbau), output_dim = 1) %>%   
    layer_flatten(),
  layer_embedding(inputs$Feu, input_dim = nlevels(df_data$Feu), output_dim = 1) %>%   
    layer_flatten(),   
  layer_embedding(inputs$LW, input_dim = nlevels(df_data$LW), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$ST, input_dim = nlevels(df_data$ST), output_dim = 1) %>%  
    layer_flatten(),   
  layer_embedding(inputs$EL, input_dim = nlevels(df_data$EL), output_dim = 1) %>% 
    layer_flatten(),
  layer_embedding(inputs$gew, input_dim = nlevels(df_data$gew), output_dim = 1) %>%  
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
                                y = k_matrix(as_trunc_obs(as.numeric(unlist(train_Y_gpd$excess_lw)))),
                                epochs = 600L,
                                batch_size = 11563L,
                                shuffle = FALSE,
                                validation_split=0.2,
                                view_metrics=FALSE)

plot_fit_nn_mixgpd_lw  <- plot(fit_nn_mixgpd_lw)

pred_params_pareto_lw_num_lr1000 <- predict(nn_pareto_lw_num, input_test_mixgpd)

