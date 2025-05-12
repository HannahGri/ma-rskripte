
df_data <- Verknüpfung_Bestand_gefahren%>%
  select(
    Jahr=F002_Jahr,
    HGK =ZUERS_HGK,
    SGK =ZUERS_SGK,
    Betr = F103_Betriebsart,
    Alter_kat = Alter_kat,
    VS_kat = VS_kat,
    qm_kat = qm_kat,
    BAKL = F044_BAKL_Priv,
    AVB = F050_AVB_VG,
    UVVZ = F054_UVVZ,
    Rohbau = F183b_Rohbau,
    Feu = F030_Feuer,
    LW = F032_LW,
    ST = F033_ST,
    EL = F035_EL,
    gew = F077_gew_Nutzung,
    Vorschaden_feuer=Vorschaden_feuer,
    Vorschaden_lw=Vorschaden_lw,
    Vorschaden_sturm=Vorschaden_sturm,
    Vorschaden_el=Vorschaden_el,
    Vorschaden_sonst=Vorschaden_sonst,
    y_feuer = y_feuer,
    y_lw = y_lw,
    y_sturm = y_sturm,
    y_el = y_el,
    y_sonst = y_sonst
  )  %>% mutate(
    across(-c(y_feuer, y_lw, y_sturm, y_el, y_sonst,
              VS_kat,Alter_kat, qm_kat, Jahr), as.factor),
    y_feuer= ifelse(y_feuer<=1, 0L, log10(y_feuer)),
    y_lw= ifelse(y_lw<=1, 0L, log10(y_lw)),
    y_sturm= ifelse(y_sturm<=1, 0L, log10(y_sturm)),
    y_el= ifelse(y_el<=1, 0L, log10(y_el)),
    y_sonst= ifelse(y_sonst<=1, 0L, log10(y_sonst))) %>%
  mutate(SchNr = (1:nrow(Verknüpfung_Bestand)),
         Alter_kat = ifelse(is.na(Alter_kat), 0, Alter_kat)) %>%
  filter(!is.na(UVVZ)) %>%
  select(-y_sonst, -Vorschaden_sonst)


# Aufteilung in Test-& Trainingsdaten
tf <- import("tensorflow")
set.seed(80)
tf$random$set_seed(80)
py_set_seed(80)

test_data<- df_data%>%
  group_by(Jahr,qm_kat, VS_kat ) %>%
  sample_frac(0.2) %>%
  ungroup()

test_features<- test_data%>% select(-c(y_feuer,
                                       y_lw,
                                       y_sturm, y_el,
                                       SchNr))

test_Y<- test_data%>% select(y_feuer, y_lw, y_sturm, y_el) 

train_data<- df_data%>% anti_join(test_data) 

train_features<- train_data%>% select(-c(y_feuer,
                                         y_lw,
                                         y_sturm, y_el,
                                         SchNr))
train_Y<- train_data%>% select(y_feuer, y_lw, y_sturm, y_el) 


# Mischverteilung definieren
dist_dir_normal <- dist_mixture(dists = list(dist_dirac(0),
                                             dist_normal(mean=NULL, sd=NULL)),
                                probs = list(NULL,NULL))

# globaler fit der Parameter alpha, mean, sd
global_fit_lw <- fit_dist(dist_dir_normal, train_Y$y_lw)
global_fit_sturm <- fit_dist(dist_dir_normal, train_Y$y_sturm)
global_fit_feuer <- fit_dist(dist_dir_normal, train_Y$y_feuer)
global_fit_el <- fit_dist(dist_dir_normal, train_Y$y_el)



# Inputs vorbereiten für das neuronale Netz
input_train <- list(
  Betr = k_constant(unclass(train_features$Betr)-1L),
  Jahr = k_constant(train_features$Jahr),
  HGK = k_constant(unclass(train_features$HGK)-1L),
  SGK = k_constant(unclass(train_features$SGK)-1L),
  Alter_kat = k_constant(train_features$Alter_kat),
  qm_kat = k_constant(train_features$qm_kat),
  VS_kat = k_constant(train_features$VS_kat),
  Vorschaden_feuer= k_constant(unclass(train_features$Vorschaden_feuer)-1L),
  Vorschaden_lw= k_constant(unclass(train_features$Vorschaden_lw)-1L),
  Vorschaden_sturm= k_constant(unclass(train_features$Vorschaden_sturm)-1L),
  Vorschaden_el= k_constant(unclass(train_features$Vorschaden_el)-1L),
  BAKL =k_constant(unclass(train_features$BAKL)-1L),
  AVB =k_constant(unclass(train_features$AVB)-1L),
  UVVZ =k_constant(unclass(train_features$UVVZ)-1L),
  Rohbau =k_constant(unclass(train_features$Rohbau)-1L),
  Feu =k_constant(unclass(train_features$Feu)-1L),
  LW =k_constant(unclass(train_features$LW)-1L),
  ST =k_constant(unclass(train_features$ST)-1L),
  EL =k_constant(unclass(train_features$EL)-1L),
  gew =k_constant(unclass(train_features$gew)-1L)
)

input_test <- list(
  Betr = k_constant(unclass(test_features$Betr)-1L),
  Jahr = k_constant(test_features$Jahr),
  HGK = k_constant(unclass(test_features$HGK)-1L),
  SGK = k_constant(unclass(test_features$SGK)-1L),
  Alter_kat = k_constant(test_features$Alter_kat),
  qm_kat = k_constant(test_features$qm_kat),
  VS_kat = k_constant(test_features$VS_kat),
  Vorschaden_feuer= k_constant(unclass(test_features$Vorschaden_feuer)-1L),
  Vorschaden_lw= k_constant(unclass(test_features$Vorschaden_lw)-1L),
  Vorschaden_sturm= k_constant(unclass(test_features$Vorschaden_sturm)-1L),
  Vorschaden_el= k_constant(unclass(test_features$Vorschaden_el)-1L),
  BAKL =k_constant(unclass(test_features$BAKL)-1L),
  AVB =k_constant(unclass(test_features$AVB)-1L),
  UVVZ =k_constant(unclass(test_features$UVVZ)-1L),
  Rohbau =k_constant(unclass(test_features$Rohbau)-1L),
  Feu =k_constant(unclass(test_features$Feu)-1L),
  LW =k_constant(unclass(test_features$LW)-1L),
  ST =k_constant(unclass(test_features$ST)-1L),
  EL =k_constant(unclass(test_features$EL)-1L),
  gew =k_constant(unclass(test_features$gew)-1L)
)


# Struktur des neuronalen Netzes für Leitungswasserschäden

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
  layer_embedding(inputs$Jahr, input_dim =  max(df_data$Jahr)+1, output_dim = 1) %>%  
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

# Schichten & Neuronen, Aktivierungsfunktion (ReLU) und Dropout festlegen
nn_output <- layer_concatenate(embedding) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 4, name = "parameter_output")


# Lernrate und Optimierer festlegen
boundaries <- c(150*100,300*100) 
values <- c(5*10^-4, 10^-5, 10^-6) 

lr_schedule <- learning_rate_schedule_piecewise_constant_decay(
  boundaries, values)

optimizer <- optimizer_adam(learning_rate=lr_schedule)


# kompilieren
nn_model_lw <- tf_compile_model(
  inputs = inputs,
  intermediate_output = nn_output,
  dist = dist_dir_normal,
  optimizer = optimizer,
  censoring = FALSE, 
  truncation = FALSE
)

# Training
fit_nn_lw <- fit(nn_model_lw$model,
                               x = input_train,
                               y = k_matrix(as_trunc_obs(as.numeric(unlist(train_Y$y_lw)))),
                               epochs = 600L,
                               batch_size = 11563L,
                               shuffle = FALSE,
                               validation_split=0.2,
                               view_metrics=FALSE)

plot_fit_nn_lw <- plot(fit_nn_lw)

# Parametervorhersagen auf Testdaten
pred_params_nn_lw <- predict(nn_model_lw, input_test)


