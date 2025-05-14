# ein Datensatz, der ähnliche Verteilungen aufweist, aber Abhängigkeiten zwischen Variablen nicht berücksichtigt
n_sample <- 100000

set.seed(111)
Jahr_sim <- sample(2014:2023 , size = n_sample, replace = TRUE, prob = as.numeric(prop.table(table(df_data$Jahr))))
HGK_sim <- sample(0:4 , size = n_sample, replace = TRUE, prob = c(0.04, 0.87, 0.07,0.01, 0.01))
SGK_sim <- sample(0:3 , size = n_sample, replace = TRUE, prob = c(0.31, 0.16, 0.46, 0.07))
Betr_sim <- sample(c(00000, 00051, 00052, 00053, 00055, 00059) , size = n_sample, replace = TRUE, prob = c(0.002, 0.03, 0.03, 0.8, 0.128, 0.01))
Alter_kat_sim <- sample(0:61 , size = n_sample, replace = TRUE, prob = dnorm(0:61, mean = 0, sd=15) / sum(dnorm(0:61, mean = 0, sd=20)) )
VS_kat_sim <- sample(0:57 , size = n_sample, replace = TRUE, prob = dnorm(0:57, mean = 23, sd = 7)/ sum(dnorm(0:57, mean = 23, sd = 7)))
qm_kat_sim <- sample(0:50 , size = n_sample, replace = TRUE, prob = c(0.45, 0.65* dnorm(1:50, mean = 6, sd = 2)/sum(dnorm(1:50, mean = 6, sd = 2))))
BAKL_sim <- sample(0:3 , size = n_sample, replace = TRUE, prob = c(0.01, 0.95, 0.03, 0.01))
AVB_sim <- sample(c(00, 01, 02, 03, 06, 12, 14, 15, 98) , size = n_sample, replace = TRUE, prob =c(0.0002, 0.004, 0.0008, 0.005, 0.24, 0.3, 0.16, 0.03, 0.26))
UVVZ_sim <- sample(c(1,2) , size = n_sample, replace = TRUE, prob =c(0.3, 0.7))
Rohbau_sim <- sample(c(1,2) , size = n_sample, replace = TRUE, prob = c(0.003, 0.997))
LW_sim <- sample(c(1,2), size = n_sample, replace = TRUE, prob = c(0.95, 0.05))
Feu_sim <- sample(c(1,2) , size = n_sample, replace = TRUE, prob = c(0.96, 0.04))
ST_sim <- sample(c(1,2) , size = n_sample, replace = TRUE, prob = c(0.97, 0.03))
EL_sim <- sample(c(1,2,3,5) , size = n_sample, replace = TRUE, prob = c(0.24, 0.51, 0.248, 0.002))
gew_sim <- sample(c(1,2,9) , size = n_sample, replace = TRUE, prob = c(0.02, 0.001, 0.979))
Vorschaden_lw_sim <- sample(c(0,1) , size = n_sample, replace = TRUE, prob = c(0.9, 0.1))
Vorschaden_feuer_sim <- sample(c(0,1) , size = n_sample, replace = TRUE, prob = c(0.98, 0.02))
Vorschaden_sturm_sim <- sample(c(0,1) , size = n_sample, replace = TRUE, prob = c(0.9, 0.1))
Vorschaden_el_sim <- sample(c(0,1) , size = n_sample, replace = TRUE, prob = c( 0.99, 0.01))

y_lw_sim <-  dist_mixture(dists = list(dist_dirac(0),
                                       dist_normal(mean= 3.2, sd=0.57)),
                          probs = list(0.958,0.042))$sample(n_sample)

y_sturm_sim <-  dist_mixture(dists = list(dist_dirac(0),
                                       dist_normal(mean= 2.9, sd=0.51)),
                          probs = list(0.965,0.035))$sample(n_sample)
y_feuer_sim <-  dist_mixture(dists = list(dist_dirac(0),
                                       dist_normal(mean= 3.13, sd=0.76)),
                          probs = list(0.995,0.005))$sample(n_sample)
y_el_sim <-  dist_mixture(dists = list(dist_dirac(0),
                                          dist_normal(mean= 3.66, sd=0.7)),
                             probs = list(0.997,0.003))$sample(n_sample)



data_sim <- tibble(
  Jahr = Jahr_sim,
  HGK = HGK_sim,
  SGK = SGK_sim,
  Betr = Betr_sim,
  Alter_kat = Alter_kat_sim,
  VS_kat = VS_kat_sim,
  qm_kat = qm_kat_sim,
  BAKL = BAKL_sim,
  AVB = AVB_sim,
  UVVZ = UVVZ_sim,
  Rohbau = Rohbau_sim,
  LW = LW_sim,
  Feu = Feu_sim,
  ST = ST_sim,
  EL = EL_sim,
  gew = gew_sim,
  Vorschaden_lw = Vorschaden_lw_sim,
  Vorschaden_sturm = Vorschaden_sturm_sim,
  Vorschaden_feuer = Vorschaden_feuer_sim,
  Vorschaden_el = Vorschaden_el_sim,
  y_lw = y_lw_sim,
  y_sturm = y_sturm_sim,
  y_feuer = y_feuer_sim,
  y_el = y_el_sim
)  %>% mutate(
  across(-c(y_feuer, y_lw, y_sturm, y_el,
            VS_kat,Alter_kat, qm_kat, Jahr), as.factor)) %>%
  mutate(Nr = (1:n_sample))

  


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
                                       Nr))

test_Y<- test_data%>% select(y_feuer, y_lw, y_sturm, y_el) 

train_data<- df_data%>% anti_join(test_data) 

train_features<- train_data%>% select(-c(y_feuer,
                                         y_lw,
                                         y_sturm, y_el,
                                         Nr))
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

# Training: hier wird die Zielvariable definiert, ersetze y_lw mit den anderen Zielgrößen, um die anderen Gefahrenmodelle zu erhalten
fit_nn_lw <- fit(nn_model_lw$model,
                               x = input_train,
                               y = k_matrix(as_trunc_obs(as.numeric(unlist(train_Y$y_lw)))),
                               epochs = 600L,
                               batch_size = round(nrow(train_features)/100),
                               shuffle = FALSE,
                               validation_split=0.2,
                               view_metrics=FALSE)
# Verlauf der Trainingsverluste
plot_fit_nn_lw <- plot(fit_nn_lw)

# Parametervorhersagen auf Testdaten
pred_params_nn_lw <- predict(nn_model_lw, input_test)


