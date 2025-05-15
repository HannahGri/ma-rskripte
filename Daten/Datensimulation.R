
# Abhängigkeiten zwischen Variablen (inklusive Zielgröße) werden bei der Simulation nicht erzeugt

# simulierte Datengröße
n_sample <- 100000

set.seed(111)
Jahr_sim <- sample(2014:2023 , size = n_sample, replace = TRUE, prob = seq(0.07, 0.16, length.out=10)/sum(seq(0.07, 0.16, length.out=10)))
HGK_sim <- sample(0:4 , size = n_sample, replace = TRUE, prob = c(0.04, 0.87, 0.07,0.01, 0.01))
SGK_sim <- sample(0:3 , size = n_sample, replace = TRUE, prob = c(0.31, 0.16, 0.46, 0.07))
Betr_sim <- sample(c(00000, 00051, 00052, 00053, 00055, 00059) , size = n_sample, replace = TRUE, prob = c(0.002, 0.03, 0.03, 0.8, 0.128, 0.01))
Alter_kat_sim <- sample(0:61 , size = n_sample, replace = TRUE, prob = dnorm(0:61, mean = 0, sd=15) / sum(dnorm(0:61, mean = 0, sd=20)))
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


# Daten zusammenführen
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
  mutate(Nr = (1:n_sample)) # Nummer zuordnen, damit jeder Datensatz als eindeutig aufgefasst wird

  

# Aufteilung in Test- und Trainingsdaten (20 - 80) 
set.seed(80)
tf$random$set_seed(80)
py_set_seed(80)

test_data<- data_sim%>%
  group_by(Jahr,qm_kat, VS_kat ) %>%
  sample_frac(0.2) %>%
  ungroup()

test_features<- test_data%>% select(-c(y_feuer,
                                       y_lw,
                                       y_sturm, y_el,
                                       Nr))

test_Y<- test_data%>% select(y_feuer, y_lw, y_sturm, y_el) 

train_data<- data_sim%>% anti_join(test_data) 

train_features<- train_data%>% select(-c(y_feuer,
                                         y_lw,
                                         y_sturm, y_el,
                                         Nr))
train_Y<- train_data%>% select(y_feuer, y_lw, y_sturm, y_el) 
