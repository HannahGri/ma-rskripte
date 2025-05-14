# LLM (Logistisch-Lineares Modell) als Vergleichsmodell, trainiert auf denselben Trainingsdaten wie das neuronale Netz

# Schadenaufwände binär kodieren für logistische Regression
logit_data <- train_data %>% mutate(x_feuer = ifelse(y_feuer==0,1,0),
                                   x_lw = ifelse(y_lw==0,1,0),
                                   x_sturm = ifelse(y_sturm==0,1,0),
                                   x_el = ifelse(y_el==0,1,0))%>%
  select(-c(Nr,y_feuer, y_lw, y_sturm, y_el ))


# Leitungswasser

# Dummy-Codierung der nominalen Kategorien mit model.matrix
# Lineare Regression für positive Aufwände
lm_lw <- lm(y_lw~.,
                      data =as.data.frame(
                        cbind( model.matrix(~ . - 1, 
                                            data = train_data %>% 
                                              filter(y_lw > 0) %>%  
                                              select(-c(Alter_kat, VS_kat, qm_kat, Jahr,Nr,y_feuer, y_sturm, y_el))), 
                               train_data %>% 
                                 filter(y_lw > 0) %>% select(Jahr, VS_kat, Alter_kat, qm_kat))))

# Logistische Regression für Null-Aufwände
logit_lw <-glm(x_lw ~.,
                      data=logit_data%>%
                        select(-c(x_feuer, x_sturm, x_el)),
                      family=binomial)

#andere Gefahren analog
# Sturm

lm_sturm <- lm(y_sturm~.,
                         data =as.data.frame(
                           cbind( model.matrix(~ . - 1, 
                                               data = train_data %>% 
                                                 filter(y_sturm > 0) %>%  
                                                 select(-c(Alter_kat, VS_kat, qm_kat, Jahr,Nr,y_feuer, y_lw, y_el))), 
                                  train_data %>% 
                                    filter(y_sturm > 0) %>% select(Jahr, VS_kat, Alter_kat, qm_kat))))



logit_sturm <-glm(x_sturm ~.,
                         data=logit_data%>%
                           select(-c(x_feuer, x_lw, x_el)),
                         family=binomial)


# Feuer


lm_feuer<- lm(y_feuer~.,
              data =as.data.frame(
                cbind( model.matrix(~ . - 1, 
                                    data = train_data %>% 
                                      filter(y_feuer > 0) %>%  
                                      select(-c(Alter_kat, VS_kat, qm_kat, Jahr,Nr, y_lw, y_sturm, y_el))), 
                       train_data %>% 
                         filter(y_feuer > 0) %>% select(Jahr, VS_kat, Alter_kat, qm_kat))))


logit_feuer <-glm(x_feuer ~.,
                  data=logit_data%>%
                    select(-c(x_lw, x_sturm, x_el)),
                  family=binomial)
# Elementar

lm_el <- lm(y_el~.,
                      data =as.data.frame(
                        cbind( model.matrix(~ . - 1, 
                                            data = train_data %>% 
                                              filter(y_el > 0) %>%  
                                              select(-c(Alter_kat, VS_kat, qm_kat, Jahr,Nr,y_feuer, y_sturm, y_lw))), 
                               train_data %>% 
                                 filter(y_el > 0) %>% select(Jahr, VS_kat, Alter_kat, qm_kat))))


logit_el <-glm(x_el ~.,
                      data=logit_data%>%
                        select(-c(x_feuer, x_sturm, x_lw)),
                      family=binomial)

# Vorhersagen

mu_llm_lw <-  predict(lm_lw, newdata= as.data.frame(
  cbind( model.matrix(~ . - 1, 
                      data = test_features %>%
                        select(-c(Alter_kat, VS_kat, qm_kat, Jahr))), 
         test_features %>% 
           select(Jahr, VS_kat, Alter_kat, qm_kat))))

p0_llm_lw <- predict(logit_lw,
                        newdata=test_features,
                        type="response")

mu_llm_sturm <-  predict(lm_sturm, newdata= as.data.frame(
  cbind( model.matrix(~ . - 1, 
                      data = test_features %>%
                        select(-c(Alter_kat, VS_kat, qm_kat, Jahr))), 
         test_features %>% 
           select(Jahr, VS_kat, Alter_kat, qm_kat))))

p0_llm_sturm <- predict(logit_sturm,
                        newdata=test_features,
                        type="response")

mu_llm_feuer <-  predict(lm_feuer, newdata= as.data.frame(
  cbind( model.matrix(~ . - 1, 
                      data = test_features %>%
                        select(-c(Alter_kat, VS_kat, qm_kat, Jahr))), 
         test_features %>% 
           select(Jahr, VS_kat, Alter_kat, qm_kat))))

p0_llm_feuer <- predict(logit_feuer,
                              newdata=test_features,
                              type="response")


mu_llm_el <-  predict(lm_el, newdata= as.data.frame(
  cbind( model.matrix(~ . - 1, 
                      data = test_features %>%
                        select(-c(Alter_kat, VS_kat, qm_kat, Jahr))), 
         test_features %>% 
           select(Jahr, VS_kat, Alter_kat, qm_kat))))

p0_llm_el <- predict(logit_el,
                        newdata=test_features,
                        type="response")
