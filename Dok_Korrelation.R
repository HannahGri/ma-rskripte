
library(rcompanion)

data_features <- data_sim %>% select(-c(y_lw, y_el, y_sturm,y_feuer, Nr))
n_features <- ncol(data_features)



# Funktion, die eine Kreuztabelle zweier Features erstellt und anschließend die Korrelation gemäß Cramers v berechnet
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  cramerV(tbl)
}

# leere Matrix erstellen mit Dimensionen der Featureanzahl
cramer_matr <- matrix(NA, ncol= length(colnames(data_features)),
                      nrow=length(colnames(data_features)), 
                      dimnames = list(colnames(data_features),
                                      colnames(data_features)))

# Korrelationsmatrix für alle möglichen Feature-Kombinationen 
for (i in 1:n_features) {
  for (j in 1:n_features) {
    cramer_matr[i, j] <- ifelse(i==j, 1,cramers_v(data_features[[i]], data_features[[j]])
    )
  }
}

ggplot(as.data.frame(as.table(cramer_matr)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), 
            data = as.data.frame(as.table(cramer_matr)),
            size = 2.5, color = "gray35") +
  scale_fill_gradientn(
    colours = c("#fff5f0", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15", "#67000d"),
    limits = c(0, 1),
    name = "Korrelation"
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()
  )

