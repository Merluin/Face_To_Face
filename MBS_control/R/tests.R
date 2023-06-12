# test


# Caricamento dei pacchetti
library(cluster)

# Creazione del dataset di esempio
# Supponiamo di avere un dataset con tre variabili categoriali (V1, V2, V3)
data <- data.frame(
  V1 = factor(c("A", "B", "A", "B", "C")),
  V2 = factor(c("X", "Y", "X", "Y", "Z")),
  V3 = factor(c("M", "N", "N", "M", "N"))
)

# Calcolo della matrice di dissimilarità utilizzando la distanza di Gower
diss_matrix <- daisy(data, metric = "gower")

# Esecuzione del clustering gerarchico agglomerativo completo
clustering <- agnes(diss_matrix, method = "complete")

# Visualizzazione dei risultati con un dendrogramma
plot(clustering, which.plots = 2)

