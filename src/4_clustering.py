### Clustering ###

# Librerias
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
import seaborn as sns
from sklearn.cluster import KMeans
from yellowbrick.cluster import KElbowVisualizer

# Lectura
estaciones = pd.read_csv("./data/3_clean/estacionhorapc.csv")

estaciones.head()

# Features
X_all = estaciones.drop(["destino_id"], axis = 1)

model = KMeans()
visualizer = KElbowVisualizer(model, k = (3,9), timings = False, locate_elbow = True)

visualizer.fit(X_all)        # Fit the data to the visualizer
visualizer.show()        # Finalize and render the figure

# Modelo
model = KMeans(n_clusters = 7)
model.fit(X_all)

## Clusters
print(model.labels_)

## Reordenar clusters
y_hat = np.choose(model.labels_, [0, 1, 2, 3, 5, 6, 7]).astype(np.int64)

## Resultados
resultados = model.predict(X_all)
resultados

## Se agregan a los datos originales
estaciones["cluster"] = list(resultados)
estaciones.head(10)

# Escritura
estaciones.to_csv("./data/3_clean/estaciones_clusters.csv", index = False)
