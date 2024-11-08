### Llamando las librerias

pacman::p_load(tidyverse, ggfortify)

### Creacion de Funcion para el Analisis PCA

PCA <- function(data, colID, ...) {
  # Asignamos valor a la data para trabajar internamente
  data_pca <- data %>% column_to_rownames(var = colID)
  # Asignamos valor a las correlaciones
  correlation <- cor(data_pca) %>% round(2)
  # Asignamos valor a los p-values de las correlaciones
  cor_p.values <- matrix(NA, nrow = nrow(correlation), ncol = ncol(correlation),
                         dimnames = list(names(data_pca), names(data_pca)))
  # Creamos la matriz de p-values
  for (i in 1:ncol(correlation)) {
    for (j in 1:nrow(correlation)) {
      cor_p.values[i, j] <- cor.test(data_pca[, i], data_pca[, j])$p.value
      cor_p.values[i, j] <- round(cor_p.values[i, j], 3)
    }
  }
  # Hacemos el ACP, en los proximos comandos vamos a extraer las informaciones de interes
  pca <- prcomp(data_pca, scale = T)
  # Aqui creamos una tabla que contenga los autovalores y la proporcion de varianza explicada
  eig.var <- data.frame(Eigenvalues = pca$sdev^2) %>%
    mutate(Factor = row_number(), `Percent. of Variance` = Eigenvalues /sum(Eigenvalues), 
           `Cumulative Variance` = cumsum(`Percent. of Variance`)) %>% 
    mutate(Factor = paste('Factor ', Factor, sep = '')) %>%
    column_to_rownames('Factor') %>%
    mutate_at(c(1:3), ~format(round(., 4), scientific = F)) %>%
    mutate_at(c(1:3), ~as.double(.))
  
  # Grafico sedimentacion
  gsedim <- ggplot(eig.var, aes(x = seq(1:nrow(eig.var)),
                                , y = Eigenvalues)) +
    geom_line(color = 'blue', linetype = 'solid', lwd = .8) +
    geom_point(color = 'blue', fill = 'white', size = 3, shape = 21, stroke = 1) +
    labs(title = 'Gráfico de Sedimentación', x = 'Factor', y = 'Eigenvalue') +
    scale_x_continuous(breaks = seq(1, nrow(eig.var), by = 1)) +
    scale_y_continuous(breaks = seq(0, max(round(eig.var$Eigenvalues))+1, by = 1)) +
    theme_test() +
    theme(plot.title = element_text(face = 'bold', hjust = .5))
  
  # Grafico biplot
  gbiplot <- autoplot(pca, data = data_pca, loadings = T, loadings.label = T) + theme_minimal() +
    geom_text(aes(label = rownames(data_pca)), hjust = 0, vjust = -.5) +
    geom_hline(yintercept = 0, linetype = 'dashed') + geom_vline(xintercept = 0, linetype = 'dashed') +
    stat_ellipse(type = 'norm', level = 0.99) +
    labs(title = 'PCA: Coordinates') +
    theme(plot.title = element_text(face = 'bold', hjust = .5))
  
  # Lo siguiente sera lo que contenga la salida
  output <- list(
    'Correlaciones (Coeficiente)' = correlation,
    'Correlaciones (Valores - P)' = cor_p.values,
    'Eigen-Variance' = eig.var,
    'Elementos PCA' = pca,
    'Contribuciones al Factor' = format(round(pca$rotation^2, 4), scientific = F),
    'View Sedim' = gsedim,
    'View Biplot' = gbiplot
  )
  # Le asignamos una clase
  class(output) <- 'PCA'
  # Fin
  return(output)
}

### Definir el formato de salida de la funcion

print.PCA <- function(pca, ...) {
  print(pca$`View Sedim`)
  print(pca$`View Biplot`)
  
  # Asignamos un valor auxiliar para reducir el texto del bucle
  cont <- as.data.frame(pca$`Contribuciones al Factor`) %>% 
    arrange(desc(PC1), desc(PC2), desc(PC3))
  
  cat('Principal Component Analysis (PCA)\n')
  cat('\n')
  cat('Eigenvalues:\n')
  print(
    pca$`Eigen-Variance`
  )
  cat('\n')
  cat('Contributions to the Factor:\n')
  cat('\n')
  print(cont)
}

### Definir el formato summary de la funcion

summary.PCA <- function(pca, ...) {
  cat('PCA Summary\n')
  cat('\n')
  cat('Factors: ', nrow(pca$`Eigen-Variance`), '\n')
  cat('Individuals: ', nrow(pca$`Contribuciones al Factor`), '\n')
  cat('Eigenvalues > 1: ', nrow(filter(pca$`Eigen-Variance`, Eigenvalues >= 1)))
}
