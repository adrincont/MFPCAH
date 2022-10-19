# ============== [Aplicación de MFDA a datos hibridos] =======================.#
# {Librerias} =============================================================.####
library(car)
library(ggplot2)
library(MASS)
library(igraph)
library(network)
library(sna)
library(patchwork)
library(pbapply)
library(reshape2)
library(viridis)
library(fda.usc)
library(fda)
library(MFPCA)
library(e1071)
# {Funciones } ============================================================.####
# Grafica para datos funcionales
plot_func = function(FunData, name_x = 'x', name_y = 'y', class = NULL){
  plt = list()
  fig = list()
  if (class(FunData) == 'funData') {FunData = list(FunData)}
  if (!is.null(class)) {class = data.frame(id = 1:length(class),class)}
  for (i in 1:length(FunData)) {
    X = FunData[[i]]@X
    colnames(X) = FunData[[i]]@argvals[[1]]
    rownames(X) = seq(1:nrow(X))
    plt[[i]] = as.data.frame.table(X)
    colnames(plt[[i]]) = c('id', 'x', 'y')
    if (!is.null(class)) {
      plt[[i]] = merge(plt[[i]],class,by = 'id',all.y = TRUE)
    }
    plt[[i]]$x = as.numeric(as.character(plt[[i]]$x))
    fig[[i]] = ggplot(plt[[i]], aes(x, y, fill = id)) + theme_bw() +
      xlab(name_x) + ylab(paste(name_y, '(V', i, ')'))
    if (is.null(class)){
      fig[[i]] = fig[[i]] + geom_line(aes(color = id), show.legend = FALSE)
    }else{
      fig[[i]] = fig[[i]] + geom_line(aes(color = class))
    }
  }
  if (length(fig) > 1) {return(Reduce(`+`, fig))}
  else {return(fig[[1]])}
}
# Escalar rango de los datos funcionales
MinMax = function(x){
  x_min = min(x)
  x_max = max(x)
  val_i = (x - x_min)/(x_max - x_min)
  return(list(args = c(min = x_min, max = x_max),val = val_i))
}
# Graficas de la varianza explicada
plot_varexp = function(values){
  val_i = values/sum(values)
  text_lab = paste0(round(val_i*100,1), '%')
  perf = data.frame(i = seq(1, length(val_i)), val_i, text_lab)
  ggp = ggplot(perf)  +
    geom_bar(aes(x = i, y = val_i), stat = 'identity', fill = '#17B978') +
    geom_line(aes(x = i, y = val_i), stat = 'identity') +
    geom_point(aes(x = i, y = val_i)) +
    labs(x = 'i' ,y = 'Valor propio') +
    geom_text(aes(x = i, y = val_i), label = text_lab, vjust = -0.4, hjust = 0) +
    theme_bw()
  return(ggp)
}
# Funciones para SVM
plot_SVM = function(svm_model, ...){
  # Rango de los predictores
  rango_X1 = range(datos_svm$x1)
  rango_X2 = range(datos_svm$x2)
  # Interpolación de puntos
  new_x1 = seq(from = rango_X1[1], to = rango_X1[2], length = 75)
  new_x2 = seq(from = rango_X2[1], to = rango_X2[2], length = 75)
  nuevos_puntos = expand.grid(x1 = new_x1, x2 = new_x2)
  # Predicción según el modelo de los nuevos puntos
  predicciones = predict(object = svm_model, newdata = nuevos_puntos)
  # Se almacenan los puntos predichos para el color
  color_regiones = data.frame(nuevos_puntos, y = predicciones)
  # Grafico
  plot_i = ggplot(data = NULL) +
    geom_point(data = color_regiones, aes(x = x1, y = x2, color = as.factor(y)),
               size = 0.5) +
    # Se añaden las observaciones
    geom_point(data = datos_train, aes(x = x1, y = x2, color = as.factor(y)),
               size = 2.5) +
    # Se identifican aquellas observaciones que son vectores soporte
    geom_point(data = datos_train[svm_model$index, ],
               aes(x = x1, y = x2, color = as.factor(y)),
               shape = 21, colour = "black",
               size = 2.5) +
    theme_bw() + theme(legend.position = "none")
  return(plot_i)
}
test_SVM = function(svm_model, test_data){
  predicciones = predict(object = svm_model, test_data[,c('x1','x2')])
  y = test_data$y
  cm = table(predicción = predicciones, valor_real = y)
  print(paste0('Accuracy: ', round(sum(diag(cm))/sum(cm)*100,2), '%'))
}
# {Codigo} ================================================================.####
## (Lectura de los datos tecator) -----------------------------------------.####
data('tecator')
clase = ifelse(tecator$y$Fat < 20, 'm20 %', 'M20 %')
datos = funData(tecator$absorp.fdata$argvals, tecator$absorp.fdata$data)
### Primera derivada de los datos
datos_fd = funData2fd(datos)
datos_fd_0 = fdata.deriv(datos_fd,nderiv = 0)
datos_fd_1 = fdata.deriv(datos_fd,nderiv = 1)
datos_0 = funData(datos_fd_0$argvals,datos_fd_0$data)
datos_1 = funData(datos_fd_1$argvals,datos_fd_1$data)
multy_fda = multiFunData(list(datos_0,datos_1))
plot_func(multy_fda, name_x = 'Wavelength', name_y = 'Absorbances',class = clase)
## (FAPC) -----------------------------------------------------------------.####
### Escalar rango de x
scale_x = list()
for (i in 1:length(multy_fda)) {
  min_max_i = MinMax(multy_fda[[i]]@argvals[[1]])
  scale_x[[i]] = min_max_i$args
  multy_fda[[i]]@argvals[[1]] = min_max_i$val
  multy_fda[[i]]@X = multy_fda[[i]]@X/max(multy_fda[[i]]@X)
}
### MPCA funcional basado en bsplines para las funciones media y covarianza
uniExpansions_r = list()
for (i in 1:2) {
  uniExpansions_r[[i]] = list(type = 'uFPCA')
}
mfda_r1 = MFPCA(multy_fda, M = 5,uniExpansions = uniExpansions_r)
plot_varexp(mfda_r1$values)
mfda_r = MFPCA(multy_fda, M = 2,uniExpansions = uniExpansions_r)
scores_pca = mfda_r$scores
scores_r = data.frame(scores_pca, id = 1:nrow(scores_pca))
colnames(scores_r) = c('PC1', 'PC2', 'id')
score_plot = ggplot(scores_r,aes(PC1, PC2)) +
  geom_vline(xintercept = 0, color = 'blue') +
  geom_hline(yintercept = 0,  color = 'blue') + theme_bw() +
  geom_point() + ggtitle('MFPCAH') +
  geom_text(aes(label = id), vjust = -0.4, hjust = 0,size = 3)
score_plot + plot_varexp(mfda_r1$values)
## (SVM) ------------------------------------------------------------------.####
### preparamos los datos
datos_svm = data.frame(x1 = scores_pca[,1], x2 = scores_pca[,2], y = clase)
datos_svm$y = as.numeric(as.factor(datos_svm$y)) - 1
datos_svm$y = as.factor(datos_svm$y)
### tomamos conjunto de test
set.seed(123)
N = nrow(datos_svm)
N_train = round(0.1*N)
datos_svm = datos_svm[sample(1:N,N),]
datos_test = datos_svm[1:N_train,]
datos_train = datos_svm[{N_train + 1}:N,]
### Entrenamos el modelo
kernel = 'polynomial'
ranges = list()
ranges$degree = c(2, 3, 4)
ranges$cost = c(1 ,2, 5, 10)
ranges$gamma = c(0.05, 1, 1.5)
ranges$coef0 = c(0, 0.5, 1, 2)
set.seed(123)
svm_cv_pol = tune("svm", y ~ x1 + x2, data = datos_train,
                  kernel = 'polynomial',ranges = ranges)
modelo1 = svm_cv_pol$best.model
test_SVM(modelo1, datos_test)

kernel = 'radial'
ranges = list()
ranges$cost = c(10, 12, 15)
ranges$gamma = c(0.05, 1, 1.5, 2)
set.seed(123)
svm_cv_rad = tune("svm", y ~ x1 + x2, data = datos_train,
                  kernel = 'radial', ranges = ranges)
modelo2 = svm_cv_rad$best.model
test_SVM(modelo2, datos_test)

kernel = 'sigmoid'
ranges = list()
ranges$cost = c(1, 3, 5, 10)
ranges$gamma = c(0.01, 0.05, 1, 1.5)
ranges$coef0 = c(-1,-0.05,0, 0.5, 1)
set.seed(123)
svm_cv_sig = tune("svm", y ~ x1 + x2, data = datos_train,
                  kernel = 'sigmoid', ranges = ranges)
modelo3 = svm_cv_sig$best.model
test_SVM(modelo3, datos_test)


(plot_SVM(modelo1) + ggtitle(paste('polynomial', ' Accuracy: 90.91%'))) +
(plot_SVM(modelo2) + ggtitle(paste('radial', ' Accuracy: 77.27%'))) +
(plot_SVM(modelo3) + ggtitle(paste('sigmoid', ' Accuracy: 95.45%')))
# [Fin del codigo]============================================================.#