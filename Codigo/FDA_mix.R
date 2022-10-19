#.============================================================================.#
#                           Manejo de objetos hibridos                         #
#.============================================================================.#
# [Librerias] ================================================================.#
source("library.R")
# [Funciones] ================================================================.#
source("Funciones/multiFunDataH.R") # Clase de objetos multiFunDataH
source("Funciones/multiFunDataH_plot.R") # Graficos para la clase multiFunDataH
# [Codigo] ===================================================================.#
## {datos maiz} -----------------------------------------------------------.####
data_2 = readMat('Datos_eig_2/corn.mat')
dat_2 = list()
## Parte funcional
X_1 = data_2$m5spec$data
X_2 = data_2$mp5spec$data
X_3 = data_2$mp6spec$data
dimnames(X_1)[[2]] = seq(1100,2498,2)
dimnames(X_2)[[2]] = seq(1100,2498,2)
dimnames(X_3)[[2]] = seq(1100,2498,2)
fun_1 = funData(seq(1100,2498,2),X_1)
fun_2 = funData(seq(1100,2498,2),X_2)
fun_3 = funData(seq(1100,2498,2),X_3)
dat_2$fmult = multiFunData(list(fun_1,fun_2,fun_3))
## Parte vectorial
dat_2$vect = data_2$propvals$data
colnames(dat_2$vect) = c("humedad","aceite","proteína","almidon")
## Crear objeto hibrido
data_h = mfh_data(fun_data = dat_2$fmult,vec_data = dat_2$vect)
## Grafico de un objeto hibrido
autoplot(data_h)
# [Fin del codigo] ===========================================================.#