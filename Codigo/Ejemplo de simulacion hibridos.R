#.============================================================================.#
#                 Simulación de observaciones híbridas                         #
#.============================================================================.#
# [Librerias] ================================================================.#
source("library.R")
# [Funciones] ================================================================.#
source("Funciones/multiFunDataH.R") # Clase de objetos multiFunDataH
source("Funciones/multiFunDataH_plot.R") # Graficos para la clase multiFunDataH
source("Funciones/Xi_gen.R") # Generar direcciones híbridas
source("Funciones/r_FunH.R") # Generar observaciones híbridas
# [Codigo] ===================================================================.#
n_fun = 4
n_vec = 10
n_grid = 60
N = 100
conf = c("PolyHigh","PolyHigh","FourierLin","Wiener")
Z = r_FunH(n_fun,n_vec,n_grid,N,conf = conf)
autoplot(Z)
# [Fin del codigo] ===========================================================.#

