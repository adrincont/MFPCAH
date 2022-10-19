# ============================================================================ #
#                           MFDA a datos híbridos                             .#
#.============================================================================.#
# [Librerias] ================================================================.#
source("library.R")
# [Funciones] ================================================================.#
source("Funciones/multiFunDataH.R") # Clase de objetos multiFunDataH
source("Funciones/multiFunDataH_plot.R") # Gráficos para la clase multiFunDataH
source("Funciones/Xi_gen.R") # Generar direcciones híbridas
source("Funciones/r_FunH.R") # Generar observaciones híbridas
source("Funciones/scale_FunDataH.R") # Estandarizar hibridos
source("Funciones/MFHPCA.R") # PCA híbrido
# {Simulacion de proceso} =================================================.####
conf = c("PolyHigh","PolyHigh","FourierLin","Wiener")
Z = r_FunH(n_fun = 4,n_vec = 10,n_grid = 60,N = 100,conf = conf)
# {Calculo del PCA híbrido} ===============================================.####
PCAH = MFHPCA(Z,M = 8,delta = 0.999)
# Resultados
summary(PCAH)
autoplot(PCAH, 'var')
autoplot(PCAH, 'eigen')
autoplot(PCAH, 'ind')
# {Fin del codigo} ===========================================================.#