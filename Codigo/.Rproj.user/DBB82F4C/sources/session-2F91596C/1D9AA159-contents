# [Funcion para generar direcciones hibridas Xi = (Psi,Theta)] ===============.#
Xi_gen = function(n_fun,n_vec,n_grid,conf = c("PolyHigh","FourierLin","Wiener")) {
  # n_fun: Tamaño de la parte funcional
  # n_vec: Tamaño de la parte vectorial
  # n_grid: numero de puntos en la grilla
  # return(): multiFunData class base de tamaño n_vec
  # Generacion direccion vectorial Theta
  Mat_aux = matrix(0.2,nrow = n_vec,ncol = n_vec)
  diag(Mat_aux) = rep(1,n_vec)
  Theta = eigen(Mat_aux)$vectors
  # Generacion direccion funcional Psi
  grid = (1:n_grid - 1)/(n_grid - 1)
  bases_list = list()
  for (name_i in unique(conf)) {
    base_i = eFun(grid,M = n_vec,type = name_i,ignoreDeg = 1:2)
    bases_list[[name_i]] = base_i@X
  }
  Psi = list()
  for (i in 1:n_vec) {
    Psi_i = list()
    for (j in 1:n_fun) {
      Psi_i[[j]] = funData(grid,matrix(bases_list[[conf[j]]][i,],nrow = 1))
    }
    Psi[[i]] = multiFunData(Psi_i)
  }
  ## Generacion direccion hibrida Psi
  Xi = list()
  for (n in 1:n_vec) {
    Xi[[n]] = mfh_data(fun_data = Psi[[n]],vec_data = matrix(Theta[,n],nrow = 1))
  }
  return(Xi)
}
# [Fin del codigo] ===========================================================.#