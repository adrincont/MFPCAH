# [Simulacion de observaciones funcionales hibridas] =========================.#
# Ruido hibrido
epsilon_H = function(Zi,sd){
  mean_i = rep(0,length(Zi@funData)) 
  ei = mvrnorm(n = length(Zi@funData[[1]]@argvals[[1]]),mu = mean_i,Sigma = sd)
  vec = Zi@vecData*0
  fun_list = list()
  for (i in 1:length(Zi@funData)) {
    Ei = matrix(ei[,i],nrow = dim(Zi@funData[[i]]@X)[1],ncol = dim(Zi@funData[[i]]@X)[2],byrow = TRUE)
    fun_list[[i]] = funData(Zi@funData[[i]]@argvals[[1]],Ei)
  }
  fun = multiFunData(fun_list)
  vec = Zi@vecData*0
  ei = mfh_data(fun_data = fun,vec_data = vec)
  return(ei)
}
# Generacion de las observaciones
r_FunH = function(n_fun,n_vec,n_grid,N,conf = c("PolyHigh","FourierLin","Wiener")){
  # Generacion de direccion principal Xi = (Psi,Theta)
  Xi = Xi_gen(n_fun = n_fun,n_vec = n_vec,n_grid = n_grid,conf = conf)
  # Generacion de las observaciones
  lambda_i = 0.5^(1:n_vec - 1)
  for (i in 1:N) {
    rho_i = apply(matrix(lambda_i),1,function(x){rnorm(1,0,x)})
    Zi = rho_i[1]*Xi[[1]]
    for (n in 2:n_vec) {
      Zi = Zi + rho_i[n]*Xi[[n]]
    }
    sd = matrix(0.1,n_fun,n_fun)
    diag(sd) = rep(0.4,n_fun)
    Zi = Zi + epsilon_H(Zi,sd = sd)
    if (i == 1) {
      Z = Zi
    }else{
      Z = append_Mfun(Z,Zi)
    }
  }
  return(Z)
}
# [Fin del codigo] ===========================================================.#