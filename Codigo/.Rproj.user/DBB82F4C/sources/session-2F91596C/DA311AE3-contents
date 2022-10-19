# [Funcion para estandarizar objetos hibridos] ===============================.#
scale_FunDataH = function(Z) {
  # Z: objeto de la clase multiFunDataH
  fun_data = Z@funData
  vec_data = Z@vecData
  # Escalar variables funcionales
  MinMax = function(x){
    x_min = min(x)
    x_max = max(x)
    val_i = (x - x_min)/(x_max - x_min)
    return(list(args = c(min = x_min, max = x_max),val_i))
  }
  scale_y = max(sapply(fun_data@.Data, function(x){max(x@X)}))
  fun_data = fun_data/scale_y
  scale_x = list()
  for (i in 1:length(fun_data@.Data)) {
    min_max_i = MinMax(fun_data@.Data[[i]]@argvals[[1]])
    scale_x[[i]] = min_max_i$args
    fun_data@.Data[[i]]@argvals[[1]] = min_max_i[[2]]
  }
  # Escalar variables vectoriales
  vec_data = scale(vec_data)
  # Eliminar efecto entre funciones y vectores
  fun_data_c = fun_data - meanFunction(fun_data)
  w = sum(scalarProduct(fun_data_c, fun_data_c))/sum(rowSums(vec_data^2))
  vec_data = sqrt(w)*vec_data
  return(mfh_data(fun_data = fun_data_c,vec_data = vec_data))
}
# [Fin del codigo] ===========================================================.#