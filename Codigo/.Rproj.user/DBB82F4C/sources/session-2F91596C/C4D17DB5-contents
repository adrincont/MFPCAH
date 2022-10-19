# [PCA híbrido] ==============================================================.#
# {Calculo para PCA híbrido} ----------------------------------------------.####
MFHPCA = function(datos, M, delta=0.99, scale=TRUE , ...){
  if (scale) {
    datos = scale_FunDataH(datos)
  }
  fun_data = datos@funData
  vec_data = datos@vecData
  p_fun = length(fun_data) # numero de variables funciones
  p_vec =  ncol(vec_data) # numero de variables vectores
  N = nrow(vec_data) # numero de observaciones
  # {MFDA de la parte funcional}
  uniExpansions_r = list()
  for (i in 1:p_fun) {
    uniExpansions_r[[i]] = list(type = 'uFPCA') # uFPCA: calculo basado en PCA
  }
  cat('Calculating MFPCA of the multivariate functional part...','\n')
  mfda_r = MFPCA(fun_data, M = 10,uniExpansions = uniExpansions_r)
  L = c(1:10)[cumsum(mfda_r$values/sum(mfda_r$values)) > delta][1]
  if (is.na(L)) {
    mfda_r = MFPCA(fun_data, M = 20,uniExpansions = uniExpansions_r)
    L = c(1:20)[cumsum(mfda_r$values/sum(mfda_r$values)) > delta][1]
  }
  mfda_r = MFPCA(fun_data ,M = L, uniExpansions = uniExpansions_r)
  scores_mfda = mfda_r$scores
  # {PCA parte vectorial}
  cat('Calculating PCA of the multivariate functional part...','\n')
  pca_r = PCA(vec_data, scale.unit = FALSE, ncp = p_vec, graph = FALSE)
  J = c(1:p_vec)[cumsum(pca_r$eig[,1]/sum(pca_r$eig[,1])) > delta][1]
  pca_r = PCA(vec_data, scale.unit = FALSE, ncp = J, graph = FALSE)
  scores_pca = pca_r$ind$coord
  # {Estimacion de la matriz V}
  scores_join = cbind(scores_mfda, scores_pca)
  cat('Calculating MFHPCA of the multivariate functional part...','\n')
  V = cov(scores_join)
  eigen_V = eigen(V)
  # {Calculo de pc_hibridos}
  mfdaH_r = list()
  mfdaH_r$functions = list() # parte funcional de las direcciones
  mfdaH_r$vectors = vector() # parte vectorial de las direcciones
  mfdaH_r$values = eigen_V$values # valores propios
  mfdaH_r$scores = vector() # puntajes del PCA hibrido
  cm_i = eigen_V$vectors[1:L,]
  dm_i = eigen_V$vectors[-c(1:L),]
  eta_i = scores_mfda
  gamma_i = scores_pca
  # funciones propias (Psi,Theta)
  # Psi
  for (j in 1:p_fun) {
    fun_i = mfda_r$functions[[j]]
    X_i = vector()
    for (i in 1:M) {
      C = matrix(cm_i[,i], nrow = L, ncol = length(fun_i@argvals[[1]]))
      X_i = rbind(X_i, colSums(fun_i@X*C))
    }
    fun_i@X = X_i
    mfdaH_r$functions[[j]] = fun_i
  }
  mfdaH_r$functions = multiFunData(mfdaH_r$functions)
  # Theta
  for (i in 1:M) {
    D = matrix(dm_i[,i], nrow = p_vec, ncol = J, byrow = TRUE)
    mfdaH_r$vectors = cbind(mfdaH_r$vectors, rowSums(pca_r$svd$V*D))
  }
  # Calculo de scores híbridos
  for (i in 1:M) {
    D = matrix(dm_i[,i], nrow = N, ncol = J, byrow = TRUE)
    C = matrix(cm_i[,i], nrow = N, ncol = L, byrow = TRUE)
    score_i = rowSums(scores_mfda*C) + rowSums(scores_pca*D)
    mfdaH_r$scores = cbind(mfdaH_r$scores, score_i)
  }
  # Resultados finales
  pca_i = list(MFPCA = mfda_r, PCA = pca_r, J = J, L = L) # PCA individuales
  dir_i = mfh_data(fun_data = mfdaH_r$functions,vec_data = mfdaH_r$vectors)
  pcah_i = list(V = V, mfdaH = mfdaH_r, dir = dir_i)
  results = list(PCAs = pca_i, MFPCAH = pcah_i)
  class(results) = 'mfhpca'
  return(results)
}
# {Resumen PCA híbrido} ---------------------------------------------------.####
summary.mfhpca = function(object){
  mfda_r = object$PCAs$MFPCA
  pca_r = object$PCAs$PCA
  mfdaH_r = object$MFPCAH$mfdaH
  L = object$PCAs$L
  J = object$PCAs$J
  M = dim(object$MFPCAH$mfdaH$scores)[2]
  # MFPCA
  tab_summary_mfda = round(summary(mfda_r), 3)[,1:L]
  tab_summary_mfda[2:3,] = tab_summary_mfda[2:3,]*100
  rownames(tab_summary_mfda) = c('val', 'explicada', 'acumulada')
  # PCA
  tab_summary_pca = t(pca_r$eig)[,1:J]
  rownames(tab_summary_pca) = rownames(tab_summary_mfda)
  colnames(tab_summary_pca) = paste('PC', 1:J)
  # MFHPCA
  val_mfh = mfdaH_r$values
  var_exp = val_mfh/sum(val_mfh)*100
  cum_mfh  = cumsum(var_exp)
  tab_summary_mfdaH = t(data.frame(val_mfh, var_exp, cum_mfh))[,1:M]
  rownames(tab_summary_mfdaH) = rownames(tab_summary_mfda)
  colnames(tab_summary_mfdaH) = paste('PC',1:M)
  cat('|=================================================================|', '\n')
  cat('PCA _____________________________________________________________.','\n')
  print(round(tab_summary_pca,4))
  cat('MFPCA ___________________________________________________________.','\n')
  print(round(tab_summary_mfda,4))
  cat('MFHPCA __________________________________________________________.','\n')
  print(round(tab_summary_mfdaH,4))
  cat('|================================================================|','\n')
  class_scores =  class(object$MFPCAH$mfdaH$scores)
  class_dir = class(object$MFPCAH$dir)
  cat('mfdaH_r$scores:', class_scores,'...', 'puntajes para PCA hibrido','\n')
  cat('mfdaH_r$dir:', class_dir,'...',
      'direcciones principales PCA hibrido','\n')
}
# {Grafico PCA híbrido} ---------------------------------------------------.####
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
autoplot.mfhpca = function(object, type = 'var'){
  mfda_r = object$PCAs$MFPCA
  pca_r = object$PCAs$PCA
  mfdaH_r = object$MFPCAH$mfdaH
  L = object$PCAs$L
  J = object$PCAs$J
  M = dim(object$MFPCAH$mfdaH$scores)[2]
  k = length(object$PCAs$MFPCA$functions)
  p = dim(object$PCAs$PCA$var$coord)[1]
  N = dim(object$PCAs$PCA$ind$coord)[1]
  # Varianza explicada
  if (type == 'var') {
    p1 = plot_varexp(mfda_r$values) + ggtitle(paste('MFPCA (k=', k,'L=', L,')'))
    p2 = plot_varexp(mfdaH_r$values[1:5]) + ggtitle(paste('MFHPCA (M=' ,M ,')'))
    p3 = plot_varexp(pca_r$eig[,1]) + ggtitle(paste('PCA (p=', p, 'J=', J, ')'))
    plot_i = (p1 + p2)/p3
    return(plot_i)
  }
  # Gráficos de funciones propias y vectores propios PCA
  if (type == 'eigen') {
    return(autoplot(object$MFPCAH$dir))
  }
  # Plano factorial
  if (type == 'ind') {
    scores_mfdaH_r = data.frame(mfdaH_r$scores, id = 1:N)
    colnames(scores_mfdaH_r) = c('PC1', 'PC2', 'id')
    plot_i = ggplot(scores_mfdaH_r,aes(PC1, PC2)) +
      geom_vline(xintercept = 0, color = 'blue') +
      geom_hline(yintercept = 0,  color = 'blue') + theme_bw() +
      geom_point() + ggtitle('MFPCAH') +
      geom_text(aes(label = id), vjust = -0.4, hjust = 0,size = 3)
    return(plot_i)
  }
}
# [Fin del codigo] ===========================================================.#