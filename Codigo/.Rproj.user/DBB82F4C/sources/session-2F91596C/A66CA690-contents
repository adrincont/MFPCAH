library(fda.usc)
library(fda)
library(ggplot2)
library(dplyr)
require(GGally)
library(patchwork)
library(R.matlab)
data = readMat('data.mat')
# Datos de Fluerecencia
# se midieron en intervalos de 0,5 nm (571 longitudes de onda) en siete longitudes
# de onda de excitación (230, 240, 255, 290, 305, 325, 340 nm).
Fluerecencia = array(dim = c(571,268,7))
dimnames(Fluerecencia)[[1]] = seq(275,560,0.5)
dimnames(Fluerecencia)[[2]] = seq(1,268)
dimnames(Fluerecencia)[[3]] = c('230nm','240nm','255nm','290nm','305nm','325nm','340nm')
for (i in 1:7) {
  Fluerecencia[,,i] = t(data$X[,seq(571*(i-1)+1,571*(i),1)])
}
# Grafico de los espectros
# excw = excitation wavelengths
# emsw = emission wavelengths
# id = muestra
# sgni = signal intensity
data_plot = as.data.frame.table(Fluerecencia)
names(data_plot) = c('emsw','id','excw','sgni')
data_plot$emsw = as.numeric(as.character(data_plot$emsw))
ggplot(data_plot,aes(x = emsw ,y = sgni,fill = id)) +
  geom_line(aes(color = id),show.legend = FALSE) +
  facet_wrap(~excw,scales = "free_y") + theme_bw()
# Parámetros de calidad
# El contenido de cenizas está determinado por la conductividad y
# es una medida de la cantidad de impurezas inorgánicas en el
# azúcar refinado. Se da en porcentajes
# El color se determina como la absorción a 420 nm de una solución
# de azúcar filtrada por membrana ajustada a pH 7. El color se da
# como una unidad derivada de la absorbancia, donde 45 es el color
# máximo permitido del azúcar
Calidad = as.data.frame(data$y)[,-1]
colnames(Calidad) = c('ceniza','color')
ggplot(Calidad,aes(color,ceniza)) + geom_point() + geom_smooth()
# Mediciones de laboratorio
Pros = data$Proc[,seq(39*(1-1)+1,39*(1),1)]
list_NA = apply(Pros,MARGIN = 2,function(x){return(sum(is.na(x)))})/268
Pros = as.data.frame(Pros[,list_NA < 0.05])[-c(1,2,3)]
Pros = mutate_all(Pros,~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

my_fn = function(data, mapping, ...){
  p = ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(Pros,lower = list(continuous = my_fn))
# Correlaciones entre variables y curvas
plot1 = ggplot(NULL,aes(x = Fluerecencia[10,,7],y = Pros[,8])) +
  geom_point() + stat_smooth(span = 1) +
  xlab(paste('emsw=',dimnames(Fluerecencia)[[1]][10],'nm',' (ti = 10)')) +
  ylab(colnames(Pros)[8])
plot2 = ggplot(NULL,aes(x = Fluerecencia[10,,6],y = Pros[,4])) +
  geom_point() + stat_smooth(span = 1) +
  xlab(paste('emsw=',dimnames(Fluerecencia)[[1]][10],'nm',' (ti = 10)')) +
  ylab(colnames(Pros)[4])
plot3 = ggplot(NULL,aes(x = Fluerecencia[10,,5],y = Pros[,2])) +
  geom_point() + stat_smooth(span = 1) +
  xlab(paste('emsw=',dimnames(Fluerecencia)[[1]][10],'nm',' (ti = 10)')) +
  ylab(colnames(Pros)[2])
plot1 + plot2 + plot3
# Metodo FDA datos mixtos ----------------------------------------------------------------.#
