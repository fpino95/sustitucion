#' @title Grafico de burbujas
#' @description Esta funcion presenta la informacion de multiples variables (continuas y categoricas) de manera grafica en el nivel mas desagregado, el cual es un grafico en 2 dimensiones con caracteristicas que permiten visualizar las distintas variables que se quieren mostrar.
#' @param data Conjunto de datos con todas las variables que se ingresaran al grafico y su descripcion. Esta tabla debe ser en formato data table. No se ingresan las variables que no se quieren mostrar en ninguna opcion.
#' @param x Variable continua correspondiente al eje x del grafico de burbujas. Se ingresa indicando la tabla$variable, por ejemplo: data$variablex.
#' @param y Variable continua correspondiente al eje y del grafico de burbujas. Se ingresa indicando la tabla$variable, por ejemplo: data$variabley.
#' @param size Variable continua correspondiente al tamaño de cada burbuja. Se ingresa igualmente de manera tabla$variable_size.
#' @param color Variable categorica correspondiente al color de cada burbuja, la cual representa el grupo o categoria al que pertenece cada dato. Igualmente se ingresa de manera tabla$variable_color.
#' @param title Titulo del grafico. Ingresar como string.
#' @return El grafico de burbujas.
#' @export burbujas
#' @import dplyr plotly
#' @examples
#' burbujas(data = iris, x = iris$Sepal.Length, y = iris$Petal.Length, size = iris$Sepal.Width,
#'          color = iris$Species, title = 'Iris', xlabel = 'Sepal Length', ylabel = 'Petal Length')

burbujas <- function(data, x, y, size, color, title, xlabel, ylabel){
  data2 <- as.data.frame(data)
  nam_var <- colnames(data)
  texto_vari <- c()
  data$col_textoc <- NA
  for (i in 1:nrow(data)) {
    for (j in 1:length(nam_var)) {
      texto_vari[j] <- paste0(nam_var[j], ': ', data2[i,j])
    }
    texto_vari <- paste(texto_vari, collapse = '</br>')
    data$col_textoc[i] <- texto_vari
  }
  p <- plotly::plot_ly(data,

                  x = ~x,

                  y = ~y,

                  size = ~size,

                  color = ~color,

                  text = ~col_textoc,

                  type = 'scatter',

                  mode = 'markers'

  )

  p <- plotly::layout(p,

    title = title,

    xaxis = list(title = xlabel, range = c(0, max(x, y)),

                 scaleratio = mean(c(x, y))/5000),

    yaxis = list(title = ylabel, range = c(0, max(x, y)),

                 scaleratio = mean(c(x, y))/5000),

    autosize = T

    #, shapes = lineas
  )
  p
}




