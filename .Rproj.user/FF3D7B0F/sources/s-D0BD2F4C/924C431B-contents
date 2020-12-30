#' @title Grafico de tortas
#' @description Resume la informacion de n variables (categoricas) en n graficos de torta, con el objetivo de ver las frecuencias de cada una de las categorias de cada variable.
#' @param data Conjunto de datos con las variables categoricas a graficar. Esta tabla debe ser en formato data frame o data table. No ingresar variables que no sean correspondientes a categoricas del grafico (no ingresar ID ni continuas).
#' @return Graficos de tortas de todas las variables categoricas ingresadas.
#' @export burbujas
#' @import dplyr plotly
#' @examples
#' tortas(data.frame(HairEyeColor)[, c(1, 2, 3)])

tortas <- function(data){
  data2 <- as.data.frame(data)
  variables <- list()
  for (i in 1:ncol(data)) {
    variables[[i]] <- data.frame(table(data2[, i]))
  }

  p <- plotly::plot_ly(textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'))
  for (i in 1:ncol(data)) {
    variablei <- variables[[i]]
    p <- plotly::add_pie(p, data = variablei, labels = ~Var1, values = ~Freq, hole = 0.4,
                    title = colnames(data)[i], domain = list(row = 0, column = i-1))
  }
  p <- plotly::layout(p, title = "", showlegend = F,
                 grid=list(rows = 1, columns = ncol(data)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = F))
  p
}


