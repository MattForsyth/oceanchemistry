#' Generate a contour or image
#'
#' This function generates a contour plot or an images (surface plot). The most frequent application of this function is surface nutrient
#' data. \code{surfacePlot} can handle multiple measurements or variables at once. You can choose between a surface plot,
#' a contour plot, or both. The surface plot uses a colour-blind-friendly palette taken from the \code{oce} package.
#' @param depth_column Depth (or pressure) values.
#' @param latitude Latitude coordinates.
#' @param longitude Longitude coordinates.
#' @param measurements The measurements to plot. If you have multiple variables they should be presented in columns or a dataframe.
#' @param plot_depth The lower limit of the surface plot. Currently this function will plot between the surface and whatever depth is input.
#' @param surface Do you want to make a surface plot?
#' @param contour Do you want to make a contour plot?
#' @keywords Contour, surface, ocean, measurements
#' @seealso This function relies on the \code{interpBarnes} function from the \code{oce}
#' package: \url{https://cran.r-project.org/web/packages/oce/oce.pdf}.
#' @export
#' @examples
#' library(oceanchemistry)
#'
#' data("greenland", package = "oceanchemistry")
#' head(greenland)
#'
#' surfacePlot(greenland$Depth,
#'      greenland$Latitude,
#'      greenland$Longitude,
#'      data.frame(greenland$`NO2 + NO3 [umol/lg]`),
#'      plot_depth = 10)
#'
#'


surfacePlot <- function(depth_column,latitude, longitude, measurements, plot_depth, surface = T, contour = F){

  measurements <- data.frame(measurements)


  suppressWarnings(library(oce))
  suppressWarnings(library(ocedata))
  library(gsw)

  df <- data.frame(depth_column,latitude,longitude,measurements)



  colnames(df)[1] <- "depth"
  colnames(df)[2] <- "lat"
  colnames(df)[3] <- "lon"



  df <- subset(df, depth <= plot_depth)



  lat <- df[,2]
  lon <- df[,3]
  variables <- df[,4:length(df)]
  var_names <- colnames(df[4:length(df)])



  asp <- 1/cos(pi/180*mean(lat))

  data("coastlineWorldMedium",package = "ocedata")


  for(x in 4:length(df)){

  xr <- 3
  yr <- xr/asp
  g <- interpBarnes(lon,lat,df[,x], xr = xr, yr = yr, trim = 0.5)
  nam <- paste("interpResults",x-3,sep="")
  assign(nam,g,envir=.GlobalEnv)
  if (surface == T){

  surf <- imagep(g$xg, g$yg, g$zg,asp=1/cos(pi/180*mean(lat)), col = oceColorsViridis)

  lines(coastlineWorldMedium[["longitude"]],coastlineWorldMedium[["latitude"]])
  mtext(LETTERS[x-3],side=3)

  }

  else {}


  if (contour == T){

  cont <- contour(g$xg, g$yg, g$zg, asp = asp)

  lines(coastlineWorldMedium[["longitude"]],coastlineWorldMedium[["latitude"]])
  text(lon,lat,round(df[,x],0), col=2, cex = 0.7 )
  mtext(LETTERS[x-3],side=3)


  }

  else {}



  }



}
