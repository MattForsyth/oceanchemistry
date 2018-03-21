#' Generate a nutrient profile
#'
#' Generate a nutrient profile. The dependent variable (nutrients etc.) is on the x-axis and the independent variable (depth, pressure, etc.) is on the y-axis.
#' The y-axis is reversed so that the top of the y-axis is the water surface - typical in oceanography literature. The result of \code{generateProfile} is stored in
#' in the global environment under \code{plotx} where \code{x} is a suffix indicating the plot number.
#' \cr \cr All measurements must be separated by a subset of some kind, whether that is the cast number or station number is up to you.
#' The function requires a subset because it must distinguish between different data points in order to draw different profiles. Otherwise all data points would be linked together. The subset you choose
#' can be restricted to particular subsets with the optional \code{subsets}.
#' @param depth_column  Depth (or pressure) values.
#' @param profile_columns  Measurments.
#' @param subset_column Variable to separate each measurement profile - usually station number or cast number.
#' @param subsets Optional specification of which subsets you are interested in - i.e. stations 1, 2, 3, and 4.
#' @param size Optional choice of point size in the depth profiles.
#' @param lwd Optional choice of line width.
#' @seealso \url{http://ggplot2.tidyverse.org/reference/} for controlling plot parameters.
#' @keywords Nutrient, depth, profile, ocean
#' @export
#' @examples
#'
#' library(oceanchemistry)
#'
#' data("greenland", package = "oceanchemistry")
#' head(greenland)
#'
#' #PLOT1 - no subsets specified#
#'
#' generateProfile(greenland$Depth,
#'      greenland$`NO2 + NO3 [umol/lg]`,
#'      greenland$Station)
#'
#' profile1 <- profile1 + xlab("Nitrate [umol/kg]")
#' print(profile1)
#'
#' #PLOT2 - subsets specified#
#'
#' generateProfile(greenland$Depth,
#'      greenland$`NO2 + NO3 [umol/lg]`,
#'      greenland$Station,
#'      subsets = c(241,244))
#'
#' profile1 <- profile1 + xlab("Nitrate [umol/kg]")
#' print(profile1)



generateProfile <- function(depth_column, profile_columns, subset_column, subsets = z, size = 3 ,lwd = 1.5){

  suppressMessages(suppressWarnings(library(ggplot2)))

  if(is.factor(subset_column)==F){

    subset_column <- as.factor(subset_column)

  } else{

    }


  if (missing(subsets)){



    df <- data.frame(subset_column,depth_column, profile_columns)
    colnames(df)[1] <- "subset"
    colnames(df)[2] <- "depth"

    df <- subset(df, complete.cases(df[,3:length(df)]))

    name_list <- colnames(df)

    for (x in 3:length(df)){

      z <- ggplot(data = df, aes_string(x = as.name(name_list[x]),
                                        y = as.name(name_list[2]),
                                        group = as.name(name_list[1]),
                                        colour = as.name(name_list[1]))) +
        geom_point(size = size) +
        scale_x_continuous(position = "top") +
        geom_path(aes_string(colour=as.name(name_list[1])),lwd = lwd) +
        xlab(name_list[x]) +
        ylab('Depth [m]') +
        scale_y_reverse(limits = c(110,0)) +
        theme_bw() +
        theme(legend.position="none")

      nam <- paste("profile",x-2,sep="")
      assign(nam,z,envir=.GlobalEnv)

    }

  }


  else{


  df <- data.frame(subset_column,depth_column, profile_columns)
  colnames(df)[1] <- "subset"
  colnames(df)[2] <- "depth"

  subsets <- subsets[order(subsets)]

  df <- subset(df, subset_column %in% subsets)
  df <- subset(df, complete.cases(df[,3:length(df)]))

  name_list <- colnames(df)

  for (x in 3:length(df)){

    z <- ggplot(data = df, aes_string(x = as.name(name_list[x]),
                                      y = as.name(name_list[2]),
                                      group = as.name(name_list[1]),
                                      colour = as.name(name_list[1]))) +
      geom_point(size = size) +
      scale_x_continuous(position = "top") +
      geom_path(aes_string(colour=as.name(name_list[1])),lwd = lwd) +
      xlab(name_list[x]) +
      ylab('Depth [m]') +
      scale_y_reverse(limits = c(110,0)) +
      theme_bw() +
      theme(legend.position="none")

    nam <- paste("profile",x-2,sep="")
    assign(nam,z,envir=.GlobalEnv)


  }

  }

  }

