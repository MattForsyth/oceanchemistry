#' Geneate nutrient range stats
#'
#' This function produces nutrient ranges at two different depths - usually at the surface and at depth. The depths are given to the function as two
#' separate depth ranges. The result of \code{nutrientRange} is stored under \code{nutrientRanges} in the global environment.
#' @param depth_column The depth (or pressure) values.
#' @param nutrient_columns All nutrient data. Must be provided in separate columns.
#' @param depth1 The upper limit of your shallow depth range.
#' @param depth2  The lower limit of your shallow depth range.
#' @param depth3  The upper limit of your deep depth range.
#' @param depth4  The lower limit of your deep depth range.
#' @keywords Nutrient, range, water column
#' @export
#' @examples
#'
#' library(oceanchemistry)
#'
#' data("greenland", package = "oceanchemistry")
#' head(greenland)
#'
#' nutrientRange(greenland$Depth,
#'      greenland[,10:11],
#'      0,15,80,115)
#'
#' View(nutrientRanges)

nutrientRange <- function(depth_column, nutrient_columns, depth1, depth2, depth3, depth4){


  df <- data.frame(depth_column,nutrient_columns)

  depth1_index <- which(depth_column >= depth1 & depth_column <= depth2)
  depth2_index <- which(depth_column <= depth4 & depth_column >= depth3)

  surface <- df[c(depth1_index),]
  deep <- df[c(depth2_index),]


  subset_list <- list(surface,deep)


  min_values <- rep(0,length(nutrient_columns))
  max_values <- rep(0,length(nutrient_columns))


  depths <- c(paste(depth1, " - ", depth2, "m" , sep=" "), paste(depth3, " - ", depth4, "m" , sep=" "))

  depth_string <- 0
  holder <- 0
  pp <- 1
  zz <- 0
  n <- length(nutrient_columns)


  for(z in 1:length(subset_list)){ #For surface and 100m

    holder <- subset_list[z] #Calling the subset of interest
    holder <- as.data.frame(holder) #When we assign holder a value, the value is a list. We turn this into a dataframe here.



    for(y in 2:length(surface)){ #For each measurement of interest



      min_values[pp] <- min(holder[,y], na.rm = TRUE) #Finding min of each variable at each depth
      max_values[pp] <- max(holder[,y], na.rm = TRUE) #Finding max of each variable at each depth

      pp <- pp + 1 #Moving over one index to find the min of the next variable. We cannot use y to do this because y is the
      #the index within each subset.

      }



      #depth_string[(n*zz+1):(n*z)] <- rep(depths[z], n) #This is a lazy way of repeating the depth numbers n number of times
      #to match the number of variables we are finding the min and max of. That is, we are repeating 1, 2, 3, 4, and 5
      #as many times as we have variables. For example, at this moment, we are finding the min and max for 7 variables, so
      #we are using the above code to repeat each numbers 7 times (1,1,1,1,1,1,1,2,2,2,2,2,2,2...)


      zz <- zz + 1 #moving over
      pp <- 1


      variables <- colnames(surface)[2:length(surface)]#Repeating variables for the table


      ranges <- data.frame(variables,min_values,max_values)

      nam <- paste("ranges",z, sep = "")
      assign(nam,ranges, envir = .GlobalEnv)
#
#       nam <- paste("min",z,sep = "")
#       assign(nam, min_values, envir = .GlobalEnv)
#
#       nam <- paste("max",z,sep = "")
#       assign(nam,max_values, envir = .GlobalEnv)



    }

  # variables <- colnames(surface)[2:length(surface)]#Repeating variables for the table
  #
  # ranges <- data.frame(variables,depth_string,min_values,max_values) #Making the final table
  # assign("nutrientRanges",ranges,envir = .GlobalEnv)


}




