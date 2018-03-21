#' Redfield ratio analysis
#'
#' This function provides redfield ratios and statistics. Any \code{NA} values are automatically removed. The function accepts the optional argument
#' \code{subset}, which lets you specify whether the redfield statistics should be organized by any particular variable.
#' @param C Drawdown of organic carbon in umol/kg.
#' @param N Drawdown of nitrate in umol/kg.
#' @param P Drawdown of phosphate in umol/kg.
#' @param subset Subset the data by any variables i.e. cruise, station.
#' @keywords Redfield, ocean
#' @export
#' @examples
#'
#' library(oceanchemistry)
#'
#' data("greenland", package = "oceanchemistry")
#' head(greenland)
#'
#' redfield(greenland$'Carbon drawdown',
#'      greenland$'Nitrate drawdown',
#'      greenland$'Phosphate drawdown',
#'      subset = greenland$Cruise)
#'
#' View(redfield1)
#' View(redstats1)


redfield <- function(C,N,P,subset=x){

        nutrientlist <- list(C,N,P)

        length1 <- length(which(is.na(C)))
        length2 <- length(which(is.na(N)))
        length3 <- length(which(is.na(P)))


        lengthlist <- list(length1, length2, length3)


        list_index <- which.max(lengthlist)


        na_index <- which(is.na(unlist(nutrientlist[list_index])) == F)

        C <- C[na_index]
        N <- N[na_index]
        P <- P[na_index]

    if (missing(subset)){

    }

    else {

          subset <- subset[na_index]

    }


    if (missing(subset)){

        CN <- C/N
        CP <- C/P
        NP <- N/P


        redfield_data <- data.frame(CN,CP,NP)
        z <- redfield_data

        colnames(redfield_data) <- c("C:N","C:P","N:P")
        assign("redfield_data", z,envir=.GlobalEnv)



        for (xx in 1:(length(redfield_data))){


          if (exists("stats") == F){

            stats <- matrix(0, length(z), length(z))

          }

          else {}

          stats[xx,1] <- mean(z[,xx])
          stats[xx,2] <- median(z[,xx])
          stats[xx,3] <- sd(z[,xx])




        }

        zlabs <- c("Nutrient Ratio","Mean","Median","Standard deviation")
        zz <- data.frame(c("C:N","C:P","N:P"), stats)
        colnames(zz) <- zlabs
        assign("redstats", zz, envir=.GlobalEnv)




    }



    else {

      divider_list <- na.omit(unique(subset))
      List <- list()

      assign("redlist",divider_list,envir=.GlobalEnv)

      for(x in 1:length(divider_list))
      {

        assign("unique_index", which(subset == divider_list[x]))


        holdC <- C[unique_index]
        holdN <- N[unique_index]
        holdP <- P[unique_index]
        columns <- as.character(subset[unique_index])

        z <- data.frame(columns,holdC/holdN,holdC/holdP,holdN/holdP)
        labs <- c("Subset","C:N","C:P","N:P")
        colnames(z) <- labs


        nam <- paste("redfield",x,sep="")
        assign(nam, z,envir=.GlobalEnv)


        for (xx in 2:(length(z))){


          if (exists("stats") == F){

          stats <- matrix(0, length(z)-1, length(z)-1)
          }
          else {}

          stats[xx-1,1] <- mean(z[,xx])
          stats[xx-1,2] <- median(z[,xx])
          stats[xx-1,3] <- sd(z[,xx])




        }
        zlabs <- c("Nutrient Ratio","Mean","Median","Standard deviation")
        zz <- data.frame(c("C:N","C:P","N:P"), stats)
        colnames(zz) <- zlabs
        nam <- paste("redstats",x,sep="")
        assign(nam, zz, envir=.GlobalEnv)

    }



  }



}
