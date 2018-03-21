#' Generate redfield plots
#'
#' This function generates redfield plots.These plots are a visual comparison between the drawdown of major nutrients to the corresponding
#' terms of the redfield ratio of C:N:P. This function produces three scatter plots depitcting (1) C:N, (2) C:P, (3) N:P. All three
#' plots include a reference line that represents the redfield ratio.\cr\cr
#' The results of this function are printed for you. Each plot is also stored in the global environment if you wish to adjust its
#' presentation or parameters. \cr \cr
#' You may subset the redfield data if you want to differentiate between the points on the scatter plot. This is usually used
#' to separate measurements from different stations or cruises.
#' @param C Drawdown of organic carbon in umol/kg.
#' @param N Drawdown of nitrate in umol/kg.
#' @param P Drawdown of phosphate in umol/kg.
#' @param subset Subset the data by any variables i.e. cruise, station.
#' @param lwd Optional graphic parameter.
#' @param size Optional graphic parameter.
#' @param font.size Optional graphic parameter.
#' @param lty Optional graphic parameter.
#' @keywords Redfield, ocean
#' @export
#' @seealso This function relies on ggplot2 (\url{http://ggplot2.tidyverse.org/reference/}) for plotting and gridExtra (\url{https://cran.r-project.org/web/packages/gridExtra/gridExtra.pdf})
#' for arranging the plots. Please refer to these packages for any plot customization.
#' @examples
#'
#' library(oceanchemistry)
#'
#' data("greenland", package = "oceanchemistry")
#' head(greenland)
#'
#' redplot(greenland$'Carbon drawdown',
#'      greenland$'Nitrate drawdown',
#'      greenland$'Phosphate drawdown',
#'      subset = greenland$Cruise)
#'


redplot <- function(C, N, P, subset = x , lwd = 2, size = 3, font.size = 23, lty = "solid"){

  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(gridExtra)))


  theme_set(theme_bw(base_size = font.size))

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


    nutrients <- data.frame(C,N,P)
    colnames(nutrients) <- c("C_d","N_d","P_d")

    plot1 <- ggplot(data=nutrients,aes(y=C_d, x=N_d)) + geom_point(size = size) +
      geom_abline(intercept = 0, slope = 106/16,size=lwd, linetype = lty) +
      xlab(expression(NO[2]^{{textstyle("-")}}+NO[3]^{{textstyle("-")}}~"["~mu~mol~kg^{-1}~"]")) +
      ylab(expression(CH[2]*O~"["~mu~mol~kg^{-1}~"]")) +
      theme(legend.position="none")


    plot2 <- ggplot(data = nutrients, aes(y=C_d, x=P_d)) + geom_point(size = size) +
      geom_abline(intercept = 0, slope = 106/1,size=lwd, linetype = lty) +
      xlab(expression(PO[4]^{{textstyle("3-")}}~"["~mu~mol~kg^{-1}~"]")) +
      ylab(expression(CH[2]*O~"["~mu~mol~kg^{-1}~"]")) +
      theme(legend.position="none")


    plot3 <- ggplot(data=nutrients,aes(y=N_d, x=P_d)) + geom_point(size = size) +
      geom_abline(intercept = 0, slope = 16/1,size=lwd, linetype = lty) +
      xlab(expression(PO[4]^{{textstyle("3-")}}~"["~mu~mol~kg^{-1}~"]")) +
      ylab(expression(NO[2]^{{textstyle("-")}}+NO[3]^{{textstyle("-")}}~"["~mu~mol~kg^{-1}~"]")) +
      theme(legend.position = "none")

    plot4 <- ggplot() +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(panel.background = element_blank())

    grid.arrange(plot1 , plot2, plot3, plot4, layout_matrix=rbind(c(1,2), c(3,4)))
    g.red <- arrangeGrob(plot1 , plot2, plot3, plot4, layout_matrix=rbind(c(1,2), c(3,4)))

    assign("redfieldPlots",g.red,envir = .GlobalEnv)

    assign("CNplot", plot1, envir=.GlobalEnv)
    assign("CPplot",plot2,envir=.GlobalEnv)
    assign("NPplot",plot3,envir=.GlobalEnv)





  }

  else {

    subset <- subset[na_index]




  nutrients <- data.frame(C,N,P, subset)
  colnames(nutrients) <- c("C_d","N_d","P_d", "Subset")

  plot1 <- ggplot(data=nutrients,aes(y=C_d, x=N_d, colour=Subset)) + geom_point(size = size) +
    geom_abline(intercept = 0, slope = 106/16,size=lwd, linetype = lty) +
    xlab(expression(NO[2]^{{textstyle("-")}}+NO[3]^{{textstyle("-")}}~"["~mu~mol~kg^{-1}~"]")) +
    ylab(expression(CH[2]*O~"["~mu~mol~kg^{-1}~"]")) +
    theme(legend.position="none")


  plot2 <- ggplot(data = nutrients, aes(y=C_d, x=P_d, colour = Subset)) + geom_point(size = size) +
    geom_abline(intercept = 0, slope = 106/1,size=lwd, linetype = lty) +
    xlab(expression(PO[4]^{{textstyle("3-")}}~"["~mu~mol~kg^{-1}~"]")) +
    ylab(expression(CH[2]*O~"["~mu~mol~kg^{-1}~"]")) +
    theme(legend.position="none")


  plot3 <- ggplot(data=nutrients,aes(y=N_d, x=P_d, colour=Subset)) + geom_point(size = size) +
    geom_abline(intercept = 0, slope = 16/1,size=lwd, linetype = lty) +
    xlab(expression(PO[4]^{{textstyle("3-")}}~"["~mu~mol~kg^{-1}~"]")) +
    ylab(expression(NO[2]^{{textstyle("-")}}+NO[3]^{{textstyle("-")}}~"["~mu~mol~kg^{-1}~"]")) +
    scale_color_discrete(name = "Region", labels = c("Labrador Sea", "Irminger Sea"), guide = guide_legend(override.aes = list(
      linetype = c(rep("blank", 2))))) +
    theme(legend.key.height=unit(3,"line"))


    get_legend <-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
    }

    if(length(unique(subset))<2){

       plot3 <- plot3 + theme(legend.position = "none")

      plot4 <- ggplot() +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(panel.border = element_blank()) +
        theme(panel.background = element_blank())

      grid.arrange(plot1 , plot2, plot3, plot4, layout_matrix=rbind(c(1,2), c(3,4)))
      g.red <- arrangeGrob(plot1 , plot2, plot3, plot4, layout_matrix=rbind(c(1,2), c(3,4)))

      assign("redfieldPlots",g.red,envir = .GlobalEnv)

      assign("CNplot", plot1, envir=.GlobalEnv)
      assign("CPplot",plot2,envir=.GlobalEnv)
      assign("NPplot",plot3,envir=.GlobalEnv)



    }



    else{

  legend <- get_legend(plot3)

  plot3 <- plot3 + theme(legend.position = "none")

  grid.arrange(plot1 , plot2, plot3, legend, layout_matrix=rbind(c(1,2), c(3,4)))
  g.red <- arrangeGrob(plot1 , plot2, plot3, legend, layout_matrix=rbind(c(1,2), c(3,4)))

  assign("redfieldPlots",g.red,envir = .GlobalEnv)

  assign("CNplot", plot1, envir=.GlobalEnv)
  assign("CPplot",plot2,envir=.GlobalEnv)
  assign("NPplot",plot3,envir=.GlobalEnv)


    }

  }

}
