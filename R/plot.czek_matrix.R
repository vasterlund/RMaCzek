plot.czek_matrix<-function(x,
                           values=NULL,
                           col=FALSE,
                           #size=NULL,
                           plot_pch = NULL,
                           plot_cex = 1.5,
                           label.cex = 0.6,
                           plot_title="Czekanowski's diagram",
                           legend=TRUE,
                           axis=TRUE,
                           ...){


  # Save the information from x
  n_classes<-attr(x,"n_classes")
  levels<-attr(x,"levels")
  partition_boundaries<-attr(x,"partition_boundaries")
  new_order<-attr(x,"order")

  # Arrange the observations
  x<-x[new_order,new_order]



  # Change values inside czek_matrix ####
  if(class(values)%in%c("numeric","character") &
     length(values)==n_classes){
    values<-values
  }
  else if (col==FALSE){
    values<-rep(0,n_classes)
    values[1]<-2
    for(i in 2:(n_classes-1)){
      values[i]<-values[i-1]/2
    }
    values[n_classes]<-0

  }
  else {
    values<-round(seq(0,100,length.out = n_classes))
    values<-paste("gray",values,sep="")
  }


  plot_values<- values[x]



  # Initial settings ####
  # Calc dim of the plot
  n<-nrow(x)
  p<-n

  # Make data points to plot
  plot_y <- rep(n:1, p)
  plot_x <- rep(1:p, rep(n, p))


  # Add marginals for the legend
  if(legend){
    old_mar=par("mar")
    old_xpd=par("xpd")

    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=T)
  }


  # Make a color plot ####
  if(is.character(plot_values)){

    if(is.null(plot_pch)){
      plot_pch<-15
    }

    legend_cex<-1.5
    legend_col<-values

    plot_cex<-plot_cex
    plot_col<-plot_values



    # plot(x = plot_x,
    #      y = plot_y,
    #      col = plot_col,
    #      pch = plot_pch,
    #      cex = plot_cex,
    #      axes = FALSE,
    #      xlab = "",
    #      ylab = "",
    #      xlim = c(0.5, p + 0.5),
    #      ylim = c(0.5, n + 0.5))


  }



  # Make a dot plot ####
  else if(is.numeric(plot_values)){


    if(is.null(plot_pch)){
      plot_pch<-19
    }


    legend_cex<-values
    legend_col<-"black"


    plot_cex<-plot_values
    plot_col<-"black"
  }




  plot(plot_x, plot_y,
       col=plot_col,
       cex = plot_cex,
       pch = plot_pch,
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = c(0.5, p + 0.5),
       ylim = c(0.5, n + 0.5),
       main = plot_title)



  # Adjust the plot ####

  # Col/row names in plot
  dlabels<-colnames(x)
  rlabels <- colnames(x)


  if(axis==TRUE){
    # Add col/row names to the plot
    axis(2, at = n:1, tick = FALSE, labels = rlabels, las = 1,
         cex.axis = label.cex)

    axis(1, at = 1:n, tick = FALSE, labels = dlabels, las = 2,
         cex.axis = label.cex)
  }

  # Add lines around the plot
  box(col = "black")


  # Fix the legend
  if(legend){
    legend("topright",
           inset=c(-0.4,0.2),
           legend=levels,
           pch=plot_pch,
           title="Distance partition \n boundaries",
           pt.cex=legend_cex,
           cex=0.6,
           box.lwd = 0,
           box.col = "white",
           col=legend_col)

    # Set back the settings for par
    par(mar=old_mar,xpd=old_xpd)
  }


}
