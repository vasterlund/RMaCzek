#'@title Divied a distance matrix into classes
#'@description This is a function that divied the values insied a distance matrix into classes.
#'@param x  a numeric matrix, data frame or "dist" object.
#'@param order the order of the observations. If TRUE the seriation function with method=SPIN_STS is used, if FALSE the present order in the distance matrix is used, if a function the function is expected to return the order the user want to use.
#'@param n_classes number of classes the distances should be divided into.
#'@param sizes_cex if col=FALSE the user can specify the sizes of cex
#'@param col if a vector with colour the classes will be expressed in colours, if TRUE standard colours will be used, if FALSE the matrix will be expressed in numbers
#'@param interval_breaks if "equal_width_between_classes" equal width between classes will be used, the user can specify the partition boundaries. If interval_breaks is not specified, equal amount of distances is divided into every class.
#'@param monitor  "plot" and "cumulativ_plot" is available.
#'@param dist_function a user defined distance function. If not specified the dist function will be used.
#'@param scale_data if the data should be scaled or not.
#'@param ... parameters to be passed into the dist or dist_function function.
#'@export
#'@return The function return a matrix with class czek_matrix. The return from the function is expected to be passed to the Czekanowski_plot function.


czek_matrix <- function(x,
                        order=TRUE,
                        n_classes = 5,
                        interval_breaks=NULL,
                        monitor="plot",
                        distfun=dist,
                        scale_data=TRUE,
                        control_ga=list(),
                        ...){

  # If not of class dist, make the data to class dist ####
  if(class(x)!="dist"){

    # Scale data
    if(scale_data){
      x<-scale(x)
    }

    # Calculate a distance matrix
    x<-distfun(x,...)

  }


  # Seriation part ####
  # If the user have specified the order
  if(class(order)=="integer"){

    #Just for conviction
    new_order<-order
  }

  # If standard settings is used
  else if (order[1]==TRUE | order[1]=="ga"){
    new_order<-seriate_ga(x,control_ga)
  }

  # If standard settings is used
  else if (class(order[1])=="character"){
    new_order<-seriation::get_order(seriation::seriate(x,method=order))
  }




  # If the user dont want to change the order
  else new_order<-1:attr(x,"Size")


  # Change the class to matrix ####
  x<-as.matrix(x)


  # Find the partition bounderies ####

  # If ther dont have specified the interval breaks
  if(is.null(interval_breaks)) {

    # If NOT the user have specified the intervals
    # Given 5 classes: 20% of the distance in class 1,..., 20% of the distance in class 5
    interval_breaks<-quantile(x[upper.tri(x)], probs=seq(0,1,length.out = n_classes+1), na.rm=TRUE)
    interval_breaks[1]<-0

  }

  # If the user want equal width brettwen classes
  else if("equal_width_between_classes"%in%interval_breaks){

    interval_breaks<-max(x[upper.tri(x)])/n_classes*(0:n_classes)
    probs<-ecdf(x[upper.tri(x)])(interval_breaks)

    names(interval_breaks)<-paste(round(probs,7)*100,"%",sep="")

  }

  # If interval_breaks is specified in procent
  else if(sum(interval_breaks)==1){

    probs<-c(0,cumsum(interval_breaks))
    interval_breaks<-quantile(x[upper.tri(x)], probs=probs, na.rm=TRUE)
    interval_breaks[1]<-0
  }

  #If the user have specified the intervals
  else{

    interval_breaks[1]<-0
    interval_breaks[length(interval_breaks)]<-max(x)

    probs<-ecdf(x[upper.tri(x)])(interval_breaks)
    names(interval_breaks)<-paste(round(probs,7)*100,"%",sep="")
  }


  # Split the distances into classes ####

  # Make the partition boundaries
  cut_the_values <- cut(x, interval_breaks, include.lowest = TRUE)

  # Make the matrix that we want to plot later on
  czek_matrix<- matrix(as.numeric(cut_the_values),ncol = ncol(x))


  # attr information to the matrix with classes ####

  # Add the partition boundaries to the matrix
  attr(czek_matrix, "levels") <- levels(cut_the_values)
  attr(czek_matrix, "partition_boundaries")<-interval_breaks
  attr(czek_matrix, "order")<-new_order
  attr(czek_matrix, "n_classes")<-length(levels(czek_matrix))


  # Add row/col names to the color matrix
  rownames(czek_matrix)<-rownames(x)
  colnames(czek_matrix)<-colnames(x)


  # Monitor ####
  if(monitor%in%c("plot","cumulativ_plot")){

    cum_probs<-as.numeric(gsub(pattern = "%",replacement = "",x = names(interval_breaks)))
    plot_values<-cum_probs[-1]
    my_title<-"Cumulative distribution of distances in each class"

    if(monitor=="plot"){
      probs<-c()
      for(i in 2:(length(cum_probs))){
        probs[i-1]<-cum_probs[i]-cum_probs[i-1]
      }
      plot_values<-probs
      my_title<-"Distribution of distances in classes"
    }

    names(plot_values)<-levels(cut_the_values)
    barplot(plot_values,
            main=my_title,
            col=c("grey30"),
            xlab = "Classes of distances",
            ylim = c(0,100),yaxt="n")

    axis(2, at = seq(0, 100, by = 10),
         labels=paste(seq(0, 100, by = 10),"%",sep=""),
         las=2)

    box(col = "black")
  }


  # Change class and return ####
  # Change class
  class(czek_matrix)<-"czek_matrix"

  # Return
  return(czek_matrix)
}

