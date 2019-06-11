#'@title Preprocess data to produce a Czekanowski’s Diagram
#'@description This is a function that divied the values insied a distance matrix into classes. The output can be used in the plot function to producce a Czekanowski’s Diagram.
#'@param x  a numeric matrix, data frame or a 'dist' object.
#'@param order specifies which seriation method should be applied. Standard setting is the seriation method OLO.
#'@param n_classes specifies how many classes the distances should be divided into. Standard setting is 5 classes.
#'@param interval_breaks specifies the partition boundaries for the distances. As a standard setting, each class represents an equal amount of distances.
#'@param monitor  specifies if the distribution of the distances should be visualized. Standard setting is that the distribution will not be visualized. TRUE and "cumulativ_plot" is available.
#'@param distfun specifies which distance function should be used. Standard setting is the dist function which uses the Euclidean distance.
#'@param scale_data specifies if the data set should be scaled. Standard setting is thatthe data will be scaled.
#'@param ... specifies further parameters that can be passed on to the seriatefunction in the seriation package.
#'@export
#'@return The function return a matrix with class czek_matrix. The return from the function is expected to be passed to the plot function.
#'@examples
#'# Set data ####
#'x<-mtcars
#'
#'
#'# Different type of input that give same result ############
#'czek_matrix(x)
#'czek_matrix(dist(scale(x)))
#'
#'
#'# Change seriation method ############
#'#seriation::show_seriation_methods("dist")
#'czek_matrix(x,order = "GW")
#'czek_matrix(x,order = "ga")
#'czek_matrix(x,order = sample(1:nrow(x)))
#'
#'
#'# Change number of classes ############
#'czek_matrix(x,n_classes = 3)
#'
#'
#'# Change the partition boundaries ############
#'czek_matrix(x,interval_breaks = c(0.1,0.4,0.5)) #10%, 40% and 50%
#'czek_matrix(x,interval_breaks = c(0,1,4,6,8.48)) #[0,1] (1,4] (4,6] (6,8.48]
#'czek_matrix(x,interval_breaks = "equal_width_between_classes") #[0,1.7] (1.7,3.39]  (3.39,5.09] (5.09,6.78] (6.78,8.48]
#'
#'
#'# Change number of classes ############
#'czek_matrix(x,monitor = TRUE)
#'czek_matrix(x,monitor = "cumulativ_plot")
#'
#'
#'# Change distance function ############
#'czek_matrix(x,distfun = function(x) dist(x,method = "manhattan"))
#'
#'
#'# Change dont scale the data ############
#'czek_matrix(x,scale_data = FALSE)
#'czek_matrix(dist(x))
#'
#'
#'# Change additinal settings to the seriation method ############
#'czek_matrix(x,order="ga",control=list(popSize=200,
#'                                      suggestions=c("SPIN_STS","QAP_2SUM")))


czek_matrix <- function(x,
                        order="OLO",
                        n_classes = 5,
                        interval_breaks=NULL,
                        monitor=FALSE,
                        distfun=dist,
                        scale_data=TRUE,
                        ...){

  # If not of class dist, make the data to class dist ####
  if(class(x)!="dist"){

    # Scale data
    if(scale_data){
      x<-scale(x)
    }

    # Calculate a distance matrix
    x<-distfun(x)

  }


  # Seriation part ####
  # If the user have specified the order
  if(class(order)=="integer"){

    #Just for conviction
    new_order<-order
  }



  # If standard settings is used
  else if (class(order[1])=="character"){



    if (!.installed("seriation"))
      stop("Package 'seriation' needs to be installed!")

    # If standard settings is used
    if (order[1]=="ga"){
      register_seriate_ga()
      order<-"seriate_ga"
    }

    new_order<-seriation::get_order(seriation::seriate(x,method=order[1],...))
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
  if(monitor%in%c(TRUE,"cumulativ_plot")){

    if(monitor==TRUE)
      monitor<-"plot"

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

