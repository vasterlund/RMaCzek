#'@title Evaluate the Um function
#'@description This function evaluate the Um function.
#'@param distMatrix a distance matrix, preferable of class dist or matrix
#'@param order the order
#'@param matrix_conversion_coefficient a positive value added to each distance in order to avoid division by zero.
#'@param inverse_um if the -Um should be returned insted of Um
#'@export
#'@return This function return a evaluation of the Um function.
#'@examples
#'# Make a distance matrix
#'distMatrix<-dist(x = mtcars,diag = TRUE,upper = TRUE)
#'
#'# Evaluate the order
#'Um_v3(distMatrix)

Um_factor<-function(distMatrix,
                order=NULL,
                matrix_conversion_coefficient=1,
                inverse_um=TRUE,
                scale_factor=FALSE){

  # Change the class to matrix
  if(class(distMatrix)!="matrix"){
    distMatrix<-as.matrix(distMatrix)
  }

  if(!is.null(order)){
    distMatrix<-distMatrix[order,order]
  }



  #Dimensions of the distance matrix
  d <- dim(distMatrix)



  #Calculate the over part in Um factor
  over<-((.row(d) -.col(d))^2)[.row(d) > .col(d)]

  #Calculate the under part in Um factor
  under<-distMatrix[.row(d) > .col(d)]+matrix_conversion_coefficient

  # Calculate Um factor
  n<-d[1]
  um_value<-(sum(over/under)*2)/n^2

  if(inverse_um){
    um_value<-(-um_value)
  }

  if(scale_factor=="n^4"){
    um_value<-um_value/nrow(distMatrix)^2
  }


  return(um_value)

}
