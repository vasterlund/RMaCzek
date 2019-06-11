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
