register_seriate_ga<-function(){


  installed <- function(pkg) !is(try(utils::installed.packages()[pkg,],
                                      silent=TRUE), "try-error")

  if (!installed("GA"))
    stop("Package 'GA' needs to be installed!")
  else require(GA)

  if (!installed("seriation"))
    stop("Package 'seriation' needs to be installed!")
  else require(seriation)



  if(is.null(try(seriation::show_seriation_methods("dist")$dist_seriate_ga))){
    seriation::set_seriation_method(kind="dist",
                                    name="seriate_ga",
                                    definition=RMaCzek::seriate_ga,
                                    description="Genetic Algorithm for permutation")

    #cat("Seriation method 'seriate_ga' is now set! \n")
  }

  #cat("Use 'seriate(x,method='seriate_ga')' to use the method!")
}
