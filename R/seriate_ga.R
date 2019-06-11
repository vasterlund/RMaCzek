seriate_ga <- function(x, control= list(fitness = RMaCzek::Um_factor,
                                        population =GA::gaControl("permutation")$population,
                                        suggestions = c("QAP2SUM"),
                                        selection = GA::gaControl("permutation")$selection,
                                        crossover = GA::gaControl("permutation")$crossover,
                                        mutation = GA::gaControl("permutation")$mutation,
                                        pcrossover = 0.8,
                                        pmutation = 0.1,
                                        popSize = 50,
                                        maxiter = 100,
                                        run = NULL,
                                        elitism =NULL,
                                        parallel = FALSE,
                                        verbose = FALSE,
                                        output="order")){

  if(!class(x)=="dist"){
    stop("x need to be of class dist")
  }



  get_parameters <- function(parameter, defaults) {
    defaults <- as.list(defaults)
    parameter <- as.list(parameter)

    ## add verbose
    if(is.null(defaults$verbose)) defaults$verbose <- FALSE

    if(length(parameter) != 0) {
      o <- pmatch(names(parameter), names(defaults))

      ## unknown parameter
      if(any(is.na(o))){
        warning(sprintf(ngettext(length(is.na(o)),
                                 "Unknown parameter: %s",
                                 "Unknown parameters: %s"),
                        paste(names(parameter)[is.na(o)],
                              collapse = ", ")), call. = FALSE, immediate. = TRUE)

        cat("Available parameter (with default values):\n")
        #print(defaults)
        cat(rbind(names(defaults)," = ", gsub("\n"," ",as.character(defaults))),
            sep=c("\t"," ","\n"))
      }

      defaults[o[!is.na(o)]] <- parameter[!is.na(o)]
    }

    if(defaults$verbose) {
      cat("Used parameters:\n")
      #print(defaults)
      cat(rbind(names(defaults)," = ",
                strtrim(gsub("\n"," ",as.character(defaults)), 50)),
          sep=c("\t"," ","\n"))
    }

    defaults
  }
  control <- get_parameters(control, list(fitness = RMaCzek::Um_factor,
                                           population =GA::gaControl("permutation")$population,
                                           suggestions = c("SPIN_STS"),
                                           selection = GA::gaControl("permutation")$selection,
                                           crossover = GA::gaControl("permutation")$crossover,
                                           mutation = GA::gaControl("permutation")$mutation,
                                           pcrossover = 0.8,
                                           pmutation = 0.1,
                                           popSize = 50,
                                           maxiter = 400,
                                           run = NULL,
                                           elitism =5,
                                           parallel = FALSE,
                                           verbose = FALSE,
                                           output="order"))

  if(is.null(control$run)){
    control$run<-control$maxiter
  }
  if(is.null(control$elitism)){
    control$elitism<-base::max(1, round(control$popSize*0.05))
  }


  # If the user want to use the seriation package to find a suggestion
  if(is.character(control$suggestions[1])){
    control$suggestions <- t(sapply(control$suggestions,
                            function(method) seriation::get_order(seriation::seriate(x,method = method))))
  }


  if (control$verbose){
    cat("\nStarting GA\n")
  }





  n <- attr(x, "Size")
  result <- GA::ga(type = "permutation",
                   fitness = control$fitness,
                   distMatrix=x,
                   lower = 1,
                   upper = n,
                   population = control$population,
                   selection = control$selection,
                   mutation = control$mutation,
                   crossover = control$crossover,
                   pmutation = control$pmutation,
                   pcrossover = control$pcrossover,
                   suggestions = control$suggestions,
                   names = as.character(1:n),
                   monitor = if (control$verbose)
                     GA::gaMonitor
                   else NULL,
                   parallel = control$parallel,
                   maxiter = control$maxiter,
                   run = control$run,
                   popSize = control$popSize)



  if (control$output=="order"){
    return(as.integer(result@solution[1, ]))
  }
  else if (control$output=="ga"){
    return(result)
  }
}
