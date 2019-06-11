.get_parameters <- function(parameter, defaults) {
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

.installed <- function(pkg) !is(try(utils::installed.packages()[pkg,],
                                    silent=TRUE), "try-error")
