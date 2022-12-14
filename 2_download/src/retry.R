#' @title Retry evaluating an expression with timeout
#' 
#' @description
#' @title Retry evaluating an expression
#' 
#' @description
#' Function to repeatedly try evaluating an expression, up to a maximum number
#' of retry attempts.
#' 
#' @param expr expression to be evaluated.
#' @param max_tries integer indicating the maximum number of retry attempts.
#' @param sleep_on_error integer indicating how long (in seconds) we should wait 
#' before making another attempt. Defaults to zero.
#' @param verbose logical; should messages about retry status be printed to the
#' console? Defaults to FALSE.  
#' @param ... list object containing additional arguments to be passed to `expr`
#' 
#' @returns 
#' If successful, returns the output of the R expression. If the expression
#' fails to successfully evaluate within the maximum number of attempts, 
#' returns an empty data frame.
#' 
retry <- function(expr, max_tries, sleep_on_error = 0, verbose = FALSE, ...){
  
  # evaluate expr in a loop, with retries
  this_try <- 1
  while(this_try <= max_tries){
    # run the desired function
    result <- tryCatch(expr = do.call(expr, ...), 
                       error = function(ex){
                         return(data.frame())
                       })
    
    # check for success or failure, retrying up to the number of attempts
    # indicated by `max_tries`.
    if(is.data.frame(result) && nrow(result) == 0){
      # print status
      if(verbose && this_try < max_tries){
        message(sprintf("Attempt %s of %s unsuccessful, retrying...", this_try, max_tries))
      }
      if(verbose && this_try == max_tries){
        message(sprintf("Attempt %s of %s unsuccessful, returning an empty data frame", this_try, max_tries))
      }
      # sleep for a while if requested
      if(sleep_on_error > 0){
        Sys.sleep(sleep_on_error)
      }
      this_try <- this_try + 1
    } else {
      if(verbose) message(sprintf("Success! after attempt %s of %s", this_try, max_tries))
      break
    }
  }
  return(result)
}

