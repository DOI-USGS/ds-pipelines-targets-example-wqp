#' @title Retry evaluating an expression with timeout
#' 
#' @description
#' Function to repeatedly try evaluating an expression with timeout,
#' up to a maximum number of retry attempts.
#' 
#' @param expr expression to be evaluated.
#' @param max_retries integer indicating the maximum number of retry attempts.
#' @param timeout_minutes integer indicating how long (in minutes) the
#' expression is allowed to run before being interrupted by the timeout.
#' @param sleep_on_error integer indicating how long (in seconds) we should wait 
#' before making another attempt. Defaults to zero.
#' @param verbose logical; should messages about retry status be printed to the
#' console? Defaults to FALSE.  
#' @param ... list object containing additional arguments to be passed to `expr`
#' 
#' @returns 
#' If successful, returns the output of the R expression. If the expression
#' fails to successfully evaluate within the allowable elapsed time and 
#' maximum number of attempts, this function will throw an error. 
#' 
retry_with_timeout <- function(expr, ..., max_tries, timeout_minutes, sleep_on_error = 0, verbose = FALSE){
  
  # evaluate expr in a loop, with retries
  this_try <- 1
  
  while(this_try <= max_tries){
    
    # run the desired function, but throw an error if it takes too long
    result <- tryCatch(
      expr = R.utils::withTimeout(expr = do.call(expr, ...), 
                                  timeout = timeout_minutes*60,
                                  onTimeout = "error"),
      error = function(ex){
        return(data.frame())
      }
    )

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






  