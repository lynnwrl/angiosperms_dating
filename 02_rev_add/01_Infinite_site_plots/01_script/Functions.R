# Function to load the `mcmc.txt` files.
# Used to get mean estimates for convergence plots.
# 
# Parameters:
# mcmc         Character, path to `mcmc.txt` file.
# delcol       Numeric, number of columns that are to be deleted 
#              as they do not contain divtimes. You can read the header
#              of the `mcmc.txt` files and count the number of `mu*` and
#             `sigma2*` elements. Do not count the `lnL` value as this is
#              automatically deleted in the calculations below. 
#              Assuming an MCMC run under a relaxed-clock model with no 
#              partitions, we would see `mu` and `sigma2` columns. Therefore,
#              `delcol = 2`. The default value is 2, but you will need to change
#              it if you run `MCMCtree` with different settings.
# perc         Numeric. Percentile to calculate the quantiles. Default: 0.975.
# def_samples  Numeric. Number of samples that the user defined through the
#              `MCMCtree` option `nsample. 
# prior        Boolean. TRUE if `MCMCtree` has been sampling from the prior, FALSE
#              otherwise
load_dat <- function( mcmc, delcol = 3, perc = 0.975, def_samples = 20000,
                      prior = FALSE )
{
  
  # 1. Load files and get parameters. Load everything with option `fill=TRUE`
  # in case a line is not complete
  cat( "Load \"mcmc.txt\" from path \n", mcmc, "\n... ...\n" )
  run_tmp  <- read.table( mcmc, header = TRUE, fill = TRUE, sep = "\t" )
  test_len <- def_samples+1
  #print( test_len )
  #print( dim(run_tmp))
  if( test_len != dim(run_tmp)[1] ){
    cat( " [[ The last line of the \"mcmc.txt\" is incomplete and will be removed ]] \n\n" )
    run <- matrix( 0, nrow = c(dim( run_tmp )[1]-1), ncol = dim( run_tmp )[2] )
    run <- run_tmp[1:c(dim( run_tmp )[1]-1),]
  }else{
    cat( " [[ MCMCtree collected the same amount of samples you specified ]] \n",
         " [[       All lines will be kept in the \"mcmc.txt\"            ]] \n\n" )
    run <- run_tmp
  }
  
  # 2. Summarise parameters
  cat( "Generate objects with summarised estimated divergence times... ...\n")
  dim.r   <- dim(run)[2]
  # divtimes <- run[,-c(1, dim(run)[2])]
  # If `MCMCtree` has sampled from the prior, then the `lnL` column will not
  # appear and we need to subtract `1` from the `delcol`
  if( prior == TRUE ){
    delcol <- delcol - 1
    divtimes <- run[,-c( 1, ( dim.r-delcol ):dim.r )]
  }else{
    divtimes <- run[,-c( 1, ( dim.r-delcol ):dim.r )]
  }
  # Calculate mean and quantiles
  mean_est_divt  <- apply( X = divtimes, MARGIN = 2, FUN = mean )
  quant_est_divt <- apply( X = divtimes, MARGIN = 2, FUN = quantile, probs = c( 1-perc,perc ) )
  quant_est_divt <- t( quant_est_divt )
  test_alleq     <- all.equal( quant_est_divt[1,], quantile( divtimes[,1], probs = c( 1-perc,perc ) ) )
  if( test_alleq != TRUE ){
    stop( "There was an issue calculating quantiles!" )
  }
  
  # 3. Return object 
  cat( "\nTasks done! Returning objects\n\n")
  return( list( divt = divtimes, mean_divt = mean_est_divt, quant_divt = quant_est_divt ) )
  
}


# Function to obtain R2 estimates when RTO
# Source: https://rpubs.com/aaronsc32/regression-through-the-origin
# 
# Parameters:
# x Numeric, values for mean div times
# y Numeric, values for CI width
rto.estimates <- function( x, y ) {
  
  b1  <- sum( x * y ) / sum( x^2 )
  ssr <- b1^2 * sum( x^2 )
  sse <- sum( y^2 ) - ssr
  mse <- sse / ( length( x ) - 1 )
  msr <- ssr / 1
  res.std.err <- sqrt( mse )
  f.stat      <- msr / mse
  std.error   <- sqrt( mse / sum( x^2 ) )
  
  r2     <- ssr / ( sse + ssr )
  adj.r2 <- r2 - ( 1 - r2 ) * ( 2 - 1 ) / ( length( x ) - 1 )
  
  res <- data.frame( rbind( b1, res.std.err, f.stat, std.error, r2, adj.r2 ) )
  rownames( res ) <- c( 'b1', 'Residual Standard Error', 'F-Statistic',
                        'b1 Standard Error', 'r-squared', 'Adjusted r-squared' )
  colnames( res ) <- 'Estimates'
  
  print( format( res, scientific = FALSE, digits = 3 ) )
}

