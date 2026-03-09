# this code should run under folder '01_MCMCTree/00_Calibration'

#-------------------#
# CLEAN ENVIRONMENT #
#-------------------#
rm( list = ls( ) )

#-----------------------#
# SET WORKING DIRECTORY #
#-----------------------#
library( rstudioapi ) 
# Get the path to current open R script and find main dir
path_to_file <- getActiveDocumentContext()$path
wd <- paste( dirname( path_to_file ), "/", sep = "" )
BB_logs_dir <- paste( wd, "../../00_BBB/01_Result/00_logs/", sep = "" )
setwd( wd )

#--------------#
# LOAD LIBRARY #
#--------------#
library( sn )

#---------------------#
# LOAD AND PARSE DATA #
#---------------------#
# 1. Load files output from the BBB analysis
#    Issue: It seems that the header has an extra `\t` to separate one of the 
#    elements. I will read and format the files using the following commands
name_BBBdir <- c( "families_65", "orders_35", "whole_angiosperms" )
name_logf   <- name_calibs <- root_ests <- vector( mode = "list",
                                                   length = length(name_BBBdir) )
time_unit   <- 100 # 100 Mya
for( i in 1:length( name_BBBdir ) ){
  cat( "\n[[ Parsing BBBdir ", name_BBBdir[i], "]]\n" )
  name_logf[[ i ]]   <- list.files( include.dirs = TRUE, pattern = "*log",
                                    recursive = TRUE,
                                    path = paste( BB_logs_dir, name_BBBdir[i],
                                                  sep = "" ) )
  name_calibs[[ i ]] <- tolower( gsub( x = name_logf[[ i ]],
                                       pattern = "_mcmc..*",
                                       replacement = "" ) )
  # Create one entry per log file in each i_th directory
  root_ests[[ i ]]   <- vector( mode = "list",
                                length = length( name_logf[[ i ]] ) )
  names( root_ests[[ i ]] ) <- name_calibs[[ i ]]
}
# Now, read all the log files for each of the three directories
for( j in 1:length( name_BBBdir ) ){
  cat( "\n[[ Parsing directory ", name_BBBdir[j], "]]\n" )
  for( i in 1:length( name_logf[[ j ]] ) ){
    # First read and format the header accordingly
    cat( "--> Formatting file ", name_logf[[j]][i], "\n" )
    header <- readLines( con = paste( BB_logs_dir, name_BBBdir[j], "/",
                                      name_logf[[j]][i], sep = "" ), n = 1 )
    header <- gsub( pattern = "\t\t", replacement = "\t", x = header )
    header <- stringr::str_split( string = header, pattern = "\t" )[[1]]
    # Now read the file in position "i" in vector `name_logf`
    f <- read.table( file = paste( BB_logs_dir, name_BBBdir[j], "/",
                                   name_logf[[j]][i], sep = "" ),
                     header = F, skip = 1, sep = "\t" )
    colnames( f ) <- header
    # Save desired column (i.e., header = `root_est`) in vector
    # E.g., 100 Myr, value specified above by user
    root_ests[[j]][[i]] <- f$root_est/time_unit
  }
}

# 2. Get mean and quantiles
mean_est_divt <- quant_est_divt <- vector( mode = "list",
                                           length = length( name_BBBdir ) )
names( mean_est_divt ) <- names( quant_est_divt ) <- name_BBBdir
for( i in 1:length( name_BBBdir ) ){
  cat( "\n[[ Calculating mean divt and quantiles for logs in directory ",
       name_BBBdir[i], "]]\n" )
  mean_est_divt[[ i ]]  <- lapply( X = root_ests[[ i ]], MARGIN = 1,
                                   FUN = mean )
  quant_est_divt[[ i ]] <- lapply( X = root_ests[[ i ]], MARGIN = 1,
                                   FUN = quantile, probs = c(0.025,0.975) )
}
# Create file structure for output files that will be subsequently generated
if( ! dir.exists( "00_fitST" ) ){
  dir.create( "00_fitST" )
}
if( ! dir.exists( "00_fitST/logs" ) ){
  dir.create( "00_fitST/logs" )
}
if( ! dir.exists( "00_fitST/plots" ) ){
  dir.create( "00_fitST/plots" )
}
if( ! dir.exists( "00_fitST/Rdata" ) ){
  dir.create( "00_fitST/Rdata" )
}
if( ! dir.exists( "00_fitST/Rout" ) ){
  dir.create( "00_fitST/Rout" )
}
# Save object with post.divtimes to later be used
save( root_ests, file = "00_fitST/Rdata/post.divtimes.RData" )

#---------------------------#
# FUNCTION TO FIND ST-FITS  #
#---------------------------#
# Run the following function for each directory
for( k in 1:length( name_BBBdir ) ){
  # 1. Create list to store fitted ST-distributions and the quantiles
  cat( "\n\n[[ Working with files in directory ", name_BBBdir[k], "]]\n" )
  ST.fitted.dists          <- vector( mode = "list",
                                      length = length( root_ests[[ k ]] ) )
  names( ST.fitted.dists ) <- names( root_ests[[ k ]] )
  ST.fitted.objects          <- vector( mode = "list",
                                        length = length( root_ests[[ k ]] ) )
  names( ST.fitted.objects ) <- names( root_ests[[ k ]] )
  quant_95CI <- vector( mode = "list",
                       length = length( root_ests[[ k ]] ) )
  names( quant_95CI ) <- names( root_ests[[ k ]] )
  
  # 2. Set seed and start loop 
  set.seed( 12345 )
  for ( i in 1:length( root_ests[[ k ]] ) ){
    
    # 0. Get the 95%CI
    tmp_node_quants <- quantile( x = root_ests[[ k ]][[i]],
                                 probs = c(0.025,0.975) )
    # 1. Fit an ST distribution to each node. Then save in 
    #    the lists previously created both the object
    #    output by sn::st.mple and only the "dp" pars
    cat( "\n--> Working with calibration \"", names( root_ests[[ k ]] )[i],
         "\" ..." )
    write( paste( "Working with calibration \"", names( root_ests[[ k ]] )[i],
                  "\" ...", sep = "" ),
           file = paste( "00_fitST/logs/log_file_convergence_BFGS_",
                         name_BBBdir[k], ".txt", sep = "" ), 
           sep = "\n", append = TRUE )
    tmp_node <- sn::st.mple( y = root_ests[[ k ]][[i]], opt.method = "BFGS" )
    
    # 2. Check for convergence, otherwise keep trying
    count_tries_conv <- 1
    while( tmp_node$opt.method$convergence != 0 ){
      count_tries_conv <- count_tries_conv + 1
      cat( "\n---->> Convergence has not been reached for calibration \"",
           names( root_ests[[ k ]] )[i],
           "\"...\n      SEARCH NUMBER", count_tries_conv, "...\n",
           "       The parameters found in the previous search:\n       ",
           tmp_node$dp[1], "|", tmp_node$dp[2], "|",tmp_node$dp[3],
           "|",tmp_node$dp[4],  "\n       are now used as starting values\n\n" )
      write( paste( "Convergence has not been reached for calibration \"",
                    names( root_ests[[ k ]] )[i],
                    "\"...\nSEARCH NUMBER", count_tries_conv, "...\n",
                    "The parameters found in the previous search are used\n",
                    "as starting values now:\n",
                    tmp_node$dp[1], "|", tmp_node$dp[2], "|",tmp_node$dp[3],
                    "|",tmp_node$dp[4], "\n\n", sep = "" ),
             file = paste( "00_fitST/logs/log_file_convergence_BFGS_",
                           name_BBBdir[k], ".txt", sep = "" ), 
             sep = "\n", append = TRUE )
      # Get fitted ST dist for node `root_ests[[k]][[i]]`
      tmp_node <- sn::st.mple( y = root_ests[[ k ]][[i]], opt.method = "BFGS",
                               dp = tmp_node$dp.complete )
      if( tmp_node$opt.method$convergence == 0 ){
        cat( "---->> Convergenced reached now!\n" )
        cat( "---->> Final parameters for calibration \"",
             names( root_ests[[ k ]] )[i], "\" are:\n       ",
             tmp_node$dp[1], "|", tmp_node$dp[2], "|",tmp_node$dp[3],
             "|",tmp_node$dp[4],"\n\n" )
        write( paste( "Convergenced reached now!\n\n", 
                      "Final parameters for calibration \"",
                      names( root_ests[[ k ]] )[i], "\" are:\n",
                      tmp_node$dp[1], "|", tmp_node$dp[2], "|",
                      tmp_node$dp[3], "|", tmp_node$dp[4],"\n\n", sep = "" ),
               file = paste( "00_fitST/logs/log_file_convergence_BFGS_",
                             name_BBBdir[k], ".txt", sep = "" ),
               sep = "\n", append = TRUE )
      }
      if( count_tries_conv == 50 ){
        cat( "\n---->> You have tried 50 times\n",
             "       We will try the optimizing approach within this ",
             "function...\n" )
        write( paste( "You have tried 50 times\n",
                      "We will try the optimizing approach within this",
                      " function...\n", sep = "" ),
               file = paste( "00_fitST/logs/log_file_convergence_BFGS_",
                             name_BBBdir[k], ".txt", sep = "" ),
               sep = "\n", append = TRUE )
        count_tries_conv_FUN <- 0
        while( tmp_node$opt.method$convergence != 0 ){
          
          count_tries_conv_FUN <- count_tries_conv_FUN + 1
          tmp_node <- sn::st.mple( y = root_ests[[ k ]][[i]],
                                   dp = tmp_node$dp.complete )
          if( tmp_node$opt.method$convergence == 0 ){
            cat( "---->> Convergenced reached with their method now!\n" )
            cat( "---->> Final parameters for calibration \"",
                 names( root_ests[[ k ]] )[i], "\"are :\n    ",
                 tmp_node$dp[1], "|", tmp_node$dp[2], "|",tmp_node$dp[3], "|",
                 tmp_node$dp[4],"\n\n" )
            write( paste( "Convergenced reached with their method now!\n\n", 
                          "Final parameters for calibration \"",
                          names( root_ests[[ k ]] )[i], "\"are :\n",
                          tmp_node$dp[1], "|", tmp_node$dp[2], "|",
                          tmp_node$dp[3], "|",
                          tmp_node$dp[4],"\n\n", sep = "" ),
                   file = paste( "00_fitST/logs/log_file_convergence_BFGS_",
                                 name_BBBdir[k], ".txt", sep = "" ),
                   sep = "\n", append = TRUE )
          }
          if( count_tries_conv_FUN == 50 ){
            cat( "---->> You have tried 50 times with their approach,",
                 "this is going to be killed\n" )
            write( paste( "You have tried 50 times with their approach,",
                          "this is going to be killed\n", sep = "" ),
                   file = paste( "00_fitST/logs/log_file_convergence_BFGS_",
                                 name_BBBdir[k], ".txt", sep = "" ),
                   sep = "\n", append = TRUE )
            break
          }
        }
      }
    }
    # Get data 
    ST.fitted.objects[[ i ]] <- tmp_node
    ST.fitted.dists[[ i ]]   <- tmp_node$dp
    quant_95CI[[ i ]]        <- tmp_node_quants
      
    # 3. Plot fitted ST for each node previously 
    #    computed using the values sampled during the MCMC
    png( filename = paste( "00_fitST/plots/Fit_ST_",
                           names( root_ests[[ k ]] )[i], "_",
                           name_BBBdir[k], ".png", sep = "" ),
         width = 1024, height = 768 )
    # 3.1. Find limit axis
    ext_len     <- 0.1 # Adjust according to the first time you plot -- 
                       # it will help to properly adjust automatically
                       # xlim and ylim
    max_x_st    <- round( max( density( rst( n = 1000,
                                             dp = tmp_node$dp.complete ),
                                        adj = 1 )$x ) + 0.5, digits = 1 )
    max_x_chain <- round( max( density( root_ests[[ k ]][[i]], adj = 1 )$x ) + 
                            ext_len, digits = 1 )
    x_lim       <- max( max_x_chain, max_x_st )
    max_y_st    <- round( max( density( rst( n = 1000,
                                             dp = tmp_node$dp.complete ),
                                        adj = 1 )$y ), digits = 1 )
    max_y_chain <- round( max( density( root_ests[[ k ]][[i]], adj = 1 )$y ) + 
                            ext_len, digits = 1 )
    y_lim       <- max( max_y_chain, max_y_st )
    write( paste( names( root_ests[[ k ]] )[i], max_x_chain, max_y_chain,
                  sep = "\t" ),
           file = paste( "00_fitST/logs/log_limaxis_", name_BBBdir[k],
                         ".txt", sep = "" ),
           sep = "\n", append = TRUE )
    # 3.2. Plot
    plot( density( root_ests[[ k ]][[i]], adj = 1 ),
          xlim = c( 0, x_lim ), ylim = c( 0, y_lim ), 
          main = paste( names( root_ests[[ k ]] )[i], " = ",
                        "ST(", paste0( round(tmp_node$dp, 2),
                                       collapse = "," ),
                        ") | q2.5% = ", round( tmp_node_quants[1], 2),
                        " | q97.5% = ", round( tmp_node_quants[2], 2), sep = "" ) )
    curve( dst( x, xi = tmp_node$dp[1], omega = tmp_node$dp[2],
                alpha = tmp_node$dp[3], nu = tmp_node$dp[4] ),
           from = 0, to = x_lim,
           n = 1e4, add = TRUE, col = "red" )
    abline( v = tmp_node_quants, col = "purple", lty = 3 )
    dev.off()
  }
  
  # 4. Save objects so they can be later used for extra plots
  save( ST.fitted.objects, file = paste( "00_fitST/Rdata/ST_fitted_objects_",
                                         name_BBBdir[k], ".RData", sep = "" ) )
  save( ST.fitted.dists, file = paste( "00_fitST/Rdata/ST_fitted_dists_",
                                       name_BBBdir[k], ".RData", sep = "" ) )
  save( quant_95CI, file = paste( "00_fitST/Rdata/quant95CI_",
                                       name_BBBdir[k], ".RData", sep = "" ) )

  # 5. Transform list into a matrix
  mat_ST <- matrix( 0, nrow = length( ST.fitted.dists ), ncol = 8 )
  colnames( mat_ST ) <- c( "xi-location", "omega-scale", "alpha-shape", "nu-df",
                           "MCMCtree-calib", "MCMCtree-calib-rounded", "95%CI",
                           "MCMCtree-hardbounds" )
  rownames( mat_ST ) <- names( ST.fitted.dists )
  for ( i in 1:length( ST.fitted.dists ) ){
    mat_ST[i,1] <- ST.fitted.dists[[ i ]][1]
    mat_ST[i,2] <- ST.fitted.dists[[ i ]][2]
    mat_ST[i,3] <- ST.fitted.dists[[ i ]][3]
    mat_ST[i,4] <- ST.fitted.dists[[ i ]][4]
    tmp.ST.fitted.dists.rounded <- round( ST.fitted.dists[[ i ]], 3 )
    mat_ST[i,5] <- paste( "ST(", ST.fitted.dists[[ i ]][1], ",",
                          ST.fitted.dists[[ i ]][2],
                          ",", ST.fitted.dists[[ i ]][3], ",",
                          ST.fitted.dists[[ i ]][4], ")",
                          sep = "" )
    mat_ST[i,6] <- paste( "'ST(", tmp.ST.fitted.dists.rounded[1], ",",
                          tmp.ST.fitted.dists.rounded[2], ",",
                          tmp.ST.fitted.dists.rounded[3],
                          ",", tmp.ST.fitted.dists.rounded[4], ")'", sep = "" )
    mat_ST[i,7] <- paste( "q2.5%-",round( quant_95CI[[ i ]][1], 2), "|q97.5%-",
                          round( quant_95CI[[ i ]][2],2 ),
                          sep = "" )
    mat_ST[i,8] <- paste( "B(", round( quant_95CI[[ i ]][1], 2), ",",
                          round( quant_95CI[[ i ]][2], 2),
                          ",1e-300,1e-300)", sep = "")
  }
  
  # 6. Output table
  write.table( mat_ST, file = paste( "00_fitST/Rout/ST_fitted_dists_",
                                     name_BBBdir[k], ".tsv", sep = "" ),
               sep = "\t", quote = F )
}

