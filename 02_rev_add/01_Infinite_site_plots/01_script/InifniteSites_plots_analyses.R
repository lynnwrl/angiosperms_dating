#-------------------#
# CLEAN ENVIRONMENT #
#-------------------#
rm( list = ls( ) )

#-----------------------#
# SET WORKING DIRECTORY #
#-----------------------#
# Load package to help find the path to this source file 
library(rstudioapi) 
library(phytools)

# Get the path of current open file
path_to_file <- getActiveDocumentContext()$path 
# Get working directory path
wd      <- paste( dirname( path_to_file ), "/", sep = "" )
wd.name <- dirname( path_to_file )
# Set wd. Note that the wd will be the same directory where this 
# script is saved.
setwd( wd )
# Load in-house R script
source( file = "Functions.R" )

#--------------------#
# DEFINE GLOBAL VARS #
#--------------------#
# Number of columns in the `mcmc.txt` that are to be deleted as they do not 
# correspond to sample values for divergence times (i.e., the entries are not 
# names following the format `t_nX`). To figure out this number quickly, you 
# can open the `mcmc.txt` file, read the header, and count the number of `mu*`
# and `sigma2*` elements. Do not count the `lnL` value when looking at 
# `mcmc.txt` files generated when sampling from the posterior -- this is 
# automatically accounted for in the in-house R functions that you will 
# subsequently use. E.g., assuming an MCMC ran under a relaxed-clock model with  
# no partitions, we would see `mu` and `sigma2` columns. Therefore, the variable  
# would be set to `delcol_post <- 2`. Please modify the value/s below 
# (depending on having one or more datasets) according to the `mcmc.txt` file
# generated when sampling from the posterior (`delcol_posterior`).
#delcol <- 2 # 1 mu + 1 sigma
delcol <- 6

# Quantile percentage that you want to set. By default, the variable below is 
# set to 0.975 so the 97.5% and 2.5% quantiles (i.e., 95%CI). If you want to
# change this, however, just modify the value.
perc <- 0.975
# Number of samples that you specified in the `MCMCtree` control file to 
# collect. NOTE: you may have not collect them all, but do not worry!
def_samples <- 20000

#-----------#
# LOAD DATA #
#-----------#
# Load data from the posterior (prior = FALSE)
divt_run1 <- load_dat( mcmc = "mcmc_1.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run2 <- load_dat( mcmc = "mcmc_2.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run3 <- load_dat( mcmc = "mcmc_3.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run4 <- load_dat( mcmc = "mcmc_4.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run5 <- load_dat( mcmc = "mcmc_5.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run6 <- load_dat( mcmc = "mcmc_6.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run7 <- load_dat( mcmc = "mcmc_7.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run8 <- load_dat( mcmc = "mcmc_8.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run9 <- load_dat( mcmc = "mcmc_9.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )
divt_run10 <- load_dat( mcmc = "mcmc_10.txt", 
                       delcol = delcol, perc = perc, def_samples = def_samples,
                       prior = FALSE )

# Calculate mean t and quantiles for 95%CI
CIwidth_run1 <- as.matrix( divt_run1$quant_divt[,2] - divt_run1$quant_divt[,1],
                           ncol = 1 )
CIwidth_run2 <- as.matrix( divt_run2$quant_divt[,2] - divt_run2$quant_divt[,1],
                           ncol = 1 )
CIwidth_run3 <- as.matrix( divt_run3$quant_divt[,2] - divt_run3$quant_divt[,1],
                           ncol = 1 )
CIwidth_run4 <- as.matrix( divt_run4$quant_divt[,2] - divt_run4$quant_divt[,1],
                           ncol = 1 )
CIwidth_run5 <- as.matrix( divt_run5$quant_divt[,2] - divt_run5$quant_divt[,1],
                           ncol = 1 )
CIwidth_run6 <- as.matrix( divt_run6$quant_divt[,2] - divt_run6$quant_divt[,1],
                           ncol = 1 )
CIwidth_run7 <- as.matrix( divt_run7$quant_divt[,2] - divt_run7$quant_divt[,1],
                           ncol = 1 )
CIwidth_run8 <- as.matrix( divt_run8$quant_divt[,2] - divt_run8$quant_divt[,1],
                           ncol = 1 )
CIwidth_run9 <- as.matrix( divt_run9$quant_divt[,2] - divt_run9$quant_divt[,1],
                           ncol = 1 )
CIwidth_run10 <- as.matrix( divt_run10$quant_divt[,2] - divt_run10$quant_divt[,1],
                           ncol = 1 )

# Merge chains and extract divtimes

# with all nodes
merged_divt_raw  <- rbind( divt_run1$divt, divt_run2$divt, divt_run3$divt, divt_run4$divt, divt_run5$divt,
                       divt_run6$divt, divt_run7$divt, divt_run8$divt, divt_run9$divt, divt_run10$divt) 


# ----------------------------------------------
# if calculate the angiosperm tree ONLY, run this snippet
# exclude node 645-647 (nodes include outgroup clades), 1279-1287 (nodes within outgroups)
# exclude rows named 't_n645~t_n647, t_n1279~t_n1287'
drop_cols <- paste0("t_n", c(645:647, 1279:1287))
# new data
merged_divt <- merged_divt_raw[, !(colnames(merged_divt_raw) %in% drop_cols), drop = FALSE]
# ----------------------------------------------


# Calculate mean t and quantiles
merged_meant   <- apply( X = merged_divt, MARGIN = 2, FUN = mean )
merged_quant   <- t( apply( X = merged_divt, MARGIN = 2,
                            FUN = quantile, probs = c(0.025,0.975) ) )
merged_CIwidth <- as.matrix( merged_quant[,2] - merged_quant[,1] )

#----------------------#
# PLOT INIFINITE SITES #
#----------------------#
# 1. Plot all values, including the root
# VALS: x --> mean divt | y --> CI width
plot( x = merged_meant*100, y = merged_CIwidth*100,
      main = "Infinite-sites plot | ILN",
      xlab = expression( paste( hat( italic(t) ), " | MCMC - merged" ) ),
      ylab = expression( atop( 'Posterior CI-width, '*italic(w)*'| MCMC - merged runs' ) ),
      cex.lab = 1.5, 
      pch = 19, cex = 0.8 )
abline( lm( c( merged_CIwidth*100 ) ~ 0 + c( merged_meant*100 ) ),
        col="red", lty = 1 )
lm.GBM <- lm( c( merged_CIwidth*100)~0 + c( merged_meant*100 ) )
summary( lm.GBM )
# Check values for:
#  - R^2: "Multiple R-squared" 
#  - w: "Estimated Std" under Coefficients 
# Then, replace accordingly in the two functions below:
text( 
      #x = 250, y = 130, # all
      x = 100, y = 85, # angio
      labels = "Root included",
      col = "red" )
# Add R^2
text( 
      #x = 250, y = 115, # all
      x = 100, y = 77, # angio
      labels = expression( atop( ''*italic( R^2 )*' = 0.475' ) ), 
      col = "red" )
# Add equation of the regression line with the root
text( 
      #x = 250, y = 108, # all
      x = 100, y = 73, # angio
      labels = expression( atop( ''*italic( w )*' = 0.280'*italic( t )*'' ) ), 
      col = "red" )
# Another function to validate the R^2 and w values:
rto.estimates( x = c( merged_meant*100 ),
               y = c( merged_CIwidth*100 ) )


# 2. Now, add a 2nd regression line WITHOUT the root
abline( lm( c( merged_CIwidth*100 )[-1] ~ 0 +  c(  merged_meant*100 )[-1] ),
        col="black", lty = 2 )
lm.GBM.noroot <- lm( c( merged_CIwidth*100 )[-1] ~ 0 + 
                       c( merged_meant*100 )[-1] )
summary( lm.GBM.noroot )
# Check values for:
#  - R^2: "Multiple R-squared" 
#  - w: "Estimated Std" under Coefficients 
# Then, replace accordingly in the two functions below:
text( 
      #x = 350, y = 130, # all
      x = 100, y = 85, # angio
      labels = "Without the root",
      col = "black" )
#  Add R^2
text( 
      #x = 350, y = 115, # all
      x = 100, y = 77, # angio
      labels = expression( atop( ''*italic( R^2 )*' = 0.478' ) ) ) 
# Add equation of the regression line with the root
text( 
      #x = 350, y = 108, # all
      x = 100, y = 69, # angio
      labels = expression( atop( '('*italic( w )*' = 0.282'*italic( t )*')' ) ) ) 
rto.estimates( x = c( merged_meant*100 )[-1] ,
               y = c( merged_CIwidth*100 )[-1] )
