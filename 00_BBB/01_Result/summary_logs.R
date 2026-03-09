# this code should run under folder '00_BBB/01_Result'

# clean up the workspace
rm(list=ls())

# libraries
library(HDInterval)
library(jsonlite)
library(tidyverse)

# function
estimate_mode <- function(x) {
d <- density(x)
d$x[which.max(d$y)]
}
summarize_log_files <- function(path, outfile="./family_summary_f0.95.txt", pattern=".log", burnin=0.1){
files = list.files(path, pattern=pattern)
head = "family\tlikelihood\tNobs\tNfossils\troot_obs\text_obs\tDA_count\troot_est\troot_m\troot_M\troot_est_mode\text_est\text_m\text_M\text_est_mode\tq_est\tq_m\tq_M\tq_est_mode\ta_est\ta_m\ta_M\ta_est_mode\tsig2_est\tsig2_m\tsig2_M\tsig2_est_mode\n"
cat(file=outfile, head, sep="\t")
sum_df <- data.frame()
for (i in 1:length(files)){
f=files[i]
print(f)
fam = strsplit(f,"_",fixed=T)[[1]][1]
tbl = read.table(f,h=T, skipNul = TRUE)
tbl = tbl[round(dim(tbl)[1]*burnin):dim(tbl)[1],]
ci = as.data.frame(hdi(tbl))
mean_est = apply(tbl,FUN=mean,2)
mode_est = apply(tbl,FUN=estimate_mode,2)
col_names = c("iteration","posterior","likelihood","prior","Nobs","Nfossils","root_obs","ext_obs","DA_counts","root_est","ext_est","q_est","a_est","sig2_est")
out_line = c(fam, as.numeric(c(mean_est[col_names[c(3,5:10)]])), ci$root_est, mode_est[col_names[10]], as.numeric(mean_est[col_names[11]]), ci$ext_est,
mode_est[col_names[11]], as.numeric(mean_est[col_names[12]]), ci$q_est, mode_est[col_names[12]], as.numeric(mean_est[col_names[13]]), ci$a_est,
mode_est[col_names[13]], as.numeric(mean_est[col_names[14]]), ci$sig2_est, mode_est[col_names[14]], "\n")
cat(file=outfile, out_line, sep="\t",append=T)
sum_df <- rbind(sum_df, out_line)
}
return (sum_df)
}


#1 families
path = "./00_logs/families_65"
setwd(path)
sum_df <- summarize_log_files(path)
colnames(sum_df) = c("family", "likelihood", "Nobs", "Nfossils", "root_obs", "ext_obs", "DA_count", "root_est", "root_m", "root_M", "root_est_mode", "ext_est", "ext_m", "ext_M",
"ext_est_mode", "q_est", "q_m", "q_M", "q_est_mode", "a_est", "a_m", "a_M", "a_est_mode", "sig2_est", "sig2_m", "sig2_M", "sig2_est_mode")
# remove column 28 - empty column
sum_df <- select(sum_df, -28)
write.csv(sum_df, file="./family_summary_f0.95.csv")


#2 orders
path_2 = "./00_logs/orders_35"
setwd(path_2)
sum_df_2 <- summarize_log_files(path_2, outfile = "./order_summary_f0.95.txt")
colnames(sum_df_2) = c("family", "likelihood", "Nobs", "Nfossils", "root_obs", "ext_obs", "DA_count", "root_est", "root_m", "root_M", "root_est_mode", "ext_est", "ext_m", "ext_M",
"ext_est_mode", "q_est", "q_m", "q_M", "q_est_mode", "a_est", "a_m", "a_M", "a_est_mode", "sig2_est", "sig2_m", "sig2_M", "sig2_est_mode")
# remove column 28 - empty column
sum_df_2 <- select(sum_df_2, -28)
write.csv(sum_df_2, file="./order_summary_f0.95.csv")


#3 angiosperm
path_3 = "./00_logs/whole_angiosperms"
setwd(path_2)
sum_df_2 <- summarize_log_files(path_2, outfile = "./angiosperm_summary_f0.95.txt")
colnames(sum_df_2) = c("family", "likelihood", "Nobs", "Nfossils", "root_obs", "ext_obs", "DA_count", "root_est", "root_m", "root_M", "root_est_mode", "ext_est", "ext_m", "ext_M",
"ext_est_mode", "q_est", "q_m", "q_M", "q_est_mode", "a_est", "a_m", "a_M", "a_est_mode", "sig2_est", "sig2_m", "sig2_M", "sig2_est_mode")
# remove column 28 - empty column
sum_df_2 <- select(sum_df_2, -28)
write.csv(sum_df_2, file="./angiosperm_summary_f0.95.csv")
