# clean up the workspace
rm(list=ls()) 

# package installing
library(dplyr)
library(purrr)
library(readr)     # Or you can use base::read.table
library(stringr)
library(tibble)
library(tools)
library(ggplot2)
library(coda)

#####################
# summarize results #
#####################

## 1. set root dir of log files ----
log_root <- "."

## 2. List all qvar.log files (recursively) ----
log_files <- list.files(
  path       = log_root,
  pattern    = "qvar\\.log$",
  full.names = TRUE,
  recursive  = TRUE
)

length(log_files)
print(log_files)

## 3. Extract family/retention ratio/replicate from the path and file name ----

parse_filename <- function(path) {
  dir_name <- basename(dirname(path))   # e.g. "subsample_30" / "subsample_70" / "."
  fname    <- basename(path)            # e.g. "fabaceae_1_mcmc_7040_f0.95_qvar.log"
  id       <- file_path_sans_ext(fname) # remove '.log'
  
  # 1) First, analyze family and rep
  # for subsample_30 / subsample_70：
  #   fabaceae_1_mcmc_7040_f0.95_qvar
  # for full：
  #   fabaceae_mcmc_7711_f0.95_qvar
  parts <- str_split(id, "_", simplify = TRUE)
  
  family_raw <- parts[1]
  # Determine if there is "replicate"
  # Under 'subsample': The second object is usually a number (rep)
  # Under full: The second object is usually "mcmc", without rep
  if (ncol(parts) >= 2 && grepl("^[0-9]+$", parts[2])) {
    rep <- as.integer(parts[2])
  } else {
    rep <- 1L   # for full, the rep set to be '1'
  }
  
  # Standardize the family name (capitalize the first letter)
  family <- case_when(
    tolower(family_raw) == "fabaceae"   ~ "Fabaceae",
    tolower(family_raw) == "asteraceae" ~ "Asteraceae",
    TRUE                                ~ str_to_title(family_raw)
  )
  
  # 2) Then determine the retention ratio based on the directory name
  prop_keep <- case_when(
    dir_name == "subsample_30" ~ 0.3,
    dir_name == "subsample_70" ~ 0.7,
    TRUE                       ~ 1.0   # Those not in the subsample folder are regarded as full
  )
  
  tibble(
    file      = path,
    family    = family,
    prop_keep = prop_keep,
    rep       = rep
  )
}

meta <- map_dfr(log_files, parse_filename)

meta
meta %>% arrange(family, prop_keep, rep)
meta %>% count(family, prop_keep)

## 4. Define the function for extracting the root posterior summary from a single log ----
summarise_one_log <- function(file, family, prop_keep, rep,
                              root_colname = NULL) {
  df <- read.table(file, header = TRUE, stringsAsFactors = FALSE)
  
  # 自动识别 root 列名
  if (is.null(root_colname)) {
    if ("root_est" %in% names(df)) {
      root_colname <- "root_est"
    } else {
      stop("Can not find 'root_est', please check file: ", file)
    }
  }
  
  x <- df[[root_colname]]
  x <- x[is.finite(x)]   
  burnin_frac <- 0.1       
  start <- floor(length(x) * burnin_frac) + 1
  x <- x[start:length(x)]
  ## 1) equal-tailed 95% CI
  q <- quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  origin_mean   <- mean(x, na.rm = TRUE)
  origin_median <- q[2]
  origin_L_ci   <- q[1]
  origin_U_ci   <- q[3]
  CI_width      <- origin_U_ci - origin_L_ci
  
  ## 2) 95% HPD interval
  #
  mcmc_x <- as.mcmc(x)
  hpd    <- HPDinterval(mcmc_x, prob = 0.95)  
  
  origin_L_hpd <- hpd[1, "lower"]
  origin_U_hpd <- hpd[1, "upper"]
  HPD_width    <- origin_U_hpd - origin_L_hpd
  
  tibble(
    family        = family,
    prop_keep     = prop_keep,
    rep           = rep,
    origin_mean   = origin_mean,
    origin_med    = origin_median,
    ## equal-tailed CI：
    origin_L_ci   = origin_L_ci,
    origin_U_ci   = origin_U_ci,
    CI_width      = CI_width,
    ## HPD interval：
    origin_L_hpd  = origin_L_hpd,
    origin_U_hpd  = origin_U_hpd,
    HPD_width     = HPD_width,
    n_samples     = length(x),
    file          = file
  )
}


## 5. Summarize all logs ----
summary_tbl <- meta %>%
  mutate(
    res = pmap(
      list(file, family, prop_keep, rep),
      summarise_one_log
    )
  ) %>%
  pull(res) %>%
  bind_rows()

summary_tbl %>% arrange(family, prop_keep, rep)

## 6. Save the result table for subsequent analysis/plotting ----
write.table(
  summary_tbl,
  file = file.path(log_root, "bbb_subsampling_summary.txt"),
  quote = FALSE, row.names = FALSE, sep = "\t"
)

openxlsx::write.xlsx(
  summary_tbl,
  file = file.path(log_root, "bbb_subsampling_summary.xlsx")
)


## 7. Summarize the results by retention ratios  ----
summary_tbl %>%
  group_by(family, prop_keep) %>%
  summarise(
    mean_origin = mean(origin_mean),
    sd_origin   = sd(origin_mean),
    mean_ci     = mean(CI_width),
    sd_CI       = sd(CI_width),
    mean_HPD    = mean(HPD_width),
    sd_HPD      = sd(HPD_width),
    .groups = "drop"
  )

'
  family     prop_keep mean_origin sd_origin mean_ci sd_CI mean_HPD sd_HPD
  <chr>          <dbl>       <dbl>     <dbl>   <dbl> <dbl>    <dbl>  <dbl>
1 Asteraceae       0.3        61.5     18.0     64.3 18.0      51.7  14.2 
2 Asteraceae       0.7        64.7      4.95    46.6  5.68     38.1   4.45
3 Asteraceae       1          64.2     NA       45.7 NA        38.5  NA   
4 Fabaceae         0.3        79.9      2.68    31.8  6.18     25.5   4.62
5 Fabaceae         0.7        75.8      1.56    15.3  1.99     12.4   1.31
6 Fabaceae         1          75.5     NA       20.9 NA        16.3  NA  
'

################
# plot results #
################

summary_tbl$prop_keep_f <- factor(summary_tbl$prop_keep,
                                  levels = c(1.0, 0.7, 0.3),
                                  labels = c("100%", "70%", "30%"))



# plot_dat
plot_dat <- summary_tbl %>%
  group_by(family, prop_keep_f) %>%
  summarise(
    mean_origin = mean(origin_mean),
    sd_origin   = sd(origin_mean),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_origin - sd_origin,
    ymax = mean_origin + sd_origin
  )

# 
p_err <- ggplot(plot_dat,
                aes(x = prop_keep_f, y = mean_origin, group = family, colour = family)) +
  
  ## 
  geom_jitter(
    data = summary_tbl,
    aes(x = prop_keep_f, y = origin_mean),
    inherit.aes = FALSE,
    width = 0.10,
    size = 1.5,
    alpha = 0.9,
    color = "darkgrey"
  ) +
  
  ## 
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.15,
    size = 0.8,
    alpha = 0.8
  ) +
  
  ## 
  geom_point(size = 3) +
  
  ## 
  geom_line(size = 1.1, alpha = 0.7) +
  
  ## facet per family
  facet_wrap(~ family, scales = "free_y") +
  
  ## axis labels etc
  labs(
    x = "Proportion of fossil occurrences retained",
    y = "Estimated origin age (Ma)",
    title = "Effect of fossil downsampling on estimated origin ages"
  ) +
  
  ## theme
  theme_bw(base_size = 15) +
  theme(
    legend.position = "right"
  )

p_err



p_box <- ggplot(summary_tbl,
                aes(x = prop_keep_f, y = origin_mean, colour = family)) +
  geom_boxplot(alpha = 0.2, width = 0.5, outlier.shape = NA) +  
  geom_jitter(width = 0.1, size = 2, alpha = 0.7) +              
  facet_wrap(~ family, scales = "free_y") +
  labs(
    x = "Proportion of fossil occurrences retained",
    y = "Estimated origin age (Ma)",
    title = "Distribution of origin ages across subsampling replicates"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

p_box











############################
# 0. Packages and loading  #
############################

library(dplyr)
library(ggplot2)
library(patchwork)

# 
summary_tbl <- summary_tbl %>%
  mutate(
    prop_keep_num = case_when(
      prop_keep == 1.0 ~ 1,
      prop_keep == 0.7 ~ 2,
      prop_keep == 0.3 ~ 3,
      TRUE             ~ NA_real_
    )
  )

x_breaks <- c(1, 2, 3)
x_labels <- c("100%", "70%", "30%")

############################
# 1. Panel A: mean ± SD    #
############################

plot_dat <- summary_tbl %>%
  group_by(family, prop_keep_num) %>%
  summarise(
    mean_origin = mean(origin_mean),
    sd_origin   = sd(origin_mean),
    .groups     = "drop"
  ) %>%
  mutate(
    ymin = mean_origin - sd_origin,
    ymax = mean_origin + sd_origin
  )

p_age <- ggplot() +
  # replicates's origin_mean
  geom_jitter(
    data = summary_tbl,
    aes(x = prop_keep_num, y = origin_mean),
    width = 0.10,
    size  = 1.5,
    alpha = 0.9,
    color = "darkgrey"
  ) +
  # mean ± SD
  geom_errorbar(
    data = plot_dat,
    aes(x = prop_keep_num, ymin = ymin, ymax = ymax, colour = family),
    width = 0.15,
    size  = 0.8,
    alpha = 0.9
  ) +
  geom_point(
    data = plot_dat,
    aes(x = prop_keep_num, y = mean_origin, colour = family),
    size = 3
  ) +
  geom_line(
    data = plot_dat,
    aes(x = prop_keep_num, y = mean_origin, colour = family, group = family),
    size = 1.1,
    alpha = 0.8
  ) +
  facet_wrap(~ family, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(
    x = "Proportion of fossil occurrences retained",
    y = "Estimated origin age (Ma)"
  ) +
  ggtitle("A. Mean origin ages under different fossil subsampling levels") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title  = element_text(size = 12),
    axis.title  = element_text(size = 11),
    axis.text   = element_text(size = 12),
    strip.text  = element_text(size = 11)
  )

#############################
# 2.1 Panel B: HPD intervals# 
#############################

# 
summary_tbl_hpd <- summary_tbl %>%
  mutate(
    x_jit = prop_keep_num + runif(n(), min = -0.06, max = 0.06)
  )

# 
hpd_dat <- summary_tbl %>%
  group_by(family, prop_keep_num) %>%
  summarise(
    mean_L_hpd = mean(origin_L_hpd),
    mean_U_hpd = mean(origin_U_hpd),
    mean_HPD_w = mean(HPD_width),
    .groups    = "drop"
  ) %>%
  mutate(
    mid_hpd = (mean_L_hpd + mean_U_hpd) / 2
  )

p_hpd <- ggplot() +
  geom_segment(
    data = summary_tbl_hpd,
    aes(x = x_jit, xend = x_jit,
        y = origin_L_hpd, yend = origin_U_hpd),
    colour = "darkgrey",
    size   = 0.7,
    alpha  = 0.8
  ) +
  
  geom_linerange(
    data = hpd_dat,
    aes(x = prop_keep_num, ymin = mean_L_hpd, ymax = mean_U_hpd, colour = family),
    size  = 1.2,
    alpha = 0.9
  ) +
  
  geom_point(
    data = hpd_dat,
    aes(x = prop_keep_num, y = mid_hpd, colour = family),
    size = 2.5
  ) +
  facet_wrap(~ family, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(
    x = "Proportion of fossil occurrences retained",
    y = "95% HPD interval (Ma)"
  ) +
  ggtitle("B. 95% HPD intervals under different fossil subsampling levels") +
  theme_bw(base_size = 14) +
  theme(
    #legend.position = "right",
    legend.position = "none",
    plot.title  = element_text(size = 12),
    axis.title  = element_text(size = 11),
    axis.text   = element_text(size = 12),
    strip.text  = element_text(size = 11)
  )

############################
# 2.2 Panel C: CI intervals# 
############################

# 
summary_tbl_ci <- summary_tbl %>%
  mutate(
    x_jit = prop_keep_num + runif(n(), min = -0.06, max = 0.06)
  )

ci_dat <- summary_tbl %>%
  group_by(family, prop_keep_num) %>%
  summarise(
    mean_L_ci = mean(origin_L_ci),
    mean_U_ci = mean(origin_U_ci),
    mean_CI_w = mean(CI_width),
    .groups   = "drop"
  ) %>%
  mutate(
    mid_ci = (mean_L_ci + mean_U_ci) / 2
  )

p_ci <- ggplot() +
  geom_segment(
    data = summary_tbl_ci,
    aes(x = x_jit, xend = x_jit,
        y = origin_L_ci, yend = origin_U_ci),
    colour = "darkgrey",
    size   = 0.7,
    alpha  = 0.8
  ) +

  geom_linerange(
    data = ci_dat,
    aes(x = prop_keep_num, ymin = mean_L_ci, ymax = mean_U_ci, colour = family),
    size  = 1.2,
    alpha = 0.9
  ) +

  geom_point(
    data = ci_dat,
    aes(x = prop_keep_num, y = mid_ci, colour = family),
    size = 2.5
  ) +
  facet_wrap(~ family, scales = "free_y") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(
    x = "Proportion of fossil occurrences retained",
    y = "95% equal-tailed CI (Ma)"
  ) +
  ggtitle("C. 95% equal-tailed credible intervals (2.5–97.5% quantiles)") +
  theme_bw(base_size = 14) +
  theme(
    #legend.position = "right",
    legend.position = "none",
    plot.title  = element_text(size = 12),
    axis.title  = element_text(size = 11),
    axis.text   = element_text(size = 12),
    strip.text  = element_text(size = 11)
  )

#######################
# 3. Combine figures  #
#######################

## A + HPD
p_A_B <- p_age / p_hpd
p_A_B

## A + CI
p_A_C <- p_age / p_ci
p_A_C

## A + B + C
p_A_B_C <- p_age / p_hpd / p_ci
p_A_B_C


#
# ggsave("bbb_subsampling_sensitivity.pdf", p_final,
#        width = 8, height = 10)





'
p1 <- ggplot(summary_tbl,
             aes(x = prop_keep_f, y = origin_mean, color = family)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(aes(group = family), size = 1.1, alpha = 0.6) +
  facet_wrap(~family, scales = "free_y") +
  theme_bw(base_size = 14) +
  labs(x = "Proportion of fossil occurrences retained",
       y = "Estimated origin age (Ma)",
       title = "Effect of fossil downsampling on estimated origin ages")

p1


p2 <- ggplot(summary_tbl,
             aes(x = prop_keep_f, y = HPD_width, color = family)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(aes(group = family), size = 1.1, alpha = 0.6) +
  facet_wrap(~family, scales = "free_y") +
  theme_bw(base_size = 14) +
  labs(x = "Proportion of fossil occurrences retained",
       y = "95% HPD width (Ma)",
       title = "Effect of fossil downsampling on uncertainty")

p2
'


