library(ggplot2)
library(gridExtra)

load("outputs/adaptive_sampling_res.RData")

get_lower <- function(x){quantile(x,0.025)}
get_upper <- function(x){quantile(x,0.975)}

plot_res <- function(x, metric){

  accuracy_rand <- sapply(x, function(y){
    y_subset <- y[[metric]]
    y_subset[y$approach=="rand"]})
  accuracy_covp <- sapply(x, function(y){
    y_subset <- y[[metric]]
    y_subset[y$approach=="covp"]})
  
  mean_accuracy_rand <- apply(accuracy_rand, 1, mean)
  mean_accuracy_covp <- apply(accuracy_covp, 1, mean)
  lower_accuracy_rand <- apply(accuracy_rand, 1, function(x){quantile(x,0.025)})
  upper_accuracy_rand <- apply(accuracy_rand, 1, function(x){quantile(x,0.975)})
  lower_accuracy_covp <- apply(accuracy_covp, 1, function(x){quantile(x,0.025)})
  upper_accuracy_covp <- apply(accuracy_covp, 1, function(x){quantile(x,0.975)})
  
  
  if(length(mean_accuracy_rand)<20){
  x_axis <- 1:length(mean_accuracy_rand)
  ggplot() +
    geom_ribbon(aes(x_axis, ymin = lower_accuracy_rand, ymax = upper_accuracy_rand),
                fill = rgb(0,0,1, 0.2)) + 
    geom_ribbon(aes(x_axis, ymin = lower_accuracy_covp, ymax = upper_accuracy_covp),
                fill = rgb(0.9,0,0, 0.2)) +
    geom_line(aes(x_axis, mean_accuracy_rand), color = "blue4", size=2) +
    geom_line(aes(x_axis, mean_accuracy_covp), color = "darkorange", size=2) +
    theme_grey(base_size = 16) + xlim(1, length(mean_accuracy_rand)) +
    scale_x_continuous("Steps", breaks=x_axis, labels = as.character(x_axis)) +
    scale_y_continuous("")
  }else{

    x_axis <- 1:length(mean_accuracy_rand)
    x_axis_lab <- seq(1, length(mean_accuracy_rand), 10)
    ggplot() +
      geom_ribbon(aes(x_axis, ymin = lower_accuracy_rand, ymax = upper_accuracy_rand),
                  fill = rgb(0,0,1, 0.2)) + 
      geom_ribbon(aes(x_axis, ymin = lower_accuracy_covp, ymax = upper_accuracy_covp),
                  fill = rgb(0.9,0.3, 0, 0.2)) +
      geom_line(aes(x_axis, mean_accuracy_rand, color = "blue4"), size=2) +
      geom_line(aes(x_axis, mean_accuracy_covp, color = "darkorange"), size=2) +
      theme_grey(base_size = 16) + xlim(1, length(mean_accuracy_rand)) +
      scale_x_continuous("Steps", breaks=x_axis_lab, labels = as.character(x_axis_lab)) +
      scale_y_continuous("") + 
      scale_colour_manual(name = 'Approach', guide = 'legend', 
           values =c('blue4'='blue4','darkorange'='darkorange'), 
           labels = c('Random','Adaptive'))
  }
  
  
}

grid.arrange(plot_res(civ_res_1, 'accuracy'),
             plot_res(mwi_res_1, 'accuracy'),
             plot_res(hti_res_1, 'accuracy'),
             plot_res(phl_res_1, 'accuracy'),
             nrow = 2,
             ncol = 2)


get_final_plot_data <- function(x){
  
  accuracy_rand <- sapply(x, function(y){
    y_subset <- y[['accuracy']]
    y_subset[y$approach=="rand"]})
  accuracy_covp <- sapply(x, function(y){
    y_subset <- y[['accuracy']]
    y_subset[y$approach=="covp"]})
  
  sensitivity_rand <- sapply(x, function(y){
    y_subset <- y[['sensitivity']]
    y_subset[y$approach=="rand"]})
  sensitivity_covp <- sapply(x, function(y){
    y_subset <- y[['sensitivity']]
    y_subset[y$approach=="covp"]})
  
  positive_pv_rand <- sapply(x, function(y){
    y_subset <- y[['positive_pv']]
    y_subset[y$approach=="rand"]})
  positive_pv_covp <- sapply(x, function(y){
    y_subset <- y[['positive_pv']]
    y_subset[y$approach=="covp"]})
  
  mse_rand <- sapply(x, function(y){
    y_subset <- y[['mse']]
    y_subset[y$approach=="rand"]})
  mse_covp <- sapply(x, function(y){
    y_subset <- y[['mse']]
    y_subset[y$approach=="covp"]})
  
  mean_accuracy_rand <- apply(accuracy_rand, 1, mean)
  mean_accuracy_covp <- apply(accuracy_covp, 1, mean)
  lower_accuracy_rand <- apply(accuracy_rand, 1, get_lower)
  upper_accuracy_rand <- apply(accuracy_rand, 1, get_upper)
  lower_accuracy_covp <- apply(accuracy_covp, 1, get_lower)
  upper_accuracy_covp <- apply(accuracy_covp, 1, get_upper)
  
  mean_sensitivity_rand <- apply(sensitivity_rand, 1, mean)
  mean_sensitivity_covp <- apply(sensitivity_covp, 1, mean)
  lower_sensitivity_rand <- apply(sensitivity_rand, 1, get_lower)
  upper_sensitivity_rand <- apply(sensitivity_rand, 1, get_upper)
  lower_sensitivity_covp <- apply(sensitivity_covp, 1, get_lower)
  upper_sensitivity_covp <- apply(sensitivity_covp, 1, get_upper)
  
  mean_positive_pv_rand <- apply(positive_pv_rand, 1, mean)
  mean_positive_pv_covp <- apply(positive_pv_covp, 1, mean)
  lower_positive_pv_rand <- apply(positive_pv_rand, 1, get_lower)
  upper_positive_pv_rand <- apply(positive_pv_rand, 1, get_upper)
  lower_positive_pv_covp <- apply(positive_pv_covp, 1, get_lower)
  upper_positive_pv_covp <- apply(positive_pv_covp, 1, get_upper)
  
  mean_mse_rand <- apply(mse_rand, 1, mean)
  mean_mse_covp <- apply(mse_covp, 1, mean)
  lower_mse_rand <- apply(mse_rand, 1, get_lower)
  upper_mse_rand <- apply(mse_rand, 1, get_upper)
  lower_mse_covp <- apply(mse_covp, 1, get_lower)
  upper_mse_covp <- apply(mse_covp, 1, get_upper)
  
 return(list(accuracy = list(rand = c(tail(lower_accuracy_rand,1), 
                      tail(mean_accuracy_rand, 1), 
                      tail(upper_accuracy_rand, 1)),
             covp = c(tail(lower_accuracy_covp,1),
                      tail(mean_accuracy_covp,1),
                      tail(upper_accuracy_covp, 1))),
            sensitivity = list(rand = c(tail(lower_sensitivity_rand,1), 
                                        tail(mean_sensitivity_rand, 1), 
                                        tail(upper_sensitivity_rand, 1)),
                               covp = c(tail(lower_sensitivity_covp,1),
                                        tail(mean_sensitivity_covp,1),
                                        tail(upper_sensitivity_covp, 1))),
            positive_pv = list(rand = c(tail(lower_positive_pv_rand,1), 
                                        tail(mean_positive_pv_rand, 1), 
                                        tail(upper_positive_pv_rand, 1)),
                               covp = c(tail(lower_positive_pv_covp,1),
                                        tail(mean_positive_pv_covp,1),
                                        tail(upper_positive_pv_covp, 1))),
            mse = list(rand = c(tail(lower_mse_rand,1), 
                                        tail(mean_mse_rand, 1), 
                                        tail(upper_mse_rand, 1)),
                               covp = c(tail(lower_mse_covp,1),
                                        tail(mean_mse_covp,1),
                                        tail(upper_mse_covp, 1)))
        )
 )
  
}

fd_civ_1 <- get_final_plot_data(civ_res_1)
fd_civ_10 <-get_final_plot_data(civ_res_10)
fd_civ_50 <-get_final_plot_data(civ_res_50)
fd_mwi_1 <- get_final_plot_data(mwi_res_1)
fd_mwi_10 <-get_final_plot_data(mwi_res_10)
fd_mwi_50 <-get_final_plot_data(mwi_res_50)
fd_phl_1 <- get_final_plot_data(phl_res_1)
fd_phl_10 <-get_final_plot_data(phl_res_10)
fd_phl_50 <-get_final_plot_data(phl_res_50)
fd_hti_1 <- get_final_plot_data(hti_res_1)
fd_hti_10 <-get_final_plot_data(hti_res_10)
fd_hti_50 <-get_final_plot_data(hti_res_50)

### CIV
gen_final_metric_plot <- function(x1, x10, x50, metric){
  ggplot() + 
    geom_errorbar(aes(c("1", "1"),ymin = x1[[metric]]$rand[1], 
                      ymax = x1[[metric]]$rand[3]),
                  col = rgb(0,0,0.9, 0.3), size = 2) +
    geom_errorbar(aes(c("10", "10"),ymin = x10[[metric]]$rand[1], 
                      ymax = x10[[metric]]$rand[3]),
                  col = rgb(0,0,0.9, 0.3), size = 2) +
  geom_errorbar(aes(c("50", "50"),ymin = x50[[metric]]$rand[1], 
                    ymax = x50[[metric]]$rand[3]),
                col = rgb(0,0,0.9, 0.3), size = 2) +
    geom_errorbar(aes(c("1", "1"),ymin = x1[[metric]]$covp[1], 
                      ymax = x1[[metric]]$covp[3]),
                  col = rgb(1,0.6,0, 0.3), size = 2) +
  geom_errorbar(aes(c("10", "10"),ymin = x10[[metric]]$covp[1], 
                    ymax = x10[[metric]]$covp[3]),
                col = rgb(1,0.6,0, 0.3), size = 2) +
    geom_errorbar(aes(c("50", "50"),ymin = x50[[metric]]$covp[1], 
                      ymax = x50[[metric]]$covp[3]),
                  col = rgb(1,0.6,0, 0.3), size = 2) +

    geom_point(aes(as.character(c(1,10,50)), c(x1[[metric]]$rand[2],
                                                        x10[[metric]]$rand[2],
                                                        x50[[metric]]$rand[2])), size = 8,
                                    col = "blue4") +
    geom_point(aes(as.character(c(1,10,50)), c(x1[[metric]]$covp[2],
                                               x10[[metric]]$covp[2],
                                               x50[[metric]]$covp[2])), size = 8, col = "darkorange") +

    theme_grey(base_size = 25) + #xlim(as.character(1:3)) +
    scale_x_discrete("Batch size") +
    scale_y_continuous("")
  
}

grid.arrange(
gen_final_metric_plot(fd_civ_1, fd_civ_10, fd_civ_50, 'accuracy'),
gen_final_metric_plot(fd_civ_1, fd_civ_10, fd_civ_50, 'sensitivity'),
gen_final_metric_plot(fd_civ_1, fd_civ_10, fd_civ_50, 'positive_pv'),
gen_final_metric_plot(fd_civ_1, fd_civ_10, fd_civ_50, 'mse'),

gen_final_metric_plot(fd_mwi_1, fd_mwi_10, fd_mwi_50, 'accuracy'),
gen_final_metric_plot(fd_mwi_1, fd_mwi_10, fd_mwi_50, 'sensitivity'),
gen_final_metric_plot(fd_mwi_1, fd_mwi_10, fd_mwi_50, 'positive_pv'),
gen_final_metric_plot(fd_mwi_1, fd_mwi_10, fd_mwi_50, 'mse'),

gen_final_metric_plot(fd_hti_1, fd_hti_10, fd_hti_50, 'accuracy'),
gen_final_metric_plot(fd_hti_1, fd_hti_10, fd_hti_50, 'sensitivity'),
gen_final_metric_plot(fd_hti_1, fd_hti_10, fd_hti_50, 'positive_pv'),
gen_final_metric_plot(fd_hti_1, fd_hti_10, fd_hti_50, 'mse'),

gen_final_metric_plot(fd_phl_1, fd_phl_10, fd_phl_50, 'accuracy'),
gen_final_metric_plot(fd_phl_1, fd_phl_10, fd_phl_50, 'sensitivity'),
gen_final_metric_plot(fd_phl_1, fd_phl_10, fd_phl_50, 'positive_pv'),
gen_final_metric_plot(fd_phl_1, fd_phl_10, fd_phl_50, 'mse'),

             nrow = 4,
             ncol = 4)


### Calculate adaptive sample size to achieve equivalent accuracy
res_data <- function(x, metric){
  
  accuracy_rand <- sapply(x, function(y){
    y_subset <- y[[metric]]
    y_subset[y$approach=="rand"]})
  accuracy_covp <- sapply(x, function(y){
    y_subset <- y[[metric]]
    y_subset[y$approach=="covp"]})
  
  mean_accuracy_rand <- apply(accuracy_rand, 1, mean)
  mean_accuracy_covp <- apply(accuracy_covp, 1, mean)
return(list(rand = mean_accuracy_rand,
            covp = mean_accuracy_covp))
}

# Get indeces of equivalents
eq_civ_res_1 <- min(which(res_data(civ_res_1, "accuracy")$covp >= tail(res_data(civ_res_1, "accuracy")$rand, 1)))
eq_civ_res_10 <- min(which(res_data(civ_res_10, "accuracy")$covp >= tail(res_data(civ_res_10, "accuracy")$rand, 1)))
eq_civ_res_50 <- min(which(res_data(civ_res_50, "accuracy")$covp >= tail(res_data(civ_res_50, "accuracy")$rand, 1)))

eq_mwi_res_1 <- min(which(res_data(mwi_res_1, "accuracy")$covp >= tail(res_data(mwi_res_1, "accuracy")$rand, 1)))
eq_mwi_res_10 <- min(which(res_data(mwi_res_10, "accuracy")$covp >= tail(res_data(mwi_res_10, "accuracy")$rand, 1)))
eq_mwi_res_50 <- min(which(res_data(mwi_res_50, "accuracy")$covp >= tail(res_data(mwi_res_50, "accuracy")$rand, 1)))

eq_hti_res_1 <- min(which(res_data(hti_res_1, "accuracy")$covp >= tail(res_data(hti_res_1, "accuracy")$rand, 1)))
eq_hti_res_10 <- min(which(res_data(hti_res_10, "accuracy")$covp >= tail(res_data(hti_res_10, "accuracy")$rand, 1)))
eq_hti_res_50 <- min(which(res_data(hti_res_50, "accuracy")$covp >= tail(res_data(hti_res_50, "accuracy")$rand, 1)))

eq_phl_res_1 <- min(which(res_data(phl_res_1, "accuracy")$covp >= tail(res_data(phl_res_1, "accuracy")$rand, 1)))
eq_phl_res_10 <- min(which(res_data(phl_res_10, "accuracy")$covp >= tail(res_data(phl_res_10, "accuracy")$rand, 1)))
eq_phl_res_50 <- min(which(res_data(phl_res_50, "accuracy")$covp >= tail(res_data(phl_res_50, "accuracy")$rand, 1)))

# Get equivalent accuracy
tail(res_data(civ_res_1, "accuracy")$rand, 1)
tail(res_data(civ_res_10, "accuracy")$rand, 1)
tail(res_data(civ_res_50, "accuracy")$rand, 1)
res_data(civ_res_1, "accuracy")$covp[eq_civ_res_1]
res_data(civ_res_10, "accuracy")$covp[eq_civ_res_10]
res_data(civ_res_50, "accuracy")$covp[eq_civ_res_50]

tail(res_data(mwi_res_1, "accuracy")$rand, 1)
tail(res_data(mwi_res_10, "accuracy")$rand, 1)
tail(res_data(mwi_res_50, "accuracy")$rand, 1)
res_data(mwi_res_1, "accuracy")$covp[eq_mwi_res_1]
res_data(mwi_res_10, "accuracy")$covp[eq_mwi_res_10]
res_data(mwi_res_50, "accuracy")$covp[eq_mwi_res_50]

tail(res_data(hti_res_1, "accuracy")$rand, 1)
tail(res_data(hti_res_10, "accuracy")$rand, 1)
tail(res_data(hti_res_50, "accuracy")$rand, 1)
res_data(hti_res_1, "accuracy")$covp[eq_hti_res_1]
res_data(hti_res_10, "accuracy")$covp[eq_hti_res_10]
res_data(hti_res_50, "accuracy")$covp[eq_hti_res_50]

tail(res_data(phl_res_1, "accuracy")$rand, 1)
tail(res_data(phl_res_10, "accuracy")$rand, 1)
tail(res_data(phl_res_50, "accuracy")$rand, 1)
res_data(phl_res_1, "accuracy")$covp[eq_phl_res_1]
res_data(phl_res_10, "accuracy")$covp[eq_phl_res_10]
res_data(phl_res_50, "accuracy")$covp[eq_phl_res_50]

### Sensitivity
tail(res_data(civ_res_1, "sensitivity")$rand, 1)
tail(res_data(civ_res_10, "sensitivity")$rand, 1)
tail(res_data(civ_res_50, "sensitivity")$rand, 1)
res_data(civ_res_1, "sensitivity")$covp[eq_civ_res_1]
res_data(civ_res_10, "sensitivity")$covp[eq_civ_res_10]
res_data(civ_res_50, "sensitivity")$covp[eq_civ_res_50]

tail(res_data(mwi_res_1, "sensitivity")$rand, 1)
tail(res_data(mwi_res_10, "sensitivity")$rand, 1)
tail(res_data(mwi_res_50, "sensitivity")$rand, 1)
res_data(mwi_res_1, "sensitivity")$covp[eq_mwi_res_1]
res_data(mwi_res_10, "sensitivity")$covp[eq_mwi_res_10]
res_data(mwi_res_50, "sensitivity")$covp[eq_mwi_res_50]

tail(res_data(hti_res_1, "sensitivity")$rand, 1)
tail(res_data(hti_res_10, "sensitivity")$rand, 1)
tail(res_data(hti_res_50, "sensitivity")$rand, 1)
res_data(hti_res_1, "sensitivity")$covp[eq_hti_res_1]
res_data(hti_res_10, "sensitivity")$covp[eq_hti_res_10]
res_data(hti_res_50, "sensitivity")$covp[eq_hti_res_50]

tail(res_data(phl_res_1, "sensitivity")$rand, 1)
tail(res_data(phl_res_10, "sensitivity")$rand, 1)
tail(res_data(phl_res_50, "sensitivity")$rand, 1)
res_data(phl_res_1, "sensitivity")$covp[eq_phl_res_1]
res_data(phl_res_10, "sensitivity")$covp[eq_phl_res_10]
res_data(phl_res_50, "sensitivity")$covp[eq_phl_res_50]
    

### PPV
tail(res_data(civ_res_1, "sensitivity")$rand, 1)
tail(res_data(civ_res_10, "positive_pv")$rand, 1)
tail(res_data(civ_res_50, "positive_pv")$rand, 1)
res_data(civ_res_1, "positive_pv")$covp[eq_civ_res_1]
res_data(civ_res_10, "positive_pv")$covp[eq_civ_res_10]
res_data(civ_res_50, "positive_pv")$covp[eq_civ_res_50]

tail(res_data(mwi_res_1, "positive_pv")$rand, 1)
tail(res_data(mwi_res_10, "positive_pv")$rand, 1)
tail(res_data(mwi_res_50, "positive_pv")$rand, 1)
res_data(mwi_res_1, "positive_pv")$covp[eq_mwi_res_1]
res_data(mwi_res_10, "positive_pv")$covp[eq_mwi_res_10]
res_data(mwi_res_50, "positive_pv")$covp[eq_mwi_res_50]

tail(res_data(hti_res_1, "positive_pv")$rand, 1)
tail(res_data(hti_res_10, "positive_pv")$rand, 1)
tail(res_data(hti_res_50, "positive_pv")$rand, 1)
res_data(hti_res_1, "positive_pv")$covp[eq_hti_res_1]
res_data(hti_res_10, "positive_pv")$covp[eq_hti_res_10]
res_data(hti_res_50, "positive_pv")$covp[eq_hti_res_50]

tail(res_data(phl_res_1, "positive_pv")$rand, 1)
tail(res_data(phl_res_10, "positive_pv")$rand, 1)
tail(res_data(phl_res_50, "positive_pv")$rand, 1)
res_data(phl_res_1, "positive_pv")$covp[eq_phl_res_1]
res_data(phl_res_10, "positive_pv")$covp[eq_phl_res_10]
res_data(phl_res_50, "positive_pv")$covp[eq_phl_res_50]


## MSE
tail(res_data(civ_res_1, "mse")$rand, 1)
tail(res_data(civ_res_10, "mse")$rand, 1)
tail(res_data(civ_res_50, "mse")$rand, 1)
res_data(civ_res_1, "mse")$covp[eq_civ_res_1]
res_data(civ_res_10, "mse")$covp[eq_civ_res_10]
res_data(civ_res_50, "mse")$covp[eq_civ_res_50]

tail(res_data(mwi_res_1, "mse")$rand, 1)
tail(res_data(mwi_res_10, "mse")$rand, 1)
tail(res_data(mwi_res_50, "mse")$rand, 1)
res_data(mwi_res_1, "mse")$covp[eq_mwi_res_1]
res_data(mwi_res_10, "mse")$covp[eq_mwi_res_10]
res_data(mwi_res_50, "mse")$covp[eq_mwi_res_50]

tail(res_data(hti_res_1, "mse")$rand, 1)
tail(res_data(hti_res_10, "mse")$rand, 1)
tail(res_data(hti_res_50, "mse")$rand, 1)
res_data(hti_res_1, "mse")$covp[eq_hti_res_1]
res_data(hti_res_10, "mse")$covp[eq_hti_res_10]
res_data(hti_res_50, "mse")$covp[eq_hti_res_50]

tail(res_data(phl_res_1, "mse")$rand, 1)
tail(res_data(phl_res_10, "mse")$rand, 1)
tail(res_data(phl_res_50, "mse")$rand, 1)
res_data(phl_res_1, "mse")$covp[eq_phl_res_1]
res_data(phl_res_10, "mse")$covp[eq_phl_res_10]
res_data(phl_res_50, "mse")$covp[eq_phl_res_50]
