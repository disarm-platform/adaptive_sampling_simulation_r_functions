suppressPackageStartupMessages(library(spaMM))
suppressPackageStartupMessages(library(sf))
library(geojsonio)
library(httr)
library(jsonlite)

get_posterior_metrics <- dget("online_functions/helpers_fn_prevalence_predictor_spamm.R")
choose_batch <- dget("online_functions/choose_batch.R")
#choose_batch2 <- dget("~/Downloads/choose_batch_simple.R")
fn_cv_ml <- dget("online_functions/fn_cv_ml.R")

function(params) {

  # Read into memory
  points_sf <- params$point_data
  
  # Annoyingly values come back as factors, so change back to numeric
  points_sf$n_trials <- as.numeric(as.character(points_sf$n_trials))
  points_sf$n_positive <- as.numeric(as.character(points_sf$n_positive))
  
  # Now pass into fn-cv-ml
  cv_hal_input_data_list <- list(points = points_sf,
                                 layer_names = params$layer_names,
                                 seed = params$seed)
  
  response_cv_ml <- fn_cv_ml(cv_hal_input_data_list)
  
  # Clean up data ready to pass into model
  mod_data_sf <- response_cv_ml$points
  mod_data <- as.data.frame(response_cv_ml$points)
  mod_data <- cbind(mod_data, st_coordinates(mod_data_sf))
  mod_data$cv_predictions <- as.numeric(as.character(mod_data$cv_predictions))
  mod_data$fitted_predictions <- as.numeric(as.character(mod_data$fitted_predictions))
  mod_data$n_trials <- as.numeric(as.character(mod_data$n_trials))
  mod_data$n_positive <- as.numeric(as.character(mod_data$n_positive))
  mod_data$n_neg <- mod_data$n_trials - mod_data$n_positive
  train_data <- mod_data[!is.na(mod_data$n_trials),]
  pred_data <- mod_data[is.na(mod_data$n_trials),]
  
  # Fit geostatistical model
  spaMM_mod <- fitme(cbind(n_positive, n_neg) ~
                       cv_predictions +
                       Matern(1|X+Y),
                     init = list(rho=0.1),
                     data=train_data,
                     family=binomial())
  
  pred_data$cv_predictions <- pred_data$fitted_predictions
  
  # Get posterior metrics
  mod_data$cv_predictions <- mod_data$fitted_predictions
  posterior_metrics <- get_posterior_metrics(spaMM_mod,
                        mod_data,
                        200,
                        params$exceedance_threshold)
  
  # Bind to point_data
  for(i in names(posterior_metrics)){
    points_sf[[i]] <- posterior_metrics[[i]]
  }
  
  ### Wrapper for adaptive sampling ###
  if(!is.null(params$batch_size)){

  new_batch <- choose_batch(XY = st_coordinates(points_sf),
                            entropy = points_sf[[params$uncertainty_fieldname]],
                            candidate = is.na(points_sf$n_positive),
                            rho = spaMM_mod$corrPars[[1]]$rho,
                            nu = spaMM_mod$corrPars[[1]]$nu,
                            batch_size = params$batch_size)
  
  # new_batch <- choose_batch2(point_data = points_sf,
  #                            candidate = is.na(points_sf$n_positive),
  #                            uncertainty_fieldname = 'exceedance_uncertainty',
  #                           batch_size = params$batch_size)
  
  # Create column identifying whether location should be daptively sampled
  points_sf$adaptively_selected <- FALSE
  points_sf$adaptively_selected[new_batch] <- TRUE
  }

  return(points_sf)
}
