# Load functions needed to run fn-prevalence-predictor-spaMM
source("online_functions/helpers_fn_cv_ml.R")
fn_prevalence_predictor_spamm <- dget("online_functions/fn_prevalence_predictor_spamm.R")


function(iso3="PHL", 
         batch_size=50, 
         #N_threads=1, 
         seed=1) {

  # Set analysis parameters
  N_steps <- 100 / batch_size + 1
  N_samples <- 100
  layer_names <- c("bioclim1", "bioclim4", "bioclim12", "bioclim15", "elev_m", "dist_to_water_m")
  country_params <- data.frame("PHL" = .02, "HTI" = .02, "CIV" = .1, "MWI" = .1)
  threshold <- as.numeric(country_params[iso3])

  # Load data
  table_name <- paste0(tolower(iso3),'_data')
  region_data_raw <- read.csv(paste0('data_inputs/', table_name, '.csv'))

  # Clip theta
  region_data_raw['n_trials'] <- NA
  region_data_raw['n_positive'] <- NA
  region_data_raw['hotspot'] <- region_data_raw['theta'] > threshold
  region_data_raw['z'] <- NA

  # Define number of locations in the country
  M <- nrow(region_data_raw)

  # Sample
  scores_df_rand <- data.frame()
  scores_df_covp <- data.frame()

    # List of samples per step
    set.seed(seed)
    sample_0 <- list(sample(1:M, 100))
    sample_batches_rand <- sample_0
    sample_batches_covp <- sample_0

    # Sampling probabilities per step
    region_rand <- region_data_raw
    region_covp <- region_data_raw

    # List of scores per step
    scores_rand <- data.frame()
    scores_covp <- data.frame()

    for (step in 1:N_steps) {

      if (step == 1) {
          # Replicate data
          survey_0 <- c()
          for (jj in  sample_0[[1]]) {
            pi <- region_data_raw[jj, 'theta']
            survey_0 <- c(survey_0, rbinom(n=1, size=100, p=pi))
          }
          region_rand[sample_0[[1]], 'n_trials'] <- 100
          region_covp[sample_0[[1]], 'n_trials'] <- 100
          region_rand[sample_0[[1]], 'n_positive'] <- survey_0
          region_covp[sample_0[[1]], 'n_positive'] <- survey_0

      } else {
          # Generate hypothetical survey data for the sampled villages
          region_rand[sample_batches_rand[[step]], 'n_trials'] <- 100
          region_covp[sample_batches_covp[[step]], 'n_trials'] <- 100

          new_survey_rand <- c()
          for (jj in sample_batches_rand[[step]]) {
            pi <- region_rand[jj, 'theta']
            new_survey_rand <- c(new_survey_rand, rbinom(n=1, size=100, p=pi))
          }
          region_rand[sample_batches_rand[[step]], 'n_positive'] <- new_survey_rand

          new_survey_covp <- c()
          for (jj in sample_batches_covp[[step]]) {
            pi <- region_covp[jj, 'theta']
            new_survey_covp <- c(new_survey_covp, rbinom(n=1, size=100, p=pi))
          }
          region_covp[sample_batches_covp[[step]], 'n_positive'] <- new_survey_covp
      }

      # Call prevalence_predictor
      point_data_rand <- sp::SpatialPointsDataFrame(coords = region_rand[,c("lng", "lat")],
                                                    data = region_rand)
      point_data_covp <- sp::SpatialPointsDataFrame(coords = region_covp[,c("lng", "lat")],
                                                    data = region_covp)

      point_data_sf_rand <- st_as_sf(point_data_rand)
      point_data_sf_covp <- st_as_sf(point_data_covp)

      params_rand <- list(batch_size = batch_size, 
                          exceedance_threshold = threshold,
                          uncertainty_fieldname = 'entropy', 
                          layer_names = layer_names,
                          point_data = point_data_sf_rand,
                          seed = seed)
      params_covp <- list(batch_size = batch_size, 
                          exceedance_threshold = threshold,
                          uncertainty_fieldname = 'entropy', 
                          layer_names = layer_names,
                          point_data = point_data_sf_covp,
                          seed = seed)

      spamm_rand <- fn_prevalence_predictor_spamm(params_rand)
      spamm_covp <- fn_prevalence_predictor_spamm(params_covp)

      # Locations visited
      ix_rand <- unique(do.call(c, args = sample_batches_rand))
      ix_covp <- unique(do.call(c, args = sample_batches_covp))

      # Prevalence
      pred_rand <- spamm_rand$prevalence_prediction[-ix_rand]
      pred_covp <- spamm_covp$prevalence_prediction[-ix_covp]
      theta_rand <- region_rand$theta[-ix_rand]
      theta_covp <- region_covp$theta[-ix_covp]

      # Classes predicted
      hotspot_rand <- spamm_rand$exceedance_probability > .5
      hotspot_covp <- spamm_covp$exceedance_probability > .5

      # Scores
      true_rand <- as.factor(1 * as.vector(region_rand[-ix_rand, 'hotspot']))
      oosm_rand <- as.factor(1 * hotspot_rand[-ix_rand])
      true_rand_all <- as.factor(1 * as.vector(region_rand[, 'hotspot']))
      oosm_rand_all <- as.factor(1 * hotspot_rand)
      s_rand <- list(accuracy = sum(oosm_rand == true_rand) / length(true_rand),
                     accuracy_all = sum(oosm_rand_all == true_rand_all) / length(true_rand_all),
                     positive_pv = sum(oosm_rand == 1 & true_rand == 1) / sum(oosm_rand == 1),
                     negative_pv = sum(oosm_rand == 0 & true_rand == 0) / sum(oosm_rand == 0),
                     sensitivity = sum(oosm_rand == 1 & true_rand == 1) / sum(true_rand == 1),
                     sensitivity_all = sum(oosm_rand_all == 1 & true_rand_all == 1) / sum(true_rand_all == 1),
                     mse = mean((pred_rand - theta_rand)^2))

      true_covp <- as.factor(1 * as.vector(region_covp[-ix_covp, 'hotspot']))
      oosm_covp <- as.factor(1 * hotspot_covp[-ix_covp])
      true_covp_all <- as.factor(1 * as.vector(region_covp[, 'hotspot']))
      oosm_covp_all <- as.factor(1 * hotspot_covp)
      s_covp <- list(accuracy = sum(as.character(oosm_covp) == as.character(true_covp)) / length(true_covp),
                     accuracy_all = sum(oosm_covp_all == true_covp_all) / length(true_covp_all),
                     positive_pv = sum(oosm_covp == 1 & true_covp == 1) / sum(oosm_covp == 1),
                     negative_pv = sum(oosm_covp == 0 & true_covp == 0) / sum(oosm_covp == 0),
                     sensitivity = sum(oosm_covp == 1 & true_covp == 1) / sum(true_covp == 1),
                     sensitivity_all = sum(oosm_covp_all == 1 & true_covp_all == 1) / sum(true_covp_all == 1),
                     mse = mean((pred_covp - theta_covp)^2))

      # Add scores results to lists
      scores_rand <- rbind(scores_rand, data.frame(s_rand))
      scores_covp <- rbind(scores_covp, data.frame(s_covp))

      # Batch selection
      new_batch_rand <- sample(c(1:nrow(region_rand))[-unlist(sample_batches_rand)], size=batch_size, replace = FALSE)
      new_batch_covp <- (1:nrow(region_covp))[spamm_covp$adaptively_selected]
      sample_batches_rand[[step+1]] <- new_batch_rand
      sample_batches_covp[[step+1]] <- new_batch_covp
    }
    

    scores_rand$step <- 1:N_steps
    scores_covp$step <- 1:N_steps

    scores_df_rand <- rbind(scores_df_rand, scores_rand)
    scores_df_covp <- rbind(scores_df_covp, scores_covp)

  scores_df_rand$iso3 <- iso3
  scores_df_covp$iso3 <- iso3
  scores_df_rand$approach <- "rand"
  scores_df_covp$approach <- "covp"
  scores_df_rand$batch_size <- batch_size
  scores_df_covp$batch_size <- batch_size
  scores_df <- rbind(scores_df_rand, scores_df_covp)

  return(scores_df)
}
