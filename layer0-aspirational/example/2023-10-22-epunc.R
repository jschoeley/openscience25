# Empirical prediction intervals for short term mortality forecasts
#
# How does the script do it?

# Init ------------------------------------------------------------

library(yaml)
library(tidyverse)
library(mgcv)
library(gamlss)
library(foreach)
library(doParallel)
library(lubridate)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  data_cv = './dat/mocy.csv',
  data_cv_old = './dat/10-data_cv.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  figure = './out/'
)

# global configuration
config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- within(list(), {
  cv_series_calibration = 1:5
  cv_series_validation = 6:7
  cv_series_application = 8
  quantiles = c(0.05, 0.1, 0.9, 0.95)
  fig_text_size = 4
  # how many threads used to fit models?
  cpu_nodes = 14
  # date of Covid start needs to be a Monday
  start_covid = as.Date("2020-01-06")
  # in weeks
  training_length = 261
  test_length = 104
  # vector of countries with data starting from year 2000 or before (24)
  countries <- c('AT', 'BE', 'BG', 'CH', 'DE', 'ES', 'EE', 'FI', 'FR', 'GB-SCT',
                 'HR', 'HU', 'IS', 'IL', 'LT', 'LU', 'LV', 'NL', 'NO', 'PL',
                 'PT', 'SE', 'SI', 'SK')
  # countries for appendix plots
  appendix_cntr <- c('NO', 'SE', 'FI',
                     'GB-SCT', 'NL', 'DE',
                     'AT', 'PL', 'LV',
                     'ES', 'HR', 'BG')
  rest_cntr <- c('IS', 'BE', 'CH',
                 'EE', 'FR', 'HU',
                 'SK', 'IL', 'LT',
                 'LU', 'PT', 'SI')
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# setup parallel computation
cnst$cl <- makeCluster(cnst$cpu_nodes, outfile = paths$output$log)
registerDoParallel(cnst$cl)

# Figure specification --------------------------------------------

# fonts
library(showtext)
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

figspec <- list()
figspec <- within(figspec, {
  
  # color coding
  colors <- list(
    sample =
      c(
        training = "grey30",
        test = "red"
      ),
    sex =
      c(
        `Male` = "#004B87",
        `Female` = "#c60c30"
      )
  )
  
  # figure dimensions in mm
  fig_dims <- list(width = 180)
  
  # ggplot theme by Jonas Schöley
  MyGGplotTheme <-
    function(size = 8,
             family = "roboto",
             scaler = 1,
             axis = "x",
             panel_border = FALSE,
             grid = "y",
             minor_grid = "",
             show_legend = TRUE,
             ar = NA,
             axis_title_just = "rt",
             axis_ticks = TRUE) {
      size_med <- size * scaler
      size_sml <- round(size * 0.7) * scaler
      base_linesize <- 0.3 * scaler
      
      # justification of axis titles
      xj <- switch(tolower(substr(axis_title_just, 1, 1)),
                   b = 0,
                   l = 0,
                   m = 0.5,
                   c = 0.5,
                   r = 1,
                   t = 1
      )
      yj <- switch(tolower(substr(axis_title_just, 2, 2)),
                   b = 0,
                   l = 0,
                   m = 0.5,
                   c = 0.5,
                   r = 1,
                   t = 1
      )
      
      list(
        theme_minimal(base_size = size_med, base_family = family),
        theme(
          # basic
          text = element_text(color = "black"),
          line = element_line(size = base_linesize, lineend = "square"),
          # axis
          axis.title = element_text(size = size_med, face = "bold"),
          axis.title.x = element_text(hjust = xj),
          axis.title.y = element_text(hjust = yj),
          axis.title.y.right = element_text(hjust = yj, angle = 90),
          axis.text = element_text(size = size_med, color = "black"),
          # strips
          strip.text = element_text(color = "black", size = size_med),
          strip.background = element_blank(),
          # plot
          title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "black", size = size_med, face = "bold"),
          plot.caption = element_text(color = "black", size = size_sml, face = "plain"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          # plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
          # grid
          panel.grid = element_blank()
        ),
        if (isTRUE(axis_ticks)) {
          theme(axis.ticks = element_line(size = rel(0.5), color = "black"))
        },
        if (identical(grid, "y")) {
          theme(
            panel.grid.major.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(grid, "x")) {
          theme(
            panel.grid.major.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(grid, "xy") | identical(grid, "yx")) {
          theme(
            panel.grid.major.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80"),
            panel.grid.major.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(minor_grid, "y")) {
          theme(
            panel.grid.minor.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(minor_grid, "x")) {
          theme(
            panel.grid.minor.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (identical(minor_grid, "xy") | identical(grid, "yx")) {
          theme(
            panel.grid.minor.y =
              element_line(size = base_linesize, linetype = 3, color = "grey80"),
            panel.grid.minor.x =
              element_line(size = base_linesize, linetype = 3, color = "grey80")
          )
        },
        if (isTRUE(panel_border)) {
          theme(
            panel.border =
              element_rect(fill = NA)
          )
        },
        if (!isTRUE(show_legend)) {
          theme(legend.position = "none")
        },
        if (axis == "x") {
          theme(
            axis.line.x = element_line(linetype = 1, color = "black")
          )
        },
        if (axis == "y") {
          theme(
            axis.line.y = element_line(linetype = 1, color = "black")
          )
        },
        if (axis == "xy") {
          theme(
            axis.line = element_line(linetype = 1, color = "black")
          )
        },
        if (!is.na(ar)) {
          theme(
            aspect.ratio = ar
          )
        }
      )
    }
})

# Global functions figures ----------------------------------------

#' Export ggplot
#'
#' @author Jonas Schöley
ExportFigure <-
  function(figure,
           path,
           filename,
           width = 180,
           height = 100,
           scale = 1,
           device = "png",
           dpi = 300,
           add_date = FALSE) {
    require(ggplot2)
    
    if (missing(filename)) {
      filename <- tolower(gsub("\\.", "_", make.names(deparse(substitute(figure)))))
    }
    if (isTRUE(add_date)) {
      filename <- paste0(Sys.Date(), "-", filename)
    }
    
    arguments <-
      list(
        filename = paste0(filename, ".", device),
        plot = figure,
        path = path,
        width = width,
        height = height,
        units = "mm",
        scale = scale,
        dpi = dpi,
        device = device
      )
    if (device == "pdf") {
      arguments$useDingbats <- FALSE
    }
    
    do.call(ggsave, arguments)
  }

# Expected Death Models -------------------------------------------

#' Count Prediction with GAM
#'
#' @param df data frame containing the variables in the model.
#' @param formula formula for gam().
#' @param family family object passed to gam(). poisson(), nb(), or quasipoisson().
#' @param col_sample name of column in <df> indicating training or test
#' data. must have values 'training' or 'test'.
#' @param col_stratum name of column in <df> indicating strata.
#' @param weeks_for_training vector of weeks in training data to be used
#' for training the model. default NULL uses all weeks of the year.
#' @param col_week name of column used for <weeks_for_training> selection.
#' @param n_years_for_training number of years in training data to be used
#' for training. counts backwards from the last year in training data.
#' default NULL uses all years in training.
#' @param col_year name of column used for <n_years_for_training> selection.
#' @param nsim number of simulated predictions.
#' @param simulate_beta should the simulated predictions contain
#' uncertainty around the parameter estimates of the model? (default = TRUE)
#' @param simulate_y should the simulated predictions contain uncertainty
#' around the sampling distribution of the outcome (default = TRUE)
#'
#' @details
#' A GAM is fitted over the training data and expected
#' counts are predicted over the complete input data frame. The
#' training data is indicated by the column <col_sample> and can further
#' be subset by specifying <weeks_for_training> and <n_years_for_training>.
#' By default, the input <df> is returned with added expected
#' counts and <nsim> columns holding simulated counts from the
#' predicted distribution of counts. The model is fitted independently
#' over the strata specified in <col_stratum>.
#'
#' @return
#' <df> with added column <predicted> containing the expected
#' death counts, and columns <simulated><1:nsim> containing simulated
#' expectations if simulate_y = FALSE or simulated counts from
#' the predicted outcome distribution if simulate_y = TRUE.
#' 
#' @author Jonas Schöley
CountGAM <- function (
    df, formula, family, method = 'GCV.Cp',
    # column names for training/test split and strata
    col_sample, col_stratum,
    # only fit on part of the year
    weeks_for_training = NULL, col_week = NULL,
    # only fit on part of the available years
    n_years_for_training = NULL, col_year = NULL,
    # simulation parameters
    nsim = 500, simulate_beta = TRUE, simulate_y = TRUE
) {
  
  require(mgcv)
  
  df['.rowid'] <- 1:nrow(df)
  
  ## subset input data to rows used for fitting the model ##
  
  # index of rows designated as training data
  idx_train <- df[[col_sample]] == 'training'
  # index of rows with weeks suitable for training
  idx_weeks <- TRUE
  if (!is.null(weeks_for_training)) {
    # only train on these weeks
    idx_weeks <- df[[col_week]] %in% weeks_for_training
  }
  # index of rows with years suitable for training
  idx_years <- TRUE
  if (!is.null(n_years_for_training)) {
    # most recent <n_years> in training data
    years_for_training <- sort(unique(df[idx_train,][[col_year]]),
                               decreasing = TRUE)[1:n_years_for_training]
    # only train on these years
    idx_years <- df[[col_year]] %in% years_for_training
  }
  # index of data used for fitting
  idx_fit <- idx_train & idx_years & idx_weeks
  
  # for each stratum, fit model, predict and simulate from model,
  # add results to df
  strata <- unique(df[[col_stratum]])
  # names for columns holding predicted death counts
  colnames_y_sim <- paste0('simulated', 1:nsim)
  for (i in strata) {
    
    # stratum subsets of training and prediction data
    idx_stratum <- df[[col_stratum]]==i
    df_prd <- df[idx_stratum,]
    df_trn <- df[idx_stratum&idx_train,]
    # normalized weights used for fitting
    # magnitude of log-likelihood not affected
    # by exclusion of periods for fitting
    include <- idx_fit[idx_stratum&idx_train]
    wgts <- (include)/mean(include)
    df_trn[,'wgts'] <- wgts
    
    ## fit model ##
    
    family_fit <- family
    # in case of quasipoisson, fit poisson and manually extract
    # overdispersion, so that we can get a quasi-likelihood from
    # the poisson fit
    if (identical(family$family, 'quasipoisson')) {family_fit$family <- 'poisson'}
    model <- gam(
      formula = formula, family = family_fit, data = df_trn,
      method = method, weights = wgts
    )
    # https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
    dfun <- function(object) {
      with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
    }
    dispersion <- dfun(model)
    
    ## predict from model ##
    
    # create a design matrix for prediction
    # keep input NAs in design matrix
    X_prd <- predict(model, newdata = df_prd, type = 'lpmatrix')
    # estimated coefficients
    beta <- coef(model)
    # linear predictor over prediction data w/o offset
    eta_prd_without_offset <- X_prd %*% beta
    # linear predictor over prediction data with offset included
    eta_prd_with_offset <- matrix(predict(model, newdata = df_prd, type = 'link'), ncol = 1)
    # I know of no easy way with mgcv to extract the offset over "newdata"
    # therefore this rather strange solution
    # offset over prediction data (may be 0)
    offset_prd <- eta_prd_with_offset - eta_prd_without_offset
    # inverse link function
    ILink <- model$family$linkinv
    # expected death counts
    Ey_prd <- ILink(eta_prd_with_offset)
    
    ## simulate model predictions ##
    
    # simulated model coefficients
    if (isTRUE(simulate_beta)) {
      beta_sim <- MASS::mvrnorm(nsim, beta, vcov(model, freq = FALSE, unconditional = TRUE))
    } else {
      beta_sim <- matrix(rep(beta, nsim), nrow = nsim, byrow = TRUE)
    }
    # simulated expectations of the outcome distribution
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) ILink(X_prd%*%b + offset_prd))
    # simulated outcome
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      # if a simulation algorithm hasn't been defined for a family
      # just return the expectation of the outcome
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu); mu_ <- mu[!idx_na]; N <- length(mu_)
      if (model$family$family == 'poisson') {
        y[!idx_na] <- rpois(n = N, lambda = mu_)      
      }
      if (model$family$family == 'quasipoisson') {
        # https://stats.stackexchange.com/q/157575
        # we estimate the rate and dispersion parameter via quasipoisson
        # and then sample from a Negative Binomial distribution with the
        # same rate and dispersion (NBI)
        phi <- dispersion
        # in case of under-dispersion, sample from Poisson
        if (phi < 1) { phi = 1 }
        y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      }
      if (grepl('^Negative Binomial', model$family$family)) {
        theta <- model$family$getTheta(TRUE)
        y[!idx_na] <- rnbinom(n = N, mu = mu_, size = theta)      
      }
      # just return the expectation if outcome simulation is FALSE
      if (!isTRUE(simulate_y)) {
        y <- Ey
      }
      return(y)
    })
    
    # add predictions and simulations to input data
    df[df_prd[['.rowid']], 'predicted'] <- Ey_prd
    df[df_prd[['.rowid']], colnames_y_sim] <- y_sim
    
  }
  
  df[,'.rowid'] <- NULL
  return(df)
  
}

# Coverage validation functions -----------------------------------

#' MEAN! Interval Score
#'
#' @param observed observed number of deaths
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param alpha quantile level (e.g. alpha = 0.1 for 90% prediction interval)
#' @param na.rm whether or not missing values should be removed
#'
#' @return
#' @export
#'
#' @examples
MIS <- function (observed, lower, upper, alpha, na.rm = FALSE) {
  N = length(observed)
  below_lower = observed < lower
  above_upper = observed > upper
  interval_width = upper - lower
  
  mis <- sum(
    interval_width +
      2/alpha*(lower-observed)*below_lower +
      2/alpha*(observed-upper)*above_upper,
    na.rm = na.rm
  ) / N
  
  return(mis)
}

#' Title
#'
#' @param observed observed number of deaths
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param na.rm whether or not missing values should be removed
#'
#' @return
#' @export
#'
#' @examples
Coverage <- function (observed, lower, upper, na.rm = FALSE) {
  N = length(observed)
  below_upper = observed < upper
  above_lower = observed > lower
  within_interval = below_upper & above_lower
  
  cov <- sum(within_interval, na.rm = na.rm) / N
  
  return(cov)
}

# Load cross validation data --------------------------------------
data_cv <- read.csv(paths$input$data_cv)

# Add new variables to cv data ------------------------------------

data_cv <- data_cv %>% 
  mutate(cv_id = 0,
         stratum_id = str_sub(id, 7, 9),
         age_group = paste("[", age_start, ",", age_start+age_width, ")", sep = ""),
         date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  rename(obs_id = id,
         observed = deaths,
         exposure = personweeks,
         iso_year = year,
         iso_week = week,) %>%
  mutate(epi_week = epiweek(date),
         epi_year = epiyear(date),
         country_iso = NULL,
         age_start = NULL,
         age_width = NULL,
         obs_id = NULL,
         stratum_id = NULL)

data_cv <- data_cv %>%
  dplyr::select(-c(region_level, hemisphere, continent, country_name,
         population, holiday, covid_cases, covid_deaths, covid_tests, temp_c_popwgt))
  
# Create Total sex and Total age group ----------------------------

library(data.table)

data_cv <-
  as.data.table(data_cv)

strata_cols <-
  c('region_iso', 'cv_id', 'sex', 'epi_year', 'epi_week',
    'age_group', 'iso_year', 'iso_week', 'date', 'region_name') 

value_cols <-
  c('observed', 'exposure')

data_cv_totals <-
  groupingsets(
    data_cv,
    j = lapply(.SD, sum),
    by = strata_cols,
    sets = list(
      strata_cols,
      strata_cols[-which(strata_cols %in% 'sex')],
      strata_cols[-which(strata_cols %in% 'age_group')],
      strata_cols[-which(strata_cols %in% c('sex', 'age_group'))]
    ),
    .SDcols = value_cols
  )

data_cv_totals <-
  as_tibble(data_cv_totals) %>%
  mutate(
    age_group = as.character(age_group),
    age_group = ifelse(is.na(age_group), 'Total', age_group),
    sex = ifelse(is.na(sex), 'Total', sex)
  )


# Split cross validation data into series -------------------------

cut_cv_data <- function(start_covid, training_length, test_length, cv_data){

  last_observed <- start_covid + as.difftime(test_length - 1, unit = "weeks")
  
  number_of_series <- floor(length(seq(from = min(cv_data$date), to = last_observed, by = 'weeks')) / test_length) - ceiling(training_length/test_length)
  
  end_dates_of_series <- integer(0)
  class(end_dates_of_series) <- "Date"
  
  for (i in 1:(number_of_series-1)) {
    end_dates_of_series[i] <- last_observed - as.difftime(i * test_length, unit = 'weeks')
  }
  end_dates_of_series <- rev(end_dates_of_series)
  end_dates_of_series[number_of_series] <- last_observed
  
  data_cv_series <- list()
  
  for (i in 1:number_of_series) {
    data_cv_series[[i]] <- cv_data %>%
      filter(
        date %in% rev(seq(end_dates_of_series[i],
                          by = '-1 weeks',
                          length = training_length + test_length))
      ) %>%
      mutate(origin_date = end_dates_of_series[i] - as.difftime(training_length + test_length - 1, unit = "weeks"),
             origin_date_test = end_dates_of_series[i] - as.difftime(test_length - 1, units = "weeks")
      ) %>%
      mutate(cv_id = i)
    
    data_cv_series[[i]] <- data_cv_series[[i]] %>%
      mutate(origin_weeks = as.numeric(difftime(date, origin_date, units = "weeks")),
             weeks_since_test_start = as.numeric(difftime(date, origin_date_test, units = "weeks"))
      )
    
    data_cv_series[[i]] <- data_cv_series[[i]] %>%
      mutate(cv_sample = if_else(weeks_since_test_start >= 0, "test", "training"))
  }
  
  cut_stats <- within(list(), {
    number_of_series = number_of_series
    end_dates_of_series = end_dates_of_series
  })
  
  print(cut_stats)
  return(bind_rows(data_cv_series))
}


data_cv_series <- cut_cv_data(start_covid = cnst$start_covid,
                           training_length = cnst$training_length,
                           test_length = cnst$test_length,
                           cv_data = data_cv_totals)





# Subset cross validation data ------------------------------------

data_cv_sub <-
  data_cv_series %>%
  filter(
    region_iso %in% cnst$countries,
    sex == 'Total', age_group == 'Total', cv_id != 0
  ) %>%
  mutate(
    loop_strata = interaction(cv_id, region_iso)
  )



# Predict deaths over cv series -----------------------------------

predictions <- list()

predictions$cv <-
  CountGAM(
    data_cv_sub,
    observed ~
      1 + origin_weeks + s(epi_week, bs = 'cp') +
      offset(log(exposure)),
    family = nb(link = 'log'),
    col_sample = 'cv_sample',
    col_stratum = 'loop_strata',
    nsim = 500
  )

fig$cv <-
  predictions$cv %>%
  filter(region_iso == 'DE', cv_id %in% cnst$cv_series_calibration) %>%
  ggplot() +
  aes(x = date, y = observed, color = cv_sample) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_line(aes(y = predicted)) +
  facet_wrap(~ cv_id, ncol = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma, n.breaks = 4) +
  scale_color_manual(
    values = c(training = '#117396', test = '#f51883')
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Observed Deaths', x = NULL)
fig$cv


cv_series_text <- data.frame(label = c("Calibration",
                                       rep(" ", length(cnst$cv_series_calibration)-1),
                                       "Validation",
                                       rep(" ", length(cnst$cv_series_validation)-1),
                                       rep("Application", length(cnst$cv_series_application))),
                             cv_id = c(cnst$cv_series_calibration,
                                       cnst$cv_series_validation,
                                       cnst$cv_series_application),
                             x = as.Date("2011-08-01"),
                             y = 25000)

fig$series <-
  predictions$cv %>%
  filter(region_iso == 'DE') %>%
  ggplot() +
  geom_point(aes(x = date, y = observed, color = cv_sample), size = 0.1, alpha = 0.3) +
  geom_line(aes(x = date, y = predicted, color = cv_sample)) +
  facet_wrap(~ cv_id, ncol = 1) +
  scale_x_date(date_breaks = '2 year', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma, n.breaks = 4) +
  scale_color_manual(
    values = c(test = '#f51883', training = '#117396'),
    labels = c("Testing Period", "Training Period")
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  #guides(color = 'none') +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.77),
        legend.text = element_text(size = 8)) +
  labs(y = 'Observed Deaths', x = NULL) + 
  geom_text(data = cv_series_text, mapping = aes(x = x, y = y, label = label))
fig$series

ExportFigure(
  fig$series, paths$output$figure, filename = 'cv_series_de',
  device = 'pdf', width = 150, height = 150
)

# Learn forecasting error -----------------------------------------

forecasting_error <- list()

# forecasts and forecasting errors
forecasting_error$error_series <-
  predictions$cv %>%
  filter(cv_sample == 'test') %>%
  mutate(month = as.factor(lubridate::month(date))) %>%
  mutate(
    resid = observed-predicted,
    logrelerror = log(observed/predicted)
  )

# error model specifications
forecasting_error$specs <- tribble(
  ~model_id, ~model_spec,
  'NO', list(
    type = 'gamlss',
    family = NO(sigma.link = 'log'),
    formula = as.formula(logrelerror~-1),
    sigma.formula = as.formula(~ 1 + weeks_since_test_start + pbc(epi_week)),
    nu.formula = as.formula(~ 1),
    tau.formula = as.formula(~ 1)
  ),
  'SNO', list(
    type = 'gamlss',
    family = SN1(sigma.link = 'log'),
    formula = as.formula(logrelerror~1),
    sigma.formula = as.formula(~ 1 + weeks_since_test_start + pbc(epi_week)),
    nu.formula = as.formula(~ 1 + pbc(epi_week)),
    tau.formula = as.formula(~ 1)
  ),
  'NB', list(
    type = 'model'
  )
)

# merge data with model definitions
forecasting_error$for_fit <-
  forecasting_error$error_series %>%
  nest(data = c(-region_iso)) %>%
  expand_grid(forecasting_error$specs)

# iterate in parallel model, region
forecasting_error$fitted <- foreach(
  x = iter(forecasting_error$for_fit, by = 'row'),
  .combine = bind_rows,
  .packages = c('dplyr', 'tidyr', 'gamlss')
) %dopar% {suppressPackageStartupMessages({
  
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Fit ", x$model_id,  " in ", x$region_iso, "\n",
      sep = ""
  )
  
  # input data
  input_data <- x[, "data"][[1]][[1]]
  
  # the errors to learn
  training <- input_data %>%
    filter(cv_id %in% cnst$cv_series_calibration)
  # the forecasted data series
  # needs to have the same length as the test series in each cv split
  forecast <- input_data %>%
    filter(cv_id %in% cnst$cv_series_validation)
  
  # forecast
  forecasted_errors <-
    forecast %>%
    filter(cv_id == cnst$cv_series_validation[1]) %>%
    dplyr::select(weeks_since_test_start, epi_week)
  
  # fit models and capture errors
  result <- tryCatch({
  
    # model parametrization
    model_para <- x$model_spec[[1]]
      
    # model out-of-sample error by forecasting horizon
    if (identical(model_para$type, 'gamlss')) {
      
      fit <- gamlss(
        formula = eval(model_para$formula),
        sigma.formula = eval(model_para$sigma.formula),
        nu.formula = eval(model_para$nu.formula),
        tau.formula = eval(model_para$tau.formula),
        family = eval(model_para$family),
        data = training,
        control = gamlss.control(n.cyc = 40)
      )
      
      # reconstruct the call to gamlss because the package
      # messes up the storage of that call evaluation when
      # in an foreach environment
      fit$call <-
        call('gamlss', model_para$formula, model_para$sigma.formula,
             model_para$nu.formula, model_para$tau.formula,
             model_para$family, training)
      
      predicted_time_varying_params <-
        predictAll(
          object = fit, data = training,
          newdata = forecasted_errors,
          type = 'response', output = 'list'
        )
      
      predicted_time_varying_params[['y']] <- NULL
      
      # get the name of the quantile function corresponding
      # to the distribution of our fitted model
      quantile_name <- paste0('q', model_para$family$family[1])
      distribution_name <- paste0('p', model_para$family$family[1])
  
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <- bind_cols(
        forecasted_errors,
        q1 = do.call(quantile_name, c(p = cnst$quantiles[1],
                                      predicted_time_varying_params)),
        q2 = do.call(quantile_name, c(p = cnst$quantiles[2],
                                      predicted_time_varying_params)),
        q3 = do.call(quantile_name, c(p = cnst$quantiles[3],
                                      predicted_time_varying_params)),
        q4 = do.call(quantile_name, c(p = cnst$quantiles[4],
                                      predicted_time_varying_params)),
        pscore_10p = do.call(distribution_name, c(q = log(1.1),
                                                  predicted_time_varying_params,
                                                  lower.tail = FALSE))
      )
      
      predicted_quantiles_of_forecast_distribution <-
        input_data %>% 
        left_join(
          predicted_quantiles_of_error_distribution %>%
            dplyr::select(-epi_week),
          by = 'weeks_since_test_start'
        ) %>%
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(c(q1, q2, q3, q4), ~exp(.)*predicted, .names = 'PI{.col}'))
      
    }

    # model out-of-sample error by forecasting horizon
    if (identical(model_para$type, 'model')) {
      
      predicted_time_varying_params <- NA
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      simu_cols <- grepl('^simulated.+', names(input_data))
      simulated_errors <- apply(input_data[,simu_cols], 2, function (x) log(x)-log(input_data[,'predicted'][[1]]))
      
      predicted_quantiles_of_error_distribution <- NA
      
      predicted_quantiles_of_forecast_distribution <-
        bind_cols(
          input_data,
          q1 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[1]),
          q2 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[2]),
          q3 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[3]),
          q4 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[4]),
          pscore_10p = apply(simulated_errors, 1, function (x) 1-ecdf(x)(log(1.1)))
        ) %>%
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(c(q1, q2, q3, q4), ~exp(.)*predicted, .names = 'PI{.col}'))
      
    }
    
    # return result if fitting succeeded
    result_if_no_error <- bind_cols(
      x,
      tibble(
        prediction = list(predicted_quantiles_of_forecast_distribution),
        predicted_quantiles = list(predicted_quantiles_of_error_distribution),
        predicted_parameters = list(predicted_time_varying_params)
      ),
      error_while_fit = FALSE,
      error_message = NA
    )
      
    return(result_if_no_error)
    
  },
  
  # return result if fitting did not succeed
  error = function(e) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Error on ", x$model_id, " in ", x$region_iso, ": ",
      geterrmessage(), "\n"
    )
    # return same object as fitted model, but with NA predictions
    input_data$q1 <- NA; input_data$q2 <- NA
    input_data$q3 <- NA; input_data$q4 <- NA
    input_data$PIq1 <- NA; input_data$PIq2 <- NA
    input_data$PIq3 <- NA; input_data$PIq4 <- NA
    result_if_error <- bind_cols(
      x,
      tibble(
        prediction = list(input_data),
        predicted_quantiles = NA,
        predicted_parameters = NA
      ),
      error_while_fit = TRUE,
      error_message = geterrmessage()
    )
    return(result_if_error)
  }
  ) # end of tryCatch()
  
  return(result)
  
})} # end of dopar(suppressPackageStartupMessages)

stopCluster(cnst$cl)

# Plot learned errors ---------------------------------------------

fig$relative_errors <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_calibration) %>%
  filter(region_iso == 'DE', model_id == 'SNO') %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = logrelerror) +
  geom_hline(yintercept = log(1.05), color = 'grey50') +
  geom_hline(yintercept = log(1/1.05), color = 'grey50') +
  geom_hline(aes(yintercept = mean(logrelerror)), color = '#f51883') +
  geom_ribbon(
    aes(x = weeks_since_test_start, ymin = q1, ymax = q4),
    color = NA, fill = 'grey70', alpha=0.5
  ) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_point(color = '#f51883', size = 0.2) +
  #facet_wrap(~ region_iso) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c(training = '#117396', test = '#f51883')
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'log(observed/predicted)', x = 'Weeks into forecasting period')
fig$relative_errors

ExportFigure(
  fig$relative_errors, paths$output$figure,
  filename = 'empsno_relative_errors',
  device = 'pdf', width = 150
)

# Plot learned errors over forecasting series of interest ---------
model_id_labs <- c("(a) Negative Binomial PIs", "(b) Empirical PIs")
names(model_id_labs) <- c("NB", "SNO")

fig$observed_vs_predicted_val <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_validation[2], region_iso == 'DE') %>% 
  filter(model_id %in% c('SNO', 'NB')) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_ribbon(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq4),
    color = NA, fill = 'grey70'
  ) +
  geom_point(aes(y = observed), color = '#f51883', size = 0.2) +
  geom_line(color = '#860644') +
  facet_wrap(~ model_id, scales = 'free_y', labeller = labeller(model_id = model_id_labs)) +
  scale_y_continuous(labels = scales::comma) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Deaths', x = 'Weeks into forecasting period')
fig$observed_vs_predicted_val

ExportFigure(
  fig$observed_vs_predicted_val, paths$output$figure,
  filename = 'observed_vs_predicted_nb_validation',
  device = 'pdf', width = 150
)

fig$observed_vs_predicted_appl <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_application, region_iso == 'DE') %>% 
  filter(model_id %in% c('SNO', 'NB')) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_ribbon(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq4),
    color = NA, fill = 'grey70'
  ) +
  geom_point(aes(y = observed), color = '#f51883', size = 0.2) +
  geom_line(color = '#860644') +
  facet_wrap(~ model_id, scales = 'free_y', labeller = labeller(model_id = model_id_labs)) +
  scale_y_continuous(labels = scales::comma) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Deaths', x = 'Weeks into forecasting period')
fig$observed_vs_predicted_appl

ExportFigure(
  fig$observed_vs_predicted_appl, paths$output$figure,
  filename = 'observed_vs_predicted_nb_application',
  device = 'pdf', width = 150
)

fig$observed_vs_predicted_appendix <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_application, region_iso %in% cnst$appendix_cntr) %>% 
  filter(model_id %in% c('SNO')) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_ribbon(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq4),
    color = NA, fill = 'grey70'
  ) +
  geom_point(aes(y = observed), color = '#f51883', size = 0.2) +
  geom_line(color = '#860644') +
  facet_wrap(~ factor(region_iso, levels = c('NO', 'SE', 'FI',
                                             'GB-SCT', 'NL', 'DE',
                                             'AT', 'PL', 'LV',
                                             'ES', 'HR', 'BG')),
             ncol = 3, scales = 'free_y') +
  scale_y_continuous(labels = scales::comma) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Deaths', x = 'Weeks into forecasting period')
fig$observed_vs_predicted_appendix

ExportFigure(
  fig$observed_vs_predicted_appendix, paths$output$figure,
  filename = 'observed_vs_predicted_nb_appendix',
  device = 'pdf', width = 150, height = 150
)

# Calculate calibration scores ------------------------------------

calibration_table <- forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_validation) %>%
  group_by(model_id, region_iso) %>%
  summarise(
    COV = Coverage(observed, PIq1, PIq4, na.rm = TRUE),
    MIS = MIS(observed, PIq1, PIq4, alpha = 0.1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = model_id, values_from = c(COV, MIS)) %>%
  mutate(
    SNO_best = MIS_SNO < MIS_NB,
    SNO_improvement = round((MIS_SNO - MIS_NB)/MIS_NB*100, 2)
  )

calibration_table %>% dplyr::select(COV_NB, MIS_NB, COV_SNO, MIS_SNO) %>% View()

# 10% P-Score -----------------------------------------------------

# What's the probability to observe a P-Score of 10% in the
# second fall of the COVID-19 pandemic, under the Null
# Hypothesis of continuing of pre-COVID trends and associated
# distribution of mortality fluctuations?

fig$pvalue10p <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(
    cv_id %in% cnst$cv_series_calibration, model_id %in% c('NB', 'SNO'),
    region_iso == 'DE'
  ) %>%
  group_by(model_id, region_iso, weeks_since_test_start) %>%
  summarise(
    threshold = mean(pscore_10p, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = threshold, color = model_id) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05,
            color = NA, fill = 'grey90') +
  geom_line(
 #   color = '#860644'
  ) +
  scale_color_manual(values = c('#117396', '#f51883'), labels = c("Negative Binomial PI", "Empirical PI")) +
  #facet_wrap(~ region_iso) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.20)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.65),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'p-value', x = 'Weeks into forecasting period',
       title = 'p-value of 10% excess deaths given H0: "continuation of past trends"')
fig$pvalue10p

ExportFigure(
  fig$pvalue10p, paths$output$figure,
  filename = 'pvalue_nb_de',
  device = 'pdf', width = 150
)

fig$pvalue10p_appendix <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(
    cv_id %in% cnst$cv_series_calibration, model_id %in% c('NB', 'SNO'),
    region_iso %in% cnst$appendix_cntr
  ) %>%
  group_by(model_id, region_iso, weeks_since_test_start) %>%
  summarise(
    threshold = mean(pscore_10p, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = threshold, color = model_id) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05,
            color = NA, fill = 'grey90') +
  geom_line(
    #   color = '#860644'
  ) +
  scale_color_manual(values = c('#117396', '#f51883'), labels = c("Negative Binomial PI", "Empirical PI")) +
  facet_wrap(~ factor(region_iso, levels = c('NO', 'SE', 'FI',
                                             'GB-SCT', 'NL', 'DE',
                                             'AT', 'PL', 'LV',
                                             'ES', 'HR', 'BG')),
              ncol = 3
  ) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.25)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(), legend.position = c(0.5, 0.9),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'p-value', x = 'Weeks into forecasting period',
       title = 'p-value of 10% excess deaths given H0: "continuation of past trends"')
fig$pvalue10p_appendix

ExportFigure(
  fig$pvalue10p_appendix, paths$output$figure,
  filename = 'pvalue_nb_appendix',
  device = 'pdf', width = 150, height = 150
)

# same figure for the rest of the countries
fig$pvalue10p_rest <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(
    cv_id %in% cnst$cv_series_calibration, model_id %in% c('NB', 'SNO'),
    region_iso %in% cnst$rest_cntr
  ) %>%
  group_by(model_id, region_iso, weeks_since_test_start) %>%
  summarise(
    threshold = mean(pscore_10p, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = threshold, color = model_id) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05,
            color = NA, fill = 'grey90') +
  geom_line(
    #   color = '#860644'
  ) +
  scale_color_manual(values = c('#117396', '#f51883'), labels = c("Negative Binomial PI", "Empirical PI")) +
  facet_wrap(~ factor(region_iso, levels = c('IS', 'BE', 'CH',
                                             'EE', 'FR', 'HU',
                                             'SK', 'IL', 'LT',
                                             'LU', 'PT', 'SI')),
             ncol = 3
  ) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.25)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(), legend.position = c(0.5, 0.9),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'p-value', x = 'Weeks into forecasting period',
       title = 'p-value of 10% excess deaths given H0: "continuation of past trends"')
fig$pvalue10p_rest

# 90% Power P-Score -----------------------------------------------

# How high must excess deaths be so that they fall outside of the 95%
# prediction interval?
fig$powerp95 <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_calibration) %>%
  filter(region_iso %in% c('DE'), model_id %in% c('NB', 'SNO')) %>%
  group_by(model_id, region_iso, weeks_since_test_start) %>%
  summarise(
    threshold = mean(q4, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = threshold, color = model_id) +
  geom_line(
 #   color = '#860644'
    ) +
  facet_wrap(~ region_iso) +
  scale_y_continuous(labels = scales::percent, n.breaks = 10,
                     limits = c(0, 0.30)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(
    y = '% excess deaths at 95% prediction interval of expected',
    x = 'Weeks into forecasting period',
    title = 'Detection limits of excess deaths vary by season'
  )
fig$powerp95

ExportFigure(
  fig$powerp95, paths$output$figure,
  filename = 'power_gam_nb_de',
  device = 'pdf', width = 150
)

