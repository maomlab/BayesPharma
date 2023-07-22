
#' Helper function to parse command line arguments for the BayesPharma CLI
#'
#' @description Pass in the command line arguments as a list and this will
#' use the \pkg{optparse} package to parse them.
#'
#' Available arguments are
#'   -v, --verbose
#'     Print extra output
#'
#'   --data=DATA
#'     path to either .csv, .tsv, .xls or .xlsx data file
#'
#'   --data_sheet=DATA_SHEET
#'     If the data file is an excel file, what sheet to use?
#'
#'   --data_range=DATA_RANGE
#'     If the data file is an excel file, what range to use?
#'
#'   --model=MODEL
#'     Which model to fit
#'
#'   --formula=FORMULA
#'     What type of formula to use? For models with a canonical formula, try to
#'     guess it.
#'
#'   --treatment_variable=TREATMENT_VARIABLE
#'     What variable to use as the treatment. If not specified use the default
#'     for the given formula.
#'
#'   --treatment_units=TREATMENT_UNITS
#'     What units units should be used for the treatment variable. If not
#'     specified use the default for the given formula.
#'
#'   --response_variable=RESPONSE_VARIABLE
#'     What variable to use as the response. If not specified use the default
#'     for the given formula.
#'
#'   --response_units=RESPONSE_UNITS
#'     What units units should be used for the response variable. If not
#'     specified use the default for the given formula.
#'
#'   --predictors=PREDICTORS
#'     Expression for the predictors for each of the model parameters.
#'
#'   --prior_terms=PRIOR_TERMS
#'     json formatted list of arguments for the prior function. For example, to
#'     constrain the EC50 to have a mean of 1 nM and the bottom to be 0 for for
#'     the sigmoid_agonist use `--prior_terms='[{"ec50":"brms::prior(prior =
#'     normal(-9, 0.5), nlpar = "ec50")},{"bottom":"0"}]'`
#'
#'   --init_terms=INIT_TERMS
#'     json formmated list of arguments for the init function.
#'
#'   --iter=ITER
#'     Number of sampling iterations.
#'
#'   --output_path=OUTPUT_PATH
#'     Path where the output results should be written
#'
#' @param args `list` of command line arguments
#' @returns named list of values for the available arguments
#'
cli_parse_options <- function(args) {
  optparse::OptionParser() |>
    ########## Meta Options ############
    optparse::add_option(
      opt_str = c("-v", "--verbose"),
      action = "store_true",
      default = FALSE,
      help = "Print extra output") |>
    ######## Data Options ##############
    optparse::add_option(
      opt_str = "--data",
      action = "store",
      help = "path to either .csv, .tsv, .xls or .xlsx data file") |>
    optparse::add_option(
      opt_str = "--data_sheet",
      action = "store",
      default = NULL,
      help = "If the data file is an excel file, what sheet to use?") |>
    optparse::add_option(
      opt_str = "--data_range",
      action = "store",
      default = NULL,
      help = "If the data file is an excel file, what range to use?") |>
    ######## Model Options ##############
    optparse::add_option(
      opt_str = "--model",
      action = "store",
      help = "Which model to fit") |>
    ######## Formula Options ##############
    optparse::add_option(
      opt_str = "--formula",
      action = "store",
      default = NULL,
      help = paste0(
        "What type of formula to use? For models with a canonical formula, ",
        "try to guess it.")) |>
    optparse::add_option(
      opt_str = "--treatment_variable",
      action = "store",
      default = NULL,
      help = paste0(
        "What variable to use as the treatment. If not specified use the ",
        "default for the given formula.")) |>
    optparse::add_option(
      opt_str = "--treatment_units",
      action = "store",
      default = NULL,
      help = paste0(
        "What units units should be used for the treatment variable. If not ",
        "specified use the default for the given formula.")) |>
    optparse::add_option(
      opt_str = "--response_variable",
      action = "store",
      default = NULL,
      help = paste0(
        "What variable to use as the response. If not specified use the ",
        "default for the given formula.")) |>
    optparse::add_option(
      opt_str = "--response_units",
      action = "store",
      default = NULL,
      help = paste0(
        "What units units should be used for the response variable. If not ",
        "specified use the default for the given formula.")) |>
    optparse::add_option(
      opt_str = "--predictors",
      action = "store",
      default = "1",
      help = paste0(
        "Expression for the predictors for each of the model parameters.")) |>
    ######## Prior Options ##############
    optparse::add_option(
      opt_str = "--prior_terms",
      action = "store",
      default = NULL,
      help = paste0(
        "json formatted list of arguments for the prior function. For example,",
        "to constrain the EC50 to have a mean of 1 nM and the bottom to be 0 ",
        "for for the sigmoid_agonist use --prior_terms='[{\"ec50\":\"",
        "brms::prior(prior = normal(-9, 0.5), nlpar = \"ec50\")},",
        "{\"bottom\":\"0\"}]'")) |>
    ######## Init Options ##############
    optparse::add_option(
      opt_str = "--init_terms",
      action = "store",
      default = NULL,
      help = paste0(
        "json formatted list of arguments for the init function.")) |>
    #### Sampling options ####
    optparse::add_option(
      opt_str = "--iter",
      action = "store",
      default = 8000,
      type = "numeric",
      help = paste0("Number of sampling iterations.")) |>
      #### Output options ####
    optparse::add_option(
      opt_str = "--output_path",
      action = "store",
      default = ".",
      type = "character",
      help = paste0("Path where the output results should be written")) |>
    optparse::parse_args(args = args)
}

#' Helper function to initialize output for the BayesPharma CLI
#'
#' @description check if the output directory exists and create it if it does
#'   not
#'
#' @param options `list` of command line arguments as parsed by
#'   [cli_parse_options()].
#' @returns NULL
#'
cli_init_output_path <- function(options) {
  #### Check output path before fitting model ####
  if (!dir.exists(options$output_path)) {
    if (options$verbose) {
      cat("Creating output path '", options$output_path, "'\n", sep = "")
    }
    dir.create(options$output_path)
  } else {
    if (options$verbose) {
      cat("Writing output to path '", options$output_path, "'\n", sep = "")
    }
  }
  NULL
}


#' Helper function to load data for the BayesPharma CLI
#'
#' @description load tabular data based on the extension used to fit the model
#'   Recognized data formats are .csv, .tsv, and .xlsx
#'
#' @param options `list` of command line arguments as parsed by
#'   [cli_parse_options()].
#' @returns `data.frame`
#'
cli_load_data <- function(options) {
  ### Load data ###
  if (options$verbose) {
    cat("Load data file '", options$data, "' ...\n", sep = "")
  }
  if (!file.exists(options$data)) {
    stop("Path to data file: ", options$data, " does not exist.")
  }
  data_extension <- options$data |> stringr::str_extract("[.][^.]*$")
  if (data_extension == ".csv") {
    data <- readr::read_csv(
      file = options$data,
      show_col_types = FALSE)
  } else if (data_extension == ".tsv") {
    data <- readr::read_tsv(
      options$data,
      show_col_types = FALSE)
  } else if (data_extension == ".xls" || data_extension == ".xslx") {
    data <- readxl::read_excel(
      path = options$data,
      sheet = options$data_sheet,
      range = options$data_range)
  } else {
    stop(paste0(
      "Unrecognized data extension '", data_extension, "'. ",
      "It should be one of [.csv, .tsv, .xls, .xlsx]"))
  }
  data
}


#' Helper function to load model function the BayesPharma CLI
#'
#' @description Initialize model function based on the command line arguments
#'   Available models are those defined in `BayesPharma::<model>_model`
#'
#' @param options `list` of command line arguments as parsed by
#'   [cli_parse_options()].
#' @returns `function` used to fit the requested model
#'
cli_load_model_fn <- function(options) {
  ### Get model ###
  if (options$verbose) {
    cat(
      "Initializing model 'BayesPharma::", options$model, "_model'\n",
      sep = "")
  }
  tryCatch({
    model_fn <- paste0("BayesPharma::", options$model, "_model") |>
      rlang::parse_expr() |>
      eval()
  }, error = function(e) {
    available_models <-  data.frame(name = ls("package:BayesPharma")) |>
      dplyr::filter(.data[["name"]] |> stringr::str_detect("_model$")) |>
      dplyr::mutate(
        name = .data[["name"]] |> stringr::str_replace("_model$", "")) |>
      purrr::pluck("name")
    stop(paste0(
      "Unrecognized model '", options$model, "'. The --model option must be ",
      "one of [", paste0(available_models, collapse = ", "), "]\n"))
  })
  model_fn
}


#' Helper function to load formula the BayesPharma CLI
#'
#' @description Initialize formula based on the command line arguments. For the
#'   some models support multiple formulas, otherwise use the default one for
#'   the model.
#'
#' @param options `list` of command line arguments as parsed by
#'   [cli_parse_options()].
#' @returns `BayesPharma::bpformula`
#'
cli_load_formula <- function(options) {
  ### Make formula ###
  if (options$model == "sigmoid") {
    if (options$verbose) {
      cat(
        "Initializing formula '", options$formula, "' ",
        "for the sigmoid model ...\n", sep = "")
    }
    # currently only the sigmoid model has different formulas
    if (options$formula == "sigmoid_agonist") {
      formula_fn <- BayesPharma::sigmoid_agonist_formula
    } else if (options$formula == "sigmoid_antagonist_formula") {
      formula_fn <- BayesPharma::sigmoid_antagonist_formula
    } else {
      stop(paste0(
        "For the sigmoid model, please specify either 'sigmoid_agonist' or ",
        "'sigmoid_antagonist' formula argument"))
    }
  } else {
    if (options$verbose) {
      cat(
        "Initializing the formula for model '", options$model, "' ...\n",
        sep = "")
    }
    canonical_formula_name <- paste0(options$model, "_formula")
    if (!is.null(options$formula) &&
        options$formula != canonical_formula_name) {
      stop(paste0(
        "For the model '", options$model, "' please don't specify the formula ",
        "argument or if you do, please use --formula=", options$model, "\n"))
    }
    formula_fn <- paste0("BayesPharma::", options$model, "_formula") |>
      rlang::parse_expr() |>
      eval()
  }

  if (options$verbose) {
    cat("Setting the treatment and response variables ...\n")
  }
  if (options$model == "MuSyC") {
    # the MuSyC formula takes two different treatments
    if (is.null(options$treatment_variable)) {
      treatment_1_variable <- "logd1"
      treatment_2_variable <- "logd2"
    } else {
      if (options$treatment_variable |> stringr::str_count(",") != 1) {
        stop(paste0(
          "Unable to parse specified treatment variables '",
          options$treatment_variable, "' for the MuSyC model. It should have ",
          "two column names in the input dataframe separated by ','."))
      }
      treatment_variables <- options$treatment_variable |>
        stringr::str_split(sep = ",")
      treatment_1_variable <- treatment_variables[1]
      treatment_2_variable <- treatment_variables[2]
    }

    if (is.null(options$treatment_units)) {
      treatment_1_units <- "Log[Molar]"
      treatment_2_units <- "Log[Molar]"
    } else {
      if (options$treatment_units |> stringr::str_count(",") != 1) {
        stop(paste0(
          "Unable to parse specified treatment units '",
          options$treatment_units, "' for the MuSyC model. It should have ",
          "two values separated by ','."))
      }
      treatment_units <- options$treatment_units |>
        stringr::str_split(sep = ",")
      treatment_1_units <- treatment_units[1]
      treatment_2_units <- treatment_units[2]
    }

    if (options$verbose) {
      cat(
        " -treatment_1_variable: '", options$treatment_1_variable, "' ",
        "with units '", options$tratment_1_units, "'\n", sep = "")
      cat(
        " -treatment_2_variable: '", options$treatment_2_variable, "' ",
        "with units '", options$treatment_2_units, "'\n", sep = "")
      cat(
        " -response_variable: '", options$response_variable, "' ",
        "with units '", options$response_units, "'\n", sep = "")
      cat(" -predictors: '", options$predictors, "'\n", sep = "")
    }

    formula <- formula_fn(
      treatment_1_variable = treatment_1_variable,
      treatment_1_units = treatment_1_units,
      treatment_2_variable = treatment_2_variable,
      treatment_2_units = treatment_2_units,
      response_variable = options$response_variable,
      response_units = options$response_units,
      predictors = rland::parse_expr(options$predictors))
  } else {

    if (options$verbose) {
      if (
        is.null(options$treatment_variable) ||
        is.null(options$treatment_units)) {
        cat(
          " -treatment_variable: '", options$treatment_variable, "' ",
          "with units '", options$tratment_units, "'\n", sep = "")
      }
      if (
        is.null(options$response_variable) ||
        is.null(options$response_units)) {
        cat(
         " -response_variable: '", options$response_variable, "' ",
         "with units '", options$response_units, "'\n", sep = "")
      }
      if (options$predictors != '1') {
        cat(" -predictors: '", options$predictors, "'\n", sep = "")
      }
    }

    formula <- do.call(
      what = formula_fn,
      args = c(
        ifelse(
          is.null(options$treatment_variable), NULL,
          list(treatment_variable = options$treatment_variable)),
        ifelse(
          is.null(options$treatment_units), NULL,
          list(treatment_units = options$treatment_units)),
        ifelse(
          is.null(options$response_variable), NULL,
          list(response_variable = options$response_variable)),
        ifelse(
          is.null(options$response_units), NULL,
          list(response_units = options$response_units)),
        ifelse(
          is.null(options$predictors), NULL,
          list(predictors = rlang::parse_expr(options$predictors)))))
  }
  formula
}


#' Helper function to load prior the BayesPharma CLI
#'
#' @description Initialize prior based on the command line arguments.
#' 
#' @param options `list` of command line arguments as parsed by
#'   [cli_parse_options()].
#' @returns `brms::brmsprior`
#'
cli_load_prior <- function(options) {
  ### Make prior ###
  if (options$verbose) {
    cat("Initialize the prior\n")
  }

  prior_fn <- paste0("BayesPharma::", options$formula, "_prior") |>
    rlang::parse_expr() |>
    eval()

  if (!is.null(options$prior_terms)) {
    prior_args <- jsonlite::fromJSON(
      txt = options$prior_terms) |>
      purrr::map(~ .x |> rlang::parse_expr() |> eval())
  } else {
    prior_args <- list()
  }
  prior <- do.call(
    what = prior_fn,
    args = prior_args)
  if (options$verbose) {
    print(prior)
  }
  prior
}

#' Helper function to load init the BayesPharma CLI
#'
#' @description Initialize init based on the command line arguments.
#' 
#' @param options `list` of command line arguments as parsed by
#'   [cli_parse_options()].
#' @returns `function`
#'
cli_load_init <- function(options) {
  ### Make init ###
  if (options$verbose) {
    cat("Initialize init\n")
  }
  init_fn <- paste0("BayesPharma::", options$formula, "_init") |>
    rlang::parse_expr() |>
    eval()

  if (!is.null(options$init_terms)) {
    init_args <- jsonlite::fromJSON(
      txt = options$init_terms) |>
      purrr::map(~ .x |> rlang::parse_expr() |> eval())
  } else {
    init_args <- list()
  }
  init <- do.call(
    what = init_fn,
    args = init_args)
  init
}


#' Fit model using command line arguments
#'
#' @description Provide a command line interface to fitting BayesPharma models
#'
#' @param args `list` of command line arguments
#'
#' @export
fit_model_cli <- function(
    args = commandArgs(trailingOnly = TRUE)) {

  options <- cli_parse_options(args)

  # do this first in case it fails
  cli_init_output_path(options)

  data <- cli_load_data(options)


  model_fn <- cli_load_model_fn(options)
  formula <- cli_load_formula(options)
  prior <- cli_load_prior(options)
  init <- cli_load_init(options)

  if (options$verbose) {
    cat("fit model ...\n")
  }
  model <- model_fn(
    data = data,
    formula = formula,
    prior = prior,
    init = init,
    iter = options$iter)

  if (options$verbose) {
    cat(
      "Saving model to '", paste0(options$output_path, "/model.Rdata"),
      "' ...\n", sep = "")
  }
  save(model, file = paste0(options$output_path, "/model.Rdata"))

  if (options$verbose) {
    cat(
      "Saving fit summary to '",
      paste0(options$output_path, "/fit_summary.txt"),
      "' ...\n", sep = "")
  }
  sink(file = paste0(options$output_path, "/fit_summary.txt"))
  summary(model)
  sink()

  if (options$verbose) {
    cat(
      "Saving parameter estimates to '",
      paste0(options$output_path, "/parameter_estimates.tsv"),
      "' ...\n", sep = "")
  }
  model |>
    posterior::summarise_draws() |>
    readr::write_tsv(
      file = paste0(options$output_path, "/parameter_estimates.tsv"))
}
