## Custom PrestoGP with all arguments of Y and X replaced by y and x.
## ISong 03/01/2024

library(tidymodels)
library(parsnip)
library(PrestoGP)

tidymodels_prefer(quiet = TRUE)

# definition: initiate the model
parsnip::set_new_model("prestogp_univecchia")

# set regression mode
parsnip::set_model_mode(
  model = "prestogp_univecchia",
  mode = "regression"
)

# set the engine
parsnip::set_model_engine(
  model = "prestogp_univecchia",
  mode = "regression",
  eng = "prestogp"
)

# link the engine to the package
parsnip::set_dependency(
  model = "prestogp_univecchia",
  eng = "prestogp",
  pkg = "PrestoGP"
)

# model definition: in this case we use VecchiaModel()
parsnip::set_model_arg(
  model = "prestogp_univecchia",
  eng = "prestogp",
  parsnip = "n_neighbors",
  original = "n_neighbors",
  func = list(pkg = "PrestoGP", fun = "VecchiaModel"),
  has_submodel = FALSE
)

# an object that will be used for tidymodels interface
prestogp_univecchia <-
  function(mode = "regression",  n_neighbors = 30L) {
    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }

    # Capture the arguments in quosures
    args <- list(n_neighbors = rlang::enquo(n_neighbors))

    # Save some empty slots for future parts of the specification
    new_model_spec(
      cls = "prestogp_univecchia",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }


# parsnip::set_fit(
#   model = "prestogp_univecchia",
#   eng = "prestogp",
#   mode = "regression",
#   value = list(
#     interface = "data.frame",
#     protect = c("model"),
#     func = c(pkg = "PrestoGP", fun = "prestogp_fit"),
#     defaults = list(
#       locs = NULL,
#       Y = NULL,
#       X = NULL,
#       parallel = FALSE
#     )
#   )
# )

# set_fit assumes that the model has standard fit and predict interface
# where `formula` and `data` or `x` and `y` are passed.
prestogp_uni_fit_set <-
  function(
    target_model = "prestogp_univecchia",
    engine = "prestogp",
    mode = "regression",
    locs,
    y,
    x,
    parallel = FALSE
  ) {
    parsnip::set_fit(
      model = target_model,
      eng = engine,
      mode = mode,
      value = list(
        interface = "matrix",
        protect = list("data"),
        func = c(pkg = "PrestoGP", fun = "prestogp_fit"),
        defaults = list(
          locs = rlang::enquo(locs),
          # y = y,
          # x = x,
          parallel = FALSE
        )
      )
    )
  }


# treating the predictor(s) and settings for the model specification
parsnip::set_encoding(
  model = "prestogp_univecchia",
  eng = "prestogp",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

# Way of handling prediction results
predict_info <-
  list(
    pre = NULL,
    post = function(out) { data.frame(mean = out$mean, variance = out$meanvar) },
    func = c(pkg = "PrestoGP", fun = "prestogp_predict"),
    args =
      # These lists should be of the form:
      # {predict.mda argument name} = {values provided from parsnip objects}
      list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. `type` is a simple object that
        # doesn't need to have its evaluation deferred.
        model = quote(object$fit),
        x = quote(x),
        locs = quote(locs_new),
        return.values = quote(c("mean", "meanvar")),
        type = NULL
      )
  )

# setting the prediction method
parsnip::set_pred(
  model = "prestogp_univecchia",
  eng = "prestogp",
  mode = "regression",
  type = "raw",
  value = predict_info
)

# look at the model specification
translate(prestogp_univecchia() %>% set_engine("prestogp"))


# nolint start
ze <-
  fst::read_fst("../../../../group/set/Projects/PrestoGP_Pesticides/output/Covariates_Calculated/data_AZO_covariates_zerofill.fst")
ze_simazine <- ze %>% filter(ChmclNm == "Simazine") %>% select(X, Y, Year, cncntrt, 21:957) %>% filter(X > 1.5e6 & Y < 1.0e6) %>%
    select(-where(is.factor)) %>% select(-where(anyNA))
# data_list <- list(locs = dd, y = NULL, x = NULL)
data_list2 <- list(y = ze_simazine$cncntrt, x = ze_simazine[, seq(5, ncol(ze_simazine))])
locs <- ze_simazine[, 1:3]
y <- matrix(data_list2$y, ncol = 1)
x <- as.matrix(data_list2$x)
rownames(y) <- rownames(x)
# rlang::inject(prestogp_uni_fit_set(!!!data_list))
# prestogp_uni_fit_set(locs = locs, y = y, x = x)
# nolint end


# error: matrix_ is unknown
# no clue in the documentation, package issues, even web search
prestogp_univecchia() %>%
  #set_mode("regression") %>%
  set_engine("prestogp") %>%
  parsnip::fit_xy(
    x = x,
    y = y
  )

# Demonstration of raw running
# dd <-
#   prestogp_fit(
#     model = VecchiaModel(),
#     y = y,
#     x = as.matrix(x),
#     locs = as.matrix(locs)
#   )

# ze_simazine_other <- ze %>%
#   filter(ChmclNm == "Simazine") %>%
#   select(X, Y, Year, cncntrt, 21:957) %>%
#   filter(!(X > 1.5e6 & Y < 1.0e6)) %>%
#   select(-where(is.factor)) %>%
#   select(all_of(names(ze_simazine)))
# locshat <- as.matrix(ze_simazine_other[, 1:3])
# xhat <- as.matrix(ze_simazine_other[, seq(5, ncol(ze_simazine_other))])

# dx <- prestogp_predict(
#   model = dd,
#   x = xhat[1:24, ],
#   locs = locshat[1:24, ]
# )
# ze_simazine_other$cncntrt[1:24] - unlist(dx$mean)
