library(tidymodels)
library(parsnip)
library(PrestoGP)

tidymodels_prefer(quiet = TRUE)

# definition: initiate the model

parsnip::set_new_model("prestogp_univecchia")
parsnip::set_model_mode(
  model = "prestogp_univecchia",
  mode = "regression"
)
parsnip::set_model_engine(
  model = "prestogp_univecchia",
  mode = "regression",
  eng = "prestogp"
)
parsnip::set_dependency(
  model = "prestogp_univecchia",
  eng = "prestogp",
  pkg = "PrestoGP"
)
parsnip::set_model_arg(
  model = "prestogp_univecchia",
  eng = "prestogp",
  parsnip = "n_neighbors",
  original = "n_neighbors",
  func = list(pkg = "PrestoGP", fun = "VecchiaModel"),
  has_submodel = FALSE
)

prestogp_univecchia <-
  function(mode = "regression",  n_neighbors = 30L) {
    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }

    # Capture the arguments in quosures
    args <- list(sub_classes = rlang::enquo(n_neighbors))

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

prestogp_uni_fit_set <-
  function(
    target_model = "prestogp_univecchia",
    engine = "prestogp",
    mode = "regression",
    locs,
    Y,
    X,
    parallel = FALSE
  ) {
    parsnip::set_fit(
    model = target_model,
    eng = engine,
    mode = mode,
    value = list(
        interface = "formula",
        protect = NULL,
        func = c(pkg = "PrestoGP", fun = "prestogp_fit"),
        defaults = list(
        locs = locs,
        Y = Y,
        X = X,
        parallel = FALSE
        )
      )
    )
  }



parsnip::set_encoding(
  model = "prestogp_univecchia",
  eng = "prestogp",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

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
        X = quote(as.matrix(X)),
        locs = quote(locs_new),
        return.values = quote(c("mean", "meanvar")),
        type = NULL
      )
  )

parsnip::set_pred(
  model = "prestogp_univecchia",
  eng = "prestogp",
  mode = "regression",
  type = "raw",
  value = predict_info
)

# nolint start
ze <-
  fst::read_fst("../../../../group/set/Projects/PrestoGP_Pesticides/output/Covariates_Calculated/data_AZO_covariates_zerofill.fst")
ze_simazine <- ze %>% filter(ChmclNm == "Simazine") %>% select(X, Y, Year, cncntrt, 21:957) %>% filter(X > 1.5e6 & Y < 1.0e6) %>%
    select(-where(is.factor)) %>% select(-where(anyNA))
data_list <- list(locs = ze_simazine[, 1:3], Y = NULL, X = NULL)
data_list2 <- list(Y = ze_simazine$cncntrt, X = ze_simazine[, seq(5, ncol(ze_simazine))])
rlang::inject(prestogp_uni_fit_set(!!!data_list))
# nolint end

prestogp_univecchia() %>%
  set_mode("regression") %>%
  set_engine("prestogp") %>%
  parsnip::fit_xy(
    x = data_list2$X,
    y = data_list2$Y
  )

