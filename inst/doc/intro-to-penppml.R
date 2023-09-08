## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup 1, eval = FALSE----------------------------------------------------
#  install.packages("penppml")

## ---- eval=FALSE--------------------------------------------------------------
#  library(penppml)

## ---- hide=TRUE, eval=FALSE---------------------------------------------------
#  devtools::load_all()

## ----filter trade data, eval=FALSE--------------------------------------------
#  selected <- countries$iso[countries$region %in% c("Americas")]
#  trade2 <- trade[(trade$exp %in% selected) & (trade$imp %in% selected), -(5:6)] # We remove columns 5 and
#  # 6 because these variables are not needed in our regressions.

## ----try hdfeppml, eval=FALSE-------------------------------------------------
#  reg1 <- hdfeppml(data = trade2,
#                   dep = "export",
#                   fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")))

## ----hdfeppml results, results = FALSE, eval=FALSE----------------------------
#  results <- data.frame(prov = rownames(reg1$coefficients), b = reg1$coefficients, se = 0)
#  results$se[!is.na(reg1$coefficients)] <- reg1$se
#  results

## ----try hdfeppml2, eval=FALSE------------------------------------------------
#  reg1_indep <- hdfeppml(data = trade2, indep=5:7,
#                   dep = "export",
#                   fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")))

## ----try mlfitpenppml lasso, results = FALSE, eval=FALSE----------------------
#  lambdas <- c(0.05, 0.025, 0.01, 0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005, 0.00025, 0.0001, 0)
#  
#  reg2 <- mlfitppml(data = trade2,
#                    dep = "export",
#                    fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")),
#                    penalty = "lasso",
#                    lambdas = lambdas)

## ----try penhdfeppml, results = FALSE, eval=FALSE-----------------------------
#  reg3 <- penhdfeppml(data = trade2,
#                    dep = "export",
#                    fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")),
#                    penalty = "lasso",
#                    lambda = 0.005)

## ----try mlfitpenppml ridge, results = FALSE, eval=FALSE----------------------
#  lambdas <- seq(0.0001, 0, length.out = 10)
#  
#  reg4 <- mlfitppml(data = trade2,
#                    dep = "export",
#                    fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")),
#                    penalty = "ridge",
#                    lambdas = lambdas)

## ----split data set by agreement, eval = FALSE--------------------------------
#  id <- unique(trade[(trade$exp %in% selected) & (trade$imp %in% selected), 5])
#  nfolds <- 20
#  unique_ids <- data.frame(id = id, fold = sample(1:nfolds, size = length(id), replace = TRUE))
#  
#  cross_ids <- merge(trade[(trade$exp %in% selected) & (trade$imp %in% selected), 5, drop = FALSE],
#                     unique_ids, by = "id", all.x = TRUE)

## ----try cross validation, eval = FALSE---------------------------------------
#  reg5 <- mlfitppml(data = trade2,
#                    dep = "export",
#                    fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")),
#                    penalty = "lasso",
#                    lambdas = c(seq(0.5, 0.1, by = -0.1), 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0),
#                    xval = TRUE,
#                    IDs =  cross_ids$fold)

## ----xval results, eval = FALSE-----------------------------------------------
#  reg5$rmse

## ----try plugin lasso, results = FALSE, eval=FALSE----------------------------
#  reg6 <- mlfitppml(data = trade2,
#                    dep = "export",
#                    fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")),
#                    penalty = "lasso",
#                    method = "plugin",
#                    cluster = c("exp", "imp"))

## ----plugin results, results = FALSE, eval=FALSE------------------------------
#  results <- data.frame(prov = rownames(reg6$beta), b_pre = reg6$beta_pre, b = reg6$beta, se = 0)
#  results$se <- reg6$ses[1,]
#  results

## ----try plugin lasso gamma, results = FALSE, eval=FALSE----------------------
#  reg6_gamma <- mlfitppml(data = trade2,
#                    dep = "export",
#                    fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")),
#                    penalty = "lasso",
#                    method = "plugin",
#                    cluster = c("exp", "imp"), gamma_val=0.3)
#  rownames(reg6$beta)[which(reg6_gamma$beta != 0)]

## ----plugin results gamma, results = FALSE, eval=FALSE------------------------
#  results_gamma <- data.frame(prov = rownames(reg6_gamma$beta), b_pre = reg6_gamma$beta_pre, b = reg6_gamma$beta, se = 0)
#  results_gamma$se <- reg6_gamma$ses[1,]
#  results_gamma

## ----try iceberg,  results = FALSE, eval = FALSE------------------------------
#  iceberg_results <- iceberg(data = trade2[, -(1:4)],
#                             dep = results$prov[results$b != 0],
#                             selectobs = (trade2$time == "2016"))

## ----iceberg correlations, results = FALSE, eval = FALSE----------------------
#  provcorr <- cor(trade2[, results$prov])
#  (provcorr <- provcorr[, results$prov[results$b != 0]])

## ---- message=FALSE, warning=FALSE, results=FALSE, eval=FALSE-----------------
#  trade3 <- trade[(trade$exp %in% selected) & (trade$imp %in% selected), ] # Now, we need id and agreement variable
#  # Let's cluster by agreement
#  trade3$alt_id <- trade3$id # ID refers to agreement ID
#  trade3$alt_id[is.na(trade3$alt_id)] <- 0 # We set this to zero when the ID is missing, interpreting this as the country pair not being part of any agreement.
#  
#  # Create pair ID
#  v1 <- do.call(paste, as.data.frame(t(apply(trade3[1:2], 1, sort))))
#  trade3$pair <-  match(v1, unique(v1))
#  trade3$pair <- trade3$pair + 500
#  
#  
#  # Create maximal ID from the preexisting ID-variable inside each pair
#  trade3 <- within(trade3, {alt_id2 = ave(alt_id,pair,FUN=max)} ) # This creates the maximum of the ID for each pair. This adjusts for the fact that some pairs might have been part of different agreements and we want to take get a unique agreement ID for each pair.
#  
#  trade3$alt_id2[trade3$alt_id2==0] <- trade3$pair[trade3$alt_id2==0]
#  unique(trade3$alt_id2) # This cluster variable collects pairs for pairs that are in agreement, uses the pair ID for those that are not.
#  # Thus, it allows errors to be clustered within agreements.
#  alt_id2 <- factor(trade3$alt_id2)
#  trade3$clus <- alt_id2 #Add the ID to the data

## ----try bootstrap lasso, warning=FALSE, message=FALSE, results = FALSE, eval=FALSE----
#  set.seed(123)
#  bs1 <- bootstrap(data=trade3, dep="export", cluster_id="clus", fixed=list(c("exp", "time"), c("imp", "time"), c("exp", "imp")), indep=7:22, bootreps=10, colcheck_x = TRUE, colcheck_x_fes = TRUE, boot_threshold = 0.01, post=TRUE, gamma_val=0.01, verbose=FALSE)

