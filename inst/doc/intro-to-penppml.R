## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup 1, eval = FALSE----------------------------------------------------
#  install.packages("penppml")

## ---- eval=FALSE--------------------------------------------------------------
#  library(penppml)

## ---- hide=TRUE, eval=TRUE----------------------------------------------------
devtools::load_all()

## ----inspect trade, echo = FALSE----------------------------------------------
knitr::kable(head(trade[, 1:10], 10), format = "pipe", caption = "Table 1: International Trade Data Set")

## ----inspect countries, echo = FALSE------------------------------------------
knitr::kable(head(countries, 10), format = "pipe", caption = "Table 2: Country Data Set",)

## ----filter trade data, eval=TRUE---------------------------------------------
selected <- countries$iso[countries$region %in% c("Americas")]
trade2 <- trade[(trade$exp %in% selected) & (trade$imp %in% selected), -(5:6)] # We remove columns 5 and 
# 6 because these variables are not needed in our regressions.

## ----try hdfeppml, eval=TRUE--------------------------------------------------
reg1 <- hdfeppml(data = trade2,
                 dep = "export",
                 fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")))

## ----hdfeppml results, results = FALSE----------------------------------------
results <- data.frame(prov = rownames(reg1$coefficients), b = reg1$coefficients, se = 0)
results$se[!is.na(reg1$coefficients)] <- reg1$se
results

## ----display results, echo = FALSE--------------------------------------------
results <- data.frame(prov = rownames(reg1$coefficients), b = reg1$coefficients, se = 0)
results$se[!is.na(reg1$coefficients)] <- reg1$se
knitr::kable(list(results[1:8, ], results[9:16, ]), 
             format = "pipe",
             col.names = c("Provision", "Coefficient", "SE"), 
             caption = "Table 3: Unpenalized PPML results",
             row.names = FALSE,
             digits = 4)

## ----try hdfeppml2, eval=FALSE------------------------------------------------
#  reg1_indep <- hdfeppml(data = trade2, indep=5:7,
#                   dep = "export",
#                   fixed = list(c("exp", "time"),
#                                c("imp", "time"),
#                                c("exp", "imp")))

## ----try mlfitpenppml lasso, results = FALSE----------------------------------
lambdas <- c(0.05, 0.025, 0.01, 0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005, 0.00025, 0.0001, 0)

reg2 <- mlfitppml(data = trade2,
                  dep = "export",
                  fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")),
                  penalty = "lasso",
                  lambdas = lambdas)

## ----plot regularization path-------------------------------------------------
results <- as.data.frame(reg2$beta_pre)
names(results) <- lambdas
results$provision <- row.names(results)
results <- reshape2::melt(results, id.vars = "provision", variable.name = "lambda", 
                          value.name = "coefficient")
results$lambda <- as.numeric(as.character(results$lambda))

ggplot2::ggplot(data = results, mapping = ggplot2::aes(x = lambda, y = coefficient, col = provision)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::scale_x_reverse(expand = ggplot2::expansion(add = c(0, 0.015))) +
  ggplot2::theme_classic() +
  directlabels::geom_dl(ggplot2::aes(label = provision), 
                        method = list(directlabels::dl.trans(x = x + 0.5), "last.bumpup")) +
  ggplot2::labs(x = "Penalty parameter (lambda)", y = "Coefficient", 
                title = "Figure 1: Regularization path for lasso")

## ----try mlfitpenppml lasso nofe, results = FALSE-----------------------------
lambdas <- c(0.025, 0.01, 0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005, 0.00025, 0.0001, 0)

reg2_nofe <- mlfitppml(data = trade2,
                  dep = "export",
                  fixed = NULL,
                  penalty = "lasso",
                  lambdas = lambdas)

## ----try penhdfeppml, results = FALSE-----------------------------------------
reg3 <- penhdfeppml(data = trade2,
                  dep = "export",
                  fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")),
                  penalty = "lasso",
                  lambda = 0.005)

## ----check penhdfeppml results------------------------------------------------
all.equal(as.vector(reg3$beta[!is.na(reg3$beta)]), as.vector(reg2$beta_pre[, 5]), tol = 1e-05)

## ----try mlfitpenppml ridge, results = FALSE----------------------------------
lambdas <- seq(0.0001, 0, length.out = 10) 

reg4 <- mlfitppml(data = trade2,
                  dep = "export",
                  fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")),
                  penalty = "ridge",
                  lambdas = lambdas)

## ----split data set by agreement----------------------------------------------
id <- unique(trade[(trade$exp %in% selected) & (trade$imp %in% selected), 5])
nfolds <- 20
unique_ids <- data.frame(id = id, fold = sample(1:nfolds, size = length(id), replace = TRUE))

cross_ids <- merge(trade[(trade$exp %in% selected) & (trade$imp %in% selected), 5, drop = FALSE], 
                   unique_ids, by = "id", all.x = TRUE)

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

## ----try cross validation nofe, eval = FALSE----------------------------------
#  reg5_nofe <- mlfitppml(data = trade2,
#                    dep = "export",
#                    fixed = NULL,
#                    penalty = "lasso",
#                    lambdas = c(seq(0.5, 0.1, by = -0.1), 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0),
#                    xval = TRUE,
#                    IDs =  cross_ids$fold)

## ----display xval results, echo = FALSE---------------------------------------
# Note for package maintainer: notice that the code above is not being evaluated when building the vignette (due to the eval = FALSE option). This is for convenience: the cross-validation algorithm takes forever to run and, since vignettes are rebuilt a couple of times every time R CMD check is run, this makes it too cumbersome to check the package. Instead, I've run the code above in a separate R session and stored the results, which I'm reproducing below (re-running the code above should produce similar results):
rmse <- structure(list(lambda = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01, 
0.005, 0.001, 5e-04, 1e-04, 0), rmse = c(108.581317035247, 108.581317035247, 
108.581317035247, 108.581317035247, 108.581317035247, 108.581317217437, 
108.575034620904, 108.574325753172, 108.573959604138, 108.573944149073, 
108.573933541391, 108.573931664275)), class = "data.frame", row.names = c(NA, 
-12L))
knitr::kable(rmse, 
             format = "pipe",
             col.names = c("Penalty (lambda)", "RMSE"), 
             caption = "Table 4: Cross-validation results",
             row.names = FALSE,
             digits = 4)

## ----try plugin lasso, results = FALSE----------------------------------------
reg6 <- mlfitppml(data = trade2,
                  dep = "export",
                  fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")),
                  penalty = "lasso",
                  method = "plugin",
                  cluster = c("exp", "imp"))

## ----plugin results, results = FALSE------------------------------------------
results <- data.frame(prov = rownames(reg6$beta), b_pre = reg6$beta_pre, b = reg6$beta, se = 0)
results$se[!is.na(reg6$beta)] <- reg6$ses
results

## ----display plugin results, echo = FALSE-------------------------------------
knitr::kable(list(results[1:7,], results[8:14,]), 
             format = "pipe",
             col.names = c("Provision", "Lasso Coefficient", "Post-Lasso Coefficient", "SE"), 
             caption = "Table 5: Plugin Lasso results",
             row.names = FALSE,
             digits = 4)

## ----try plugin lasso gamma, results = FALSE----------------------------------
reg6_gamma <- mlfitppml(data = trade2,
                  dep = "export",
                  fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")),
                  penalty = "lasso",
                  method = "plugin",
                  cluster = c("exp", "imp"), gamma_val=0.3)
rownames(reg6$beta)[which(reg6_gamma$beta != 0)]

## ----plugin results gamma, results = FALSE------------------------------------
results_gamma <- data.frame(prov = rownames(reg6_gamma$beta), b_pre = reg6_gamma$beta_pre, b = reg6_gamma$beta, se = 0)
results_gamma$se[!is.na(reg6_gamma$beta)] <- reg6_gamma$ses
results_gamma

## ----display plugin results gamma, echo = FALSE-------------------------------
knitr::kable(list(results_gamma[1:7,], results_gamma[8:14,]), 
             format = "pipe",
             col.names = c("Provision", "Lasso Coefficient", "Post-Lasso Coefficient", "SE"), 
             caption = "Table 6: Plugin Lasso results, different gamma value",
             row.names = FALSE,
             digits = 4)

## ----try iceberg,  results = FALSE--------------------------------------------
iceberg_results <- iceberg(data = trade2[, -(1:4)],
                           dep = results$prov[results$b != 0],
                           selectobs = (trade2$time == "2016"))

## ----iceberg results, results = FALSE-----------------------------------------
iceberg_results

## ----display iceberg results, echo = FALSE------------------------------------
knitr::kable(iceberg_results, 
             format = "pipe",
             caption = "Table 7: Iceberg Lasso coefficients",
             row.names = TRUE,
             digits = 4)

## ----iceberg correlations, results = FALSE------------------------------------
provcorr <- cor(trade2[, results$prov])
(provcorr <- provcorr[, results$prov[results$b != 0]])

## ----iceberg correlations table, echo = FALSE---------------------------------
knitr::kable(provcorr, 
             format = "pipe",
             caption = "Table 7: Iceberg Lasso correlations",
             row.names = TRUE,
             digits = 4)

## ---- message=FALSE, warning=FALSE, results=FALSE, eval=TRUE------------------
trade3 <- trade[(trade$exp %in% selected) & (trade$imp %in% selected), ] # Now, we need id and agreement variable
# Let's cluster by agreement
trade3$alt_id <- trade3$id # ID refers to agreement ID
trade3$alt_id[is.na(trade3$alt_id)] <- 0 # We set this to zero when the ID is missing, interpreting this as the country pair not being part of any agreement.

# Create pair ID
v1 <- do.call(paste, as.data.frame(t(apply(trade3[1:2], 1, sort))))
trade3$pair <-  match(v1, unique(v1))
trade3$pair <- trade3$pair + 500


# Create maximal ID from the preexisting ID-variable inside each pair
trade3 <- within(trade3, {alt_id2 = ave(alt_id,pair,FUN=max)} ) # This creates the maximum of the ID for each pair. This adjusts for the fact that some pairs might have been part of different agreements and we want to take get a unique agreement ID for each pair. 

trade3$alt_id2[trade3$alt_id2==0] <- trade3$pair[trade3$alt_id2==0]
unique(trade3$alt_id2) # This cluster variable collects pairs for pairs that are in agreement, uses the pair ID for those that are not.
# Thus, it allows errors to be clustered within agreements.
alt_id2 <- factor(trade3$alt_id2)
trade3$clus <- alt_id2 #Add the ID to the data

## ----try bootstrap lasso, warning=FALSE, message=FALSE, results = FALSE, eval=TRUE----
set.seed(123)
bs1 <- bootstrap(data=trade3, dep="export", cluster_id="clus", fixed=list(c("exp", "time"), c("imp", "time"), c("exp", "imp")), indep=7:22, bootreps=10, colcheck_x = TRUE, colcheck_x_fes = TRUE, boot_threshold = 0.01, post=TRUE, gamma_val=0.01, verbose=FALSE)

## ----bss table, echo = FALSE, eval=TRUE---------------------------------------
knitr::kable(bs1$betas, 
             format = "pipe",
             caption = "Table 8: Coefficients of bootstrap repetitions",
             row.names = TRUE,
             digits = 2)

