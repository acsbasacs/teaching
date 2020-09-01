data(Groceries,package = "arules")
Groceries

M_Groceries <- as.data.frame(as(
  object = Groceries,
  Class = "matrix"
))
colnames(M_Groceries) <- make.names(colnames(M_Groceries))
M_Groceries <- M_Groceries[
  order(rowSums(M_Groceries),decreasing = TRUE),
  order(colSums(M_Groceries),decreasing = TRUE)
]

v_sums <- colSums(M_Groceries)
mean(cumsum(v_sums)/sum(v_sums) <= 0.8)
sum(cumsum(v_sums)/sum(v_sums) <= 0.8)
v_names <- names(v_sums[cumsum(v_sums)/sum(v_sums) <= 0.8])
M_Groceries <- M_Groceries[,v_names]

## Note: using numeric for 0:1 to improve speed
for(j in 1:ncol(M_Groceries)){
  M_Groceries[,j] <- as.numeric(M_Groceries[,j])
}
for(j in 1:ncol(M_Groceries)){
  M_Groceries[,j] <- factor(M_Groceries[,j])
}

v_algorithms <- c(
  "iamb.fdr","fast.iamb","inter.iamb","gs","iamb","hpc","si.hiton.pc","mmpc","pc.stable",
  "hc","tabu",
  "h2pc","mmhc","rsmax2",
  "aracne","chow.liu"
)
require(bnlearn)
## Not ran, I would run this overnight if I needed the best model possible.
## list_bnlearn <- list()
## for(j in v_algorithms) try({
##   list_bnlearn[[j]] <- do.call(
##     what = j,
##     args = list(x = M_Groceries[,v_names])
##   )
## },silent = TRUE)

list_bnlearn <- list()
for(j in "hc") try({
  list_bnlearn[[j]] <- do.call(
    what = j,
    args = list(x = M_Groceries[,v_names])
  )
},silent = TRUE)

bn_Groceries <- bn.fit(
  x = list_bnlearn[["hc"]],
  data = M_Groceries[,v_names]
)
v_subset <- sort(sample.int(
  n = nrow(M_Groceries),
  size = 10
))
subset_Groceries <- M_Groceries[v_subset,]
predict_Groceries <- matrix(0,nrow = nrow(subset_Groceries),ncol = ncol(subset_Groceries))
rownames(predict_Groceries) <- rownames(subset_Groceries)
colnames(predict_Groceries) <- colnames(subset_Groceries)
v_index <- 1:ncol(subset_Groceries)
for(j in 1:nrow(subset_Groceries)) for(k in v_index[subset_Groceries[j,] == "0"]){
  predict_Groceries[j,k] <- attr(
    x = predict(
      object = bn_Groceries,
      data = subset_Groceries[j,-k],
      node = v_names[k],
      prob = TRUE
    ),
    which = "prob"
  )["1",1]
}

predict_Groceries
list_predict <- list()
for(j in 1:nrow(subset_Groceries)){
  list_predict[[j]] <- list()
  list_predict[[j]]$In_Basket <- v_names[subset_Groceries[j,] == "1"]
  list_predict[[j]]$Promote <- sort(predict_Groceries[j,predict_Groceries[j,] > 0],decreasing = TRUE)
}
list_predict
