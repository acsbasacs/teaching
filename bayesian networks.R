require(bnlearn)


data(gaussian.test,package = "bnlearn") ## all numeric
data(clgaussian.test,package = "bnlearn") ## mix of factor and numeric

v_models <- c(
  "pc.stable","gs","iamb","fast.iamb","inter.iamb","iamb.fdr",
  "hc", "tabu",
  "mmhc","rsmax2","h2pc",
  "mmpc","si.hiton.pc","hpc", 
  "chow.liu","aracne"
)

#########################################################
## example with all factor variables

data(hailfinder,package = "bnlearn") ## all factors
list_cv <- list()
for(j in v_models) try({ 
  list_cv[[j]] <- bn.cv(
    data = hailfinder,
    bn = j,
    k = 2,
    runs = 2
  )
},silent = TRUE)
list_cv
list_mean <- list()
for(j in names(list_cv)){
  for(k in 1:length(list_cv[[j]])){
    list_mean[[j]][[k]] <- rep(NA,length(list_cv[[j]][[k]]))
    for(l in 1:length(list_cv[[j]][[k]])){
      list_mean[[j]][[k]][l] <- list_cv[[j]][[k]][[l]]$loss
    }
  }
  list_mean[[j]] <- unlist(list_mean[[j]])
}
sort(base::sapply(X = list_mean,FUN = mean))

model_hailfinder <- hc(hailfinder)
graphviz.plot(model_hailfinder)
bn_hailfinder <- bn.fit(
  x = model_hailfinder,
  data = hailfinder
)
graphviz.chart(
  x = bn_hailfinder,
  type = "barchart"
)
graphviz.chart(
  x = bn_hailfinder,
  type = "dotplot"
)
graphviz.chart(
  x = bn_hailfinder,
  type = "barprob"
)
arc.strength_hailfinder <- arc.strength(
  x = model_hailfinder,
  data = hailfinder
)
strength.plot(
  x = model_hailfinder,
  strength = arc.strength_hailfinder
)


#########################################################
## example with all numeric variables

data(gaussian.test,package = "bnlearn") ## all factors
list_cv <- list()
for(j in v_models) try({ 
  list_cv[[j]] <- bn.cv(
    data = gaussian.test,
    bn = j,
    k = 2,
    runs = 2
  )
},silent = TRUE)
list_cv
list_mean <- list()
for(j in names(list_cv)){
  for(k in 1:length(list_cv[[j]])){
    list_mean[[j]][[k]] <- rep(NA,length(list_cv[[j]][[k]]))
    for(l in 1:length(list_cv[[j]][[k]])){
      list_mean[[j]][[k]][l] <- list_cv[[j]][[k]][[l]]$loss
    }
  }
  list_mean[[j]] <- unlist(list_mean[[j]])
}
sort(base::sapply(X = list_mean,FUN = mean))

model_gaussian.test <- rsmax2(gaussian.test)
graphviz.plot(model_gaussian.test)
bn_gaussian.test <- bn.fit(
  x = model_gaussian.test,
  data = gaussian.test
)
arc.strength_gaussian.test <- arc.strength(
  x = model_gaussian.test,
  data = gaussian.test
)
strength.plot(
  x = model_gaussian.test,
  arc.strength_gaussian.test
)

#########################################################
## example with mixed variables

data(clgaussian.test,package = "bnlearn") ## all factors
list_cv <- list()
for(j in v_models) try({ 
  list_cv[[j]] <- bn.cv(
    data = clgaussian.test,
    bn = j,
    k = 2,
    runs = 2
  )
},silent = TRUE)
list_cv
list_mean <- list()
for(j in names(list_cv)){
  for(k in 1:length(list_cv[[j]])){
    list_mean[[j]][[k]] <- rep(NA,length(list_cv[[j]][[k]]))
    for(l in 1:length(list_cv[[j]][[k]])){
      list_mean[[j]][[k]][l] <- list_cv[[j]][[k]][[l]]$loss
    }
  }
  list_mean[[j]] <- unlist(list_mean[[j]])
}
sort(base::sapply(X = list_mean,FUN = mean))

model_clgaussian.test <- tabu(clgaussian.test)
graphviz.plot(model_clgaussian.test)
bn_clgaussian.test <- bn.fit(
  x = model_clgaussian.test,
  data = clgaussian.test
)
arc.strength_clgaussian.test <- arc.strength(
  x = model_clgaussian.test,
  data = clgaussian.test
)
strength.plot(
  x = model_clgaussian.test,
  strength = arc.strength_clgaussian.test
)
