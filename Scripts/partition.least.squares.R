partition.least.squares.cv <- function(partitions, labels, n.k=15, n.repeats = 20){

  library(MLmetrics)
  n.subjects = dim(partitions$mass)[1]
  n.partitions = dim(partitions$mass)[3]

  n.p2 = n.partitions^2

  #leasts quares with eqaulity constraints
  A <- matrix( as.vector(partitions$cost), nrow=42^2, ncol=n.p2)
  x <- cmdscale(partitions$distances, k=n.k)


  res = list( solution = rep(0, n.p2) )
  
  n.repeats <- 100
  glmCV <- rep(NA, n.repeats)
  sols <- list()
  glms <- c()

  for(k in 1:n.repeats){
    perm <-sample(1:n.subjects)
    index.train = perm[1:28]
    index.test = perm[29:42]
  



    data1 = data.frame( x[index.train, ], gender = as.factor(labels[index.train,2]) )
    levels( data1$gender ) <- c("F", "M", NA)
    glm.model <- glm(gender ~ ., data=data1, family=binomial() )
    glms <- rbind(glms, glm.model$coefficients)
    glm.dir   <- glm.model$coefficients[-1]
    glm.dir   <- glm.dir / sum(glm.dir^2)
    x.proj <- x %*% glm.dir
    B <- as.vector( as.matrix(dist(x.proj))^2 )

    #B <- as.vector( as.matrix( dist(as.integer( labels[index.train,2] ) ))^2 )

    #least squares
    f.eval <- function(x){
      y = A %*% x
      objective = sum( (y - B)^2 )

      gradient <- t(A) %*% ( 2*(y-B) )
  
      list(objective=objective, gradient=gradient)
    }



    library(nloptr)

    opts <- list("algorithm"="NLOPT_LD_MMA", 
             "xtol_rel"=1.0e-5,
             "xtol_abs"=rep(1e-6, n.p2),
             "print_level"=1, 
             "maxeval" = 200
             )
    res <- nloptr( x0 = res$solution, 
                 eval_f = f.eval,
                 lb = rep(0, n.p2),
                 ub = rep(1, n.p2),
                 opts = opts
                )

    sol <- matrix(res$solution, n.partitions)
    sols[[k]] <- sol
    dists.proj <- A %*% res$solution

    mds2 <- cmdscale( matrix(dists.proj, n.subjects), eig=TRUE, k=41)
    data.train = data.frame( x = mds2$points[index.train,1], 
                             gender = as.factor(labels[index.train,2]) )
    data.test  = data.frame( x = mds2$points[index.test,1] , 
                             gender = as.factor(labels[index.test,2]) )
    colnames(data.test) <- colnames(data.train) 
    levels( data.train$gender ) <- c("F", "M", NA)
    levels( data.test$gender ) <- c("F", "M", NA)

    glm.model <- glm(gender ~ ., data=data.train, family=binomial() )
    p.test <- predict(glm.model, newdata=data.test, type="response")
    p.test = factor(p.test > 0.5 )
    levels(p.test) <- c("F", "M")

    glmCV[k] <- Accuracy(p.test, data.test$gender)

  }

  list(glmCV = glmCV, sols = sols)
}





partition.least.squares<- function(partitions, labels, n.k=15){

  n.subjects = dim(partitions$mass)[1]
  n.partitions = dim(partitions$mass)[3]

  n.p2 = n.partitions^2

  #leasts quares with eqaulity constraints
  A <- matrix( as.vector(partitions$cost), nrow=42^2, ncol=n.p2)
  x <- cmdscale(partitions$distances, k=n.k)



  data1 = data.frame( x, gender = as.factor(labels[,2]) )
  levels( data1$gender ) <- c("F", "M", NA)
  glm.model <- glm(gender ~ ., data=data1, family=binomial() )
  glm.dir   <- glm.model$coefficients[-1]
  glm.dir   <- glm.dir / sum(glm.dir^2)
  x.proj <- x %*% glm.dir
  B <- as.vector( as.matrix(dist(x.proj))^2 )

  #least squares
  f.eval <- function(x){
    y = A %*% x
    objective = sum( (y - B)^2 )

    gradient <- t(A) %*% ( 2*(y-B) )
  
    list(objective=objective, gradient=gradient)
  }



  library(nloptr)

  opts <- list("algorithm"="NLOPT_LD_MMA", 
             "xtol_rel"=1.0e-5,
             "xtol_abs"=rep(1e-6, n.p2),
             "print_level"=1, 
             "maxeval" = 200
             )
  res <- nloptr( x0 = rep(0, n.p2), 
                 eval_f = f.eval,
                 lb = rep(0, n.p2),
                 ub = rep(1, n.p2),
                 opts = opts
                )

  sol <- matrix(res$solution, n.partitions)
  dists.proj <- A %*% res$solution
 
  list(sol = sol, dists.proj = dists.proj)
}





distance.fraction <- function(sol, dists.proj, A){
  library(data.table)
  sig.dist <- c()
  i.rm <- which(sol !=0, arr.ind=TRUE)
  for( i in 1:nrow(i.rm) ){
    if(i.rm[i,1] < i.rm[i, 2]){
    
      tmp <- matrix(0, nrow(sol), ncol(sol) )  
      tmp[i.rm[i,1], i.rm[i, 2] ] = sol[i.rm[i,1], i.rm[i, 2] ]
      tmp[i.rm[i,2], i.rm[i, 1] ] = sol[i.rm[i,2], i.rm[i, 1] ]
      dists.tmp <- A %*% as.vector(tmp)
      sig.dist = rbind( sig.dist, c( mean(dists.tmp / dists.proj, na.rm=T), i.rm[i,]) )
    }
  }
  sig.dist <- as.data.table(sig.dist)
  colnames(sig.dist) <- c("fraction", "index1", "index2")
  sig.dist[order(fraction, decreasing=T), ]
}


distance.fraction.cor <- function(sol, dists.dir, A){
  library(data.table)
  sig.dist <- c()
  i.rm <- which(sol !=0, arr.ind=TRUE)
  for( i in 1:nrow(i.rm) ){
    if(i.rm[i,1] < i.rm[i, 2]){
    
      tmp <- matrix(0, nrow(sol), ncol(sol) )  
      tmp[i.rm[i,1], i.rm[i, 2] ] = sol[i.rm[i,1], i.rm[i, 2] ]
      tmp[i.rm[i,2], i.rm[i, 1] ] = sol[i.rm[i,2], i.rm[i, 1] ]
      dists.tmp <- A %*% as.vector(tmp)
      sig.dist = rbind(sig.dist, c(cor(dists.tmp, dists.dir), i.rm[i,]) )
    }
  }
  sig.dist <- as.data.table(sig.dist)
  colnames(sig.dist) <- c("cor", "index1", "index2")
  sig.dist[order(cor, decreasing=T), ]
}
