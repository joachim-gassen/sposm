desc_table <- function(df, funcs = c(n = function(x) sum(is.finite(x)), 
                                     mean = mean, sd = sd, min = min, 
                                     q25 = function(x) quantile(x, 0.25), 
                                     median = median, 
                                     q75 = function(x) quantile(x, 0.75), 
                                     max = max)) {
  if (is.null(df) || !is.data.frame(df)) stop("df needs to be a data frame")
  if (nrow(df) < 1) stop("df needs to be a non-empty data frame")
  if (sum(sapply(df, is.numeric)) + sum(sapply(df, is.logical)) == 0)
    stop("df needs to contain at least one numeric or logical variable")
  
  if(!is.vector(funcs)) 
    stop(paste("funcs needs to be a vector of functions (wrap your function",
               "in c() if you only have one function)"))
  if(!all(sapply(funcs, is.function)))
    stop("funcs contains a member which is not a function")
  
  vars <- which(sapply(df, is.numeric) | sapply(df, is.logical))
  sdf <- as.data.frame(df[, vars])
  names(sdf) <- names(df)[vars] 
  rv <- as.data.frame(matrix(NA, nrow = ncol(sdf), ncol = length(funcs)))
  for (c in 1:length(funcs)) {
    rv[, c] <- sapply(sdf, function(x) {x <- x[!is.na(x)]; funcs[[c]](x)})
  }
  if (!is.null(names(funcs)) && 
      length(unique(make.names(names(funcs)))) == length(funcs)) 
    names(rv) <- make.names(names(funcs))
  rownames(rv) <- names(sdf)
  return(rv)
}
