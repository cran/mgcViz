#'
#' Binning and checking GAM residuals
#' 
#' @description This layer bins the residuals, r, according to the value of the corresponding
#'              covariate, x. Then the residuals in each bin are summarized using a 
#'              scalar-valued statistic. Confidence intervals for the statistic corresponding
#'              to each bin can be obtained by simulating residuals from the fitted GAM
#'              model, binning and summarizing them. Mainly useful in conjuction with [check1D]. 
#' @name l_gridCheck1D
#' @param gridFun scalar-valued function used to summarize the residuals in each bin. 
#'                It takes a vector as input. By default it is 
#'                \code{mean(r)*sqrt(length(r))}, where \code{r} is the vector of 
#'                residuals in that bin.
#' @param n number of grid intervals along the relevant covariate.
#' @param level the level of the confidence intervals (e.g. 0.9 means 90% intervals).
#' @param stand if "none" the residuals in each bin are transformed by \code{gridFun} and
#'              the result statistics are plotted directly. If "sc" the statistics 
#'              in each bin are scaled and centered using the mean and standard
#'              deviation of the simulated stats in that bin. 
#'              If "s" we do only scaling, if "c" only centering.
#' @param showReps if \code{TRUE} the individuals simulated statistics are also plotted using small points.
#' @param showObs if \code{TRUE} the observed statistics are plotted using large points.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return An object of class \code{gamLayer}
#' @examples 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' x <- rnorm(n); y <- rnorm(n);
#' 
#' # Residuals are heteroscedastic w.r.t. x
#' ob <- (x)^2 + (y)^2 + (0.2*abs(x) + 1)  * rnorm(n)
#' b <- bam(ob ~ s(x,k=30) + s(y, k=30), discrete = TRUE)
#' b <- getViz(b, nsim = 50)
#' 
#' # Don't see much by looking at mean
#' check1D(b, "x") + l_gridCheck1D()
#' 
#' # Heteroscedasticity clearly visible here
#' check1D(b, "x") + l_gridCheck1D(gridFun = sd, stand = "sc") # <- we are scaling and centering
#' # Last point on the right of the rug seems to indicate that a bin is missing.
#' # It is not an error, only on observation falls in that bin, hence the
#' # standard deviation is not defined there.
#' 
#' @importFrom matrixStats colSds
#' @importFrom plyr aaply
#' @rdname l_gridCheck1D
#' @export l_gridCheck1D
l_gridCheck1D <- function(gridFun = NULL, n = 20, level = 0.8, stand = "none", showReps = TRUE, showObs = TRUE, ...){
  arg <- list(...)
  
  if( !is.null(arg$show.reps) ){ # Here for compatibility (on old mgcViz version the argument name was show.reps)
    message("Pedantic message from l_gridCheck1D(): argument \"show.reps\" is deprecated, please use \"showReps\".")
    showReps <- arg$show.reps
  }
  
  arg$xtra <- list("gridFun"=gridFun, "n"=n, 
                   "level"=level, "stand"=stand, "showReps"=showReps, "showObs"=showObs)
  o <- structure(list("fun" = "l_gridCheck1D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for numeric covariates
#' @noRd
l_gridCheck1D.Check1DNumeric <- function(a){
  
  a$xtra$class <- "numeric"
  
  .l_gridCheck1D( a )
  
}

######## Internal method for factor covariates 
#' @noRd
l_gridCheck1D.Check1DFactor <- l_gridCheck1D.Check1DLogical <- function(a){
  
  a$xtra$class <- "factor"
  a$data$res$x <- as.factor( a$data$res$x )
  
  .l_gridCheck1D( a )
  
}

######## Internal method 
#' @noRd
.l_gridCheck1D <- function(a){
  
  ### 1. Preparation
  xtra <- a$xtra
  a$xtra <- NULL
  
  x <- a$data$res$x
  y <- as.matrix(a$data$res$y)
  sim <- a$data$sim
  n <- xtra$n
  level <- xtra$level
  cls <- xtra$class 
  dy <- ncol(y)
  
  usr_fun <- xtra$gridFun
  if( is.null(usr_fun) ){
    xtra$gridFun <- function(.x){
      .o <- mean(.x) * sqrt(length(.x))
      return( .o )
    }
  } else {
    if( dy > 1 ){
      xtra$gridFun <- function(.x){
        if( !is.matrix(.x) ){ .x <- matrix(.x, ncol = dy) }
        return( usr_fun(.x) )
      } 
    }
  }
  
  ### 2. Computation on grid
  if(cls == "numeric"){ # Bin observed data
    grid <- seq(min(x), max(x), length.out = n)
    inX <- findInterval(x, grid, rightmost.closed = T)
    grX <- tapply(x, inX, mean)     # Averaging x inside each bin
    grY <- sapply(split.data.frame(y, inX), xtra$gridFun)
  }
  if(cls == "factor"){ # Bin observed data
    grid <- grX <- levels( x )
    grY <- sapply(split.data.frame(y, x), xtra$gridFun)
  }
  
  rep <- 0
  if( is.null(sim) ){ # NO simulations
    if( level > 0 ){
      message("l_gridCheck1D: level>0 but object does not contain any simulations. See ?getViz.")
    } else {
      if( xtra$stand != "none" ){
        message("l_gridCheck1D: stand!=`none` but object does not contain any simulations. See ?getViz.")
      }
    }
  } else {  # YES simulations!
    rep <- length( sim )
    
    # Bin simulated data 
    if(cls == "numeric"){
      inS <- findInterval(x, grid, rightmost.closed = T)
      lev <- sort( unique(inS) ) 
      # lev <- lev[ (lev != 0) & (lev != n) ]   # Discard x's that fall outside grid 
    }
    if(cls == "factor"){
      inS <- x
      lev <- grX 
    }
    
    # Calculate function for each bin and each repetition
    grS <- matrix(NA, rep, length(lev))
    for( ir in 1:rep ){ 
      grS[ir, ] <- sapply(lev, function(.ii) { xtra$gridFun(as.matrix(sim[[ir]])[inS==.ii, , drop = FALSE]) } ) 
    }
    
    # Standardize grS and grY
    if( xtra$stand != "none" ) {
      if(xtra$stand %in% c("c", "sc")){ # Center 
        tmpC <- colMeans(grS)
        tmpC[ is.na(tmpC) ] <- 0
      } else{
        tmpC <- numeric( ncol(grS) )
      }
      if(xtra$stand %in% c("s", "sc")){ # Scale
        tmpS <- colSds(grS)
        tmpS[ is.na(tmpS) ] <- 1 
      } else {
        tmpS <- rep(1, ncol(grS))
      }
      grS <- scale(grS, center = tmpC, scale = tmpS)
      grY <- (grY - tmpC) / tmpS
    }
    
    # Compute confidence intervals
    conf <- NULL
    if( level > 0 ){ 
      conf <- apply(grS, 2, quantile, c(0.5 - level/2, 0.5 + level/2), na.rm = T) 
      good <- !is.na( grY ) # Discard NAs
      goX <- grX[ good ]
      if( level>0 ) { conf <- conf[ , good] }
    }
    
  }
  
  ### 3. Plotting
  a$data <- data.frame("x" = grX, "y" = grY)
  a$mapping <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }
  if( is.null(a$shape) ){ a$shape <- 19 }
  
  # Build layers
  showObs <- xtra$showObs
  out <- list()
  if( showObs ){ out[[1]] <- do.call("geom_point", a) }
  
  if( rep > 0){
    datS <- data.frame("x" = rep(grX, rep), "y" = as.vector(t(grS)))
    if(xtra$showReps) {
      out[[1+showObs]] <- geom_point(data = datS, aes(x = x, y = y), na.rm = TRUE, shape = 46)
    }
    if(level > 0){
      datCI <- data.frame("x" = goX, "ll" = conf[1, ], "ul" = conf[2, ])
      if(cls == "factor"){
        a$colour <- 2
        a$shape <- 3
        a$size <- 2
        out[[2+showObs]] <- geom_point(data = datCI, aes(x = x, y = ll), 
                                       na.rm = TRUE, colour = "red", shape = 3, size = 2, inherit.aes = FALSE)
        out[[3+showObs]] <- geom_point(data = datCI, aes(x = x, y = ul), 
                                       na.rm = TRUE, colour = "red", shape = 3, size = 2, inherit.aes = FALSE)
        
        # out[[2+showObs]] <- geom_errorbar(data = datCI, aes(x = x, ymin = ll, ymax = ul), na.rm = TRUE, colour = "red", inherit.aes = FALSE)
        # out[[3+showObs]] <- geom_errorbar(data = datCI, aes(x = x, ymin = ll, ymax = ul), na.rm = TRUE, colour = "red", inherit.aes = FALSE)
      }else{
        out[[2+showObs]] <- geom_line(data = datCI, aes(x = as.numeric(x), y = ll), na.rm = TRUE, linetype = 2, colour = "red")
        out[[3+showObs]] <- geom_line(data = datCI, aes(x = as.numeric(x), y = ul), na.rm = TRUE, linetype = 2, colour = "red")
      }
    }
  }
  
  class(out) <- "listOfLayers"
  
  return( out )
  
}

