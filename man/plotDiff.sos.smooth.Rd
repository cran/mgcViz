% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDiff_sos_smooth.R
\name{plotDiff.sos.smooth}
\alias{plotDiff.sos.smooth}
\title{Plotting differences between two smooths on the sphere}
\usage{
\method{plotDiff}{sos.smooth}(
  s1,
  s2,
  n = 40,
  too.far = 0.1,
  phi = 30,
  theta = 30,
  scheme = 0,
  trans = identity,
  unconditional = FALSE,
  ...
)
}
\arguments{
\item{s1}{a smooth effect object, extracted using \link{sm}.}

\item{s2}{another smooth effect object.}

\item{n}{sqrt of the number of grid points used to compute the effect plot.}

\item{too.far}{if greater than 0 then this is used to determine when a location is too far
from data to be plotted. This is useful since smooths tend to go wild
away from data. The data are scaled into the unit square before deciding
what to exclude, and too.far is a distance within the unit square.
Setting to zero can make plotting faster for large datasets, but care
then needed with interpretation of plots.}

\item{phi}{one of the plotting angles, relevant only if \code{scheme = 0}.}

\item{theta}{the other plotting angle, relevant only if \code{scheme = 0}.}

\item{scheme}{if 0 the smooth effect is plotted on the sphere. If 1 the smooth effect is plotted
on the two hemispheres.}

\item{trans}{monotonic function to apply to the smooth and residuals, before plotting.
Monotonicity is not checked.}

\item{unconditional}{if \code{TRUE} then the smoothing parameter uncertainty corrected covariance
matrix is used to compute uncertainty bands, if available.
Otherwise the bands treat the smoothing parameters as fixed.}

\item{...}{currently unused.}
}
\value{
An objects of class \code{plotSmooth}.
}
\description{
This method can be used to plot the difference between two smooth
effects on the sphere. Mainly meant to be used with by-factor smooths.
}
\details{
Let sd be the difference between the fitted smooths, that is: sd = s1 - s2.
sd is a vector of length n, and its covariance matrix is
Cov(sd) = X1\\%\emph{\\%Sig11\\%}\\%t(X1) + X2\\%\emph{\\%Sig22\\%}\\%t(X2) - X1\\%\emph{\\%Sig12\\%}\\%t(X2) - X2\\%\emph{\\%Sig12\\%}\\%t(X1),
where: X1 (X2) and Sig11 (Sig22) are the design matrix and the covariance matrix
of the coefficients of s1 (s2), while Sig12 is the cross-covariance matrix between
the coefficients of s1 and s2. To get the confidence intervals we need only diag(Cov(sd)),
which here is calculated efficiently (without computing the whole of Cov(sd)).
}
\examples{
\dontrun{
#### 1) Simulate data and add factors uncorrelated to the response
library(mgcViz)
set.seed(0)
n <- 500

f <- function(la,lo) { ## a test function...
  sin(lo)*cos(la-.3)
}

## generate with uniform density on sphere...  
lo <- runif(n)*2*pi-pi ## longitude
la <- runif(3*n)*pi-pi/2
ind <- runif(3*n)<=cos(la)
la <- la[ind];
la <- la[1:n]

ff <- f(la,lo)
y <- ff + rnorm(n)*.2 ## test data

## generate data for plotting truth...
lam <- seq(-pi/2,pi/2,length=30)
lom <- seq(-pi,pi,length=60)
gr <- expand.grid(la=lam,lo=lom)
fz <- f(gr$la,gr$lo)
zm <- matrix(fz,30,60)

dat <- data.frame(la = la *180/pi,lo = lo *180/pi,y=y)
dat$fac <- as.factor( sample(c("A1", "A2", "A3"), nrow(dat), replace = TRUE) ) 

#### 2) fit spline on sphere model...
bp <- gam(y~s(la,lo,bs="sos",k=60, by = fac),data=dat)
bp <- getViz(bp)

# Extract the smooths correspoding to "A1" and "A3" and plot their difference
# Using scheme = 0 
pl0 <- plotDiff(s1 = sm(bp, 1), s2 = sm(bp, 3))
pl0 + l_fitRaster() + l_fitContour() + l_coordContour() + l_bound()

# Plot p-values for significance of differences
pl0 + l_pvRaster() + l_pvContour(breaks=c(0.05, 0.1, 0.2, 0.3, 0.5))

# Using scheme = 1
pl1 <- plotDiff(s1 = sm(bp, 1), s2 = sm(bp, 2), scheme = 1) 
pl1 + l_fitRaster() + l_fitContour()

# Plot p-values for significance of differences
pl1 + l_pvRaster() + l_pvContour(breaks=c(0.05, 0.1, 0.2, 0.3, 0.5))
}
}
\references{
Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for
Generalized Additive Model Components. Scandinavian Journal of Statistics.
}
