% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.crs.R
\name{plot.crs}
\alias{plot.crs}
\title{Plot \code{crs} objects}
\usage{
\method{plot}{crs}(x, H = NULL, cls.rs.vec = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{crs}.}

\item{H}{An integer indicating the number of response-style-based clusters to display the correction result. If \code{H=NULL} and \code{cls.rs.vec=NULL}, \code{H} is set as \code{H=n}. If \code{H=NULL} but \code{cls.rs.vec!=NULL}, \code{H} is set as \code{H=max(cls.rs.vec)}. The default is \code{H=NULL}.}

\item{cls.rs.vec}{An integer vector of length n indicating response-style-based clusters for n respondents. If \code{cls.rs.vec=NULL} and \code{H!=NULL}, clusters are determined by k-means clustering of Beta. The default is \code{cls.rs.vec=NULL}.}

\item{\dots}{Additional arguments passed to \code{\link{plot}}.}
}
\description{
Plots results of correction (1st plot: estimated response functions, 2nd plot: coefficient plot. See Appendix A of the reference paper for the 2nd plot).
}
\details{
Correction results for each respondent are displayed. If either response-style-based clusters or the number of response-style-based clusters are specified, the correction results of response-style-based clusters are displayed.
}
\examples{
###data setting
n <- 30 ; m <- 10 ; H.true <- 2 ; K.true <- 2 ; q <- 5
datagene <- generate.rsdata(n=n,m=m,K.true=K.true,H.true=H.true,q=q,clustered.rs = TRUE)
###obtain n x m data matrix
X <- datagene$X
ccrsdata.list <- create.ccrsdata(X,q=q)
crs.list <- correct.rs(ccrsdata.list)
###You can check correction result using this \\code{crs.plot} function.
plot(crs.list)

#####You can also check correction result obtained
#####by a simultaneous analysis of correction and content-based clustering.
###CCRS
lam <- 0.8 ; K <- 2
ccrs.list <- ccrs(ccrsdata.list,K=K,lam=lam)
###check correction result using this \\code{crs.plot} function.
plot(ccrs.list$crs.list)
}
\references{
Takagishi, M., Velden, M. van de & Yadohisa, H. (2019). Clustering preference data in the presence of response style bias, to appear in British Journal of Mathematical and Statistical Psychology.
}
\seealso{
\code{\link{ccrs}}
}
\keyword{utility}
