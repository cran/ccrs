
#' Correct response-style-biased data
#'
#' @description Corrects response-style-biased data, given \code{ccrsdata.list} created by \code{\link{create.ccrsdata}}.
#' @usage correct.rs(ccrsdata.list)
#' @param ccrsdata.list A list generated by \code{create.ccrsdata}, which contains \code{Fmat}, \code{Mmat.q1}, \code{Mmat.q} and \code{X}.
#' @return Returns an object of \code{crs} with the following elements.
#' \item{\code{Beta}}{An n by q-1 matrix of coefficiets for response functions.}
#' \item{\code{Y.hat}}{An n by m matrix of corrected data matrix.}
#' \item{\code{MB}}{An n by q matrix of values of response functions evaluated at the midpoint between boundaries.}
#' @seealso \code{\link{create.ccrsdata}}
#' @references Takagishi, M., Velden, M. van de & Yadohisa, H. (2019). Clustering preference data in the presence of response style bias, to appear in British Journal of Mathematical and Statistical Psychology.
#' @export
#' @examples
#' ###data setting
#' n <- 30 ; m <- 10 ; H.true <- 2 ; K.true <- 2 ; q <- 5
#' datagene <- generate.rsdata(n=n,m=m,K.true=K.true,H.true=H.true,q=q,clustered.rs = TRUE)
#' ###obtain n x m data matrix
#' X <- datagene$X
#' ccrsdata.list <- create.ccrsdata(X,q=q)
#' crs.list <- correct.rs(ccrsdata.list)



correct.rs <- function(ccrsdata.list){

  Beta <- smooth.bound(ccrsdata.list$Fmat,Mmat.q1=ccrsdata.list$Mmat.q1)
  trans.list <- transformRSdata(ccrsdata.list$X,Beta=Beta,Mmat.q=ccrsdata.list$Mmat.q)

  crs.list <- list(Beta=Beta,Y.hat=trans.list$Y.hat,MB=trans.list$MB)
  class(crs.list) <- "crs"
  crs.list

}


