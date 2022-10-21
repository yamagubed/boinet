
#' Print tite.boinet
#'
#' tite.boinet results are printed.
#' @param x Object from tite.boinet
#' @param ... Other parameters
#' @rdname print.tite.boinet
#' @export

print.tite.boinet <- function(x,...)
{
  ptab1 <- rbind(
    x$toxprob,
    x$effprob,
    x$n.patient,
    x$prop.select)
  rnames <- c("Toxicity prob.","Efficacy prob.","No. Pts treated","Select %")
  dimnames(ptab1) <- list(rnames,names(x$toxprob))

  ptab2 <- rbind(
    x$prop.stop,
    x$duration)
  rnames <- c(names(x$prop.stop),names(x$duration))
  dimnames(ptab2) <- list(rnames,"")

  ptab3 <- rbind(
    x$phi,
    x$delta,
    x$tau.T,
    x$tau.E,
    x$accrual)
  rnames <- c(names(x$phi),names(x$delta),names(x$tau.T),names(x$tau.E),names(x$accrual))
  dimnames(ptab3) <- list(rnames,"")

  cat("\n")
  cat("Simulation results:\n")
  cat("\n")
  print.default(ptab1,print.gap=2L,quote=FALSE)
  print.default(ptab2,print.gap=2L,quote=FALSE)
  cat("\n")
  cat("Trial design settings:\n")
  print.default(ptab3,print.gap=2L,quote=FALSE)
  cat("\n")
  cat("Efficacy prob. estimation:",x$estpt.method)
  cat("\n")
  cat("\n")
  cat("OBD selection:",x$obd.method)
  cat("\n")
}
