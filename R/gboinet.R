
#' Simulation study of gBOIN-ET design
#'
#' Simulation study is conducted for gBOIN-ET design.
#' @param n.dose Number of dose (default: 6)
#' @param start.dose Starting dose (default: 1)
#' @param size.cohort Cohort size (default: 3)
#' @param n.cohort Number of cohort (default: 12)
#' @param toxprob True toxicity probability
#' @param effprob True efficacy probability
#' @param sev.weight Weight for toxicity category (default: c(0.00,0.50,1.00,1.50))
#' @param res.weight Weight for efficacy category (default: c(0.00,0.25,1.00,3.00))
#' @param phi Target toxicity probability (default: 0.33)
#' @param phi1 Lower bound of toxicity probability (default: 0.033)
#' @param phi2 Upper bound of toxicity probability (default: 0.462)
#' @param delta Target efficacy probability (default: 0.70)
#' @param delta1 Lower bound of efficacy probability (default: 0.42)
#' @param alpha.T1 Probability that toxicity event occurs in the late half of assessment window (default: 0.5)
#' @param alpha.E1 Probability that efficacy event occurs in the late half of assessment window (default: 0.5)
#' @param tau.T Assessment period for toxicity (days) (default: 30)
#' @param tau.E Assessment period for efficacy (days) (default: 45)
#' @param accrual Accrual rate (days) (default: 10)
#' @param stopping.prob.T Stopping probability for toxicity (default: 0.95)
#' @param stopping.prob.E Stopping probability for efficacy (default: 0.95)
#' @param estpt.method Methods to estimate efficacy probability
#' @param obd.method Methods to select OBD
#' @param w1 Weight for toxicity-efficacy trade-off (default: 0.33)
#' @param w2 Weight for penalty imposed on toxic doses (default: 1.09)
#' @param psi00 Score for toxicity=no and efficacy=no (default: 40)
#' @param psi11 Score for toxicity=yes and efficacy=yes (default: 60)
#' @param n.sim Number of simulated trial (default: 1000)
#' @param seed.sim Seed for random number generator (default: 66)
#' @return Summary of simulation study results
#' @examples
#' gboinet(estpt.method="obs.prob",obd.method="max.effprob");
#' @import Iso
#' @importFrom stats binomial dbinom pbeta pbinom rmultinom runif rweibull
#' @export

gboinet <- function(
            n.dose          = 6,
            start.dose      = 1,
            size.cohort     = 3,
            n.cohort        = 12,
            toxprob         = rbind(c(0.94,0.87,0.79,0.68,0.62,0.50),
                                    c(0.05,0.10,0.15,0.20,0.20,0.20),
                                    c(0.01,0.03,0.05,0.10,0.15,0.25),
                                    c(0.00,0.00,0.01,0.02,0.03,0.05)),
            effprob         = rbind(c(0.64,0.52,0.45,0.35,0.20,0.05),
                                    c(0.30,0.40,0.40,0.40,0.40,0.15),
                                    c(0.05,0.05,0.10,0.15,0.20,0.35),
                                    c(0.01,0.03,0.05,0.10,0.20,0.45)),
            sev.weight      = c(0.00,0.50,1.00,1.50),
            res.weight      = c(0.00,0.25,1.00,3.00),
            phi             = 0.33,
            phi1            = 0.033,
            phi2            = 0.462,
            delta           = 0.70,
            delta1          = 0.42,
            alpha.T1        = 0.5,
            alpha.E1        = 0.5,
            tau.T           = 30,
            tau.E           = 45,
            accrual         = 10,
            stopping.prob.T = 0.95,
            stopping.prob.E = 0.95,
            estpt.method    = c("multi.iso","fp.logistic","obs.prob"),
            obd.method      = c("utility.weighted","utility.truncated.linear","utility.scoring","max.effprob"),
            w1              = 0.33,
            w2              = 1.09,
            psi00           = 40,
            psi11           = 60,
            n.sim           = 1000,
            seed.sim        = 66)
{
  dosen <- 1:n.dose
  dose  <- paste("Dose",dosen,sep="")

  toxp <- data.frame(toxprob)
  colnames(toxp) <- dose

  effp <- data.frame(effprob)
  colnames(effp) <- dose

  ncat.T <- nrow(toxp)-1
  ncat.E <- nrow(effp)-1

  ncoh <- size.cohort
  nesc <- n.cohort
  nmax <- ncoh*nesc

  design.par <- gridoptim(phi=phi,phi1=phi1,phi2=phi2,delta=delta,delta1=delta1)

  lambda1 <- design.par$lambda1
  lambda2 <- design.par$lambda2
  eta1    <- design.par$eta1

  pr.alpha <- 1
  pr.beta  <- 1

  alpha.T1 <- alpha.T1
  alpha.T2 <- 0.5
  alpha.E1 <- alpha.E1
  alpha.E2 <- 0.5

  data.obs.n <- array(0,dim=c(n.sim,n.dose))
  data.dur   <- array(0,dim=c(n.sim))

  obd <- array(0,dim=c(n.sim))

  set.seed(seed.sim)

  for(ss in 1:n.sim){

    efftoxp <- list(toxp=toxp,effp=effp)

    obs.n   <- numeric(n.dose)
    obs.tox <- numeric(n.dose)
    obs.eff <- numeric(n.dose)
    pe      <- numeric(n.dose)
    pt      <- numeric(n.dose)

    t.enter    <- NULL
    t.decision <- 0

    curdose <- start.dose

    for(i in 1:nesc){
      dlab <- paste("Dose",curdose,sep="")
      obs.n[curdose] <- obs.n[curdose] + ncoh

      for(j in 1:ncoh){
        if(j==1){
          t.enter <- c(t.enter,t.decision)
        }else{
          t.enter <- c(t.enter,t.enter[length(t.enter)]+runif(1,0,2*accrual))
      }}
      t.decision <- t.enter[length(t.enter)]+max(tau.T,tau.E)

      psi.T    <- sum(efftoxp$toxp[-1,dlab])
      zetta.T1 <- log(log(1-psi.T)/log(1-psi.T+alpha.T1*psi.T))/log(1/(1-alpha.T2))
      zetta.T2 <- tau.T/(-log(1-psi.T))^(1/zetta.T1)
      time.tox <- rweibull(ncoh,shape=zetta.T1,scale=zetta.T2)
      event.T  <- as.numeric(time.tox<=tau.T)
      grade    <- event.T*((1:ncat.T)%*%rmultinom(ncat.T,1,efftoxp$toxp[-1,dlab]))+1
      ETS      <- apply(grade,2,function(x){return(sev.weight[x])})
      nETS     <- ETS/max(sev.weight)

      psi.E    <- sum(efftoxp$effp[-1,dlab])
      zetta.E1 <- log(log(1-psi.E)/log(1-psi.E+alpha.E1*psi.E))/log(1/(1-alpha.E2))
      zetta.E2 <- tau.E/(-log(1-psi.E))^(1/zetta.E1)
      time.eff <- rweibull(ncoh,shape=zetta.E1,scale=zetta.E2)
      event.E  <- as.numeric(time.eff<=tau.E)
      response <- event.E*((1:ncat.E)%*%rmultinom(ncat.E,1,efftoxp$effp[-1,dlab]))+1
      EES      <- apply(response,2,function(x){return(res.weight[x])})
      nEES     <- EES/max(res.weight)

      obs.tox[curdose] <- obs.tox[curdose]+sum(nETS)
      pt[curdose] <- obs.tox[curdose]/obs.n[curdose]

      obs.eff[curdose] <- obs.eff[curdose]+sum(nEES)
      pe[curdose] <- obs.eff[curdose]/obs.n[curdose]

      if((pt[curdose]<=lambda1)&(pe[curdose]<=eta1)){
        nxtdose <- curdose+1
      }else if((pt[curdose]<lambda2)&(pe[curdose]>eta1)){
        nxtdose <- curdose
      }else if(pt[curdose]>=lambda2){
        nxtdose <- curdose-1

      }else if((pt[curdose]>lambda1)&(pt[curdose]<lambda2)&(pe[curdose]<=eta1)){

        if(curdose==n.dose){
          three   <- c(curdose-1,curdose)
          maxpe   <- max(pe[three])
          nxtdose <- sample(dosen[which((pe==maxpe)&(is.element(dosen,three)))],1)

        }else if(obs.n[curdose+1]==0){
          nxtdose <- curdose+1

        }else if(curdose==1){
          three   <- c(curdose,curdose+1)
          maxpe   <- max(pe[three])
          nxtdose <- sample(dosen[which((pe==maxpe)&(is.element(dosen,three)))],1)

        }else{
          three   <- c(curdose-1,curdose,curdose+1)
          maxpe   <- max(pe[three])
          nxtdose <- sample(dosen[which((pe==maxpe)&(is.element(dosen,three)))],1)
        }
      }

      po.shape1 <- pr.alpha + obs.tox
      po.shape2 <- pr.beta  + (obs.n-obs.tox)
      tterm     <- pbeta(phi,po.shape1,po.shape2)

      po.shape1 <- pr.alpha + obs.eff
      po.shape2 <- pr.beta  + (obs.n-obs.eff)
      eterm     <- 1-pbeta(delta1,po.shape1,po.shape2)

      admflg  <- !((eterm<(1-stopping.prob.E))|(tterm<(1-stopping.prob.T)))
      admdose <- dosen[admflg]

      if(sum(admflg)==0){
        break
      }else{

        if(nxtdose==0){
          if(admflg[1]){
            curdose <- 1
          }else{
            break
          }

        }else if(nxtdose==(n.dose+1)){
          curdose <- n.dose

        }else if(is.element(nxtdose,admdose)){
          curdose <- nxtdose

        }else if(curdose<nxtdose){
          if(sum(admdose>=nxtdose)!=0){
            curdose <- min(admdose[admdose>=nxtdose])
          }

        }else if(curdose>=nxtdose){
          if(sum(admdose<=nxtdose)!=0){
            curdose <- max(admdose[admdose<=nxtdose])
          }else{
            break
          }
      }}
    }

    data.obs.n[ss,] <- obs.n
    data.dur[ss]    <- t.decision

    evadose <- dosen[obs.n!=0]
    obspt <- obs.tox[evadose]/obs.n[evadose]
    obspe <- obs.eff[evadose]/obs.n[evadose]

    tterm.obd <- numeric(n.dose)
    eterm.obd <- numeric(n.dose)

    for(i in evadose){
      po.shape1    <- pr.alpha + obs.tox[i]
      po.shape2    <- pr.beta  + (obs.n[i]-obs.tox[i])
      tterm.obd[i] <- pbeta(phi,po.shape1,po.shape2)

      po.shape1    <- pr.alpha + obs.eff[i]
      po.shape2    <- pr.beta  + (obs.n[i]-obs.eff[i])
      eterm.obd[i] <- 1-pbeta(delta1,po.shape1,po.shape2)
    }

    if(sum(obs.n)<nmax){

      obd[ss] <- 0

    }else if(length(evadose)==1){

      if((tterm.obd[evadose]>=(1-stopping.prob.T))&(eterm.obd[evadose]>=(1-stopping.prob.E))){
        obd[ss] <- evadose
      }

    }else if(sum((tterm.obd[evadose]>=(1-stopping.prob.T))&(eterm.obd[evadose]>=(1-stopping.prob.E)))>=1){

      estpt <- Iso::pava(obspt)

      if(estpt.method=="multi.iso"){
        estpe <- multi.iso(obs=obs.eff[evadose],n=obs.n[evadose])

      }else if(estpt.method=="fp.logistic"){
        estpe <- fp.logit(obs=obs.eff[evadose],n=obs.n[evadose],dose=evadose)

      }else if(estpt.method=="obs.prob"){
        estpe <- obspe
      }

      obd[ss] <- obd.select(
                   probt     = estpt,
                   probe     = estpe,
                   method    = obd.method,
                   phi       = phi,
                   phi1      = phi1,
                   phi2      = phi2,
                   delta     = delta,
                   delta1    = delta1,
                   tterm     = tterm.obd[evadose],
                   eterm     = eterm.obd[evadose],
                   stopT     = stopping.prob.T,
                   stopE     = stopping.prob.E,
                   w1        = w1,
                   w2        = w2,
                   tox.upper = phi2,
                   psi00     = psi00,
                   psi11     = psi11)
    }
  }

  prop.select <- array(0,dim=c(n.dose))
  for(i in 1:n.dose){
    prop.select[i] <- round(mean(obd==i)*100,digits=1)
  }
  names(prop.select) <- dose

  prop.stop <- round(mean(obd==0)*100,digits=1)
  names(prop.stop) <- "Stop %"

  n.patient <- round(apply(data.obs.n,2,mean),digits=1)
  names(n.patient) <- dose

  duration  <- round(mean(data.dur),digits=1)
  names(duration) <- "Trial duration (days)"

  t.nets <- round(apply(apply(toxprob,2,function(x)x*sev.weight/max(sev.weight)),2,sum),digits=2)
  t.nees <- round(apply(apply(effprob,2,function(x)x*res.weight/max(res.weight)),2,sum),digits=2)

  dimnames(toxprob)   <- list(paste("Tox.cat",1:(ncat.T+1),sep=""),dose)
  dimnames(effprob)   <- list(paste("Eff.cat",1:(ncat.E+1),sep=""),dose)
  names(phi)          <- "Target toxicity prob."
  names(delta)        <- "Target efficacy prob."
  names(tau.T)        <- "Tox. assessment window (days)"
  names(tau.E)        <- "Eff. assessment window (days)"
  names(accrual)      <- "Accrual rate (days)"
  names(ncat.T)       <- "Number of toxicity category"
  names(ncat.E)       <- "Number of efficacy category"
  names(estpt.method) <- "Efficacy prob. estimation"
  names(obd.method)   <- "OBD selection"

  result <- list(toxprob      = toxprob,
                 effprob      = effprob,
                 nETS         = t.nets,
                 nEES         = t.nees,
                 phi          = phi,
                 delta        = delta,
                 tau.T        = tau.T,
                 tau.E        = tau.E,
                 accrual      = accrual,
                 ncat.T       = ncat.T+1,
                 ncat.E       = ncat.E+1,
                 estpt.method = estpt.method,
                 obd.method   = obd.method,
                 n.patient    = n.patient,
                 prop.select  = prop.select,
                 prop.stop    = prop.stop,
                 duration     = duration)

  class(result) <- "gboinet"
  result
}



