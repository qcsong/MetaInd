# MetaInd: Meta-Analysis Package for Industrial-Organizational Psychology (based on Schmidt & Hunter, 2014; artifacts corrected individually)
# Developer: Q. Chelsea Song
# Contact: qianqisong@gmail.com
# Last Update: 03/20/2018

##### MetaSummary #####

#' MetaSummary
#'
#' Summary table for meta-analysis results (artifacts corrected individually)
#' @param x Meta-analytic data
#' @param correct_Rxx If TRUE, reliability of predictor (indepedent variable) will be corrected
#' @param correct_Ryy If TRUE, reliability of criterion (dependent variable) will be corrected
#' @param correct_RR If TRUE, range restriction will be corrected (see Schmidt, Hunter, Le, 2000). Note that reliability of dependent variable will be corrected during the process.
#' @param direct If TRUE, direct range restriction will be corrected. If FALSE, indirect range restriction will be corrected.
#' @import psychometric
#' @return Summary table for meta-analysis results (artifacts corrected indiviudally)
#' @export
MetaSummary = function (x, correct_Rxx = TRUE, correct_Ryy = TRUE, correct_RR = TRUE, direct = TRUE)
{
  x <- as.data.frame(x)
  x <- x[rowSums(is.na(x))!=dim(x)[2], ] # remove all NA rows

  n <- sum(aggregate(x, by = list(x$study), FUN = mean, na.rm = T)[,'n'], na.rm = T)
  k <- length(unique(x[,'study']))

  ## obtain sample-weighted result ##
  x_b <- aggregate(x, by = list(x$study), FUN = mean, na.rm = T)
  x_rb <- psychometric::rbar(x_b)
  x_vr <- psychometric::varr(x_b)
  x_ve <- psychometric::vare(x_b)
  x_pv <- pvse2(x_b)[1]
  x_lCIhet <- psychometric::CIrb(x_b, LEVEL = 0.95, homogenous = F)[1]
  x_uCIhet <- psychometric::CIrb(x_b, LEVEL = 0.95, homogenous = F)[2]

  ## obtain corrected result ##
  x_c = x

  if(correct_RR==TRUE){ # Ryy: dependent variable reliability is corrected
    x_c = cRRn(x_c, direct = direct, correct_Rxx = correct_Rxx)
  }else{
    if(correct_Rxx==TRUE){x_c = cRxx(x_c)}
    if(correct_Ryy==TRUE){x_c = cRyy(x_c)}
  }
  
  x_c <- aggregate(x_c, by = list(x_c$study), FUN = mean, na.rm = T)
  rho_rb <- psychometric::rbar(x_c)
  rho_vr <- psychometric::vare(x_c) # Var(rho) in Schmidt & Hunter 2014 (p.149)
  # rho_pv <- pvse2(x_c)[1] # percent of variance due to sampling error
  # rho_lCIhet <- psychometric::CIrb(x_c, LEVEL = .95, homogenous = F)[1]
  # rho_uCIhet <- psychometric::CIrb(x_c, LEVEL = .95, homogenous = F)[2]
  # rho_ve <- psychometric::varr(x_c) # Ave(ve) in Schmidt & Hunter 2014 (p.149)
  rho_ve <- (sqrt(rho_vr)/sqrt(k))^2

  # estimate confidence interval
  # Schmidt & Hunter (2015) p. 230
  rho_stdr <- ((rho_rb/x_rb)*(sqrt(x_vr)))/(sqrt(k))
  level = 0.95
  zs <- -qnorm((1 - level)/2)
  rho_lCI <- rho_rb - zs * rho_stdr
  rho_uCI <- rho_rb + zs * rho_stdr

  # estimate credibility interval
  rho_stdr <- sqrt(rho_vr)
  level = 0.80
  zs <- -qnorm((1 - level)/2)
  rho_lCV <- rho_rb - zs * rho_stdr
  rho_uCV <- rho_rb + zs * rho_stdr

  out <- data.frame(n = n, k = k,
                    rbar = x_rb, Var.rbar = x_vr, VarSE.rbar = x_ve, 
                    LCL95.rbar = x_lCIhet, UCL95.rbar = x_uCIhet,
                    rho = rho_rb, Var.rho = rho_vr,
                    # PerVarExp.rho = rho_pv,
                    LCI95.rho = rho_lCI, UCL95.rho = rho_uCI,
                    LCV80 = rho_lCV, UCV80 = rho_uCV,
                    PerVarExp = x_pv)
  return(out)
}

##### cRxx #####

#' cRxx
#'
#' Description: Conduct reliability correction for predictor (independent variable) for each effect size (i.e., each row)
#' @param x Meta-analytic data
#' @return Meta-analytic data corrected for independent variable reliability
#' @export
cRxx <- function (x)
{
    Rxx <- x$Rxx
    n <- length(Rxx[!(is.na(Rxx))])
    if (n == 0) {
        aRxx <- 1
    }
    else {
        Rxx[is.na(Rxx)] = mean(Rxx, na.rm=T)
        aRxx <- sqrt(Rxx)
    }
    cRxy <- x[, "Rxy"]/aRxx
    out <- x
    out[, "Rxy"] <- cRxy
    return(out)
}

##### cRyy #####

#' cRyy
#'
#' Description: Conduct reliability correction for criterion (dependent variable) for each effect size (i.e., each row)
#' @param x Meta-analytic data
#' @return Meta-analytic data corrected for dependent variable reliability
#' @export
#
cRyy <- function (x)
{
  Ryy <- x$Ryy
  n <- length(Ryy[!(is.na(Ryy))])
  if (n == 0) {
    aRyy <- 1
  }
  else {
    Ryy[is.na(Ryy)] = mean(Ryy,na.rm=T)
    aRyy <- sqrt(Ryy)
  }
  cRxy <- x[,'Rxy']/aRyy
  out <- x
  out[,'Rxy'] <- cRxy
  return(out)
}

##### cRRn #####

#' cRRn
#'
#' Conduct correction for range restriction for each effect size (i.e., each row)
#' based on: Hunter, Schmidt & Le (2000)
#' Options
#' 1) "direct = TRUE": direct/indirect range restriction
#' 2) "correct_Rxx = TRUE": correct/not correct for independent variable reliability
#' Note that Ryy: dependent variable reliability is corrected
#' @param x Meta-analytic data
#' @param correct_Rxx If TRUE, reliability of predictor (indepedent variable) will be corrected
#' @param direct If TRUE, direct range restriction will be corrected. If FALSE, indirect range restriction will be corrected.
#' @return Meta-analytic data corrected for range restriction (and dependent variable)
#' @export
cRRn <- function(x, correct_Rxx = TRUE, direct = TRUE)
{
  # direct range restriction
  if(direct == TRUE){

    n <- length(x$u[!(is.na(x$u))])
    u <- x$u

    # 1. Purpose: Correct for measurement error in Y
    # correlation between X and P in the restricted population: r_XPi
    Rxy = x[,'Rxy']
    Ryy = x[,'Ryy']
    if(length(is.na(Ryy))==0){
      Ryy=1
    }else{
      Ryy[is.na(Ryy)] = mean(Ryy, na.rm=T)
    }
    r_XPi = Rxy/Ryy

    # 2. Purpose: Correct for the effect of direct range restriction on X
    if (n == 0) {
      aRR <- 1
    }
    else {
      # attenuation factor for direct range restriction
      u[is.na(u)] = mean(u,na.rm=T)
      aRR <- sqrt((1 - u^2) * r_XPi^2 + u^2)
    }
    # correlation between T and P in the unrestricted population: r_XPa
    r_XPa <- r_XPi/aRR

    # whether or not to correct for reliability in independent variable
    if(correct_Rxx==TRUE){
      # 3. Correct for measurement error in X
      # reliability of X in the unrestricted population: r_XXa
      Rxx = x[,'Rxx']
      if(length(is.na(Rxx))==0){
        Rxx = 1
      }else{
        Rxx[is.na(Rxx)] = mean(Rxx, na.rm=T)
      }
      r_XXa <- 1 - u^2*(1-Rxx)
      # correlation between T and P in the unrestricted population: cRxy
      cRxy <- r_XPa/sqrt(r_XXa) # operational validity of measure X
    }else{
      cRxy = r_XPa
    }

  }

  # indirect range restriction
  if(direct == FALSE){

    n <- length(x$u[!(is.na(x$u))])
    Rxy = x[,'Rxy']
    Rxx = x[,'Rxx']
    Ryy = x[,'Ryy']
    if (n == 0) {
      cRxy = Rxy
    }
    else {
      u <- x$u
      u[is.na(u)] = mean(u,na.rm=T)

      # 1. Purpose: Correct for measurement error in Y
      # correlation between X and P in the restricted population: r_XPi
      if(length(is.na(Ryy))==0){
        Ryy = 1
      }else{
        Ryy[is.na(Ryy)] = mean(Ryy, na.rm=T)
      }
      r_XPi = Rxy/Ryy

      # 2. Purpose: Obtain reliability of X in the restricted population
      # reliability of X in the restricted population: r_XXi
      if(length(is.na(Rxx))==0){
        Rxx = 1
      }else{
        Rxx[is.na(Rxx)] = mean(Rxx, na.rm=T)
      }
      r_XXi = Rxx

      # 3. Purpose: Correct for measurement error in X
      # correlation between T and P in the restricted population: r_TPi
      r_TPi = r_XPi/sqrt(r_XXi)

      # 4. Purpose: Estimate reliability of X in the unrestricted population: r_XXa
      # reliability of X in the unrestricted population: r_XXa
      r_XXa = 1 - u^2*(1-x[,'Rxx'])

      # 5. Estimate range restriction on T: u_T
      # range restriction on T: u_T
      u_T = sqrt((u^2-(1-r_XXa))/r_XXa)

      # 6. Correct for the effect of indirect range restriction
      # attenuation factor for indirect range restriction
      aRR <- sqrt((1 - u_T^2) * r_XP^2 + u_T^2)
      # correlation between T and P in the unrestricted population: r_TPa
      r_TPa = r_TPi/aRR

      # whether or not to correct for reliability in independent variable
      if(correct_Rxx==TRUE){
        cRxy = r_TPa
      }else{
        # 7. Reintroduce measurement error in T to estimate the operational validity of X: r_XPa
        # correlation between X and P in the unrestricted population: cRxy
        cRxy = r_TPa*sqrt(r_XXa) # operational validity of measure X
      }

    }

  }

  # output
  out <- x
  out[,'Rxy'] <- cRxy
  return(out)

}

##### pvse2 #####

#' pvse2
#'
#' Percent variance explained by sampling error
#' @param x Meta-analytic data
#' @import psychometric
#' @return Percent variance explained by sampling error
#' @export
pvse2 <- function (x)
{
  ve <- psychometric::vare(x)
  vr <- psychometric::varr(x)
  pv <- ve/vr * 100
  if(pv > 100){pv = 100}
  out <- matrix(pv)
  colnames(out) <- "Compare to > 75%"
  return(out)
}

##### CredInt #####

#' CredInt
#'
#' Estimates credibility interval
#' @param x Meta-analytic data
#' @param level alpha of credibility interval
#' @import psychometric
#' @return Upper and lower boundaries of the credibility interval
#' @export
CredInt <- function (x, level = 0.80)
{
  r <- psychometric::rbar(x)
  vr <- psychometric::varResT(x, aprox = FALSE) # true residual variance in correlations
  sdr <- sqrt(vr)
  zs <- -qnorm((1 - level)/2)
  lcl <- r - zs * sdr
  ucl <- r + zs * sdr
  return(list(lcl, ucl))
}


