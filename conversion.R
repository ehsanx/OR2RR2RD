OR2RD <- function(OR,p0){
  O0 = p0/(1-p0)
  RD = O0*(OR-1)/( (1+OR*O0)*(1+O0) )
  return(RD)
}

OR2RR <- function(OR,p0){
  O0 = p0/(1-p0)
  RD = O0*(OR-1)/( (1+OR*O0)*(1+O0) )
  RR = (RD+p0)/p0
  return(RR)
}

OR2RRx <- function(OR,p0){
  O0 = p0/(1-p0)
  O1 = OR*O0 
  p1 = O1/(1+O1) 
  RR = p1/p0
  return(RR)
}

OR2RRy <- function(OR,p0){
  RR = OR/(1 - p0 + (OR * p0))
  return(RR)
}