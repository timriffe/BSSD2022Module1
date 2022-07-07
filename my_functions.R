

mx_to_qx <- function(mx, ax){
  qx <- mx / (1 + (1 - ax) * mx)
  return(qx)
}

qx_to_px <- function(qx){
  px <- 1 - qx
  return(px)
}

px_to_lx <- function(px, radix = 1){
  n <- length(px)
  lx <- c(1,cumprod(px))
  lx <- lx[1:n]
  return(radix * lx)
}

qx_to_dx <- function(qx, radix = 1){
  px <- qx_to_px(qx)
  lx <- px_to_lx(px = px, radix = radix)
  dx <- qx * lx
  return(dx)
}

qxlx_to_dx <- function(qx, lx){
  dx <- qx * lx
  return(dx)
}

lxdx_to_Lx <- function(lx, dx, ax){
  Lx <- lx - (1 - ax) * dx
  return(Lx)
}

Lx_to_Tx <- function(Lx){
  Tx <- Lx %>% 
    rev() %>% 
    cumsum() %>% 
    rev()
  return(Tx)
}

Txlx_to_ex <- function(Tx, lx){
  ex <- Tx / lx
  return(ex)
}
