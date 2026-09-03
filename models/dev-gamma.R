#### Testing the gamma priors
# beta_m = a/(a+b)
# var = a*b/((a+b)^2*(a+b+1))



gamma_mv <- function(mu,sd){
  shape = mu^2 / sd^2
  scale = sd^2/mu
  rate = 1/scale
  # return(c("shape" = round(shape, 2), "scale" =round(scale, 2), "rate" = round(rate, 2)))
  return(c("shape" = shape, "scale" =scale, "rate" = rate))
}


sd_from_ci <- function(min, max, n=10){
  sd1 <- (max-min) / (2*1.96)
  sd2 <- (max-min)/2 * (sqrt(n) / 1.96)
  # return(c("sd1" = sd1, "sd2" = sd2))
  return(sd1)
}

gg <- gamma_mv(mean(c(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
                      -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1])), 
               sd_from_ci(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
                          -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1]))
plotgamma <- function(gg){
  plot(density(rgamma(10000,gg[1],gg[3])))}


g_pr <- function(min, max,time="year"){
  if(time !="year"){
    x <- gamma_mv(mean(c(min,max)), sd_from_ci(min,max))
  } else {
    x <- gamma_mv(mean(c(y2q(min),y2q(max))), sd_from_ci(y2q(min),y2q(max)))
  }
  print(plot(density(rgamma(10000,x[1],x[3]))))
  return(x)
}