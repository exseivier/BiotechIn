# Exponential growth function
growth_fun <- function(P0, Time, Td, erate) {
  # Time is the variable.
  gen = Time/Td
  P = P0 * 2**gen
  if (rbinom(1,1,0.5)) {
    P = P + (P*runif(1, 0, erate))
  } else {
    P = P - (P*runif(1, 0, erate))
  }
  P
}

# Growth simulation function
grow <- function(P0, time_samples, miu, erate) {
  Td = 1/miu
  sapply(time_samples, function(Time) {growth_fun(P0, Time, Td, erate)})
}

monod_fun <- function(ks, miu_max, S) {
  miu <- miu_max * (S/(ks+S))
  miu
}

# Main program ######################################
#Time in expinential growth
tieg <- 10 #hours
# Sample time rate
str <- 0.5
# Time samples
ts <- seq(0, tieg, str)
# Initial Substrate Concentration
S0 <- 0.5 #mM
# Final Substrate concentration
Sf <- 3 #mM
# Substrate concentration change rate
Sr <- 0.2
# Substrate concentrations
S <- seq(S0, Sf, Sr)
#Ks mM
Ks <- 0.95
# miu_max h-1
miu_max <- 0.95
# Error rate
erate <- 0.05
# Initial population
p0 <- 125
# Getting data
#data <- grow(P0=150, time_samples = ts, miu=monod_fun(Ks, miu_max, S))
data <- sapply(S,
               function(each_s) {
                 grow(P0=p0,
                      time_samples = ts,
                      miu = monod_fun(Ks, miu_max, each_s),
                      erate = erate
                      )
                 }
               )

dimnames(data) <- list(as.character(ts), as.character(S))
