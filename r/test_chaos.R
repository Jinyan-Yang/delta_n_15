set.seed(34)
# Simulates time-series data from the Logistic map with chaos
ts        <- DChaos::logistic.sim(n=1000, a=4)
show(head(ts, 5))

## Provides the Lyapunov exponent spectrum by the QR decomposition procedure considering the
## bootstrap blocking method directly from the Logistic map with chaos simulated.
exponent <- DChaos::lyapunov(ts, m=3:3, lag=1:1, timelapse="FIXED", h=2:10, w0maxit=100,
                    wtsmaxit=1e6, pre.white=TRUE, lyapmethod="SLE", blocking="ALL",
                    B=100, trace=1, seed.t=TRUE, seed=56666459, doplot=FALSE)
summary(exponent)
