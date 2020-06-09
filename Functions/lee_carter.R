# Forecasting LFPR using the Lee-Carter Method
# Original R code by Bernardo Queiroz (Modified by Matheus Ferreira)

# Function to estimate the parameters of the model
leecarter = function(lfpr, k){
  log.lfpr = log(lfpr)
  ax <- apply(log.lfpr, 2, mean)
  swept.lfpr = sweep(log.lfpr, 2, ax)
  svd.lfpr = svd(swept.lfpr)
  bx = svd.lfpr$v[ , 1]/sum(svd.lfpr$v[ , 1])
  kt = svd.lfpr$d[1]*svd.lfpr$u[ , 1]*sum(svd.lfpr$v[ , 1])
  kt_diff = diff(kt)
  summary_kt = summary(lm(kt_diff ~ 1))
  kt_drift = summary_kt$coefficients[1,1]
  sec = summary_kt$coefficients[1,2]
  see = summary_kt$sigma
  h = seq(1, k) # k is the length of the forecast
  kt_stderr = ( (h*see^2) + (h*sec)^2 )^.5
  kt_forecast = tail(kt, 1) + (h * kt_drift)
  kt_lo_forecast = kt_forecast - (1.96 * kt_stderr)
  kt_hi_forecast = kt_forecast + (1.96 * kt_stderr)
  result = list(ax = ax, bx = bx, kt = kt, kt_diff = kt_diff, summary_kt = summary_kt, kt_drift = kt_drift, sec = sec, see = see,
                 h = h, kt_stderr = kt_stderr, kt_forecast = kt_forecast, kt_lo_forecast = kt_lo_forecast,
                 kt_hi_forecast = kt_hi_forecast)
  return(result)
}