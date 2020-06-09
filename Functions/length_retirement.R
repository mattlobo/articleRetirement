length.retirement = function(lfpr, lifetable) {

  years_lfpr = unique(lfpr$Year)
  years_lifetable = as.numeric(colnames(lifetable[[1]]))
  mutual_years = Reduce(intersect, list(years_lfpr, years_lifetable))

  ages = seq(49, 85)

  lfpr = subset(lfpr, Age >= 49 & Year %in% mutual_years)
  RetirementRate = 1 - lfpr
  lfpr = dcast(lfpr, Age ~ Year)[, -1]
  dimnames(lfpr) = list(ages, mutual_years)


  Gamma = matrix(nrow = nrow(lfpr), ncol = ncol(lfpr))
  for (i in 2:nrow(Gamma)) {
    for (j in 1:ncol(Gamma)) {
      Gamma[i, j] = max(0, 1 - lfpr[i, j]/lfpr[i - 1, j])
    }
  }
  dimnames(Gamma) = list(ages, mutual_years)

  agesLT = seq(49, 85)
  
  lx = lifetable$lx
  lx$Age = as.numeric(rownames(lx))
  lx = melt(lx, id.vars = "Age")
  colnames(lx) = c("Age", "Year", "lx")
  lx = subset(lx, Age >= 49 & Year %in% mutual_years)
  lx = dcast(lx, Age ~ Year)[, -1]
  dimnames(lx) = list(agesLT, mutual_years)


  sx = matrix(nrow = nrow(lx), ncol = ncol(lx))
  for (i in 2:nrow(sx)) {
    for (j in 1:ncol(sx)) {
      sx[i, j] = lx[i, j]/lx[2, j]
    }
  }
  dimnames(sx) = list(agesLT, mutual_years)

  qx = lifetable$qx
  qx$Age = as.numeric(rownames(qx))
  qx = melt(qx, id.vars = "Age")
  colnames(qx) = c("Age", "Year", "qx")
  qx = subset(qx, Age >= 49 & Year %in% mutual_years)
  qx = dcast(qx, Age ~ Year)[, -1]
  dimnames(qx) = list(agesLT, mutual_years)

  ex = lifetable$ex
  ex$Age = as.numeric(rownames(ex))
  ex = melt(ex, id.vars = "Age")
  colnames(ex) = c("Age", "Year", "ex")
  ex = subset(ex, Age >= 49 & Year %in% mutual_years)
  ex = dcast(ex, Age ~ Year)[, -1]
  dimnames(ex) = list(agesLT, mutual_years)

  p = lifetable$lx
  p$Age = as.numeric(rownames(p))
  p = melt(p, id.vars = "Age")
  colnames(p) = c("Age", "Year", "lx")
  p = subset(p, Age %in% c(20, 50) & Year %in% mutual_years)
  p = dcast(p, Age ~ Year)[, -1]
  dimnames(p) = list(c(20, 50), mutual_years)
  p20to50 = p[2, ]/p[1, ]

  elrp = matrix(nrow = nrow(sx), ncol = ncol(sx))
  for (i in 2:nrow(elrp)) {
    for (j in 1:ncol(elrp)) {
      elrp[i, j] = (lfpr[i, j] * Gamma[i, j] * sx[i, j]) * (1 - (0.5 * qx[i,
        j])) * ifelse(i == nrow(elrp), ((ex[i, j])/2), ((ex[i, j] + ex[i +
        1, j])/2))
    }
  }
  dimnames(elrp) = list(ages, mutual_years)

  result = apply(elrp, 2, sum, na.rm = T) * p20to50
  dimnames(result) = list("ELRP", mutual_years)

  return(result)
}