life.table = function(data) {

  ages = c(0, 1, seq(5, 85))
  years = seq(1953, 2098, by = 5)

  # Subsetting data in order to use only Brazilian information
  data = subset(data, LocID == 76)
  # Subsetting data again in order to use only age groups lower or equal to 85 to match PNAD availiability
  data = subset(data, Age <= 85)
  
  for(i in 1:nrow(data)){
    if(data$Age[i] == 85){
      data$ax[i] = data$ex[i]
      data$qx[i] = 1
    } else {
      data$ax[i] = data$ax[i]
      data$qx[i] = data$qx[i]
    }
  }
  
  data$qx = as.numeric(data$qx)
  data$Period = rep(years, each = 19)
  
  # Creating qx, mx and ax 5-yr age groups Matrix You must load the reshape2
  # R-package before running the next lines of code
  Matrix_qx = dcast(data, Age ~ Period, value.var = "qx")[, -1]
  rownames(Matrix_qx) = c(0, 1, seq(5, 85, by = 5))
  Matrix_ax = dcast(data, Age ~ Period, value.var = "ax")[, -1]

  # Using beers' osculatory interpolation method to split data into single ages
  # (excludes the '0-1', '1-4' and '85+' age groups)
  Single_qx = as.matrix(rbind(Matrix_qx[1:2, 1], 1 - 
                                exp(graduate_beers(as.matrix(log(1 - Matrix_qx)[-c(1, 2, 19), 1]), Age = seq(5, 80, by = 5),
                                          OAG = FALSE, method = "ord", johnson = FALSE)), 1))
  
  Single_qx = matrix(nrow = length(seq(5, 84, by = 1)), ncol = length(unique(data$Period)))
  dimnames(Single_qx) = list(seq(5, 84, by = 1), years)
  
  for(i in 1:ncol(Matrix_qx)) {
    Single_qx[, i] = as.matrix(1 - exp(graduate_beers(as.matrix(log(1 - Matrix_qx)[-c(1, 2, 19), i]), 
                                                       Age = seq(5, 80, by = 5), OAG = FALSE, method = "ord", johnson = FALSE))) 
  }
  
  Single_qx = as.matrix(rbind(Matrix_qx[1:2, ], Single_qx, 1))
  
  dimnames(Single_qx) = list(ages, years)
  rm(Matrix_qx)

  # Estimating single age lx Matrix -- Single_lx
  Cumprod_px = apply((1 - Single_qx), 2, cumprod)
  Single_lx = matrix(nrow = nrow(Cumprod_px), ncol = ncol(Cumprod_px))
  for (i in 2:nrow(Cumprod_px)) {
    for (j in 1:ncol(Cumprod_px)) {
      Single_lx[i, j] = Cumprod_px[i - 1, j]
    }
  }
  Single_lx[1, ] = 1
  dimnames(Single_lx) = list(ages, years)
  rm(Cumprod_px)

  # Estimating single age dx Matrix -- Single_dx
  Single_dx = Single_lx * Single_qx
  dimnames(Single_dx) = list(ages, years)

  # Estimating single age ax Matrix -- Single_ax
  Single_ax = matrix(nrow = nrow(Single_dx), ncol = ncol(Single_dx))
  Single_ax[1, ] = as.numeric(Matrix_ax[1, ])
  Single_ax[2, ] = as.numeric(Matrix_ax[2, ])
  Single_ax[nrow(Single_ax), ] = as.numeric(Matrix_ax[nrow(Matrix_ax), ])
  for (i in seq(3, nrow(Single_ax) - 1)) {
    for (j in 1:ncol(Single_ax)) {
      Single_ax[i, j] = 0.5
    }
  }
  dimnames(Single_ax) = list(ages, years)
  rm(Matrix_ax)

  # Estimating single age Lx Matrix -- Single_Lx The 'n' vector gives us the age
  # intervals
  n = c(diff(c(0, 1, seq(5, 85, by = 1))), 999)

  Single_Lx = matrix(nrow = nrow(Single_ax), ncol = ncol(Single_ax))
  for (i in 1:nrow(Single_lx)) {
    for (j in 1:ncol(Single_lx)) {
      Single_Lx[i, j] = n[i] * Single_lx[i, j] - Single_ax[i, j] * Single_dx[i,
        j]
    }
  }
  Single_Lx[nrow(Single_Lx), ] = Single_lx[nrow(Single_Lx), ] * Single_ax[nrow(Single_Lx),
    ]
  dimnames(Single_Lx) = list(ages, years)

  # Estimating single age Tx Matrix -- Single_Tx
  Single_Tx = matrix(nrow = nrow(Single_Lx), ncol = ncol(Single_Lx))
  Single_Tx = apply(Single_Lx, 2, rev)
  Single_Tx = apply(Single_Tx, 2, cumsum)
  Single_Tx = apply(Single_Tx, 2, rev)
  dimnames(Single_Tx) = list(ages, years)

  # Estimating single age ex Matrix -- Single_ex
  Single_ex = matrix(nrow = nrow(Single_Tx), ncol = ncol(Single_Tx))
  for (i in 1:nrow(Single_ex)) {
    for (j in 1:ncol(Single_ex)) {
      Single_ex[i, j] = Single_Tx[i, j]/Single_lx[i, j]
    }
  }
  dimnames(Single_ex) = list(ages, years)

  result = list(lx = as.data.frame(Single_lx), qx = as.data.frame(Single_qx), ex = as.data.frame(Single_ex))
}
