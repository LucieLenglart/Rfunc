
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

CAF <- function(data, quant = c(...)) {

  sub = unique(data$subject) #select each unique subject code
  nSub = length(sub) #get number of subjects

  globQuant = matrix(0, nrow = nSub, ncol = length(quant)) #initiate df for quantiles
  cafOut = as.data.frame(matrix(0, nrow = 2, ncol = length(quant))) #initiale df for output data

  for (i in seq_along(sub)) { #iterate on each subject to compute and keep individual quantiles
    subQuant = quantile(data$rt[data$subject == sub[i]], quant)
    globQuant[i, ] = subQuant
  }
  globQuant = na.omit(globQuant) #remove NA
  vinQuant = t(as.data.frame(colMeans(globQuant))) #average subject's quantiles

  for (i in 2:length(vinQuant)) { #compute accuracy for each quantile
    cafOut[1, (i-1)] = sum(data$accuracy[data$rt < vinQuant[1,i] & data$rt > vinQuant[1, (i - 1)]]) / length(data$accuracy[data$rt < vinQuant[1,i] & data$rt > vinQuant[1, (i - 1)]])
    cafOut[2,(i-1)] = sum(data$rt[data$rt < vinQuant[1,i] & data$rt > vinQuant[1, (i - 1)]]) / length(data$rt[data$rt < vinQuant[1, i] & data$rt > vinQuant[1, (i - 1)]])

  }
  cafOut = as.data.frame(cafOut[,-ncol(cafOut)])
  cafOut <<- t(cafOut[,(length(cafOut))-1])
  print(cafOut)
}










