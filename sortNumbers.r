###
# Title: Sort number via loop without using sort function
# Problem Statement: Sort number using loop
# Author: Mousaidna Rosario
###

a <- sample(1:100, replace = T)

a

sortNo <- function(ParamNo) {
  
  for ( n in (length(ParamNo)-1):1) {
    for (i in n:(length(ParamNo)-1)) {
      if (ParamNo[i] > ParamNo[i+1]) {
        temp <- ParamNo[i]
        ParamNo[i] <- ParamNo[i+1]
        ParamNo[i+1] <- temp
      }
    }
  }
  return(ParamNo)
  
}

sortNo(a)
