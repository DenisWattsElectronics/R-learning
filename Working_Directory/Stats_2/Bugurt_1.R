### Stats_2 






smart_test <-  function(x){
  if(sum(!table(x)>=5) == 0){
    return(chisq.test(x[,1], x[,2]))
  }
  else{
    return(fisher.test(x[,1], x[,2]))
  }
}


smart_test <-  function(x){
  if(sum(!table(x)>=5) == 0){
    return(c(chisq.test(x[,1], x[,2])$statistic, c(chisq.test(x[,1], x[,2])$parameter, c(chisq.test(x[,1], x[,2])$p.value ))))
  }
  else{
    return(fisher.test(x[,1], x[,2])$p.value)
  }
}

x <- mtcars[,c("am", "vs")]
str(chisq.test(x[,1], x[,2]))
y <- chisq.test(x[,1], x[,2])
y$statistic
y$parameter
y$p.value

smart_test(mtcars[,c("am", "vs")])
smart_test(mtcars[1:20,c("am", "vs")])





