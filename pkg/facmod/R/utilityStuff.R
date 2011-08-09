dfClasses <- function(x)
{
  for(i in 1:ncol(x))
  {
    print(paste("col:",i,", name:",colnames(x)[i],", class: ",class(x[,i])))
  }
}