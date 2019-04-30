data_exercises <- read.csv("exercises.csv" )

BP <- read.csv("bp.csv")
colnames(BP) <- c("generic","common","use","side_effect","precaution")
BP$generic <- as.character(BP$generic)
BP$common <- as.character(BP$common)
BP$use <- as.character(BP$use)
BP$side_effect <- as.character(BP$side_effect)
BP$precaution <- as.character(BP$precaution)


sugar <- read.csv("sugar.csv")
colnames(sugar) <- c("generic","common","use","side_effect","precaution")
sugar$generic <- as.character(sugar$generic)
sugar$common <- as.character(sugar$common)
sugar$use <- as.character(sugar$use)
sugar$side_effect <- as.character(sugar$side_effect)
sugar$precaution <- as.character(sugar$precaution)

chol <- read.csv("chol.csv")
colnames(chol) <- c("generic","common","use","side_effect","precaution")
chol$generic <- as.character(chol$generic)
chol$common <- as.character(chol$common)
chol$use <- as.character(chol$use)
chol$side_effect <- as.character(chol$side_effect)
chol$precaution <- as.character(chol$precaution)

CP <- read.csv("CP.csv")
colnames(CP) <- c("generic","common","use","side_effect","precaution")
CP$generic <- as.character(CP$generic)
CP$common <- as.character(CP$common)
CP$use <- as.character(CP$use)
CP$side_effect <- as.character(CP$side_effect)
CP$precaution <- as.character(CP$precaution)

exercises <- read.csv("exercises.csv")
colnames(exercises) <- c("param","age","exercises")
exercises$param <- as.character(exercises$param)
exercises$age <- as.numeric(exercises$age)
exercises$exercises <- as.character(exercises$exercises)

chol_ex <- subset(exercises,param=="chol")
fbs_ex <- subset(exercises,param=="fbs")
heart_rate_ex <- subset(exercises,param=="max. heart rate")                                                                                        
exang_ex<-subset(exercises,param=="exang")
bp_ex<-subset(exercises,param=="resting bp")

upperBound <- function(n,p){
  l <- 0
  l <- as.integer(l)
  r <- n
  r <- as.integer(n)
  i <- 1/n
  
  while( r-l > 1)
  {
    m <- (l+r)/2
    m <- as.integer(m)
    #print(m)
    if(m*i <= p)
      l=m
    else
      r=m
  }
  return(r)
}
                                                                                        
                                                                                       