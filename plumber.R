#plumber.R

library(plumber)

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @get /random_numbers
#* @param maxn
function(maxn) {
  maxn<-as.numeric(maxn)
  runif(1,min=0,max=maxn)
}
#* @post /message
#* @param msg vector of numbers
function(msg) {
  list ("Message is", msg)
}


Cost <- function(Modules,Exp1,Exp2,Exp3,Size1,Size2,Size3,Work1,Work2,Work3,RuleD1,RuleD2){
  
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  
  
  ########
  #Costs
  ###########
  ProCost <- 10
  SpecCost <- 12 # Default 12 - 
  AmCost <- 1
  DecompCost <- 10  # Default 10  - 
  TrackCost <- 1
  
  # cast size to numeric
  Size1 <- as.numeric(Size1)
  Size2 <- as.numeric(Size2)
  Size3 <- as.numeric(Size3)
  
  # Cost to coordinate
  if(RuleD1 == 2){
    cost_track1 <- Size1*TrackCost
  }else if(RuleD1 == 1){
    cost_track1 <- TrackCost
  }else{
    cost_track1 <- 0
  }
  if(RuleD2 == 2){
    cost_track2 <- Size2*TrackCost
  }else if(RuleD2 == 1){
    cost_track2 <- TrackCost
  }else{
    cost_track2 <- 0
  }
  # Cost to staff
  if(Exp1 == Pro){
    #print(ProCost)
    #print(Work1)
    #print(Size1)
    costM1 <- ProCost*Work1*Size1
  }else if(Exp1 == Am){
    costM1 <- AmCost + AmCost*Size1*0.1 #Pay for tournament, not work or size# change back to 0.1 for baseline
  }else if(Exp1 == Spec){
    costM1 <- SpecCost*Work1+((Size1-1)*SpecCost*0.1) # parantesis term for bidding cost
  }else{
    costM1 <- 0
  }
  if(Exp2 == Pro){
    costM2 <- ProCost*Size2*Work2
  }else if(Exp2 == Am){
    costM2 <- AmCost + AmCost*Size2*0.1 # change back to 0.1 for baseline
  }else if(Exp2 == Spec) {
    costM2 <- SpecCost*Work2+((Size2-1)*SpecCost*0.1) # parantesis term for bidding cost
  }else{
    costM2 <- 0
  }
  if(Exp3 == Pro){
    costM3 <- ProCost*Size3*Work1
  }else if(Exp3 == Am){
    costM3 <- AmCost + AmCost*Size3*0.1# change back to 0.1 for baseline
  }else if(Exp3 == Spec) {
    costM3 <- SpecCost*Work3+((Size3-1)*SpecCost*0.1)
  }else{
    costM3 <- 0
  }
  # Cost to decompose
  if(Modules == 1){
    cost_decomp <- 0
  }else{
    cost_decomp <- DecompCost
  }
  #  cost_decomp <- (Modules-1)*DecompCost
  return(cost_decomp+costM1+costM2+costM3+cost_track1+cost_track2)
}


Solver <- data.frame(     
  Type = c("Pro", "Novice", "Specialist"),
  Drive_mean = c(250L, 150L, 450L),
  Drive_sd = c(15L, 30L, 30L),
  Drive_Fairway_handoff = c(40,40,40),
  LFairway_mean = c(200L,100L,100L),  
  LFairway_sd = c(10L,20L,20L),
  AFairway_sd_factor = c(0.05,0.2,0.2), #accuracy is a function of distance
  AFairway_sd_sweetspot = c(0.03,0.05,0.05), #accuracy is a function of distance
  Fairway_sweetspot = c(75L,0L,0L),
  Fairway_Putt_handoff = c(15,15,15),
  Putt_prob = c(0.80,0.20,0.20),
  Goodputt_sd_factor = c(0.01,0.05,0.05),
  Badputt_min = c(0,0,0),
  Badputt_max = c(10,15,15)
)

############### Need to change how strategy is implemented, so that there's an advantage not a disadvantage
# Long fairway is like driving just less effective.
############### Need to change how strategy is implemented, so that there's an advantage not a disadvantage
# Long fairway is like driving just less effective.
LFairway <- function (target, Expertise, strategy, n=1){
  
  Expertise <- as.numeric(Expertise)
  
  #strategy <- 0
  if(Expertise== 2 | Expertise == 3){
    strategy <- 0} ## I placed it here because this function is cal
  RemainingDistance <- min(abs(target - rnorm(n,Solver[Expertise,c('LFairway_mean')],Solver[Expertise,c('LFairway_sd')])))
  #  if (RemainingDistance>0.001 && RemainingDistance<Offset){
  #    RemainingDistance<-1
  #  }
  #  if (RemainingDistance<=Sink){RemainingDistance <- Sink+0.1} ### 
  return(RemainingDistance)
}

# Aiming fairway aims near the pin. If Experts are in their sweetspot, they're really accurate. Novices don't have a sweet spot.
AFairway <- function(target, Expertise, n=1, strategy){
  
  Expertise <- as.numeric(Expertise)
  
  #strategy <- 0
  if(Expertise== 2 | Expertise == 3){
    strategy <- 0} ## I placed it here because this function is cal
  if (strategy ==1 & Expertise == 1){ #If it's a subproblem with strategy and you're an expert you can aim
    RemainingDistance <- min(abs(target - rnorm(n,(target-0.5*Offset),target*Solver[Expertise,c('AFairway_sd_sweetspot')])))      
  } 
  
  if ((strategy == 0 && Expertise == 1) || Expertise == 2 || Expertise == 3) {
    RemainingDistance <- min(abs(target - rnorm(n,(target-Offset),target*Solver[Expertise,c('AFairway_sd_factor')])))
  }
  #      if (RemainingDistance>0.001 && RemainingDistance<Offset){
  #        RemainingDistance<-1
  #      }
  #  if (RemainingDistance<=Sink){RemainingDistance <- Sink+0.1}
  return(RemainingDistance)
}

# Edited from earlier: add strategy to driving. Only drive if you control the next one.

Drive <- function (target,Expertise, strategy, n=1) {
  
  Expertise <- as.numeric(Expertise)
  target <- as.numeric(target)
  
  #strategy <- 0 # 9/14/20 bi boyle dene olmazsa buraya   if(Expertise==1) fln tarzi bir gate koy
  if(Expertise== 2 | Expertise == 3){
    strategy <- 0} ## I placed it here because this function is cal ## I placed it here because this function is called by all architectures and this was the only way to decouple the strategy.
  if(Solver[Expertise,c('Drive_mean')]>target){
    RemainingDistance <- LFairway(target,Expertise,strategy,1)
  }else{
    RemainingDistance <- min(abs(target - rnorm(n,Solver[Expertise,c('Drive_mean')],Solver[Expertise,c('Drive_sd')])))  
  }
  return(RemainingDistance)
}

# Drive <- function (target,Expertise, strategy, n=1) {
#   #strategy <- 0 # 9/14/20 bi boyle dene olmazsa buraya   if(Expertise==1) fln tarzi bir gate koy
#   if(Solver[Expertise,c('Drive_mean')]>target){
#     RemainingDistance <- LFairway(target,Expertise,strategy,1)
#   }else{
#     RemainingDistance <- min(abs(target - rnorm(n,Solver[Expertise,c('Drive_mean')],Solver[Expertise,c('Drive_sd')])))  
#   }
#   return(RemainingDistance)
# }

#This is trying to sink (I'm not currently implementing a putting strategy for experts)
Putt <- function (target, Expertise, n=1) {
  #I want it to randomly flip between good shots and bad shots, so I'm a random number from 0 to 1 is bigger than the flip condition..
  if (runif(1,0,1) < Solver[Expertise,c('Putt_prob')]) {
    RemainingDistance <- min(abs(target - rnorm(n,target,target*Solver[Expertise,c('Goodputt_sd_factor')])))
  } else {
    RemainingDistance <- min(abs(target - runif(n,Solver[Expertise,c('Badputt_min')],Solver[Expertise,c('Badputt_max')])))
  }
  return(RemainingDistance)
}

############################################################################################
# Design rules are also a base function. I can use different decision rules to do the handoff
# I did this differently when I was running the path function, so I need to come back and check
# if it still belongs here.
#############################################################################################
#!!!!!! Need to update this with the vector version

#PickBest <- function(distance,strokes,rule){
#  if (rule == 1) { # pick the closest to the hole
#    z <- order(distance)
#    best <- z[1]
#  } else { # of the ones with the fewest strokes, pick the one that's closest to the hole.
#    dist_normalized <- distance/max(distance)
#    # This is because distances are bigger than stroke numbers and I mostly want to sort on strokes
#    z <- order(strokes+dist_normalized)
#    # I was having trouble trying to access order directly, so I'm assigning it to a made up vector
#    best <- z[1]
#  }
#  # There's probably a better way to pass to parameters...
#  result <- c(distance[best],strokes[best])
#  return(result)
#}


##########################
# Now I'm creating functions to run subproblems within golf. Eventually  I will want to be able to call every version of subproblem to make a whole hole.

############################################
# This function putts until you sink
# This has been revised since 2.4 to also track the solving PathTaken.
# It now returns a vector of the path taken, with the last element being the number of strokes. 
# It only returns the best path for the N tries.

#* @post /playPutt
#* @param BallNow initial distance to hole
#* @param Expertise chosen solver
#* @param N Quantity of solver 
#* @param size distance to accept ball in hole, 0.5
PlayPutt <- function(BallNow, Expertise, N,size){
  
  BallNow <- as.numeric(BallNow)
  Expertise <- as.numeric(Expertise)
  N <- as.numeric(N)
  size <- as.numeric(size)
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  Start <- BallNow
  PathTaken <- rep(100,sizeP) # Come back!!! This needs a variable name.
  for (i in 1:N){
    PathTakenAlt <- rep(0.1,sizeP)
    NumStrokes <- 0L
    #   BallNow <- Start ## We think This fixes the model by forcing everyone to take at least one put 10/5 
    BallNow <- Putt(Start,Expertise) 
    NumStrokes <- 1L
    while (abs(BallNow) > size){
      BallNow <-  Putt(BallNow,Expertise)
      NumStrokes <- as.numeric(NumStrokes)
      NumStrokes = NumStrokes + 1
      #cat ("when i is",i, "NumStrokes is", NumStrokes, "and j is", j, "\n")
      if(NumStrokes > 14){ # Mercy rule
        BallNow <- 0
        NumStrokes = NumStrokes +1
      }
      PathTakenAlt[NumStrokes] <- BallNow #continuing to write the PathTaken to this vector
    }
    PathTakenAlt[sizeP-1] <- NumStrokes
    PathTakenAlt[sizeP] <- BallNow
    #cat("For i = ",i,"PathTaken is", PathTaken, "and PathTakenAlt is", PathTakenAlt)
    
    # Best is defined as fewest strokes to sink
    if(PathTakenAlt[sizeP-1]<PathTaken[sizeP-1]){
      PathTaken <- PathTakenAlt
    }
  }
  return(PathTaken)
}

####################################
# This is the fairway function. It starts with a handoff from the drive and takes it until it first crosses the green transition     #
####################################

#* @post /playFairway
#* @param BallNow initial distance to hole
#* @param Expertise chosen solver
#* @param N Quantity of solver 
#* @param rule value of 1 tries to get closest to hole, value of 2 tries to minimize strokes. default 1
#* @param strategy default 0
PlayFairway <- function(BallNow,Expertise,N,rule,strategy){
  
  BallNow <- as.numeric(BallNow)
  Expertise <- as.numeric(Expertise)
  N <- as.numeric(N)
  rule <- as.numeric(rule)
  strategy <- as.numeric(strategy)
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  #strategy <- 0 # removing strategy
  Start <- BallNow
  PathTaken <- rep(1000,sizeF)
  for (i in 1:N){
    PathTakenAlt <- rep(0.1,sizeF)
    NumStrokes <- 0L
    BallNow <- Start
    while (BallNow > Solver[Expertise,c('Fairway_Putt_handoff')]){
      print("While loop ball now: ")
      print(BallNow)
      if (BallNow > (Solver[Expertise,c('LFairway_mean')])){
        BallNow <- LFairway(BallNow, Expertise,strategy,1)
        print("Result from L-fairway is: ")
        print(BallNow)
      } else {
        BallNow <- AFairway(BallNow,Expertise,1,strategy)
        print("Result from A-fairway is: ")
        print(BallNow)
      }
      NumStrokes <- as.numeric(NumStrokes)
      NumStrokes = as.numeric(NumStrokes) + 1
      PathTakenAlt[NumStrokes] <- BallNow #continuing to write the PathTaken to this vector
    }
    PathTakenAlt[sizeF-1] <- NumStrokes
    PathTakenAlt[sizeF] <- BallNow
    #cat("For i = ",i,"PathTaken is", PathTaken, "and PathTakenAlt is", PathTakenAlt)
    # For rule 1, best is defined as the closest ball (ignores strokes)
    if(PathTakenAlt[sizeF]<PathTaken[sizeF] & rule==1){
      PathTaken <- PathTakenAlt
    }
    # For rule 2, best is defined as the one with the fewest strokes, that is closest to the hole.
    if(PathTakenAlt[sizeF-1]<PathTaken[sizeF-1] & rule==2){
      PathTaken <- PathTakenAlt
    } 
    if((PathTakenAlt[sizeF-1]==PathTaken[sizeF-1])&(PathTakenAlt[sizeF]<PathTaken[sizeF]) & rule == 2){
      PathTaken <- PathTakenAlt
    }
    #cat( "So we keep", PathTaken,"\n")
  }
  return(PathTaken)
}
#####################################
# This function drives until you cross the fairway transition #
#####################################

#* @post /playDrive
#* @param HoleDist initial distance to hole
#* @param Expertise chosen solver
#* @param N Quantity of solver 
#* @param rule
#* @param strategy
PlayDrive <- function(HoleDist,Expertise,N,rule,strategy){
  
  HoleDist <- as.numeric(HoleDist)
  Expertise <- as.numeric(Expertise)
  N <- as.numeric(N)
  rule <- as.numeric(rule)
  strategy <- as.numeric(strategy)
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  #strategy <- 0 #removing strategy
  Start <- HoleDist
  PathTaken <- rep(1000,sizeD)
  for (i in 1:N){
    PathTakenAlt <- rep(0.1,sizeD)
    NumStrokes <- 1
    BallNow <- Start
    PathTakenAlt[NumStrokes] <- BallNow
    BallNow <- Drive(HoleDist,Expertise,strategy,1)
    PathTakenAlt[NumStrokes+1] <- BallNow
    PathTakenAlt[sizeD-1] <- NumStrokes
    PathTakenAlt[sizeD] <- BallNow
    #cat("For i = ",i,"PathTaken is", PathTaken, "and PathTakenAlt is", PathTakenAlt)
    if(PathTakenAlt[sizeD]<PathTaken[sizeD] & rule==1){
      #cat(PathTakenAlt[sizeD],"vs.",PathTaken[sizeD])
      PathTaken <- PathTakenAlt
    }
    # For rule 2, best is defined as the one with the fewest strokes, that is closest to the hole.
    if(PathTakenAlt[sizeD-1]<PathTaken[sizeD-1] & rule==2){
      PathTaken <- PathTakenAlt
    } 
    if((PathTakenAlt[sizeD-1]==PathTaken[sizeD-1])&(PathTakenAlt[sizeD]<PathTaken[sizeD]) & rule == 2){
      PathTaken <- PathTakenAlt
    }
    #cat( "So we keep", PathTaken,"\n")
  }
  return(PathTaken)
}


#####################################
# This function starts from the tee and plays until the green transition
#####################################

#* @post /playLong
#* @param BallNow initial distance to hole
#* @param Expertise chosen solver
#* @param N Quantity of solver 
#* @param rule
PlayLong <- function(BallNow,Expertise,N,rule){
  
  Ballnow <- as.numeric(BallNow)
  Expertise <- as.numeric(Expertise)
  N <- as.numeric(N)
  rule <- as.numeric(rule)
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <- 10
  sizeF <- 25
  sizeP <- 17
  Offset <- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  
  # removed strategy when it's executed - TT. I tried to put the strategy back, lets see if it works. 
  PathTaken <- rep(100,sizeD+sizeF) #allowing max strokes with an extra element for count 
  for (i in 1:N){
    PathTakenTemp <- rep(0.1,sizeD+sizeF)
    #PathTakenTemp[1] <- BallNow
    drive <- rep(0,sizeD)
    drive <- PlayDrive(BallNow,Expertise,1,2,1)
    #cat("drive is:",drive,"\n")
    NumStrokes <- drive[sizeD-1]
    CurrentBall <- drive[sizeD]
    PathTakenTemp[1:(NumStrokes+1)] <- drive[1:(NumStrokes+1)]
    #fairway
    fairway <- rep(0,sizeF)
    fairway <- PlayFairway(CurrentBall,Expertise,1,1,1) #no knowledge of what putter wants.
    #cat("fairway is:",fairway,"\n")
    NumStrokesFairway <- fairway[sizeF-1]
    StrokesAfterFairway <- NumStrokesFairway+NumStrokes
    CurrentBall <- fairway[sizeF]
    PathTakenTemp[(NumStrokes+2):(StrokesAfterFairway+1)] <- fairway[1:NumStrokesFairway]
    PathTakenTemp[sizeD+sizeF-1] <- StrokesAfterFairway
    PathTakenTemp[sizeD+sizeF] <- fairway[sizeF]
    if(PathTakenTemp[sizeD+sizeF]<PathTaken[sizeD+sizeF] & rule == 1){
      PathTaken <- PathTakenTemp
    }
    # For rule 2, best is defined as the one with the fewest strokes, that is closest to the hole.
    if(PathTakenTemp[sizeD+sizeF-1]<PathTaken[sizeD+sizeF-1] & rule==2){
      PathTaken <- PathTakenTemp
    } 
    if((PathTakenTemp[sizeD+sizeF-1]==PathTaken[sizeD+sizeF-1])&(PathTakenTemp[sizeD+sizeF]<PathTaken[sizeD+sizeF]) & rule == 2){
      PathTaken <- PathTakenTemp
    }
    #cat("PathTaken is:",PathTaken,"\n")
  }
  return(PathTaken)
}

#####################################
# This function starts from the fairway transition and plays until you sink
#####################################

#* @post /playShort
#* @param BallNow distance ball starts at
#* @param Expertise chosen selector
#* @param N 
#* @param size acceptance distance for making hole, 0.5
PlayShort <- function(BallNow,Expertise,N,size){
  
  BallNow <- as.numeric(BallNow)
  Expertise <- as.numeric(Expertise) # default 700
  N <- as.numeric(N)
  size <- as.numeric(size)
  
  PathTaken <- rep(100000000,sizeF+sizeP) #allowing max strokes with an extra element for count 
  #strokes <- rep(0,N)
  for (i in 1:N){
    PathTakenTemp <- rep(0.1,sizeF+sizeP)
    PathTakenTemp[1] <- BallNow
    #Fairway
    fairway <- rep(0,sizeF)
    fairway <- PlayFairway(BallNow,Expertise,1,2,0) #strategy because you can set up your <- too. ## the rule was =2 here so I fixed that
    #cat("fairway is:",fairway,"\n")
    StrokesFairway <- fairway[sizeF-1]
    CurrentBall <- fairway[sizeF]
    PathTakenTemp[2:(StrokesFairway+1)] <- fairway[1:StrokesFairway]
    #Putt
    putt <- rep(0,sizeP)
    putt <- PlayPutt(CurrentBall,Expertise,1,size)
    #cat("putt is:",putt,"\n")
    NumStrokesPutt <- putt[sizeP-1]
    StrokesAfterPutt <- StrokesFairway + NumStrokesPutt
    PathTakenTemp[(StrokesFairway+2):(StrokesAfterPutt+1)] <- putt[1:NumStrokesPutt]
    PathTakenTemp[sizeF+sizeP-1] <- StrokesAfterPutt
    PathTakenTemp[sizeF+sizeP] <- putt[sizeP]
    
    #No need for rules because we're playing to the hole.
    if(PathTakenTemp[sizeF+sizeP-1]<PathTaken[sizeF+sizeP-1]){
      PathTaken <- PathTakenTemp
    }
    #cat("PathTaken is:",PathTaken,"\n")
  }
  # Returns a vector where the first stroke is the starting position.
  return(PathTaken)
}

#####################################
# This function starts from the tee and plays until you sink
#####################################
#!!!!!!Update to return a vector that's the best of N plays the whole hole

PlayWholeHole <- function(BallNow,Expertise,N,size){
  
  HoleLength <- BallNow
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  PathTaken <- rep(1000000000,buffer) #allowing max strokes with an extra element for count 
  for (i in 1:N){
    # print("One Stroke?")
    PathTakenTemp <- rep(0,buffer)
    #PathTakenTemp[1] <- BallNow
    #drive
    if(BallNow==HoleLength){
      #### This is what we added to prevent DS guys from driving one more time (Which we think was granting Ams an advantage)
      drive <- rep(0,sizeD)
      drive <- PlayDrive(BallNow,Expertise,1,2,1) #rule=2 because this is whole hole; strategy could be removed
      #cat("drive is:",drive,"\n")
      NumStrokes <- drive[sizeD-1]
      CurrentBall <- drive[sizeD]
      PathTakenTemp[1:(NumStrokes+1)] <- drive[1:(NumStrokes+1)]
    } else{CurrentBall <-BallNow} ## this should  
    #fairway
    fairway <- rep(0,sizeF)
    fairway <- PlayFairway(CurrentBall,Expertise,1,2,1)
    #cat("fairway is:",fairway,"\n")
    NumStrokesFairway <- fairway[sizeF-1]
    StrokesAfterFairway <- NumStrokesFairway+NumStrokes
    CurrentBall <- fairway[sizeF]
    PathTakenTemp[(NumStrokes+2):(StrokesAfterFairway+1)] <- fairway[1:NumStrokesFairway]
    #putt
    putt <- rep(0,sizeP)
    putt <- PlayPutt(CurrentBall,Expertise,1,size)
    #cat("putt is:",putt,"\n")
    #cat("here's the putt:",putt,"\n")
    NumStrokesPutt <- putt[sizeP-1]
    if (NumStrokesPutt >0){
      StrokesAfterPutt <- StrokesAfterFairway + NumStrokesPutt
      PathTakenTemp[(StrokesAfterFairway+2):(StrokesAfterPutt+1)] <- putt[1:NumStrokesPutt]  
    }
    PathTakenTemp[buffer-1] <- StrokesAfterFairway + NumStrokesPutt
    PathTakenTemp[buffer] <- putt[sizeP]
    # Pick the best
    #cat("PathTakenTemp is:",PathTakenTemp,"\n")
    if(PathTakenTemp[buffer-1]<PathTaken[buffer-1]){
      PathTaken <- PathTakenTemp
    }
    #cat("PathTaken is:",PathTaken,"\n")
    
  }
  #cat("PathTaken is:",PathTaken,"\n")
  return(PathTaken)
}


# ACTUAL APIS TO HIT


############################
# This runs the whole hole (H)
############################
#Figour out what to change

# Pro is expertise 1, Amateur is 2, and specialist is 3

#* @post /h_arch
#* @param HoleLength vector of numbers
#* @param Expertise
#* @param TournamentSize 
#* @param Holes
#* @param runs
H_Arch <- function(HoleLength,Expertise,TournamentSize,Holes,runs){
  
  #returnVal <- list ("Played whole hole", HoleLength, Expertise, TournamentSize, Holes, runs)
  
  #convert incoming params to numbers
  Holes <- as.numeric(Holes)
  HoleLength <- as.numeric(HoleLength) # default 700
  runs <- as.numeric(runs)
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  Result <- matrix(0,buffer*Holes+head,runs)
  #cat("results size =",size(Result),"\n")
  for (i in 1:runs){
    for (j in 1:Holes){
      # print("on a hole")
      start <- (j-1)*buffer+head-1
      #cat(":",start)
      end <- start+buffer-1
      #cat(end,"\n")
      # print(PlayWholeHole(HoleLength,Expertise,TournamentSize, Sink))
      
      
      Result[start:end,i] <- PlayWholeHole(HoleLength,Expertise,TournamentSize, Sink)
      Result[1,i] <- Result[1,i]+Result[(end-1),i]
      Result[2,i] <- Result[1,i]
      #Result[6,i] <- Result[6,i] + max(Result[2:4,i])
    }
    
    
    
    Result[5,i] <- Cost(1,Expertise,0,0,TournamentSize,0,0,Result[2,i]/Holes,Result[3,i]/Holes,Result[4,i]/Holes,0,0)
    Result[6,i] <- Result[1,i]/Holes
    #print("Number of strokes is: ")
    #print(Result[1,i])
    #print("Strokes per hole: ")
    #print(Result[6,i])
    #print("Cost is: ")
    #print(Result[5,i])
    if(Expertise == 1){
      Result[7,i] = 1
    } else {
      Result[7,i] = 0
    }
  }
  
  
  #PathTaken[TournamentSize*20+1] <- Numstrokes
  # print(Result)
  return(Result)
}

############################
# This runs the long + putt architecture. 
###########################
# Rule 1 is practica, Rule 2 is optimal, Rule 3 is decoupled

#* @post /lp_arch
#* @param HoleLength
#* @param Expertise_L 
#* @param Expertise_P
#* @param TournamentSize_L
#* @param TournamentSize_P
#* @param Rule
#* @param Holes
#* @param runs
LP_Arch <- function(HoleLength, Expertise_L, Expertise_P, TournamentSize_L, TournamentSize_P,Rule,Holes,runs){
  
  
  #convert incoming params to numbers
  Holes <- as.numeric(Holes)
  HoleLength <- as.numeric(HoleLength) # default 700
  runs <- as.numeric(runs)
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  Rule <- 1 #practical handoffs
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  
  Result <- matrix(0,buffer*Holes+head,runs)
  cat(Result)
  #NumStrokes <- 0L
  for (i in 1:runs){
    for (j in 1:Holes){
      Long <- rep(0,(sizeD+sizeF))
      Temp <- rep(0,buffer)
      if(Rule == 1){
        Long <- PlayLong(HoleLength,Expertise_L,TournamentSize_L,1)
        LongStrokes <- Long[sizeD+sizeF-1]
        BallNow <- Long[sizeD+sizeF]
      } else if(Rule == 2){
        Long <- PlayLong(HoleLength,Expertise_L,TournamentSize_L,2)
        LongStrokes <- Long[sizeD+sizeF-1]
        BallNow <- Long[sizeD+sizeF]
      } else {
        NewTarget <- HoleLength - GreenTransition
        # Find my issue!!!!!!!
        Temp <- PlayWholeHole(NewTarget,Expertise_L,TournamentSize_L,zone)
        #cat("Temp is:",Temp,"\n")
        LongStrokes <- Temp[buffer-1]
        #cat("strokes is:",LongStrokes,"\n")
        ######## does this need to be Temp[buffer]
        BallNow <- Temp[buffer]+GreenTransition
        #cat("ball now is:",BallNow,"\n")
        Long <- Temp + GreenTransition
        #cat("long is:",Long,"\n")
      }
      #cat("long is:",Long,"\n")
      start <- (j-1)*buffer+head+1
      end <- start+LongStrokes
      #cat("when j is",j,"start is",start,"and end is",end,"\n")
      #cat("longstrokes is:",LongStrokes)
      Result[start:end,i] <- Long[1:(LongStrokes+1)]
      Putt <- rep(0,sizeP)
      Putt <- PlayPutt(BallNow,Expertise_P,TournamentSize_P,Sink)
      #cat("putt is:",Putt,"\n")
      PuttStrokes <- Putt[sizeP-1]
      start <- end
      end <- start+PuttStrokes
      Result[start:end,i] <- Putt[1:(PuttStrokes+1)] # problem is here
      Result[((j-1)*(buffer)+(buffer+head-1)),i] <- LongStrokes+PuttStrokes
      Result[((j-1)*(buffer)+(buffer+head)),i] <- Putt[sizeP]
      Result[1,i] <- Result[1,i]+ Result[((j-1)*(buffer)+(buffer+head-1)),i]
      if(Rule == 3){
        Result[6,i] <- Result[6,i] + max(c(LongStrokes,PuttStrokes))# in steady state
        Result[7,i] <- Result[6,i]
      } else {
        Result[6,i] <- Result[6,i] + max(c(LongStrokes,PuttStrokes))# in steady state
        Result[7,i] <- Result[1,i]
      }
      Result[2,i] <- Result[2,i] + LongStrokes
      Result[3,i] <- Result[3,i] + PuttStrokes
      # Calculating fraction of work done by experts
      Fraction <- 0
      if (Expertise_L == 1){
        Fraction <- LongStrokes
      }
      if (Expertise_P ==1){
        Fraction <- Fraction + PuttStrokes
      }
      Result[7,i] <- Fraction/(LongStrokes + PuttStrokes)
      #cat("result is:",Result,"\n")
    }
    Result[5,i] <- Cost(2,Expertise_L,Expertise_P,0,TournamentSize_L,TournamentSize_P,0,Result[2,i]/Holes,Result[3,i]/Holes,Result[4,i]/Holes,Rule,0)
    Result[6,i] <- Result[6,i]/Holes
    #Result[7,i] <- Result[7,i]/Holes
    #print("Strokes: ")
    #print(Result[1,i])
    #print("Cost is: ")
    #print(Result[5, i])
  }
  return(Result)
}


###########################
# This is the drive-fairway-putt architecture (DAP)
###########################

### Adopt fixe when use non PNAS - DAP need to swithch fairway to play from ballnow.

#* @post /dap_arch
#* @param HoleLength
#* @param Expertise_D 
#* @param Expertise_F
#* @param Expertise_P
#* @param TournamentSize_D
#* @param TournamentSize_F
#* @param TournamentSize_P
#* @param Holes
#* @param runs
DAP_Arch <- function(HoleLength,Expertise_D,Expertise_F,Expertise_P,TournamentSize_D,TournamentSize_F,TournamentSize_P,Holes,runs){
  
  #convert incoming params to numbers
  Holes <- as.numeric(Holes)
  HoleLength <- as.numeric(HoleLength) # default 700
  runs <- as.numeric(runs)
  Rule <- 1 #practical handoffs
  RuleDF <- Rule
  RuleFP <- Rule
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  Result <- matrix(0,buffer*Holes+head,runs)
  for (i in 1:runs){
    for (j in 1:Holes){
      # the D subproblem
      drive <- rep(0,sizeD)
      Temp <- rep(0,buffer)
      #cat("here 1")
      if(RuleDF == 1){
        #cat("here 1a")
        drive <- PlayDrive(HoleLength,Expertise_D,TournamentSize_D,1,0)
        DriveStrokes <- drive[sizeD-1]
        BallNow <- drive[sizeD]
      } else if (RuleDF == 2){
        #cat("here 1b")
        drive <- PlayDrive(HoleLength,Expertise_D,TournamentSize_D,2,0)
        DriveStrokes <- drive[sizeD-1]
        BallNow <- drive[sizeD]
      } else {
        #cat("here 1c")
        NewTarget <- HoleLength - FairwayTransition
        Temp <- PlayWholeHole(NewTarget,Expertise_D,TournamentSize_D,zone)
        #cat("long is:",Temp,"\n")
        DriveStrokes <- Temp[buffer-1]
        #cat("strokes is:",LongStrokes,"\n")
        BallNow <- Temp[buffer]+FairwayTransition
        #cat("ball now is:",BallNow,"\n")
        drive <- Temp + FairwayTransition
      }
      # return("GOT THIS FAR")
      start <- (j-1)*buffer+8
      end <- start+DriveStrokes
      Result[start:end,i] <- drive[1:(DriveStrokes+1)]
      ### Approach
      Approach <- rep(0,(sizeF))
      Temp <- rep(0,buffer)
      if(RuleFP == 1){
        Approach <- PlayFairway(BallNow,Expertise_F,TournamentSize_F,1,0) # I turned the strategy off here after speaking with Zoe
        ApproachStrokes <- Approach[sizeF-1]
        BallNow <- Approach[sizeF]
      } else if(RuleFP == 2){
        Approach <- PlayFairway(BallNow,Expertise_F,TournamentSize_F,2,0) # I turned the strategy off here after speaking with Zoe
        ApproachStrokes <- Approach[sizeF-1]
        BallNow <- Approach[sizeF]
      } else {
        NewTarget <- BallNow - GreenTransition
        # Find my issue!!!!!!!
        Temp <- PlayWholeHole(NewTarget,Expertise_F,TournamentSize_F,zone)
        #cat("Temp is:",Temp,"\n")
        ApproachStrokes <- Temp[buffer-1]
        #cat("strokes is:",LongStrokes,"\n")
        ######## does this need to be Temp[buffer]
        BallNow <- Temp[buffer]+GreenTransition
        #cat("ball now is:",BallNow,"\n")
        Approach <- Temp + GreenTransition
        #cat("long is:",Long,"\n")
      }
      start <- end+1 # delete +1 if it doesn't solve anything.
      end <- start+ApproachStrokes
      Result[start:end,i] <- Approach[1:(ApproachStrokes+1)]
      ## This is where Putting stage begins
      Putt <- rep(0,sizeP)
      Putt <- PlayPutt(BallNow,Expertise_P,TournamentSize_P,Sink)
      #cat("putt is:",Putt,"\n")
      PuttStrokes <- Putt[sizeP-1]
      start <- end
      end <- start+PuttStrokes
      Result[start:end,i] <- Putt[1:(PuttStrokes+1)]
      Result[((j-1)*(buffer)+(buffer+head-1)),i] <- DriveStrokes+ApproachStrokes+PuttStrokes
      #cat(((j-1)*(buffer+4)+(buffer+4)),"=", DriveStrokes, FairwayStrokes,PuttStrokes)
      Result[((j-1)*(buffer)+(buffer+head)),i] <- Putt[sizeP]
      #cat(Putt[sizeP])
      #cat("drive is:", drive,"\n","approach is:",approach,"\n","Putt is:",Putt,"\n","Result is:",Result,"\n")
      Result[1,i] <- Result[1,i]+Result[((j-1)*(buffer)+(buffer+head-1)),i]
      Result[2,i] <- Result[2,i] + DriveStrokes
      #cat("Result is:",Result,"\n")
      Result[3,i] <- Result[3,i] + ApproachStrokes
      Result[4,i] <- Result[4,i] + PuttStrokes
      if(RuleDF == 3&RuleFP==3){
        Result[6,i] <- Result[6,i] + max(c(DriveStrokes,ApproachStrokes,PuttStrokes))# in steady state
        #Result[7,i] <- Result[6,i]
      } else if (RuleDF == 3){
        Result[6,i] <- Result[6,i] + max(c(DriveStrokes,ApproachStrokes,PuttStrokes))# in steady state
        #Result[7,i] <- Result[6,i] + max(c(DriveStrokes,ApproachStrokes))+PuttStrokes
      } else if (RuleFP == 3){
        Result[6,i] <- Result[6,i] + max(c(DriveStrokes,ApproachStrokes,PuttStrokes))# in steady state
        #Result[7,i] <- Result[6,i] + max(c(PuttStrokes,ApproachStrokes))+DriveStrokes
      } else {
        Result[6,i] <- Result[6,i] + max(c(DriveStrokes,ApproachStrokes,PuttStrokes))# in steady state
        #Result[7,i] <- Result[1,i]
      }
      Fraction <- 0
      if (Expertise_D == 1){
        Fraction <- DriveStrokes
      }
      if (Expertise_F ==1){
        Fraction <- Fraction + ApproachStrokes
      }
      if (Expertise_P ==1){
        Fraction <- Fraction + PuttStrokes
      }
      Result[7,i] <- Fraction/(DriveStrokes + ApproachStrokes + PuttStrokes)
    }
    Result[5,i] <- Cost(3,Expertise_D,Expertise_F,Expertise_P,TournamentSize_D,TournamentSize_F,TournamentSize_P,Result[2,i]/Holes,Result[3,i]/Holes,Result[4,i]/Holes,RuleDF,RuleFP)
    Result[6,i] <- Result[6,i]/Holes
    #Result[7,i] <- Result[7,i]/Holes
    #print("Strokes: ")
    #print(Result[1,i])
    #print("Cost is: ")
    #print(Result[5, i])
  }
  return(Result)
}

#############################
# The runs Drive-Short (DS)
#############################

#* @post /ds_arch
#* @param HoleLength
#* @param Expertise_D 
#* @param Expertise_S
#* @param TournamentSize_D
#* @param TournamentSize_S
#* @param Rule
#* @param Holes
#* @param runs
DS_Arch <- function(HoleLength,Expertise_D,Expertise_S,TournamentSize_D,TournamentSize_S,Holes,runs){
  
  #convert incoming params to numbers
  Holes <- as.numeric(Holes)
  HoleLength <- as.numeric(HoleLength) # default 700
  runs <- as.numeric(runs)
  Rule <- 1 #practical handoffs
  
  GreenSize <- 15L # default was 20, Playing around with this.
  FudgeFactor <- 20L #this is a stroke on sweetspot
  n <-  1 # number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
  s <- 3 #number of solver types
  t <- 100 #tournament size
  t_Pro <-1 # Professional tournament Size
  Pen_am <-1 #Penalty for Amateur Putters
  sp <- 3 # specialist tournament size (currently set to == s, but need to change if that changes)
  Pro <- 1L
  Am <- 2L
  Spec <- 3L
  Am_run <- t
  Pro_run <- 1L
  # spacers
  stroke <- 15
  buffer <- 40
  head <- 7
  sizeD <<- 10
  sizeF <<- 25
  sizeP <<- 17
  Offset <<- 10L # you don't typically aim at the pin (must be less than 20) - default 15
  FudgeFactor <- 5L #this is a stroke on sweetspot
  Sink <- 0.5 #This hopefully ensures that the loop closes.
  zone <- 5 #you can handoff to the next stage if you're within +/- 5 for rule three
  
  Result <- matrix(0,buffer*Holes+7,runs)
  for (i in 1:runs){
    for (j in 1:Holes){
      drive <- rep(0,sizeD)
      Temp <- rep(0,buffer)
      #cat("here 1")
      if(Rule == 1){
        #cat("here 1a")
        drive <- PlayDrive(HoleLength,Expertise_D,TournamentSize_D,1,0)
        DriveStrokes <- drive[sizeD-1]
        BallNow <- drive[sizeD]
      } else if (Rule == 2){
        #cat("here 1b")
        drive <- PlayDrive(HoleLength,Expertise_D,TournamentSize_D,2,0)
        DriveStrokes <- drive[sizeD-1]
        BallNow <- drive[sizeD]
      } else {
        #cat("here 1c")
        NewTarget <- HoleLength - FairwayTransition
        Temp <- PlayWholeHole(NewTarget,Expertise_D,TournamentSize_D,zone)
        #cat("long is:",Temp,"\n")
        DriveStrokes <- Temp[buffer-1]
        #cat("strokes is:",LongStrokes,"\n")
        BallNow <- Temp[buffer]+FairwayTransition
        #cat("ball now is:",BallNow,"\n")
        drive <- Temp + FairwayTransition
      }
      #cat(drive)
      #cat("here 2")
      start <- (j-1)*buffer+8
      end <- start+DriveStrokes
      #cat("when j is",j,"start is",start,"DriveStrokes is:", DriveStrokes,"and end is",end,"\n")
      Result[start:end,i] <- drive[1:(DriveStrokes+1)]
      short <- rep(0,(sizeF+sizeP))
      short <- PlayShort(BallNow,Expertise_S,TournamentSize_S,Sink)
      ShortStrokes <- short[sizeF+sizeP-1] 
      BallNow <- short[sizeF+sizeP]
      start <- end
      end <- start+ ShortStrokes
      #cat("here 1:", ShortStrokes)
      Result[start:end,i] <- short[1:(ShortStrokes+1)]
      #cat("here 2")
      Result[((j-1)*buffer+buffer+6),i] <- DriveStrokes+ShortStrokes
      Result[((j-1)*buffer+buffer+7),i] <- BallNow
      Result[1,i] <- Result[1,i]+Result[((j-1)*buffer+buffer+6),i]
      Result[2,i] <- Result[2,i] + DriveStrokes
      Result[3,i] <- Result[3,i] + ShortStrokes
      if(Rule == 3){
        Result[6,i] <- Result[6,i] + max(c(DriveStrokes,ShortStrokes))# in steady state
        #Result[7,i] <- Result[6,i] #for a given hole
      } else {
        Result[6,i] <- Result[6,i] + max(c(DriveStrokes,ShortStrokes))# in steady state
        #Result[7,i] <- Result[1,i]
      }
      Fraction <- 0
      if (Expertise_D == 1){
        Fraction <- DriveStrokes
      }
      if (Expertise_S ==1){
        Fraction <- Fraction + ShortStrokes
      }
      Result[7,i] <- Fraction/(DriveStrokes + ShortStrokes)
      #cat("result is:",Result,"\n")
    }
    Result[5,i] <- Cost(2,Expertise_D,Expertise_S,0,TournamentSize_D,TournamentSize_S,0,Result[2,i]/Holes,Result[3,i]/Holes,Result[4,i]/Holes,Rule,0)
    Result[6,i] <- Result[6,i]/Holes
    #Result[7,i] <- Result[7,i]/Holes
  }
  return(Result)
}

