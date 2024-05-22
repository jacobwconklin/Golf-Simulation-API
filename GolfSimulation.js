// Translation of R code into JS by Jacob Conklin
var randomNormal = require('random-normal');

// r to js math function translations:
// generate 1 random value between min and max
// runif(1,min,max) = (Math.random() * (max - min) + min) 

// generate 1 random value from a normal distribution with mean and sd
// rnorm(1,mean,sd) = randomNormal({mean: mean, dev: sd}) // using: https://www.npmjs.com/package/random-normal

// initialize array of length numberOfRepititions with value
// rep(value, numberOfRepititions) = Array(numberOfRepititions).fill(value)

// Globals
// const GreenSize = 15 // default was 20, Playing around with this.
// const Rule = 1 //practical handoffs
// const n =  1 // number of monte-carlo runs-  set to 1 now because I want to run one attempt for each student. 
// const s = 3 //number of solver types
// const t = 100 //tournament size
// const t_Pro =1 // Professional tournament Size
// const Pen_am =1 //Penalty for Amateur Putters
// const sp = 3 // specialist tournament size (currently set to === s, but need to change if that changes)
const Pro = 1
const Am = 2
const Spec = 3
// const Am_run = t
// const Pro_run = 1
// spacers
// const stroke = 15
const buffer = 40
const head = 7
const sizeD = 10
const sizeF = 25
const sizeP = 17
const Offset = 10 // you don't typically aim at the pin (must be less than 20) - default 15
// const FudgeFactor = 5 //this is a stroke on sweetspot
const Sink = 0.5 //This hopefully ensures that the loop closes.
const zone = 5 //you can ha
const GreenTransition = 15
 const FairwayTransition = (HoleLength) => { return HoleLength - 200 }

const Cost = (Modules,Exp1,Exp2,Exp3,Size1,Size2,Size3,Work1,Work2,Work3,RuleD1,RuleD2) => {

  // Costs
  const ProCost = 10
  const SpecCost = 12 // Default 12 - 
  const AmCost = 1
  const DecompCost = 10  // Default 10  - 
  const TrackCost = 1
      
  let cost_track1;
  let cost_track2;

  // Cost to coordinate
  if(RuleD1 === 2){
    cost_track1 = Size1*TrackCost
  }else if(RuleD1 === 1){
    cost_track1 = TrackCost
  }else{
    cost_track1 = 0
  }
  if(RuleD2 === 2){
    cost_track2 = Size2*TrackCost
  }else if(RuleD2 === 1){
    cost_track2 = TrackCost
  }else{
    cost_track2 = 0
  }

  let costM1;
  let costM2;
  let costM3;
  let cost_decomp;

  // Cost to staff
  if(Exp1 === Pro){
    costM1 = ProCost*Work1*Size1
  }else if(Exp1 === Am){
    costM1 = AmCost + AmCost*Size1*0.1 //Pay for tournament, not work or size// change back to 0.1 for baseline
  }else if(Exp1 === Spec){
    costM1 = SpecCost*Work1+((Size1-1)*SpecCost*0.1) // parantesis term for bidding cost
  }else{
    costM1 = 0
  }
  if(Exp2 === Pro){
    costM2 = ProCost*Size2*Work2
  }else if(Exp2 === Am){
    costM2 = AmCost + AmCost*Size2*0.1 // change back to 0.1 for baseline
  }else if(Exp2 === Spec) {
    costM2 = SpecCost*Work2+((Size2-1)*SpecCost*0.1) // parantesis term for bidding cost
  }else{
    costM2 = 0
  }
  if(Exp3 === Pro){
    costM3 = ProCost*Size3*Work1
  }else if(Exp3 === Am){
    costM3 = AmCost + AmCost*Size3*0.1// change back to 0.1 for baseline
  }else if(Exp3 === Spec) {
    costM3 = SpecCost*Work3+((Size3-1)*SpecCost*0.1)
  }else{
    costM3 = 0
  }
  // Cost to decompose
  if(Modules === 1){
    cost_decomp = 0
  }else{
    cost_decomp = DecompCost
  }
  //  cost_decomp = (Modules-1)*DecompCost
  return(cost_decomp+costM1+costM2+costM3+cost_track1+cost_track2)
}

// Pro = 1
// Novice / Amateur = 2
// Specialist = 3

const Solver = [
  {}, // empty object to make indexing easier
  // Pro values 
  {
    Drive_mean: 250,
    Drive_sd: 15,
    Drive_Fairway_handoff: 40,
    LFairway_mean: 200,  
    LFairway_sd: 10,
    AFairway_sd_factor: 0.05, //accuracy is a function of distance
    AFairway_sd_sweetspot: 0.03, //accuracy is a function of distance
    Fairway_sweetspot: 75,
    Fairway_Putt_handoff: 15,
    Putt_prob: 0.80,
    Goodputt_sd_factor: 0.01,
    Badputt_min: 0,
    Badputt_max: 10,
  },
  // Novice values
  {
    Drive_mean: 150,
    Drive_sd: 30,
    Drive_Fairway_handoff: 40,
    LFairway_mean: 100,  
    LFairway_sd: 20,
    AFairway_sd_factor: 0.2, //accuracy is a function of distance
    AFairway_sd_sweetspot: 0.05, //accuracy is a function of distance
    Fairway_sweetspot: 0,
    Fairway_Putt_handoff: 15,
    Putt_prob: 0.20,
    Goodputt_sd_factor: 0.05,
    Badputt_min: 0,
    Badputt_max: 15,
  },
  // Specialist values
  {
    Drive_mean: 450,
    Drive_sd: 30,
    Drive_Fairway_handoff: 40,
    LFairway_mean: 100,  
    LFairway_sd: 20,
    AFairway_sd_factor: 0.2, //accuracy is a function of distance
    AFairway_sd_sweetspot: 0.05, //accuracy is a function of distance
    Fairway_sweetspot: 0,
    Fairway_Putt_handoff: 15,
    Putt_prob: 0.20,
    Goodputt_sd_factor: 0.05,
    Badputt_min: 0,
    Badputt_max: 15,
  }
]

// ////////////////////////////// Need to change how strategy is implemented, so that there's an advantage not a disadvantage
// // Long fairway is like driving just less effective.
// ////////////////////////////// Need to change how strategy is implemented, so that there's an advantage not a disadvantage
// // Long fairway is like driving just less effective.
const LFairway = (target, Expertise, strategy) => {
  
  //strategy = 0
  if(Expertise === 2 | Expertise === 3){
    strategy = 0} //// I placed it here because this function is cal
  const RemainingDistance = Math.min(Math.abs(target - 
    randomNormal({mean: Solver[Expertise]['LFairway_mean'], dev: Solver[Expertise]['LFairway_sd']})))
  //  if (RemainingDistance>0.001 && RemainingDistance<Offset){
  //    RemainingDistance=1
  //  }
  //  if (RemainingDistance<=Sink){RemainingDistance = Sink+0.1} ////// 
  return(RemainingDistance)
}

// // Aiming fairway aims near the pin. If Experts are in their sweetspot, they're really accurate. Novices don't have a sweet spot.
const AFairway = (target, Expertise, n=1, strategy) => {

  //strategy = 0
  if(Expertise === 2 | Expertise === 3){
    strategy = 0} //// I placed it here because this function is cal
  let RemainingDistance;
  if (strategy === 1 && Expertise === 1){ //If it's a subproblem with strategy and you're an expert you can aim
    RemainingDistance = Math.min(Math.abs(target - 
      randomNormal({mean: (target-0.5*Offset), dev: target*Solver[Expertise]['AFairway_sd_sweetspot']})))
  } 
  
  if ((strategy === 0 && Expertise === 1) || Expertise === 2 || Expertise === 3) {
    RemainingDistance = Math.min(Math.abs(target -
      randomNormal({mean: (target-Offset), dev: target*Solver[Expertise]['AFairway_sd_factor']})))
  }
  //      if (RemainingDistance>0.001 && RemainingDistance<Offset){
  //        RemainingDistance=1
  //      }
  //  if (RemainingDistance<=Sink){RemainingDistance = Sink+0.1}
  return(RemainingDistance)
}

// // Edited from earlier: add strategy to driving. Only drive if you control the next one.

const Drive = (target,Expertise, strategy, n=1) => {
  
  //strategy = 0 // 9/14/20 bi boyle dene olmazsa buraya   if(Expertise==1) fln tarzi bir gate koy
  if(Expertise === 2 | Expertise === 3){
    strategy = 0} //// I placed it here because this function is cal //// I placed it here because this function is called by all architectures and this was the only way to decouple the strategy.
  let RemainingDistance;
  if(Solver[Expertise]['Drive_mean'] > target){
    RemainingDistance = LFairway(target,Expertise,strategy)
  }else{
    RemainingDistance = Math.min(Math.abs(target - 
      randomNormal({mean: Solver[Expertise]['Drive_mean'], dev: Solver[Expertise]['Drive_sd']})))
  }
  return(RemainingDistance)
}


//This is trying to sink (I'm not currently implementing a putting strategy for experts)
const Putt = (target, Expertise) => {
  //I want it to randomly flip between good shots and bad shots, so I'm a random number from 0 to 1 is bigger than the flip condition..
  let RemainingDistance;
  if (Math.random() < Solver[Expertise]['Putt_prob']) {
    RemainingDistance = Math.min(Math.abs(target -  randomNormal({mean: target, dev: target*Solver[Expertise]['Goodputt_sd_factor']})))
  } else {
    RemainingDistance = Math.min(Math.abs(target - 
      (Math.random() * (Solver[Expertise]['Badputt_max'] - Solver[Expertise]['Badputt_min']) + Solver[Expertise]['Badputt_min'])))
  }
  return(RemainingDistance)
}

// ////////////////////////////////////////////////////
// // Now I'm creating functions to run subproblems within golf. Eventually  I will want to be able to call every version of subproblem to make a whole hole.

// ////////////////////////////////////////////////////////////////////////////////////////
// // This function putts until you sink
// // This has been revised since 2.4 to also track the solving PathTaken.
// // It now returns a vector of the path taken, with the last element being the number of strokes. 
// // It only returns the best path for the N tries.

//* @post /playPutt
//* @param BallNow initial distance to hole
//* @param Expertise chosen solver
//* @param N Quantity of solver 
//* @param size distance to accept ball in hole, 0.5
export const PlayPutt = (BallNow, Expertise, N, size) => {
  
  let Start = BallNow
  let PathTaken = Array(1+ sizeP).fill(100); // Come back!!! This needs a variable name.
  for (let i = 1; i <= N; i++) {
    let PathTakenAlt = Array(1+ sizeP).fill(0.1);
    let NumStrokes = 0
    //   BallNow = Start //// We think This fixes the model by forcing everyone to take at least one put 10/5 
    BallNow = Putt(Start,Expertise) 
    NumStrokes = 1
    while (Math.abs(BallNow) > size){
      BallNow =  Putt(BallNow,Expertise)
      NumStrokes = NumStrokes + 1
      //cat ("when i is", "NumStrokes is", NumStrokes, "and j is", j, "\n")
      if(NumStrokes > 14){ // Mercy rule
        BallNow = 0
        NumStrokes = NumStrokes +1
      }
      PathTakenAlt[NumStrokes] = BallNow //continuing to write the PathTaken to this vector
    }
    PathTakenAlt[sizeP-1] = NumStrokes
    PathTakenAlt[sizeP] = BallNow
    //cat("For i = ",i,"PathTaken is", PathTaken, "and PathTakenAlt is", PathTakenAlt)
    
    // Best is defined as fewest strokes to sink
    if(PathTakenAlt[sizeP-1]<PathTaken[sizeP-1]){
      PathTaken = PathTakenAlt
    }
  }
  return(PathTaken)
}


////////////////////////////////////////////////////////////////////////
// This is the fairway function. It starts with a handoff from the drive and takes it until it first crosses the green transition     //
////////////////////////////////////////////////////////////////////////

//* @post /playFairway
//* @param BallNow initial distance to hole
//* @param Expertise chosen solver
//* @param N Quantity of solver 
//* @param rule value of 1 tries to get closest to hole, value of 2 tries to minimize strokes. default 1
//* @param strategy default 0
export const PlayFairway = (BallNow,Expertise,N,rule,strategy) => {
  //strategy = 0 // removing strategy
  let Start = BallNow
  let PathTaken = Array(1+ sizeF).fill(1000)
  for (let i = 1; i <= N; i++) {
    let PathTakenAlt = Array(1+ sizeF).fill(0.1);
    let NumStrokes = 0
    BallNow = Start
    while (BallNow > Solver[Expertise]['Fairway_Putt_handoff']){ 
      if (BallNow > (Solver[Expertise]['LFairway_mean'])){
        BallNow = LFairway(BallNow,Expertise,strategy)
      } else {
        BallNow = AFairway(BallNow,Expertise,1,strategy)
      }
      NumStrokes = NumStrokes + 1
      PathTakenAlt[NumStrokes] = BallNow //continuing to write the PathTaken to this vector
    }
    PathTakenAlt[sizeF-1] = NumStrokes
    PathTakenAlt[sizeF] = BallNow
    //cat("For i = ",i,"PathTaken is", PathTaken, "and PathTakenAlt is", PathTakenAlt)
    // For rule 1, best is defined as the closest ball (ignores strokes)
    if(PathTakenAlt[sizeF] < PathTaken[sizeF] && rule === 1){
      PathTaken = PathTakenAlt
    }
    // For rule 2, best is defined as the one with the fewest strokes, that is closest to the hole.
    if(PathTakenAlt[sizeF-1] < PathTaken[sizeF-1] && rule === 2){
      PathTaken = PathTakenAlt
    } 
    if((PathTakenAlt[sizeF-1] === PathTaken[sizeF-1]) && (PathTakenAlt[sizeF]<PathTaken[sizeF]) && rule === 2){
      PathTaken = PathTakenAlt
    }
    //cat( "So we keep", PathTaken,"\n")
  }
  return(PathTaken)
}


//////////////////////////////////////////////////////////////////////////
// This function drives until you cross the fairway transition //
//////////////////////////////////////////////////////////////////////////

//* @post /playDrive
//* @param HoleDist initial distance to hole
//* @param Expertise chosen solver
//* @param N Quantity of solver 
//* @param rule
//* @param strategy
// MATCHES OUTPUT OF R FUNCTION (indexes shifted by 1, but all access shifted by 1 as well)
export const PlayDrive = (HoleDist,Expertise,N,rule,strategy) => {
  
  //strategy = 0 //removing strategy
  let Start = HoleDist
  let PathTaken = Array(1+ sizeD).fill(1000);
  for (let i = 1; i <= N; i++){
    let PathTakenAlt = Array(1+ sizeD).fill(0.1);
    let NumStrokes = 1
    let BallNow = Start
    PathTakenAlt[NumStrokes] = BallNow
    BallNow = Drive(HoleDist,Expertise,strategy)
    PathTakenAlt[NumStrokes+1] = BallNow
    PathTakenAlt[sizeD-1] = NumStrokes
    PathTakenAlt[sizeD] = BallNow
    //cat("For i = ",i,"PathTaken is", PathTaken, "and PathTakenAlt is", PathTakenAlt)
    if(PathTakenAlt[sizeD] < PathTaken[sizeD] && rule === 1){
      //cat(PathTakenAlt[sizeD],"vs.",PathTaken[sizeD])
      PathTaken = PathTakenAlt
    }
    // For rule 2, best is defined as the one with the fewest strokes, that is closest to the hole.
    if(PathTakenAlt[sizeD-1]<PathTaken[sizeD-1] && rule ===2){
      PathTaken = PathTakenAlt
    } 
    if((PathTakenAlt[sizeD-1] === PathTaken[sizeD-1])&&(PathTakenAlt[sizeD]<PathTaken[sizeD]) && rule === 2){
      PathTaken = PathTakenAlt
    }
    //cat( "So we keep", PathTaken,"\n")
  }
  return(PathTaken)
}


//////////////////////////////////////////////////////////////////////////
// This function starts from the tee and plays until the green transition
//////////////////////////////////////////////////////////////////////////

//* @post /playLong
//* @param BallNow initial distance to hole
//* @param Expertise chosen solver
//* @param N Quantity of solver 
//* @param rule
export const PlayLong = (BallNow,Expertise,N,rule) => {
  
  // removed strategy when it's executed - TT. I tried to put the strategy back, lets see if it works. 
  let PathTaken = Array(1+ sizeD + sizeF).fill(100) //allowing max strokes with an extra element for count 
  for (let i = 1; i <= N; i++){
    let PathTakenTemp = Array(1+ sizeD + sizeF).fill(0.1)
    let drive = PlayDrive(BallNow,Expertise,1,2,1)
    //cat("drive is:",drive,"\n")
    let NumStrokes = drive[sizeD-1]
    let CurrentBall = drive[sizeD]
    for (let copyIndex = 1; copyIndex <= NumStrokes + 1; copyIndex++){
      PathTakenTemp[copyIndex] = drive[copyIndex]
    }
    //fairway
    let fairway = PlayFairway(CurrentBall,Expertise,1,1,1) //no knowledge of what putter wants.
    //cat("fairway is:",fairway,"\n")
    let NumStrokesFairway = fairway[sizeF-1]
    let StrokesAfterFairway = NumStrokesFairway+NumStrokes
    CurrentBall = fairway[sizeF]
    for (let copyIndex = 1; copyIndex <= NumStrokesFairway; copyIndex++){
      PathTakenTemp[NumStrokes+1+copyIndex] = fairway[copyIndex]
    }
    PathTakenTemp[sizeD+sizeF-1] = StrokesAfterFairway
    PathTakenTemp[sizeD+sizeF] = fairway[sizeF]
    if(PathTakenTemp[sizeD+sizeF]<PathTaken[sizeD+sizeF] && rule === 1){
      PathTaken = PathTakenTemp
    }
    // For rule 2, best is defined as the one with the fewest strokes, that is closest to the hole.
    if(PathTakenTemp[sizeD+sizeF-1]<PathTaken[sizeD+sizeF-1] && rule===2){
      PathTaken = PathTakenTemp
    } 
    if((PathTakenTemp[sizeD+sizeF-1]===PathTaken[sizeD+sizeF-1])&&(PathTakenTemp[sizeD+sizeF]<PathTaken[sizeD+sizeF]) && rule === 2){
      PathTaken = PathTakenTemp
    }
    //cat("PathTaken is:",PathTaken,"\n")
  }
  return(PathTaken)
}

//////////////////////////////////////////////////////////////////////////
// This function starts from the fairway transition and plays until you sink
//////////////////////////////////////////////////////////////////////////

//* @post /playShort
//* @param BallNow distance ball starts at
//* @param Expertise chosen selector
//* @param N 
//* @param size acceptance distance for making hole, 0.5
export const PlayShort = (BallNow,Expertise,N,size) => {

  let PathTaken = Array(1+ sizeF+sizeP).fill(100000000); //allowing max strokes with an extra ele
  //strokes = rep(0,N)
  for (let i = 1; i <= N; i++){
    let PathTakenTemp = Array(1+ sizeF+sizeP).fill(0.1);
    PathTakenTemp[1] = BallNow
    //Fairway
    let fairway = PlayFairway(BallNow,Expertise,1,2,0) //strategy because you can set up your = too. //// the rule was =2 here so I fixed that
    //cat("fairway is:",fairway,"\n")
    let StrokesFairway = fairway[sizeF-1]
    let CurrentBall = fairway[sizeF]
    for (let copyIndex = 1; copyIndex <= StrokesFairway; copyIndex++){
      PathTakenTemp[copyIndex+1] = fairway[copyIndex]
    }
    //Putt
    let putt = PlayPutt(CurrentBall,Expertise,1,size)
    //cat("putt is:",putt,"\n")
    let NumStrokesPutt = putt[sizeP-1]
    let StrokesAfterPutt = StrokesFairway + NumStrokesPutt
    for (let copyIndex = 1; copyIndex <= NumStrokesPutt; copyIndex++){
      PathTakenTemp[StrokesFairway+1+copyIndex] = putt[copyIndex]
    }
    PathTakenTemp[sizeF+sizeP-1] = StrokesAfterPutt
    PathTakenTemp[sizeF+sizeP] = putt[sizeP]
    
    //No need for rules because we're playing to the hole.
    if(PathTakenTemp[sizeF+sizeP-1] < PathTaken[sizeF+sizeP-1]){
      PathTaken = PathTakenTemp
    }
    //cat("PathTaken is:",PathTaken,"\n")
  }
  // Returns a vector where the first stroke is the starting position.
  return(PathTaken)
}

//////////////////////////////////////////////////////////////////////////
// This function starts from the tee and plays until you sink
//////////////////////////////////////////////////////////////////////////
//!!!!!!Update to return a vector that's the best of N plays the whole hole

const PlayWholeHole = (BallNow,Expertise,N,size) => {
  
  const HoleLength = BallNow;
  let CurrentBall;
  let NumStrokes;
  let PathTaken = Array(1+ buffer).fill(1000000000); //allowing max strokes with an extra element for count 
  for (let i = 1; i <= N; i++){
    let PathTakenTemp = Array(1+ buffer).fill(0);
    //PathTakenTemp[1] = BallNow
    //drive
    if(BallNow===HoleLength){
      //////// This is what we added to prevent DS guys from driving one more time (Which we think was granting Ams an advantage)
      let drive = PlayDrive(BallNow,Expertise,1,2,1) //rule=2 because this is whole hole; strategy could be removed
      //cat("drive is:",drive,"\n")
      NumStrokes = drive[sizeD-1]
      CurrentBall = drive[sizeD]
      for (let copyIndex = 1; copyIndex <= NumStrokes + 1; copyIndex++){
        PathTakenTemp[copyIndex] = drive[copyIndex]
      }
    } else{CurrentBall = BallNow} //// this should  
    //fairway
    let fairway = PlayFairway(CurrentBall,Expertise,1,2,1)
    //cat("fairway is:",fairway,"\n")
    let NumStrokesFairway = fairway[sizeF-1]
    let StrokesAfterFairway = NumStrokesFairway+NumStrokes
    CurrentBall = fairway[sizeF]
    for (let copyIndex = 1; copyIndex <= NumStrokesFairway; copyIndex++){
      PathTakenTemp[NumStrokes+1+copyIndex] = fairway[copyIndex]
    }
    //putt
    let putt = PlayPutt(CurrentBall,Expertise,1,size)
    //cat("putt is:",putt,"\n")
    //cat("here's the putt:",putt,"\n")
    let NumStrokesPutt = putt[sizeP-1]
    if (NumStrokesPutt >0){
      for(let copyIndex = 1; copyIndex <= NumStrokesPutt; copyIndex++){
        PathTakenTemp[StrokesAfterFairway+1+copyIndex] = putt[copyIndex]
      }
    }
    PathTakenTemp[buffer-1] = StrokesAfterFairway + NumStrokesPutt
    PathTakenTemp[buffer] = putt[sizeP]
    // Pick the best
    //cat("PathTakenTemp is:",PathTakenTemp,"\n")
    if(PathTakenTemp[buffer-1]<PathTaken[buffer-1]){
      PathTaken = PathTakenTemp
    }
    //cat("PathTaken is:",PathTaken,"\n")
    
  }
  //cat("PathTaken is:",PathTaken,"\n")
  return(PathTaken)
}

// FOR ALL ARCHITECTURES RUNS IS ALWAYS 1 AND WILL THEREFORE BE ELIMINATED

////////////////////////////////////////////////////////
// This runs the whole hole (H)
////////////////////////////////////////////////////////
//Figour out what to change

// Pro is expertise 1, Amateur is 2, and specialist is 3

//* @post /h_arch
//* @param HoleLength vector of numbers
//* @param Expertise
//* @param TournamentSize 
//* @param Holes
//* @param runs
export const H_Arch = (HoleLength,Expertise,TournamentSize,Holes) => {
  
  // Since runs are eliminated don't need a 2D array, a simple array for results will suffice
  // TODO honestly could simply return total shots and cost to be simpler
  let Result = Array(1+ (buffer*Holes+head)).fill(0)
  //cat("results size =",size(Result),"\n")
    for (let j = 1; j <= Holes; j++){
      // print("on a hole")
      let start = (j-1)*buffer+head-1
      //cat(":",start)
      let end = start+buffer-1
      
      let wholeHoleResult = PlayWholeHole(HoleLength,Expertise,TournamentSize, Sink);
      // Result[start:end] = PlayWholeHole(HoleLength,Expertise,TournamentSize, Sink)
      let wholeHoleIndex = 0;
      for (let copyIndex = start; copyIndex <= end; copyIndex++){
        Result[copyIndex] = wholeHoleResult[wholeHoleIndex];
        wholeHoleIndex++;
      }
      Result[1] = Result[1] + wholeHoleResult[wholeHoleResult.length-2]
      Result[2] = Result[1]
      //Result[6] = Result[6] + max(Result[2:4])
    }
    
    Result[5] = Cost(1,Expertise,0,0,TournamentSize,0,0,Result[2]/Holes,Result[3]/Holes,Result[4]/Holes,0,0)

    Result[6] = Result[1]/Holes
    //print("Number of strokes is: ")
    //print(Result[1,i])
    //print("Strokes per hole: ")
    //print(Result[6,i])
    //print("Cost is: ")
    //print(Result[5,i])
    if(Expertise === 1){
      Result[7] = 1
    } else {
      Result[7] = 0
    }
  
  
  //PathTaken[TournamentSize*20+1] = Numstrokes
  // print(Result)
  return(Result)
}

// ////////////////////////////////////////////////////////
// // This runs the long + putt architecture. 
// //////////////////////////////////////////////////////
// // Rule 1 is practica, Rule 2 is optimal, Rule 3 is decoupled

// //* @post /lp_arch
// //* @param HoleLength
// //* @param Expertise_L 
// //* @param Expertise_P
// //* @param TournamentSize_L
// //* @param TournamentSize_P
// //* @param Rule
// //* @param Holes
// //* @param runs
export const LP_Arch = (HoleLength, Expertise_L, Expertise_P, TournamentSize_L, TournamentSize_P,Rule,Holes) => {

  let Result = Array(1 + (buffer*Holes+head)).fill(0)
  let BallNow;
  let LongStrokes;
  //NumStrokes = 0
  for (let j = 1; j <= Holes; j++){
      let Long = Array(sizeD+sizeF).fill(0);
      let Temp = Array(buffer).fill(0);
      if(Rule === 1){
        Long = PlayLong(HoleLength,Expertise_L,TournamentSize_L,1)
        LongStrokes = Long[sizeD+sizeF-1]
        BallNow = Long[sizeD+sizeF]
      } else if(Rule === 2){
        Long = PlayLong(HoleLength,Expertise_L,TournamentSize_L,2)
        LongStrokes = Long[sizeD+sizeF-1]
        BallNow = Long[sizeD+sizeF]
      } else {
        let NewTarget = HoleLength - GreenTransition
        // Find my issue!!!!!!!
        Temp = PlayWholeHole(NewTarget,Expertise_L,TournamentSize_L,zone)
        //cat("Temp is:",Temp,"\n")
        LongStrokes = Temp[buffer-1]
        //cat("strokes is:",LongStrokes,"\n")
        //////////////// does this need to be Temp[buffer]
        BallNow = Temp[buffer]+GreenTransition
        //cat("ball now is:",BallNow,"\n")
        Long = Temp + GreenTransition
        //cat("long is:",Long,"\n")
      }
      //cat("long is:",Long,"\n")
      let start = (j-1)*buffer+head+1
      let end = start+LongStrokes
      //cat("when j is",j,"start is",start,"and end is",end,"\n")
      //cat("longstrokes is:",LongStrokes)
      // Result[start:end] = Long[1:(LongStrokes+1)]
      let longIndex = 0;
      for (let copyIndex = start; copyIndex <= end; copyIndex++){
        Result[start+copyIndex] = Long[longIndex];
        longIndex++;
      }
      let Putt = PlayPutt(BallNow,Expertise_P,TournamentSize_P,Sink)
      //cat("putt is:",Putt,"\n")
      let PuttStrokes = Putt[sizeP-1]
      start = end
      end = start+PuttStrokes
      // Result[start:end] = Putt[1:(PuttStrokes+1)] // problem is here
      for (let copyIndex = 0; copyIndex <= PuttStrokes; copyIndex++){
        Result[start+copyIndex] = Putt[copyIndex];
      }

      Result[((j-1)*(buffer)+(buffer+head-1))] = LongStrokes+PuttStrokes
      Result[((j-1)*(buffer)+(buffer+head))] = Putt[sizeP]
      Result[1] = Result[1]+ Result[((j-1)*(buffer)+(buffer+head-1))]
      if(Rule === 3){
        Result[6] = Result[6] + Math.max(LongStrokes, PuttStrokes);// in steady state
        Result[7] = Result[6]
      } else {
        Result[6] = Result[6] + Math.max(LongStrokes, PuttStrokes); // in steady state
        Result[7] = Result[1]
      }
      Result[2] = Result[2] + LongStrokes
      Result[3] = Result[3] + PuttStrokes
      // Calculating fraction of work done by experts
      let Fraction = 0
      if (Expertise_L === 1){
        Fraction = LongStrokes
      }
      if (Expertise_P === 1){
        Fraction = Fraction + PuttStrokes
      }
      Result[7] = Fraction/(LongStrokes + PuttStrokes)
      //cat("result is:",Result,"\n")
    }
    Result[5] = Cost(2,Expertise_L,Expertise_P,0,TournamentSize_L,TournamentSize_P,0,Result[2]/Holes,Result[3]/Holes,Result[4]/Holes,Rule,0)
    Result[6] = Result[6]/Holes
    //Result[7,i] = Result[7,i]/Holes
    //print("Strokes: ")
    //print(Result[1,i])
    //print("Cost is: ")
    //print(Result[5, i])
  return(Result)
}


// //////////////////////////////////////////////////////
// // This is the drive-fairway-putt architecture (DAP)
// //////////////////////////////////////////////////////

// ////// Adopt fixe when use non PNAS - DAP need to swithch fairway to play from ballnow.

// //* @post /dap_arch
// //* @param HoleLength
// //* @param Expertise_D 
// //* @param Expertise_F
// //* @param Expertise_P
// //* @param TournamentSize_D
// //* @param TournamentSize_F
// //* @param TournamentSize_P
// //* @param Holes
// //* @param runs
export const DAP_Arch = (HoleLength,Expertise_D,Expertise_F,Expertise_P,TournamentSize_D,TournamentSize_F,TournamentSize_P,Holes) => {
  
  const RuleFP = 1;
  const RuleDF = 1;
  let Result = Array(1 + buffer*Holes+head).fill(0);
  for (let j = 1; j <= Holes; j++){
      // the D subproblem
      let drive = Array(sizeD).fill(0);
      let Temp = Array(buffer).fill(0);
      let DriveStrokes;
      let BallNow;
      if(RuleDF === 1){
        drive = PlayDrive(HoleLength,Expertise_D,TournamentSize_D,1,0)
        DriveStrokes = drive[sizeD-1]
        BallNow = drive[sizeD]
      } else if (RuleDF === 2){
        drive = PlayDrive(HoleLength,Expertise_D,TournamentSize_D,2,0)
        DriveStrokes = drive[sizeD-1]
        BallNow = drive[sizeD]
      } else {
        let NewTarget = HoleLength - FairwayTransition(HoleLength)
        Temp = PlayWholeHole(NewTarget,Expertise_D,TournamentSize_D,zone)
        DriveStrokes = Temp[buffer-1]
        BallNow = Temp[buffer]+FairwayTransition(HoleLength)
        drive = Temp + FairwayTransition(HoleLength)
      }
      let start = (j-1)*buffer+8
      let end = start+DriveStrokes
      // Result[start:end] = drive[1:(DriveStrokes+1)]
      for (let copyIndex = 0; copyIndex <= DriveStrokes; copyIndex++){
        Result[start+copyIndex] = drive[copyIndex];
      }
      ////// Approach
      let Approach = Array(sizeF).fill(0);
      Temp = Array(buffer).fill(0);
      let ApproachStrokes;
      if(RuleFP === 1){
        Approach = PlayFairway(BallNow,Expertise_F,TournamentSize_F,1,0) // I turned the strategy off here after speaking with Zoe
        ApproachStrokes = Approach[sizeF-1]
        BallNow = Approach[sizeF]
      } else if(RuleFP === 2){
        Approach = PlayFairway(BallNow,Expertise_F,TournamentSize_F,2,0) // I turned the strategy off here after speaking with Zoe
        ApproachStrokes = Approach[sizeF-1]
        BallNow = Approach[sizeF]
      } else {
        let NewTarget = BallNow - GreenTransition
        // Find my issue!!!!!!!
        Temp = PlayWholeHole(NewTarget,Expertise_F,TournamentSize_F,zone)
        //cat("Temp is:",Temp,"\n")
        ApproachStrokes = Temp[buffer-1]
        //////////////// does this need to be Temp[buffer]
        BallNow = Temp[buffer]+GreenTransition
        Approach = Temp + GreenTransition
      }
      start = end+1 // delete +1 if it doesn't solve anything.
      end = start+ApproachStrokes
      // Result[start:end] = Approach[1:(ApproachStrokes+1)]\
      for (let copyIndex = 0; copyIndex <= ApproachStrokes; copyIndex++){
        Result[start+copyIndex] = Approach[copyIndex];
      }
      //// This is where Putting stage begins
      // Putt = rep(0,sizeP)
      let Putt = PlayPutt(BallNow,Expertise_P,TournamentSize_P,Sink)
      let PuttStrokes = Putt[sizeP-1]
      start = end
      end = start+PuttStrokes
      // Result[start:end] = Putt[1:(PuttStrokes+1)]
      for (let copyIndex = 0; copyIndex <= PuttStrokes; copyIndex++){
        Result[start+copyIndex] = Putt[copyIndex];
      }
      Result[((j-1)*(buffer)+(buffer+head-1))] = DriveStrokes+ApproachStrokes+PuttStrokes
      Result[((j-1)*(buffer)+(buffer+head))] = Putt[sizeP]
      Result[1] = Result[1]+Result[((j-1)*(buffer)+(buffer+head-1))]
      Result[2] = Result[2] + DriveStrokes
      Result[3] = Result[3] + ApproachStrokes
      Result[4] = Result[4] + PuttStrokes
      if(RuleDF === 3 && RuleFP === 3){
        Result[6] = Result[6] + Math.max(DriveStrokes, ApproachStrokes, PuttStrokes);// in steady state
        //Result[7] = Result[6]
      } else if (RuleDF === 3){
        Result[6] = Result[6] + Math.max(DriveStrokes, ApproachStrokes, PuttStrokes);// in steady state
        //Result[7] = Result[6] + max(c(DriveStrokes,ApproachStrokes))+PuttStrokes
      } else if (RuleFP === 3){
        Result[6] = Result[6] + Math.max(DriveStrokes, ApproachStrokes, PuttStrokes);// in steady state
        //Result[7] = Result[6] + max(c(PuttStrokes,ApproachStrokes))+DriveStrokes
      } else {
        Result[6] = Result[6] + Math.max(DriveStrokes, ApproachStrokes, PuttStrokes);// in steady state
        //Result[7] = Result[1]
      }
      let Fraction = 0
      if (Expertise_D === 1){
        Fraction = DriveStrokes
      }
      if (Expertise_F === 1){
        Fraction = Fraction + ApproachStrokes
      }
      if (Expertise_P === 1){
        Fraction = Fraction + PuttStrokes
      }
      Result[7] = Fraction/(DriveStrokes + ApproachStrokes + PuttStrokes)
    }
    Result[5] = Cost(3,Expertise_D,Expertise_F,Expertise_P,TournamentSize_D,TournamentSize_F,TournamentSize_P,Result[2]/Holes,Result[3]/Holes,Result[4]/Holes,RuleDF,RuleFP)
    Result[6] = Result[6]/Holes
    //Result[7] = Result[7]/Holes
    //print("Strokes: ")
    //print(Result[1])
    //print("Cost is: ")
    //print(Result[5, i])
  return(Result)
}

// //////////////////////////////////////////////////////////
// // The runs Drive-Short (DS)
// //////////////////////////////////////////////////////////

// //* @post /ds_arch
// //* @param HoleLength
// //* @param Expertise_D 
// //* @param Expertise_S
// //* @param TournamentSize_D
// //* @param TournamentSize_S
// //* @param Rule
// //* @param Holes
// //* @param runs
export const DS_Arch = (HoleLength,Expertise_D,Expertise_S,TournamentSize_D,TournamentSize_S,Holes) => {
  
  const Rule = 1;
  let Result = Array(1 + buffer*Holes+7).fill(0);
  for (let j = 1; j <= Holes; j++){
      let drive = Array(sizeD).fill(0);
      let Temp = Array(buffer).fill(0); 
      let DriveStrokes;
      let BallNow;
      if(Rule === 1){
        drive = PlayDrive(HoleLength,Expertise_D,TournamentSize_D,1,0)
        DriveStrokes = drive[sizeD-1]
        BallNow = drive[sizeD]
      } else if (Rule === 2){
        drive = PlayDrive(HoleLength,Expertise_D,TournamentSize_D,2,0)
        DriveStrokes = drive[sizeD-1]
        BallNow = drive[sizeD]
      } else {
        let NewTarget = HoleLength - FairwayTransition(HoleLength)
        Temp = PlayWholeHole(NewTarget,Expertise_D,TournamentSize_D,zone)
        DriveStrokes = Temp[buffer-1]
        BallNow = Temp[buffer]+FairwayTransition(HoleLength)
        drive = Temp + FairwayTransition(HoleLength)
      }
      let start = (j-1)*buffer+8
      let end = start+DriveStrokes
      // Result[start:end] = drive[1:(DriveStrokes+1)]
      for (let copyIndex = 0; copyIndex <= DriveStrokes; copyIndex++){
        Result[start+copyIndex] = drive[copyIndex];
      }
      // let short = Array(sizeF+sizeP).fill(0);
      let short = PlayShort(BallNow,Expertise_S,TournamentSize_S,Sink)
      let ShortStrokes = short[sizeF+sizeP-1] 
      BallNow = short[sizeF+sizeP]
      start = end
      end = start+ ShortStrokes
      // Result[start:end,i] = short[1:(ShortStrokes+1)]
      for (let copyIndex = 0; copyIndex <= ShortStrokes; copyIndex++){
        Result[start+copyIndex] = short[copyIndex];
      }
      Result[((j-1)*buffer+buffer+6)] = DriveStrokes+ShortStrokes
      Result[((j-1)*buffer+buffer+7)] = BallNow
      Result[1] = Result[1]+Result[((j-1)*buffer+buffer+6)]
      Result[2] = Result[2] + DriveStrokes
      Result[3] = Result[3] + ShortStrokes
      if(Rule === 3){
        Result[6] = Result[6] + Math.max(DriveStrokes, ShortStrokes);// in steady state
        //Result[7] = Result[6] //for a given hole
      } else {
        Result[6] = Result[6] + Math.max(DriveStrokes, ShortStrokes);// in steady state
        //Result[7] = Result[1]
      }
      let Fraction = 0
      if (Expertise_D === 1){
        Fraction = DriveStrokes
      }
      if (Expertise_S === 1){
        Fraction = Fraction + ShortStrokes
      }
      Result[7] = Fraction/(DriveStrokes + ShortStrokes)
    }
    Result[5] = Cost(2,Expertise_D,Expertise_S,0,TournamentSize_D,TournamentSize_S,0,Result[2]/Holes,Result[3]/Holes,Result[4]/Holes,Rule,0)
    Result[6] = Result[6]/Holes
    //Result[7] = Result[7]/Holes
  return(Result)
}

