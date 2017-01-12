world <- array(rep(0,16), dim=c(4,4))
blockworld <- array(c(0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0), dim=c(4,4))
lineworld <- array(c(0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0), dim=c(4,4))

evolve <- function(world){
  nextState <- world
  for(i in 1:nrow(world)){
    for(j in 1:ncol(world)){
      count <- countNeighbors(world,i,j)
      if(world[i,j] == 1){
        nextState[i,j] <- ifelse(count == 2 | count == 3, 1, 0)
      }
      if(world[i,j] == 0){
        nextState[i,j] <- ifelse(count == 3, 1, 0)
      }
    }
  }
  return(nextState)
}

## test countNeighbors

countNeighbors <- function(world, i, j){
  numNeighbors = 0
  # lower.x <- ifelse(i-1 < 1, 1, i-1)
  for(x in (i-1):(i+1)){
    for(y in (j-1):(j+1)){
      if((x > 0 & x <= nrow(world)) & ((y>0 & y <= ncol(world)))){
        #message(paste(x,y,sep=","))
        numNeighbors = numNeighbors + world[x,y]
      }
    }
  }
  numNeighbors = numNeighbors - world[i,j]
  return(numNeighbors)
}

world <- array(c(0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0), dim=c(4,4))

countNeighbors(blockworld, 2,2)
countNeighbors(lineworld, 3,3)

## test evolve

evolve(lineworld)

bigworld <- array(rep(c(0,1,0,0,1),20), dim=c(10,10))

outCome <- array(dim=c(10,10,10))
gameOfLife <- function(initial, steps){
  outCome[,,1] <- initial
  for(i in 2:steps){
    outCome[,,i] <- evolve(outCome[,,(i-1)])
  }
}

gameOfLife(bigworld, 10)
