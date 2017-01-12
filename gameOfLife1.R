##########################
## Code Retreat 2017
## Samuel Callisto
##
## 12 January 2017
##########################

library(caTools) # for creating animated gif
library(testthat)

world <- array(data=c(F,F,F,F,F,F,F,F,F), dim=c(3,3,3))

test_that("array",{
  expect_true(is.array(world))
})


test_that("3d matrix",{
  expect_equal(dim(world), c(3,3,3))
})

test_that("is empty",{
  expect_false(any(world))
})

applyRules <- function(state){
  nextState <- array(rep(F,16), dim=dim(state))
  for(i in 1:nrow(state)){
    for(j in 1:ncol(state)){
      nextState[i,j] <- applyRulesForCell(state,i,j)
    }
  }
  return(nextState)
}


gameOfLife <- function(currentState, steps){
  for(i in 1:steps){
    currentState <- applyRules(currentState)
  }
  return(currentState)
}

test_that("function",{
  expect_true(is.function(gameOfLife))
})


test_that("block of four state",{
  blockFour <- array(data=c(F,F,F,F,F,T,T,F,F,T,T,F,F,F,F,F), dim=c(4,4))
  expect_equal(gameOfLife(blockFour,0), blockFour)
})


applyRulesForCell <- function(state,i,j){
  neighbors <- state[i-1:i+1,j-1:j+1]   ## can't do this
  paste(neighbors)
  numAlive <- neighbors * 1
  paste(numAlive)
  return(numAlive == 2 | numAlive == 3)
}

test_that("Rule 1: 2-3 neighb == alive",{
  testState <- array(data=rep(T,16), dim=c(4,4))
  expect_true(applyRulesForCell(testState, 2,2))
})

test_that("Rule 2: >3 neighb == dead",{
  testState <- array(data=rep(T,16), dim=c(4,4))
  expect_false(applyRulesForCell(testState, 2,2))
})

