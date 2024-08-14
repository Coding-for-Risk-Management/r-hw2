# test-sample.R

library(testthat)
source("../../r-hw2.r")

test_that("MatMult2", {
    vec = matrix(data = 1:3,nrow = 1,ncol = 3)
    mat = matrix(data = c(1,4,7),nrow = 3,ncol = 1)

    x = MatMult2(vec,mat)
    # x[1] == 30 
    expect_equal(x[1], 30)

    functionText = capture.output(MatMult2)
    # if(length(grep("for",functionText))>1){
    #     cat("Only 1 'for' loop allowed!")
    # }
    expect_equal(length(grep("for",functionText)), 1)

    functionText = capture.output(MatMult2)
    # if(length(grep("%*%",functionText))>0){
    #     cat("No Cheating :)")
    # }
    expect_equal(length(grep("%*%",functionText)), 0)
})

test_that("MatMult1", {
    vec = matrix(data = 1:3,nrow = 1,ncol = 3)
    mat = matrix(data = 1:9,nrow = 3,ncol = 3,byrow = T)

    x = MatMult1(vec,mat)
    expect_equal(x[1], 30)
    expect_equal(x[2], 36)
    expect_equal(x[3], 42)

    functionText = capture.output(MatMult1)
    # if(length(grep("for",functionText))>1){
    #     cat("Only 1 'for' loop allowed!")
    # }
    expect_equal(length(grep("for",functionText)), 1)

    functionText = capture.output(MatMult1)
    # if(length(grep("%*%",functionText))>0){
    #     cat("No Cheating :)")
    # }
    expect_equal(length(grep("%*%",functionText)), 0)

})

test_that("TMT1", {
    rLast = rep(c('A','B','C'),c(3,4,3))
    rNow  = rep(c('A','B','C'),c(5,0,5))
    out= TMAT1(rLast, rNow)
    # Unit Tests
    out = TMAT1(rLast,rNow)
    answer = c(1,.5,0,0,0,0,0,.5,1)
    dim(answer) = c(3,3)
    # all.equal(out,answer)
    expect_equal(all.equal(out,answer), TRUE)

})

test_that("MatMult", {
    # Unit tests

    vec = matrix(data = c(20,30,10),nrow = 1,ncol = 3)
    mat = c(1,.5,0,0,0,0,0,.5,1)
    dim(mat) = c(3,3)

    x = MatMult(vec,mat)
    # x[1] == 35 & x[2] == 0 & x[3] == 25
    expect_equal(x[1], 35)
    expect_equal(x[2], 0)
    expect_equal(x[3], 25)

    functionText = capture.output(MyMatMult)
    # if(length(grep("for",functionText))!=1){
    #     cat("Only 1 'for' loop allowed!")
    # }
    expect_equal(length(grep("for",functionText)), 1)

})

test_that("getBondPrice", {
    y = 0.03
    face = 2000000
    couponRate = 0.04
    m = 10
    ppy = 2

    x = getBondPrice(y, face, couponRate, m,  1)
    # round(x) == 2170604
    expect_equal(round(x), 2170604)

    x = getBondPrice(y, face, couponRate, m,  2)
    # round(x) == 2171686
    expect_equal(round(x), 2171686)


})

test_that("getBondDuration", {
    y = 0.03
    face = 2000000
    couponRate = 0.04
    m = 10

    x = getBondDuration(y, face, couponRate, m)
    # round(x,2) == 8.51
    expect_equal(round(x,2), 8.51)

    functionText = capture.output(getBondDuration)
    # if(length(grep("for",functionText))){
    #     cat("No 'for' loops allowed!")
    # }
    expect_equal(length(grep("for",functionText)), 0)

})

test_that("getBondPriceYC", {
    y = .03
    face = 2000000
    couponRate = 0.04
    m = 10

    x1 = getBondPriceYC(y, face, couponRate, m)
    # round(x1) == 2170604
    expect_equal(round(x1), 2170604)

    x2 = getBondPriceYC(YC, face, couponRate, m)
    # round(x2) ==  1267138
    expect_equal(round(x2), 1267138)

    functionText = capture.output(getBondPriceYC)
    # if(
    #     length(grep("if",functionText))==0 &
    #     round(x1) == 2170604 &
    #     round(x2) ==  1267138
    # ){
    #     cat("Congraulations!\n")
    #     cat("You answered without using 'if'.\n")
    #     cat("You get an extra point in this assignment.")
    # }
    expect_equal(length(grep("if",functionText)), 0)
    expect_equal(round(x1), 2170604)
    expect_equal(round(x2), 1267138)


})

test_that("FizzBuzz", {
    x = FizzBuzz(40,45)
    # x[1] == "buzz" & x[2] == "41" & x[6] == "fizzbuzz"
    expect_equal(x[1], "buzz")
    expect_equal(x[2], "41")
    expect_equal(x[6], "fizzbuzz")

})


