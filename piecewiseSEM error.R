#' --- 
#' title: Incompatible models in piecewiseSEM 2.0.2
#' author: Evalyne W Muiruri 
#' date: January 2019
#' output: html_document
#' #output: 
#'  #  md_document:
#'   #   variant: markdown_github
#' ---
#+ warning=FALSE
library(piecewiseSEM)
# Load model packages
library(lme4)
library(nlme)

#' Set up toy data 
dat <- data.frame(mildew = runif(100), miners = rnbinom(100, 1,0.3), Lshootlength = runif(100), Pshootlength = runif(100),
                  block = rep(LETTERS[1:5], each=20), plot=rep(rep(LETTERS[22:26], each=4),5)) #, y3 = runif(50))

#' Create psem object
mildewSEM1 <- psem(
  lmer(mildew ~ miners + Lshootlength + Pshootlength + (1|block/plot), dat), 
  glmer(miners ~ Pshootlength + (1|block/plot), dat, family="poisson"), 
  lmer(Lshootlength ~ miners + (1|block/plot), dat))



#' Create psem object
mildewSEM1b <- psem(
  lmer(mildew ~ miners + Lshootlength + Pshootlength + (1|block/plot), dat), 
  glmer.nb(miners ~ Pshootlength + (1|block/plot), dat), 
  lmer(Lshootlength ~ miners + (1|block/plot), dat))

#' Calls to summary yield errors with glmer and glmer.nb 
#+ error=TRUE 
summary(mildewSEM1)
summary(mildewSEM1b)

#' Try again with lme
mildewSEM2 <- psem(
  lme(mildew ~ miners + Lshootlength + Pshootlength, random=~1|block/plot, dat), 
  glmer(miners ~ Pshootlength + (1|block/plot), dat, family="poisson"), 
  lme(Lshootlength ~ miners, random=~1|block/plot, dat))
summary(mildewSEM2)
dTable <- dSep(mildewSEM2)
fisherC(dTable)

#' And with glmer.nb
mildewSEM3 <- psem(
  lme(mildew ~ miners + Lshootlength + Pshootlength, random=~1|block/plot, dat), 
  glmer.nb(miners ~ Pshootlength + (1|block/plot), dat), 
  lme(Lshootlength ~ miners , random=~1|block/plot, dat))
summary(mildewSEM3)


