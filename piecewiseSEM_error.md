``` r
library(piecewiseSEM)
```

    ## 
    ##   This is piecewiseSEM version 2.0.2
    ## 
    ##   If you have used the package before, it is strongly recommended you read Section 3 of the vignette('piecewiseSEM') to familiarize yourself with the new syntax
    ## 
    ##   Questions or bugs can be addressed to <jlefcheck@bigelow.org>

``` r
# Load model packages
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(nlme)
```

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmList

Set up toy data

``` r
dat <- data.frame(mildew = runif(100), miners = rnbinom(100, 1,0.3), Lshootlength = runif(100), Pshootlength = runif(100),
                  block = rep(LETTERS[1:5], each=20), plot=rep(rep(LETTERS[22:26], each=4),5)) #, y3 = runif(50))
```

Create psem object

``` r
mildewSEM1 <- psem(
  lmer(mildew ~ miners + Lshootlength + Pshootlength + (1|block/plot), dat), 
  glmer(miners ~ Pshootlength + (1|block/plot), dat, family="poisson"), 
  lmer(Lshootlength ~ miners + (1|block/plot), dat))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00112995 (tol =
    ## 0.001, component 1)

Create psem object

``` r
mildewSEM1b <- psem(
  lmer(mildew ~ miners + Lshootlength + Pshootlength + (1|block/plot), dat), 
  glmer.nb(miners ~ Pshootlength + (1|block/plot), dat), 
  lmer(Lshootlength ~ miners + (1|block/plot), dat))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00112995 (tol =
    ## 0.001, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00174487 (tol =
    ## 0.001, component 1)

Calls to summary yield errors with glmer and glmer.nb

``` r
summary(mildewSEM1)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ## Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, : arguments imply differing number of rows: 4, 3

``` r
summary(mildewSEM1b)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ## Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, : arguments imply differing number of rows: 4, 3

Try again with lme

``` r
mildewSEM2 <- psem(
  lme(mildew ~ miners + Lshootlength + Pshootlength, random=~1|block/plot, dat), 
  glmer(miners ~ Pshootlength + (1|block/plot), dat, family="poisson"), 
  lme(Lshootlength ~ miners, random=~1|block/plot, dat))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00112995 (tol =
    ## 0.001, component 1)

``` r
summary(mildewSEM2)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ## 
    ## Structural Equation Model of mildewSEM2 
    ## 
    ## Call:
    ##   mildew ~ miners + Lshootlength + Pshootlength
    ##   miners ~ Pshootlength
    ##   Lshootlength ~ miners
    ## 
    ##     AIC      BIC
    ##  33.289   74.972
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                        Independ.Claim Estimate Std.Error DF Crit.Value
    ##   Lshootlength  ~  Pshootlength + ...   0.0637    0.0996 73      0.639
    ##   P.Value 
    ##    0.5248 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 1.289 with P-value = 0.525 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response    Predictor Estimate Std.Error  DF Crit.Value P.Value
    ##         mildew       miners   0.0055    0.0099  72     0.5524  0.5824
    ##         mildew Lshootlength  -0.0461    0.0963  72    -0.4794  0.6331
    ##         mildew Pshootlength   0.0071    0.0971  72     0.0735  0.9416
    ##         miners Pshootlength   0.4586    0.2540 100     1.8055  0.0710
    ##   Lshootlength       miners  -0.0176    0.0101  74    -1.7539  0.0836
    ##   Std.Estimate 
    ##         0.0572 
    ##        -0.0498 
    ##         0.0075 
    ##             NA 
    ##        -0.1710 
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## Individual R-squared:
    ## 
    ##       Response   method Marginal Conditional
    ##         mildew     none     0.01        0.02
    ##         miners trigamma     0.02        0.37
    ##   Lshootlength     none     0.03        0.15

``` r
dTable <- dSep(mildewSEM2)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

``` r
fisherC(dTable)
```

    ##   Fisher.C df P.Value
    ## 1    1.289  2   0.525

And with glmer.nb

``` r
mildewSEM3 <- psem(
  lme(mildew ~ miners + Lshootlength + Pshootlength, random=~1|block/plot, dat), 
  glmer.nb(miners ~ Pshootlength + (1|block/plot), dat), 
  lme(Lshootlength ~ miners , random=~1|block/plot, dat))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00112995 (tol =
    ## 0.001, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00174487 (tol =
    ## 0.001, component 1)

``` r
summary(mildewSEM3)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ## 
    ## Structural Equation Model of mildewSEM3 
    ## 
    ## Call:
    ##   mildew ~ miners + Lshootlength + Pshootlength
    ##   miners ~ Pshootlength
    ##   Lshootlength ~ miners
    ## 
    ##     AIC      BIC
    ##  35.289   79.577
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                        Independ.Claim Estimate Std.Error DF Crit.Value
    ##   Lshootlength  ~  Pshootlength + ...   0.0637    0.0996 73      0.639
    ##   P.Value 
    ##    0.5248 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 1.289 with P-value = 0.525 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response    Predictor Estimate Std.Error  DF Crit.Value P.Value
    ##         mildew       miners   0.0055    0.0099  72     0.5524  0.5824
    ##         mildew Lshootlength  -0.0461    0.0963  72    -0.4794  0.6331
    ##         mildew Pshootlength   0.0071    0.0971  72     0.0735  0.9416
    ##         miners Pshootlength   0.4267    0.4541 100     0.9397  0.3474
    ##   Lshootlength       miners  -0.0176    0.0101  74    -1.7539  0.0836
    ##   Std.Estimate 
    ##         0.0572 
    ##        -0.0498 
    ##         0.0075 
    ##             NA 
    ##        -0.1710 
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## Individual R-squared:
    ## 
    ##       Response   method Marginal Conditional
    ##         mildew     none     0.01        0.02
    ##         miners trigamma     0.00        0.00
    ##   Lshootlength     none     0.03        0.15
