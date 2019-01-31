``` r
#devtools::install_github("jslefche/piecewiseSEM", build_vignette = TRUE)
devtools::install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE, force=TRUE)
```

    ## Downloading GitHub repo jslefche/piecewiseSEM@devel
    ## from URL https://api.github.com/repos/jslefche/piecewiseSEM/zipball/devel

    ## Installing piecewiseSEM

    ## "C:/Users/EM10/DOCUME~1/R/R-35~1.1/bin/x64/R" --no-site-file  \
    ##   --no-environ --no-save --no-restore --quiet CMD build  \
    ##   "C:\Users\EM10\AppData\Local\Temp\RtmpYPySCM\devtools2ca01c1a8ee\jslefche-piecewiseSEM-4c6fcb7"  \
    ##   --no-resave-data --no-manual

    ## 

    ## "C:/Users/EM10/DOCUME~1/R/R-35~1.1/bin/x64/R" --no-site-file  \
    ##   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
    ##   "C:/Users/EM10/AppData/Local/Temp/RtmpYPySCM/piecewiseSEM_2.1.0.tar.gz"  \
    ##   --library="C:/Users/EM10/Documents/R/R-3.5.1/library" --install-tests

    ## 

``` r
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

``` r
library(piecewiseSEM)
```

    ## 
    ##   This is piecewiseSEM version 2.1.0
    ## 
    ## 
    ##   If you have used the package before, it is strongly recommended you read Section 3 of the vignette('piecewiseSEM') to familiarize yourself with the new syntax
    ## 
    ##   Questions or bugs can be addressed to <LefcheckJ@si.edu>

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

    ## singular fit
    ## singular fit
    ## singular fit

Calls to summary yield warnings of a singular fit

``` r
summary(mildewSEM1)
```

    ## 
      |                                                                       
      |                                                                 |   0%

    ## singular fit

    ## 
      |                                                                       
      |=================================================================| 100%

    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit

    ## 
    ## Structural Equation Model of mildewSEM1 
    ## 
    ## Call:
    ##   mildew ~ miners + Lshootlength + Pshootlength
    ##   miners ~ Pshootlength
    ##   Lshootlength ~ miners
    ## 
    ##     AIC      BIC
    ##  32.484   74.167
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                      Independ.Claim Estimate Std.Error    DF Crit.Value
    ##   Lshootlength ~ Pshootlength + ...  -0.0283    0.0979 93.68    -0.2892
    ##   P.Value 
    ##    0.7851 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.484 with P-value = 0.785 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response    Predictor Estimate Std.Error     DF Crit.Value P.Value
    ##         mildew       miners  -0.0081    0.0118  86.64    -0.6850  0.5091
    ##         mildew Lshootlength  -0.0850    0.1119  95.09    -0.7601  0.4677
    ##         mildew Pshootlength  -0.1569    0.1080  92.37    -1.4525  0.1734
    ##         miners Pshootlength  -0.0532    0.2635 100.00    -0.2021  0.8399
    ##   Lshootlength       miners   0.0021    0.0108  93.03     0.1968  0.8490
    ##   Std.Estimate 
    ##        -0.0683 
    ##        -0.0762 
    ##        -0.1464 
    ##              - 
    ##           0.02 
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## Individual R-squared:
    ## 
    ##       Response   method Marginal Conditional
    ##         mildew     none     0.03        0.05
    ##         miners trigamma     0.00        0.33
    ##   Lshootlength     none     0.00        0.06

Create psem object, now with a glmer.nb() model as miner abundance follows a negative binomial distribution

``` r
mildewSEM1b <- psem(
  lmer(mildew ~ miners + Lshootlength + Pshootlength + (1|block/plot), dat), 
  glmer.nb(miners ~ Pshootlength + (1|block/plot), dat), 
  lmer(Lshootlength ~ miners + (1|block/plot), dat))
```

    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00192126 (tol =
    ## 0.001, component 1)

    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit

summary call with glmer.nb also runs

``` r
summary(mildewSEM1b)
```

    ## 
      |                                                                       
      |                                                                 |   0%

    ## singular fit

    ## 
      |                                                                       
      |=================================================================| 100%

    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit

    ## 
    ## Structural Equation Model of mildewSEM1b 
    ## 
    ## Call:
    ##   mildew ~ miners + Lshootlength + Pshootlength
    ##   miners ~ Pshootlength
    ##   Lshootlength ~ miners
    ## 
    ##     AIC      BIC
    ##  34.484   78.772
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                      Independ.Claim Estimate Std.Error    DF Crit.Value
    ##   Lshootlength ~ Pshootlength + ...  -0.0283    0.0979 93.68    -0.2892
    ##   P.Value 
    ##    0.7851 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.484 with P-value = 0.785 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response    Predictor Estimate Std.Error     DF Crit.Value P.Value
    ##         mildew       miners  -0.0081    0.0118  86.64    -0.6850  0.5091
    ##         mildew Lshootlength  -0.0850    0.1119  95.09    -0.7601  0.4677
    ##         mildew Pshootlength  -0.1569    0.1080  92.37    -1.4525  0.1734
    ##         miners Pshootlength   0.0524    0.4307 100.00     0.1217  0.9031
    ##   Lshootlength       miners   0.0021    0.0108  93.03     0.1968  0.8490
    ##   Std.Estimate 
    ##        -0.0683 
    ##        -0.0762 
    ##        -0.1464 
    ##              - 
    ##           0.02 
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## Individual R-squared:
    ## 
    ##       Response   method Marginal Conditional
    ##         mildew     none     0.03        0.05
    ##         miners trigamma     0.00        0.04
    ##   Lshootlength     none     0.00        0.06

Try again with lme instead of lmer and trying with a poisson GLMM (mildewSEM2) and a NB GLMM (mildewSEM3)

``` r
mildewSEM2 <- psem(
  lme(mildew ~ miners + Lshootlength + Pshootlength, random=~1|block/plot, dat), 
  glmer(miners ~ Pshootlength + (1|block/plot), dat, family="poisson"), 
  lme(Lshootlength ~ miners, random=~1|block/plot, dat))
```

    ## singular fit

``` r
summary(mildewSEM2)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ## singular fit

    ## 
    ## Structural Equation Model of mildewSEM2 
    ## 
    ## Call:
    ##   mildew ~ miners + Lshootlength + Pshootlength
    ##   miners ~ Pshootlength
    ##   Lshootlength ~ miners
    ## 
    ##     AIC      BIC
    ##  32.514   74.197
    ## 
    ## ---
    ## Tests of directed separation:
    ## 
    ##                      Independ.Claim Estimate Std.Error DF Crit.Value
    ##   Lshootlength ~ Pshootlength + ...  -0.0283    0.0979 73    -0.2892
    ##   P.Value 
    ##    0.7733 
    ## 
    ## Global goodness-of-fit:
    ## 
    ##   Fisher's C = 0.514 with P-value = 0.773 and on 2 degrees of freedom
    ## 
    ## ---
    ## Coefficients:
    ## 
    ##       Response    Predictor Estimate Std.Error  DF Crit.Value P.Value
    ##         mildew       miners  -0.0081    0.0118  72    -0.6850  0.4955
    ##         mildew Lshootlength  -0.0850    0.1119  72    -0.7601  0.4496
    ##         mildew Pshootlength  -0.1569    0.1080  72    -1.4525  0.1507
    ##         miners Pshootlength  -0.0532    0.2635 100    -0.2021  0.8399
    ##   Lshootlength       miners   0.0021    0.0108  74     0.1968  0.8446
    ##   Std.Estimate 
    ##        -0.0683 
    ##        -0.0762 
    ##        -0.1464 
    ##              - 
    ##           0.02 
    ## 
    ##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
    ## 
    ## Individual R-squared:
    ## 
    ##       Response   method Marginal Conditional
    ##         mildew     none     0.03        0.05
    ##         miners trigamma     0.00        0.33
    ##   Lshootlength     none     0.00        0.06

``` r
dTable <- dSep(mildewSEM2); fisherC(dTable)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ##   Fisher.C df P.Value
    ## 1    0.514  2   0.773

Replacing the poisson GLMM with a negative binomial GLMM... no summary output but Fisher statistic

``` r
mildewSEM3 <- list(
  lme(mildew ~ miners + Lshootlength + Pshootlength, random=~1|block/plot, dat), 
  glmer.nb(miners ~ Pshootlength + (1|block/plot), dat), 
  lme(Lshootlength ~ miners , random=~1|block/plot, dat))
```

    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
    ## $checkConv, : Model failed to converge with max|grad| = 0.00192126 (tol =
    ## 0.001, component 1)

    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit
    ## singular fit

``` r
summary(mildewSEM3)
```

    ##      Length Class    Mode
    ## [1,] 18     lme      list
    ## [2,]  1     glmerMod S4  
    ## [3,] 18     lme      list

``` r
dTable2 <- dSep(mildewSEM3); fisherC(dTable2)
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=================================================================| 100%

    ##   Fisher.C df P.Value
    ## 1    0.514  2   0.773

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 17763)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.1252 
    ## [2] LC_CTYPE=English_United Kingdom.1252   
    ## [3] LC_MONETARY=English_United Kingdom.1252
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] piecewiseSEM_2.1.0 nlme_3.1-137       lme4_1.1-19       
    ## [4] Matrix_1.2-15     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] zip_1.0.0         Rcpp_0.12.18      cellranger_1.1.0 
    ##  [4] pillar_1.3.0      compiler_3.5.1    nloptr_1.0.4     
    ##  [7] git2r_0.23.0      highr_0.7         forcats_0.3.0    
    ## [10] tools_3.5.1       digest_0.6.16     evaluate_0.11    
    ## [13] memoise_1.1.0     tibble_1.4.2      lattice_0.20-35  
    ## [16] pkgconfig_2.0.2   rlang_0.2.2       openxlsx_4.1.0   
    ## [19] parallel_3.5.1    curl_3.2          yaml_2.2.0       
    ## [22] haven_1.1.2       xfun_0.3          rio_0.5.10       
    ## [25] withr_2.1.2       httr_1.3.1        stringr_1.3.1    
    ## [28] knitr_1.21        devtools_1.13.6   hms_0.4.2        
    ## [31] grid_3.5.1        data.table_1.11.8 R6_2.2.2         
    ## [34] readxl_1.1.0      foreign_0.8-70    rmarkdown_1.11   
    ## [37] carData_3.0-2     minqa_1.2.4       car_3.0-2        
    ## [40] magrittr_1.5      htmltools_0.3.6   MASS_7.3-50      
    ## [43] splines_3.5.1     pbkrtest_0.4-7    abind_1.4-5      
    ## [46] stringi_1.1.7     crayon_1.3.4
