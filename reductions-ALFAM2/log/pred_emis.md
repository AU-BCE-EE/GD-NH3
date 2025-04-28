---
title: 'ALFAM2 predictions'
output: pdf_document
date: "28 April, 2025 Apr:04"
---

Use conf.int = 'all' to get results from all 100 parameter sets to compare to reference scenario with uncertainty

Uncertainty preds, returning all for external quantiles.


``` r
alfam2pars03
```

```
##            int.f0    app.mthd.os.f0    app.mthd.cs.f0 man.source.pig.f0 
##        0.45305451       -2.89718049       -7.09642528       -0.95213804 
##         man.dm.f0            int.r1    app.mthd.bc.r1    app.mthd.ts.r1 
##        0.49956176       -1.45119862        0.73714111       -0.07393662 
##         man.dm.r1         man.ph.r1       air.temp.r1      wind.sqrt.r1 
##       -0.03300931        0.42121280        0.03321186        0.46104870 
##            int.r2      rain.rate.r2            int.r3    app.mthd.cs.r3 
##       -1.16953266        0.60163865       -2.68829766       -0.38439637 
##    incorp.deep.r3         man.ph.r3 incorp.shallow.f4    incorp.deep.f4 
##       -5.35112099        0.11776977       -1.41820869       -2.94966810 
##            int.r5      rain.rate.r5 
##       -1.80000000        0.48425409
```


``` r
pred <- alfam2(dat, time.incorp = 'time.incorp', conf.int = 'all', group = 'group', pass.col = c('sid', 'lwr.gd', 'upr.gd'))
```

```
## Default parameters (Set 3) are being used.
```

```
## Incorporation applied for groups: 08-di deep incorp quick cattle, 08-di deep incorp quick pig, 09-dii shallow incorp quick cattle, 09-dii shallow incorp quick pig, 10-diii deep incorp 4 h cattle, 10-diii deep incorp 4 h pig, 11-div deep incorp 24 h cattle, 11-div deep incorp 24 h pig.
```

``` r
setDT(pred)
```

Get overall prediction with parameter set 3 also.

``` r
predm <- alfam2(dat, time.incorp = 'time.incorp', group = 'group', pass.col = c('sid', 'lwr.gd', 'upr.gd'))
```

```
## Default parameters (Set 3) are being used.
```

```
## Incorporation applied for groups: 08-di deep incorp quick cattle, 08-di deep incorp quick pig, 09-dii shallow incorp quick cattle, 09-dii shallow incorp quick pig, 10-diii deep incorp 4 h cattle, 10-diii deep incorp 4 h pig, 11-div deep incorp 24 h cattle, 11-div deep incorp 24 h pig.
```

``` r
setDT(predm)
```


