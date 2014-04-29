---
title: "Logistic_Exposure"
author: "Zhenglei Gao"
date: "20-04-2014"
output: html_document
---

The following programs adapted from [USGS website](http://www.npwrc.usgs.gov/resource/birds/nestsurv/download/CreateLogisticExposureFamily.R) illustrate 1) logistic-exposure modeling of nest survival, 2) computation of AIC-based model-selection criteria, and 3) computation of model-averaged regression coefficients and unconditional standard errors. These programs accompany Shaffer (2004). 




```r
# Logistical Exposure Link Function See Shaffer, T.  2004. A unifying
# approach to analyzing nest success.  Auk 121(2): 526-540.
library(MASS)
logexp.2.15 <- function(days = 1) {
    linkfun <- function(mu) qlogis(mu^(1/days))
    linkinv <- function(eta) plogis(eta)^days
    mu.eta <- function(eta) days * plogis(eta)^(days - 1) * .Call("logit_mu_eta", 
        eta, PACKAGE = "stats")
    valideta <- function(eta) TRUE
    link <- paste("logexp(", days, ")", sep = "")
    structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, valideta = valideta, 
        name = link), class = "link-glm")
}
logexp <- function(exposure = 1) {
    linkfun <- function(mu) qlogis(mu^(1/exposure))
    ## FIXME: is there some trick we can play here to allow evaluation in the
    ## context of the 'data' argument?
    linkinv <- function(eta) plogis(eta)^exposure
    mu.eta <- function(eta) exposure * plogis(eta)^(exposure - 1) * .Call(stats:::C_logit_mu_eta, 
        eta, PACKAGE = "stats")
    valideta <- function(eta) TRUE
    link <- paste("logexp(", deparse(substitute(exposure)), ")", sep = "")
    structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, valideta = valideta, 
        name = link), class = "link-glm")
}
# Example using chat data from Shaffer(2004).
nestdata <- read.table("http://data.prbo.org/tools/NestSurvival/chat.txt")
chat.glm.logexp <- glm(survive/trials ~ parastat + nest_ht * patsize, family = binomial(logexp(exposure = nestdata$expos)), 
    data = nestdata)
## I would like to use step instead stepAIC, they basicall do the same thing.
chat.step <- step(chat.glm.logexp, scope = list(upper = ~parastat + nest_ht * 
    patsize, lower = ~1))
```

```
## Start:  AIC=203.9
## survive/trials ~ parastat + nest_ht * patsize
## 
##                   Df Deviance AIC
## - nest_ht:patsize  1      194 202
## <none>                    194 204
## - parastat         1      198 206
## 
## Step:  AIC=201.9
## survive/trials ~ parastat + nest_ht + patsize
## 
##                   Df Deviance AIC
## - nest_ht          1      194 200
## <none>                    194 202
## - parastat         1      198 204
## + nest_ht:patsize  1      194 204
## - patsize          1      199 205
## 
## Step:  AIC=200
## survive/trials ~ parastat + patsize
## 
##            Df Deviance AIC
## <none>             194 200
## + nest_ht   1      194 202
## - parastat  1      198 202
## - patsize   1      199 203
```

```r
chat.step$anova
```

```
##                Step Df Deviance Resid. Df Resid. Dev   AIC
## 1                   NA       NA       287      193.9 203.9
## 2 - nest_ht:patsize  1  0.01686       288      193.9 201.9
## 3         - nest_ht  1  0.07142       289      194.0 200.0
```

```r
summary(chat.step)
```

```
## 
## Call:
## glm(formula = survive/trials ~ parastat + patsize, family = binomial(logexp(exposure = nestdata$expos)), 
##     data = nestdata)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.997   0.212   0.361   0.499   1.444  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     2.747      0.352    7.80  6.2e-15 ***
## parastat        1.035      0.540    1.92    0.055 .  
## patsizesmall   -1.084      0.526   -2.06    0.039 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 175.38  on 291  degrees of freedom
## Residual deviance: 194.00  on 289  degrees of freedom
## AIC: 200
## 
## Number of Fisher Scoring iterations: 5
```



##Appendix


```r
nestdata
```

```
##     expos nest_ht parastat patsize survive trials
## 1     1.0    0.80        1   small       1      1
## 2     3.0    0.80        1   small       1      1
## 3     6.0    0.80        1   small       1      1
## 4     3.0    0.80        1   small       1      1
## 5     2.0    0.80        1   small       1      1
## 6     3.0    0.80        1   small       1      1
## 7     2.0    0.80        1   small       1      1
## 8     1.0    0.90        0   large       1      1
## 9     1.0    0.90        0   large       1      1
## 10    2.0    0.90        0   large       1      1
## 11    1.0    0.90        0   large       1      1
## 12    1.0    0.90        0   large       1      1
## 13    1.0    0.90        0   large       1      1
## 14    1.0    0.90        0   large       1      1
## 15    5.0    0.90        0   large       1      1
## 16    1.0    0.90        0   large       1      1
## 17    1.0    0.90        0   large       0      1
## 18    1.0    0.60        0   small       1      1
## 19    2.0    0.60        0   small       0      1
## 26    1.0    0.30        1   small       1      1
## 27    1.0    0.30        1   small       1      1
## 28    2.0    0.30        1   small       1      1
## 29    2.0    0.30        1   small       1      1
## 30    4.0    0.30        1   small       1      1
## 31    2.0    0.30        1   small       1      1
## 32    1.0    0.30        1   small       0      1
## 36    1.0    1.70        1   large       1      1
## 37    1.0    1.70        1   large       1      1
## 38    1.0    1.70        1   large       1      1
## 39    1.0    0.50        1   small       1      1
## 40    1.0    0.50        1   small       1      1
## 41    1.0    0.50        1   small       1      1
## 42    1.0    0.50        1   small       1      1
## 43    2.0    0.50        1   small       1      1
## 44    2.0    0.50        1   small       1      1
## 45    5.0    0.50        1   small       1      1
## 46    3.0    0.50        1   small       1      1
## 47    5.0    0.50        1   small       1      1
## 48    3.0    0.50        1   small       1      1
## 49    1.0    0.60        1   large       1      1
## 50    5.0    0.60        1   large       1      1
## 51    3.0    0.60        1   large       1      1
## 52    1.0    0.60        1   large       1      1
## 53    3.0    0.60        1   large       1      1
## 54    4.0    0.60        1   large       1      1
## 55    3.0    0.60        1   large       1      1
## 56    2.0    0.50        0   large       1      1
## 57    1.0    0.50        0   large       1      1
## 58    2.0    0.50        0   large       1      1
## 59    1.0    0.50        0   large       1      1
## 60    1.0    0.50        0   large       1      1
## 61    1.0    0.50        0   large       1      1
## 62    1.0    0.50        0   large       1      1
## 63    4.0    0.50        0   large       1      1
## 64    3.0    0.50        0   large       0      1
## 65    1.0    0.50        1   large       1      1
## 66    1.0    0.50        1   large       1      1
## 67    1.0    0.45        1   small       1      1
## 68    3.0    0.45        1   small       1      1
## 69    1.0    0.45        1   small       1      1
## 70    1.0    0.45        1   small       1      1
## 71    1.0    0.45        1   small       1      1
## 72    1.0    0.45        1   small       1      1
## 73    4.0    0.45        1   small       1      1
## 74    1.5    0.45        1   small       0      1
## 75    4.0    0.50        1   large       1      1
## 76    4.0    0.50        1   large       1      1
## 77    1.0    0.50        1   large       1      1
## 78    1.0    0.50        1   large       1      1
## 79    4.0    0.50        1   large       1      1
## 80    6.0    0.50        1   large       1      1
## 81    1.0    0.50        1   large       1      1
## 82    1.0    0.50        1   large       1      1
## 83    1.0    0.50        1   large       1      1
## 84    6.0    0.70        0   small       1      1
## 85    1.0    0.70        0   small       1      1
## 86    1.0    0.70        0   small       0      1
## 87    2.0    0.70        1   large       1      1
## 88    3.0    0.70        1   large       1      1
## 89    1.0    0.70        1   large       1      1
## 90    2.0    0.70        1   large       1      1
## 91    1.0    0.70        1   large       1      1
## 92    1.0    0.70        1   large       1      1
## 93    4.0    0.61        0   large       1      1
## 94    2.0    0.61        0   large       1      1
## 95    2.5    0.61        0   large       0      1
## 96    2.0    1.52        1   small       1      1
## 97    2.0    1.52        1   small       1      1
## 98    1.0    1.52        1   small       1      1
## 99    2.0    1.52        1   small       1      1
## 100   1.0    1.52        1   small       0      1
## 101   2.0    0.50        1   small       1      1
## 102   1.0    0.50        1   small       1      1
## 103   1.5    0.50        1   small       0      1
## 104   4.0    0.55        0   large       1      1
## 105   4.0    0.55        0   large       1      1
## 106   4.0    0.55        0   large       1      1
## 107   1.0    0.55        0   large       1      1
## 108   1.0    0.55        0   large       1      1
## 109   2.0    0.55        0   large       1      1
## 110   1.0    0.55        0   large       1      1
## 111   1.0    0.55        0   large       1      1
## 112   0.5    0.55        0   large       0      1
## 113   2.0    0.77        0   large       1      1
## 114   6.0    0.77        0   large       1      1
## 115   1.0    0.77        0   large       1      1
## 116   2.0    0.77        0   large       1      1
## 117   1.0    0.77        0   large       1      1
## 118   1.0    0.77        0   large       1      1
## 119   0.5    0.77        0   large       0      1
## 120   1.0    0.82        0   large       1      1
## 121   2.0    0.82        0   large       1      1
## 122   3.0    0.82        0   large       1      1
## 123   2.0    0.82        0   large       1      1
## 124   2.0    0.82        0   large       1      1
## 125   1.0    0.82        0   large       0      1
## 126   1.0    0.45        1   small       1      1
## 127   1.0    0.45        1   small       1      1
## 128   1.0    0.45        1   small       1      1
## 129   3.0    0.45        1   small       1      1
## 130   1.0    0.45        1   small       1      1
## 131   1.0    0.45        1   small       1      1
## 132   1.5    0.45        1   small       0      1
## 133   2.0    0.60        1   large       1      1
## 134   1.0    0.60        1   large       1      1
## 135   1.0    0.60        1   large       1      1
## 136   1.5    0.60        1   large       0      1
## 137   1.0    0.80        1   large       1      1
## 138   2.0    0.80        1   large       1      1
## 139   1.0    0.80        1   large       1      1
## 140   1.0    0.80        1   large       1      1
## 141   1.0    0.80        1   large       1      1
## 142   2.0    0.80        1   large       1      1
## 143   1.0    0.80        1   large       1      1
## 144   1.0    0.80        1   large       1      1
## 145   1.0    0.80        1   large       1      1
## 146   1.0    0.80        1   large       1      1
## 147   1.0    0.80        1   large       1      1
## 148   1.0    0.80        1   large       1      1
## 149   1.0    0.80        1   large       1      1
## 150   1.0    0.28        1   small       1      1
## 151   3.0    0.28        1   small       1      1
## 152   1.0    0.28        1   small       1      1
## 153   1.0    0.28        1   small       1      1
## 154   1.0    0.28        1   small       1      1
## 155   1.0    0.28        1   small       0      1
## 156   2.0    0.42        1   small       1      1
## 157   2.0    0.42        1   small       1      1
## 158   2.0    0.42        1   small       1      1
## 159   2.0    0.42        1   small       1      1
## 160   2.5    0.42        1   small       0      1
## 161   2.0    0.50        1   small       1      1
## 162   2.0    0.50        1   small       1      1
## 163   2.0    0.50        1   small       1      1
## 164   2.0    0.50        1   small       1      1
## 165   5.0    0.50        1   small       1      1
## 166   2.0    0.50        1   small       1      1
## 167   1.0    0.50        1   small       1      1
## 168   1.0    0.50        1   small       1      1
## 169   1.0    0.50        1   small       1      1
## 170   1.0    0.50        1   small       1      1
## 171   2.0    0.50        1   small       1      1
## 172   2.0    0.70        1   small       1      1
## 173   4.0    0.70        1   small       1      1
## 174   1.0    0.70        1   small       1      1
## 175   2.0    0.70        1   small       1      1
## 176   3.0    0.70        1   small       1      1
## 177   1.0    0.70        1   small       1      1
## 178   2.0    0.70        1   small       1      1
## 179   1.0    0.70        1   small       1      1
## 180   1.0    0.80        0   large       1      1
## 181   3.0    0.80        0   large       1      1
## 182   1.0    0.80        0   large       1      1
## 183   0.5    0.80        0   large       0      1
## 184   1.0    0.53        1   small       1      1
## 185   1.0    0.53        1   small       1      1
## 186   2.0    0.53        1   small       1      1
## 187   1.0    0.53        1   small       1      1
## 188   1.0    0.53        1   small       1      1
## 189   1.5    0.53        1   small       0      1
## 190   1.0    0.90        1   large       1      1
## 191   1.0    0.90        1   large       1      1
## 192   1.0    0.90        1   large       1      1
## 193   1.0    0.90        1   large       1      1
## 194   2.0    0.90        1   large       1      1
## 195   1.5    0.90        1   large       0      1
## 196   2.0    1.20        1   small       1      1
## 197   2.0    1.20        1   small       1      1
## 198   2.0    1.20        1   small       1      1
## 199   1.0    1.20        1   small       1      1
## 200   1.0    1.20        1   small       1      1
## 201   1.0    0.70        1   large       1      1
## 202   1.0    0.70        1   large       1      1
## 203   2.0    0.70        1   large       1      1
## 204   2.0    0.70        1   large       1      1
## 205   3.0    0.70        1   large       1      1
## 206   2.0    0.70        1   large       1      1
## 207   1.5    0.70        1   large       0      1
## 208   1.0    1.05        1   small       1      1
## 209   1.0    1.05        1   small       1      1
## 210   0.5    1.05        1   small       0      1
## 211   2.0    0.90        1   large       1      1
## 212   2.0    1.30        1   small       1      1
## 213   0.5    1.30        1   small       0      1
## 214   1.0    0.90        0   large       1      1
## 215   1.0    0.90        0   large       1      1
## 216   1.0    0.90        0   large       1      1
## 217   1.0    0.90        0   large       1      1
## 218   2.0    0.90        0   large       1      1
## 219   1.0    0.90        0   large       1      1
## 220   1.0    0.90        0   large       1      1
## 221   2.0    0.90        0   large       1      1
## 222   2.0    0.90        0   large       1      1
## 223   2.0    0.90        0   large       1      1
## 224   2.0    0.90        0   large       1      1
## 225   2.0    0.90        0   large       1      1
## 226   1.0    0.90        0   large       1      1
## 227   1.0    0.85        0   large       1      1
## 228   1.0    0.85        0   large       1      1
## 229   1.0    0.85        0   large       1      1
## 230   1.0    0.85        0   large       1      1
## 231   1.0    0.85        0   large       1      1
## 232   1.0    0.85        0   large       1      1
## 233   1.0    0.85        0   large       1      1
## 234   2.0    0.85        0   large       1      1
## 235   4.0    0.85        0   large       1      1
## 236   1.0    0.85        0   large       1      1
## 237   1.0    0.85        0   large       1      1
## 238   2.0    0.85        0   large       1      1
## 239   2.0    0.85        0   large       1      1
## 240   3.0    0.85        0   large       1      1
## 241   2.0    0.85        0   large       1      1
## 242   1.0    0.85        0   large       1      1
## 243   1.0    0.45        1   large       1      1
## 244   1.0    0.45        1   large       1      1
## 245   1.0    0.45        1   large       1      1
## 246   1.0    0.45        1   large       1      1
## 247   1.0    0.45        1   large       1      1
## 248   3.0    0.45        1   large       1      1
## 249   3.0    0.45        1   large       1      1
## 250   2.0    0.45        1   large       1      1
## 251   2.0    0.45        1   large       1      1
## 252   2.0    0.45        1   large       1      1
## 253   1.0    0.45        1   large       1      1
## 254   2.0    0.45        1   large       1      1
## 255   1.0    0.45        1   large       1      1
## 256   1.0    0.45        1   large       1      1
## 257   1.0    0.45        1   large       1      1
## 258   1.0    0.45        1   large       1      1
## 259   1.0    0.75        1   large       1      1
## 260   2.0    0.75        1   large       1      1
## 261   2.0    0.75        1   large       1      1
## 262   2.0    0.75        1   large       1      1
## 263   1.0    0.85        1   large       1      1
## 264   1.0    0.85        1   large       1      1
## 265   1.0    0.85        1   large       1      1
## 266   1.0    0.85        1   large       1      1
## 267   1.0    0.85        1   large       1      1
## 268   3.0    0.85        1   large       1      1
## 269   0.5    0.85        1   large       0      1
## 270   1.0    0.50        1   small       1      1
## 271   1.0    0.50        1   small       1      1
## 272   2.0    0.50        1   small       1      1
## 273   1.0    0.50        1   small       1      1
## 274   1.0    0.50        1   small       1      1
## 275   1.0    0.50        1   small       1      1
## 276   1.0    0.50        1   small       1      1
## 277   2.0    0.50        1   small       1      1
## 278   1.0    0.50        1   small       1      1
## 279   1.0    0.50        1   small       1      1
## 280   2.0    0.50        1   small       1      1
## 281   1.0    0.50        1   small       1      1
## 282   3.0    0.50        1   small       1      1
## 283   3.0    0.50        1   small       1      1
## 284   1.0    0.50        1   small       1      1
## 285   2.0    0.50        1   small       1      1
## 286   1.0    0.50        1   small       1      1
## 287   1.0    0.50        1   small       1      1
## 288   1.0    0.50        1   small       1      1
## 289   1.0    0.30        1   small       1      1
## 290   1.0    0.30        1   small       1      1
## 291   1.0    0.30        1   small       1      1
## 292   1.0    0.30        1   small       1      1
## 293   1.0    0.30        1   small       0      1
## 294   1.0    0.45        1   small       1      1
## 295   1.0    0.45        1   small       1      1
## 296   1.0    0.45        1   small       1      1
## 297   1.0    0.45        1   small       1      1
## 298   1.0    0.45        1   small       0      1
## 299   1.0    0.45        0   small       1      1
## 300   1.0    0.45        0   small       1      1
## 301   0.5    0.45        0   small       0      1
```


