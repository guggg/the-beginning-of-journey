乳癌/糖尿病 預測模型
================

資料前處理
----------

### 資料讀取

此資料來源為UCI Machine Learning Repository。

資料由威斯康星大學醫院 威斯康星州麥迪遜市Dr. Wolberg醫生提供，記載由各個醫療檢測紀錄相關，記錄到的各個探勘數值，一共有9個參數，依序有叢厚度、細胞均勻大小、細胞形狀、細胞邊際附著力、單上皮細胞尺寸、裸核、布蘭德染色質、普通核仁、有絲分裂情形。 主要由這些參數，對照紀錄，來判斷是否會得到乳癌的可能。 另外，分類結果為二元分類，良性(benign)及惡性(malignant)。

``` r
library(mlbench)
data(BreastCancer)
head(BreastCancer)
```

    ##        Id Cl.thickness Cell.size Cell.shape Marg.adhesion Epith.c.size
    ## 1 1000025            5         1          1             1            2
    ## 2 1002945            5         4          4             5            7
    ## 3 1015425            3         1          1             1            2
    ## 4 1016277            6         8          8             1            3
    ## 5 1017023            4         1          1             3            2
    ## 6 1017122            8        10         10             8            7
    ##   Bare.nuclei Bl.cromatin Normal.nucleoli Mitoses     Class
    ## 1           1           3               1       1    benign
    ## 2          10           3               2       1    benign
    ## 3           2           3               1       1    benign
    ## 4           4           3               7       1    benign
    ## 5           1           3               1       1    benign
    ## 6          10           9               7       1 malignant

### 處理資料

``` r
BreastCancerC<-BreastCancer[complete.cases(BreastCancer),
!names(BreastCancer)%in%c("Id")]
c(nrow(BreastCancer),nrow(BreastCancerC))
```

    ## [1] 699 683

### 分成訓練組跟測試組,並紀錄各組人數

隨機將4/5的資料分到訓練組（Test==F），剩下1/5為測試組（Test==T〕。

``` r
BreastCancerC$Test<-F
BreastCancerC[
sample(1:nrow(BreastCancerC),nrow(BreastCancerC)/5),]$Test<-T
c(sum(BreastCancerC$Test==F),sum(BreastCancerC$Test==T))
```

    ## [1] 547 136

可得訓練組案例數為547，測試組案例數為136。

預測模型建立
------------

### 模型建立

由於變數眾多，而輸出為二元類別變項，malignant及benign，所以選擇邏輯回歸演算法建立模型。

``` r
fit<-glm(Class~., BreastCancerC[BreastCancerC$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```

    ##                        Estimate Std. Error       z value  Pr(>|z|)
    ## (Intercept)       -264.57564496   62473.40 -4.235013e-03 0.9966210
    ## Cl.thickness.L     449.57980864   94176.20  4.773815e-03 0.9961911
    ## Cl.thickness.Q     135.23527435   43393.69  3.116474e-03 0.9975134
    ## Cl.thickness.C       8.32510500   52961.95  1.571903e-04 0.9998746
    ## Cl.thickness^4     -83.96169731   61249.90 -1.370805e-03 0.9989063
    ## Cl.thickness^5     173.01255883   36388.56  4.754587e-03 0.9962064
    ## Cl.thickness^6      71.11388916  108068.32  6.580456e-04 0.9994750
    ## Cl.thickness^7     240.69244839   45885.97  5.245448e-03 0.9958148
    ## Cl.thickness^8      40.09995717   24257.45  1.653099e-03 0.9986810
    ## Cl.thickness^9     -22.67482262   33028.47 -6.865236e-04 0.9994522
    ## Marg.adhesion.L    115.42378656  148370.99  7.779404e-04 0.9993793
    ## Marg.adhesion.Q     -0.02469369   77537.66 -3.184735e-07 0.9999997
    ## Marg.adhesion.C     79.84804809   71082.44  1.123316e-03 0.9991037
    ## Marg.adhesion^4    227.42518229  180552.95  1.259604e-03 0.9989950
    ## Marg.adhesion^5    305.20409185  192126.23  1.588560e-03 0.9987325
    ## Marg.adhesion^6    237.41475610  187061.32  1.269181e-03 0.9989873
    ## Marg.adhesion^7    113.12759974  134786.77  8.393079e-04 0.9993303
    ## Marg.adhesion^8     59.63603887   64980.16  9.177577e-04 0.9992677
    ## Marg.adhesion^9     42.63531865   42949.67  9.926809e-04 0.9992080
    ## Bare.nuclei2        19.85497193   72672.77  2.732106e-04 0.9997820
    ## Bare.nuclei3       194.40215635   21933.89  8.863094e-03 0.9929284
    ## Bare.nuclei4       330.90772619   67593.40  4.895563e-03 0.9960939
    ## Bare.nuclei5       215.23470399   28830.21  7.465597e-03 0.9940434
    ## Bare.nuclei6       480.78204801  130838.34  3.674627e-03 0.9970681
    ## Bare.nuclei7       241.85133383  110534.86  2.188010e-03 0.9982542
    ## Bare.nuclei8       321.17028408   67005.42  4.793199e-03 0.9961756
    ## Bare.nuclei9       295.70988620 1078183.96  2.742666e-04 0.9997812
    ## Bare.nuclei10      256.15408846   27258.21  9.397319e-03 0.9925021
    ## Bl.cromatin2       234.82447848   37432.34  6.273304e-03 0.9949947
    ## Bl.cromatin3       272.38813965   37432.16  7.276848e-03 0.9941940
    ## Bl.cromatin4       410.88747518   48386.31  8.491812e-03 0.9932246
    ## Bl.cromatin5       274.80488222   57415.18  4.786276e-03 0.9961811
    ## Bl.cromatin6       351.29894006 1082449.38  3.245408e-04 0.9997411
    ## Bl.cromatin7       264.60772027   42311.59  6.253788e-03 0.9950102
    ## Bl.cromatin8       187.08081354  160873.02  1.162910e-03 0.9990721
    ## Bl.cromatin9       199.66456038   97194.62  2.054276e-03 0.9983609
    ## Bl.cromatin10      466.50728624   83958.87  5.556379e-03 0.9955667
    ## Normal.nucleoli2  -196.36825008   66519.58 -2.952037e-03 0.9976446
    ## Normal.nucleoli3   -68.49672798   64985.83 -1.054026e-03 0.9991590
    ## Normal.nucleoli4  -231.14466291   25525.88 -9.055305e-03 0.9927750
    ## Normal.nucleoli5  -135.29372122  124813.14 -1.083970e-03 0.9991351
    ## Normal.nucleoli6  -138.47803636   15611.72 -8.870136e-03 0.9929227
    ## Normal.nucleoli7  -292.59336920   38046.41 -7.690433e-03 0.9938640
    ## Normal.nucleoli8  -199.84737865 1078010.72 -1.853853e-04 0.9998521
    ## Normal.nucleoli9   173.48647527  103362.71  1.678424e-03 0.9986608
    ## Normal.nucleoli10  137.07187203   73993.35  1.852489e-03 0.9985219

### 模型說明

由上述參數可知，使用多種參數檢測人體所得到醫療資料，以邏輯迴歸建立模型預測是否得到該病症，經最佳化後，模型使用參數為Cl.thickness.L, Cl.thickness.Q, Cl.thickness.C, Cl.thickness^4, Cl.thickness^5, Cl.thickness^6, Cl.thickness^7, Cl.thickness^8, Cl.thickness^9, Marg.adhesion.L, Marg.adhesion.Q, Marg.adhesion.C, Marg.adhesion^4, Marg.adhesion^5, Marg.adhesion^6, Marg.adhesion^7, Marg.adhesion^8, Marg.adhesion^9, Bare.nuclei2, Bare.nuclei3, Bare.nuclei4, Bare.nuclei5, Bare.nuclei6, Bare.nuclei7, Bare.nuclei8, Bare.nuclei9, Bare.nuclei10, Bl.cromatin2, Bl.cromatin3, Bl.cromatin4, Bl.cromatin5, Bl.cromatin6, Bl.cromatin7, Bl.cromatin8, Bl.cromatin9, Bl.cromatin10, Normal.nucleoli2, Normal.nucleoli3, Normal.nucleoli4, Normal.nucleoli5, Normal.nucleoli6, Normal.nucleoli7, Normal.nucleoli8, Normal.nucleoli9, Normal.nucleoli10，共46個參數，各參數代表從人體檢測出來的數據值。

``` r
MinePred<-predict(finalFit,newdata = BreastCancerC[BreastCancerC$Test==T,])
MineAns<-ifelse(MinePred<0.5,"benign","malignant") 
MineAns<-factor(MineAns,levels = c("benign","malignant"))
library(caret)
sensitivity(MineAns,BreastCancerC[BreastCancerC$Test==T,]$Class)
```

    ## [1] 0.9888889

``` r
specificity(MineAns,BreastCancerC[BreastCancerC$Test==T,]$Class)
```

    ## [1] 0.9782609

``` r
posPredValue(MineAns,BreastCancerC[BreastCancerC$Test==T,]$Class)
```

    ## [1] 0.9888889

``` r
negPredValue(MineAns,BreastCancerC[BreastCancerC$Test==T,]$Class)
```

    ## [1] 0.9782609

-   敏感度 98.8888889%
-   特異性 97.826087%
-   陽性預測率 98.8888889%
-   陰性預測率 97.826087%
