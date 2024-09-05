Facebook粉絲團分析（分析專頁名稱為朱立倫的FB粉絲專頁）
================

分析朱立倫臉書粉絲專業，資料分析區間為2016/01/01-2016/4/9。

``` r
if (!require('Rfacebook')){
    install.packages("Rfacebook")
    library(Rfacebook)
}
```

    ## Loading required package: Rfacebook

    ## Warning: package 'Rfacebook' was built under R version 3.2.4

    ## Loading required package: httr

    ## Warning: package 'httr' was built under R version 3.2.4

    ## Loading required package: rjson

    ## Loading required package: httpuv

    ## Warning: package 'httpuv' was built under R version 3.2.4

    ## 
    ## Attaching package: 'Rfacebook'

    ## The following object is masked from 'package:methods':
    ## 
    ##     getGroup

``` r
token<-"CAACEdEose0cBAOW9QjjEMGHGp54JEGytGz9tPRltLiHRGJsHae0dSEHxlVmogqJ4hd8EoUQvB3yvDWFDpnHX8dfA2AIgelMfXQ7jZCrQbeaTEIWfjZCWoVZCzZB3cqrq8HFwr73qD1eaiNZAZCmFq0iZCi6kWFHn6Dh8gPFkRLMqvLhl17CuvbpixEO6k0ZBr1FcXq9L9pmFvL3JeX1ZCIBg8"

FBData=GET(

paste0(
 "https://graph.facebook.com/v2.5/llchu?fields=posts.limit(10)%7Blikes%2Ccomments%2Cmessage%7D&access_token=CAACEdEose0cBAPZBON6vvVgStfZA2c0kEXMFu0V4YygJdxAdIHDPHM2JZBZAcZBz472chZB2wlOHdwVynbSkBeBpVLCMaeP1TqRuNiZBEdeQ8ywu1V8aN8ZA57P8vmYByGvfLPxG4dhJkaX8WQhXS3yLk8uditasXrBsCZCou0ofxb0jeKMMdZCoIHjAdHdtwwUVidhe3sMQWQJPazZC5mKsJyn",

token))

names(FBData)
```

    ##  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    ##  [6] "content"     "date"        "times"       "request"     "handle"

讀取朱立倫粉絲團資料
--------------------

``` r
token<-'CAACEdEose0cBAPZBON6vvVgStfZA2c0kEXMFu0V4YygJdxAdIHDPHM2JZBZAcZBz472chZB2wlOHdwVynbSkBeBpVLCMaeP1TqRuNiZBEdeQ8ywu1V8aN8ZA57P8vmYByGvfLPxG4dhJkaX8WQhXS3yLk8uditasXrBsCZCou0ofxb0jeKMMdZCoIHjAdHdtwwUVidhe3sMQWQJPazZC5mKsJyn'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("llchu", token,
                      since = DateVectorStr[i],until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
```

    ## 14 posts 13 posts 25 posts 4 posts 5 posts 5 posts 7 posts 5 posts 1 posts 6 posts 5 posts 3 posts 4 posts 6 posts 4 posts 8 posts 5 posts 5 posts 4 posts

``` r
nrow(totalPage)
```

    ## [1] 129

從2016/01/01至2016/04/09，共有129篇貼文!

每日發文數分析:
---------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```

|     | dateTPE    |   id|
|-----|:-----------|----:|
| 12  | 2016-01-12 |    7|
| 13  | 2016-01-13 |    5|
| 14  | 2016-01-14 |    5|
| 15  | 2016-01-15 |    5|
| 65  | 2016-03-20 |    4|
| 1   | 2016-01-01 |    3|

在2016/01/12發文數最多，共有7篇，因時間近選舉日，都是呼籲民眾出來投票或是說台灣人真的很棒或是拜票文。

每日獲得讚數分析:
-----------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
LikeCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(LikeCount[order(LikeCount$likes_count,decreasing = T),]))
```

|     | dateTPE    |  likes\_count|
|-----|:-----------|-------------:|
| 16  | 2016-01-16 |       83386.0|
| 34  | 2016-02-06 |       57639.0|
| 9   | 2016-01-09 |       52729.5|
| 15  | 2016-01-15 |       49404.8|
| 17  | 2016-01-18 |       46132.0|
| 36  | 2016-02-08 |       41877.0|

在2016/01/16 獲得讚數最多，達8萬3千人次，因為是選舉日當天，所以大家都很關注總統候選人的各個消息。

每日評論數分析
--------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
CommentsCount<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(CommentsCount[order(CommentsCount$comments_count,decreasing = T),]))
```

|     | dateTPE    |  comments\_count|
|-----|:-----------|----------------:|
| 16  | 2016-01-16 |          10605.5|
| 15  | 2016-01-15 |           7843.8|
| 17  | 2016-01-18 |           3629.0|
| 9   | 2016-01-09 |           1883.0|
| 18  | 2016-01-19 |           1649.0|
| 34  | 2016-02-06 |           1377.0|

在2016/01/16獲得評論數最多，高達1萬多人次，因為當天為選舉日，所以大家意見都很多。

每日分享數分析
--------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
SharesCount<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(SharesCount[order(SharesCount$shares_count,decreasing = T),]))
```

|     | dateTPE    |  shares\_count|
|-----|:-----------|--------------:|
| 15  | 2016-01-15 |       2342.600|
| 1   | 2016-01-01 |       1521.000|
| 16  | 2016-01-16 |       1363.500|
| 34  | 2016-02-06 |       1265.000|
| 12  | 2016-01-12 |       1000.571|
| 9   | 2016-01-09 |        937.000|

在2016/01/15文章分享數最多，高達2千3百多人次，因為大選前一天，PO文皆屬於積極、正面、時事種類，可能獲得較多支持者認同並已分享。
