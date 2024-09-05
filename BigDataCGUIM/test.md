Untitled
================

O
-

``` r
if (!require('jsonlite')){
    install.packages("jsonlite")
    library(jsonlite)
}
if (!require('curl')){
    install.packages("curl")
    library(curl)
}
```

Graph API in R
--------------

``` r
if (!require('httr')){
    install.packages("httr")
    library(httr)
}
```

``` r
#put your token
token<-"CAACEdEose0cBAOW9QjjEMGHGp54JEGytGz9tPRltLiHRGJsHae0dSEHxlVmogqJ4hd8EoUQvB3yvDWFDpnHX8dfA2AIgelMfXQ7jZCrQbeaTEIWfjZCWoVZCzZB3cqrq8HFwr73qD1eaiNZAZCmFq0iZCi6kWFHn6Dh8gPFkRLMqvLhl17CuvbpixEO6k0ZBr1FcXq9L9pmFvL3JeX1ZCIBg8" #put your token
FBData = GET(paste0("https://graph.facebook.com/v2.5/tsaiingwen?fields=posts{message,likes.summary(true)}&access_token=",token))
names(FBData)
```

    ##  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    ##  [6] "content"     "date"        "times"       "request"     "handle"

Graph API in R
--------------

``` r
json1 = content(FBData)
json2 = jsonlite::fromJSON(toJSON(json1))
names(json2)
```

    ## [1] "posts" "id"

``` r
names(json2$posts)
```

    ## [1] "data"   "paging"

``` r
names(json2$posts$data)
```

    ## [1] "message" "id"      "likes"

Graph API in R
--------------

``` r
json2$posts$data$message[[1]]
```

    ## [1] "再多雨，總有放晴的一天。\n\n圖片版權：\nkuoyoung@flickr (https://goo.gl/UPIZ5r)\nCC BY-NC-ND 2.0 (https://goo.gl/cefU8)"

``` r
json2$posts$data$likes$summary$total_count[[1]]
```

    ## [1] 19404
