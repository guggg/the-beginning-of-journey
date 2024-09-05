for(i in 1:nrow(NBA1415)){
    if(NBA1415[i,"GamesPlayed"]>70&
       NBA1415[i,"TotalPoints"]>1500)
        {
        print(NBA1415[i,c("Name","Team","Position")])
    }
}