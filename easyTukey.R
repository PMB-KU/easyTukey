requireLibs <- function(libs) {
  for(lib in libs){
    if(!require(lib, character.only = T)){
      install.packages(lib, repos="http://cran.ism.ac.jp/")
      require(lib, character.only = T)
    }
  }
}

requireLibs('multcomp')


tukeyFromFile <- function(path, sep="\t") {
    df <- read.csv(path, sep=sep, header=FALSE)
    df['condition'] <- c(1:length(rownames(df)))
    mergedDf <- df[c(colnames(df)[1], 'condition')]
    for (i in colnames(df)[1:length(colnames(df))-1]) {
        tmp <- df[c(i, "condition")]
        colnames(tmp) <- c(colnames(df)[1], "condition")
        mergedDf <- rbind(mergedDf, tmp)
    }
    colnames(mergedDf) <- c("value", "condition")
    aovData<-data.frame(group=factor(mergedDf$condition), data=mergedDf$value)
    aovResult<-aov(data~group,d=aovData)
    tukeyResult <- glht(aovResult ,linfct=mcp(group="Tukey"))
    print(summary(tukeyResult))
    print(cld(tukeyResult))
}


tukeyFromFiles <- function(path, pattern=".csv", sep=","){
    files <- list.files(path=path, pattern=pattern, full.names=T)
    for (file in files) {
        notice = paste0("--------------", file, "---------------", sep=" ")
        cat(notice)
        tukeyFromFile(path=file, sep=sep)
    }
}