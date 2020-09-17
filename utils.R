cleandf <- function(df){
  # recode all empty cells and na cells to true NA
  df[df==""] <- NA
  levels(df$BUSTED)[levels(df$BUSTED)=="na"] <- "N"
  levels(df$BppM1M2)[levels(df$BppM1M2)=="na"] <- "N"
  levels(df$BppM7M8)[levels(df$BppM7M8)=="na"] <- "N"
  levels(df$codemlM1M2)[levels(df$codemlM1M2)=="na"] <- "N"
  levels(df$codemlM7M8)[levels(df$codemlM7M8)=="na"] <- "N"
  
  # df$BUSTED[df$BUSTED=="na"] <- "N"
  # df$BppM1M2[df$BppM1M2=="na"] <- "N"
  # df$BppM7M8[df$BppM7M8=="na"] <- "N"
  # df$codemlM1M2[df$codemlM1M2=="na"] <- "N"
  # df$codemlM7M8[df$codemlM7M8=="na"] <- "N"
  df[df=="na"] <- NA

  # count methods saying Y and methods saying N
  df$nbY <- apply(df, 1, function(x) length(which(x=="Y")))
  df$nbN <- apply(df, 1, function(x) length(which(x=="N")))

  # calculate percentage of sites under positive selection
  cols <- grep("PSS", colnames(df))
  df$MEME.PSS[df$BUSTED == "N"] <- NA
  df$UniqueSites <- apply(df[, cols], 1, function(x) toString(na.omit(x)))
  df$UniqueSites <- gsub(", ", "-", df$UniqueSites)
  df$UniqueSites <- lapply(df$UniqueSites, function(x) unique(unlist(strsplit(x, split = "-"))))
  df$nbUniqueSites <- lapply(df$UniqueSites, function(x) length(x))

  # check if user has provided a RefSeqSize column and use it for percentage calculation
  if("RefSeqSize" %in% colnames(df) && is.numeric(df$RefSeqSize) && !is.na(df$RefSeqSize)){
    df$percentage <- as.numeric(df$nbUniqueSites)/df$RefSeqSize * 100
  } else {
    df$percentage <- as.numeric(df$nbUniqueSites)/df$GeneSize * 100
  }

  return(df)
}

# resFile = "/home/lea/Documents/DGINN-visualization/DGINN_202006110220summary_fig.csv"
# df  <- read.table(resFile, header = T, sep = "\t")
# res <- cleandf(df)

orderdf <- function(df){
  # order genes by number of methods detecting positive selection
  # then by percentage of sites (decreasing for both)
  orderMethods <- order(df$nbY, decreasing = TRUE)
  orderPercent <- order(df$percentage, decreasing = TRUE)
  geneOrder <- with(df, order(nbY, percentage, decreasing = TRUE))
  df <- df[geneOrder,]
  
  return(df)
}