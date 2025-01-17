cleandf <- function(df){
  # recode all empty cells and na cells to true NA
  print("start cleandf")
  df[df==""] <- NA
  print("check methods")
  levels(df$BUSTED)[levels(df$BUSTED)=="na"] <- "N"
  print("check BUSTED")
  levels(df$BppM1M2)[levels(df$BppM1M2)=="na"] <- "N"
  print("check BppM1M2")
  levels(df$BppM7M8)[levels(df$BppM7M8)=="na"] <- "N"
  print("check BppM7M8")
  levels(df$codemlM1M2)[levels(df$codemlM1M2)=="na"] <- "N"
  print("check odemlM1M2")
  levels(df$codemlM7M8)[levels(df$codemlM7M8)=="na"] <- "N"
  print("check odemlM7M8")
  # df$BUSTED[df$BUSTED=="na"] <- "N"
  # df$BppM1M2[df$BppM1M2=="na"] <- "N"
  # df$BppM7M8[df$BppM7M8=="na"] <- "N"
  # df$codemlM1M2[df$codemlM1M2=="na"] <- "N"
  # df$codemlM7M8[df$codemlM7M8=="na"] <- "N"
  df[df=="na"] <- NA
  print("row 18")
  # count methods saying Y and methods saying N
  df$nbY <- apply(df, 1, function(x) length(which(x=="Y")))
  print("row 21")
  df$nbN <- apply(df, 1, function(x) length(which(x=="N")))
  print("row 23")
  # calculate percentage of sites under positive selection
  cols <- grep("PSS", colnames(df))
  print("row 26")
  df$MEME.PSS[df$BUSTED == "N"] <- NA
  df$UniqueSites <- apply(df[, cols], 1, function(x) toString(na.omit(x)))
  df$UniqueSites <- gsub(", ", "-", df$UniqueSites)
  df$UniqueSites <- lapply(df$UniqueSites, function(x) unique(unlist(strsplit(x, split = "-"))))
  df$nbUniqueSites <- lapply(df$UniqueSites, function(x) length(x))
  print("row 32")
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