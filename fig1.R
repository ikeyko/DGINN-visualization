makeFig1 <- function(df){

  ################################### FIGURE 1 - RANKING
  
  # prepare data for colors etc
  colMethods <- c("deepskyblue4", "darkorange" , "deepskyblue3" , "mediumseagreen" , "yellow3" , "black")
  nameMethods <- c("BUSTED", "BppM1M2", "BppM7M8", "codemlM1M2", "codemlM7M8", "MEME")
  metColor <- data.frame(Name = nameMethods , Col = colMethods , stringsAsFactors = FALSE)
  
  # subset for this specific figure
  # df <- df[df$nbY >= 1, ] # to drop genes found by 0 methods (big datasets)
  xt <- df[, c("BUSTED", "BppM1M2", "BppM7M8", "codemlM1M2", "codemlM7M8")]
  row.names(xt) <- df$Gene
  nbrMeth <- 5
  # reverse order of dataframe so that genes with the most Y are at the bottom (to be on top of the barplot)
  xt = xt[order(nrow(xt):1),]
  xt <- ifelse(xt == "Y", 1, 0)
  
  colFig1 <- metColor[which(metColor$Name %in% colnames(xt)) , ]
  
  ##### PART 1 : NUMBER OF METHODS
  par(xpd = NA , mar=c(2,7,4,0) , mfrow  =c(1,2) ,   oma = c(0,0,0,0) , mgp = c(3,0.3,0))
  
  h =  barplot(
    t(xt),
    border = NA ,
    axes = F ,
    col = adjustcolor(colFig1$Col, alpha.f = 1),
    horiz = T ,
    las  = 2 ,
    main = "Methods detecting positive selection" , 
    cex.main = 0.85,
    cex.names  = min(50/nrow(xt), 1.5)
  )
  
  axis(3, line = 0, at = c(0:nbrMeth), label = c("0", rep("", nbrMeth -1), nbrMeth), tck = 0.02)
  
  legend("bottomleft",
         horiz = T,
         border = colFig1$Col,
         legend = colFig1$Name, 
         fill = colFig1$Col,
         cex = 0.8,
         bty = "n",
         xpd = NA
  )
  
  ##### PART 2: PERCENTAGES OF PSS
  par(xpd = NA, mar=c(2,2,4,2))
  MaxPerc <- round(max(df$percentage) + 1, 1)
  
  xx = barplot(
    rev(df$percentage)  ,
    names.arg = NULL ,
    las = 1,
    cex.names = 1,
    col = adjustcolor("blue4", alpha.f = 0.5),
    border = NA,
    horiz = T ,
    axes = F,
    main = "Percentage of positively selected sites",
    xlim = c(0, MaxPerc) ,
    cex.main = 0.85
  )

  axis(3, line = 0, labels =  c("0%" , paste0(MaxPerc,"%")), at = seq(0, MaxPerc, length.out = 2), las  = 1, tck = 0.02)

}
