################################### FIGURE 2 - PROFILES

# function to construct matrix of PSS with number of methods finding them then produce profile plot
profileMatrix <- function(gene, maxPos, maxDet, metColor){
  # create matrix and attribute positions of PSS as rownames
  methods <- grep("PSS", colnames(gene), value = TRUE)
  geneMeth <- matrix(data=NA, ncol = length(methods), nrow = gene$nbUniqueSites[[1]])
  colnames(geneMeth) <- methods
  rownames(geneMeth) <- gene$UniqueSites[[1]]
  geneMeth <- cbind(geneMeth, countFound = 0)
  # add empty row at position 0 in case only one site is found
  geneMeth <- rbind(geneMeth, "0" = NA)
  geneMeth <- geneMeth[order(as.numeric(row.names(geneMeth))),]
  
  # fill matrix
  firstMet = methods[1]
  for (MET in methods){
    pss <- gsub(", ", "-", gene[[MET]])
    pss <- unlist(strsplit(pss, split = "-"))
    
    # count number of times a position is found to assign proper value to the matrix
    geneMeth[, "countFound"][which(rownames(geneMeth) %in% pss)] <- geneMeth[, "countFound"][which(rownames(geneMeth) %in% pss)] + 1
    geneMeth[, MET][which(rownames(geneMeth) %in% pss)] <- geneMeth[, "countFound"][which(rownames(geneMeth) %in% pss)]
  }
  
  # drop counting column
  geneMeth <- geneMeth[, methods]
  drawProfile(geneMeth, gene$Gene, gene$GeneSize, gene$Depth, maxPos, maxDet, metColor)
}

# function to draw profile + coverage
drawProfile <- function(geneMeth, geneName, geneSize, geneDepth, maxPos, maxDet, metColor){
  colnames(geneMeth) <- c("MEME", "BppM1M2", "BppM7M8", "codemlM1M2", "codemlM7M8")
  
  for(CC in 1:ncol(geneMeth)){
    if(CC == 1){
      plot(0, 0, axes = F, xlim = c(0, maxPos), ylim = c(-1, maxDet), bty="n", xlab = "" , ylab ="", type ="n")
      mtext(geneName, side = 2, las = 1, line = 0, cex  = 1, at = 0.5)
    }
    
    X <- as.numeric(rownames(geneMeth)) # place vertical lines
    Y <- geneMeth[,CC]                  # height of vertical lines
    
    # color by methods
    colFig2 <- metColor[which(metColor$Name == colnames(geneMeth)[CC]), "Col"]
    segments(X , Y-1+0.1 , X , Y-0.2 , col = adjustcolor(colFig2, alpha.f = 1), lwd = 2) # vertical lines
  }
  
  if (!is.na(geneDepth)){
    drawarea(geneDepth)
  }
  segments(0 , -0.1 , geneSize , -0.1 , col = "black" , lwd = 1) #Trait horizontal. MARIE: déplacé pour passer sur le rectangle gris
  
}

# function to draw coverage
drawarea <- function(depth){
  depth <- strsplit(depth, ", ")
  depth <- as.numeric(unlist(depth))
  
  # Plot de la couverture (sous l'axe des abscisse)
  #x=1:trunc(length(depth)/3)
  #y=-depth[c(T,F,F)]*2
  x = 1:length(depth)
  y = -depth*2
  polygon(x = c(min(x), x, max(x)), y = c(0, y, 0), col =gray(0.8), border=NA)
}

makeFig2 <- function(df, nbGene, cov){
  # drop genes with no PSS found (not interesting to plot them)
  xt2 <- df[df$nbUniqueSites != 0, ]
  # keep only genes picked by three methods or more (large datasets)
  xt2 <- xt2[xt2$nbY >= 5, ]
  row.names(xt2) <- c(1:nrow(xt2))
  #xt2 <- xt2[-c(1,2,4,14), ]
  # get maximum length of gene to draw axis
  maxPos <- max(xt2$GeneSize)
  maxDet <- max(xt2$nbY)
  
  # prepare data for colors etc
  colMethods <- c("deepskyblue4", "darkorange" , "deepskyblue3" , "mediumseagreen" , "yellow3" , "black")
  nameMethods <- c("BUSTED", "BppM1M2", "BppM7M8", "codemlM1M2", "codemlM7M8", "MEME")
  metColor <- data.frame(Name = nameMethods , Col = colMethods , stringsAsFactors = FALSE)
  
  if(is.data.frame(cov)){
    # clean cov file
    colnames(cov)[1] <- "File"
    colnames(cov)[2] <- "Name"
    cov$Depth <- 0
    cov2 <- subset(cov, select = c(1, 2))
    colnames(cov2) <- c("File", "Depth")
    cov3 <- subset(cov, select = -c(File, Name, Depth))
    cov2$Depth <- apply(cov3, 1, function(x) toString(x[!is.na(x)]))
    # add depth to xt2
    xt2 <- merge(xt2, cov2, by.x="File", by.y = "File", all.x = TRUE)
  }
  else{
    xt2$Depth <- NA
  }
  
  orderMethods <- order(xt2$nbY, decreasing = TRUE)
  orderPercent <- order(xt2$percentage, decreasing = TRUE)
  geneOrder <- with(xt2, order(nbY, percentage, decreasing = TRUE))
  xt2 <- xt2[geneOrder,]
  
  # mfrow 16 pour 15 gènes/page
  PAS = nbGene
  par(xpd = NA, mfrow = c(PAS,1), oma = c(5,2,0,0) + 0.1, mar = c(20/PAS,8,0,4) + 0.1, mgp = c(3, 0.4, 0))
  # par(mfrow = c(nrow(xt2)+2,1) , oma = c(0,2,0,0) + 0.1, mar = c(0,8,0,2) + 0.1)
  
  for (t in 1:nrow(xt2)){
    profileMatrix(xt2[t,], maxPos, maxDet, metColor)
    if(t %in%  c(PAS, nrow(xt2))){ # pour le placement des axes de bas de page.
      #-- étendue de l'axe
      etend <- seq(0, maxPos, length.out = 5) - (seq(0, maxPos, length.out = 5) %% 100)
      axis(side  = 1 , labels = etend , at = etend , cex.axis = 1 , outer = T ,  tick = T , pos = -6 , col ="black", lwd.ticks = 1 ) #uniquement pour afficher les chiffres ,line = -6
    }
  }
  
  colFig2 <- metColor[2:6,]
  
  # legend for the "normal" script
  legend(2.8, -10,
         horiz = T,
         border = colFig2$Col,
         legend = colFig2$Name,
         fill = colFig2$Col,
         cex = 1,
         bty = "n",
         xpd = NA,
  )
}

