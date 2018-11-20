# SMOTE
#

smote.im <- function(minority,class,k,Npercent) {
  if(!is.matrix(minority)) {
    minority <- data.matrix(minority)
  }
  
  #printf("-> %.2f\n",Npercent)
  
  minority_nolabel <- minority[,colnames(minority) != class]
  smoted <- matrix(NA, nrow = 0, ncol = ncol(minority_nolabel))
  idx <- c(1:nrow(minority))
  
  if(Npercent < 100) {
    # Randomize minority class samples
    Npercent <- Npercent
    minority <- minority
    idx <- sample(1:nrow(minority), size = (round(Npercent/100)*nrow(minority)))
    Npercent <- 100
  }
  
  for(i in idx) {
    object <- minority_nolabel[i,]
    minority_dataset <- minority_nolabel[-i,]
    
    if(!is.matrix(minority_dataset)) {
      if(length(minority_dataset) == 0) {
        minority_dataset <- minority_nolabel
      }
      
      minority_dataset <- matrix(minority_dataset, ncol = length(minority_dataset), nrow = 1)
    }

    neighbours_ids <- knn.index.im(minority_dataset,object,dist_range = NULL,k)
    neighbours <- minority_dataset[neighbours_ids,]
    
    if(!is.matrix(neighbours)) {
      neighbours <- matrix(neighbours, ncol = length(neighbours), nrow = 1)
    }
    
    N <- round(Npercent/100)
    
    while(N > 0) {
      # Select one of the neighbours
      nn <- sample(1:nrow(neighbours), size = 1)
      neighbour <- neighbours[nn,] 
      size <- length(neighbour)
	  
      # Compute synthethic instance
	  for (i in 1:size)
	  {
		dif[i] <- neighbour[i]-object[i]
		# Compute gap in range [0, 1]
		gap <- runif(1, min = 0, max = 1)
		synth[i] <- object[i]+(gap*dif[i])
	  }
      
      # Store synthetic sample
      smoted <- rbind(smoted, synth)
      
      N <- N-1
    }
  }
  
  # Update row names
  rownames(smoted) <- c(1:nrow(smoted))
  
  # Create data frame
  smoted <- data.frame(smoted)
  
  # Get factor
  minority_label <- as.numeric(minority[1, class])
  
  # Create class column
  class_col <- rep(minority_label, nrow(smoted))

  # Bind column
  smoted <- cbind(smoted, class_col)
  
  # Update column names
  colnames(smoted) <- colnames(minority)
    
  return <- smoted
}
