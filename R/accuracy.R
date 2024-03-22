monte_carlo_accuracy <- function(training_pts, classifier, seg_id, reclass, iterations = 100, validPrc = 0.1, dropClasses = NULL){

  # Read training data
  alldata <- do.call(rbind, lapply(training_pts, function(tp) read.csv(tp@datafile)))

  # Drop columns
  dropCols <- c("tile_name" , seg_id)
  alldata <- alldata[,!names(alldata) %in% dropCols]

  # Remove any classes that aren't in 'reclass'
  alldata <- alldata[alldata$seg_class %in% unlist(reclass),]

  # Get predictors
  predictors <- attributes(classifier$terms)$term.labels

  # Factorize 'seg_class' attribute
  alldata$seg_class <- as.factor(alldata$seg_class)

  # Check for rows with NA values and remove
  badRows <- apply(is.na(alldata), 1, any)
  if(any(badRows)) alldata <- alldata[!badRows,]

  accs <- lapply(1:iterations, function(i){

    samp <- sample(1:nrow(alldata), round(nrow(alldata) * validPrc))

    valid_data <- alldata[samp,]
    train_data <- alldata[setdiff(1:nrow(alldata), samp),]

    # Create classifier
    ccf <- randomForest::randomForest(
      as.formula(paste("seg_class ~", paste(predictors, collapse = " + "))),
      data       = train_data,
      importance = TRUE,
      ntree      = 1000)

    votes   <- randomForest:::predict.randomForest(ccf, valid_data, type = "vote")
    elected <- colnames(votes)[apply(votes, 1, function(x) which.max(x)[1])]

    observed <- as.character(valid_data$seg_class)

    for(class in names(reclass)){
      elected[elected %in% reclass[[class]]]   <- class
      observed[observed %in% reclass[[class]]] <- class
    }

    rfUtilities::accuracy(
      factor(elected, names(reclass)),
      factor(observed, names(reclass))
    )
  })

  list(
    PCC = mean(sapply(accs, function(acc) acc$PCC )),
    kappa = mean(sapply(accs, function(acc) acc$kappa )),
    sensitivity = mean(sapply(accs, function(acc) acc$sensitivity ), na.rm = T),
    specificity = mean(sapply(accs, function(acc) acc$specificity ), na.rm = T),
    users = apply(do.call(rbind, lapply(accs, function(acc){
      acc$users.accuracy[is.nan(acc$users.accuracy)] <- NA
      acc$users.accuracy
    } )), 2 ,mean, na.rm = TRUE),
    producers = apply(do.call(rbind, lapply(accs, function(acc){
      acc$producers.accuracy[is.nan(acc$producers.accuracy)] <- NA
      acc$producers.accuracy
    })), 2 ,mean, na.rm = TRUE)
  )


}

training_data_summary <- function(training_pts, reclass){

  # Read training data
  alldata <- do.call(rbind, lapply(training_pts, function(tp) read.csv(tp@datafile)))

  observed <- alldata$seg_class

  for(class in names(reclass)){
      observed[observed %in% reclass[[class]]] <- class
  }

  table(observed)
}
