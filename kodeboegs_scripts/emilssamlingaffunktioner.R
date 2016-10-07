


# fra http://stackoverflow.com/questions/9213949/how-to-display-lists-side-by-side-in-r-a-cbind-for-lists
#til at se lister side by side (ikke testet endnu)
sidebyside <- function(..., width=60){
  l <- list(...)
  p <- lapply(l, function(x){
        xx <- capture.output(print(x, width=width))
        xx <- gsub("\"", "", xx)
        format(xx, justify="left", width=width)
      }
  )
  p <- do.call(cbind, p)
  sapply(seq_len(nrow(p)), function(x)paste(p[x, ], collapse=""))
}


write.table(, file="./.csv", sep = ";", fileEncoding = "UTF-8")