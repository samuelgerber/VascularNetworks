
read.tree <- function(inputFile){
  con  <- file(inputFile, open = "r")

  
  X = c()
  id = 0
  pid = 0
  while (length( line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if( startsWith( line, "ID" ) ){
      id = as.integer( strsplit(line, "=")[[1]][2] )
    }
    else if( startsWith( line, "ParentID" ) ){
      pid = as.integer( strsplit(line, "=")[[1]][2] )
    }
    else if( startsWith( line, "NPoints" ) ){
      npoints = as.integer( strsplit(line, "=")[[1]][2] )
      readLines(con, n=1)
      X = rbind(X, cbind( read.table(con, nrows=npoints)[,1:4], id, pid  ) )
    }

  } 

  close(con)
  X

}


read.all.trees <- function(folder){
  dirs <- list.dirs( folder )

  names = list()
  data = list()
  index = 1
  for( d in dirs){
    name = strsplit(d, "/")[[1]]
    name = name[length(name)]
    #d = paste(d, "/VascularNetwork.tre", sep="")
    d = paste(d, "/color-new-t1Phantom.tre", sep="")
    if( file.access(d) == 0 ){
      print(name)
      names[[index]] = name
      data[[index]] = read.tree( d )
      index = index + 1
    }
  }
  list(names=names, data=data)
}
