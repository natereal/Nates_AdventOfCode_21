`%notin%` <- Negate(`%in%`)

xrange <- c(230:283)
yrange <- c(57:107) * -1

#sx <- sy <- 0

# x <- 23
# y <- 106


# sx <- sy <- 0
# y_vec <- c(sy)
# inc <- 0
# # x needs to be at least 21
# # y is at most 106
# while(sx %notin% xrange | sy %notin% yrange) {
#   sx <- sx + x
#   sy <- sy + y
#   y_vec <- c(y_vec, sy)
#   if(x > 0) x <- x - 1
#   y <- y - 1
#   inc <- inc + 1
# }

# max(y_vec)

# target area: x=230..283, y=-107..-57

# x needs to be at least 21, and at the very most 282
# y is at most 106
# valid_p <- 0
xseq <- 21:282
yseq <- -107:106
# valid <- 0
  #matrix(0, nrow = length(xseq), ncol = length(yseq)) # dimnames = list(as.character(xseq), as.character(yseq)))
y_vec <- vector(mode = "integer", length = length(yseq))
# i <- j <- 0

in_range <- function(x, y) {
  return(x %in% xrange & y %in% yrange)
}

# i <- 1
valid <- 0
for(ix in xseq) {
  j <- 1
  for(iy in yseq) {

    sx <- sy <- 0

    x <- ix
    y <- iy
    while(!in_range(sx,sy) & sy > min(yrange)) {
      sx <- sx + x
      sy <- sy + y
      if(x > 0) x <- x - 1
      y <- y - 1
      # inc <- inc + 1

    }
    if(in_range(sx, sy)) {
      valid <- valid + 1
      y_vec[j] <- iy
      # valid[i, j] <- 1
    }
    j <- j + 1
  }
  # print(ix)
  # i <- i + 1
}
