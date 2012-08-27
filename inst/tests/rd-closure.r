#' First function
f <- function(){
   .a <- 0
   function(x = 1){
     .a <<- .a + x
     .a
   }
}

#' Second function
f2 <- f()
rm(f)
