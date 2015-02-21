source('cachematrix.R')

############################################################################
# Test 1: Create a matrix and invert.  Create cachable matrix from orignal
# and invert.  Compare the two inversions.
############################################################################
m <- matrix(1:4, 2, 2)
mi <- solve(m)

# Solve the matrix using the cachable object and compare against the uncachable solution
c <- makeCacheMatrix(m)
ci = cacheSolve(c)
cat('Inverse equal:', all.equal(mi, ci), '\n')


############################################################################
# Test 2: Create a new cacheable matrix from the inverted matrix, re-invert
# and compare with the original matrix
############################################################################
a <- makeCacheMatrix(ci)
cat('Equal to orignal:', all.equal(cacheSolve(a), m), '\n')


############################################################################
# Test 3: Create a large cachable matrix.  Time the initial solution, then
# time the cacheSolve function a second time and see that it runs quickly
############################################################################
size = 1000
m = matrix(rnorm(size^2), size, size)
c <- makeCacheMatrix(m)
t1 = as.POSIXct(Sys.time())
ci = cacheSolve(c)
t2 = as.POSIXct(Sys.time())
d1 = t2 - t1
cat('Initial time:', d1, 'seconds', '\n')

t1 = as.POSIXct(Sys.time())
ci = cacheSolve(c)
t2 = as.POSIXct(Sys.time())
d2 = t2 - t1
cat('Cached time: ', d2, 'seconds', '\n')


############################################################################
# Test 4: Test that the 'reval' parameter forces inverse calculation to
# re-evaluate
############################################################################
t1 = as.POSIXct(Sys.time())
ci = cacheSolve(c, reval=TRUE)
t2 = as.POSIXct(Sys.time())
d3 = t2 - t1
dd = as.numeric(abs(d3-d1)) / as.numeric(d1)
cat('Reval time:  ', d3, 'seconds', dd<0.10, sprintf('%.2f%%', dd*100), '\n')

t1 = as.POSIXct(Sys.time())
ci = cacheSolve(c, reval=FALSE)
t2 = as.POSIXct(Sys.time())
d4 = t2 - t1
cat('Cached time: ', d4, 'seconds', d4<0.001, '\n')


############################################################################
# Test 5: Test that changing the original matrix results in a recalculation
############################################################################
m = matrix(rnorm(size^2), size, size)
c$set(m)

t1 = as.POSIXct(Sys.time())
ci = cacheSolve(c, reval=TRUE)
t2 = as.POSIXct(Sys.time())
d5 = t2 - t1
dd = as.numeric(abs(d5-d1)) / as.numeric(d1)
cat('Change time: ', d5, 'seconds', dd<0.10, sprintf('%.2f%%', dd*100), '\n')

t1 = as.POSIXct(Sys.time())
ci = cacheSolve(c, reval=FALSE)
t2 = as.POSIXct(Sys.time())
d6 = t2 - t1
cat('Cached time: ', d6, 'seconds', d6<0.001, '\n')

