# Function to find the intersections between two circles

intersect_circles <- function(x, y, r) {
  x1 <- x[1]
  y1 <- y[1]
  r1 <- r[1]
  # Circle 2: center (x2, y2), radius r2
  x2 <- x[2]
  y2 <- y[2]
  r2 <- r[2]
  
  # 1. Calculate the distance between the centers
  d <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  # 2. Check for intersection cases
  if (d > r1 + r2 || d < abs(r1 - r2)) {
    # No intersection
    print("No intersection")
    intersection_points <- NULL
  } else if (d == 0 && r1 == r2) {
    # Circles are identical
    print("Circles are identical")
    intersection_points <- NULL # Or represent as infinite points
  } else {
    # 3. Calculate intermediate values
    # a = distance from center 1 to the point on the line connecting centers
    #     that is closest to the intersection points
    a <- (r1^2 - r2^2 + d^2) / (2 * d)
    
    # h = half the distance between the two intersection points
    h <- sqrt(r1^2 - a^2)
    
    # P5 = the point on the line connecting the centers
    P5_x <- x1 + a * (x2 - x1) / d
    P5_y <- y1 + a * (y2 - y1) / d
    
    # 4. Calculate the intersection points using P5 and h
    intersection_point1_x <- P5_x + h * (y2 - y1) / d
    intersection_point1_y <- P5_y - h * (x2 - x1) / d
    
    intersection_point2_x <- P5_x - h * (y2 - y1) / d
    intersection_point2_y <- P5_y + h * (x2 - x1) / d
    
    # 5. Store the results
    intersection_points <- matrix(
      c(
      intersection_point1_x,
      intersection_point1_y,
      intersection_point2_x,
      intersection_point2_y
    ),
    ncol = 2, byrow = TRUE)
    colnames(intersection_points) <- c("x", "y")
  }
  return(intersection_points)
}
  
# # Define circle parameters
# 
# xs <- c(0, 7)
# ys <- c(0, 0)
# rs <- c(5, 4)
# 
# intersect_circles(xs, ys, rs)

# END