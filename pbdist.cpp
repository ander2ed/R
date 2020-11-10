

#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;



// Additional functions to support those visible to R;
double dist(double x1, double y1, double x2, double y2) {
  double d2r = M_PI / 180;
  double X1 = x1 * d2r;
  double Y1 = y1 * d2r;
  double X2 = x2 * d2r;
  double Y2 = y2 * d2r;

  double dX = X2 - X1;
  double dY = Y2 - Y1;

  double a = (pow(sin(dY / 2), 2)) + cos(Y1) * cos(Y2) * pow(sin(dX / 2), 2);
  double c = 2 * atan2(sqrt(a), sqrt(1 - a));

  return(3956 * c);
}

NumericMatrix initialize_return_matrix(int start_size, int end_size, bool cartesian, bool byid) {
  if(cartesian) {
    if(byid) {
      NumericMatrix out(start_size * end_size, 3);
      return out;
    } else {
      NumericMatrix out(start_size * end_size, 1);
      return out;
    }
  } else {
    NumericMatrix out(start_size, 1);
    return out;
  }
}

NumericMatrix initialize_return_matrix_findNearest(int start_size, bool keepDistance) {
  if(keepDistance) {
    NumericMatrix out(start_size, 3);
    return out;
  } else {
    NumericMatrix out(start_size, 2);
    return out;
  }
}
// End additional supporting functions;



//' Calculates haversine distance between two sets of X & Y coordinates
//'
//' @param start_x vector of x coordinates representing the start point
//' @param start_y vector of y coordinates representing the start point
//' @param end_x vector of x coordinates representing the end point
//' @param end_y vector of y coordinates representing the end point
//' @param cartesian logical indicating if all pairwise (cross product) distances should be calculated. Default TRUE
//' @param byid logical should id's be returned in the result set?
//' @export
// [[Rcpp::export]]
NumericMatrix haversine_dist(NumericVector start_x,
                             NumericVector start_y,
                             NumericVector end_x,
                             NumericVector end_y,
                             bool cartesian = true,
                             bool byid = true) {


  NumericMatrix out = initialize_return_matrix(start_x.size(), end_x.size(), cartesian, byid);



  if(cartesian) {
    int j = 0;

      for(int i = 0; i < start_x.size(); ++i) {
        for(int k = 0; k < end_x.size(); ++k) {
          if(byid) {
            out(j, 0) = i+1;
            out(j, 1) = k+1;
            out(j, 2) = dist(start_x[i], start_y[i], end_x[k], end_y[k]);
          } else {
            out[j] = dist(start_x[i], start_y[i], end_x[k], end_y[k]);
          }
          j++;
        }
      }
      return out;

  } else {
    if(start_x.size() != end_x.size()) {
      stop("Length of start and end points is not the same. Cannot use cartesian = FALSE with differing start and end arrays");
    }

    for(int i = 0; i < start_x.size(); ++i) {
      out[i] = dist(start_x[i], start_y[i], end_x[i], end_y[i]);
    }
    return out;
  }
}

//' For each point in the first set (defind by x1,y1), find the nearest point in a second set (x2/y2).
//'
//' @param x1 x (longitude) of first set of points
//' @param y1 y (latitude) of first set of points
//' @param x2 x (longitude) of second set of points
//' @param y2 y (latitude) of second set of points
//' @param keepDistance logical. Should the distance between pairs be returned?
//' @param excludeSelf logical. If calculating over a single set, should the site identified by x1/y1 be excluded from x2/y2 when calculating. If false, and x1 == x2 and y1 = y2, then the nearest site to x1/y1 in x2/y2 is always the same as x1/y1 with distance 0.
//' @details
//' x1 and y1 should be the same length. x2 and y2 should be the same length.
//' This function will find the point in x2/y2 nearest to each point in x1/y1. The return value is a list
//' the same length as x1 & y1 with the index of the point from the second set. It is therefore important
//' that you don't re-order your data after passing it to this function and merging the results back to
//' the input data. Each x/y input should be passed as a numeric vector, and or something that can be
//' coerced to a numeric vector such as (most commonly) a data frame column, data table column.
//' @export
// [[Rcpp::export]]
NumericMatrix findNearest(Rcpp::NumericVector x1,
                          Rcpp::NumericVector y1,
                          Rcpp::NumericVector x2,
                          Rcpp::NumericVector y2,
                          bool keepDistance = true,
                          bool excludeSelf = false) {


  if(excludeSelf && x1.size() != x2.size()) {
    stop("Cannot `excludeSelf` is x1/y1 and x2/y2 are different sizes");
  }

  NumericMatrix out = initialize_return_matrix_findNearest(x1.size(), keepDistance);
  double dist_temp;
  NumericMatrix min_row(1, 3);




  for(int i = 0; i < x1.size(); ++i) {
    for(int k = 0; k < x2.size(); ++k) {


      if(excludeSelf) {
        if(i != k) {
          dist_temp= dist(x1[i], y1[i], x2[k], y2[k]);

          if(k == 0 || (i == 0 && k == 1)) {
            min_row(0, 0) = i+1;
            min_row(0, 1) = k+1;
            min_row(0, 2) = dist_temp;
          } else {

            if(dist_temp < min_row(0, 2)) {
              min_row(0, 0) = i+1;
              min_row(0, 1) = k+1;
              min_row(0, 2) = dist_temp;
            }
          }
        }

      } else {

        dist_temp= dist(x1[i], y1[i], x2[k], y2[k]);

        if(k == 0) {
        // if(k == 0 || (excludeSelf && i == 0 && k == 1)) {
          min_row(0, 0) = i+1;
          min_row(0, 1) = k+1;
          min_row(0, 2) = dist_temp;
        } else {

          if(dist_temp < min_row(0, 2)) {
            min_row(0, 0) = i+1;
            min_row(0, 1) = k+1;
            min_row(0, 2) = dist_temp;
          }
        }
      }

    }


    out(i, 0) = min_row(0, 0);
    out(i, 1) = min_row(0, 1);
    if(keepDistance) {
      out(i, 2) = min_row(0, 2);
    }

  }

  return out;

}











//' For each point in the first set (defind by x1,y1), find the N nearest points in a second set (x2/y2).
//'
//' @param x1 x (longitude) of first set of points
//' @param y1 y (latitude) of first set of points
//' @param x2 x (longitude) of second set of points
//' @param y2 y (latitude) of second set of points
//' @param n how many near points to return (this is the `n` in findNearestN)
//' @param keepDistance logical. Should the distance between pairs be returned?
//' @param excludeSelf logical. If calculating over a single set, should the site identified by x1/y1 be excluded from x2/y2 when calculating. If false, and x1 == x2 and y1 = y2, then the nearest site to x1/y1 in x2/y2 is always the same as x1/y1 with distance 0.
//' @details
//' x1 and y1 should be the same length. x2 and y2 should be the same length.
//' This function will find the N points in x2/y2 nearest to each point in x1/y1. The return value is a list
//' the same length as x1 & y1, * n, with the index of the point from the second set. It is therefore important
//' that you don't re-order your data after passing it to this function and merging the results back to
//' the input data. \cr \cr
//' Each x/y input should be passed as a numeric vector, and or something that can be
//' coerced to a numeric vector such as (most commonly) a data frame column, data table column.
//' The time to execute this function scales linearly with (j * k) where j and k are the length of x1 and x2 according
//' approximately to the function:
//' \deqn{t = 2.668*10^-7*(j*k)}{t = 2.668e-7 * (j * k)}
//' \cr This result suggests that the following values of j and k will execute in the following time:\cr
//' \tabular{rrr}{
//'   size of j and k \tab total iterations \tab time (seconds) \cr
//'   1,000   \tab 1,000,000     \tab 0.27 \cr
//'   2,000   \tab 4,000,000     \tab 1.08 \cr
//'   3,000   \tab 9,000,000     \tab 2.15 \cr
//'   4,000   \tab 16,000,000    \tab 4.30 \cr
//'   5,000   \tab 25,000,000    \tab 6.72 \cr
//'   10,000  \tab 100,000,000   \tab 26.88 \cr
//'   15,000  \tab 225,000,000   \tab 60.48 \cr
//'   20,000  \tab 400,000,000   \tab 107.52 \cr
//'   40,000  \tab 1,600,000,000 \tab 430.08 \cr
//'   50,000  \tab 2,500,000,000 \tab 672 \cr
//'   100,000 \tab 10,000,000,000\tab 2688 \cr
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix findNearestN(Rcpp::NumericVector x1,
                          Rcpp::NumericVector y1,
                          Rcpp::NumericVector x2,
                          Rcpp::NumericVector y2,
                          int n,
                          bool keepDistance = true,
                          bool excludeSelf = false) {
  
  if(x1.size() != y1.size()) {
    stop("x1.size() != y1.size()");
  }
  if(x2.size() != y2.size()) {
    stop("x2.size() != y2.size()");
  }
  if(n > x2.size()) {
    stop("Makes no sense to calculate nearest N when N is larger than x2.size()");
  }
  if((excludeSelf && n > (x2.size() - 1))) {
    stop("Makes no sense to caluclate nearest N with `excludeSelf` when N is larger than x2.size() - 1");
  }
  if(excludeSelf && x1.size() != x2.size()) {
    stop("Cannot `excludeSelf` is x1/y1 and x2/y2 are different sizes");
  }
  
  
  NumericMatrix out = initialize_return_matrix_findNearest(x1.size() * n, keepDistance);
  double dist_temp;
  NumericMatrix min_row(n, 3); 
  //std::fill(min_row.begin(), min_row.end(), R_PosInf);
  
  
  
  
  
  for(int i = 0; i < x1.size(); ++i) {
    
    for(int z = 0; z < n; ++z) {
      min_row(z, 0) = -1;
      min_row(z, 1) = -1;
      min_row(z, 2) = R_PosInf;
    }
    
  
    for(int k = 0; k < x2.size(); ++k) {
      
      
      
      dist_temp = dist(x1[i], y1[i], x2[k], y2[k]);
      
    
      if(excludeSelf) {
        
        // handle excluding self reference here;
        if(i != k) {
          if(k == 0) {
            min_row(0, 0) = i + 1;
            min_row(0, 1) = k + 1;
            min_row(0, 2) = dist_temp;
          } else {
            int z = n - 1;
            while(dist_temp < min_row(z, 2) && z >= 0) {
              // shift values down until new dist no longer smaller;
              if(z == (n - 1)) {
                min_row(z, 0) = i + 1;
                min_row(z, 1) = k + 1;
                min_row(z, 2) = dist_temp;
              } else {
                min_row(z + 1, 0) = min_row(z, 0);
                min_row(z + 1, 1) = min_row(z, 1);
                min_row(z + 1, 2) = min_row(z, 2);

                min_row(z, 0) = i + 1;
                min_row(z, 1) = k + 1;
                min_row(z, 2) = dist_temp;
              }
              --z;
            }
          }
        }

      } else {
        if(k == 0) {
          min_row(0, 0) = i + 1;
          min_row(0, 1) = k + 1;
          min_row(0, 2) = dist_temp;
        } else {
          int z = n - 1;
          
        
          while(dist_temp < min_row(z, 2) && z >= 0) {
            // shift values down until new dist no longer smaller;
            if(z == (n - 1)) {
              min_row(z, 0) = i + 1;
              min_row(z, 1) = k + 1;
              min_row(z, 2) = dist_temp;
            } else {
              min_row(z + 1, 0) = min_row(z, 0);
              min_row(z + 1, 1) = min_row(z, 1);
              min_row(z + 1, 2) = min_row(z, 2);
              
              min_row(z, 0) = i + 1;
              min_row(z, 1) = k + 1;
              min_row(z, 2) = dist_temp;
            }
            --z;
          }
        }
      }
    }
    

    
    for(int z = 0; z < n; ++z) {
      out((n * i) + z, 0) = min_row(z, 0);
      out((n * i) + z, 1) = min_row(z, 1);
      if(keepDistance) {
        out((n * i) + z, 2) = min_row(z, 2);
      }
    }
    
    
  }
  
  
  return out;
  
}


