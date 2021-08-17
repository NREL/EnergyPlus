//
// Created by jackcook on 8/17/21.
//

#include <LU-Decomposition/lu.h>


void jcc::decomposition(vector<vector<double> > &A, int &n, vector<int> &indx,
                   double &d) {
    const double TINY = 1.0e-20;  // a small number
    int i, imax, j, k;
    double big, dum, sum, temp;

    vector<double> vv(n); // vv stores the implicit scaling of each row
    d = 1.0; // No row interchanges yet
    for (i=0; i<n;i++){  // loop over rows to get implicit scaling formation
        big = 0.0;
        for (j=0; j<n;j++) {
            if (fabs(A[i][j]) > big) big = fabs(A[i][j]);
        } // next j
        if (big == 0.0) throw invalid_argument("Singular matrix in routine "
                                               "decomposition");
        // No nonzero biggest element
        vv[i] = 1.0/big; // save the scaling
    } // next i

    for (j=0; j<n;j++) { // This is the loop over columns of Crout's method
        for (i=0; i<j; i++) {  // this is equation (2.3.12) except for i=j
            sum = A[i][j];
            for (k=0; k<i; k++) sum -= A[i][k] * A[k][j];
            A[i][j] = sum;
        } // next i
        big = 0.0;
        for (i=j; i<n; i++){
            sum = A[i][j];
            for (k=0; k<j;k++) sum -= A[i][k] * A[k][j];
            A[i][j] = sum;
            if (fabs(sum) >= big) {
                big = fabs(sum);
                imax = i;
            } // end if()
        } // next i
        if (j != imax) {
            for (k=0; k<n; k++) {
                dum = A[imax][k];
                A[imax][k] = A[j][k];
                A[j][k] = dum;
            } // next k
            d = -d;
            vv[imax] = vv[j];
        } // end if()
        indx[j] = imax;
        if (A[j][j] == 0.0) A[j][j] = TINY;
        if (j != (n-1)) {
            dum = 1.0 / A[j][j];
            for (i=j+1; i<n; i++) A[i][j] *= dum;
        }
    } // next j

} // decomposition()

void jcc::back_substitution(vector<vector<double> > &A, int &n, vector<int> indx,
                       vector<double> &b) {
    int i, ii=0, ip, j;
    double sum;

    for (i=0; i<n; i++) {
        ip = indx[i];
        sum=b[ip];
        b[ip]=b[i];
        if (ii != 0) {
            for (j=ii-1; j<i; j++) sum -= A[i][j] * b[j];
        } else if (sum != 0.0) {
            ii = i+1;
        }
        b[i] = sum;
    } // next i

    for (i=n-1; i>=0;i--) {
        sum=b[i];
        for (j=i+1;j<n;j++) sum -= A[i][j]*b[j];
        b[i]=sum/A[i][i];
    } // next i

} // back_substitution