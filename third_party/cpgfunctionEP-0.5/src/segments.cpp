//
// Created by jackcook on 7/15/21.
//

#include <iostream>
#include <cmath>
#include <cpgfunction/segments.h>
#include <cpgfunction/interpolation.h>

using namespace std;

namespace gt::segments {

    void SegmentResponse::get_h_value(double &h, const int i, const int j,
                                      const int k) {
        int index;
        switch (storage_mode) {
            case 0 :
                cout << "Case 0 not written yet" << endl;
                break;
            case 1 :
                if (i <= j) {
                    get_index_value(index, i, j);
                    h = h_ij[index][k];
                } else {
                    get_index_value(index, j, i);
                    h = boreSegments[j].H/boreSegments[i].H * h_ij[index][k];
                }
                break;
            default:
                throw invalid_argument("The case selected is not currently "
                                       "implemented.");
        }  // switch();
    }  // SegmentResponse::get_h_value();

    void SegmentResponse::get_index_value(int &index, const int i, const int j) {
        index = i * (2*nSources - i - 1) / 2 + j;
    }  // SegmentResponse::get_index_value();

    int adaptive::discretize(double height, double drilling_depth) {
        // Given the height of a field and the total drilling depth
        // determine the ideal number of segments for UBHWT to match
        // a converged UIFT solution
        double nq;
        int n1 = heights.size() - 1;

        if (height <= heights[0]) {
            // use the results for the lowest height (single interpolation)
            nq = jcc::interpolation::interp1d(drilling_depth,
                                              drilling_depths[0],
                                              ideal_segment_lengths[0]);
        } else if(heights[0] < height < heights[n1]) {
            // do bilinear interpolation
            // find the lower and upper height values
            bool finished = false;
            int count = 0;
            while (!finished) {
                if (height >= heights[count] && height <= heights[count+1]) {
                    finished = true;
                }
                count++;
            }
            int lower = count - 1;
            int upper = count;
            double height_low = heights[lower];
            double height_up = heights[upper];
            double nq_low = jcc::interpolation::interp1d(drilling_depth,
                                             drilling_depths[lower],
                                             ideal_segment_lengths[lower]);
            double nq_up = jcc::interpolation::interp1d(drilling_depth,
                                                        drilling_depths[upper],
                                                        ideal_segment_lengths[upper]);
            nq = jcc::interpolation::linterp(height, height_low, nq_low,
                                             height_up, nq_up);
        } else {  // (height >= heights[-1])
            nq = jcc::interpolation::interp1d(drilling_depth,
                                              drilling_depths[n1],
                                              ideal_segment_lengths[n1]);
        }
        int _nq = floor(nq);
        return _nq;
    }
}