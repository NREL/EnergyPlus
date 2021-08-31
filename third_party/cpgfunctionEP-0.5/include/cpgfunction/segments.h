//
// Created by jackcook on 7/15/21.
//

#ifndef CPGFUNCTIONEP_SEGMENTS_H
#define CPGFUNCTIONEP_SEGMENTS_H

#include <cpgfunction/boreholes.h>
#include <iostream>
#include <map>
#include <vector>

namespace gt::segments {

    struct SegmentResponse {
        ~SegmentResponse() {} // destructor

        int nSources;
        int nSum;
        std::vector<gt::boreholes::Borehole> boreSegments;
        std::vector < std::vector < double > > h_ij;

        SegmentResponse(int nSources,
                        int nSum,
                        int nt) :
                nSources(nSources),
                nSum(nSum),
                boreSegments(nSources),
                h_ij(nSum, std::vector<double>(nt, 0))
        {} // constructor

        // storage_mode = 1 is the reduced segment response std::vector
        int storage_mode = 1;

//        void ReSizeContainers(int n, int nt);
        void get_h_value(double &h, int i, int j, int k);
        void get_index_value(int &index, int i, int j);
    };  // struct SegmentResponse();

    struct adaptive {
        ~adaptive() = default;  // destructor

        std::vector<double> heights = { 24, 48, 96, 192, 384};
        std::vector<std::vector<double > > drilling_depths {
                {96,600,1536,2904,3456,4056,4704,5400,6144,6936,7776,8664,9600,
                 10584,11616,12696,13824,15000,16224,17496,18816,20184,21600,
                 23064,24576},
                {192,1200,3072,5808,6912,8112,9408,10800,12288,13872,15552,
                 17328,19200,21168,23232,25392,27648,30000,32448,34992,37632,
                 40368,43200,46128,49152},
                {384,2400,6144,11616,13824,16224,18816,21600,24576,27744,31104,
                 34656,38400,42336,46464,50784,55296,60000,64896,69984,75264,
                 80736,86400,92256},
                {768,4800,12288,23232,27648,32448,37632,43200,49152,55488,62208,
                 69312,76800,84672,92928,101568,120000},
                {1536,9600,24576,46464,55296,64896,75264,86400,98304,110976,
                 124416,138624}
        };
        std::vector<std::vector<double > > ideal_segment_lengths {
                {6.897,8.297,8.318,8.158,8.108,8.062,8.02,7.982,7.947,7.915,
                 7.886,7.859,7.835,7.813,7.792,7.773,7.755,7.739,7.724,7.71,
                 7.696,7.684,7.672,7.661,7.651},
                {9.893,12.65,12.473,12.219,12.15,12.089,12.036,11.949,11.772,
                 11.616,11.478,11.354,11.244,11.144,11.054,10.972,10.897,10.829,
                 10.766,10.708,10.654,10.604,10.558,10.515,10.475},
                {13.761,15.065,14.106,13.415,13.497,13.112,12.992,12.889,12.799,
                 12.72,12.651,12.59,12.535,12.486,12.442,12.402,12.366,12.333,
                 12.412,12.381,12.351,12.324,12.299,12.276},
                {17.176,17.662,15.837,13.527,13.066,12.693,12.387,12.133,11.918,
                 11.734,11.575,11.438,11.317,11.21,11.114,11.029,10.883},
                {19.739,19.591,16.914,14.801,14.064,13.493,13.041,12.674,12.372,
                 12.119,11.904,11.72}
        };

        adaptive() = default; // constructor

        int discretize(double height, double drilling_depth);

    };

}

#endif //CPGFUNCTIONEP_SEGMENTS_H
