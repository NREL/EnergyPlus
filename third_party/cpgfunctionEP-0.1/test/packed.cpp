//
// Created by jackcook on 6/28/21.
//

#include <cpgfunction/heat_transfer.h>
#include <vector>

auto sum_to_n = [](const int n) {
    return n * (n + 1) / 2;
};

int main(){
    int nSources = 5;
    int nSum = sum_to_n(nSources);
    int nt = 5;

    gt::heat_transfer::SegmentResponse SegRes(nSources, nSum, nt);

    int index;
    for (int i=0; i<5; i++) {
        for (int j=i; j<5; j++) {
            SegRes.get_index_value(index, i, j);
            std::cout << i << "\t" << j << "\t" << index << std::endl;
        }
    }

    return 0;
}