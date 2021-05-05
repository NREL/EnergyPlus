/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _SORT_
#define _SORT_ 1

#include <algorithm>
#include <vector>

template <typename Comparable, typename Tag>
void insertionSort( std::vector<Comparable> & a, std::vector<Tag> & b, int left, int right );
/**
 * Quicksort algorithm (driver).
 */

 /*------------------ Quicksort for two vectors -------------------- */


/**
 * Return median of left, center, and right.
 * Order these and hide the pivot.
 */
template <typename Comparable, typename Tag>
const Comparable & median3( std::vector<Comparable> & a, std::vector<Tag> &b, int left, int right )
{
    int center = ( left + right ) / 2;
    if( a[ center ] < a[ left ] ){
        std::swap( a[ left ], a[ center ] );
		std::swap( b[ left ], b[ center ] );}
    if( a[ right ] < a[ left ] ){
		std::swap( a[ left ], a[ right ] );
		std::swap( b[ left ], b[ right ] );}
    if( a[ right ] < a[ center ] ){
		std::swap( a[ center ], a[ right ] );
		std::swap( b[ center ], b[ right ] );}

        // Place pivot at position right - 1
	std::swap( a[ center ], a[ right - 1 ] );
	std::swap( b[ center ], b[ right - 1 ] );
    return a[ right - 1 ];
}

/**
 * Internal insertion sort routine for subarrays
 * that is used by quicksort.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable, typename Tag>
void insertionSort( std::vector<Comparable> & a, std::vector<Tag> & b, int left, int right )
{
    for( int p = left + 1; p <= right; p++ )
    {
        Comparable tmp = a[ p ];
		Tag tmp2 = b[ p ];
        int j;

        for( j = p; j > left && tmp < a[ j - 1 ]; j-- ){
            a[ j ] = a[ j - 1 ];
			b[ j ] = b[ j - 1 ];
		}
        a[ j ] = tmp;
		b[ j ] = tmp2;
    }
}

/**
 * Internal quicksort method that makes recursive calls.
 * Uses median-of-three partitioning and a cutoff of 10.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable, typename Tag>
void quicksort( std::vector<Comparable> & a, std::vector<Tag> & b, int left, int right )
{
    if( left + 10 <= right )
    {
        Comparable pivot = median3( a, b, left, right );

            // Begin partitioning
        int i = left, j = right - 1;
        for( ; ; )
        {
            while( a[ ++i ] < pivot ) { }
            while( pivot < a[ --j ] ) { }
            if( i < j ){
				std::swap( a[ i ], a[ j ] );
				std::swap( b[ i ], b[ j ] );}
			else {break;}
        }

		std::swap( a[ i ], a[ right - 1 ] );  // Restore pivot
		std::swap( b[ i ], b[ right - 1 ] );

        quicksort( a, b, left, i - 1 );     // Sort small elements
		quicksort( a, b, i + 1, right );    // Sort large elements
		
    }
    else { // Do an insertion sort on the subarray
        insertionSort( a, b, left, right );
	}
}




/**
 * Return median of left, center, and right.
 * Order these and hide the pivot.
 */
template <typename Comparable>
const Comparable & median3( std::vector<Comparable> & a, int left, int right )
{
    int center = ( left + right ) / 2;
    if( a[ center ] < a[ left ] ){
        std::swap( a[ left ], a[ center ] );
		}
    if( a[ right ] < a[ left ] ){
        std::swap( a[ left ], a[ right ] );
		}
    if( a[ right ] < a[ center ] ){
        std::swap( a[ center ], a[ right ] );
		}

        // Place pivot at position right - 1
    std::swap( a[ center ], a[ right - 1 ] );
	
    return a[ right - 1 ];
}

/**
 * Internal insertion sort routine for subarrays
 * that is used by quicksort.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable>
void insertionSort( std::vector<Comparable> & a, int left, int right )
{
    for( int p = left + 1; p <= right; p++ )
    {
        Comparable tmp = a[ p ];
		
        int j;

        for( j = p; j > left && tmp < a[ j - 1 ]; j-- ){
            a[ j ] = a[ j - 1 ];
			
		}
        a[ j ] = tmp;
		
    }
}

/**
 * Internal quicksort method that makes recursive calls.
 * Uses median-of-three partitioning and a cutoff of 10.
 * a is an array of Comparable items.
 * left is the left-most index of the subarray.
 * right is the right-most index of the subarray.
 */
template <typename Comparable>
void quicksort( std::vector<Comparable> & a, int left, int right )
{
    if( left + 10 <= right )
    {
        Comparable pivot = median3( a, left, right );

            // Begin partitioning
        int i = left, j = right - 1;
        for( ; ; )
        {
            while( a[ ++i ] < pivot ) { }
            while( pivot < a[ --j ] ) { }
            if( i < j ){
                std::swap( a[ i ], a[ j ] );
				}
			else {break;}
        }

        std::swap( a[ i ], a[ right - 1 ] );  // Restore pivot
		

        quicksort( a, left, i - 1 );     // Sort small elements
		quicksort( a, i + 1, right );    // Sort large elements
		
    }
    else { // Do an insertion sort on the subarray
        insertionSort( a, left, right );
	}
}

template <typename Comparable, typename Tag>
void quicksort( std::vector<Comparable> & a, std::vector<Tag> & b)
{
    quicksort( a, b, 0, (int)a.size( ) - 1 );
}

/*------------------ Quicksort for one vector -------------------- */

template <typename Comparable>
void quicksort( std::vector<Comparable> & a)
{
    quicksort( a, 0, (int)a.size( ) - 1 );
}
#endif
