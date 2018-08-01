#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"


using namespace SpectralAveraging;
using namespace FenestrationCommon;

class TestSampleNFRC_103_Angular_Photopic : public testing::Test {

private:
	std::shared_ptr< CSpectralSample > m_Sample;

protected:
	std::shared_ptr< CSeries > getSolarRadiation() const {
		auto aSolarRadiation = std::make_shared< CSeries >();

		// Full CIE Illuminant D651 nm ssp table (used for PHOTOPIC properties)
		aSolarRadiation->addProperty( 0.3000, 0.0341 );
		aSolarRadiation->addProperty( 0.3010, 0.3601 );
		aSolarRadiation->addProperty( 0.3020, 0.6862 );
		aSolarRadiation->addProperty( 0.3030, 1.0122 );
		aSolarRadiation->addProperty( 0.3040, 1.3383 );
		aSolarRadiation->addProperty( 0.3050, 1.6643 );
		aSolarRadiation->addProperty( 0.3060, 1.9903 );
		aSolarRadiation->addProperty( 0.3070, 2.3164 );
		aSolarRadiation->addProperty( 0.3080, 2.6424 );
		aSolarRadiation->addProperty( 0.3090, 2.9685 );
		aSolarRadiation->addProperty( 0.3100, 3.2945 );
		aSolarRadiation->addProperty( 0.3110, 4.9887 );
		aSolarRadiation->addProperty( 0.3120, 6.6828 );
		aSolarRadiation->addProperty( 0.3130, 8.3770 );
		aSolarRadiation->addProperty( 0.3140, 10.0711 );
		aSolarRadiation->addProperty( 0.3150, 11.7652 );
		aSolarRadiation->addProperty( 0.3160, 13.4594 );
		aSolarRadiation->addProperty( 0.3170, 15.1535 );
		aSolarRadiation->addProperty( 0.3180, 16.8477 );
		aSolarRadiation->addProperty( 0.3190, 18.5418 );
		aSolarRadiation->addProperty( 0.3200, 20.2360 );
		aSolarRadiation->addProperty( 0.3210, 21.9177 );
		aSolarRadiation->addProperty( 0.3220, 23.5995 );
		aSolarRadiation->addProperty( 0.3230, 25.2812 );
		aSolarRadiation->addProperty( 0.3240, 26.9630 );
		aSolarRadiation->addProperty( 0.3250, 28.6447 );
		aSolarRadiation->addProperty( 0.3260, 30.3265 );
		aSolarRadiation->addProperty( 0.3270, 32.0082 );
		aSolarRadiation->addProperty( 0.3280, 33.6900 );
		aSolarRadiation->addProperty( 0.3290, 35.3717 );
		aSolarRadiation->addProperty( 0.3300, 37.0535 );
		aSolarRadiation->addProperty( 0.3310, 37.3430 );
		aSolarRadiation->addProperty( 0.3320, 37.6326 );
		aSolarRadiation->addProperty( 0.3330, 37.9221 );
		aSolarRadiation->addProperty( 0.3340, 38.2116 );
		aSolarRadiation->addProperty( 0.3350, 38.5011 );
		aSolarRadiation->addProperty( 0.3360, 38.7907 );
		aSolarRadiation->addProperty( 0.3370, 39.0802 );
		aSolarRadiation->addProperty( 0.3380, 39.3697 );
		aSolarRadiation->addProperty( 0.3390, 39.6593 );
		aSolarRadiation->addProperty( 0.3400, 39.9488 );
		aSolarRadiation->addProperty( 0.3410, 40.4451 );
		aSolarRadiation->addProperty( 0.3420, 40.9414 );
		aSolarRadiation->addProperty( 0.3430, 41.4377 );
		aSolarRadiation->addProperty( 0.3440, 41.9340 );
		aSolarRadiation->addProperty( 0.3450, 42.4302 );
		aSolarRadiation->addProperty( 0.3460, 42.9265 );
		aSolarRadiation->addProperty( 0.3470, 43.4228 );
		aSolarRadiation->addProperty( 0.3480, 43.9191 );
		aSolarRadiation->addProperty( 0.3490, 44.4154 );
		aSolarRadiation->addProperty( 0.3500, 44.9117 );
		aSolarRadiation->addProperty( 0.3510, 45.0844 );
		aSolarRadiation->addProperty( 0.3520, 45.2570 );
		aSolarRadiation->addProperty( 0.3530, 45.4297 );
		aSolarRadiation->addProperty( 0.3540, 45.6023 );
		aSolarRadiation->addProperty( 0.3550, 45.7750 );
		aSolarRadiation->addProperty( 0.3560, 45.9477 );
		aSolarRadiation->addProperty( 0.3570, 46.1203 );
		aSolarRadiation->addProperty( 0.3580, 46.2930 );
		aSolarRadiation->addProperty( 0.3590, 46.4656 );
		aSolarRadiation->addProperty( 0.3600, 46.6383 );
		aSolarRadiation->addProperty( 0.3610, 47.1834 );
		aSolarRadiation->addProperty( 0.3620, 47.7285 );
		aSolarRadiation->addProperty( 0.3630, 48.2735 );
		aSolarRadiation->addProperty( 0.3640, 48.8186 );
		aSolarRadiation->addProperty( 0.3650, 49.3637 );
		aSolarRadiation->addProperty( 0.3660, 49.9088 );
		aSolarRadiation->addProperty( 0.3670, 50.4539 );
		aSolarRadiation->addProperty( 0.3680, 50.9989 );
		aSolarRadiation->addProperty( 0.3690, 51.5440 );
		aSolarRadiation->addProperty( 0.3700, 52.0891 );
		aSolarRadiation->addProperty( 0.3710, 51.8777 );
		aSolarRadiation->addProperty( 0.3720, 51.6664 );
		aSolarRadiation->addProperty( 0.3730, 51.4550 );
		aSolarRadiation->addProperty( 0.3740, 51.2437 );
		aSolarRadiation->addProperty( 0.3750, 51.0323 );
		aSolarRadiation->addProperty( 0.3760, 50.8209 );
		aSolarRadiation->addProperty( 0.3770, 50.6096 );
		aSolarRadiation->addProperty( 0.3780, 50.3982 );
		aSolarRadiation->addProperty( 0.3790, 50.1869 );
		aSolarRadiation->addProperty( 0.3800, 49.9755 );
		aSolarRadiation->addProperty( 0.3810, 50.4428 );
		aSolarRadiation->addProperty( 0.3820, 50.9100 );
		aSolarRadiation->addProperty( 0.3830, 51.3773 );
		aSolarRadiation->addProperty( 0.3840, 51.8446 );
		aSolarRadiation->addProperty( 0.3850, 52.3118 );
		aSolarRadiation->addProperty( 0.3860, 52.7791 );
		aSolarRadiation->addProperty( 0.3870, 53.2464 );
		aSolarRadiation->addProperty( 0.3880, 53.7137 );
		aSolarRadiation->addProperty( 0.3890, 54.1809 );
		aSolarRadiation->addProperty( 0.3900, 54.6482 );
		aSolarRadiation->addProperty( 0.3910, 57.4589 );
		aSolarRadiation->addProperty( 0.3920, 60.2695 );
		aSolarRadiation->addProperty( 0.3930, 63.0802 );
		aSolarRadiation->addProperty( 0.3940, 65.8909 );
		aSolarRadiation->addProperty( 0.3950, 68.7015 );
		aSolarRadiation->addProperty( 0.3960, 71.5122 );
		aSolarRadiation->addProperty( 0.3970, 74.3229 );
		aSolarRadiation->addProperty( 0.3980, 77.1336 );
		aSolarRadiation->addProperty( 0.3990, 79.9442 );
		aSolarRadiation->addProperty( 0.4000, 82.7549 );
		aSolarRadiation->addProperty( 0.4010, 83.6280 );
		aSolarRadiation->addProperty( 0.4020, 84.5011 );
		aSolarRadiation->addProperty( 0.4030, 85.3742 );
		aSolarRadiation->addProperty( 0.4040, 86.2473 );
		aSolarRadiation->addProperty( 0.4050, 87.1204 );
		aSolarRadiation->addProperty( 0.4060, 87.9936 );
		aSolarRadiation->addProperty( 0.4070, 88.8667 );
		aSolarRadiation->addProperty( 0.4080, 89.7398 );
		aSolarRadiation->addProperty( 0.4090, 90.6129 );
		aSolarRadiation->addProperty( 0.4100, 91.4860 );
		aSolarRadiation->addProperty( 0.4110, 91.6806 );
		aSolarRadiation->addProperty( 0.4120, 91.8752 );
		aSolarRadiation->addProperty( 0.4130, 92.0697 );
		aSolarRadiation->addProperty( 0.4140, 92.2643 );
		aSolarRadiation->addProperty( 0.4150, 92.4589 );
		aSolarRadiation->addProperty( 0.4160, 92.6535 );
		aSolarRadiation->addProperty( 0.4170, 92.8481 );
		aSolarRadiation->addProperty( 0.4180, 93.0426 );
		aSolarRadiation->addProperty( 0.4190, 93.2372 );
		aSolarRadiation->addProperty( 0.4200, 93.4318 );
		aSolarRadiation->addProperty( 0.4210, 92.7568 );
		aSolarRadiation->addProperty( 0.4220, 92.0819 );
		aSolarRadiation->addProperty( 0.4230, 91.4069 );
		aSolarRadiation->addProperty( 0.4240, 90.7320 );
		aSolarRadiation->addProperty( 0.4250, 90.0570 );
		aSolarRadiation->addProperty( 0.4260, 89.3821 );
		aSolarRadiation->addProperty( 0.4270, 88.7071 );
		aSolarRadiation->addProperty( 0.4280, 88.0322 );
		aSolarRadiation->addProperty( 0.4290, 87.3572 );
		aSolarRadiation->addProperty( 0.4300, 86.6823 );
		aSolarRadiation->addProperty( 0.4310, 88.5006 );
		aSolarRadiation->addProperty( 0.4320, 90.3188 );
		aSolarRadiation->addProperty( 0.4330, 92.1371 );
		aSolarRadiation->addProperty( 0.4340, 93.9554 );
		aSolarRadiation->addProperty( 0.4350, 95.7736 );
		aSolarRadiation->addProperty( 0.4360, 97.5919 );
		aSolarRadiation->addProperty( 0.4370, 99.4102 );
		aSolarRadiation->addProperty( 0.4380, 101.2280 );
		aSolarRadiation->addProperty( 0.4390, 103.0470 );
		aSolarRadiation->addProperty( 0.4400, 104.8650 );
		aSolarRadiation->addProperty( 0.4410, 106.0790 );
		aSolarRadiation->addProperty( 0.4420, 107.2940 );
		aSolarRadiation->addProperty( 0.4430, 108.5080 );
		aSolarRadiation->addProperty( 0.4440, 109.7220 );
		aSolarRadiation->addProperty( 0.4450, 110.9360 );
		aSolarRadiation->addProperty( 0.4460, 112.1510 );
		aSolarRadiation->addProperty( 0.4470, 113.3650 );
		aSolarRadiation->addProperty( 0.4480, 114.5790 );
		aSolarRadiation->addProperty( 0.4490, 115.7940 );
		aSolarRadiation->addProperty( 0.4500, 117.0080 );
		aSolarRadiation->addProperty( 0.4510, 117.0880 );
		aSolarRadiation->addProperty( 0.4520, 117.1690 );
		aSolarRadiation->addProperty( 0.4530, 117.2490 );
		aSolarRadiation->addProperty( 0.4540, 117.3300 );
		aSolarRadiation->addProperty( 0.4550, 117.4100 );
		aSolarRadiation->addProperty( 0.4560, 117.4900 );
		aSolarRadiation->addProperty( 0.4570, 117.5710 );
		aSolarRadiation->addProperty( 0.4580, 117.6510 );
		aSolarRadiation->addProperty( 0.4590, 117.7320 );
		aSolarRadiation->addProperty( 0.4600, 117.8120 );
		aSolarRadiation->addProperty( 0.4610, 117.5170 );
		aSolarRadiation->addProperty( 0.4620, 117.2220 );
		aSolarRadiation->addProperty( 0.4630, 116.9270 );
		aSolarRadiation->addProperty( 0.4640, 116.6320 );
		aSolarRadiation->addProperty( 0.4650, 116.3360 );
		aSolarRadiation->addProperty( 0.4660, 116.0410 );
		aSolarRadiation->addProperty( 0.4670, 115.7460 );
		aSolarRadiation->addProperty( 0.4680, 115.4510 );
		aSolarRadiation->addProperty( 0.4690, 115.1560 );
		aSolarRadiation->addProperty( 0.4700, 114.8610 );
		aSolarRadiation->addProperty( 0.4710, 114.9670 );
		aSolarRadiation->addProperty( 0.4720, 115.0730 );
		aSolarRadiation->addProperty( 0.4730, 115.1800 );
		aSolarRadiation->addProperty( 0.4740, 115.2860 );
		aSolarRadiation->addProperty( 0.4750, 115.3920 );
		aSolarRadiation->addProperty( 0.4760, 115.4980 );
		aSolarRadiation->addProperty( 0.4770, 115.6040 );
		aSolarRadiation->addProperty( 0.4780, 115.7110 );
		aSolarRadiation->addProperty( 0.4790, 115.8170 );
		aSolarRadiation->addProperty( 0.4800, 115.9230 );
		aSolarRadiation->addProperty( 0.4810, 115.2120 );
		aSolarRadiation->addProperty( 0.4820, 114.5010 );
		aSolarRadiation->addProperty( 0.4830, 113.7890 );
		aSolarRadiation->addProperty( 0.4840, 113.0780 );
		aSolarRadiation->addProperty( 0.4850, 112.3670 );
		aSolarRadiation->addProperty( 0.4860, 111.6560 );
		aSolarRadiation->addProperty( 0.4870, 110.9450 );
		aSolarRadiation->addProperty( 0.4880, 110.2330 );
		aSolarRadiation->addProperty( 0.4890, 109.5220 );
		aSolarRadiation->addProperty( 0.4900, 108.8110 );
		aSolarRadiation->addProperty( 0.4910, 108.8650 );
		aSolarRadiation->addProperty( 0.4920, 108.9200 );
		aSolarRadiation->addProperty( 0.4930, 108.9740 );
		aSolarRadiation->addProperty( 0.4940, 109.0280 );
		aSolarRadiation->addProperty( 0.4950, 109.0820 );
		aSolarRadiation->addProperty( 0.4960, 109.1370 );
		aSolarRadiation->addProperty( 0.4970, 109.1910 );
		aSolarRadiation->addProperty( 0.4980, 109.2450 );
		aSolarRadiation->addProperty( 0.4990, 109.3000 );
		aSolarRadiation->addProperty( 0.5000, 109.3540 );
		aSolarRadiation->addProperty( 0.5010, 109.1990 );
		aSolarRadiation->addProperty( 0.5020, 109.0440 );
		aSolarRadiation->addProperty( 0.5030, 108.8880 );
		aSolarRadiation->addProperty( 0.5040, 108.7330 );
		aSolarRadiation->addProperty( 0.5050, 108.5780 );
		aSolarRadiation->addProperty( 0.5060, 108.4230 );
		aSolarRadiation->addProperty( 0.5070, 108.2680 );
		aSolarRadiation->addProperty( 0.5080, 108.1120 );
		aSolarRadiation->addProperty( 0.5090, 107.9570 );
		aSolarRadiation->addProperty( 0.5100, 107.8020 );
		aSolarRadiation->addProperty( 0.5110, 107.5010 );
		aSolarRadiation->addProperty( 0.5120, 107.2000 );
		aSolarRadiation->addProperty( 0.5130, 106.8980 );
		aSolarRadiation->addProperty( 0.5140, 106.5970 );
		aSolarRadiation->addProperty( 0.5150, 106.2960 );
		aSolarRadiation->addProperty( 0.5160, 105.9950 );
		aSolarRadiation->addProperty( 0.5170, 105.6940 );
		aSolarRadiation->addProperty( 0.5180, 105.3920 );
		aSolarRadiation->addProperty( 0.5190, 105.0910 );
		aSolarRadiation->addProperty( 0.5200, 104.7900 );
		aSolarRadiation->addProperty( 0.5210, 105.0800 );
		aSolarRadiation->addProperty( 0.5220, 105.3700 );
		aSolarRadiation->addProperty( 0.5230, 105.6600 );
		aSolarRadiation->addProperty( 0.5240, 105.9500 );
		aSolarRadiation->addProperty( 0.5250, 106.2390 );
		aSolarRadiation->addProperty( 0.5260, 106.5290 );
		aSolarRadiation->addProperty( 0.5270, 106.8190 );
		aSolarRadiation->addProperty( 0.5280, 107.1090 );
		aSolarRadiation->addProperty( 0.5290, 107.3990 );
		aSolarRadiation->addProperty( 0.5300, 107.6890 );
		aSolarRadiation->addProperty( 0.5310, 107.3610 );
		aSolarRadiation->addProperty( 0.5320, 107.0320 );
		aSolarRadiation->addProperty( 0.5330, 106.7040 );
		aSolarRadiation->addProperty( 0.5340, 106.3750 );
		aSolarRadiation->addProperty( 0.5350, 106.0470 );
		aSolarRadiation->addProperty( 0.5360, 105.7190 );
		aSolarRadiation->addProperty( 0.5370, 105.3900 );
		aSolarRadiation->addProperty( 0.5380, 105.0620 );
		aSolarRadiation->addProperty( 0.5390, 104.7330 );
		aSolarRadiation->addProperty( 0.5400, 104.4050 );
		aSolarRadiation->addProperty( 0.5410, 104.3690 );
		aSolarRadiation->addProperty( 0.5420, 104.3330 );
		aSolarRadiation->addProperty( 0.5430, 104.2970 );
		aSolarRadiation->addProperty( 0.5440, 104.2610 );
		aSolarRadiation->addProperty( 0.5450, 104.2250 );
		aSolarRadiation->addProperty( 0.5460, 104.1900 );
		aSolarRadiation->addProperty( 0.5470, 104.1540 );
		aSolarRadiation->addProperty( 0.5480, 104.1180 );
		aSolarRadiation->addProperty( 0.5490, 104.0820 );
		aSolarRadiation->addProperty( 0.5500, 104.0460 );
		aSolarRadiation->addProperty( 0.5510, 103.6410 );
		aSolarRadiation->addProperty( 0.5520, 103.2370 );
		aSolarRadiation->addProperty( 0.5530, 102.8320 );
		aSolarRadiation->addProperty( 0.5540, 102.4280 );
		aSolarRadiation->addProperty( 0.5550, 102.0230 );
		aSolarRadiation->addProperty( 0.5560, 101.6180 );
		aSolarRadiation->addProperty( 0.5570, 101.2140 );
		aSolarRadiation->addProperty( 0.5580, 100.8090 );
		aSolarRadiation->addProperty( 0.5590, 100.4050 );
		aSolarRadiation->addProperty( 0.5600, 100.0000 );
		aSolarRadiation->addProperty( 0.5610, 99.6334 );
		aSolarRadiation->addProperty( 0.5620, 99.2668 );
		aSolarRadiation->addProperty( 0.5630, 98.9003 );
		aSolarRadiation->addProperty( 0.5640, 98.5337 );
		aSolarRadiation->addProperty( 0.5650, 98.1671 );
		aSolarRadiation->addProperty( 0.5660, 97.8005 );
		aSolarRadiation->addProperty( 0.5670, 97.4339 );
		aSolarRadiation->addProperty( 0.5680, 97.0674 );
		aSolarRadiation->addProperty( 0.5690, 96.7008 );
		aSolarRadiation->addProperty( 0.5700, 96.3342 );
		aSolarRadiation->addProperty( 0.5710, 96.2796 );
		aSolarRadiation->addProperty( 0.5720, 96.2250 );
		aSolarRadiation->addProperty( 0.5730, 96.1703 );
		aSolarRadiation->addProperty( 0.5740, 96.1157 );
		aSolarRadiation->addProperty( 0.5750, 96.0611 );
		aSolarRadiation->addProperty( 0.5760, 96.0065 );
		aSolarRadiation->addProperty( 0.5770, 95.9519 );
		aSolarRadiation->addProperty( 0.5780, 95.8972 );
		aSolarRadiation->addProperty( 0.5790, 95.8426 );
		aSolarRadiation->addProperty( 0.5800, 95.7880 );
		aSolarRadiation->addProperty( 0.5810, 95.0778 );
		aSolarRadiation->addProperty( 0.5820, 94.3675 );
		aSolarRadiation->addProperty( 0.5830, 93.6573 );
		aSolarRadiation->addProperty( 0.5840, 92.9470 );
		aSolarRadiation->addProperty( 0.5850, 92.2368 );
		aSolarRadiation->addProperty( 0.5860, 91.5266 );
		aSolarRadiation->addProperty( 0.5870, 90.8163 );
		aSolarRadiation->addProperty( 0.5880, 90.1061 );
		aSolarRadiation->addProperty( 0.5890, 89.3958 );
		aSolarRadiation->addProperty( 0.5900, 88.6856 );
		aSolarRadiation->addProperty( 0.5910, 88.8177 );
		aSolarRadiation->addProperty( 0.5920, 88.9497 );
		aSolarRadiation->addProperty( 0.5930, 89.0818 );
		aSolarRadiation->addProperty( 0.5940, 89.2138 );
		aSolarRadiation->addProperty( 0.5950, 89.3459 );
		aSolarRadiation->addProperty( 0.5960, 89.4780 );
		aSolarRadiation->addProperty( 0.5970, 89.6100 );
		aSolarRadiation->addProperty( 0.5980, 89.7421 );
		aSolarRadiation->addProperty( 0.5990, 89.8741 );
		aSolarRadiation->addProperty( 0.6000, 90.0062 );
		aSolarRadiation->addProperty( 0.6010, 89.9655 );
		aSolarRadiation->addProperty( 0.6020, 89.9248 );
		aSolarRadiation->addProperty( 0.6030, 89.8841 );
		aSolarRadiation->addProperty( 0.6040, 89.8434 );
		aSolarRadiation->addProperty( 0.6050, 89.8026 );
		aSolarRadiation->addProperty( 0.6060, 89.7619 );
		aSolarRadiation->addProperty( 0.6070, 89.7212 );
		aSolarRadiation->addProperty( 0.6080, 89.6805 );
		aSolarRadiation->addProperty( 0.6090, 89.6398 );
		aSolarRadiation->addProperty( 0.6100, 89.5991 );
		aSolarRadiation->addProperty( 0.6110, 89.4091 );
		aSolarRadiation->addProperty( 0.6120, 89.2190 );
		aSolarRadiation->addProperty( 0.6130, 89.0290 );
		aSolarRadiation->addProperty( 0.6140, 88.8389 );
		aSolarRadiation->addProperty( 0.6150, 88.6489 );
		aSolarRadiation->addProperty( 0.6160, 88.4589 );
		aSolarRadiation->addProperty( 0.6170, 88.2688 );
		aSolarRadiation->addProperty( 0.6180, 88.0788 );
		aSolarRadiation->addProperty( 0.6190, 87.8887 );
		aSolarRadiation->addProperty( 0.6200, 87.6987 );
		aSolarRadiation->addProperty( 0.6210, 87.2577 );
		aSolarRadiation->addProperty( 0.6220, 86.8167 );
		aSolarRadiation->addProperty( 0.6230, 86.3757 );
		aSolarRadiation->addProperty( 0.6240, 85.9347 );
		aSolarRadiation->addProperty( 0.6250, 85.4936 );
		aSolarRadiation->addProperty( 0.6260, 85.0526 );
		aSolarRadiation->addProperty( 0.6270, 84.6116 );
		aSolarRadiation->addProperty( 0.6280, 84.1706 );
		aSolarRadiation->addProperty( 0.6290, 83.7296 );
		aSolarRadiation->addProperty( 0.6300, 83.2886 );
		aSolarRadiation->addProperty( 0.6310, 83.3297 );
		aSolarRadiation->addProperty( 0.6320, 83.3707 );
		aSolarRadiation->addProperty( 0.6330, 83.4118 );
		aSolarRadiation->addProperty( 0.6340, 83.4528 );
		aSolarRadiation->addProperty( 0.6350, 83.4939 );
		aSolarRadiation->addProperty( 0.6360, 83.5350 );
		aSolarRadiation->addProperty( 0.6370, 83.5760 );
		aSolarRadiation->addProperty( 0.6380, 83.6171 );
		aSolarRadiation->addProperty( 0.6390, 83.6581 );
		aSolarRadiation->addProperty( 0.6400, 83.6992 );
		aSolarRadiation->addProperty( 0.6410, 83.3320 );
		aSolarRadiation->addProperty( 0.6420, 82.9647 );
		aSolarRadiation->addProperty( 0.6430, 82.5975 );
		aSolarRadiation->addProperty( 0.6440, 82.2302 );
		aSolarRadiation->addProperty( 0.6450, 81.8630 );
		aSolarRadiation->addProperty( 0.6460, 81.4958 );
		aSolarRadiation->addProperty( 0.6470, 81.1285 );
		aSolarRadiation->addProperty( 0.6480, 80.7613 );
		aSolarRadiation->addProperty( 0.6490, 80.3940 );
		aSolarRadiation->addProperty( 0.6500, 80.0268 );
		aSolarRadiation->addProperty( 0.6510, 80.0456 );
		aSolarRadiation->addProperty( 0.6520, 80.0644 );
		aSolarRadiation->addProperty( 0.6530, 80.0831 );
		aSolarRadiation->addProperty( 0.6540, 80.1019 );
		aSolarRadiation->addProperty( 0.6550, 80.1207 );
		aSolarRadiation->addProperty( 0.6560, 80.1395 );
		aSolarRadiation->addProperty( 0.6570, 80.1583 );
		aSolarRadiation->addProperty( 0.6580, 80.1770 );
		aSolarRadiation->addProperty( 0.6590, 80.1958 );
		aSolarRadiation->addProperty( 0.6600, 80.2146 );
		aSolarRadiation->addProperty( 0.6610, 80.4209 );
		aSolarRadiation->addProperty( 0.6620, 80.6272 );
		aSolarRadiation->addProperty( 0.6630, 80.8336 );
		aSolarRadiation->addProperty( 0.6640, 81.0399 );
		aSolarRadiation->addProperty( 0.6650, 81.2462 );
		aSolarRadiation->addProperty( 0.6660, 81.4525 );
		aSolarRadiation->addProperty( 0.6670, 81.6588 );
		aSolarRadiation->addProperty( 0.6680, 81.8652 );
		aSolarRadiation->addProperty( 0.6690, 82.0715 );
		aSolarRadiation->addProperty( 0.6700, 82.2778 );
		aSolarRadiation->addProperty( 0.6710, 81.8784 );
		aSolarRadiation->addProperty( 0.6720, 81.4791 );
		aSolarRadiation->addProperty( 0.6730, 81.0797 );
		aSolarRadiation->addProperty( 0.6740, 80.6804 );
		aSolarRadiation->addProperty( 0.6750, 80.2810 );
		aSolarRadiation->addProperty( 0.6760, 79.8816 );
		aSolarRadiation->addProperty( 0.6770, 79.4823 );
		aSolarRadiation->addProperty( 0.6780, 79.0829 );
		aSolarRadiation->addProperty( 0.6790, 78.6836 );
		aSolarRadiation->addProperty( 0.6800, 78.2842 );
		aSolarRadiation->addProperty( 0.6810, 77.4279 );
		aSolarRadiation->addProperty( 0.6820, 76.5716 );
		aSolarRadiation->addProperty( 0.6830, 75.7153 );
		aSolarRadiation->addProperty( 0.6840, 74.8590 );
		aSolarRadiation->addProperty( 0.6850, 74.0027 );
		aSolarRadiation->addProperty( 0.6860, 73.1465 );
		aSolarRadiation->addProperty( 0.6870, 72.2902 );
		aSolarRadiation->addProperty( 0.6880, 71.4339 );
		aSolarRadiation->addProperty( 0.6890, 70.5776 );
		aSolarRadiation->addProperty( 0.6900, 69.7213 );
		aSolarRadiation->addProperty( 0.6910, 69.9101 );
		aSolarRadiation->addProperty( 0.6920, 70.0989 );
		aSolarRadiation->addProperty( 0.6930, 70.2876 );
		aSolarRadiation->addProperty( 0.6940, 70.4764 );
		aSolarRadiation->addProperty( 0.6950, 70.6652 );
		aSolarRadiation->addProperty( 0.6960, 70.8540 );
		aSolarRadiation->addProperty( 0.6970, 71.0428 );
		aSolarRadiation->addProperty( 0.6980, 71.2315 );
		aSolarRadiation->addProperty( 0.6990, 71.4203 );
		aSolarRadiation->addProperty( 0.7000, 71.6091 );
		aSolarRadiation->addProperty( 0.7010, 71.8831 );
		aSolarRadiation->addProperty( 0.7020, 72.1571 );
		aSolarRadiation->addProperty( 0.7030, 72.4311 );
		aSolarRadiation->addProperty( 0.7040, 72.7051 );
		aSolarRadiation->addProperty( 0.7050, 72.9790 );
		aSolarRadiation->addProperty( 0.7060, 73.2530 );
		aSolarRadiation->addProperty( 0.7070, 73.5270 );
		aSolarRadiation->addProperty( 0.7080, 73.8010 );
		aSolarRadiation->addProperty( 0.7090, 74.0750 );
		aSolarRadiation->addProperty( 0.7100, 74.3490 );
		aSolarRadiation->addProperty( 0.7110, 73.0745 );
		aSolarRadiation->addProperty( 0.7120, 71.8000 );
		aSolarRadiation->addProperty( 0.7130, 70.5255 );
		aSolarRadiation->addProperty( 0.7140, 69.2510 );
		aSolarRadiation->addProperty( 0.7150, 67.9765 );
		aSolarRadiation->addProperty( 0.7160, 66.7020 );
		aSolarRadiation->addProperty( 0.7170, 65.4275 );
		aSolarRadiation->addProperty( 0.7180, 64.1530 );
		aSolarRadiation->addProperty( 0.7190, 62.8785 );
		aSolarRadiation->addProperty( 0.7200, 61.6040 );
		aSolarRadiation->addProperty( 0.7210, 62.4322 );
		aSolarRadiation->addProperty( 0.7220, 63.2603 );
		aSolarRadiation->addProperty( 0.7230, 64.0885 );
		aSolarRadiation->addProperty( 0.7240, 64.9166 );
		aSolarRadiation->addProperty( 0.7250, 65.7448 );
		aSolarRadiation->addProperty( 0.7260, 66.5730 );
		aSolarRadiation->addProperty( 0.7270, 67.4011 );
		aSolarRadiation->addProperty( 0.7280, 68.2293 );
		aSolarRadiation->addProperty( 0.7290, 69.0574 );
		aSolarRadiation->addProperty( 0.7300, 69.8856 );
		aSolarRadiation->addProperty( 0.7310, 70.4057 );
		aSolarRadiation->addProperty( 0.7320, 70.9259 );
		aSolarRadiation->addProperty( 0.7330, 71.4460 );
		aSolarRadiation->addProperty( 0.7340, 71.9662 );
		aSolarRadiation->addProperty( 0.7350, 72.4863 );
		aSolarRadiation->addProperty( 0.7360, 73.0064 );
		aSolarRadiation->addProperty( 0.7370, 73.5266 );
		aSolarRadiation->addProperty( 0.7380, 74.0467 );
		aSolarRadiation->addProperty( 0.7390, 74.5669 );
		aSolarRadiation->addProperty( 0.7400, 75.0870 );
		aSolarRadiation->addProperty( 0.7410, 73.9376 );
		aSolarRadiation->addProperty( 0.7420, 72.7881 );
		aSolarRadiation->addProperty( 0.7430, 71.6387 );
		aSolarRadiation->addProperty( 0.7440, 70.4893 );
		aSolarRadiation->addProperty( 0.7450, 69.3398 );
		aSolarRadiation->addProperty( 0.7460, 68.1904 );
		aSolarRadiation->addProperty( 0.7470, 67.0410 );
		aSolarRadiation->addProperty( 0.7480, 65.8916 );
		aSolarRadiation->addProperty( 0.7490, 64.7421 );
		aSolarRadiation->addProperty( 0.7500, 63.5927 );
		aSolarRadiation->addProperty( 0.7510, 61.8752 );
		aSolarRadiation->addProperty( 0.7520, 60.1578 );
		aSolarRadiation->addProperty( 0.7530, 58.4403 );
		aSolarRadiation->addProperty( 0.7540, 56.7229 );
		aSolarRadiation->addProperty( 0.7550, 55.0054 );
		aSolarRadiation->addProperty( 0.7560, 53.2880 );
		aSolarRadiation->addProperty( 0.7570, 51.5705 );
		aSolarRadiation->addProperty( 0.7580, 49.8531 );
		aSolarRadiation->addProperty( 0.7590, 48.1356 );
		aSolarRadiation->addProperty( 0.7600, 46.4182 );
		aSolarRadiation->addProperty( 0.7610, 48.4569 );
		aSolarRadiation->addProperty( 0.7620, 50.4956 );
		aSolarRadiation->addProperty( 0.7630, 52.5344 );
		aSolarRadiation->addProperty( 0.7640, 54.5731 );
		aSolarRadiation->addProperty( 0.7650, 56.6118 );
		aSolarRadiation->addProperty( 0.7660, 58.6505 );
		aSolarRadiation->addProperty( 0.7670, 60.6892 );
		aSolarRadiation->addProperty( 0.7680, 62.7280 );
		aSolarRadiation->addProperty( 0.7690, 64.7667 );
		aSolarRadiation->addProperty( 0.7700, 66.8054 );
		aSolarRadiation->addProperty( 0.7710, 66.4631 );
		aSolarRadiation->addProperty( 0.7720, 66.1209 );
		aSolarRadiation->addProperty( 0.7730, 65.7786 );
		aSolarRadiation->addProperty( 0.7740, 65.4364 );
		aSolarRadiation->addProperty( 0.7750, 65.0941 );
		aSolarRadiation->addProperty( 0.7760, 64.7518 );
		aSolarRadiation->addProperty( 0.7770, 64.4096 );
		aSolarRadiation->addProperty( 0.7780, 64.0673 );
		aSolarRadiation->addProperty( 0.7790, 63.7251 );
		aSolarRadiation->addProperty( 0.7800, 63.3828 );
		aSolarRadiation->addProperty( 0.7810, 63.4749 );
		aSolarRadiation->addProperty( 0.7820, 63.5670 );
		aSolarRadiation->addProperty( 0.7830, 63.6592 );
		aSolarRadiation->addProperty( 0.7840, 63.7513 );
		aSolarRadiation->addProperty( 0.7850, 63.8434 );
		aSolarRadiation->addProperty( 0.7860, 63.9355 );
		aSolarRadiation->addProperty( 0.7870, 64.0276 );
		aSolarRadiation->addProperty( 0.7880, 64.1198 );
		aSolarRadiation->addProperty( 0.7890, 64.2119 );
		aSolarRadiation->addProperty( 0.7900, 64.3040 );
		aSolarRadiation->addProperty( 0.7910, 63.8188 );
		aSolarRadiation->addProperty( 0.7920, 63.3336 );
		aSolarRadiation->addProperty( 0.7930, 62.8484 );
		aSolarRadiation->addProperty( 0.7940, 62.3632 );
		aSolarRadiation->addProperty( 0.7950, 61.8779 );
		aSolarRadiation->addProperty( 0.7960, 61.3927 );
		aSolarRadiation->addProperty( 0.7970, 60.9075 );
		aSolarRadiation->addProperty( 0.7980, 60.4223 );
		aSolarRadiation->addProperty( 0.7990, 59.9371 );
		aSolarRadiation->addProperty( 0.8000, 59.4519 );
		aSolarRadiation->addProperty( 0.8010, 58.7026 );
		aSolarRadiation->addProperty( 0.8020, 57.9533 );
		aSolarRadiation->addProperty( 0.8030, 57.2040 );
		aSolarRadiation->addProperty( 0.8040, 56.4547 );
		aSolarRadiation->addProperty( 0.8050, 55.7054 );
		aSolarRadiation->addProperty( 0.8060, 54.9562 );
		aSolarRadiation->addProperty( 0.8070, 54.2069 );
		aSolarRadiation->addProperty( 0.8080, 53.4576 );
		aSolarRadiation->addProperty( 0.8090, 52.7083 );
		aSolarRadiation->addProperty( 0.8100, 51.9590 );
		aSolarRadiation->addProperty( 0.8110, 52.5072 );
		aSolarRadiation->addProperty( 0.8120, 53.0553 );
		aSolarRadiation->addProperty( 0.8130, 53.6035 );
		aSolarRadiation->addProperty( 0.8140, 54.1516 );
		aSolarRadiation->addProperty( 0.8150, 54.6998 );
		aSolarRadiation->addProperty( 0.8160, 55.2480 );
		aSolarRadiation->addProperty( 0.8170, 55.7961 );
		aSolarRadiation->addProperty( 0.8180, 56.3443 );
		aSolarRadiation->addProperty( 0.8190, 56.8924 );
		aSolarRadiation->addProperty( 0.8200, 57.4406 );
		aSolarRadiation->addProperty( 0.8210, 57.7278 );
		aSolarRadiation->addProperty( 0.8220, 58.0150 );
		aSolarRadiation->addProperty( 0.8230, 58.3022 );
		aSolarRadiation->addProperty( 0.8240, 58.5894 );
		aSolarRadiation->addProperty( 0.8250, 58.8765 );
		aSolarRadiation->addProperty( 0.8260, 59.1637 );
		aSolarRadiation->addProperty( 0.8270, 59.4509 );
		aSolarRadiation->addProperty( 0.8280, 59.7381 );
		aSolarRadiation->addProperty( 0.8290, 60.0253 );
		aSolarRadiation->addProperty( 0.8300, 60.3125 );

		return aSolarRadiation;
	}

	std::shared_ptr< std::vector< double > > getWavelengths() const {
		auto aWavelengths = std::make_shared< std::vector< double > >();

		aWavelengths->push_back( 0.380 );
		aWavelengths->push_back( 0.385 );
		aWavelengths->push_back( 0.390 );
		aWavelengths->push_back( 0.395 );
		aWavelengths->push_back( 0.400 );
		aWavelengths->push_back( 0.405 );
		aWavelengths->push_back( 0.410 );
		aWavelengths->push_back( 0.415 );
		aWavelengths->push_back( 0.420 );
		aWavelengths->push_back( 0.425 );
		aWavelengths->push_back( 0.430 );
		aWavelengths->push_back( 0.435 );
		aWavelengths->push_back( 0.440 );
		aWavelengths->push_back( 0.445 );
		aWavelengths->push_back( 0.450 );
		aWavelengths->push_back( 0.455 );
		aWavelengths->push_back( 0.460 );
		aWavelengths->push_back( 0.465 );
		aWavelengths->push_back( 0.470 );
		aWavelengths->push_back( 0.475 );
		aWavelengths->push_back( 0.480 );
		aWavelengths->push_back( 0.485 );
		aWavelengths->push_back( 0.490 );
		aWavelengths->push_back( 0.495 );
		aWavelengths->push_back( 0.500 );
		aWavelengths->push_back( 0.505 );
		aWavelengths->push_back( 0.510 );
		aWavelengths->push_back( 0.515 );
		aWavelengths->push_back( 0.520 );
		aWavelengths->push_back( 0.525 );
		aWavelengths->push_back( 0.530 );
		aWavelengths->push_back( 0.535 );
		aWavelengths->push_back( 0.540 );
		aWavelengths->push_back( 0.545 );
		aWavelengths->push_back( 0.550 );
		aWavelengths->push_back( 0.555 );
		aWavelengths->push_back( 0.560 );
		aWavelengths->push_back( 0.565 );
		aWavelengths->push_back( 0.570 );
		aWavelengths->push_back( 0.575 );
		aWavelengths->push_back( 0.580 );
		aWavelengths->push_back( 0.585 );
		aWavelengths->push_back( 0.590 );
		aWavelengths->push_back( 0.595 );
		aWavelengths->push_back( 0.600 );
		aWavelengths->push_back( 0.605 );
		aWavelengths->push_back( 0.610 );
		aWavelengths->push_back( 0.615 );
		aWavelengths->push_back( 0.620 );
		aWavelengths->push_back( 0.625 );
		aWavelengths->push_back( 0.630 );
		aWavelengths->push_back( 0.635 );
		aWavelengths->push_back( 0.640 );
		aWavelengths->push_back( 0.645 );
		aWavelengths->push_back( 0.650 );
		aWavelengths->push_back( 0.655 );
		aWavelengths->push_back( 0.660 );
		aWavelengths->push_back( 0.665 );
		aWavelengths->push_back( 0.670 );
		aWavelengths->push_back( 0.675 );
		aWavelengths->push_back( 0.680 );
		aWavelengths->push_back( 0.685 );
		aWavelengths->push_back( 0.690 );
		aWavelengths->push_back( 0.695 );
		aWavelengths->push_back( 0.700 );
		aWavelengths->push_back( 0.705 );
		aWavelengths->push_back( 0.710 );
		aWavelengths->push_back( 0.715 );
		aWavelengths->push_back( 0.720 );
		aWavelengths->push_back( 0.725 );
		aWavelengths->push_back( 0.730 );
		aWavelengths->push_back( 0.735 );
		aWavelengths->push_back( 0.740 );
		aWavelengths->push_back( 0.745 );
		aWavelengths->push_back( 0.750 );
		aWavelengths->push_back( 0.755 );
		aWavelengths->push_back( 0.760 );
		aWavelengths->push_back( 0.765 );
		aWavelengths->push_back( 0.770 );
		aWavelengths->push_back( 0.775 );
		aWavelengths->push_back( 0.780 );

		return aWavelengths;
	}

	std::shared_ptr< CSeries > getDetecorData() const {
		auto detectorData = std::make_shared< CSeries >();

		detectorData->addProperty( 0.380, 0.0000 );
		detectorData->addProperty( 0.385, 0.0001 );
		detectorData->addProperty( 0.390, 0.0001 );
		detectorData->addProperty( 0.395, 0.0002 );
		detectorData->addProperty( 0.400, 0.0004 );
		detectorData->addProperty( 0.405, 0.0006 );
		detectorData->addProperty( 0.410, 0.0012 );
		detectorData->addProperty( 0.415, 0.0022 );
		detectorData->addProperty( 0.420, 0.0040 );
		detectorData->addProperty( 0.425, 0.0073 );
		detectorData->addProperty( 0.430, 0.0116 );
		detectorData->addProperty( 0.435, 0.0168 );
		detectorData->addProperty( 0.440, 0.0230 );
		detectorData->addProperty( 0.445, 0.0298 );
		detectorData->addProperty( 0.450, 0.0380 );
		detectorData->addProperty( 0.455, 0.0480 );
		detectorData->addProperty( 0.460, 0.0600 );
		detectorData->addProperty( 0.465, 0.0739 );
		detectorData->addProperty( 0.470, 0.0910 );
		detectorData->addProperty( 0.475, 0.1126 );
		detectorData->addProperty( 0.480, 0.1390 );
		detectorData->addProperty( 0.485, 0.1693 );
		detectorData->addProperty( 0.490, 0.2080 );
		detectorData->addProperty( 0.495, 0.2586 );
		detectorData->addProperty( 0.500, 0.3230 );
		detectorData->addProperty( 0.505, 0.4073 );
		detectorData->addProperty( 0.510, 0.5030 );
		detectorData->addProperty( 0.515, 0.6082 );
		detectorData->addProperty( 0.520, 0.7100 );
		detectorData->addProperty( 0.525, 0.7932 );
		detectorData->addProperty( 0.530, 0.8620 );
		detectorData->addProperty( 0.535, 0.9149 );
		detectorData->addProperty( 0.540, 0.9540 );
		detectorData->addProperty( 0.545, 0.9803 );
		detectorData->addProperty( 0.550, 0.9950 );
		detectorData->addProperty( 0.555, 1.0000 );
		detectorData->addProperty( 0.560, 0.9950 );
		detectorData->addProperty( 0.565, 0.9786 );
		detectorData->addProperty( 0.570, 0.9520 );
		detectorData->addProperty( 0.575, 0.9154 );
		detectorData->addProperty( 0.580, 0.8700 );
		detectorData->addProperty( 0.585, 0.8163 );
		detectorData->addProperty( 0.590, 0.7570 );
		detectorData->addProperty( 0.595, 0.6949 );
		detectorData->addProperty( 0.600, 0.6310 );
		detectorData->addProperty( 0.605, 0.5668 );
		detectorData->addProperty( 0.610, 0.5030 );
		detectorData->addProperty( 0.615, 0.4412 );
		detectorData->addProperty( 0.620, 0.3810 );
		detectorData->addProperty( 0.625, 0.3210 );
		detectorData->addProperty( 0.630, 0.2650 );
		detectorData->addProperty( 0.635, 0.2170 );
		detectorData->addProperty( 0.640, 0.1750 );
		detectorData->addProperty( 0.645, 0.1382 );
		detectorData->addProperty( 0.650, 0.1070 );
		detectorData->addProperty( 0.655, 0.0816 );
		detectorData->addProperty( 0.660, 0.0610 );
		detectorData->addProperty( 0.665, 0.0446 );
		detectorData->addProperty( 0.670, 0.0320 );
		detectorData->addProperty( 0.675, 0.0232 );
		detectorData->addProperty( 0.680, 0.0170 );
		detectorData->addProperty( 0.685, 0.0119 );
		detectorData->addProperty( 0.690, 0.0082 );
		detectorData->addProperty( 0.695, 0.0057 );
		detectorData->addProperty( 0.700, 0.0041 );
		detectorData->addProperty( 0.705, 0.0029 );
		detectorData->addProperty( 0.710, 0.0021 );
		detectorData->addProperty( 0.715, 0.0015 );
		detectorData->addProperty( 0.720, 0.0010 );
		detectorData->addProperty( 0.725, 0.0007 );
		detectorData->addProperty( 0.730, 0.0005 );
		detectorData->addProperty( 0.735, 0.0004 );
		detectorData->addProperty( 0.740, 0.0002 );
		detectorData->addProperty( 0.745, 0.0002 );
		detectorData->addProperty( 0.750, 0.0001 );
		detectorData->addProperty( 0.755, 0.0001 );
		detectorData->addProperty( 0.760, 0.0001 );
		detectorData->addProperty( 0.765, 0.0000 );
		detectorData->addProperty( 0.770, 0.0000 );
		detectorData->addProperty( 0.775, 0.0000 );
		detectorData->addProperty( 0.780, 0.0000 );

		return detectorData;
	}

	std::shared_ptr< CSpectralSampleData > getMeasurements() const {
		auto aMeasurements = std::make_shared< CSpectralSampleData >();

		aMeasurements->addRecord( 0.300, 0.0000, 0.0470, 0.0490 );
		aMeasurements->addRecord( 0.305, 0.0050, 0.0470, 0.0490 );
		aMeasurements->addRecord( 0.310, 0.0000, 0.0470, 0.0480 );
		aMeasurements->addRecord( 0.315, 0.0030, 0.0460, 0.0480 );
		aMeasurements->addRecord( 0.320, 0.0190, 0.0460, 0.0480 );
		aMeasurements->addRecord( 0.325, 0.0660, 0.0450, 0.0460 );
		aMeasurements->addRecord( 0.330, 0.1600, 0.0450, 0.0470 );
		aMeasurements->addRecord( 0.335, 0.2940, 0.0490, 0.0500 );
		aMeasurements->addRecord( 0.340, 0.4370, 0.0550, 0.0560 );
		aMeasurements->addRecord( 0.345, 0.5660, 0.0620, 0.0620 );
		aMeasurements->addRecord( 0.350, 0.6710, 0.0690, 0.0690 );
		aMeasurements->addRecord( 0.355, 0.7440, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.360, 0.7930, 0.0780, 0.0780 );
		aMeasurements->addRecord( 0.365, 0.8220, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.370, 0.8320, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.375, 0.8190, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.380, 0.8090, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.385, 0.8290, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.390, 0.8530, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.395, 0.8680, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.400, 0.8750, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.410, 0.8750, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.420, 0.8730, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.430, 0.8730, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.440, 0.8730, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.450, 0.8800, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.460, 0.8870, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.470, 0.8900, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.480, 0.8920, 0.0830, 0.0830 );
		aMeasurements->addRecord( 0.490, 0.8930, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.500, 0.8940, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.510, 0.8950, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.520, 0.8950, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.530, 0.8940, 0.0820, 0.0820 );
		aMeasurements->addRecord( 0.540, 0.8930, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.550, 0.8910, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.560, 0.8880, 0.0810, 0.0810 );
		aMeasurements->addRecord( 0.570, 0.8840, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.580, 0.8810, 0.0800, 0.0800 );
		aMeasurements->addRecord( 0.590, 0.8760, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.600, 0.8710, 0.0790, 0.0790 );
		aMeasurements->addRecord( 0.610, 0.8650, 0.0780, 0.0780 );
		aMeasurements->addRecord( 0.620, 0.8590, 0.0770, 0.0770 );
		aMeasurements->addRecord( 0.630, 0.8530, 0.0770, 0.0770 );
		aMeasurements->addRecord( 0.640, 0.8470, 0.0760, 0.0760 );
		aMeasurements->addRecord( 0.650, 0.8400, 0.0750, 0.0750 );
		aMeasurements->addRecord( 0.660, 0.8330, 0.0750, 0.0750 );
		aMeasurements->addRecord( 0.670, 0.8260, 0.0740, 0.0740 );
		aMeasurements->addRecord( 0.680, 0.8180, 0.0730, 0.0730 );
		aMeasurements->addRecord( 0.690, 0.8100, 0.0730, 0.0730 );
		aMeasurements->addRecord( 0.700, 0.8020, 0.0720, 0.0720 );
		aMeasurements->addRecord( 0.710, 0.7940, 0.0710, 0.0720 );
		aMeasurements->addRecord( 0.720, 0.7860, 0.0710, 0.0710 );
		aMeasurements->addRecord( 0.730, 0.7770, 0.0700, 0.0700 );
		aMeasurements->addRecord( 0.740, 0.7690, 0.0690, 0.0700 );
		aMeasurements->addRecord( 0.750, 0.7610, 0.0690, 0.0690 );
		aMeasurements->addRecord( 0.760, 0.7520, 0.0680, 0.0680 );
		aMeasurements->addRecord( 0.770, 0.7440, 0.0670, 0.0680 );
		aMeasurements->addRecord( 0.780, 0.7360, 0.0670, 0.0670 );
		aMeasurements->addRecord( 0.790, 0.7290, 0.0660, 0.0660 );
		aMeasurements->addRecord( 0.800, 0.7220, 0.0660, 0.0660 );
		aMeasurements->addRecord( 0.810, 0.7150, 0.0650, 0.0660 );
		aMeasurements->addRecord( 0.820, 0.7100, 0.0650, 0.0650 );
		aMeasurements->addRecord( 0.830, 0.7020, 0.0640, 0.0650 );
		aMeasurements->addRecord( 0.840, 0.6980, 0.0640, 0.0640 );
		aMeasurements->addRecord( 0.850, 0.6900, 0.0630, 0.0640 );
		aMeasurements->addRecord( 0.860, 0.6870, 0.0650, 0.0650 );
		aMeasurements->addRecord( 0.870, 0.6810, 0.0670, 0.0670 );
		aMeasurements->addRecord( 0.880, 0.6770, 0.0650, 0.0660 );
		aMeasurements->addRecord( 0.890, 0.6730, 0.0660, 0.0660 );
		aMeasurements->addRecord( 0.900, 0.6700, 0.0650, 0.0660 );
		aMeasurements->addRecord( 0.910, 0.6670, 0.0650, 0.0650 );
		aMeasurements->addRecord( 0.920, 0.6640, 0.0640, 0.0640 );
		aMeasurements->addRecord( 0.930, 0.6600, 0.0630, 0.0630 );
		aMeasurements->addRecord( 0.940, 0.6580, 0.0640, 0.0640 );
		aMeasurements->addRecord( 0.950, 0.6560, 0.0630, 0.0630 );
		aMeasurements->addRecord( 0.960, 0.6540, 0.0610, 0.0610 );
		aMeasurements->addRecord( 0.970, 0.6530, 0.0620, 0.0620 );
		aMeasurements->addRecord( 0.980, 0.6510, 0.0610, 0.0620 );
		aMeasurements->addRecord( 0.990, 0.6490, 0.0610, 0.0620 );
		aMeasurements->addRecord( 1.000, 0.6480, 0.0590, 0.0600 );
		aMeasurements->addRecord( 1.050, 0.6450, 0.0590, 0.0600 );
		aMeasurements->addRecord( 1.100, 0.6450, 0.0580, 0.0590 );
		aMeasurements->addRecord( 1.150, 0.6470, 0.0590, 0.0590 );
		aMeasurements->addRecord( 1.200, 0.6530, 0.0590, 0.0590 );
		aMeasurements->addRecord( 1.250, 0.6610, 0.0580, 0.0590 );
		aMeasurements->addRecord( 1.300, 0.6730, 0.0600, 0.0600 );
		aMeasurements->addRecord( 1.350, 0.6870, 0.0600, 0.0600 );
		aMeasurements->addRecord( 1.400, 0.7020, 0.0610, 0.0610 );
		aMeasurements->addRecord( 1.450, 0.7220, 0.0610, 0.0620 );
		aMeasurements->addRecord( 1.500, 0.7410, 0.0630, 0.0640 );
		aMeasurements->addRecord( 1.550, 0.7570, 0.0630, 0.0640 );
		aMeasurements->addRecord( 1.600, 0.7690, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.650, 0.7750, 0.0650, 0.0640 );
		aMeasurements->addRecord( 1.700, 0.7790, 0.0640, 0.0650 );
		aMeasurements->addRecord( 1.750, 0.7790, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.800, 0.7770, 0.0650, 0.0650 );
		aMeasurements->addRecord( 1.850, 0.7760, 0.0650, 0.0630 );
		aMeasurements->addRecord( 1.900, 0.7730, 0.0620, 0.0620 );
		aMeasurements->addRecord( 1.950, 0.7730, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.000, 0.7720, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.050, 0.7740, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.100, 0.7750, 0.0640, 0.0650 );
		aMeasurements->addRecord( 2.150, 0.7730, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.200, 0.7580, 0.0640, 0.0650 );
		aMeasurements->addRecord( 2.250, 0.7590, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.300, 0.7660, 0.0650, 0.0650 );
		aMeasurements->addRecord( 2.350, 0.7670, 0.0640, 0.0650 );
		aMeasurements->addRecord( 2.400, 0.7660, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.450, 0.7570, 0.0640, 0.0640 );
		aMeasurements->addRecord( 2.500, 0.7500, 0.0630, 0.0630 );

		return aMeasurements;
	}

	void SetUp() override {

		auto aSolarRadiation = getSolarRadiation();
		auto aWavelengths = getWavelengths();
		auto detectorData = getDetecorData();
		auto aMeasurements = getMeasurements();

		m_Sample = std::make_shared< CSpectralSample >( aMeasurements, aSolarRadiation );
		m_Sample->setDetectorData( detectorData );

		m_Sample->setWavelengths( WavelengthSet::Custom, aWavelengths );

	}

public:
	std::shared_ptr< CSpectralSample > getSample() const {
		return m_Sample;
	};

};

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties0degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 0.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.883647, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.080395, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.080395, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties10degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 10.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.883411, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.080401, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.080401, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties20degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 20.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.882397, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.080729, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.080729, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties30degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 30.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.879453, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.082542, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.082542, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties40degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 40.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.871710, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.088749, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.088750, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties50degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 50.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.852261, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.106358, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.106358, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties60degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 60.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.804351, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.152339, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.152339, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties70degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 70.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.688205, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.266908, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.266908, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties80degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 80.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0.427266, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 0.527714, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 0.527714, reflectanceBack, 1e-6 );

}

TEST_F( TestSampleNFRC_103_Angular_Photopic, TestProperties90degrees ) {

	auto thickness = 5.715e-3; // [m]
	auto layerType = MaterialType::Monolithic;

	auto angle = 90.0;

	auto aMeasuredSample = getSample();

	auto angularSample = std::make_shared< CAngularSpectralSample >( aMeasuredSample, thickness, layerType );

	// VISIBLE (PHOTOPIC) RANGE
	auto lowLambda = 0.38;
	auto highLambda = 0.78;

	auto transmittance = angularSample->getProperty( lowLambda, highLambda, Property::T, Side::Front, angle );
	EXPECT_NEAR( 0, transmittance, 1e-6 );

	auto reflectanceFront = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Front, angle );
	EXPECT_NEAR( 1, reflectanceFront, 1e-6 );

	auto reflectanceBack = angularSample->getProperty( lowLambda, highLambda, Property::R, Side::Back, angle );
	EXPECT_NEAR( 1, reflectanceBack, 1e-6 );

}
