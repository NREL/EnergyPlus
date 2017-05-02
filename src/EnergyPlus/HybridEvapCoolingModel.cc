
#include <HybridEvapCoolingModel.hh>

#include <UtilityRoutines.hh>

#include <string>
#include <list>
#include <math.h>
#include <windows.h>
#include <ScheduleManager.hh>
#include <General.hh>

namespace EnergyPlus {//***************

	namespace HybridEvapCoolingModel {
		using namespace std;
		typedef int* MyType;
		map<string, string> testop;
		#define  DEF_Tdb 0
		#define  DEF_RH 1
		#define  TEMP_CURVE 0 
		#define  W_CURVE 1
		#define  POWER_CURVE 2

				// constructor of Model,

		#include <fstream>
		fstream logfile;
		//ZoneHybridUnitaryACSystem
		Model::Model() :
				Tsa_schedule_pointer(0),
				ZoneNodeNum(0),
				MsaCapacityRatedCond(0),
				SchedPtr(0),
				UnitTotalCoolingRate(0.0),
				UnitTotalCoolingEnergy(0.0),
				UnitSensibleCoolingRate(0.0),
				UnitSensibleCoolingEnergy(0.0),
				RequestedLoadToCoolingSetpoint(0.0),
				Mode(0),
				ErrorCode(0),
				InletNode(0),
				OutletNode(0),
				SecondaryInletNode(0),
				SecondaryOutletNode(0),
				InletMassFlowRate(0.0),
				InletTemp(0.0),
				InletWetBulbTemp(0.0),
				InletHumRat(0.0),
				InletEnthalpy(0.0),
				InletPressure(0.0),
				InletRH(0.0),
				OutletMassFlowRate(0.0),
				OutletTemp(0.0),
				OutletWetBulbTemp(0.0),
				OutletHumRat(0.0),
				OutletEnthalpy(0.0),
				OutletPressure(0.0),
				OutletRH(0.0),
				SecInletMassFlowRate(0.0),
				SecInletTemp(0.0),
				SecInletWetBulbTemp(0.0),
				SecInletHumRat(0.0),
				SecInletEnthalpy(0.0),
				SecInletPressure(0.0),
				SecInletRH(0.0),
				SecOutletMassFlowRate(0.0),
				SecOutletTemp(0.0),
				SecOutletWetBulbTemp(0.0),
				SecOutletHumRat(0.0),
				SecOutletEnthalpy(0.0),
				SecOutletPressure(0.0),
				SecOutletRH(0.0), 
				Wsa(0.0),
			Initialized(false)
		{
			//InitializeModelParams();
			Minimum_Supply_Air_Temp = 8; //must set this propoerly 
			cp = 1; //kJ/degreesC.kg ePlus should cary this as an avialble variable
			Lambna = 2260; //(kJ/kg) latent heat of vaporization ePlus should carry this an available variable
			count_EnvironmentConditionsMetOnce = 0;
			count_SAHR_OC_MetOnce = 0;
			count_SAT_OC_MetOnce = 0;
			count_DidWeMeetLoad = 0;
			InitializeModelParams();
		}
		//*************************************************

		void Model::InitializeModelParams()
		{
			optimal_EnvCondMet = false;
			optimal_power = 10e+10;
			optimal_Msa = 0;
			optimal_OSAF = 0;
			optimal_H_sensible_room = 0;
			optimal_TotalSystemH = 0;
			optimal_SHR = 0;
			optimal_Mvent = 0;
			optimal_Mode = 0;
			optimal_Point = 0; //this is used to identify the point that
			optimal_Wsa = 0;
			optimal_Tsa = 0;
			Tsa = 0;
			//Wsa = 0;
			DidWeMeetLoad = 0;
			RunningPeakCapacity_EnvCondMet = false;
			RunningPeakCapacity_power = 10e+10;
			RunningPeakCapacity_Msa = 0;
			RunningPeakCapacity_OSAF = 0;
			RunningPeakCapacity_H_sensible_room = 0;
			RunningPeakCapacity_TotalSystemH = 0;
			RunningPeakCapacity_SHR = 0;
			RunningPeakCapacity_Mvent = 0;
			RunningPeakCapacity_Mode = 0;
			RunningPeakCapacity_Wsa = 0;
			RunningPeakCapacity_Tsa = 0;
			RunningPeakCapacity_Point = 0;
			RequestedLoad_t_n1 = 0;
			RequestedLoad_t_n2 = 0;
			RequestedLoad_t_n3 = 0;
			RequestedLoad_t_n4 = 0;
			RequestedLoad_t_n5 = 0;
			RequestedLoad_t_n6 = 0;
			RequestedLoad_t_n7 = 0;
			RequestedLoad_t_n8 = 0;

		}

		Model::~Model()                 // destructor, just an example
		{
			list<CModeSolutionSpace*>::iterator iter;
			for (iter = XandYPoints.begin(); iter != XandYPoints.end(); ++iter) {
				CModeSolutionSpace* p = (*iter);
				delete p;
			}
		}

		// GetAge, Public accessor function
		// returns value of itsAge member
		int Model::GetID()
		{
			return ID;
		}


		double Model::EstimateQRemaining(double TroomTemp, Real64 communicationStepSize) {
			double Q;
			double volume = 800; //m3
			double mass = volume*1.2;
			double TheatingSetpoint = 18;
			double TcoolingSetpoint = 24;

			Q = 0;
			if (TroomTemp > TcoolingSetpoint) {
				Q = -((TroomTemp - TcoolingSetpoint)*mass * 1006) / (2 * communicationStepSize);
			}

			if (TroomTemp < TheatingSetpoint) {
				Q = ((TheatingSetpoint - TroomTemp)*mass * 1006) / (2 * communicationStepSize);
			}


			return -Q;
		}
		void Model::Initialize(std::string fmuLocation)//, ConfigFile* pConfig)
		{
			if (Initialized) return;
			std::string dir;
			std::string file;
			std::string strfmuLocation(fmuLocation);
			
			//	Config->TrimFilename(strfmuLocation,dir);
			//	cout<<"debug 1"<<strfmuLocation;
			Config = new ConfigFile;
			Config->ParseConfigFile(strfmuLocation);
			Initialized = true;
			//Iterate through modes of operation
			ResolutionX = 0.02; //msa/msaRATED
			ResolutionY = 0.02; //OSAF as absolute fraction (not %)
								// get system values

			RatedH = Config->GetRatedH();
			//	MinOA_Msa=Config->GetMinVR();
			//	MsaRated=Config->GetRatedMsa();
			//	Generate point matrix using Tessellate function from operatiing conditions
			//store in  XandYPoints
			//int OC_N=Config->OperatingConditionN;
			vector<double>  Xvals;
			vector<double>  Yvals;
			while (Config->GetNextOpConst(Xvals, Yvals))//i=0,i<>OC_N,i++)
			{
				try
				{
					CModeSolutionSpace* sol = Tessellate(Xvals, Yvals);//first pass points from 609 are zero
					XandYPoints.push_back(sol);
				}
				catch (int e)
				{
					ShowFatalError("An exception occurred in Tessellate. Exception Nr");
					//cout << "An exception occurred in Tessellate. Exception Nr. " << e << '\n';
					//Sleep(100000);
					return;
				}
			}
			Initialized = true;
		}


		void Model::ModelLog(std::string fmi_logmessage)
		{
			std::string logmessage(fmi_logmessage);
			cout << logmessage;
		}

		double Model::Round(double x)
		{
			return floor(x + 0.5);
		}

		double Model::CalculateCurveVal(double X_0, double X_1, double X_2, double X_3, double X_4, double X_5, double X_6, int mode_number, int curve_ID)
		{
			double Y_val = 0;
			vector<double> curve2;
			int CurveNumber = 3 * mode_number + curve_ID;
			ConfigDataItem * curve = Config->GetBcoefficientCurve(CurveNumber);
			vector<double>  B = ((*curve).Items);
			double B0 = B[0];
			double t1 = B[0] * (X_0 * X_0);
			double t2 = B[1] * (X_0 * X_1);
			double t3 = B[2] * (X_0 * X_2);
			double t4 = B[3] * (X_0 * X_3);
			double t5 = B[4] * (X_0 * X_4);
			double t6 = B[5] * (X_0 * X_5);
			double t7 = B[6] * (X_0 * X_6);

			Y_val = B[0] * (X_0 * X_0) + B[1] * (X_0 * X_1) + B[2] * (X_0 * X_2) + B[3] * (X_0 * X_3) + B[4] * (X_0 * X_4) + B[5] * (X_0 * X_5) + B[6] * (X_0 * X_6);
			Y_val = Y_val + B[7] * (X_1 * X_1) + B[8] * (X_1 * X_2) + B[9] * (X_1 * X_3) + B[10] * (X_1 * X_4) + B[11] * (X_1 * X_5) + B[12] * (X_1 * X_6);
			Y_val = Y_val + B[13] * (X_2 * X_2) + B[14] * (X_2 * X_3) + B[15] * (X_2 * X_4) + B[16] * (X_2 * X_5) + B[17] * (X_2 * X_6);
			Y_val = Y_val + B[18] * (X_3 * X_3) + B[19] * (X_3 * X_4) + B[20] * (X_3 * X_5) + B[21] * (X_3 * X_6);
			Y_val = Y_val + B[22] * (X_4 * X_4) + B[23] * (X_4 * X_5) + B[24] * (X_4 * X_6);
			Y_val = Y_val + B[25] * (X_5 * X_5) + B[26] * (X_5 * X_6);
			Y_val = Y_val + B[27] * (X_6 * X_6);
			return Y_val;
		}

		CModeSolutionSpace* Model::Tessellate(vector<double>&  Xvals, vector<double>&  Yvals)
		{
			double X1, X2, X3, X4, Y1, Y2, Y3, Y4;
			bool RotationFlag;
			vector<int> AscendingOrder(4);
			vector<double> StartY(3), EndY(3), RangeY(3), StepY(3), StartX, EndX, RangeX, StepX, PointBuffer;
			vector<int> nY(3), nX;
			double MaxX, MinX, dxdyLeft, dxdyRight;
			int nMax;

			X1 = Xvals[0];
			X2 = Xvals[1];
			X3 = Xvals[2];
			X4 = Xvals[3];
			Y1 = Yvals[0];
			Y2 = Yvals[1];
			Y3 = Yvals[2];
			Y4 = Yvals[3];
			CModeSolutionSpace *Solution = new CModeSolutionSpace;

			if ((Y2 >= Y1 && Y2 >= Y3) || (Y3 <= Y2 && Y3 <= Y4))
			{
				RotationFlag = true;
				if (X4 < X1) {
					static const double arrX[] = { X4, X3, X2, X1 };
					static const double arrY[] = { Y4, Y3, Y2, Y1 };
					vector<double> vecX(arrX, arrX + sizeof(arrX) / sizeof(arrX[0]));
					PolygonYs = vecX; //so becuase this is rotated the X's are Y's or something like that.

					vector<double> vecY(arrY, arrY + sizeof(arrY) / sizeof(arrY[0]));
					PolygonXs = vecY; //so becuase this is rotated the X's are Y's or something like that.
				}
				else
				{
					static const double arrX[] = { X1, X4, X3, X2 };
					static const double arrY[] = { Y1, Y4, Y3, Y2 };
					vector<double> vecX(arrX, arrX + sizeof(arrX) / sizeof(arrX[0]));
					PolygonYs = vecX; //so becuase this is rotated the X's are Y's or something like that.

					vector<double> vecY(arrY, arrY + sizeof(arrY) / sizeof(arrY[0]));
					PolygonXs = vecY; //so becuase this is rotated the X's are Y's or something like that.
				}
			}
			else
			{
				RotationFlag = false;
				static const double arrX[] = { X1, X2, X3, X4 };
				static const double arrY[] = { Y1, Y2, Y3, Y4 };
				vector<double> vecX(arrX, arrX + sizeof(arrX) / sizeof(arrX[0]));
				PolygonXs = vecX;

				vector<double> vecY(arrY, arrY + sizeof(arrY) / sizeof(arrY[0]));
				PolygonYs = vecY;
			}
			//Determine the ascending order of the points in the Y direction
			//Store the order indices in "AscendingOrder"
			AscendingOrder[0] = 0;
			if (PolygonYs[1] < PolygonYs[2])
			{
				AscendingOrder[1] = 1;
				AscendingOrder[2] = 2;
			}
			else
			{
				AscendingOrder[1] = 2;
				AscendingOrder[2] = 1;
			}

			if (PolygonYs[3] < PolygonYs[AscendingOrder[1]])
			{
				AscendingOrder[3] = AscendingOrder[2];
				AscendingOrder[2] = AscendingOrder[1];
				AscendingOrder[1] = 3;
			}
			else if (PolygonYs[3] < PolygonYs[AscendingOrder[2]])
			{
				AscendingOrder[3] = AscendingOrder[2];
				AscendingOrder[2] = 3;
			}
			else
			{
				AscendingOrder[3] = 3;
			}

			//Divide the Y direction into discrete sections separated by the polygon points
			//First section
			StartY[0] = PolygonYs[AscendingOrder[0]];
			EndY[0] = PolygonYs[AscendingOrder[1]];
			RangeY[0] = EndY[0] - StartY[0];
			nY[0] = Round(RangeY[0] / ResolutionY); //Normally I would use mod here but VBA has a weird Mod function
			if (nY[0] != 0)
			{
				StepY[0] = RangeY[0] / nY[0];
			}
			else
			{
				StepY[0] = 0;
			}
			//Second section

			StartY[1] = PolygonYs[AscendingOrder[1]];
			EndY[1] = PolygonYs[AscendingOrder[2]];
			RangeY[1] = EndY[1] - StartY[1];
			nY[1] = Round(RangeY[1] / ResolutionY);
			if (nY[1] != 0)
			{
				StepY[1] = RangeY[1] / nY[1];
			}
			else
			{
				StepY[1] = 0;
			}

			//Third section
			StartY[2] = PolygonYs[AscendingOrder[2]];
			EndY[2] = PolygonYs[AscendingOrder[3]];
			RangeY[2] = EndY[2] - StartY[2];
			nY[2] = Round(RangeY[2] / ResolutionY);
			if (nY[2] != 0)
			{
				StepY[2] = RangeY[2] / nY[2];
			}
			else
			{
				StepY[2] = 0;
			}

			MaxX = PolygonXs[0];
			MinX = PolygonXs[0];
			for (int i = 1;i != 4;i++)
			{
				if (PolygonXs[i] < MinX)
				{
					MinX = PolygonXs[i];
				}
				else if (PolygonXs[i] > MaxX) //why has this an if and an else if that happend if its the same.
				{
					MaxX = PolygonXs[i];
				}
			}
			nMax = (((PolygonYs[AscendingOrder[3]] - PolygonYs[AscendingOrder[0]]) / ResolutionY) + 2) * (((MaxX - MinX) / ResolutionX) + 2);//int?
			nMax = Round(floor(nMax + 0.5));

			int newStartXsize = (nY[0] + nY[1] + nY[2] + 1);
			StartX.resize(newStartXsize + 1);///added plus 1 (should it perhaps populate from 0 insstead? (see below)

			int newEndXsize = (nY[0] + nY[1] + nY[2] + 1);
			EndX.resize(newEndXsize + 1);///added plus 1 (should it perhaps populate from 0 insstead? 

			RangeX.resize(nY[0] + nY[1] + nY[2] + 1);//added plus 1 (should it perhaps populate from 0 insstead?
			nX.resize(nY[0] + nY[1] + nY[2] + 1); //same
			StepX.resize(nY[0] + nY[1] + nY[2] + 1);//same
			Solution->PointX.resize(nMax);
			Solution->PointY.resize(nMax);
			Solution->PointMeta.resize(nMax);
			PointBuffer.resize(nMax);


			//NCD-------------------------------------
			//I removed the initialization so the values start at 0 to help with plotting
			//initialize array
			//For i = 0 To UBound(PointX)
			//PointX(i) = -99
			//Next

			//'For i = 0 To UBound(PointY)
			//'PointY(i) = -99
			//'Next
			//'-----------------------------------------
			//
			//'k will be used for the point ID
			int k = 0;

			//First Section

			//If two points are at the same height calculating dxdy will cause a divide by zero error
			if (PolygonYs[AscendingOrder[0]] == PolygonYs[AscendingOrder[1]])
			{
				StartX[0] = PolygonXs[AscendingOrder[0]];
				EndX[0] = PolygonXs[AscendingOrder[1]];
				dxdyLeft = (PolygonXs[3] - PolygonXs[0]) / (PolygonYs[3] - PolygonYs[0]);
				dxdyRight = (PolygonXs[2] - PolygonXs[1]) / (PolygonYs[2] - PolygonYs[1]);
			}
			else
			{
				StartX[0] = PolygonXs[AscendingOrder[0]];
				EndX[0] = PolygonXs[AscendingOrder[0]];
				dxdyLeft = (PolygonXs[3] - PolygonXs[0]) / (PolygonYs[3] - PolygonYs[0]);
				dxdyRight = (PolygonXs[1] - PolygonXs[0]) / (PolygonYs[1] - PolygonYs[0]);
			}
			//Step through the points in the first section and record their coordinates
			for (int i = 0; i != (nY[0] + 1);i++)//check
			{
				RangeX[i] = EndX[i] - StartX[i];
				nX[i] = Round(RangeX[i] / ResolutionX);
				if (nX[i] != 0)
				{
					StepX[i] = RangeX[i] / nX[i];
				}
				else
				{
					StepX[i] = 0;
				}

				for (int j = 0;j != (nX[i] + 1);j++)
				{
					Solution->PointX[k] = StartX[i] + j * StepX[i];
					Solution->PointY[k] = StartY[0] + i * StepY[0];
					k = k + 1;
				}

				StartX[i + 1] = StartX[i] + dxdyLeft * StepY[0];
				EndX[i + 1] = EndX[i] + dxdyRight * StepY[0];
			}

			//Second section

			if (PolygonYs[AscendingOrder[1]] == PolygonYs[AscendingOrder[2]])
			{
				dxdyLeft = (PolygonXs[2] - PolygonXs[3]) / (PolygonYs[2] - PolygonYs[3]);
				dxdyRight = (PolygonXs[2] - PolygonXs[1]) / (PolygonYs[2] - PolygonYs[1]);
			}
			else            //Determine weather the slope of the left or right bounding line has changed in this section
			{
				if (AscendingOrder[1] == 1)
				{
					dxdyRight = (PolygonXs[2] - PolygonXs[1]) / (PolygonYs[2] - PolygonYs[1]);
				}
				else        //AscendingOrder(1] must equal 3
				{
					dxdyLeft = (PolygonXs[2] - PolygonXs[3]) / (PolygonYs[2] - PolygonYs[3]);
				}
			}

			//Step through the points in the second section and record their coordinates
			for (int i = nY[0] + 1; i != (nY[0] + nY[1] + 1);i++)//is this an error should it start from 0
			{
				RangeX[i] = EndX[i] - StartX[i];
				nX[i] = Round(RangeX[i] / ResolutionX);
				if (nX[i] != 0)
				{
					StepX[i] = RangeX[i] / nX[i];
				}
				else
				{
					StepX[i] = 0;
				}

				for (int j = 0; j != (nX[i] + 1);j++)
				{
					Solution->PointX[k] = StartX[i] + j * StepX[i];
					Solution->PointY[k] = StartY[1] + ((i - nY[0]) * StepY[1]);
					k = k + 1;
				}

				StartX[i + 1] = StartX[i] + dxdyLeft * StepY[1];
				EndX[i + 1] = EndX[i] + dxdyRight * StepY[1];
			}

			//Third section

			if (PolygonYs[AscendingOrder[2]] == PolygonYs[AscendingOrder[3]])
			{
				dxdyLeft = 0;
				dxdyRight = 0;
			}
			else            //Determine weather the slope of the left or right bounding line has changed in this section
			{
				if (AscendingOrder[3] == 1)
				{
					dxdyLeft = (PolygonXs[1] - PolygonXs[2]) / (PolygonYs[1] - PolygonYs[2]);
				}
				else if (AscendingOrder[3] == 3)
				{
					dxdyRight = (PolygonXs[3] - PolygonXs[2]) / (PolygonYs[3] - PolygonYs[2]);
				}
				else        //AscendingOrder(3) must equal 2
				{
					if (AscendingOrder[2] == 1)
					{
						dxdyRight = (PolygonXs[2] - PolygonXs[1]) / (PolygonYs[2] - PolygonYs[1]);
					}
					else    //AscendingOrder(2) must equal 3
					{
						dxdyLeft = (PolygonXs[2] - PolygonXs[3]) / (PolygonYs[2] - PolygonYs[3]);
					}
				}
			}

			for (int i = nY[0] + nY[1] + 1; i != (1 + nY[0] + nY[1] + nY[2]);i++)
			{
				RangeX[i] = EndX[i] - StartX[i];
				nX[i] = Round(RangeX[i] / ResolutionX);
				if (nX[i] != 0)
				{
					StepX[i] = RangeX[i] / nX[i];
				}
				else
				{
					StepX[i] = 0;
				}

				for (int j = 0; j != nX[i] + 1;j++)
				{
					Solution->PointX[k] = StartX[i] + j * StepX[i];
					Solution->PointY[k] = StartY[2] + ((i - nY[0] - nY[1]) * StepY[2]);
					k = k + 1;
				}

				StartX[i + 1] = StartX[i] + dxdyLeft * StepY[2];
				EndX[i + 1] = EndX[i] + dxdyRight * StepY[2];
			}

			if (RotationFlag == true)
			{
				for (int i = 0; i != (k - 1 + 1);i++)
				{
					PointBuffer[i] = Solution->PointX[i];
					Solution->PointX[i] = Solution->PointY[i];
					Solution->PointY[i] = PointBuffer[i];
				}
			}



			//	Solution->PointX.push_front(1.0);
			//	Solution->PointY.push_front(1.0);
			//	Solution->PointMeta.push_front(1.0);
			return Solution;
		}

		double Model::Part_press(double P, double W)
		{
			// Function to compute partial vapor pressure in [kPa]
			// From page 6.9 equation 38 in ASHRAE Fundamentals handbook (2005)
			//   P = ambient pressure [kPa]
			//   W = humidity ratio [kg/kg dry air]

			return (P * W / (0.62198 + W));
		}

		double Model::Sat_press(double Tdb)
		{
			// Function to compute saturation vapor pressure in [kPa]
			//ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
			//   Tdb = Dry bulb temperature [degC]
			// Valid from -100C to 200 C

			double  C1 = -5674.5359;
			double  C2 = 6.3925247;
			double  C3 = -0.009677843;
			double  C4 = 0.00000062215701;
			double  C5 = 2.0747825E-09;
			double  C6 = -9.484024E-13;
			double  C7 = 4.1635019;
			double  C8 = -5800.2206;
			double  C9 = 1.3914993;
			double  C10 = -0.048640239;
			double  C11 = 0.000041764768;
			double  C12 = -0.000000014452093;
			double  C13 = 6.5459673;
			double  Sat_press_val = 0;

			double   TK = Tdb + 273.15;         //Converts from degC to degK

			if (TK <= 273.15)
			{
				Sat_press_val = exp(C1 / TK + C2 + C3 * TK + C4 * pow(TK, 2) + C5 * pow(TK, 3) + C6 * pow(TK, 4) + C7 * log(TK)) / 1000;
			}
			else
			{
				Sat_press_val = exp(C8 / TK + C9 + C10 * TK + C11 * pow(TK, 2) + C12 * pow(TK, 3) + C13 * log(TK)) / 1000;
			}
			return Sat_press_val;

		}


		double Model::CalcHum_ratio_W(double Tdb, double RH, double P)
		{
			// Function to calculate humidity ratio [kg H2O/kg air]
			// Given dry bulb and wet bulb temperature inputs [degC]
			// ASHRAE Fundamentals handbood (2005)
			//   Tdb = Dry bulb temperature [degC]
			//   RH = Relative Humidity [Fraction or %]
			//   P = Ambient Pressure [kPa]

			double Pws = Sat_press(Tdb);
			double Hum_rat = 0.62198 * RH * Pws / (P - RH * Pws);   // Equation 22, 24, p6.8
			return Hum_rat;
		}
		bool Model::MeetsSupplyAirTOC(double Tosa)
		{
			using ScheduleManager::GetCurrentScheduleValue;
			double MinSAT = 10;
			if (Tsa_schedule_pointer > 0) {
				MinSAT = GetCurrentScheduleValue(Tsa_schedule_pointer);
			}
			if (Tosa < MinSAT) //implement
				return false;
			return true;
		}

		bool Model::MeetsSupplyAirRHOC(double Wosa)
		{
			// implement
			if (Wosa < 0)
				//temp fix
				Wosa = 0.003;
				//return false;
			return true;
		}
		double Model::CheckVal_W(double W)
		{
			if ((W > 1) || (W < 0))
			{
				//issue
				W = W;
			}
			return W;
		}
		double Model::CheckVal_T(double T)
		{
			if ((T > 50) || (T < 10))
			{
				//issue
				T = T;
			}
			return T;
		}

		bool Model::MeetsOAEnvConstraints(double Tosa, double Wosa, double RHosa, int ModeNumber)
		{
			bool OATempConstraintmet = false;
			bool OARHConstraintmet = false;
			double ToaConstrainLow = Config->GetECLow(DEF_Tdb, ModeNumber); // spencer make changes here
			double ToaConstrainHi = Config->GetECHi(DEF_Tdb, ModeNumber);

			double RHoaConstrainLow = Config->GetECLow(DEF_RH, ModeNumber);
			double RHoaConstrainHi = Config->GetECHi(DEF_RH, ModeNumber);

			//	double RHosa = Part_press(101.325, Wosa) / Sat_press(Tosa);

			if (Tosa >= ToaConstrainLow && Tosa <= ToaConstrainHi)
			{
				OATempConstraintmet = true;
			}

			if (RHosa >= RHoaConstrainLow && RHosa <= RHoaConstrainHi)
			{
				OARHConstraintmet = 1;
			}


			if (OARHConstraintmet == 1 && OATempConstraintmet == 1)
			{
				return true;
			}
			else
			{
				return false;
			}
		}

		/*
		void MeetsIAEnvConstraints()
		{
		RATempConstraintmet = 0
		RAWConstraintmet = 0
		TraConstrainLow = ECLow(2, ModeNumber)
		TraConstrainHi = ECHi(2, ModeNumber)

		WraConstrainLow = ECLow(3, ModeNumber)
		WraConstrainHi = ECHi(3, ModeNumber)

		If Tra > TraConstrainLow And Tra < TraConstrainHi Then
		RATempConstraintmet = 1
		End If

		If Wra > WraConstrainLow And Wra < WraConstrainHi Then

		RAWConstraintmet = 1
		End If

		If RAWConstraintmet = 1 And RATempConstraintmet = 1 Then
		MeetsIAEnvConstraints = 1
		Else
		MeetsIAEnvConstraints = 0
		End If

		}*/
		double Model::CalculateSupplyAirDBTempAtRefCon()
		{
			double Tosa_ref = 37.78; //these can not be hard coded and need to be from config file?
			double wOSA_ref = 0.010058395;
			double TdbRA_ref = 25.55555556;
			double wRA_ref = 0.0095145622;
			double MsaRatio_ref = 1;
			double OSAF_ref = 0.45;
			int modenumber = 2;

			double TsaRef = CalculateCurveVal(1, Tosa_ref, wOSA_ref, TdbRA_ref, wRA_ref, MsaRatio_ref, OSAF_ref, modenumber, TEMP_CURVE); //TEMP_CURVE W_CURVE POWER_CURVE
			return TsaRef;
		}

		double Model::CalculateMixedAirTemp()
		{
			double Tosa_ref = 37.78; //these can not be hard coded and need to be from config file?
			double MsaRatio_ref = 0.45;
			double TdbRA_ref = 25.56;
			double MixedAirDBTempAtRefCon = TdbRA_ref + MsaRatio_ref*(Tosa_ref - TdbRA_ref);
			return MixedAirDBTempAtRefCon;

		}
		void Model::doStep(double Tosa, double Tra, double RHosa, double RHra, double RequestedLoad, double CapacityRatedCond, int CapacityFlag, double DesignMinVR, double rTestFlag, double *returnQSensible, double *returnQLatent, double *returnSupplyAirMassFlow, double *returnSupplyAirTemp, double *returnSupplyAirRelHum, double *returnVentilationAir, int *FMUmode, double *ElectricalPowerUse, double communicationStepSize, int *bpErrorCode) {
			
			using General::RoundSigDigits; 
			int modenumber = 0;
			int point_number = 0;
			double MsaRatio = 0;
			double OSAF = 0;
			double Msa = 0;
			double Mvent = 0;
			double EIR, ElectricalPower, SHR, Tma, Wma, Hsa, Hma, Y_DeltaH, H_SENS_ROOM, mode_optimal_power, mode_optimal_point, RequestedCoolingLoad;
			mode_optimal_power = 0;
			double PreviousMaxiumOutput = 0;
			double Wosa = CalcHum_ratio_W(Tosa, RHosa / 100, 101.325);
			double Wra = CalcHum_ratio_W(Tra, RHra / 100, 101.325);
			bool EnvironmentConditionsMet, EnvironmentConditionsMetOnce, MinVRMet, MinVRMetOnce, SAT_OC_Met, SAT_OC_MetOnce, SARH_OC_Met, SAHR_OC_MetOnce;
			EnvironmentConditionsMetOnce = SAT_OC_Met = SAT_OC_MetOnce = SARH_OC_Met = SAHR_OC_MetOnce = false;
			double SupplyAirDBTempAtRefCon = 10;// CalculateSupplyAirDBTempAtRefCon();
			double MixedAirDBTempAtRefCon = 25;// CalculateMixedAirTemp();
			MinOA_Msa = DesignMinVR;

			if (CapacityFlag == 1)
			{
				MsaRated = CapacityRatedCond;//MsaRated
			}
			else
			{
				MsaRated = CapacityRatedCond / 1000 * (MixedAirDBTempAtRefCon - SupplyAirDBTempAtRefCon);
			}

			double averaged_requestedLoad = 0;

			*returnVentilationAir = 0;
			*returnSupplyAirMassFlow = 0;
			*bpErrorCode = 0;

			if (rTestFlag == 1)
			{
				double tempHumidity = RHosa;
				Mvent = MinOA_Msa;
				double tempSAT = Tosa;
				optimal_Msa = Mvent;

				//*use this line with dampening
				//	averaged_requestedLoad=(RequestedLoad+RequestedLoad_t_n1+RequestedLoad_t_n2+RequestedLoad_t_n3+RequestedLoad_t_n4+RequestedLoad_t_n5+RequestedLoad_t_n6+RequestedLoad_t_n7+RequestedLoad_t_n8)/8; // with dampening
				//use this line for no dampening
				averaged_requestedLoad = RequestedLoad;//no dampening
				if (RequestedLoad < 0 && averaged_requestedLoad < 0)
				{
					EIR = 8;
					double max_Msa = MsaRated;
					tempSAT = 12;
					tempHumidity = RHra;
					*returnQSensible = averaged_requestedLoad;
					*returnQLatent = Tosa;
					*FMUmode = 2;
					*ElectricalPowerUse = -averaged_requestedLoad / EIR;

					if (Tra != tempSAT)
					{
						optimal_Msa = -averaged_requestedLoad / (1006 * (Tra - tempSAT));
						if (optimal_Msa > max_Msa)
						{
							optimal_Msa = max_Msa;
						}
					}
					else
					{
						optimal_Msa = 0.5;
					}

					if (MinOA_Msa > optimal_Msa)
					{
						optimal_Msa = MinOA_Msa;
					}
				}
				*returnSupplyAirMassFlow = optimal_Msa;
				*returnSupplyAirTemp = tempSAT;
				*returnSupplyAirRelHum = RHra;
				*returnVentilationAir = Mvent;
				RequestedLoad_t_n8 = RequestedLoad_t_n7;
				RequestedLoad_t_n7 = RequestedLoad_t_n6;
				RequestedLoad_t_n6 = RequestedLoad_t_n5;
				RequestedLoad_t_n5 = RequestedLoad_t_n4;
				RequestedLoad_t_n4 = RequestedLoad_t_n3;
				RequestedLoad_t_n3 = RequestedLoad_t_n2;
				RequestedLoad_t_n2 = RequestedLoad_t_n1;
				RequestedLoad_t_n1 = RequestedLoad;
				return;
			}

			if (RequestedLoad < 0)
			{
				RequestedCoolingLoad = -RequestedLoad / 1000; //convert to kw
				std::list<CModeSolutionSpace*>::const_iterator iterator;
				for (iterator = XandYPoints.begin(); iterator != XandYPoints.end(); ++iterator) // iterate though the modes.
				{
					CModeSolutionSpace* solutionspace = *iterator;

					int solution_map_sizeX = solutionspace->PointY.size() - 1;
					int solution_map_sizeY = solutionspace->PointX.size() - 1;
					int solution_map_sizeM = solutionspace->PointMeta.size() - 1;
					if (solution_map_sizeX != solution_map_sizeY)
					{
						ShowWarningError("Error in CModeSolutionSpace for mose, called in HybridEvapCooling:dostep");
						//cout << "Error in CModeSolutionSpace for mose ";
						return;
					}

					// Check that in this mode the 
					//Outside Air Relative Humidity(0 - 100 % )
					//Outside Air Humidity Ratio(g / g)
					//Outside Air Temperature(°C)
					if (MeetsOAEnvConstraints(Tosa, Wosa, RHosa, modenumber) == true)//fix this it should not end if it does not meet contraints.
					{
						EnvironmentConditionsMet = EnvironmentConditionsMetOnce = true;
					}
					else
					{
						EnvironmentConditionsMet = false;
					}

					if (EnvironmentConditionsMet == true)
					{

						for (point_number = 0;point_number != solution_map_sizeX;point_number++) // within each mode go though all the combinations of solution spaces.
						{
							//Supply Air Mass Flow Rate(kg / s)
							//Outside Air Fraction(0 - 1)
							MsaRatio = solutionspace->PointX[point_number];// fractions of rated mass flow rate, so for some modes this might be low but others hi
							OSAF = solutionspace->PointY[point_number];
							// there is no operating condition test, becuase it uses those to make the map so only allowed values are here.
							Msa = MsaRated * MsaRatio;
							//Calculate the ventilation mass flow rate
							Mvent = Msa * OSAF;
							if (Mvent > MinOA_Msa) MinVRMet = MinVRMetOnce = true;
							else MinVRMet = false;

							if (MinVRMet)
							{
								//all these points meet the minimum VR requirement
								solutionspace->PointMeta[point_number] = 1;
								//'Set B_coefficients for DeltaH from lookup table for the specific mode
								Tsa = CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, TEMP_CURVE); //TEMP_CURVE W_CURVE POWER_CURVE
																														  //Set B_coefficients for SHR from lookup table for the specific mode
																														  //Return Air Temperature(°C)
								if (MeetsSupplyAirTOC(Tsa)) SAT_OC_Met = SAT_OC_MetOnce = true;
								else
								{
									SAT_OC_Met = false;
								}

								Wsa = CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, W_CURVE);
								//Return Air Relative Humidity(0 - 100 % )
								//Return Air Humidity Ratio(g / g)
								if (MeetsSupplyAirRHOC(Wsa)) SARH_OC_Met = SAHR_OC_MetOnce = true;
								else
								{
									//Wsa = 0;
									ShowWarningError("MeetsSupplyAirRHOC failed given a Wsa of"+ RoundSigDigits(Wsa,5));
									
									SARH_OC_Met = false;
								}

								if (SARH_OC_Met == false || SAT_OC_Met == false)
								{
									//Calculate the delta H 
									Tma = Tra + OSAF * (Tosa - Tra);
									Wma = Wra + OSAF * (Wosa - Wra);
									Hma = 1.006 * Tma * (2501 + 1.86 * Tma);
									Hsa = 1.006 * Tsa * (2501 + 1.86 * Tsa);
									Y_DeltaH = (Hma - Hsa) * Msa;     //kW
																	  //Calculate possible sensible load
									H_SENS_ROOM = cp *Msa* (Tra - Tsa);
									// even though we don't even know yet if this point can

									// does it meet minimum load requirement 
									if (H_SENS_ROOM > RequestedCoolingLoad)
									{

										//all these points meet the sensible heating load
										solutionspace->PointMeta[point_number] = 2;
										double Y_val = CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, POWER_CURVE);
										ElectricalPower = Y_val / 1000;  //kW
																		 //calculate the electrical power usage
																		 //ElectricalPower = EIR * Y_DeltaH * H_Rated 'kw?
																		 //Calculate EIR and SHR
										EIR = ElectricalPower / Y_DeltaH;
										SHR = cp * (Tma - Tsa) / (Hma - Hsa);
										//NCD Additions------------------------------------
										if (ElectricalPower < mode_optimal_power)
										{
											mode_optimal_power = ElectricalPower;
											mode_optimal_point = point_number;
										}
										//-------------------------------------------------
										//store a copy of the acceptable (meets load points for debug reaons can remove from final version
										if (ElectricalPower < optimal_power)
										{
											optimal_EnvCondMet = EnvironmentConditionsMet;
											optimal_Msa = Msa;
											optimal_OSAF = OSAF;
											optimal_power = ElectricalPower;
											optimal_H_sensible_room = H_SENS_ROOM;
											optimal_TotalSystemH = Y_DeltaH; //* H_Rated
											optimal_SHR = SHR;
											optimal_Mvent = Mvent;
											optimal_Mode = modenumber;
											optimal_Wsa = Wsa;
											optimal_Tsa = Tsa;
											optimal_Point = point_number; //this is used to identify the point that
											DidWeMeetLoad = true;
										}
									}
									else
									{
										//the system might never actually be able to meet the load so we need a method of remembering the best attempt at satisfying load
										if (H_SENS_ROOM > PreviousMaxiumOutput)
										{
											PreviousMaxiumOutput = H_SENS_ROOM;
											double Y_val = CalculateCurveVal(1, Tosa, Wosa, Tra, Wra, MsaRatio, OSAF, modenumber, POWER_CURVE);
											ElectricalPower = Y_val / 1000;  //kW
																			 //Calculate EIR and SHR
											EIR = ElectricalPower / Y_DeltaH;
											SHR = cp * (Tma - Tsa) / (Hma - Hsa);
											//-------------------------------------------------
											//store a copy of the acceptable (meets load points for debug reaons can remove from final version
											//  If ElectricalPower < RunningPeakCapacity_power Then
											RunningPeakCapacity_EnvCondMet = EnvironmentConditionsMet;
											RunningPeakCapacity_power = ElectricalPower;
											RunningPeakCapacity_Msa = Msa;
											RunningPeakCapacity_OSAF = OSAF;
											RunningPeakCapacity_H_sensible_room = H_SENS_ROOM;
											RunningPeakCapacity_TotalSystemH = Y_DeltaH; //* H_Rated
											RunningPeakCapacity_SHR = SHR;
											RunningPeakCapacity_Mvent = Mvent;
											RunningPeakCapacity_Mode = modenumber;
											RunningPeakCapacity_Tsa = Tsa;
											RunningPeakCapacity_Wsa = Wsa;
											RunningPeakCapacity_Point = point_number; //this is used to identify the point that
										}
									}
								}

							}



						}
					}

					modenumber++;
				}
				if (EnvironmentConditionsMetOnce == false)
				{
					*bpErrorCode = 1;
					count_EnvironmentConditionsMetOnce++;

					//error 
				}
				if (SAHR_OC_MetOnce == false)
				{
					count_SAHR_OC_MetOnce++;
					*bpErrorCode = 2;
					//error 
				}
				if (SAT_OC_MetOnce == false)
				{
					count_SAT_OC_MetOnce++;
					*bpErrorCode = 3;
					//error 
				}
				if (DidWeMeetLoad == false)
				{
					//*bpErrorCode = 3;
					count_DidWeMeetLoad++;
					//what is this ??
					if (RunningPeakCapacity_power > 100000)
					{
						ShowWarningError("Model was not able to provide cooling for a time step, called in HybridEvapCooling:dostep");
						//cout << "Model was not able to provide cooling.";
						RunningPeakCapacity_power = 0;
					}
					optimal_EnvCondMet = RunningPeakCapacity_EnvCondMet;
					optimal_Msa = RunningPeakCapacity_Msa;
					optimal_OSAF = RunningPeakCapacity_OSAF;
					optimal_power = RunningPeakCapacity_power;
					optimal_H_sensible_room = RunningPeakCapacity_H_sensible_room;
					optimal_TotalSystemH = RunningPeakCapacity_TotalSystemH;
					optimal_SHR = RunningPeakCapacity_SHR;
					optimal_Mvent = RunningPeakCapacity_Mvent;
					optimal_Mode = RunningPeakCapacity_Mode;
					optimal_Wsa = RunningPeakCapacity_Wsa;
					optimal_Tsa = RunningPeakCapacity_Tsa;
				}
				if (optimal_EnvCondMet == false)
				{
					ShowWarningError("Environmental conditions exceeded model limits, called in HybridEvapCooling:dostep");
					//cout << "Environmental conditions exceeded model limits./n";
				}
				if (*bpErrorCode == 0)
				{
					*returnQSensible = -optimal_H_sensible_room * 1000;//RequestedLoad/(2);//*communicationStepSize);
					*returnQLatent = 0;
					*returnSupplyAirMassFlow = optimal_Msa;
					*returnSupplyAirTemp = CheckVal_T(optimal_Tsa);
					*returnSupplyAirRelHum = CheckVal_W(optimal_Wsa);
					*returnVentilationAir = optimal_Mvent;
					*FMUmode = optimal_Mode;
					*ElectricalPowerUse = optimal_power;
				}
				else
				{
					*returnQSensible = 0;
					*returnQLatent = 0;
					*returnSupplyAirMassFlow = 0;
					*returnSupplyAirTemp = Tra;
					*returnSupplyAirRelHum = Wra;
					*returnVentilationAir = 0;
					*FMUmode = -2;
					*ElectricalPowerUse = 0;

				}

			}
			else
			{ //current heating mode, do nothing
				*returnQSensible = 0;
				*returnQLatent = 0;
				*returnSupplyAirMassFlow = 0;
				*returnSupplyAirTemp = Tra;
				*returnSupplyAirRelHum = Wra;
				*returnVentilationAir = 0;
				*FMUmode = -1;
				*ElectricalPowerUse = 0;
			}
			InitializeModelParams();
			//SetEnvironmentConditions

			//Iterate through modes of operation

			//	Generate point matrix using Tessellate function from operatiing conditions

			//	Calculate the number of modes based on the configuration file

			//	Iterate through solution spaceof mode to identify lowest energy consuming solution for each mode.



		}

	}
}

