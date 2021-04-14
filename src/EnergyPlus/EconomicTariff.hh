// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef EconomicTariff_hh_INCLUDED
#define EconomicTariff_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace EconomicTariff {

    enum class iEconVarObjType
    {
        Unknown,
        Tariff,
        Qualify,
        ChargeSimple,
        ChargeBlock,
        Ratchet,
        Variable,
        Computation,
        Category,
        Native,
        AssignCompute,
    };

    enum class iEconConv
    {
        USERDEF,
        KWH,
        THERM,
        MMBTU, // million btu
        MJ,
        KBTU,
        MCF, // thousand cubic feet
        CCF, // hundred cubic feet
        M3,  // cubic meter
        GAL,
        KGAL, // thousand gallons
    };

    constexpr std::string_view convEneStrings(iEconConv &e)
    {
        switch (e) {
        case iEconConv::USERDEF:
            return "";
        case iEconConv::KWH:
            return "kWh";
        case iEconConv::THERM:
            return "Therm";
        case iEconConv::MMBTU:
            return "MMBtu";
        case iEconConv::MJ:
            return "MJ";
        case iEconConv::KBTU:
            return "kBtu";
        case iEconConv::MCF:
            return "MCF";
        case iEconConv::CCF:
            return "CCF";
        case iEconConv::M3:
            return "m3";
        case iEconConv::GAL:
            return "gal";
        case iEconConv::KGAL:
            return "kgal";
        default:
            assert(false);
            return "";
        }
    }

    constexpr std::string_view convDemStrings(iEconConv &e)
    {
        switch (e) {
        case iEconConv::USERDEF:
            return "";
        case iEconConv::KWH:
            return "kW";
        case iEconConv::THERM:
            return "Therm";
        case iEconConv::MMBTU:
            return "MMBtu";
        case iEconConv::MJ:
            return "MJ";
        case iEconConv::KBTU:
            return "kBtu";
        case iEconConv::MCF:
            return "MCF";
        case iEconConv::CCF:
            return "CCF";
        case iEconConv::M3:
            return "m3";
        case iEconConv::GAL:
            return "gal";
        case iEconConv::KGAL:
            return "kgal";
        default:
            assert(false);
            return "";
        }
    }

    enum class iDemandWindow
    {
        Unassigned,
        Quarter,
        Half,
        Hour,
        Day,
        Week,
    };

    constexpr std::string_view demWindowStrings(iDemandWindow &e)
    {
        switch (e) {
        case iDemandWindow::Unassigned:
            return "";
        case iDemandWindow::Quarter:
        case iDemandWindow::Half:
        case iDemandWindow::Hour:
            return "/Hr";
        case iDemandWindow::Day:
            return "/Day";
        case iDemandWindow::Week:
            return "/Wk";
        default:
            assert(false);
            return "";
        }
    }

    int constexpr buyFromUtility(1);
    int constexpr sellToUtility(2);
    int constexpr netMetering(3);

    // For several different objects that reference seasons
    int constexpr seasonWinter(1);
    int constexpr seasonSpring(2);
    int constexpr seasonSummer(3);
    int constexpr seasonFall(4);
    int constexpr seasonAnnual(5);
    int constexpr seasonMonthly(6);

    // For AssignVariablePt
    int constexpr varIsArgument(1); // if used as a value or on right side of expression
    int constexpr varIsAssigned(2); // if variable is assigned to or on left side of expression

    // For ComputeSteps
    // All are negative because all variables are positive
    int constexpr opSUM(-1);
    int constexpr opMULTIPLY(-2);
    int constexpr opSUBTRACT(-3);
    int constexpr opDIVIDE(-4);
    int constexpr opABSOLUTE(-5);
    int constexpr opINTEGER(-6);
    int constexpr opSIGN(-7);
    int constexpr opROUND(-8);
    int constexpr opMAXIMUM(-9);
    int constexpr opMINIMUM(-10);
    int constexpr opEXCEEDS(-11);
    int constexpr opANNUALMINIMUM(-12);
    int constexpr opANNUALMAXIMUM(-13);
    int constexpr opANNUALSUM(-14);
    int constexpr opANNUALAVERAGE(-15);
    int constexpr opANNUALOR(-16);
    int constexpr opANNUALAND(-17);
    int constexpr opANNUALMAXIMUMZERO(-18);
    int constexpr opANNUALMINIMUMZERO(-19);
    int constexpr opIF(-20);
    int constexpr opGREATERTHAN(-21);
    int constexpr opGREATEREQUAL(-22);
    int constexpr opLESSTHAN(-23);
    int constexpr opLESSEQUAL(-24);
    int constexpr opEQUAL(-25);
    int constexpr opNOTEQUAL(-26);
    int constexpr opAND(-27);
    int constexpr opOR(-28);
    int constexpr opNOT(-29);
    int constexpr opADD(-30);
    int constexpr opNOOP(-31); // no operation - just list the operand variables - shown as FROM

    // not predefined variable (user defined name - many variables and all objects)
    // used in econvar%specific
    int constexpr varUserDefined(1);
    int constexpr varNotYetDefined(2);

    // category variables (used in econvar%specific)
    int constexpr catEnergyCharges(11);
    int constexpr catDemandCharges(12);
    int constexpr catServiceCharges(13);
    int constexpr catBasis(14);
    int constexpr catAdjustment(15);
    int constexpr catSurcharge(16);
    int constexpr catSubtotal(17);
    int constexpr catTaxes(18);
    int constexpr catTotal(19);
    int constexpr catNotIncluded(20);

    // native variables (based on energy and demands from the simulation) used in econvar%specific
    int constexpr nativeTotalEnergy(101);
    int constexpr nativeTotalDemand(102);
    int constexpr nativePeakEnergy(103);
    int constexpr nativePeakDemand(104);
    int constexpr nativeShoulderEnergy(105);
    int constexpr nativeShoulderDemand(106);
    int constexpr nativeOffPeakEnergy(107);
    int constexpr nativeOffPeakDemand(108);
    int constexpr nativeMidPeakEnergy(109);
    int constexpr nativeMidPeakDemand(110);
    int constexpr nativePeakExceedsOffPeak(111);
    int constexpr nativeOffPeakExceedsPeak(112);
    int constexpr nativePeakExceedsMidPeak(113);
    int constexpr nativeMidPeakExceedsPeak(114);
    int constexpr nativePeakExceedsShoulder(115);
    int constexpr nativeShoulderExceedsPeak(116);
    int constexpr nativeIsWinter(117);
    int constexpr nativeIsNotWinter(118);
    int constexpr nativeIsSpring(119);
    int constexpr nativeIsNotSpring(120);
    int constexpr nativeIsSummer(121);
    int constexpr nativeIsNotSummer(122);
    int constexpr nativeIsAutumn(123);
    int constexpr nativeIsNotAutumn(124);

    int constexpr nativePeakAndShoulderEnergy(125);
    int constexpr nativePeakAndShoulderDemand(126);
    int constexpr nativePeakAndMidPeakEnergy(127);
    int constexpr nativePeakAndMidPeakDemand(128);
    int constexpr nativeShoulderAndOffPeakEnergy(129);
    int constexpr nativeShoulderAndOffPeakDemand(130);
    int constexpr nativePeakAndOffPeakEnergy(131);
    int constexpr nativePeakAndOffPeakDemand(132);

    int constexpr nativeRealTimePriceCosts(133);
    int constexpr nativeAboveCustomerBaseCosts(134);
    int constexpr nativeBelowCustomerBaseCosts(135);
    int constexpr nativeAboveCustomerBaseEnergy(136);
    int constexpr nativeBelowCustomerBaseEnergy(137);

    int constexpr countPeriod(4);
    int constexpr MaxNumMonths(12);
    int constexpr maxNumBlk(15);

    int constexpr periodPeak(1);
    int constexpr periodShoulder(2);
    int constexpr periodOffPeak(3);
    int constexpr periodMidPeak(4);

    int constexpr kindMeterNotElectric(0); // must be zero because testing of >0 done later.
    int constexpr kindMeterElecSimple(1);
    int constexpr kindMeterElecProduced(2);
    int constexpr kindMeterElecPurchased(3);
    int constexpr kindMeterElecSurplusSold(4);
    int constexpr kindMeterElecNet(5);

    int constexpr kindMeterNotWater(0);
    int constexpr kindMeterWater(1);

    int constexpr kindMeterNotGas(0);
    int constexpr kindMeterGas(1);

    int constexpr varUnitTypeEnergy(1);
    int constexpr varUnitTypeDemand(2);
    int constexpr varUnitTypeDimensionless(3);
    int constexpr varUnitTypeCurrency(4);

    // Types

    struct EconVarType
    {
        // Members
        std::string name;          // name of the economics object or variable
        int tariffIndx;            // index of the tariff name in the tariff array
        iEconVarObjType kindOfObj; // enumerated list for the kind of economics object
        int index;                 // pointer to item in specific array
        Array1D<Real64> values;    // values
        // the following items are not part of the object description
        bool isArgument; // flag if the variable is ever used as an argument (value needed)
        bool isAssigned; // flag if the variable is ever assigned to
        int specific;    // the specific type of variable - see enumerated lists
        // the following items are used in determinging the dependency relationship of variables
        // and consist of an operator and a list of variables.
        int cntMeDependOn; // count of items in depend this line depends upon
        int Operator;      // operator used in equation (usually opSUM or opNOOP)
        int firstOperand;  // first item in the operand array
        int lastOperand;   // last item in the operand array
        bool activeNow;    // flag if the econVar is used in the current tariff
        bool isEvaluated;  // flag if the economics object that results in this variable
        // has already been evaulated
        bool isReported; // flag if the econVar has been reported in the output file
        int varUnitType; // variable unit type: energy, demand, dimensionless, currency

        // Default Constructor
        EconVarType()
            : tariffIndx(0), kindOfObj(iEconVarObjType::Unknown), index(0), values(MaxNumMonths, 0.0), isArgument(false), isAssigned(false),
              specific(0), cntMeDependOn(0), Operator(0), firstOperand(0), lastOperand(0), activeNow(false), isEvaluated(false), isReported(false),
              varUnitType(0)
        {
        }
    };

    struct TariffType
    {
        // Members
        std::string tariffName;                        // name of the tariff
        std::string reportMeter;                       // name of the report meter
        int reportMeterIndx;                           // index of the report meter
        int kindElectricMtr;                           // kind of electric meter - see enumerated list above, 0 is not electric
        int kindWaterMtr;                              // kind of water meter - 0 (default) is not water, 1 is water
        int kindGasMtr;                                // kind of gas meter - 0 (default) is not gas, 1 is gas
        DataGlobalConstants::ResourceType resourceNum; // based on list of DataGlobalConstants
        iEconConv convChoice;                          // enumerated choice index of the conversion factor
        Real64 energyConv;                             // energy conversion factor
        Real64 demandConv;                             // demand conversion factor
        std::string periodSchedule;                    // name of the period schedule (time of day)
        int periodSchIndex;                            // index to the period schedule
        std::string seasonSchedule;                    // name of the season schedule (winter/summer)
        int seasonSchIndex;                            // index to the season schedule
        std::string monthSchedule;                     // name of month schedule (when months end)
        int monthSchIndex;                             // index to the month schedule
        iDemandWindow demandWindow;                    // enumerated list of the kind of demand window
        Real64 demWinTime;                             // length of time for the demand window
        Real64 monthChgVal;                            // monthly charge value
        int monthChgPt;                                // pointer to a variable that contains the monthly charge
        // if 0 then use monthChgVal
        Real64 minMonthChgVal; // minimum monthly charge value
        int minMonthChgPt;     // pointer to a variable that contains the minimum monthly charge
        // if 0 then use minMonthChgVal
        std::string chargeSchedule;  // name of the charge schedule (for real time pricing)
        int chargeSchIndex;          // index to the charge schedule
        std::string baseUseSchedule; // name of the baseline use schedule (for real time pricing)
        int baseUseSchIndex;         // index to the baseline use schedule
        std::string groupName;       // name of the group
        std::string monetaryUnit;    // text string representing monetary unit, usually $
        int buyOrSell;               // enumerated choice index of the buy or sell options
        // index to the first and last category variables
        int firstCategory; // first category referenced
        int lastCategory;  // last category referenced
        // pointers to econ variables for categories
        int ptEnergyCharges;
        int ptDemandCharges;
        int ptServiceCharges;
        int ptBasis;
        int ptAdjustment;
        int ptSurcharge;
        int ptSubtotal;
        int ptTaxes;
        int ptTotal;
        int ptNotIncluded;
        // index to the first and last native variables (energies and demands from the simulation)
        int firstNative;
        int lastNative;
        // native variables (based on energy and demands from the simulation)
        int nativeTotalEnergy;
        int nativeTotalDemand;
        int nativePeakEnergy;
        int nativePeakDemand;
        int nativeShoulderEnergy;
        int nativeShoulderDemand;
        int nativeOffPeakEnergy;
        int nativeOffPeakDemand;
        int nativeMidPeakEnergy;
        int nativeMidPeakDemand;
        int nativePeakExceedsOffPeak;
        int nativeOffPeakExceedsPeak;
        int nativePeakExceedsMidPeak;
        int nativeMidPeakExceedsPeak;
        int nativePeakExceedsShoulder;
        int nativeShoulderExceedsPeak;
        int nativeIsWinter;
        int nativeIsNotWinter;
        int nativeIsSpring;
        int nativeIsNotSpring;
        int nativeIsSummer;
        int nativeIsNotSummer;
        int nativeIsAutumn;
        int nativeIsNotAutumn;
        int nativePeakAndShoulderEnergy;
        int nativePeakAndShoulderDemand;
        int nativePeakAndMidPeakEnergy;
        int nativePeakAndMidPeakDemand;
        int nativeShoulderAndOffPeakEnergy;
        int nativeShoulderAndOffPeakDemand;
        int nativePeakAndOffPeakEnergy;
        int nativePeakAndOffPeakDemand;
        // real time pricing native variable pointers
        int nativeRealTimePriceCosts;
        int nativeAboveCustomerBaseCosts;
        int nativeBelowCustomerBaseCosts;
        int nativeAboveCustomerBaseEnergy;
        int nativeBelowCustomerBaseEnergy;
        // arrays for holding gathered values
        Array2D<Real64> gatherEnergy;
        Array2D<Real64> gatherDemand;
        Real64 collectTime;
        Real64 collectEnergy;
        // arryas for holding real time pricing gathered values
        Array1D<Real64> RTPcost;
        Array1D<Real64> RTPaboveBaseCost;
        Array1D<Real64> RTPbelowBaseCost;
        Array1D<Real64> RTPaboveBaseEnergy;
        Array1D<Real64> RTPbelowBaseEnergy;
        Array1D_int seasonForMonth;
        // overall qualification of the rate
        bool isQualified;
        int ptDisqualifier;
        // overall selection and annual cost
        bool isSelected;
        Real64 totalAnnualCost;
        Real64 totalAnnualEnergy;

        // Default Constructor
        TariffType()
            : reportMeterIndx(0), kindElectricMtr(0), kindWaterMtr(0), kindGasMtr(0), resourceNum(DataGlobalConstants::ResourceType::None),
              convChoice(iEconConv::USERDEF), energyConv(0.0), demandConv(0.0), periodSchIndex(0), seasonSchIndex(0), monthSchIndex(0),
              demandWindow(iDemandWindow::Unassigned), demWinTime(0.0), monthChgVal(0.0), monthChgPt(0), minMonthChgVal(0.0), minMonthChgPt(0),
              chargeSchIndex(0), baseUseSchIndex(0), buyOrSell(0), firstCategory(0), lastCategory(0), ptEnergyCharges(0), ptDemandCharges(0),
              ptServiceCharges(0), ptBasis(0), ptAdjustment(0), ptSurcharge(0), ptSubtotal(0), ptTaxes(0), ptTotal(0), ptNotIncluded(0),
              firstNative(0), lastNative(0), nativeTotalEnergy(0), nativeTotalDemand(0), nativePeakEnergy(0), nativePeakDemand(0),
              nativeShoulderEnergy(0), nativeShoulderDemand(0), nativeOffPeakEnergy(0), nativeOffPeakDemand(0), nativeMidPeakEnergy(0),
              nativeMidPeakDemand(0), nativePeakExceedsOffPeak(0), nativeOffPeakExceedsPeak(0), nativePeakExceedsMidPeak(0),
              nativeMidPeakExceedsPeak(0), nativePeakExceedsShoulder(0), nativeShoulderExceedsPeak(0), nativeIsWinter(0), nativeIsNotWinter(0),
              nativeIsSpring(0), nativeIsNotSpring(0), nativeIsSummer(0), nativeIsNotSummer(0), nativeIsAutumn(0), nativeIsNotAutumn(0),
              nativePeakAndShoulderEnergy(0), nativePeakAndShoulderDemand(0), nativePeakAndMidPeakEnergy(0), nativePeakAndMidPeakDemand(0),
              nativeShoulderAndOffPeakEnergy(0), nativeShoulderAndOffPeakDemand(0), nativePeakAndOffPeakEnergy(0), nativePeakAndOffPeakDemand(0),
              nativeRealTimePriceCosts(0), nativeAboveCustomerBaseCosts(0), nativeBelowCustomerBaseCosts(0), nativeAboveCustomerBaseEnergy(0),
              nativeBelowCustomerBaseEnergy(0), gatherEnergy(MaxNumMonths, countPeriod, 0.0), gatherDemand(MaxNumMonths, countPeriod, 0.0),
              collectTime(0.0), collectEnergy(0.0), RTPcost(MaxNumMonths, 0.0), RTPaboveBaseCost(MaxNumMonths, 0.0),
              RTPbelowBaseCost(MaxNumMonths, 0.0), RTPaboveBaseEnergy(MaxNumMonths, 0.0), RTPbelowBaseEnergy(MaxNumMonths, 0.0),
              seasonForMonth(MaxNumMonths, 0), isQualified(false), ptDisqualifier(0), isSelected(false), totalAnnualCost(0.0), totalAnnualEnergy(0.0)
        {
        }
    };

    struct QualifyType
    {
        // Members
        int namePt;          // index of the name and variable in the variable array
        int tariffIndx;      // index of the tariff name in the tariff array
        int sourcePt;        // index of the variable in the variable array
        bool isMaximum;      // indicator if maximum test otherwise minimum
        Real64 thresholdVal; // value of the threshold
        int thresholdPt;     // pointer to the variable holding the values
        int season;          // enumerated list of the kind of season
        bool isConsecutive;  // indicator if consecutive months otherwise count
        int numberOfMonths;  // number of months the test must be good for

        // Default Constructor
        QualifyType()
            : namePt(0), tariffIndx(0), sourcePt(0), isMaximum(false), thresholdVal(0.0), thresholdPt(0), season(0), isConsecutive(false),
              numberOfMonths(0)
        {
        }
    };

    struct ChargeSimpleType
    {
        // Members
        int namePt;        // index of the name and variable in the variable array
        int tariffIndx;    // index of the tariff name in the tariff array
        int sourcePt;      // index of the variable in the variable array
        int season;        // enumerated list of the kind of season
        int categoryPt;    // index of the category in the variable array
        Real64 costPerVal; // cost per unit value
        int costPerPt;     // cost per unit index in the variable array (0 is flag for no variable)

        // Default Constructor
        ChargeSimpleType() : namePt(0), tariffIndx(0), sourcePt(0), season(0), categoryPt(0), costPerVal(0.0), costPerPt(0)
        {
        }
    };

    struct ChargeBlockType
    {
        // Members
        int namePt;                 // index of the name and variable in the variable array
        int tariffIndx;             // index of the tariff name in the tariff array
        int sourcePt;               // index of the variable in the variable array
        int season;                 // enumerated list of the kind of season
        int categoryPt;             // index of the category in the variable array
        int remainingPt;            // index of the remaining into variable in the variable array
        Real64 blkSzMultVal;        // block size multiplier value
        int blkSzMultPt;            // block size variable in the variable array (0 is flag for no variable)
        int numBlk;                 // number of blocks used
        Array1D<Real64> blkSzVal;   // array of block size values
        Array1D_int blkSzPt;        // block size variables index to the variable array (0 is no variable)
        Array1D<Real64> blkCostVal; // array of block cost values
        Array1D_int blkCostPt;      // block cost variables index to the variable array (0 is no variable)

        // Default Constructor
        ChargeBlockType()
            : namePt(0), tariffIndx(0), sourcePt(0), season(0), categoryPt(0), remainingPt(0), blkSzMultVal(0.0), blkSzMultPt(0), numBlk(0),
              blkSzVal(maxNumBlk, 0.0), blkSzPt(maxNumBlk, 0), blkCostVal(maxNumBlk, 0.0), blkCostPt(maxNumBlk, 0)
        {
        }
    };

    struct RatchetType
    {
        // Members
        int namePt;           // index of the name and variable in the variable array
        int tariffIndx;       // index of the tariff name in the tariff array
        int baselinePt;       // index of the baseline variable in the variable array
        int adjustmentPt;     // index fo the adjustment variable in the variable array
        int seasonFrom;       // enumerated list of the kind of season
        int seasonTo;         // enumerated list of the kind of season
        Real64 multiplierVal; // value of the ratchet multiplier
        int multiplierPt;     // multiplier variable in the variable array (0 for no variable)
        Real64 offsetVal;     // value of the ratchet offset
        int offsetPt;         // offset variable in the variable array (0 for no variable)

        // Default Constructor
        RatchetType()
            : namePt(0), tariffIndx(0), baselinePt(0), adjustmentPt(0), seasonFrom(0), seasonTo(0), multiplierVal(0.0), multiplierPt(0),
              offsetVal(0.0), offsetPt(0)
        {
        }
    };

    struct ComputationType
    {
        // Members
        std::string computeName; // name of the compute
        int firstStep;           // index to steps array for the first step in this compute steps
        int lastStep;            // index to steps array for the last step in this compute steps
        bool isUserDef;          // if the computation steps were user defined

        // Default Constructor
        ComputationType() : firstStep(0), lastStep(0), isUserDef(false)
        {
        }
    };

    struct StackType
    {
        // Members
        int varPt;              // pointer to item in specific array
        Array1D<Real64> values; // values

        // Default Constructor
        StackType() : varPt(0), values(MaxNumMonths, 0.0)
        {
        }
    };

    void UpdateUtilityBills(EnergyPlusData &state);

    //======================================================================================================================
    //======================================================================================================================

    //    GET INPUT ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    void GetInputEconomicsTariff(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsQualify(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsChargeSimple(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsChargeBlock(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsRatchet(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsVariable(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsComputation(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void GetInputEconomicsCurrencyType(EnergyPlusData &state, bool &ErrorsFound); // true if errors found during getting input objects.

    void parseComputeLine(EnergyPlusData &state, std::string const &lineOfCompute, int const fromTariff);

    void GetLastWord(std::string const &lineOfText, std::string::size_type &endOfScan, std::string &aWord);

    void initializeMonetaryUnit(EnergyPlusData &state);

    int LookUpSeason(EnergyPlusData &state, std::string const &nameOfSeason, std::string const &nameOfReferingObj);

    int FindTariffIndex(EnergyPlusData &state,
                        std::string const &nameOfTariff,
                        std::string const &nameOfReferingObj,
                        bool &ErrorsFound,
                        std::string const &nameOfCurObj);

    void warnIfNativeVarname(
        EnergyPlusData &state, std::string const &objName, int const curTariffIndex, bool &ErrorsFound, std::string const &curobjName);

    int AssignVariablePt(EnergyPlusData &state,
                         std::string const &stringIn,
                         bool const flagIfNotNumeric,
                         int const useOfVar,
                         int const varSpecific,
                         iEconVarObjType const econObjKind,
                         int const objIndex,
                         int const tariffPt);

    void incrementEconVar(EnergyPlusData &state);

    void incrementSteps(EnergyPlusData &state);

    std::string RemoveSpaces(EnergyPlusData &state, std::string const &StringIn);

    void CreateCategoryNativeVariables(EnergyPlusData &state);

    int lookupOperator(std::string const &opString);

    //======================================================================================================================
    //======================================================================================================================

    //    DEFAULT COMPUTATION RELATED ROUTINES

    //======================================================================================================================
    //======================================================================================================================

    void CreateDefaultComputation(EnergyPlusData &state);

    void addOperand(EnergyPlusData &state, int varMe, int varOperand);

    void addChargesToOperand(EnergyPlusData &state, int curTariff, int curPointer);

    //======================================================================================================================
    //======================================================================================================================

    //    GATHER TIMESTEP VALUES ROUTINE

    //======================================================================================================================
    //======================================================================================================================

    void GatherForEconomics(EnergyPlusData &state);

    bool isWithinRange(EnergyPlusData &state, int const testVal, int const minThreshold, int const maxThreshold);

    //======================================================================================================================
    //======================================================================================================================

    //    COMPUTE THE UTILITY BILLS AND CREATE REPORTS

    //======================================================================================================================
    //======================================================================================================================

    void ComputeTariff(EnergyPlusData &state);

    void pushStack(EnergyPlusData &state, Array1A<Real64> const monthlyArray, int const variablePointer);

    void popStack(EnergyPlusData &state, Array1A<Real64> monthlyArray, int &variablePointer);

    void evaluateChargeSimple(EnergyPlusData &state, int const usingVariable);

    void evaluateChargeBlock(EnergyPlusData &state, int const usingVariable);

    void evaluateRatchet(EnergyPlusData &state, int const usingVariable);

    void evaluateQualify(EnergyPlusData &state, int const usingVariable);

    void addMonthlyCharge(EnergyPlusData &state, int const usingVariable);

    void checkMinimumMonthlyCharge(EnergyPlusData &state, int const curTariff);

    void setNativeVariables(EnergyPlusData &state);

    void LEEDtariffReporting(EnergyPlusData &state);

    void WriteTabularTariffReports(EnergyPlusData &state);

    void showWarningsBasedOnTotal(EnergyPlusData &state);

    void getMaxAndSum(EnergyPlusData &state, int const varPointer, Real64 &sumResult, Real64 &maxResult);

    void ReportEconomicVariable(EnergyPlusData &state,
                                std::string const &titleString,
                                bool const includeCategory,
                                bool const showCurrencySymbol,
                                std::string const &forString);

    void selectTariff(EnergyPlusData &state);

    void GetMonthlyCostForResource(EnergyPlusData &state, DataGlobalConstants::ResourceType const inResourceNumber, Array1A<Real64> outMonthlyCosts);

} // namespace EconomicTariff

struct EconomicTariffData : BaseGlobalStruct
{

    int numEconVar = 0;
    int sizeEconVar = 0;

    // holds the outbound connections for each variable
    Array1D_int operand; // sized to sizeOperand
    int numOperand = 0;
    int sizeOperand = 0;

    int numTariff = 0;
    int numQualify = 0;
    int numChargeSimple = 0;
    int numChargeBlock = 0;
    int numRatchet = 0;
    int numComputation = 0;

    // list of pointers to variable, 0 end of line, negative indicate operations
    Array1D_int steps;
    Array1D_int stepsCopy;
    int numSteps = 0;
    int sizeSteps = 0;

    int topOfStack = 0;
    int sizeStack = 0;

    bool Update_GetInput = true;
    int addOperand_prevVarMe = 0;

    Array1D<EconomicTariff::EconVarType> econVar;
    EPVector<EconomicTariff::TariffType> tariff;
    EPVector<EconomicTariff::QualifyType> qualify;
    EPVector<EconomicTariff::ChargeSimpleType> chargeSimple;
    EPVector<EconomicTariff::ChargeBlockType> chargeBlock;
    EPVector<EconomicTariff::RatchetType> ratchet;
    EPVector<EconomicTariff::ComputationType> computation;
    Array1D<EconomicTariff::StackType> stack;

    void clear_state() override
    {
        this->numEconVar = 0;
        this->sizeEconVar = 0;
        this->operand.deallocate();
        this->numOperand = 0;
        this->sizeOperand = 0;
        this->numTariff = 0;
        this->numQualify = 0;
        this->numChargeSimple = 0;
        this->numChargeBlock = 0;
        this->numRatchet = 0;
        this->numComputation = 0;
        this->steps.deallocate();
        this->stepsCopy.deallocate();
        this->numSteps = 0;
        this->sizeSteps = 0;
        this->topOfStack = 0;
        this->sizeStack = 0;
        this->Update_GetInput = true;
        this->addOperand_prevVarMe = 0;
        this->econVar.deallocate();
        this->tariff.deallocate();
        this->qualify.deallocate();
        this->chargeSimple.deallocate();
        this->chargeBlock.deallocate();
        this->ratchet.deallocate();
        this->computation.deallocate();
        this->stack.deallocate();
    }
};

} // namespace EnergyPlus

#endif
