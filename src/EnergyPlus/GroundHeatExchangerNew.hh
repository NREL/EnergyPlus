// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#ifndef GroundHeatExchangers_hh_INCLUDED
#define GroundHeatExchangers_hh_INCLUDED

// C++ headers
#include <iostream>
#include <deque>

// JSON headers
#include <../third_party/nlohmann/json.hpp>

// EnergyPlus headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataGlobals.hh>

using json = nlohmann::json;

namespace EnergyPlus {

namespace GroundHeatExchangers {

    struct BaseProps
    {
        // member variables
        Real64 k = 0.0;           // Thermal conductivity [W/m-K]
        Real64 rho = 0.0;         // Density [kg/m3]
        Real64 cp = 0.0;          // Specific heat [J/kg-K]
        Real64 rhoCp = 0.0;       // Heat capacity [J/m3-K]
        Real64 diffusivity = 0.0; // Thermal diffusivity [m2/s]

        // constructor
        explicit BaseProps(const json &j)
        {
            this->k = j["conductivity"];
            this->rho = j["density"];
            this->cp = j["specific-heat"];
            this->rhoCp = rho * cp;
            this->diffusivity = k / this->rhoCp;
        }

        // default constructor
        BaseProps() = default;

        // destructor
        ~BaseProps() = default;
    };

    struct FluidWorker
    {
        // E+ member variables
        int loopNum = 0;

        // constructor
        explicit FluidWorker(const json &j) {
            this->loopNum = j["loop-num"];
        }

        // default constructor
        FluidWorker() = default;

        // destructor
        ~FluidWorker() = default;

        // member functions
        Real64 get_cp(Real64 &temperature, const std::string &routineName);
        Real64 get_k(Real64 &temperature, const std::string &routineName);
        Real64 get_mu(Real64 &temperature, const std::string &routineName);
        Real64 get_rho(Real64 &temperature, const std::string &routineName);
        Real64 get_Pr(Real64 &temperature, const std::string &routineName);
    };

    struct Pipe : public BaseProps, FluidWorker
    {
        // E+ member variables
        int loopNum = 0;

        // parent classes
        FluidWorker fluid;

        // model member variables
        Real64 outDia = 0.0;        // Outer diameter [m]
        Real64 innerDia = 0.0;      // Inner diameter [m]
        Real64 length = 0.0;        // Length [m]
        Real64 outRadius = 0.0;     // Outer radius [m]
        Real64 innerRadius = 0.0;   // Inner radius [m]
        Real64 wallThickness = 0.0; // Pipe wall thickness [m]
        Real64 areaCrOuter = 0.0;   // Outer cross-sectional area [m2]
        Real64 areaCrInner = 0.0;   // Inner cross-sectional area [m2]
        Real64 areaCrPipe = 0.0;    // Pipe wall cross-sectional area [m2]
        Real64 areaSurfOuter = 0.0; // Pipe outer surface area [m2]
        Real64 areaSurfInner = 0.0; // Pipe inner surface area [m2]
        Real64 volTotal = 0.0;      // Total pipe volume [m3]
        Real64 volFluid = 0.0;      // Fluid volume [m3]
        Real64 volPipeWall = 0.0;   // Pipe wall volume [m3]
        Real64 friction = 0.0;      // Friction factor [-]
        Real64 resistPipe = 0.0;    // Total pipe resistance [K/(W/m)]
        Real64 resistConv = 0.0;    // Pipe convection resistance [K/(W/m)]
        int const numCells = 16;    // Number of pipe elements
        std::vector<Real64> cellTemps = {
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; // Pipe temperature for each node
        std::deque<Real64> inletTemps = {0.0};                                              // Inlet temperature history [C]
        std::deque<Real64> inletTempTimes = {0.0};                                           // Times for respective inlet temperatures [s]
        Real64 outletTemp = 0.0;                                                             // Pipe outlet temperature [C]
        bool applyTransitDelay = true;

        // constructor
        explicit Pipe(const json &j)
        {
            // properties
            BaseProps tmpProps(j);
            this->fluid = FluidWorker(j);
            this->k = tmpProps.k;
            this->rho = tmpProps.rho;
            this->cp = tmpProps.cp;
            this->rhoCp = tmpProps.rhoCp;
            this->diffusivity = tmpProps.diffusivity;

            // geometry
            this->outDia = j["outer-diameter"];
            this->innerDia = j["inner-diameter"];
            this->length = j["length"];
            this->outRadius = this->outDia / 2;
            this->innerRadius = this->innerDia / 2;
            this->wallThickness = this->outRadius - this->innerRadius;

            // areas
            this->areaCrOuter = (DataGlobals::Pi / 4) * std::pow(this->outDia, 2);
            this->areaCrInner = (DataGlobals::Pi / 4) * std::pow(this->innerDia, 2);
            this->areaCrPipe = this->areaCrOuter - this->areaCrInner;
            this->areaSurfOuter = DataGlobals::Pi * this->outDia * this->length;
            this->areaSurfInner = DataGlobals::Pi * this->innerDia * this->length;

            // volumes
            this->volTotal = this->areaCrOuter * this->length;
            this->volFluid = this->areaCrInner * this->length;
            this->volPipeWall = this->volTotal - this->volFluid;

            Real64 initTemp = j["initial-temperature"];
            std::replace(this->cellTemps.begin(), this->cellTemps.end(), 0.0, initTemp);
            std::replace(this->inletTemps.begin(), this->inletTemps.end(), 0.0, initTemp);
        }

        // default constructor
        Pipe() = default;

        // destructor
        ~Pipe() = default;

        // members functions
        Real64 calcTransitTime(Real64 flowRate, Real64 temperature);
        void simulate(Real64 time, Real64 timeStep, Real64 flowRate, Real64 inletTemp);
        Real64 plugFlowOutletTemp(Real64 time);
        void logInletTemps(Real64 inletTemp, Real64 time);
        Real64 mdotToRe(Real64 flowRate, Real64 temperature);
        Real64 calcFrictionFactor(Real64 Re);
        Real64 calcConductionResistance();
        Real64 calcConvectionResistance(Real64 flowRate, Real64 temperature);
        Real64 calcResistance(Real64 flowRate, Real64 temperature);
        Real64 turbulentNusselt(Real64 Re, Real64 temperature);

        static Real64 laminarNusselt()
        {
            // laminar Nusselt number for smooth pipes
            // mean(4.36, 3.66)

            return 4.01;
        }

        static Real64 laminarFrictionFactor(Real64 Re)
        {
            // laminar friction factor

            // @param Re: Reynolds number

            return 64 / Re;
        }

        static Real64 turbulentFrictionFactor(Real64 Re)
        {
            // turbulent friction factor

            // Petukhov, B. S. (1970). Advances in Heat Transfer, volume 6, chapter Heat transfer and
            // friction in turbulent pipe flow with variable physical properties, pages 503–564.
            // Academic Press, Inc., New York, NY.

            // @param Re: Reynolds number

            return std::pow(0.79 * std::log(Re) - 1.64, -2.0);
        }
    };

    struct Interp1D
    {
        std::vector<Real64> x_data;
        std::vector<Real64> y_data;
        std::string routineName;
        std::vector<std::pair<Real64, Real64> > table;
        bool extrapolate = false;

        // constructor
        Interp1D(std::vector<Real64> &x_data, std::vector<Real64> &y_data,
                 std::string &routineName, bool extrapolate = false) {

            this->x_data = x_data;
            this->y_data = y_data;
            this->routineName = routineName;
            this->extrapolate = extrapolate;

            if (this->x_data.size() == this->y_data.size()) {
                for (std::size_t i = 0; i != this->x_data.size(); ++i) {
                    this->table.emplace_back(std::pair<Real64, Real64> {this->x_data[i], this->y_data[i]});
                }
            } else {
                ShowFatalError(routineName + ": Number of X and Y data must be equal.");
            }
            // add option later to ask if the data needs to be sorted
            // std::sort(table.begin(), table.end());
        }

        // default constructor
        Interp1D() = default;

        // destructor
        ~Interp1D() = default;

        // member functions
        Real64 interpolate(Real64 &x);
    };

    struct BaseAgg
    {
        // member variables
        Real64 ts;  // GHE time scale
        Interp1D g_data;  // g-function data
        std::vector<Real64> energy;  // energy history
        std::vector<Real64> dts;  // time steps
        Real64 prev_update_time;  // previous update time

        // constructor
        BaseAgg() : ts(0.0), prev_update_time(0.0)
        {};

        // destructor
        ~BaseAgg() = default;

        // member functions
        static std::vector<Real64> calc_times(std::vector<Real64> &times)
        {
            std::vector<Real64> v = times;
            std::reverse(std::begin(v), std::end(v));
            std::vector<Real64> sums (v.size());
            std::partial_sum(std::begin(v), std::end(v), sums.begin());
            std::reverse(std::begin(sums), std::end(sums));
            return sums;
        }

        // virtual functions
        virtual void aggregate(Real64 &time, Real64 &energy) = 0;
        virtual Real64 calc_temporal_superposition(Real64 &timeStep, Real64 & flowRate) = 0;
        virtual Real64 get_g_value(Real64 &time) = 0;
        virtual Real64 get_q_prev() = 0;

    };

    struct SubHourAgg : BaseAgg
    {
        std::string routineName = "Subhourly Aggregation";
        Real64 subHrEnergy = 0.0;

        // constructor
        explicit SubHourAgg(const json &j)
        {
            this->energy.emplace_back(0.0);
            this->dts.emplace_back(DataGlobals::SecInHour);
            this->ts = j["time-scale"];
            std::vector<Real64> lntts = j["g-function-data"]["lntts"];
            std::vector<Real64> g = j["g-function-data"]["g"];
            this->g_data = Interp1D(lntts, g, routineName, true);
        };

        // destructor
        ~SubHourAgg() = default;

        // member functions
        void aggregate(Real64 &time, Real64 &energy) override;
        Real64 calc_temporal_superposition(Real64 &timeStep, Real64 & flowRate) override;
        Real64 get_g_value(Real64 &time) override;
        Real64 get_q_prev() override;
    };


} // namespace GroundHeatExchangers

} // namespace EnergyPlus

#endif
