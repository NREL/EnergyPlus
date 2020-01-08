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


// C++ headers
#include <iostream>

// EnergyPlus headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/GroundHeatExchangerNew.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/FluidProperties.hh>


namespace EnergyPlus {

namespace GroundHeatExchangers {

    Real64 FluidWorker::get_cp(Real64 &temperature, const std::string &routineName)
    {
        return FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->loopNum).FluidName,
                                                      temperature, DataPlant::PlantLoop(this->loopNum).FluidIndex, routineName);
    }

    Real64 FluidWorker::get_k(Real64 &temperature, const std::string &routineName)
    {
        return FluidProperties::GetConductivityGlycol(DataPlant::PlantLoop(this->loopNum).FluidName,
                                                      temperature, DataPlant::PlantLoop(this->loopNum).FluidIndex, routineName);
    }

    Real64 FluidWorker::get_mu(Real64 &temperature, const std::string &routineName) {
        return FluidProperties::GetViscosityGlycol(DataPlant::PlantLoop(this->loopNum).FluidName,
                                                   temperature, DataPlant::PlantLoop(this->loopNum).FluidIndex, routineName);
    }

    Real64 FluidWorker::get_rho(Real64 &temperature, const std::string &routineName)
    {
        return FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->loopNum).FluidName,
                                                 temperature, DataPlant::PlantLoop(this->loopNum).FluidIndex, routineName);
    }

    Real64 FluidWorker::get_Pr(Real64 &temperature, const std::string &routineName)
    {
        // worker to get the Prandtl number

        // @param temperature: fluid temperature, C
        // @param routineName: caller routine name, str
        // @returns Prandtl number

        Real64 cp = this->get_cp(temperature, routineName);
        Real64 mu = this->get_mu(temperature, routineName);
        Real64 k = this->get_k(temperature, routineName);

        return cp * mu / k;
    }

    Real64 Interp1D::interpolate(Real64 &x)
    {
        // adapted from: https://stackoverflow.com/a/11675205/5965685
        static const Real64 INF = INF;
        // Assumes that "table" is sorted by .first
        // Check if x is out of bounds. extrapolate if able
        std::vector<std::pair<Real64, Real64> >::iterator it, it2;
        if (x > this->table.back().first) {
            if (this->extrapolate) {
                it = std::prev(this->table.end());
                it2 = it;
                --it2;
            } else {
                ShowFatalError(this->routineName + ": interpolation out of bounds.");
            }
        } else if (x < this->table[0].first) {
            if (this->extrapolate) {
                it2 = this->table.begin();
                it = it2;
                ++it;
            } else {
                ShowFatalError(this->routineName + ": interpolation out of bounds.");
            }
        } else {
            it = lower_bound(this->table.begin(), this->table.end(), std::pair<Real64, Real64> {x, -INF});
            // Corner case
            if (it == this->table.begin()) return it->second;
            it2 = it;
            --it2;
        }
        return it2->second + (it->second - it2->second)*(x - it2->first)/(it->first - it2->first);
    }

    void SubHourAgg::aggregate(Real64 &time, Real64 &energy) {

        Real64 const small_value = 1E-3;

        // check for iteration
        // if time is same as previous, we're iterating so do nothing.
        // else aggregate the energy
        if (std::abs(this->prev_update_time - time) < small_value) {
            this->subHrEnergy = 0;
        } else {
            // store current energy value
            this->energy.emplace_back(energy);

            // store respective time step for each bin
            this->dts.emplace_back(time - this->prev_update_time);

            // find upper bin edge referenced from current simulation time
            auto dt_u = calc_times(this->dts);

            // find lower bin edge referenced from current simulation time
//            auto dt_l =

            Real64 delteme = 0;

        }
    };

    Real64 SubHourAgg::calc_temporal_superposition(Real64 &EP_UNUSED(timeStep), Real64 &EP_UNUSED(flowRate)) {
        // not currently used
        // fatal if called
        ShowFatalError(this->routineName + ": calculate temporal superpostion not implemented.");
        return 0;
    };

    Real64 SubHourAgg::get_g_value(Real64 &EP_UNUSED(time)) {
        // not currently used
        // fatal if called
        ShowFatalError(this->routineName + ": get g value is not implemented.");
        return 0;
    };

    Real64 SubHourAgg::get_q_prev() {
        // not currently used
        // fatal if called
        ShowFatalError(this->routineName + ": get previous q value is not implemented.");
        return 0;
    };

    Real64 Pipe::calcTransitTime(Real64 flowRate, Real64 temperature)
    {
        // Compute the fluid transit time

        // @param flowRate: mass flow rate, kg/s
        // @param temperature: temperature, C
        // @returns pipe transit time

        static std::string const routineName("Pipe::calcTransitTime");

        Real64 rho = this->fluid.get_rho(temperature, routineName);
        Real64 vdot = flowRate / rho;
        return this->volFluid / vdot;
    }

    void Pipe::simulate(Real64 time, Real64 timeStep, Real64 flowRate, Real64 inletTemp)
    {
        //  Simulate the inletTemp response of an adiabatic pipe with internal fluid mixing.

        //  Rees, S.J. 2015. 'An extended two-dimensional borehole heat exchanger model for
        //  simulation of short and medium timescale thermal response.' Renewable Energy. 83: 518-526.

        //  Skoglund, T, and P. Dejmek. 2007. 'A dynamic object-oriented model for efficient
        //  fluid simulation of fluid dispersion in turbulent flow with varying fluid properties.'
        //  Chem. Eng. Sci.. 62: 2168-2178.

        //  Bischoff, K.B., and O. Levenspiel. 1962. 'Fluid dispersion--generalization and comparision
        //  of mathematical models--II; Comparison of models.' Chem. Eng. Sci.. 17: 257-264.

        // @param time: simulation time, s
        // @param timeStep: simulation time step, s
        // @param flowRate: mass flow rate, kg/s
        // @param inletTemp: fluid inlet temperature, C

        static std::string const routineName("Pipe::simulate");

        if (timeStep > 0) {
            // Reynolds number
            Real64 Re = this->mdotToRe(flowRate, inletTemp);

            // total transit time
            Real64 tau = this->calcTransitTime(flowRate, inletTemp);

            // Peclet number
            // Rees Eq. 18
            Real64 Pe = 1 / (2 * this->innerRadius / this->length * (3E7 * std::pow(Re, -2.1) + 1.35 * std::pow(Re, -0.125)));

            // transit time for plug-flow cell
            // Rees Eq. 17
            Real64 tau_n = tau * std::sqrt(2 / (this->numCells * Pe));

            // transit time for ideal-mixed cells
            Real64 tau_0 = tau - this->numCells * tau_n;

            // volume flow rate
            Real64 rho = this->fluid.get_rho(inletTemp, routineName);
            Real64 v_dot = flowRate / rho;

            // volume for ideal-mixed cells
            Real64 v_n = tau_n * v_dot;

            // check for sub-stepping
            // limit maximum step to 10% of the transit time
            int num_sub_steps = 1;
            Real64 dt_sub = timeStep;
            if (timeStep / tau > 0.1) {
                num_sub_steps = std::ceil(timeStep/ tau);
                dt_sub = timeStep / num_sub_steps;
            }

            Real64 t_sub = time;

            // setup tri-diagonal equations
            std::vector<Real64> a(this->numCells, -v_dot);
            a[0] = 0;
            std::vector<Real64> b(this->numCells, v_n / dt_sub + v_dot);
            b[0] = 1;
            std::vector<Real64> c(this->numCells, 0);
            for (int step = 0; step < num_sub_steps; ++step) {
                // vector multiply
                std::vector<Real64> d;
                for (double cellTemp : this->cellTemps) {
                    d.push_back(v_n / dt_sub * cellTemp);
                }
                if (this->applyTransitDelay) {
                    this->logInletTemps(inletTemp, t_sub + dt_sub);
                    d[0] = this->plugFlowOutletTemp(t_sub + dt_sub - tau_0);
                } else {
                    d[0] = inletTemp;
                }

                // solve for cell temps
                this->cellTemps = TDMA(a, b, c, d);

                // update time
                t_sub += dt_sub;
            }
            this->outletTemp = this->cellTemps.back();
        }
    }

    Real64 Pipe::plugFlowOutletTemp(Real64 time)
    {
        // computes the plug-flow outlet temperature

        // @param time: simulation time, s
        // @returns outlet temperature for respective time, C

        if (time <= 0) {
            return this->inletTemps[0];
        }

        int idx = 0;
        for (auto it = this->inletTempTimes.begin(); it != this->inletTempTimes.end(); ++it) {
            Real64 t_l = *it;
            if (t_l > time) {
                int idx_h = idx;
                int idx_l = idx - 1;
                t_l = this->inletTempTimes[idx_l];
                Real64 t_h = this->inletTempTimes[idx_h];
                Real64 temp_l = this->inletTemps[idx_l];
                Real64 temp_h = this->inletTemps[idx_h];

                // eliminate old history
                std::vector<int> x(idx_l);
                std::iota(std::begin(x), std::end(x), 0);

                for (auto i : x) {
                    this->inletTemps.pop_front();
                    this->inletTempTimes.pop_front();
                }
                return linInterp(time, t_l, t_h, temp_l, temp_h);
            }
            ++idx;
        }
    }

    void Pipe::logInletTemps(Real64 inletTemp, Real64 time)
    {
        this->inletTemps.emplace_back(inletTemp);
        this->inletTempTimes.emplace_back(time);
    }

    Real64 Pipe::mdotToRe(Real64 flowRate, Real64 temperature)
    {
        // convert mass flow rate to Reynolds number

        // @param flowRate: mass flow rate, kg/s
        // @param temperature: temperature, C

        // @returns Reynolds number

        static std::string const routineName("Pipe::calcTransitTime");
        Real64 mu = this->fluid.get_mu(temperature, routineName);
        return 4 * flowRate / (mu * Pi * this->innerDia);
    }

    Real64 Pipe::calcFrictionFactor(Real64 Re)
    {
        // smooth pipe friction factor

        // @param Re: Reynolds number
        // @returns friction factor

        Real64 lowRe = 1500;
        Real64 highRe = 5000;

        if (Re < lowRe) {
            this->friction = laminarFrictionFactor(Re);
        } else if (lowRe <= Re && Re < highRe) {
            Real64 fLow = laminarFrictionFactor(Re);
            Real64 fHigh = turbulentFrictionFactor(Re);
            Real64 sigma = smoothingFunc(Re, 3000, 450);
            this->friction = (1 - sigma) * fLow + sigma * fHigh;
        } else
            this->friction = turbulentFrictionFactor(Re);
        return this->friction;
    }

    Real64 Pipe::calcConductionResistance()
    {
        // Calculates the thermal resistance of a pipe, in [K/(W/m)].

        // Javed, S. and Spitler, J.D. 2017. 'Accuracy of borehole thermal resistance calculation methods
        // for grouted single U-tube ground heat exchangers.' Applied Energy. 187: 790-806.

        // @returns conduction resistance, K/(W/m)

        return std::log(this->outDia / this->innerDia) / (2 * Pi * this->k);
    }

    Real64 Pipe::turbulentNusselt(Real64 Re, Real64 temperature)
    {
        // turbulent Nusselt number

        // Gnielinski, V. 1976. 'New equations for heat and mass transfer in turbulent pipe and channel flow.'
        // International Chemical Engineering 16(1976), pp. 359-368.

        // @param Re: Reynolds number
        // @param temperature: temperature, C
        // @returns Nusselt number

        static std::string const routineName("Pipe::turbulentNusselt");

        // friction factor
        Real64 f = this->calcFrictionFactor(Re);

        // Prandtl number
        Real64 Pr = this->fluid.get_Pr(temperature, routineName);

        return (f / 8) * (Re - 1000) * Pr / (1 + 12.7 * std::pow(f / 8, 0.5) * (std::pow(Pr, 2.0 / 3.0) - 1));
    }

    Real64 Pipe::calcConvectionResistance(Real64 flowRate, Real64 temperature)
    {
        // Calculates the convection resistance using Gnielinski and Petukhov, in [k/(W/m)]

        // Gnielinski, V. 1976. 'New equations for heat and mass transfer in turbulent pipe and channel flow.'
        // International Chemical Engineering 16(1976), pp. 359-368.

        // @param flow_rate: mass flow rate, kg/s
        // @param temperature: temperature, C
        // @returns convection resistance, K/(W/m)

        static std::string const routineName("Pipe::calcConvectionResistance");

        Real64 lowRe = 2000;
        Real64 highRe = 4000;

        Real64 Re = this->mdotToRe(flowRate, temperature);
        Real64 Nu = 0;

        if (Re < lowRe) {
            Nu = laminarNusselt();
        } else if (lowRe <= Re && Re < highRe) {
            Real64 NuLow = laminarNusselt();
            Real64 NuHigh = turbulentNusselt(Re, temperature);
            Real64 sigma = smoothingFunc(Re, 3000, 150);
            Nu = (1 - sigma) * NuLow + sigma * NuHigh;
        } else
            Nu = turbulentNusselt(Re, temperature);

        Real64 k = this->fluid.get_k(temperature, routineName);
        this->resistConv = 1 / (Nu * DataGlobals::Pi * k);
        return this->resistConv;
    }

    Real64 Pipe::calcResistance(Real64 flowRate, Real64 temperature) {
        // Calculates the combined conduction and convection pipe resistance

        // Javed, S. and Spitler, J.D. 2017. 'Accuracy of borehole thermal resistance calculation methods
        // for grouted single U-tube ground heat exchangers.' Applied Energy. 187: 790-806.

        // Equation 3

        // @param flowRate: mass flow rate, kg/s
        // @param temperature: temperature, C
        // @returns pipe resistance, K/(W/m)

        this->resistPipe = this->calcConvectionResistance(flowRate, temperature) + this->calcConductionResistance();
        return this->resistPipe;

    }

} // namespace GroundHeatExchangers

} // namespace EnergyPlus
