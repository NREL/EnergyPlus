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

#include <algorithm>
#include <cmath>
#include "interconnect.h"
#include "htf_props.h"
#include "sam_csp_util.h"

double pi = acos(-1.);
double T_ref_K = 298.150;
int NA_cpnt = -1;

double FrictionFactor_FlexHose(double Re, double D) {
	// Returns the friction factor for corrugated flex hose, model Witzenmann RS 330 / 331
	//  (this model hose has average characteristics for the hoses with available data)
	// see:
	//		Witzenmann Metal Hose Manual
	//		R. Gropp, Pforzheim, 1974. Flow resistance of flexible metallic lines
	//		Excel spreadsheet Flex_hose_interpolation_test.xlsx

	assert(Re > 6000);
    double ff;
	double d_in_mm = D * 1.e3;					// convert diameter from m to mm for use in the models

	// Boundaries for the turbulent transition region, set by low and high Reynold's numbers for each hose diameter
	const double D_lo_mm = 6.;				// low diameter, mm
	const double Re_lo_D_lo = 3.46e4;       // Re at low transition point for low diameter
	const double ff_lo_D_lo = 9.92e-2;      // ff at low transition point for low diameter
	const double Re_hi_D_lo = 1.24e5;       // Re at high transition point for low diameter
	const double ff_hi_D_lo = 2.21e-1;      // ff at high transition point for low diameter
	const double D_hi_mm = 150.;			// high diameter, mm
	const double Re_lo_D_hi = 1.37e5;       // Re at low transition point for high diameter
    const double ff_lo_D_hi = 5.48e-2;      // ff at low transition point for high diameter
    const double Re_hi_D_hi = 4.97e5;       // Re at high transition point for high diameter
    const double ff_hi_D_hi = 9.86e-2;      // ff at high transition point for high diameter

	// Reynold's number turbulent transition points as function of diameter [mm]
	double Re_lo_D_in;								// Low Re transition point
	const double m_Re_lo = 3.00e4;					// scale term in model
	const double b_incpt_Re_lo = -1.10e4;			// intercept
	Re_lo_D_in = m_Re_lo * log(d_in_mm) + b_incpt_Re_lo;		// log is natural log

	double Re_hi_D_in;								// High Re transition point
	const double m_Re_hi = 1.12e5;					// scale term in model
	const double b_incpt_Re_hi = -6.40e4;			// intercept
	Re_hi_D_in = m_Re_hi * log(d_in_mm) + b_incpt_Re_hi;		// log is natural log


	// Friction factor at turbulent transition points as function of Re transition point and diameter [mm]
	double m_ff_vs_Re_lo;							// slope of Re loci vs. ff at low transition point
	double ff_lo_D_in;								// friction factor at low transition point
	m_ff_vs_Re_lo = (log10(ff_lo_D_hi) - log10(ff_lo_D_lo)) / (log10(Re_lo_D_hi) - log10(Re_lo_D_lo));
	ff_lo_D_in = pow(10, log10(ff_lo_D_lo) + m_ff_vs_Re_lo * (log10(Re_lo_D_in) - log10(Re_lo_D_lo)));

	double m_ff_vs_Re_hi;							// slope of Re loci vs. ff at high transition point
	double ff_hi_D_in;								// friction factor at high transition point
	m_ff_vs_Re_hi = (log10(ff_hi_D_hi) - log10(ff_hi_D_lo)) / (log10(Re_hi_D_hi) - log10(Re_hi_D_lo));
	ff_hi_D_in = pow(10, log10(ff_hi_D_lo) + m_ff_vs_Re_hi * (log10(Re_hi_D_in) - log10(Re_hi_D_lo)));


	// Friction factor on transition line as function of Re and D [mm]
	//   values are constant beyond transition line
    double slope;                                   // slope of transition line for input diameter
    slope = (log10(ff_hi_D_in) - log10(ff_lo_D_in)) / (log10(Re_hi_D_in) - log10(Re_lo_D_in));
    if (Re < Re_lo_D_in) {
        ff = ff_lo_D_in;
    }
    else if (Re > Re_hi_D_in) {
        ff = ff_hi_D_in;
    }
    else {
        ff = pow(10, log10(ff_lo_D_in) + slope * (log10(Re) - log10(Re_lo_D_in)));
    }

	return ff;
}

IntcOutputs::IntcOutputs() {
    heat_loss = 0;
    temp_drop = 0;
    temp_out = 0;
    temp_ave = 0;
    pressure_drop = 0;
    pressure_out = 0;
    pressure_ave = 0;
    internal_energy = 0;
}


intc_cpnt::intc_cpnt(double k, double d, double l, double rough, double u, double mc, CpntType type)
    :k_(k),
    d_in_(d),
    l_(l),
    rough_(rough),
    hl_coef_(u),
    mc_(mc),
    wall_thick_(0),
    Type(type),
    OuterSurfArea_valid_(false),
    OuterSurfArea_(0),
    FlowArea_valid_(false),
    FlowArea_(0),
    FluidVolume_valid_(false),
    FluidVolume_(0)
{
    if (k_ < 0) throw std::invalid_argument("The minor loss coefficient (K) cannot be less than 0.");
    if (d_in_ < 0) throw std::invalid_argument("The inner diameter (D_in) cannot be less than 0.");
    if (l_ < 0) throw std::invalid_argument("The length (L) cannot be less than 0.");
    if (rough_ < 0) throw std::invalid_argument("The relative roughness cannot be less than 0.");
    if (hl_coef_ < 0) throw std::invalid_argument("The heat loss coefficient (U) cannot be less than 0.");
    if (mc_ < 0) throw std::invalid_argument("The heat capacity cannot be less than 0.");

    setWallThick(CSP::WallThickness(d_in_));
}

intc_cpnt::~intc_cpnt()
{
}

double intc_cpnt::getK() const {
    return k_;
}

void intc_cpnt::setK(double k) {
    if (k >= 0)
    {
        k_ = k;
    }
    else {
        throw std::invalid_argument("The minor loss coefficient (K) cannot be less than 0.");
    }
}

double intc_cpnt::getD() const {
    return d_in_;
}

void intc_cpnt::setD(double d) {
    if (d >= 0)
    {
        d_in_ = d;
        OuterSurfArea_valid_ = false;
    }
    else {
        throw std::invalid_argument("The inner diameter (D_in) cannot be less than 0.");
    }
}

double intc_cpnt::getLength() const {
    return l_;
}

void intc_cpnt::setLength(double l) {
    if (l >= 0)
    {
        l_ = l;
        OuterSurfArea_valid_ = false;
    }
    else {
        throw std::invalid_argument("The length (L) cannot be less than 0.");
    }
}

double intc_cpnt::getRelRough() const {
    return rough_;
}

void intc_cpnt::setRelRough(double rough) {
    if (rough >= 0)
    {
        rough_ = rough;
    }
    else {
        throw std::invalid_argument("The relative roughness cannot be less than 0.");
    }
}

double intc_cpnt::getHLCoef() const {
    return hl_coef_;
}

void intc_cpnt::setHLCoef(double u) {
    if (u >= 0)
    {
        hl_coef_ = u;
    }
    else {
        throw std::invalid_argument("The heat loss coefficient (U) cannot be less than 0.");
    }
}

double intc_cpnt::getHeatCap() const {
    return mc_;
}

void intc_cpnt::setHeatCap(double mc) {
    if (mc >= 0)
    {
        mc_ = mc;
    }
    else {
        throw std::invalid_argument("The heat capacity cannot be less than 0.");
    }
}

double intc_cpnt::getWallThick() const {
    return wall_thick_;
}

void intc_cpnt::setWallThick(double wall_thick) {
    if (wall_thick >= 0)
    {
        wall_thick_ = wall_thick;
    }
    else {
        throw std::invalid_argument("The wall thickness cannot be less than 0.");
    }
}

CpntType intc_cpnt::getType() const {
    return Type;
}

double intc_cpnt::getOuterSurfArea() {
    if (!OuterSurfArea_valid_) { calcOuterSurfArea(); }
    return OuterSurfArea_;
}

void intc_cpnt::calcOuterSurfArea() {
    OuterSurfArea_ = pi * (d_in_ + 2*wall_thick_) * l_;
    OuterSurfArea_valid_ = true;
}

double intc_cpnt::getFlowArea() {
    if (!FlowArea_valid_) { calcFlowArea(); }
    return FlowArea_;
}

void intc_cpnt::calcFlowArea() {
    FlowArea_ = pi * (d_in_ * d_in_) / 4;
    FlowArea_valid_ = true;
}

double intc_cpnt::getFluidVolume() {
    if (!FluidVolume_valid_) { calcFluidVolume(); }
    return FluidVolume_;
}

void intc_cpnt::calcFluidVolume() {
    FluidVolume_ = pi * (d_in_ * d_in_) / 4. * l_;
    FluidVolume_valid_ = true;
}

double intc_cpnt::HeatLoss(double T_cpnt, double T_db) {
    double A = getOuterSurfArea();  // fun needed b/c area is not always valid
    return hl_coef_ * A * (T_cpnt - T_db);
}

double intc_cpnt::TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double heatLoss) {
    double cp = fluidProps->Cp(T_in) * 1000;  // J/kg-K
    return heatLoss / (m_dot * cp);   // positive value means T_out < T_in
}

double intc_cpnt::TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double T_cpnt, double T_db) {
    double cp = fluidProps->Cp(T_in) * 1000;  // J/kg-K
    return HeatLoss(T_cpnt, T_db) / (m_dot * cp);   // positive value means T_out < T_in
}

double intc_cpnt::PressureDrop(HTFProperties *fluidProps, double m_dot, double T_htf_ave, double P_htf_ave) {
    double rho = fluidProps->dens(T_htf_ave, P_htf_ave);
    double vel = m_dot / ( rho * getFlowArea() );
    double Re, ff;

    switch (Type)
    {
        case CpntType::Fitting:
            return CSP::MinorPressureDrop(vel, rho, k_);
        case CpntType::Pipe:
            Re = fluidProps->Re(T_htf_ave, P_htf_ave, vel, d_in_);
            ff = CSP::FrictionFactor(rough_ / d_in_, Re);
            return CSP::MajorPressureDrop(vel, rho, ff, l_, d_in_);
        case CpntType::Flex_Hose:
            Re = fluidProps->Re(T_htf_ave, P_htf_ave, vel, d_in_);
			if (Re < 6000) {
				ff = CSP::FrictionFactor(rough_ / d_in_, Re);  // call standard pipe friction factor function
			}
			else {
				ff = FrictionFactor_FlexHose(Re, d_in_);
                // TODO : Implement resistance coefficient for a curved installation
			}
            return CSP::MajorPressureDrop(vel, rho, ff, l_, d_in_);
        default:
            throw std::invalid_argument("This component type has no pressure drop calculation.");
    }
}

double intc_cpnt::InternalEnergy(HTFProperties *fluidProps, double T_cpnt, double T_htf_ave, double P_htf_ave) {
    double cp = fluidProps->Cp(T_htf_ave) * 1000;  // J/kg-K
    return (getFluidVolume() * fluidProps->dens(T_htf_ave, P_htf_ave) * cp +
        getHeatCap()) * (T_cpnt - T_ref_K);
}

IntcOutputs intc_cpnt::State(HTFProperties *fluidProps, double m_dot, double T_in, double T_cpnt, double T_db, double P_htf_ave) {
    IntcOutputs output;
    output.heat_loss = HeatLoss(T_cpnt, T_db);
    output.temp_drop = TempDrop(fluidProps, m_dot, T_in, output.heat_loss);
    output.temp_out = T_in - output.temp_drop;
    output.temp_ave = (T_in + output.temp_out) / 2;
    output.pressure_drop = PressureDrop(fluidProps, m_dot, output.temp_ave, P_htf_ave);
    output.pressure_out = P_htf_ave - output.pressure_drop / 2;  // just an approximation to fill an output
    output.pressure_ave = P_htf_ave;
    output.internal_energy = InternalEnergy(fluidProps, T_cpnt, output.temp_ave, P_htf_ave);

    return output;
}



interconnect::interconnect()
    :N_cpnts_(0),
    Length_valid_(false),
    l_(0),
    HeatCap_valid_(false),
    mc_(0),
    OuterSurfArea_valid_(false),
    OuterSurfArea_(0),
    FluidVolume_valid_(false),
    FluidVolume_(0)
{
}

interconnect::interconnect(HTFProperties *fluidProps, double *k, double *d, double *l, double *rel_rough, double *u, double *mc, double *type, int n_cpnts)
    :N_cpnts_(0),
    Length_valid_(false),
    l_(0),
    HeatCap_valid_(false),
    mc_(0),
    OuterSurfArea_valid_(false),
    OuterSurfArea_(0),
    FluidVolume_valid_(false),
    FluidVolume_(0)
{
    import_cpnts(k, d, l, rel_rough, u, mc, type, n_cpnts);
    setFluidProps(fluidProps);
}

interconnect::~interconnect()
{
}

void interconnect::import_cpnts(double *k, double *d, double *l, double *rel_rough, double *u, double *mc, double *type, int num_cpnts)
{
    std::size_t max_cpnts = num_cpnts;
    std::size_t n_cpnts = 0;  // double check number of components
    while (k[n_cpnts] != NA_cpnt && n_cpnts < max_cpnts) { n_cpnts++; }

    if (!cpnts.empty()) { cpnts.clear(); }
    cpnts.reserve(n_cpnts);

    intc_cpnt cpnt;
    for (int i = 0; i < n_cpnts; i++) {
        if (type[i] < 0 || type[i] >= static_cast<int>(CpntType::FINAL_ENTRY)) {
            throw std::invalid_argument("The component type is out of range at index" + std::to_string(i));
        }
        cpnt = intc_cpnt(k[i], d[i], l[i], rel_rough[i], u[i], mc[i], static_cast<CpntType>((int)type[i]));
        cpnts.push_back(cpnt);
        N_cpnts_++;
        l_ += cpnt.getLength();
        mc_ += cpnt.getHeatCap();
        OuterSurfArea_ += cpnt.getOuterSurfArea();
        FluidVolume_ += cpnt.getFluidVolume();
    }
    Length_valid_ = true;
    HeatCap_valid_ = true;
    OuterSurfArea_valid_ = true;
    FluidVolume_valid_ = true;
}

void interconnect::resetValues() {
    cpnts.clear();
    N_cpnts_ = 0;
    FluidProps_ = NULL;
    Length_valid_ = false;
    l_ = 0;
    HeatCap_valid_ = false;
    mc_ = 0;
    OuterSurfArea_valid_ = false;
    OuterSurfArea_ = 0;
    FluidVolume_valid_ = false;
    FluidVolume_ = 0;
}

void interconnect::setFluidProps(HTFProperties *fluidProps) {
    FluidProps_ = fluidProps;
}

int interconnect::getNcpnts() {
    return N_cpnts_;
}

double interconnect::getK(std::size_t cpnt) const
{
    return cpnts.at(cpnt).getK();
}

double interconnect::getD(std::size_t cpnt) const
{
    return cpnts.at(cpnt).getD();
}

double interconnect::getLength() {
    if (!Length_valid_) { calcLength(); }
    return l_;
}

double interconnect::getLength(std::size_t cpnt) const
{
    return cpnts.at(cpnt).getLength();
}

void interconnect::calcLength() {
    l_ = 0;
    for (std::vector<intc_cpnt>::iterator it = cpnts.begin(); it < cpnts.end(); ++it) {
        l_ += it->getLength();  // intc_cpnt::getLength()
    }
    Length_valid_ = true;
}

double interconnect::getRelRough(std::size_t cpnt) const
{
    return cpnts.at(cpnt).getRelRough();
}

double interconnect::getHLCoef(std::size_t cpnt) const
{
    return cpnts.at(cpnt).getHLCoef();
}

double interconnect::getHeatCap() {
    if (!HeatCap_valid_) { calcHeatCap(); }
    return mc_;
}

double interconnect::getHeatCap(std::size_t cpnt) const
{
    return cpnts.at(cpnt).getHeatCap();
}

void interconnect::calcHeatCap() {
    mc_ = 0;
    for (std::vector<intc_cpnt>::iterator it = cpnts.begin(); it < cpnts.end(); ++it) {
        mc_ += it->getHeatCap();  // intc_cpnt::getHeatCap()
    }
    HeatCap_valid_ = true;
}

CpntType interconnect::getType(std::size_t cpnt) const {
    return cpnts.at(cpnt).getType();
}

double interconnect::getOuterSurfArea() {
    if (!OuterSurfArea_valid_) { calcOuterSurfArea(); }
    return OuterSurfArea_;
}

double interconnect::getOuterSurfArea(std::size_t cpnt) {
    return cpnts.at(cpnt).getOuterSurfArea();
}

void interconnect::calcOuterSurfArea() {
    OuterSurfArea_ = 0;
    for (std::vector<intc_cpnt>::iterator it = cpnts.begin(); it < cpnts.end(); ++it) {
        OuterSurfArea_ += it->getOuterSurfArea();  // intc_cpnt::getOuterSurfArea()
    }
    OuterSurfArea_ = true;
}

double interconnect::getFlowArea(std::size_t cpnt) {
    return cpnts.at(cpnt).getFlowArea();
}

double interconnect::getFluidVolume() {
    if (!FluidVolume_valid_) { calcFluidVolume(); }
    return FluidVolume_;
}

double interconnect::getFluidVolume(std::size_t cpnt) {
    return cpnts.at(cpnt).getFluidVolume();
}

void interconnect::calcFluidVolume() {
    FluidVolume_ = 0;
    for (std::vector<intc_cpnt>::iterator it = cpnts.begin(); it < cpnts.end(); ++it) {
        FluidVolume_ += it->getFluidVolume();  // intc_cpnt::getFluidVolume()
    }
    FluidVolume_valid_ = true;
}

IntcOutputs interconnect::State(double m_dot, double T_in, double T_db, double P_in) {
    IntcOutputs output;
    
    if (N_cpnts_ > 0) {
        IntcOutputs IntcOutput;
        double T_out_prev = T_in;
        double P_out_prev = P_in;

        for (std::vector<intc_cpnt>::iterator it = cpnts.begin(); it < cpnts.end(); ++it) {
            IntcOutput = it->State(FluidProps_, m_dot, T_out_prev, T_out_prev, T_db, P_out_prev);  // intc_cpnt::State()
            output.heat_loss += IntcOutput.heat_loss;
            output.pressure_drop += IntcOutput.pressure_drop;
            output.internal_energy += IntcOutput.internal_energy;

            T_out_prev = IntcOutput.temp_out;
            P_out_prev = P_out_prev - IntcOutput.pressure_drop;
        }
        output.temp_drop = T_in - IntcOutput.temp_out;
        output.temp_out = IntcOutput.temp_out;
        output.temp_ave = (T_in + output.temp_out) / 2;
        output.pressure_out = P_in - output.pressure_drop;
        output.pressure_ave = (P_in + output.pressure_out) / 2;
    }
    else {
        output.heat_loss = 0;
        output.temp_drop = 0;
        output.temp_out = T_in;
        output.temp_ave = T_in;
        output.pressure_drop = 0;
        output.pressure_out = P_in;
        output.pressure_ave = P_in;
        output.internal_energy = 0;
    }
    
    return output;
}


