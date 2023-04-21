#ifndef __LIB_TROUGH_TEST_H__
#define __LIB_TROUGH_TEST_H__

#include <gtest/gtest.h>
#include "csp_solver_trough_collector_receiver.h"

// Aliases (e.g., typedef)
using Trough = C_csp_trough_collector_receiver;
using Location = C_csp_collector_receiver::S_csp_cr_init_inputs;
using TroughSolvedParams = C_csp_collector_receiver::S_csp_cr_solved_params;
using TimeAndWeather = C_csp_weatherreader::S_outputs;
using FluidInletState = C_csp_solver_htf_1state;
using TimestepAndTou = C_csp_solver_sim_info;
using TroughOutputs = C_csp_collector_receiver::S_csp_cr_out_solver;

namespace csp_trough
{
    struct TroughSpecifications;    // forward declaration
    struct TroughState;

    const double kErrorToleranceLo = 0.001;    // 0.1%
    const double kErrorToleranceHi = 0.01;     // 1.0%

    class TroughFactory {
    public:
        TroughFactory() {};

        virtual std::unique_ptr<Trough> MakeTrough(Location location) const = 0;
        std::unique_ptr<Trough> MakeTrough(TroughSpecifications* trough_specifications,
            Location location) const;
        virtual std::unique_ptr<TroughSpecifications> MakeSpecifications() const = 0;
        virtual std::unique_ptr<TroughState> MakeTroughState() const = 0;
        virtual std::unique_ptr<TimeAndWeather> MakeTimeLocationWeather(Location location) const = 0;
        virtual TimestepAndTou MakeTimestepAndTou() const = 0;
        virtual FluidInletState MakeInletState() const = 0;
        virtual double MakeDefocus() const = 0;
        virtual Location MakeLocation() const = 0;

        static void SetTroughState(Trough* trough, TroughState* trough_state);
    };

    class DefaultTroughFactory : public TroughFactory {
    public:
        DefaultTroughFactory() {};

        virtual std::unique_ptr<Trough> MakeTrough(Location location) const;
        virtual std::unique_ptr<TroughSpecifications> MakeSpecifications() const;
        virtual std::unique_ptr<TroughState> MakeTroughState() const;
        virtual std::unique_ptr<TimeAndWeather> MakeTimeLocationWeather(Location location) const;
        virtual TimestepAndTou MakeTimestepAndTou() const;
        virtual FluidInletState MakeInletState() const;
        virtual double MakeDefocus() const;
        virtual Location MakeLocation() const;
    };

    struct TroughSpecifications
    {
        int nSCA;                                                 //[-] Number of SCA's in a loop
        int nHCEt;                                                //[-] Number of HCE types
        int nColt;                                                //[-] Number of collector types
        int nHCEVar;                                              //[-] Number of HCE variants per t
        int nLoops;                                               //[-] Number of loops in the field
        int FieldConfig;                                          //[-] Number of subfield headers
        double L_power_block_piping;                              //[m] Length of piping (full mass flow) through power block (if applicable)
        bool include_fixed_power_block_runner;	                  //[-] Should model consider piping through power block (is_model_power_block_piping)?
        double eta_pump;                                          //[-] HTF pump efficiency
        int Fluid;                                                //[-] Field HTF fluid number
        //int fthrok;                                               //[-] Flag to allow partial defocusing of the collectors
        int fthrctrl;                                             //[-] Defocusing strategy; hardcode2 for now
        int accept_loc;                                           //[-] In acceptance testing mode - temperature sensor location (1=hx,2=loop)
        double HDR_rough;                                         //[m] Header pipe roughness
        double theta_stow;                                        //[deg] stow angle
        double theta_dep;                                         //[deg] deploy angle
        double Row_Distance;                                      //[m] Spacing between rows (centerline to centerline)

        double T_loop_in_des;                                     //[C] Design loop inlet temperature, converted to K in init
        double T_loop_out_des;                                    //[C] Target loop outlet temperature, converted to K in init
        double T_startup;                                         //[C] The required temperature (converted to K in init) of the system before the power block can be switched on
        double m_dot_htfmin;                                      //[kg/s] Minimum loop HTF flow rate
        double m_dot_htfmax;                                      //[kg/s] Maximum loop HTF flow rate
        util::matrix_t<double> field_fl_props;                    //[-] User-defined field HTF properties
        double T_fp;                                              //[C] Freeze protection temperature (heat trace activation temperature), convert to K in init
        double I_bn_des;                                          //[W/m^2] Solar irradiation at design
        double V_hdr_cold_max;                                    //[m/s] Maximum HTF velocity in the cold header at design
        double V_hdr_cold_min;                                    //[m/s] Minimum HTF velocity in the cold header at design
        double V_hdr_hot_max;                                     //[m/s] Maximum HTF velocity in the hot header at design
        double V_hdr_hot_min;                                     //[m/s] Minimum HTF velocity in the hot header at design
        double V_hdr_max;                                         //[m/s] Maximum HTF velocity in the header at design, for backwards compatibility, marked for removal
        double V_hdr_min;                                         //[m/s] Minimum HTF velocity in the header at design, for backwards compatibility, marked for removal
        double Pipe_hl_coef;                                      //[W/m2-K] Loss coefficient from the header, runner pipe, and non-HCE piping
        double SCA_drives_elec;                                   //[W/SCA] Tracking power, in Watts per SCA drive
        double ColTilt;                                           //[deg] Collector tilt angle (0 is horizontal, 90deg is vertical) ("tilt")
        double ColAz;                                             //[deg] Collector azimuth angle ("azimuth")
        double wind_stow_speed;                                   //[m/s] Wind speed at and above which the collectors will be stowed
        int accept_mode;                                          //[-] Acceptance testing mode? (1=yes, 0=no)
        bool accept_init;                                         //[-] In acceptance testing mode - require steady-state startup
        double solar_mult;                                        //[-] Solar Multiple
        double mc_bal_hot_per_MW;                                 //[kWht/K-MWt] The heat capacity of the balance of plant on the hot side ("mc_bal_hot")
        double mc_bal_cold_per_MW;                                //[kWht/K-MWt] The heat capacity of the balance of plant on the cold side ("mc_bal_cold")
        double mc_bal_sca;                                        //[Wht/K-m] Non-HTF heat capacity associated with each SCA - per meter basis

        std::vector<double> W_aperture;                           //[m] The collector aperture width (Total structural area.. used for shadowing)
        std::vector<double> A_aperture;                           //[m^2] Reflective aperture area of the collector
        std::vector<double> TrackingError;                        //[-] Tracking error derate
        std::vector<double> GeomEffects;                          //[-] Geometry effects derate
        std::vector<double> Rho_mirror_clean;                     //[-] Clean mirror reflectivity
        std::vector<double> Dirt_mirror;                          //[-] Dirt on mirror derate
        std::vector<double> Error;                                //[-] General optical error derate
        std::vector<double> Ave_Focal_Length;                     //[m] The average focal length of the collector 
        std::vector<double> L_SCA;                                //[m] The length of the SCA 
        std::vector<double> L_aperture;                           //[m] The length of a single mirror/HCE unit
        std::vector<double> ColperSCA;                            //[-] The number of individual collector sections in an SCA
        std::vector<double> Distance_SCA;                         //[m] Piping distance between SCA's in the field

        util::matrix_t<double> IAM_matrix;                        //[-] IAM coefficients, matrix for 4 collectors                                                                          
        util::matrix_t<double> HCE_FieldFrac;                     //[-] Fraction of the field occupied by this HCE type
        util::matrix_t<double> D_2;                               //[m] Inner absorber tube diameter
        util::matrix_t<double> D_3;                               //[m] Outer absorber tube diameter
        util::matrix_t<double> D_4;                               //[m] Inner glass envelope diameter
        util::matrix_t<double> D_5;                               //[m] Outer glass envelope diameter
        util::matrix_t<double> D_p;                               //[m] Diameter of the absorber flow plug (optional)
        util::matrix_t<double> Flow_type;                         //[-] Flow type through the absorber
        util::matrix_t<double> Rough;                             //[m] Roughness of the internal surface
        util::matrix_t<double> alpha_env;                         //[-] Envelope absorptance

        util::matrix_t<double> epsilon_3_11;                      //[-] Absorber emittance for receiver type 1 variation 1
        util::matrix_t<double> epsilon_3_12;                      //[-] Absorber emittance for receiver type 1 variation 2
        util::matrix_t<double> epsilon_3_13;                      //[-] Absorber emittance for receiver type 1 variation 3
        util::matrix_t<double> epsilon_3_14;                      //[-] Absorber emittance for receiver type 1 variation 4
        util::matrix_t<double> epsilon_3_21;                      //[-] Absorber emittance for receiver type 2 variation 1
        util::matrix_t<double> epsilon_3_22;                      //[-] Absorber emittance for receiver type 2 variation 2
        util::matrix_t<double> epsilon_3_23;                      //[-] Absorber emittance for receiver type 2 variation 3
        util::matrix_t<double> epsilon_3_24;                      //[-] Absorber emittance for receiver type 2 variation 4
        util::matrix_t<double> epsilon_3_31;                      //[-] Absorber emittance for receiver type 3 variation 1
        util::matrix_t<double> epsilon_3_32;                      //[-] Absorber emittance for receiver type 3 variation 2
        util::matrix_t<double> epsilon_3_33;                      //[-] Absorber emittance for receiver type 3 variation 3
        util::matrix_t<double> epsilon_3_34;                      //[-] Absorber emittance for receiver type 3 variation 4
        util::matrix_t<double> epsilon_3_41;                      //[-] Absorber emittance for receiver type 4 variation 1
        util::matrix_t<double> epsilon_3_42;                      //[-] Absorber emittance for receiver type 4 variation 2
        util::matrix_t<double> epsilon_3_43;                      //[-] Absorber emittance for receiver type 4 variation 3
        util::matrix_t<double> epsilon_3_44;                      //[-] Absorber emittance for receiver type 4 variation 4

        util::matrix_t<double> alpha_abs;                         //[-] Absorber absorptance
        util::matrix_t<double> Tau_envelope;                      //[-] Envelope transmittance
        util::matrix_t<double> EPSILON_4;                         //[-] Inner glass envelope emissivities
        util::matrix_t<double> EPSILON_5;                         //[-] Outer glass envelope emissivities
        util::matrix_t<double> GlazingIntact_dbl;                 //[-] Glazing intact (broken glass) flag {1=true, else=false}, as double
        util::matrix_t<bool> GlazingIntact;                       //[-] Glazing intact (broken glass) flag {1=true, else=false}
        util::matrix_t<double> P_a;                               //[torr] Annulus gas pressure
        util::matrix_t<double> AnnulusGas;                        //[-] Annulus gas type (1=air, 26=Ar, 27=H2)
        util::matrix_t<double> AbsorberMaterial;                  //[-] Absorber material type
        util::matrix_t<double> Shadowing;                         //[-] Receiver bellows shadowing loss factor
        util::matrix_t<double> Dirt_HCE;                          //[-] Loss due to dirt on the receiver envelope
        util::matrix_t<double> Design_loss;                       //[-] Receiver heat loss at design
        util::matrix_t<double> SCAInfoArray;                      //[-] Receiver (,1) and collector (,2) type for each assembly in loop

        bool calc_design_pipe_vals;                               //[-] Should the HTF state be calculated at design conditions
        double L_rnr_pb;                                          //[m] Length of hot or cold runner pipe around the power block
        double N_max_hdr_diams;                                   //[-] Maximum number of allowed diameters in each of the hot and cold headers
        double L_rnr_per_xpan;                                    //[m] Threshold length of straight runner pipe without an expansion loop
        double L_xpan_hdr;                                        //[m] Combined length in meters of the two perpendicular segments of a header expansion loop
        double L_xpan_rnr;                                        //[m] Combined length in meters of the two perpendicular segments of a runner expansion loop
        double Min_rnr_xpans;                                     //[-] Minimum number of expansion loops per single-diameter runner section
        double northsouth_field_sep;                              //[m] Shortest north/south distance between SCAs in different subfields
        double N_hdr_per_xpan;                                    //[-] Number of collector loops per header expansion loops. 1expansion loop between every collector loop
        util::matrix_t<double> K_cpnt;                            //[-] Minor loss coefficients of the components in each loop interconnect
        util::matrix_t<double> D_cpnt;                            //[m] Inner diameters of the components in each loop interconnect
        util::matrix_t<double> L_cpnt;                            //[m] Lengths of the components in each loop interconnect
        util::matrix_t<double> Type_cpnt;                         //[-] Type of component in each loop interconnect [0=fitting | 1=pipe | 2=flex_hose]
        bool custom_sf_pipe_sizes;                                //[-] Should the field pipe diameters, wall thickness and lengths be imported instead of calculated
        util::matrix_t<double> sf_rnr_diams;                      //[m] Imported runner diameters, used if custom_sf_pipe_sizes is true
        util::matrix_t<double> sf_rnr_wallthicks;                 //[m] Imported runner wall thicknesses, used if custom_sf_pipe_sizes is true
        util::matrix_t<double> sf_rnr_lengths;                    //[m] Imported runner lengths, used if custom_sf_pipe_sizes is true
        util::matrix_t<double> sf_hdr_diams;                      //[m] Imported header diameters, used if custom_sf_pipe_sizes is true
        util::matrix_t<double> sf_hdr_wallthicks;                 //[m] Imported header wall thicknesses, used if custom_sf_pipe_sizes is true
        util::matrix_t<double> sf_hdr_lengths;                    //[m] Imported header lengths, used if custom_sf_pipe_sizes is true
    };

    struct TroughState
    {
        double T_in_loop_prev;                  // corresponds to m_T_sys_c_t_end_converged
        double T_out_loop_prev;                 // corresponds to m_T_sys_h_t_end_converged
        std::vector<double> T_out_SCAs_prev;    // corresponds to m_T_htf_out_t_end_converged;
    };
}
#endif
