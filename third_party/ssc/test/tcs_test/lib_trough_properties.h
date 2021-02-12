#ifndef __LIB_TROUGH_PROPERTIES__
#define __LIB_TROUGH_PROPERTIES__

#include <gtest/gtest.h>
#include <lib_util.h>


// Generic physical trough for the csp solver to be re-used
class TroughProperties : public ::testing::Test
{
public:

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

    void SetUp()
    {
        nSCA = 8;
        nHCEt = 4;
        nColt = 4;
        nHCEVar = 4;
        nLoops = 181;
        FieldConfig = 2;
        L_power_block_piping = 50.;
        include_fixed_power_block_runner = true;
        eta_pump = 0.85;
        Fluid = 21;
        fthrctrl = 2;
        accept_loc = 1;
        HDR_rough = 4.57e-5;
        theta_stow = 170.;
        theta_dep = 10.;
        Row_Distance = 15.;

        T_loop_in_des = 293.;
        T_loop_out_des = 391.;
        T_startup = 0.67*T_loop_in_des + 0.33*T_loop_out_des; //[C]
        m_dot_htfmin = 1.;
        m_dot_htfmax = 12.;
        double vals[] = { 0 };
        field_fl_props.assign(vals, 1, 1);
        T_fp = 150.;
        I_bn_des = 950.;
        V_hdr_cold_max = 3.;
        V_hdr_cold_min = 2.;
        V_hdr_hot_max = 3.;
        V_hdr_hot_min = 2.;
        V_hdr_max = std::min(V_hdr_cold_max, V_hdr_hot_max);
        V_hdr_min = std::max(V_hdr_cold_min, V_hdr_hot_min);
        Pipe_hl_coef = 0.45;
        SCA_drives_elec = 125.;
        ColTilt = 0.;
        ColAz = 0.;
        wind_stow_speed = 25.;
        accept_mode = 0;
        accept_init = false;
        solar_mult = 2.;
        mc_bal_hot_per_MW = 0.2;
        mc_bal_cold_per_MW = 0.2;
        mc_bal_sca = 4.5;

        W_aperture = { 6, 6, 6, 6 };
        A_aperture = { 656, 656, 656, 656 };
        TrackingError = { 0.988, 0.988, 0.988, 0.988 };
        GeomEffects = { 0.952, 0.952, 0.952, 0.952 };
        Rho_mirror_clean = { 0.93, 0.93, 0.93, 0.93 };
        Dirt_mirror = { 0.97, 0.97, 0.97, 0.97 };
        Error = { 1., 1., 1., 1. };
        Ave_Focal_Length = { 2.15, 2.15, 2.15, 2.15 };
        L_SCA = { 115., 115., 115., 115. };
        L_aperture = { 14.375, 14.375, 14.375, 14.375 };
        ColperSCA = { 8., 8., 8., 8. };
        Distance_SCA = { 1., 1., 1., 1. };

        double vals2[] = {
            1, 0.0327, -0.1351,
            1, 0.0327, -0.1351,
            1, 0.0327, -0.1351,
            1, 0.0327, -0.1351 };
        IAM_matrix.assign(vals2, 4, 3);

        double vals3[] = {
            0.985, 0.01, 0.005, 0.,
            1., 0., 0., 0.,
            1., 0., 0., 0.,
            1., 0., 0., 0. };
        HCE_FieldFrac.assign(vals3, 4, 4);

        double vals4[] = {
            0.076, 0.076, 0.076, 0.076,
            0.076, 0.076, 0.076, 0.076,
            0.076, 0.076, 0.076, 0.076,
            0.076, 0.076, 0.076, 0.076 };
        D_2.assign(vals4, 4, 4);

        double vals5[] = {
            0.08, 0.08, 0.08, 0.08,
            0.08, 0.08, 0.08, 0.08,
            0.08, 0.08, 0.08, 0.08,
            0.08, 0.08, 0.08, 0.08 };
        D_3.assign(vals5, 4, 4);

        double vals6[] = {
            0.115, 0.115, 0.115, 0.115,
            0.115, 0.115, 0.115, 0.115,
            0.115, 0.115, 0.115, 0.115,
            0.115, 0.115, 0.115, 0.115 };
        D_4.assign(vals6, 4, 4);

        double vals7[] = {
            0.12, 0.12, 0.12, 0.12,
            0.12, 0.12, 0.12, 0.12,
            0.12, 0.12, 0.12, 0.12,
            0.12, 0.12, 0.12, 0.12 };
        D_5.assign(vals7, 4, 4);

        double vals8[] = {
            0., 0., 0., 0.,
            0., 0., 0., 0.,
            0., 0., 0., 0.,
            0., 0., 0., 0. };
        D_p.assign(vals8, 4, 4);

        double vals9[] = {
            1., 1., 1., 1.,
            1., 1., 1., 1.,
            1., 1., 1., 1.,
            1., 1., 1., 1. };
        Flow_type.assign(vals9, 4, 4);

        double vals10[] = {
            4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5,
            4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5,
            4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5,
            4.5e-5, 4.5e-5, 4.5e-5, 4.5e-5 };
        Rough.assign(vals10, 4, 4);

        double vals11[] = {
            0.02, 0.02, 0., 0.,
            0.02, 0.02, 0., 0.,
            0.02, 0.02, 0., 0.,
            0.02, 0.02, 0., 0. };
        alpha_env.assign(vals11, 4, 4);

        double vals12[] = {
            100., 150., 200., 250., 300., 350., 400., 450., 500.,
            0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
        epsilon_3_11.assign(vals12, 2, 9);

        double vals13[] = { 0.65 };
        epsilon_3_12.assign(vals13, 1, 1);

        double vals14[] = { 0.65 };
        epsilon_3_13.assign(vals14, 1, 1);

        double vals15[] = { 0. };
        epsilon_3_14.assign(vals15, 0, 0);

        double vals16[] = {
            100., 150., 200., 250., 300., 350., 400., 450., 500.,
            0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
        epsilon_3_21.assign(vals16, 2, 9);

        double vals17[] = { 0.65 };
        epsilon_3_22.assign(vals17, 1, 1);

        double vals18[] = { 0.65 };
        epsilon_3_23.assign(vals18, 1, 1);

        double vals19[] = { 0. };
        epsilon_3_24.assign(vals19, 1, 1);

        double vals20[] = {
            100., 150., 200., 250., 300., 350., 400., 450., 500.,
            0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
        epsilon_3_31.assign(vals20, 2, 9);

        double vals21[] = { 0.65 };
        epsilon_3_32.assign(vals21, 1, 1);

        double vals22[] = { 0.65 };
        epsilon_3_33.assign(vals22, 1, 1);

        double vals23[] = { 0. };
        epsilon_3_34.assign(vals23, 1, 1);

        double vals24[] = {
            100., 150., 200., 250., 300., 350., 400., 450., 500.,
            0.064, 0.0665, 0.07, 0.0745, 0.08, 0.0865, 0.094, 0.1025, 0.112 };
        epsilon_3_41.assign(vals24, 2, 9);

        double vals25[] = { 0.65 };
        epsilon_3_42.assign(vals25, 1, 1);

        double vals26[] = { 0.65 };
        epsilon_3_43.assign(vals26, 1, 1);

        double vals27[] = { 0. };
        epsilon_3_44.assign(vals27, 1, 1);

        double vals28[] = {
            0.963, 0.963, 0.8, 0.,
            0.963, 0.963, 0.8, 0.,
            0.963, 0.963, 0.8, 0.,
            0.963, 0.963, 0.8, 0.};
        alpha_abs.assign(vals28, 4, 4);

        double vals29[] = {
            0.964, 0.964, 1., 0.,
            0.964, 0.964, 1., 0.,
            0.964, 0.964, 1., 0.,
            0.964, 0.964, 1., 0.};
        Tau_envelope.assign(vals29, 4, 4);

        double vals30[] = {
            0.86, 0.86, 1., 0.,
            0.86, 0.86, 1., 0.,
            0.86, 0.86, 1., 0.,
            0.86, 0.86, 1., 0.};
        EPSILON_4.assign(vals30, 4, 4);

        double vals31[] = {
            0.86, 0.86, 1., 0.,
            0.86, 0.86, 1., 0.,
            0.86, 0.86, 1., 0.,
            0.86, 0.86, 1., 0. };
        EPSILON_5.assign(vals31, 4, 4);

        double vals32[] = {
            1., 1., 0., 1.,
            1., 1., 0., 1.,
            1., 1., 0., 1.,
            1., 1., 0., 1.};
        GlazingIntact_dbl.assign(vals32, 4, 4);
        // convert <double> to <bool>
        int n_gl_row = (int)GlazingIntact_dbl.nrows();
        int n_gl_col = (int)GlazingIntact_dbl.ncols();
        GlazingIntact.resize(n_gl_row, n_gl_col);
        for (int i = 0; i < n_gl_row; i++) {
            for (int j = 0; j < n_gl_col; j++) {
                GlazingIntact(i, j) = (GlazingIntact_dbl(i, j) > 0);
            }
        }
        
        double vals33[] = {
            1.e-4, 750., 750., 0.,
            1.e-4, 750., 750., 0., 
            1.e-4, 750., 750., 0., 
            1.e-4, 750., 750., 0., };
        P_a.assign(vals33, 4, 4);

        double vals34[] = {
            27., 1., 1., 27.,
            27., 1., 1., 27., 
            27., 1., 1., 27., 
            27., 1., 1., 27., };
        AnnulusGas.assign(vals34, 4, 4);

        double vals35[] = {
            1., 1., 1., 1.,
            1., 1., 1., 1., 
            1., 1., 1., 1., 
            1., 1., 1., 1., };
        AbsorberMaterial.assign(vals35, 4, 4);

        double vals36[] = {
            0.935, 0.935, 0.935, 0.963,
            0.935, 0.935, 0.935, 0.963, 
            0.935, 0.935, 0.935, 0.963, 
            0.935, 0.935, 0.935, 0.963};
        Shadowing.assign(vals36, 4, 4);

        double vals37[] = {
            0.98, 0.98, 1., 0.98,
            0.98, 0.98, 1., 0.98, 
            0.98, 0.98, 1., 0.98, 
            0.98, 0.98, 1., 0.98, };
        Dirt_HCE.assign(vals37, 4, 4);

        double vals38[] = {
            190., 1270., 1500., 0.,
            190., 1270., 1500., 0., 
            190., 1270., 1500., 0., 
            190., 1270., 1500., 0.};
        Design_loss.assign(vals38, 4, 4);

        double vals39[] = {
            1., 1.,
            1., 1., 
            1., 1., 
            1., 1., 
            1., 1., 
            1., 1., 
            1., 1., 
            1., 1.};
        SCAInfoArray.assign(vals39, 8, 2);

        calc_design_pipe_vals = true;
        L_rnr_pb = 25.;
        N_max_hdr_diams = 10.;
        L_rnr_per_xpan = 70.;
        L_xpan_hdr = 20.;
        L_xpan_rnr = 20.;
        Min_rnr_xpans = 1.;
        northsouth_field_sep = 20.;
        N_hdr_per_xpan = 2.;

        double vals40[] = {
            0.9, 0., 0.19, 0., 0.9, -1., -1., -1., -1., -1., -1.,
            0., 0.6, 0.05, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.42, 0., 0.15,
            0.05, 0., 0.42, 0., 0.6, 0., 0.6, 0., 0.15, 0.6, 0.,
            0.9, 0., 0.19, 0., 0.9, -1., -1., -1., -1., -1., -1.};
        K_cpnt.assign(vals40, 11, 11);

        double vals41[] = {
            0.085, 0.0635, 0.085, 0.0635, 0.085, -1., -1., -1., -1., -1., -1.,
            0.085, 0.085, 0.085, 0.0635, 0.0635, 0.0635,0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635, 0.085,
            0.085, 0.0635, 0.0635, 0.0635, 0.0635, 0.0635,0.0635, 0.0635, 0.085, 0.085, 0.085,
            0.085, 0.0635, 0.085, 0.0635, 0.085, -1., -1., -1., -1., -1., -1.};
        D_cpnt.assign(vals41, 11, 11);

        double vals42[] = {
            0., 0., 0., 0., 0., -1., -1., -1., -1., -1., -1.,
            0., 0., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 1., 0.,
            0., 1., 0., 1., 0., 0., 0., 1., 0., 0., 0.,
            0., 0., 0., 0., 0., -1., -1., -1., -1., -1., -1.};
        L_cpnt.assign(vals42, 11, 11);

        double vals43[] = {
            0., 1., 0., 1., 0., -1., -1., -1., -1., -1., -1.,
            1., 0., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 2., 0.,
            0., 2., 0., 2., 0., 1., 0., 2., 0., 0., 1.,
            0., 1., 0., 1., 0., -1., -1., -1., -1., -1., -1.};
        Type_cpnt.assign(vals43, 11, 11);

        custom_sf_pipe_sizes = false;

        double vals44[] = { -1. };
        sf_rnr_diams.assign(vals44, 1, 1);

        double vals45[] = { -1 };
        sf_rnr_wallthicks.assign(vals45, 1, 1);

        double vals46[] = { -1 };
        sf_rnr_lengths.assign(vals46, 1, 1);

        double vals47[] = { -1 };
        sf_hdr_diams.assign(vals47, 1, 1);

        double vals48[] = { -1 };
        sf_hdr_wallthicks.assign(vals48, 1, 1);

        double vals49[] = { -1 };
        sf_hdr_lengths.assign(vals49, 1, 1);
    }

    //nothing to do
    void TearDown(){}
};

#endif