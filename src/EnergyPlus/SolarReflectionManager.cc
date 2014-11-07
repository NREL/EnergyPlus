// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <SolarReflectionManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataVectorTypes.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <ScheduleManager.hh>
#include <Vectors.hh>

namespace EnergyPlus {

namespace SolarReflectionManager {

	// MODULE INFORMATION
	//       AUTHOR         Fred Winkelmann
	//       DATE WRITTEN   September 2003
	//       MODIFIED       May 2004, FCW: modify calculation of receiving point location on a
	//                        receiving surface so can handle surface of any number of vertices
	//                        (previously restricted to 3- or 4-sided surfaces).
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Manages the calculation of factors for solar reflected from obstructions and ground.

	// METHODOLOGY EMPLOYED:
	// REFERENCES: na

	// OTHER NOTES: na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace ScheduleManager;
	using namespace DataEnvironment;

	using namespace DataVectorTypes;

	// Data
	// MODULE PARAMETER DEFINITIONS:na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int TotSolReflRecSurf( 0 ); // Total number of exterior surfaces that can receive reflected solar
	int TotPhiReflRays( 0 ); // Number of rays in altitude angle (-90 to 90 deg) for diffuse refl calc
	int TotThetaReflRays( 0 ); // Number of rays in azimuth angle (0 to 180 deg) for diffuse refl calc

	// SUBROUTINE SPECIFICATIONS FOR MODULE ExteriorSolarReflectionManager

	// Object Data
	FArray1D< SolReflRecSurfData > SolReflRecSurf;

	// MODULE SUBROUTINES:

	// Functions

	void
	InitSolReflRecSurf()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the derived type SolReflRecSurf, which contains information
		// needed to calculate factors for solar reflection from obstructions and ground.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		int RecSurfNum; // Receiving surface number
		int loop; // DO loop indices
		int loop1; // DO loop indices
		int loopA; // DO loop indices
		int loopB; // DO loop indices
		int ObsSurfNum; // Surface number of an obstruction
		bool ObsBehindRec; // True if an obstruction is entirely behind a receiving surface
		bool ObsHasView; // True if view between receiving surface and heat trans surf obstruction
		FArray1D< Real64 > RecVec( 3 ); // First vertex of a receiving surface (m)
		FArray1D< Real64 > ObsVec( 3 ); // A vertex of a candidate obstructing surface (m)
		FArray1D< Real64 > VecAB( 3 ); // Vector from receiving surface vertex to obstruction surface vertex (m)
		FArray1D< Real64 > HitPt( 3 ); // Hit point (m)
		Real64 DotProd; // Dot product of vectors (m2)
		int RecPtNum; // Receiving point number
		//unused  REAL(r64)         :: SumX                 ! Sum of X (or Y or Z) coordinate values of a surface
		//unused  REAL(r64)         :: SumY                 ! Sum of X (or Y or Z) coordinate values of a surface
		//unused  REAL(r64)         :: SumZ                 ! Sum of X (or Y or Z) coordinate values of a surface
		Real64 PhiSurf; // Altitude of normal to receiving surface (radians)
		Real64 ThetaSurf; // Azimuth of normal to receiving surface (radians)
		Real64 PhiMin; // Minimum and maximum values of ray altitude angle (radians)
		Real64 PhiMax; // Minimum and maximum values of ray altitude angle (radians)
		Real64 ThetaMin; // Minimum and maximum values of ray azimuth angle (radians)
		Real64 ThetaMax; // Minimum and maximum values of ray azimuth angle (radians)
		Real64 Phi; // Ray altitude angle, increment, sine, and cosine
		Real64 DPhi; // Ray altitude angle, increment, sine, and cosine
		Real64 SPhi; // Ray altitude angle, increment, sine, and cosine
		Real64 CPhi; // Ray altitude angle, increment, sine, and cosine
		Real64 Theta; // Ray azimuth angle and increment
		Real64 DTheta; // Ray azimuth angle and increment
		int IPhi; // Ray altitude angle and azimuth angle indices
		int ITheta; // Ray altitude angle and azimuth angle indices
		//unused  REAL(r64)         :: APhi                 ! Intermediate variable
		int RayNum; // Ray number
		FArray1D< Real64 > URay( 3 ); // Unit vector along ray pointing away from receiving surface
		Real64 CosIncAngRay; // Cosine of angle of incidence of ray on receiving surface
		Real64 dOmega; // Solid angle associated with a ray
		int IHit; // = 1 if obstruction is hit, 0 otherwise
		int TotObstructionsHit; // Number of obstructions hit by a ray
		Real64 HitDistance; // Distance from receiving point to hit point for a ray (m)
		int NearestHitSurfNum; // Surface number of nearest obstruction hit by a ray
		FArray1D< Real64 > NearestHitPt( 3 ); // Nearest hit pit for a ray (m)
		Real64 NearestHitDistance; // Distance from receiving point to nearest hit point for a ray (m)
		int ObsSurfNumToSkip; // Surface number of obstruction to be ignored
		FArray1D< Real64 > RecPt( 3 ); // Receiving point (m)
		FArray1D< Real64 > RayVec( 3 ); // Unit vector along ray
		FArray1D< Real64 > Vec1( 3 ); // Vectors between hit surface vertices (m)
		FArray1D< Real64 > Vec2( 3 ); // Vectors between hit surface vertices (m)
		FArray1D< Real64 > VNorm( 3 ); // For a hit surface, unit normal vector pointing into the hemisphere
		// containing the receiving point
		int ObsConstrNum; // Construction number of obstruction; = 0 if a shading surface
		Real64 Alfa; // Direction angles for ray heading towards the ground (radians)
		Real64 Beta;
		Real64 HorDis; // Distance between ground hit point and proj'n of receiving pt onto ground (m)
		FArray1D< Real64 > GroundHitPt( 3 ); // Coordinates of ground hit point
		//unused  REAL(r64)         :: ArgASin
		Real64 ACosTanTan;
		int J; // DO loop indices
		int K; // DO loop indices
		int L; // DO loop indices
		int NumRecPts; // Number of surface receiving points for reflected solar radiation
		Real64 VertexWt; // Vertex weighting factor for calculating receiving points

		// FLOW:

		// Find number of surfaces that are sun-exposed exterior building heat transfer surfaces.
		// These are candidates for receiving solar reflected from obstructions and ground.
		// CR 7640.  12/3/2008 BG simplified logic to allow for Other Side Conditions Modeled boundary condition.
		//           and solar collectors on shading surfaces that need this.

		// shading surfaces have ExtSolar = False, so they are not included in TotSolReflRecSurf
		TotSolReflRecSurf = 0;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).ExtSolar ) {
				++TotSolReflRecSurf;
			}
		}

		// TH 3/29/2010. ShadowSurfPossibleReflector is not used!
		// Set flag that determines whether a surface can be an exterior reflector
		//DO SurfNum = 1,TotSurfaces
		//  Surface(SurfNum)%ShadowSurfPossibleReflector = .FALSE.
		// Exclude non-exterior heat transfer surfaces (but not OtherSideCondModeledExt = -4 CR7640)
		//  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond > 0 ) CYCLE
		//  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == Ground) CYCLE
		//  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt) CYCLE
		//  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) CYCLE

		// Exclude daylighting shelves. A separate solar reflection calculation is done for these.
		//  IF(Surface(SurfNum)%Shelf > 0) CYCLE

		// Exclude duplicate shading surfaces
		// TH 3/24/2010. Why? a mirror shading surface can reflect solar (either beam or diffuse)
		//  can use a flag like Surface(SurfNum)%Mirrored (True or False) to avoid string comparison
		//   and to allow surface names starting with 'Mir'
		//IF(Surface(SurfNum)%Name(1:3) == 'Mir') CYCLE
		//  IF(Surface(SurfNum)%MirroredSurf) CYCLE

		//  Surface(SurfNum)%ShadowSurfPossibleReflector = .TRUE.
		//END DO

		if ( TotSolReflRecSurf == 0 ) {
			ShowWarningError( "Calculation of solar reflected from obstructions has been requested but there" );
			ShowContinueError( "are no building surfaces that can receive reflected solar. Calculation will not be done." );
			CalcSolRefl = false;
			return;
		}

		// Should this be moved up front?
		if ( IgnoreSolarRadiation ) {
			TotSolReflRecSurf = 0;
			CalcSolRefl = false;
			return;
		}

		SolReflRecSurf.allocate( TotSolReflRecSurf );

		ReflFacBmToDiffSolObs.allocate( TotSurfaces, 24 );
		ReflFacBmToDiffSolObs = 0.0;
		ReflFacBmToDiffSolGnd.allocate( TotSurfaces, 24 );
		ReflFacBmToDiffSolGnd = 0.0;
		ReflFacBmToBmSolObs.allocate( TotSurfaces, 24 );
		ReflFacBmToBmSolObs = 0.0;
		ReflFacSkySolObs.allocate( TotSurfaces );
		ReflFacSkySolObs = 0.0;
		ReflFacSkySolGnd.allocate( TotSurfaces );
		ReflFacSkySolGnd = 0.0;
		CosIncAveBmToBmSolObs.allocate( TotSurfaces, 24 );
		CosIncAveBmToBmSolObs = 0.0;

		// Only surfaces with sun exposure can receive solar reflection from ground or onstructions
		//  Shading surfaces are always not exposed to solar (ExtSolar = False)
		RecSurfNum = 0;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			Surface( SurfNum ).ShadowSurfRecSurfNum = 0;
			if ( Surface( SurfNum ).ExtSolar ) {
				++RecSurfNum;
				SolReflRecSurf( RecSurfNum ).SurfNum = SurfNum;
				SolReflRecSurf( RecSurfNum ).SurfName = Surface( SurfNum ).Name;
				Surface( SurfNum ).ShadowSurfRecSurfNum = RecSurfNum;

				// Warning if any receiving surface vertex is below ground level, taken to be at Z = 0 in absolute coords
				for ( loop = 1; loop <= Surface( SurfNum ).Sides; ++loop ) {
					if ( Surface( SurfNum ).Vertex( loop ).z < GroundLevelZ ) {
						ShowWarningError( "Calculation of reflected solar onto surface=" + Surface( SurfNum ).Name + " may be inaccurate" );
						ShowContinueError( "because it has one or more vertices below ground level." );
						break;
					}
				}
			}
		}

		// Get MaxRecPts for allocating SolReflRecSurf arrays that depend on number of receiving points
		MaxRecPts = 1;
		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SolReflRecSurf( RecSurfNum ).NumRecPts = Surface( SolReflRecSurf( RecSurfNum ).SurfNum ).Sides;
			if ( SolReflRecSurf( RecSurfNum ).NumRecPts > MaxRecPts ) MaxRecPts = SolReflRecSurf( RecSurfNum ).NumRecPts;
		}

		MaxReflRays = AltAngStepsForSolReflCalc * AzimAngStepsForSolReflCalc;
		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SolReflRecSurf( RecSurfNum ).NormVec = 0.0;
			SolReflRecSurf( RecSurfNum ).RecPt.allocate( 3, MaxRecPts );
			SolReflRecSurf( RecSurfNum ).RecPt = 0.0;
			SolReflRecSurf( RecSurfNum ).RayVec.allocate( 3, MaxReflRays );
			SolReflRecSurf( RecSurfNum ).RayVec = 0.0;
			SolReflRecSurf( RecSurfNum ).CosIncAngRay.allocate( MaxReflRays );
			SolReflRecSurf( RecSurfNum ).CosIncAngRay = 0.0;
			SolReflRecSurf( RecSurfNum ).dOmegaRay.allocate( MaxReflRays );
			SolReflRecSurf( RecSurfNum ).dOmegaRay = 0.0;
			SolReflRecSurf( RecSurfNum ).HitPt.allocate( 3, MaxRecPts, MaxReflRays );
			SolReflRecSurf( RecSurfNum ).HitPt = 0.0;
			SolReflRecSurf( RecSurfNum ).HitPtSurfNum.allocate( MaxRecPts, MaxReflRays );
			SolReflRecSurf( RecSurfNum ).HitPtSurfNum = 0;
			SolReflRecSurf( RecSurfNum ).HitPtSolRefl.allocate( MaxRecPts, MaxReflRays );
			SolReflRecSurf( RecSurfNum ).HitPtSolRefl = 0.0;
			SolReflRecSurf( RecSurfNum ).RecPtHitPtDis.allocate( MaxRecPts, MaxReflRays );
			SolReflRecSurf( RecSurfNum ).RecPtHitPtDis = 0.0;
			SolReflRecSurf( RecSurfNum ).HitPtNormVec.allocate( 3, MaxRecPts, MaxReflRays );
			SolReflRecSurf( RecSurfNum ).HitPtNormVec = 0.0;
			SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums.allocate( TotSurfaces );
			SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums = 0;
		}

		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SurfNum = SolReflRecSurf( RecSurfNum ).SurfNum;
			// Outward norm to receiving surface
			SolReflRecSurf( RecSurfNum ).NormVec = Surface( SurfNum ).OutNormVec;
			RecVec = Surface( SurfNum ).Vertex( 1 );
			// Loop over all surfaces and find those that can be obstructing surfaces for this receiving surf
			SolReflRecSurf( RecSurfNum ).NumPossibleObs = 0;
			for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
				// Exclude the receiving surface itself and its base surface (if it has one)
				if ( ObsSurfNum == SurfNum || ObsSurfNum == Surface( SurfNum ).BaseSurf ) continue;
				// Exclude non-exterior heat transfer surfaces
				if ( Surface( ObsSurfNum ).HeatTransSurf && Surface( ObsSurfNum ).ExtBoundCond != 0 ) continue;
				// Exclude duplicate shading surfaces
				//IF(Surface(ObsSurfNum)%Name(1:3) == 'Mir') CYCLE
				//TH2 CR8959
				//IF(Surface(ObsSurfNum)%MirroredSurf) CYCLE

				// Exclude surfaces that are entirely behind the receiving surface.This is true if dot products of the
				// rec. surface outward normal and vector from first vertex of rec. surface and each vertex of
				// obstructing surface are all negative.
				ObsBehindRec = true;
				for ( loop = 1; loop <= Surface( ObsSurfNum ).Sides; ++loop ) {
					ObsVec = Surface( ObsSurfNum ).Vertex( loop );
					DotProd = dot( SolReflRecSurf( RecSurfNum ).NormVec, ObsVec - RecVec );
					//CR8251      IF(DotProd > 0.01d0) THEN  ! This obstructing-surface vertex is not behind receiving surface
					if ( DotProd > 1.e-6 ) { // This obstructing-surface vertex is not behind receiving surface
						ObsBehindRec = false;
						break;
					}
				}
				if ( ObsBehindRec ) continue;

				// Exclude heat transfer surfaces that have no view with the receiving surface.
				// There is view if: for at least one vector VecAB from a receiving surface vertex to
				// a vertex of a potential obstructing surface that satisfies VecAB.nA > 0.0 and VecAB.nB < 0.0,
				// where nA and nB are the outward normal to the receiving and obstructing surface, resp.
				if ( Surface( ObsSurfNum ).HeatTransSurf ) {
					ObsHasView = false;
					for ( loopA = 1; loopA <= Surface( SurfNum ).Sides; ++loopA ) {
						for ( loopB = 1; loopB <= Surface( ObsSurfNum ).Sides; ++loopB ) {
							VecAB = Surface( ObsSurfNum ).Vertex( loopB ) - Surface( SurfNum ).Vertex( loopA );
							if ( dot( VecAB, SolReflRecSurf( RecSurfNum ).NormVec ) > 0.0 && dot( VecAB, Surface( ObsSurfNum ).OutNormVec ) < 0.0 ) {
								ObsHasView = true;
								break;
							}
						}
						if ( ObsHasView ) break;
					}
					if ( ! ObsHasView ) continue;
				}

				// This is a possible obstructing surface for this receiving surface
				++SolReflRecSurf( RecSurfNum ).NumPossibleObs;
				SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums( SolReflRecSurf( RecSurfNum ).NumPossibleObs ) = ObsSurfNum;
			}

			// Get coordinates of receiving points on this receiving surface. The number of receiving points
			// is equal to the number of surface vertices (3 or higher).

			NumRecPts = SolReflRecSurf( RecSurfNum ).NumRecPts;
			for ( J = 1; J <= NumRecPts; ++J ) {
				for ( L = 1; L <= 3; ++L ) {
					SolReflRecSurf( RecSurfNum ).RecPt( L, J ) = 0.0;
				}
				for ( K = 1; K <= NumRecPts; ++K ) {
					if ( NumRecPts == 3 ) { // Receiving surface is a triangle
						VertexWt = 0.2;
						if ( K == J ) VertexWt = 0.6;
					} else { // Receiving surface has 4 or more vertices
						VertexWt = 1.0 / ( 2.0 * double( NumRecPts ) );
						if ( K == J ) VertexWt = ( double( NumRecPts ) + 1.0 ) / ( 2.0 * double( NumRecPts ) );
					}
					SolReflRecSurf( RecSurfNum ).RecPt( 1, J ) += VertexWt * Surface( SurfNum ).Vertex( K ).x;
					SolReflRecSurf( RecSurfNum ).RecPt( 2, J ) += VertexWt * Surface( SurfNum ).Vertex( K ).y;
					SolReflRecSurf( RecSurfNum ).RecPt( 3, J ) += VertexWt * Surface( SurfNum ).Vertex( K ).z;
				}
			}

			// Create rays going outward from receiving surface. The same rays will be used at each receiving point.
			// The rays are used in calculating diffusely reflected solar incident on receiving surface.

			// Divide hemisphere around receiving surface into elements of altitude Phi and
			// azimuth Theta and create ray unit vector at each Phi,Theta pair in front of the surface.
			// Phi = 0 at the horizon; Phi = Pi/2 at the zenith

			PhiSurf = std::asin( SolReflRecSurf( RecSurfNum ).NormVec( 3 ) );
			if ( std::abs( SolReflRecSurf( RecSurfNum ).NormVec( 1 ) ) > 1.0e-5 || std::abs( SolReflRecSurf( RecSurfNum ).NormVec( 2 ) ) > 1.0e-5 ) {
				ThetaSurf = std::atan2( SolReflRecSurf( RecSurfNum ).NormVec( 2 ), SolReflRecSurf( RecSurfNum ).NormVec( 1 ) );
			} else {
				ThetaSurf = 0.0;
			}
			SolReflRecSurf( RecSurfNum ).PhiNormVec = PhiSurf;
			SolReflRecSurf( RecSurfNum ).ThetaNormVec = ThetaSurf;
			PhiMin = max( -PiOvr2, PhiSurf - PiOvr2 );
			PhiMax = min( PiOvr2, PhiSurf + PiOvr2 );
			DPhi = ( PhiMax - PhiMin ) / AltAngStepsForSolReflCalc;
			RayNum = 0;

			// Altitude loop
			for ( IPhi = 1; IPhi <= AltAngStepsForSolReflCalc; ++IPhi ) {
				Phi = PhiMin + ( IPhi - 0.5 ) * DPhi;
				SPhi = std::sin( Phi );
				CPhi = std::cos( Phi );
				// Third component of ray unit vector in (Theta,Phi) direction
				URay( 3 ) = SPhi;

				if ( PhiSurf >= 0.0 ) {
					if ( Phi >= PiOvr2 - PhiSurf ) {
						ThetaMin = -Pi;
						ThetaMax = Pi;
					} else {
						ACosTanTan = std::acos( -std::tan( Phi ) * std::tan( PhiSurf ) );
						ThetaMin = ThetaSurf - std::abs( ACosTanTan );
						ThetaMax = ThetaSurf + std::abs( ACosTanTan );
					}

				} else { // PhiSurf < 0.0
					if ( Phi <= -PhiSurf - PiOvr2 ) {
						ThetaMin = -Pi;
						ThetaMax = Pi;
					} else {
						ACosTanTan = std::acos( -std::tan( Phi ) * std::tan( PhiSurf ) );
						ThetaMin = ThetaSurf - std::abs( ACosTanTan );
						ThetaMax = ThetaSurf + std::abs( ACosTanTan );
					}
				}

				DTheta = ( ThetaMax - ThetaMin ) / AzimAngStepsForSolReflCalc;
				dOmega = CPhi * DTheta * DPhi;

				// Azimuth loop
				for ( ITheta = 1; ITheta <= AzimAngStepsForSolReflCalc; ++ITheta ) {
					Theta = ThetaMin + ( ITheta - 0.5 ) * DTheta;
					URay( 1 ) = CPhi * std::cos( Theta );
					URay( 2 ) = CPhi * std::sin( Theta );
					// Cosine of angle of incidence of ray on receiving surface
					CosIncAngRay = SPhi * std::sin( PhiSurf ) + CPhi * std::cos( PhiSurf ) * std::cos( Theta - ThetaSurf );
					if ( CosIncAngRay < 0.0 ) continue; // Ray is behind receiving surface (although there shouldn't be any)
					++RayNum;
					SolReflRecSurf( RecSurfNum ).RayVec( {1,3}, RayNum ) = URay;
					SolReflRecSurf( RecSurfNum ).CosIncAngRay( RayNum ) = CosIncAngRay;
					SolReflRecSurf( RecSurfNum ).dOmegaRay( RayNum ) = dOmega;
				} // End of azimuth loop

			} // End of altitude loop
			SolReflRecSurf( RecSurfNum ).NumReflRays = RayNum;

		} // End of loop over receiving surfaces

		// Loop again over receiving surfaces and, for each ray, get hit point and info associated with that point
		// (hit point = point that ray intersects nearest obstruction, or, if ray is downgoing and hits no
		// obstructions, point that ray intersects ground plane).

		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SurfNum = SolReflRecSurf( RecSurfNum ).SurfNum;
			for ( RecPtNum = 1; RecPtNum <= SolReflRecSurf( RecSurfNum ).NumRecPts; ++RecPtNum ) {
				RecPt = SolReflRecSurf( RecSurfNum ).RecPt( {1,3}, RecPtNum );
				for ( RayNum = 1; RayNum <= SolReflRecSurf( RecSurfNum ).NumReflRays; ++RayNum ) {
					IHit = 0;
					// Loop over possible obstructions. If ray hits one or more obstructions get hit point on closest obstruction.
					// If ray hits no obstructions and is going upward set HitPointSurfNum = 0.
					// If ray hits no obstructions and is going downward set HitPointSurfNum = -1 and get hit point on ground.
					TotObstructionsHit = 0;
					NearestHitSurfNum = 0;
					NearestHitDistance = 1.e+8;
					ObsSurfNumToSkip = 0;
					RayVec = SolReflRecSurf( RecSurfNum ).RayVec( {1,3}, RayNum );
					for ( loop1 = 1; loop1 <= SolReflRecSurf( RecSurfNum ).NumPossibleObs; ++loop1 ) {
						// Surface number of this obstruction
						ObsSurfNum = SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums( loop1 );
						// If a window was hit previously (see below), ObsSurfNumToSkip was set to the window's base surface in order
						// to remove that surface from consideration as a hit surface for this ray
						if ( ObsSurfNum == ObsSurfNumToSkip ) continue;
						// Determine if this ray hits ObsSurfNum (in which case IHit > 0) and, if so, what the
						// distance from the receiving point to the hit point is
						PierceSurface( ObsSurfNum, RecPt, RayVec, IHit, HitPt );
						if ( IHit > 0 ) {
							// added TH 3/29/2010 to set ObsSurfNumToSkip
							if ( Surface( ObsSurfNum ).Class == SurfaceClass_Window ) {
								ObsSurfNumToSkip = Surface( ObsSurfNum ).BaseSurf;
							}

							// If obstruction is a window and its base surface is the nearest obstruction hit so far,
							// set NearestHitSurfNum to this window. Note that in this case NearestHitDistance has already
							// been calculated, so does not have to be recalculated.
							if ( Surface( ObsSurfNum ).Class == SurfaceClass_Window && Surface( ObsSurfNum ).BaseSurf == NearestHitSurfNum ) {
								NearestHitSurfNum = ObsSurfNum;
							} else {
								++TotObstructionsHit;
								// Distance from receiving point to hit point
								HitDistance = distance( HitPt, RecPt );
								// Reset NearestHitSurfNum and NearestHitDistance if this hit point is closer than previous closest
								if ( HitDistance < NearestHitDistance ) {
									NearestHitDistance = HitDistance;
									NearestHitSurfNum = ObsSurfNum;
									NearestHitPt = HitPt;
								} else if ( HitDistance == NearestHitDistance ) { // TH2 CR8959
									// Ray hits mirrored surfaces. Choose the surface facing the ray.
									if ( dot( Surface( ObsSurfNum ).OutNormVec, RayVec ) <= 0.0 ) {
										NearestHitSurfNum = ObsSurfNum;
									}
								}
							}
						} // End of check if obstruction was hit
					} // End of loop over possible obstructions for this ray

					if ( TotObstructionsHit > 0 ) {
						// One or more obstructions were hit by this ray
						SolReflRecSurf( RecSurfNum ).HitPtSurfNum( RecPtNum, RayNum ) = NearestHitSurfNum;
						SolReflRecSurf( RecSurfNum ).RecPtHitPtDis( RecPtNum, RayNum ) = NearestHitDistance;
						SolReflRecSurf( RecSurfNum ).HitPt( {1,3}, RecPtNum, RayNum ) = NearestHitPt;
						// For hit surface, calculate unit normal vector pointing into the hemisphere
						// containing the receiving point
						Vec1 = Surface( NearestHitSurfNum ).Vertex( 1 ) - Surface( NearestHitSurfNum ).Vertex( 3 );
						Vec2 = Surface( NearestHitSurfNum ).Vertex( 2 ) - Surface( NearestHitSurfNum ).Vertex( 3 );
						CrossProduct( Vec1, Vec2, VNorm );
						VNorm /= magnitude( VNorm );
						if ( dot( VNorm, -RayVec ) < 0.0 ) VNorm = -VNorm;
						SolReflRecSurf( RecSurfNum ).HitPtNormVec( {1,3}, RecPtNum, RayNum ) = VNorm;
						// Get solar and visible beam-to-diffuse reflectance at nearest hit point
						ObsConstrNum = Surface( NearestHitSurfNum ).Construction;
						if ( ObsConstrNum > 0 ) {
							// Exterior building surface is nearest hit
							if ( ! Construct( ObsConstrNum ).TypeIsWindow ) {
								// Obstruction is not a window, i.e., is an opaque surface
								SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum ) = 1.0 - Construct( ObsConstrNum ).OutsideAbsorpSolar;
							} else {
								// Obstruction is a window. Assume it is bare so that there is no beam-to-diffuse reflection
								// (beam-to-beam reflection is calculated in subroutine CalcBeamSolSpecularReflFactors).
								SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum ) = 0.0;
							}
						} else {
							// Shading surface is nearest hit
							SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum ) = Surface( NearestHitSurfNum ).ShadowSurfDiffuseSolRefl;
						}
					} else {
						// No obstructions were hit by this ray
						SolReflRecSurf( RecSurfNum ).HitPtSurfNum( RecPtNum, RayNum ) = 0;
						// If ray is going downward find the hit point on the ground plane if the receiving point
						// is above ground level; note that GroundLevelZ is <= 0.0
						if ( RayVec( 3 ) < 0.0 && SolReflRecSurf( RecSurfNum ).RecPt( 3, RecPtNum ) > GroundLevelZ ) {
							// Ray hits ground
							Alfa = std::acos( -RayVec( 3 ) );
							Beta = std::atan2( RayVec( 2 ), RayVec( 1 ) );
							HorDis = ( RecPt( 3 ) - GroundLevelZ ) * std::tan( Alfa );
							GroundHitPt( 3 ) = GroundLevelZ;
							GroundHitPt( 1 ) = RecPt( 1 ) + HorDis * std::cos( Beta );
							GroundHitPt( 2 ) = RecPt( 2 ) + HorDis * std::sin( Beta );
							SolReflRecSurf( RecSurfNum ).HitPt( {1,3}, RecPtNum, RayNum ) = GroundHitPt( {1,3} );
							SolReflRecSurf( RecSurfNum ).HitPtSurfNum( RecPtNum, RayNum ) = -1;
							SolReflRecSurf( RecSurfNum ).RecPtHitPtDis( RecPtNum, RayNum ) = ( RecPt( 3 ) - GroundLevelZ ) / ( -RayVec( 3 ) );
							SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum ) = GndReflectance;
							SolReflRecSurf( RecSurfNum ).HitPtNormVec( 1, RecPtNum, RayNum ) = 0.0;
							SolReflRecSurf( RecSurfNum ).HitPtNormVec( 2, RecPtNum, RayNum ) = 0.0;
							SolReflRecSurf( RecSurfNum ).HitPtNormVec( 3, RecPtNum, RayNum ) = 1.0;
						} // End of check if ray hits ground
					} // End of check if obstruction hit
				} // End of RayNum loop
			} // End of receiving point loop
		} // End of receiving surface loop

	}

	//=====================================================================================================

	void
	CalcBeamSolDiffuseReflFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   September 2003
		//       MODIFIED       TH 4/6/2010, fixed CR 7872
		//       RE-ENGINEERED  B. Griffith, October 2012, for timestep integrated solar

		// PURPOSE OF THIS SUBROUTINE:
		// manage calculations for factors for irradiance on exterior heat transfer surfaces due to
		// beam-to-diffuse solar reflection from obstructions and ground.

		// METHODOLOGY EMPLOYED: call worker routine depending on solar calculation method

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataGlobals::HourOfDay;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int IHr( 0 ); // Hour number

		// FLOW:

		if ( ! DetailedSolarTimestepIntegration ) {
			if ( BeginSimFlag ) {
				DisplayString( "Calculating Beam-to-Diffuse Exterior Solar Reflection Factors" );
			} else {
				DisplayString( "Updating Beam-to-Diffuse Exterior Solar Reflection Factors" );
			}
			ReflFacBmToDiffSolObs = 0.0;
			ReflFacBmToDiffSolGnd = 0.0;
			for ( IHr = 1; IHr <= 24; ++IHr ) {
				FigureBeamSolDiffuseReflFactors( IHr );
			} // End of IHr loop
		} else { // timestep integrated solar, use current hour of day
			ReflFacBmToDiffSolObs( {1,TotSurfaces}, HourOfDay ) = 0.0;
			ReflFacBmToDiffSolGnd( {1,TotSurfaces}, HourOfDay ) = 0.0;
			FigureBeamSolDiffuseReflFactors( HourOfDay );
		}

	}

	void
	FigureBeamSolDiffuseReflFactors( int const iHour )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann, derived from original CalcBeamSolDiffuseReflFactors
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, October 2012, revised for timestep integrated solar

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates factors for irradiance on exterior heat transfer surfaces due to
		// beam-to-diffuse solar reflection from obstructions and ground.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static FArray1D< Real64 > SunVec( 3, 0.0 ); // Unit vector to sun
		static int RecSurfNum( 0 ); // Receiving surface number
		static int SurfNum( 0 ); // Heat transfer surface number corresponding to RecSurfNum
		static int RecPtNum( 0 ); // Receiving point number
		static int NumRecPts( 0 ); // Number of receiving points on a receiving surface
		static int HitPtSurfNum( 0 ); // Surface number of hit point: -1 = ground,
		// 0 = sky or obstruction with receiving point below ground level,
		// >0 = obstruction with receiving point above ground level
		FArray1D< Real64 > ReflBmToDiffSolObs( MaxRecPts ); // Irradiance at a receiving point for
		// beam solar diffusely reflected from obstructions, divided by
		// beam normal irradiance
		FArray1D< Real64 > ReflBmToDiffSolGnd( MaxRecPts ); // Irradiance at a receiving point for
		// beam solar diffusely reflected from the ground, divided by
		// beam normal irradiance
		static int RayNum( 0 ); // Ray number
		static int IHit( 0 ); // > 0 if obstruction is hit; otherwise = 0
		static FArray1D< Real64 > OriginThisRay( 3, 0.0 ); // Origin point of a ray (m)
		static FArray1D< Real64 > ObsHitPt( 3, 0.0 ); // Hit point on obstruction (m)
		static int ObsSurfNum( 0 ); // Obstruction surface number
		static Real64 CosIncBmAtHitPt( 0.0 ); // Cosine of incidence angle of beam solar at hit point
		static Real64 CosIncBmAtHitPt2( 0.0 ); // Cosine of incidence angle of beam solar at hit point,
		//  the mirrored shading surface
		static Real64 BmReflSolRadiance( 0.0 ); // Solar radiance at hit point due to incident beam, divided
		//  by beam normal irradiance
		static Real64 dReflBeamToDiffSol( 0.0 ); // Contribution to reflection factor at a receiving point
		//  from beam solar reflected from a hit point
		static Real64 SunLitFract( 0.0 ); // Sunlit fraction

		ReflBmToDiffSolObs = 0.0;
		ReflBmToDiffSolGnd = 0.0;

		// Unit vector to sun
		SunVec = SUNCOSHR( {1,3}, iHour );

		// loop through each surface that can receive beam solar reflected as diffuse solar from other surfaces
		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SurfNum = SolReflRecSurf( RecSurfNum ).SurfNum;

			for ( RecPtNum = 1; RecPtNum <= SolReflRecSurf( RecSurfNum ).NumRecPts; ++RecPtNum ) {
				ReflBmToDiffSolObs( RecPtNum ) = 0.0;
				ReflBmToDiffSolGnd( RecPtNum ) = 0.0;

				for ( RayNum = 1; RayNum <= SolReflRecSurf( RecSurfNum ).NumReflRays; ++RayNum ) {
					HitPtSurfNum = SolReflRecSurf( RecSurfNum ).HitPtSurfNum( RecPtNum, RayNum );

					// Skip rays that do not hit an obstruction or ground.
					// (Note that if a downgoing ray does not hit an obstruction it will have HitPtSurfNum = 0
					// if the receiving point is below ground level (see subr. InitSolReflRecSurf); this means
					// that a below-ground-level receiving point receives no ground-reflected radiation although
					// it is allowed to receive obstruction-reflected solar radiation and direct (unreflected)
					// beam and sky solar radiation. As far as reflected solar is concerned, the program does
					// not handle a sloped ground plane or a horizontal ground plane whose level is different
					// from one side of the building to another.)
					if ( HitPtSurfNum == 0 ) continue; // Ray hits sky or obstruction with receiving pt. below ground level

					if ( HitPtSurfNum > 0 ) {
						// Skip rays that hit a daylighting shelf, from which solar reflection is calculated separately.
						if ( Surface( HitPtSurfNum ).Shelf > 0 ) continue;

						// Skip rays that hit a window
						// If hit point's surface is a window or glass door go to next ray since it is assumed for now
						// that windows have only beam-to-beam, not beam-to-diffuse, reflection
						// TH 3/29/2010. Code modified and moved
						if ( Surface( HitPtSurfNum ).Class == SurfaceClass_Window || Surface( HitPtSurfNum ).Class == SurfaceClass_GlassDoor ) continue;

						// Skip rays that hit non-sunlit surface. Assume first time step of the hour.
						SunLitFract = SunlitFrac( HitPtSurfNum, iHour, 1 );

						// If hit point's surface is not sunlit go to next ray
						// TH 3/25/2010. why limit to HeatTransSurf? shading surfaces should also apply
						//IF(Surface(HitPtSurfNum)%HeatTransSurf .AND. SunLitFract < 0.01d0) CYCLE
						if ( SunLitFract < 0.01 ) continue;

						// TH 3/26/2010. If the hit point falls into the shadow even though SunLitFract > 0, can Cycle.
						//  This cannot be done now, therefore there are follow-up checks of blocking sun ray
						//   from the hit point.

						// TH 3/29/2010. Code modified and moved up
						// If hit point's surface is a window go to next ray since it is assumed for now
						// that windows have only beam-to-beam, not beam-to-diffuse, reflection
						//IF(Surface(HitPtSurfNum)%Construction > 0) THEN
						//  IF(Construct(Surface(HitPtSurfNum)%Construction)%TypeIsWindow) CYCLE
						//END IF
					}

					// Does an obstruction block the vector from this ray's hit point to the sun?
					IHit = 0;
					OriginThisRay = SolReflRecSurf( RecSurfNum ).HitPt( {1,3}, RecPtNum, RayNum );

					// Note: if sun is in back of hit surface relative to receiving point, CosIncBmAtHitPt will be < 0
					CosIncBmAtHitPt = dot( SolReflRecSurf( RecSurfNum ).HitPtNormVec( {1,3}, RecPtNum, RayNum ), SunVec );
					if ( CosIncBmAtHitPt <= 0.0 ) continue;

					// CR 7872 - TH 4/6/2010. The shading surfaces should point to the receiveing heat transfer surface
					//  according to the the right hand rule. If user inputs do not follow the rule, use the following
					//  code to check the mirrored shading surface
					if ( HitPtSurfNum > 0 ) {
						if ( Surface( HitPtSurfNum ).ShadowingSurf ) {
							if ( HitPtSurfNum + 1 < TotSurfaces ) {
								if ( Surface( HitPtSurfNum + 1 ).ShadowingSurf && Surface( HitPtSurfNum + 1 ).MirroredSurf ) {
									// Check whether the sun is behind the mirrored shading surface
									CosIncBmAtHitPt2 = dot( Surface( HitPtSurfNum + 1 ).OutNormVec, SunVec );
									if ( CosIncBmAtHitPt2 >= 0.0 ) continue;
								}
							}
						}
					}

					// TH 3/25/2010. CR 7872. Seems should loop over all possible obstructions for the HitPtSurfNum
					//  rather than RecSurfNum, because if the HitPtSurfNum is a shading surface,
					//  it does not belong to SolReflRecSurf which only contain heat transfer surfaces
					//  that can receive reflected solar (ExtSolar = True)!

					// To speed up, ideally should store all possible shading surfaces for the HitPtSurfNum
					//  obstruction surface in the SolReflSurf(HitPtSurfNum)%PossibleObsSurfNums(loop) array as well
					for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
						//        DO loop = 1,SolReflRecSurf(RecSurfNum)%NumPossibleObs
						//          ObsSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop)

						//CR 8959 -- The other side of a mirrored surface cannot obstruct the mirrored surface
						if ( HitPtSurfNum > 0 ) {
							if ( Surface( HitPtSurfNum ).MirroredSurf ) {
								if ( ObsSurfNum == HitPtSurfNum - 1 ) continue;
							}
						}

						// skip the hit surface
						if ( ObsSurfNum == HitPtSurfNum ) continue;

						// skip mirrored surfaces
						if ( Surface( ObsSurfNum ).MirroredSurf ) continue;
						//IF(Surface(ObsSurfNum)%ShadowingSurf .AND. Surface(ObsSurfNum)%Name(1:3) == 'Mir') THEN
						//  CYCLE
						//ENDIF

						// skip interior surfaces
						if ( Surface( ObsSurfNum ).ExtBoundCond >= 1 ) continue;

						// For now it is assumed that obstructions that are shading surfaces are opaque.
						// An improvement here would be to allow these to have transmittance.
						PierceSurface( ObsSurfNum, OriginThisRay, SunVec, IHit, ObsHitPt );
						if ( IHit > 0 ) break; // An obstruction was hit
					}
					if ( IHit > 0 ) continue; // Sun does not reach this ray's hit point

					// Sun reaches this ray's hit point; get beam-reflected diffuse radiance at hit point for
					// unit beam normal solar

					//CosIncBmAtHitPt = DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%HitPtNormVec(1:3,RecPtNum,RayNum),SunVec)
					// Note: if sun is in back of hit surface relative to receiving point, CosIncBmAtHitPt will be < 0
					// and use of MAX in following gives zero beam solar reflecting at hit point.
					//BmReflSolRadiance = MAX(0.0d0,CosIncBmAtHitPt)*SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum)

					BmReflSolRadiance = CosIncBmAtHitPt * SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum );

					if ( BmReflSolRadiance > 0.0 ) {
						// Contribution to reflection factor from this hit point
						if ( HitPtSurfNum > 0 ) {
							// Ray hits an obstruction
							dReflBeamToDiffSol = BmReflSolRadiance * SolReflRecSurf( RecSurfNum ).dOmegaRay( RayNum ) * SolReflRecSurf( RecSurfNum ).CosIncAngRay( RayNum ) / Pi;
							ReflBmToDiffSolObs( RecPtNum ) += dReflBeamToDiffSol;
						} else {
							// Ray hits ground (in this case we do not multiply by BmReflSolRadiance since
							// ground reflectance and cos of incidence angle of sun on
							// ground is taken into account later when ReflFacBmToDiffSolGnd is used)
							dReflBeamToDiffSol = SolReflRecSurf( RecSurfNum ).dOmegaRay( RayNum ) * SolReflRecSurf( RecSurfNum ).CosIncAngRay( RayNum ) / Pi;
							ReflBmToDiffSolGnd( RecPtNum ) += dReflBeamToDiffSol;
						}
					}
				} // End of loop over rays from receiving point
			} // End of loop over receiving points

			// Average over receiving points
			ReflFacBmToDiffSolObs( SurfNum, iHour ) = 0.0;
			ReflFacBmToDiffSolGnd( SurfNum, iHour ) = 0.0;
			NumRecPts = SolReflRecSurf( RecSurfNum ).NumRecPts;
			for ( RecPtNum = 1; RecPtNum <= NumRecPts; ++RecPtNum ) {
				ReflFacBmToDiffSolObs( SurfNum, iHour ) += ReflBmToDiffSolObs( RecPtNum );
				ReflFacBmToDiffSolGnd( SurfNum, iHour ) += ReflBmToDiffSolGnd( RecPtNum );
			}
			ReflFacBmToDiffSolObs( SurfNum, iHour ) /= NumRecPts;
			ReflFacBmToDiffSolGnd( SurfNum, iHour ) /= NumRecPts;

			// Do not allow ReflFacBmToDiffSolGnd to exceed the surface's unobstructed ground view factor
			ReflFacBmToDiffSolGnd( SurfNum, iHour ) = min( 0.5 * ( 1.0 - Surface( SurfNum ).CosTilt ), ReflFacBmToDiffSolGnd( SurfNum, iHour ) );
			// Note: the above factors are dimensionless; they are equal to
			// (W/m2 reflected solar incident on SurfNum)/(W/m2 beam normal solar)
		} // End of loop over receiving surfaces

	}

	//=================================================================================================

	void
	CalcBeamSolSpecularReflFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, October 2012, for timestep integrated solar

		// PURPOSE OF THIS SUBROUTINE:
		// Manage calculation of factors for beam solar irradiance on exterior heat transfer surfaces due to
		// specular (beam-to-beam) reflection from obstructions such as a highly-glazed neighboring
		// building.

		// METHODOLOGY EMPLOYED:
		// call worker routine as appropriate

		// REFERENCES: na

		// Using/Aliasing
		using DataGlobals::HourOfDay;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int IHr( 0 ); // Hour number

		// FLOW:
		if ( ! DetailedSolarTimestepIntegration ) {
			if ( BeginSimFlag ) {
				DisplayString( "Calculating Beam-to-Beam Exterior Solar Reflection Factors" );
			} else {
				DisplayString( "Updating Beam-to-Beam Exterior Solar Reflection Factors" );
			}
			ReflFacBmToBmSolObs = 0.0;
			CosIncAveBmToBmSolObs = 0.0;
			for ( IHr = 1; IHr <= 24; ++IHr ) {
				FigureBeamSolSpecularReflFactors( IHr );
			} // End of IHr loop
		} else { // timestep integrated solar, use current hour of day
			ReflFacBmToBmSolObs( {1,TotSurfaces}, HourOfDay ) = 0.0;
			CosIncAveBmToBmSolObs( {1,TotSurfaces}, HourOfDay ) = 0.0;
			FigureBeamSolSpecularReflFactors( HourOfDay );
		}

	}

	void
	FigureBeamSolSpecularReflFactors( int const iHour )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   September 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, October 2012, for timestep integrated solar

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates factors for beam solar irradiance on exterior heat transfer surfaces due to
		// specular (beam-to-beam) reflection from obstructions such as a highly-glazed neighboring
		// building. Specular reflection can occur from shading surfaces with non-zero specular
		// reflectance and from exterior windows of the building (in calculating reflection from
		// these windows, they are assumed to have no shades or blinds).
		// Reflection from the ground and opaque building surfaces is assumed to be totally diffuse,
		// i.e. these surfaces has no specular reflection component.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::POLYF;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int loop( 0 ); // DO loop indices
		static int loop2( 0 ); // DO loop indices
		static FArray1D< Real64 > SunVec( 3, 0.0 ); // Unit vector to sun
		static FArray1D< Real64 > SunVecMir( 3, 0.0 ); // Unit vector to sun mirrored by a reflecting surface
		static int RecSurfNum( 0 ); // Receiving surface number
		static int SurfNum( 0 ); // Heat transfer surface number corresponding to RecSurfNum
		static int NumRecPts( 0 ); // Number of receiving points on a receiving surface
		static int RecPtNum( 0 ); // Receiving point number
		static FArray1D< Real64 > RecPt( 3, 0.0 ); // Receiving point (m)
		static FArray1D< Real64 > HitPtRefl( 3, 0.0 ); // Hit point on a reflecting surface (m)
		FArray1D< Real64 > ReflBmToDiffSolObs( MaxRecPts ); // Irradiance at a receiving point for
		// beam solar diffusely reflected from obstructions, divided by
		// beam normal irradiance
		//unused  INTEGER           :: RayNum               =0   ! Ray number
		static int IHitRefl( 0 ); // > 0 if reflecting surface is hit; otherwise = 0
		static int IHitObs( 0 ); // > 0 if obstruction is hit
		static FArray1D< Real64 > HitPtObs( 3, 0.0 ); // Hit point on obstruction (m)
		static int IHitObsRefl( 0 ); // > 0 if obstruction hit between rec. pt. and reflection point
		static int ObsSurfNum( 0 ); // Obstruction surface number
		static int ReflSurfNum( 0 ); // Reflecting surface number
		static int ReflSurfRecNum( 0 ); // Receiving surface number corresponding to a reflecting surface number
		static FArray1D< Real64 > ReflNorm( 3, 0.0 ); // Unit normal to reflecting surface
		FArray1D< Real64 > ReflBmToBmSolObs( MaxRecPts ); // Irradiance at a receiving point for
		// beam solar specularly reflected from obstructions, divided by
		// beam normal irradiance
		static Real64 ReflDistance( 0.0 ); // Distance from receiving point to hit point on a reflecting surface (m)
		static Real64 ObsDistance( 0.0 ); // Distance from receiving point to hit point on an obstruction (m)
		static Real64 SpecReflectance( 0.0 ); // Specular reflectance of a reflecting surface
		static int ConstrNumRefl( 0 ); // Construction number of a reflecting surface
		static Real64 CosIncAngRefl( 0.0 ); // Cosine of incidence angle of beam on reflecting surface
		static Real64 CosIncAngRec( 0.0 ); // Angle of incidence of reflected beam on receiving surface
		static Real64 ReflFac( 0.0 ); // Contribution to specular reflection factor
		FArray1D< Real64 > ReflFacTimesCosIncSum( MaxRecPts ); // Sum of ReflFac times CosIncAngRefl
		static Real64 CosIncWeighted( 0.0 ); // Cosine of incidence angle on receiving surf weighted by reflection factor

		ReflBmToDiffSolObs = 0.0;
		ReflFacTimesCosIncSum = 0.0;

		if ( SUNCOSHR( 3, iHour ) < SunIsUpValue ) return; // Skip if sun is below horizon

		// Unit vector to sun
		SunVec = SUNCOSHR( {1,3}, iHour );

		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SurfNum = SolReflRecSurf( RecSurfNum ).SurfNum;
			if ( SolReflRecSurf( RecSurfNum ).NumPossibleObs > 0 ) {
				ReflBmToBmSolObs = 0.0;
				ReflFacTimesCosIncSum = 0.0;
				// Find possible reflecting surfaces for this receiving surface
				for ( loop = 1; loop <= SolReflRecSurf( RecSurfNum ).NumPossibleObs; ++loop ) {
					ReflSurfNum = SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums( loop );
					// Keep windows; keep shading surfaces with specular reflectance
					if ( ( Surface( ReflSurfNum ).Class == SurfaceClass_Window && Surface( ReflSurfNum ).ExtSolar ) || ( Surface( ReflSurfNum ).ShadowSurfGlazingFrac > 0.0 && Surface( ReflSurfNum ).ShadowingSurf ) ) {
						// Skip if window and not sunlit
						if ( Surface( ReflSurfNum ).Class == SurfaceClass_Window && SunlitFrac( ReflSurfNum, iHour, 1 ) < 0.01 ) continue;
						// Check if sun is in front of this reflecting surface.
						ReflNorm = Surface( ReflSurfNum ).OutNormVec;
						CosIncAngRefl = dot( SunVec, ReflNorm );
						if ( CosIncAngRefl < 0.0 ) continue;

						// Get sun position unit vector for mirror image of sun in reflecting surface
						SunVecMir = SunVec - 2.0 * dot( SunVec, ReflNorm ) * ReflNorm;
						// Angle of incidence of reflected beam on receiving surface
						CosIncAngRec = dot( SolReflRecSurf( RecSurfNum ).NormVec, SunVecMir );
						if ( CosIncAngRec <= 0.0 ) continue;
						for ( RecPtNum = 1; RecPtNum <= SolReflRecSurf( RecSurfNum ).NumRecPts; ++RecPtNum ) {
							// See if ray from receiving point to mirrored sun hits the reflecting surface
							RecPt = SolReflRecSurf( RecSurfNum ).RecPt( {1,3}, RecPtNum );
							PierceSurface( ReflSurfNum, RecPt, SunVecMir, IHitRefl, HitPtRefl );
							if ( IHitRefl > 0 ) {
								// Reflecting surface was hit
								ReflDistance = distance( HitPtRefl, RecPt );
								// Determine if ray from receiving point to hit point is obstructed
								IHitObsRefl = 0;
								for ( loop2 = 1; loop2 <= SolReflRecSurf( RecSurfNum ).NumPossibleObs; ++loop2 ) {
									ObsSurfNum = SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums( loop2 );
									if ( ObsSurfNum == ReflSurfNum || ObsSurfNum == Surface( ReflSurfNum ).BaseSurf ) continue;
									PierceSurface( ObsSurfNum, RecPt, SunVecMir, IHitObs, HitPtObs );
									if ( IHitObs > 0 ) {
										ObsDistance = distance( HitPtObs, RecPt );
										if ( ObsDistance < ReflDistance ) {
											IHitObsRefl = 1;
											break;
										}
									}
								}
								if ( IHitObsRefl > 0 ) continue; // Obstruct'n closer than reflect'n pt. was hit; go to next rec. pt.
								// There is no obstruction for this ray between rec. pt. and hit point on reflecting surface.
								// See if ray from hit pt. on reflecting surface to original (unmirrored) sun position is obstructed
								IHitObs = 0;
								if ( Surface( ReflSurfNum ).Class == SurfaceClass_Window ) {
									// Reflecting surface is a window.
									// Receiving surface number for this window.
									ReflSurfRecNum = Surface( ReflSurfNum ).ShadowSurfRecSurfNum;
									if ( ReflSurfRecNum > 0 ) {
										// Loop over possible obstructions for this window
										for ( loop2 = 1; loop2 <= SolReflRecSurf( ReflSurfRecNum ).NumPossibleObs; ++loop2 ) {
											ObsSurfNum = SolReflRecSurf( ReflSurfRecNum ).PossibleObsSurfNums( loop2 );
											PierceSurface( ObsSurfNum, HitPtRefl, SunVec, IHitObs, HitPtObs );
											if ( IHitObs > 0 ) break;
										}
									}
								} else {
									// Reflecting surface is a building shade
									for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
										if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
										if ( ObsSurfNum == ReflSurfNum ) continue;

										//TH2 CR8959 -- Skip mirrored surfaces
										if ( Surface( ObsSurfNum ).MirroredSurf ) continue;
										//TH2 CR8959 -- The other side of a mirrored surface cannot obstruct the mirrored surface
										if ( Surface( ReflSurfNum ).MirroredSurf ) {
											if ( ObsSurfNum == ReflSurfNum - 1 ) continue;
										}

										PierceSurface( ObsSurfNum, HitPtRefl, SunVec, IHitObs, HitPtObs );
										if ( IHitObs > 0 ) break;
									}
								}

								if ( IHitObs > 0 ) continue; // Obstruct'n hit between reflect'n hit point and sun; go to next receiving pt.

								// No obstructions. Calculate reflected beam irradiance at receiving pt. from this reflecting surface.
								SpecReflectance = 0.0;
								if ( Surface( ReflSurfNum ).Class == SurfaceClass_Window ) {
									ConstrNumRefl = Surface( ReflSurfNum ).Construction;
									SpecReflectance = POLYF( std::abs( CosIncAngRefl ), Construct( ConstrNumRefl ).ReflSolBeamFrontCoef( {1,6} ) );
								}
								if ( Surface( ReflSurfNum ).ShadowingSurf && Surface( ReflSurfNum ).ShadowSurfGlazingConstruct > 0 ) {
									ConstrNumRefl = Surface( ReflSurfNum ).ShadowSurfGlazingConstruct;
									SpecReflectance = Surface( ReflSurfNum ).ShadowSurfGlazingFrac * POLYF( std::abs( CosIncAngRefl ), Construct( ConstrNumRefl ).ReflSolBeamFrontCoef( {1,6} ) );
								}
								// Angle of incidence of reflected beam on receiving surface
								CosIncAngRec = dot( SolReflRecSurf( RecSurfNum ).NormVec, SunVecMir );
								ReflFac = SpecReflectance * CosIncAngRec;
								// Contribution to specular reflection factor
								ReflBmToBmSolObs( RecPtNum ) += ReflFac;
								ReflFacTimesCosIncSum( RecPtNum ) += ReflFac * CosIncAngRec;
							} // End of check if reflecting surface was hit
						} // End of loop over receiving points
					} // End of check if valid reflecting surface
				} // End of loop over obstructing surfaces
				// Average over receiving points
				NumRecPts = SolReflRecSurf( RecSurfNum ).NumRecPts;

				for ( RecPtNum = 1; RecPtNum <= NumRecPts; ++RecPtNum ) {
					if ( ReflBmToBmSolObs( RecPtNum ) != 0.0 ) {
						CosIncWeighted = ReflFacTimesCosIncSum( RecPtNum ) / ReflBmToBmSolObs( RecPtNum );
					} else {
						CosIncWeighted = 0.0;
					}
					CosIncAveBmToBmSolObs( SurfNum, iHour ) += CosIncWeighted;
					ReflFacBmToBmSolObs( SurfNum, iHour ) += ReflBmToBmSolObs( RecPtNum );
				}
				ReflFacBmToBmSolObs( SurfNum, iHour ) /= double( NumRecPts );
				CosIncAveBmToBmSolObs( SurfNum, iHour ) /= double( NumRecPts );
			} // End of check if number of possible obstructions > 0
		} // End of loop over receiving surfaces

	}

	//=================================================================================================

	void
	CalcSkySolDiffuseReflFactors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   October 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates factors for irradiance on exterior heat transfer surfaces due to
		// reflection of sky diffuse solar radiation from obstructions and ground.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
		using namespace Vectors;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int RecSurfNum( 0 ); // Receiving surface number
		static int SurfNum( 0 ); // Heat transfer surface number corresponding to RecSurfNum
		static int ObsSurfNum( 0 ); // Obstruction surface number
		static int RecPtNum( 0 ); // Receiving point number
		static int NumRecPts( 0 ); // Number of receiving points on a receiving surface
		static int HitPtSurfNum( 0 ); // Surface number of hit point: -1 = ground,
		// 0 = sky or obstruction with receiving point below ground level,
		// >0 = obstruction with receiving point above ground level
		static int HitPtSurfNumX( 0 ); // For a shading surface, HitPtSurfNum for original surface,
		// HitPitSurfNum + 1 for mirror surface
		FArray1D< Real64 > ReflSkySolObs( MaxRecPts ); // Irradiance at a receiving point for sky diffuse solar
		// reflected from obstructions, divided by unobstructed
		// sky diffuse horizontal irradiance
		FArray1D< Real64 > ReflSkySolGnd( MaxRecPts ); // Irradiance at a receiving point for sky diffuse solar
		// reflected from ground, divided by unobstructed
		// sky diffuse horizontal irradiance
		static int RayNum( 0 ); // Ray number
		static FArray1D< Real64 > HitPtRefl( 3, 0.0 ); // Coordinates of hit point on obstruction or ground (m)
		static int IHitObs( 0 ); // > 0 if obstruction is hit; otherwise = 0
		static FArray1D< Real64 > HitPtObs( 3, 0.0 ); // Hit point on an obstruction (m)
		//unused  REAL(r64)         :: ObsHitPt(3)          =0.0 ! Hit point on obstruction (m)
		static Real64 dOmega( 0.0 ); // Solid angle increment (steradians)
		static Real64 CosIncAngRayToSky( 0.0 ); // Cosine of incidence angle on ground of ray to sky
		static Real64 SkyReflSolRadiance( 0.0 ); // Reflected radiance at hit point divided by unobstructed
		//  sky diffuse horizontal irradiance
		static Real64 dReflSkySol( 0.0 ); // Contribution to reflection factor at a receiving point
		//  from sky solar reflected from a hit point
		static Real64 Phi( 0.0 ); // Altitude angle and increment (radians)
		static Real64 DPhi( 0.0 ); // Altitude angle and increment (radians)
		static Real64 SPhi( 0.0 ); // Sine of Phi
		static Real64 CPhi( 0.0 ); // Cosine of Phi
		static Real64 Theta( 0.0 ); // Azimuth angle (radians)
		static Real64 DTheta( 0.0 ); // Azimuth increment (radians)
		static int IPhi( 0 ); // Altitude angle index
		static int ITheta( 0 ); // Azimuth angle index
		static FArray1D< Real64 > URay( 3, 0.0 ); // Unit vector along ray from ground hit point
		static FArray1D< Real64 > SurfVertToGndPt( 3, 0.0 ); // Vector from a vertex of possible obstructing surface to ground
		//  hit point (m)
		static FArray1D< Real64 > SurfVert( 3, 0.0 ); // Surface vertex (m)
		static Real64 dReflSkyGnd( 0.0 ); // Factor for ground radiance due to direct sky diffuse reflection
		// FLOW:

		DisplayString( "Calculating Sky Diffuse Exterior Solar Reflection Factors" );
		ReflSkySolObs = 0.0;
		ReflSkySolGnd = 0.0;

		for ( RecSurfNum = 1; RecSurfNum <= TotSolReflRecSurf; ++RecSurfNum ) {
			SurfNum = SolReflRecSurf( RecSurfNum ).SurfNum;
			for ( RecPtNum = 1; RecPtNum <= SolReflRecSurf( RecSurfNum ).NumRecPts; ++RecPtNum ) {
				ReflSkySolObs( RecPtNum ) = 0.0;
				ReflSkySolGnd( RecPtNum ) = 0.0;
				for ( RayNum = 1; RayNum <= SolReflRecSurf( RecSurfNum ).NumReflRays; ++RayNum ) {
					HitPtSurfNum = SolReflRecSurf( RecSurfNum ).HitPtSurfNum( RecPtNum, RayNum );
					// Skip rays that do not hit an obstruction or ground.
					// (Note that if a downgoing ray does not hit an obstruction it will have HitPtSurfNum = 0
					// if the receiving point is below ground level (see subr. InitSolReflRecSurf); this means
					// that a below-ground-level receiving point receives no ground-reflected radiation although
					// it is allowed to receive obstruction-reflected solar radiation and direct (unreflected)
					// beam and sky solar radiation. As far as reflected solar is concerned, the program does
					// not handle a sloped ground plane or a horizontal ground plane whose level is different
					// from one side of the building to another.)
					if ( HitPtSurfNum == 0 ) continue; // Ray hits sky or obstruction with receiving pt. below ground level
					HitPtRefl = SolReflRecSurf( RecSurfNum ).HitPt( {1,3}, RecPtNum, RayNum );
					if ( HitPtSurfNum > 0 ) {
						// Ray hits an obstruction
						// Skip hit points on daylighting shelves, from which solar reflection is separately calculated
						if ( Surface( HitPtSurfNum ).Shelf > 0 ) continue;
						// Reflected radiance at hit point divided by unobstructed sky diffuse horizontal irradiance
						HitPtSurfNumX = HitPtSurfNum;
						// Each shading surface has a "mirror" duplicate surface facing in the opposite direction.
						// The following gets the correct side of a shading surface in order to get the right value
						// of DifShdgRatioIsoSky (the two sides can have different sky shadowing).
						if ( Surface( HitPtSurfNum ).ShadowingSurf ) {
							if ( dot( SolReflRecSurf( RecSurfNum ).RayVec( {1,3}, RayNum ), Surface( HitPtSurfNum ).OutNormVec ) > 0.0 ) {
								if ( HitPtSurfNum + 1 < TotSurfaces ) HitPtSurfNumX = HitPtSurfNum + 1;
								if ( Surface( HitPtSurfNumX ).Shelf > 0 ) continue;
							}
						}

						if ( ! DetailedSkyDiffuseAlgorithm || ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
							SkyReflSolRadiance = Surface( HitPtSurfNumX ).ViewFactorSky * DifShdgRatioIsoSky( HitPtSurfNumX ) * SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum );
						} else {
							SkyReflSolRadiance = Surface( HitPtSurfNumX ).ViewFactorSky * DifShdgRatioIsoSkyHRTS( HitPtSurfNumX, 1, 1 ) * SolReflRecSurf( RecSurfNum ).HitPtSolRefl( RecPtNum, RayNum );
						}
						dReflSkySol = SkyReflSolRadiance * SolReflRecSurf( RecSurfNum ).dOmegaRay( RayNum ) * SolReflRecSurf( RecSurfNum ).CosIncAngRay( RayNum ) / Pi;
						ReflSkySolObs( RecPtNum ) += dReflSkySol;
					} else {
						// Ray hits ground;
						// Find radiance at hit point due to reflection of sky diffuse reaching
						// ground directly, i.e., without reflecting from obstructions.
						// Send rays upward from hit point and see which ones are unobstructed and so go to sky.
						// Divide hemisphere centered at ground hit point into elements of altitude Phi and
						// azimuth Theta and create upward-going ray unit vector at each Phi,Theta pair.
						// Phi = 0 at the horizon; Phi = Pi/2 at the zenith.
						DPhi = PiOvr2 / ( AltAngStepsForSolReflCalc / 2.0 );
						dReflSkyGnd = 0.0;
						// Altitude loop
						for ( IPhi = 1; IPhi <= ( AltAngStepsForSolReflCalc / 2 ); ++IPhi ) {
							Phi = ( IPhi - 0.5 ) * DPhi;
							SPhi = std::sin( Phi );
							CPhi = std::cos( Phi );
							// Third component of ray unit vector in (Theta,Phi) direction
							URay( 3 ) = SPhi;
							DTheta = 2.0 * Pi / ( 2.0 * AzimAngStepsForSolReflCalc );
							dOmega = CPhi * DTheta * DPhi;
							// Cosine of angle of incidence of ray on ground
							CosIncAngRayToSky = SPhi;
							// Azimuth loop
							for ( ITheta = 1; ITheta <= 2 * AzimAngStepsForSolReflCalc; ++ITheta ) {
								Theta = ( ITheta - 0.5 ) * DTheta;
								URay( 1 ) = CPhi * std::cos( Theta );
								URay( 2 ) = CPhi * std::sin( Theta );
								// Does this ray hit an obstruction?
								IHitObs = 0;
								for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
									if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
									// Horizontal roof surfaces cannot be obstructions for rays from ground
									if ( Surface( ObsSurfNum ).Tilt < 5.0 ) continue;
									if ( ! Surface( ObsSurfNum ).ShadowingSurf ) {
										if ( dot( URay, Surface( ObsSurfNum ).OutNormVec ) >= 0.0 ) continue;
										// Special test for vertical surfaces with URay dot OutNormVec < 0; excludes
										// case where ground hit point is in back of ObsSurfNum
										if ( Surface( ObsSurfNum ).Tilt > 89.0 && Surface( ObsSurfNum ).Tilt < 91.0 ) {
											SurfVert = Surface( ObsSurfNum ).Vertex( 2 );
											SurfVertToGndPt = HitPtRefl - SurfVert;
											if ( dot( SurfVertToGndPt, Surface( ObsSurfNum ).OutNormVec ) < 0.0 ) continue;
										}
									}
									PierceSurface( ObsSurfNum, HitPtRefl, URay, IHitObs, HitPtObs );
									if ( IHitObs > 0 ) break;
								}

								if ( IHitObs > 0 ) continue; // Obstruction hit
								// Sky is hit
								dReflSkyGnd += CosIncAngRayToSky * dOmega / Pi;
							} // End of azimuth loop
						} // End of altitude loop
						ReflSkySolGnd( RecPtNum ) += dReflSkyGnd * SolReflRecSurf( RecSurfNum ).dOmegaRay( RayNum ) * SolReflRecSurf( RecSurfNum ).CosIncAngRay( RayNum ) / Pi;
					} // End of check if ray from receiving point hits obstruction or ground
				} // End of loop over rays from receiving point
			} // End of loop over receiving points

			// Average over receiving points
			ReflFacSkySolObs( SurfNum ) = 0.0;
			ReflFacSkySolGnd( SurfNum ) = 0.0;
			NumRecPts = SolReflRecSurf( RecSurfNum ).NumRecPts;
			for ( RecPtNum = 1; RecPtNum <= NumRecPts; ++RecPtNum ) {
				ReflFacSkySolObs( SurfNum ) += ReflSkySolObs( RecPtNum );
				ReflFacSkySolGnd( SurfNum ) += ReflSkySolGnd( RecPtNum );
			}
			ReflFacSkySolObs( SurfNum ) /= NumRecPts;
			ReflFacSkySolGnd( SurfNum ) /= NumRecPts;
			// Do not allow ReflFacBmToDiffSolGnd to exceed the surface's unobstructed ground view factor
			ReflFacSkySolGnd( SurfNum ) = min( 0.5 * ( 1.0 - Surface( SurfNum ).CosTilt ), ReflFacSkySolGnd( SurfNum ) );
			// Note: the above factors are dimensionless; they are equal to
			// (W/m2 reflected solar incident on SurfNum)/(W/m2 unobstructed horizontal sky diffuse irradiance)
		} // End of loop over receiving surfaces

	}

	//=================================================================================================

	void
	CrossProduct(
		FArray1A< Real64 > A, // Vector components: C = A X B
		FArray1A< Real64 > B,
		FArray1A< Real64 > C
	)
	{

		// Cross product between vectors A and B

		// Argument array dimensioning
		A.dim( 3 );
		B.dim( 3 );
		C.dim( 3 );

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:
		C( 1 ) = A( 2 ) * B( 3 ) - A( 3 ) * B( 2 );
		C( 2 ) = A( 3 ) * B( 1 ) - A( 1 ) * B( 3 );
		C( 3 ) = A( 1 ) * B( 2 ) - A( 2 ) * B( 1 );

	}

	void
	PierceSurface(
		int const ISurf, // Surface index
		FArray1A< Real64 > const R1, // Point from which ray originates
		FArray1A< Real64 > const RN, // Unit vector along in direction of ray whose
		int & IPIERC, // =1 if line through point R1 in direction of unit vector
		FArray1A< Real64 > CPhit // Point that ray along RN intersects plane of surface
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       Sept 2003, FCW: modification of Daylighting routine DayltgPierceSurface
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns point CPhit that line through point R1 in direction of unit vector RN intersects
		// the plan of surface ISurf. IPIERC = 1 if CPhit is inside the perimeter of ISurf. If not,
		// IPIERC = 0. This routine works for convex and concave surfaces with 3 or more vertices.
		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Based on DOE-2.1E subroutine DPIERC.

		// USE STATEMENTS:na

		// Argument array dimensioning
		R1.dim( 3 );
		RN.dim( 3 );
		CPhit.dim( 3 );

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  intersection with surface is to be determined
		//  RN intersects surface ISurf; =0 otherwise.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NV; // Number of vertices (3 or 4)
		FArray1D< Real64 > AXC( 3 ); // Cross product of A and C
		FArray1D< Real64 > SN( 3 ); // Vector normal to surface (SN = A1 X A2)
		//unused  REAL(r64) :: AA(3)                    ! AA(I) = A(N,I)
		//unused  REAL(r64) :: CC(3)                    ! CC(I) = C(N,I)
		FArray1D< Real64 > CCC( 3 ); // Vector from vertex 2 to CP
		FArray1D< Real64 > AAA( 3 ); // Vector from vertex 2 to vertex 1
		FArray1D< Real64 > BBB( 3 ); // Vector from vertex 2 to vertex 3
		int N; // Vertex loop index
		Real64 F1; // Intermediate variables
		Real64 F2;
		Real64 SCALE; // Scale factor
		Real64 DOTCB; // Dot product of vectors CCC and BBB
		Real64 DOTCA; // Dot product of vectors CCC and AAA
		//unused  REAL(r64) :: DOTAXCSN                 ! Dot product of vectors AXC and SN

		static FArray2D< Real64 > V; // Vertices of surfaces
		static FArray2D< Real64 > A; // Vertex-to-vertex vectors; A(1,i) is from vertex 1 to 2, etc.
		static FArray2D< Real64 > C; // Vectors from vertices to intersection point
		static bool firstTime( true );

		// FLOW:
		IPIERC = 0;

		// Vertex vectors
		if ( firstTime ) {
			V.allocate( MaxVerticesPerSurface, 3 );
			V = 0.0;
			A.allocate( MaxVerticesPerSurface, 3 );
			A = 0.0;
			C.allocate( MaxVerticesPerSurface, 3 );
			C = 0.0;
			firstTime = false;
		}

		// Set the first two V & A
		V( 1, 1 ) = Surface( ISurf ).Vertex( 1 ).x;
		V( 1, 2 ) = Surface( ISurf ).Vertex( 1 ).y;
		V( 1, 3 ) = Surface( ISurf ).Vertex( 1 ).z;

		A( 1, 1 ) = Surface( ISurf ).Vertex( 2 ).x - V( 1, 1 );
		A( 1, 2 ) = Surface( ISurf ).Vertex( 2 ).y - V( 1, 2 );
		A( 1, 3 ) = Surface( ISurf ).Vertex( 2 ).z - V( 1, 3 );

		V( 2, 1 ) = Surface( ISurf ).Vertex( 2 ).x;
		V( 2, 2 ) = Surface( ISurf ).Vertex( 2 ).y;
		V( 2, 3 ) = Surface( ISurf ).Vertex( 2 ).z;

		A( 2, 1 ) = Surface( ISurf ).Vertex( 3 ).x - V( 2, 1 );
		A( 2, 2 ) = Surface( ISurf ).Vertex( 3 ).y - V( 2, 2 );
		A( 2, 3 ) = Surface( ISurf ).Vertex( 3 ).z - V( 2, 3 );

		// Vector normal to surface
		SN( 1 ) = A( 1, 2 ) * A( 2, 3 ) - A( 1, 3 ) * A( 2, 2 );
		SN( 2 ) = A( 1, 3 ) * A( 2, 1 ) - A( 1, 1 ) * A( 2, 3 );
		SN( 3 ) = A( 1, 1 ) * A( 2, 2 ) - A( 1, 2 ) * A( 2, 1 );

		// Scale factor, the solution of SN.(CPhit-V2) = 0 and
		// CPhit = R1 + SCALE*RN, where CPhit is the point that RN,
		// when extended, intersects the plane of the surface.
		F2 = dot( SN, RN );
		if ( std::abs( F2 ) < 0.01 ) return; // Skip surfaces that are parallel to RN
		F1 = SN( 1 ) * ( V( 2, 1 ) - R1( 1 ) ) + SN( 2 ) * ( V( 2, 2 ) - R1( 2 ) ) + SN( 3 ) * ( V( 2, 3 ) - R1( 3 ) );
		//F1 = DOT_PRODUCT(SN, V2 - R1)
		SCALE = F1 / F2;
		if ( SCALE <= 0.0 ) return; // Skip surfaces that RN points away from
		CPhit = R1 + RN * SCALE; // Point that RN intersects plane of surface

		// Two cases: rectangle and non-rectangle; do rectangle
		// first since most common shape and faster calculation
		if ( Surface( ISurf ).Shape == Rectangle || Surface( ISurf ).Shape == RectangularDoorWindow || Surface( ISurf ).Shape == RectangularOverhang || Surface( ISurf ).Shape == RectangularLeftFin || Surface( ISurf ).Shape == RectangularRightFin ) {
			// Surface is rectangular
			// Vectors from vertex 2 to vertex 1 and vertex 2 to vertex 3

			// Intersection point, CCC, is inside rectangle if
			// 0 < CCC.BBB < BBB.BBB AND 0 < CCC.AAA < AAA.AAA

			//CCC = CPhit - V2  ! Vector from vertex 2 to CPhit
			CCC( 1 ) = CPhit( 1 ) - V( 2, 1 );
			CCC( 2 ) = CPhit( 2 ) - V( 2, 2 );
			CCC( 3 ) = CPhit( 3 ) - V( 2, 3 );

			// Set third V just for here
			V( 3, 1 ) = Surface( ISurf ).Vertex( 3 ).x;
			V( 3, 2 ) = Surface( ISurf ).Vertex( 3 ).y;
			V( 3, 3 ) = Surface( ISurf ).Vertex( 3 ).z;

			//BBB = V3 - V2
			BBB( 1 ) = V( 3, 1 ) - V( 2, 1 );
			BBB( 2 ) = V( 3, 2 ) - V( 2, 2 );
			BBB( 3 ) = V( 3, 3 ) - V( 2, 3 );

			DOTCB = dot( CCC, BBB );
			if ( DOTCB < 0.0 ) return;
			if ( DOTCB > magnitude_squared( BBB ) ) return;

			//AAA = V1 - V2
			AAA( 1 ) = V( 1, 1 ) - V( 2, 1 );
			AAA( 2 ) = V( 1, 2 ) - V( 2, 2 );
			AAA( 3 ) = V( 1, 3 ) - V( 2, 3 );

			DOTCA = dot( CCC, AAA );
			if ( DOTCA < 0.0 ) return;
			if ( DOTCA > magnitude_squared( AAA ) ) return;
			// Surface is intersected
			IPIERC = 1;

		} else {
			// Surface is not rectangular

			//if (NV == 3) then
			//else ! NV=4
			//endif

			// First two of V & A already set
			// test first vertex:
			C( 1, 1 ) = CPhit( 1 ) - V( 1, 1 );
			C( 1, 2 ) = CPhit( 2 ) - V( 1, 2 );
			C( 1, 3 ) = CPhit( 3 ) - V( 1, 3 );
			AXC( 1 ) = A( 1, 2 ) * C( 1, 3 ) - A( 1, 3 ) * C( 1, 2 );
			AXC( 2 ) = A( 1, 3 ) * C( 1, 1 ) - A( 1, 1 ) * C( 1, 3 );
			AXC( 3 ) = A( 1, 1 ) * C( 1, 2 ) - A( 1, 2 ) * C( 1, 1 );
			if ( dot( AXC, SN ) < 0.0 ) return; // If at least one dot product is negative, intersection outside of surface

			// test second vertex:
			C( 2, 1 ) = CPhit( 1 ) - V( 2, 1 );
			C( 2, 2 ) = CPhit( 2 ) - V( 2, 2 );
			C( 2, 3 ) = CPhit( 3 ) - V( 2, 3 );
			AXC( 1 ) = A( 2, 2 ) * C( 2, 3 ) - A( 2, 3 ) * C( 2, 2 );
			AXC( 2 ) = A( 2, 3 ) * C( 2, 1 ) - A( 2, 1 ) * C( 2, 3 );
			AXC( 3 ) = A( 2, 1 ) * C( 2, 2 ) - A( 2, 2 ) * C( 2, 1 );
			if ( dot( AXC, SN ) < 0.0 ) return; // If at least one dot product is negative, intersection outside of surface

			NV = Surface( ISurf ).Sides;
			// Since first two of V & A already set, start with 3.  (so if NV=3, this loop won't happen)
			for ( N = 3; N <= NV - 1; ++N ) {
				V( N, 1 ) = Surface( ISurf ).Vertex( N ).x;
				V( N, 2 ) = Surface( ISurf ).Vertex( N ).y;
				V( N, 3 ) = Surface( ISurf ).Vertex( N ).z;

				A( N, 1 ) = Surface( ISurf ).Vertex( N + 1 ).x - V( N, 1 );
				A( N, 2 ) = Surface( ISurf ).Vertex( N + 1 ).y - V( N, 2 );
				A( N, 3 ) = Surface( ISurf ).Vertex( N + 1 ).z - V( N, 3 );

				C( N, 1 ) = CPhit( 1 ) - V( N, 1 );
				C( N, 2 ) = CPhit( 2 ) - V( N, 2 );
				C( N, 3 ) = CPhit( 3 ) - V( N, 3 );

				AXC( 1 ) = A( N, 2 ) * C( N, 3 ) - A( N, 3 ) * C( N, 2 );
				AXC( 2 ) = A( N, 3 ) * C( N, 1 ) - A( N, 1 ) * C( N, 3 );
				AXC( 3 ) = A( N, 1 ) * C( N, 2 ) - A( N, 2 ) * C( N, 1 );

				if ( dot( AXC, SN ) < 0.0 ) return; // If at least one dot product is negative, intersection outside of surface

			}

			// Last vertex (NV=3 or NV=4)
			V( NV, 1 ) = Surface( ISurf ).Vertex( NV ).x;
			V( NV, 2 ) = Surface( ISurf ).Vertex( NV ).y;
			V( NV, 3 ) = Surface( ISurf ).Vertex( NV ).z;

			A( NV, 1 ) = V( 1, 1 ) - V( NV, 1 );
			A( NV, 2 ) = V( 1, 2 ) - V( NV, 2 );
			A( NV, 3 ) = V( 1, 3 ) - V( NV, 3 );

			C( NV, 1 ) = CPhit( 1 ) - V( NV, 1 );
			C( NV, 2 ) = CPhit( 2 ) - V( NV, 2 );
			C( NV, 3 ) = CPhit( 3 ) - V( NV, 3 );

			AXC( 1 ) = A( NV, 2 ) * C( NV, 3 ) - A( NV, 3 ) * C( NV, 2 );
			AXC( 2 ) = A( NV, 3 ) * C( NV, 1 ) - A( NV, 1 ) * C( NV, 3 );
			AXC( 3 ) = A( NV, 1 ) * C( NV, 2 ) - A( NV, 2 ) * C( NV, 1 );

			if ( dot( AXC, SN ) < 0.0 ) return; // If at least one dot product is negative, intersection outside of surface

			IPIERC = 1; // Surface is intersected
		}

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // SolarReflectionManager

} // EnergyPlus
