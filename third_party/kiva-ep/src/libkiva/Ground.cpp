/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef Ground_CPP
#define Ground_CPP

#undef PRNTSURF

#include "Ground.hpp"
#include "Errors.hpp"
//#include <unsupported/Eigen/SparseExtra>

namespace Kiva {

static const double PI = 4.0*atan(1.0);

static const bool TDMA = true;

Ground::Ground(Foundation &foundation) : foundation(foundation)
{
  pSolver = std::make_shared<Eigen::BiCGSTAB<Eigen::SparseMatrix<double>, Eigen::IncompleteLUT<double>>>();
}

Ground::Ground(Foundation &foundation, GroundOutput::OutputMap &outputMap)
  : foundation(foundation), groundOutput(outputMap)
{
  pSolver = std::make_shared<Eigen::BiCGSTAB<Eigen::SparseMatrix<double>, Eigen::IncompleteLUT<double>>>();
}

Ground::~Ground()
{
}

void Ground::buildDomain()
{
  // Create mesh
  foundation.createMeshData();

  // Build matrices for PDE term coefficients
  domain.setDomain(foundation);

  nX = domain.meshX.centers.size();
  nY = domain.meshY.centers.size();
  nZ = domain.meshZ.centers.size();

  // Initialize matices
  if (foundation.numericalScheme == Foundation::NS_ADE)
  {
    U.resize(nX,std::vector<std::vector<double> >(nY,std::vector<double>(nZ)));
    UOld.resize(nX,std::vector<std::vector<double> >(nY,std::vector<double>(nZ)));

    V.resize(nX,std::vector<std::vector<double> >(nY,std::vector<double>(nZ)));
    VOld.resize(nX,std::vector<std::vector<double> >(nY,std::vector<double>(nZ)));
  }

  if ((foundation.numericalScheme == Foundation::NS_ADI ||
    foundation.numberOfDimensions == 1) && TDMA)
  {
    a1.resize(nX*nY*nZ, 0.0);
    a2.resize(nX*nY*nZ, 0.0);
    a3.resize(nX*nY*nZ, 0.0);
    b_.resize(nX*nY*nZ, 0.0);
    x_.resize(nX*nY*nZ);
  }

  pSolver->setMaxIterations(foundation.maxIterations);
  pSolver->setTolerance(foundation.tolerance);
  tripletList.reserve(nX*nY*nZ*(1+2*foundation.numberOfDimensions));
  Amat.resize(nX*nY*nZ,nX*nY*nZ);
  b.resize(nX*nY*nZ);
  x.resize(nX*nY*nZ);
  x.fill(283.15);

  TNew.resize(nX,std::vector<std::vector<double> >(nY,std::vector<double>(nZ)));
  TOld.resize(nX,std::vector<std::vector<double> >(nY,std::vector<double>(nZ)));
}

void Ground::calculateADE()
{
  // Set Old values
  for (size_t i = 0; i < nX; ++i)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; ++k)
      {
        UOld[i][j][k] = VOld[i][j][k] = TOld[i][j][k];
      }

    }
  }

  // Solve for new values (Main loop)
  #pragma omp parallel sections num_threads(2)
  {
    #pragma omp section
      calculateADEUpwardSweep();
    #pragma omp section
      calculateADEDownwardSweep();
  }

  for (size_t i = 0; i < nX; ++i)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; ++k)
      {
        // Calculate average of sweeps
        TNew[i][j][k] = 0.5*(U[i][j][k] + V[i][j][k]);

        // Update old values for next timestep
        TOld[i][j][k] = TNew[i][j][k];
      }
    }
  }
}

void Ground::calculateADEUpwardSweep()
{
  // Upward sweep (Solve U Matrix starting from 1, 1)
  for (size_t i = 0; i < nX; i++)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; k++)
      {
        switch (domain.cell[i][j][k].cellType)
        {
        case Cell::BOUNDARY:
          {
          double tilt;
          if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_POS)
            tilt = 0;
          else if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_NEG)
            tilt = PI;
          else
            tilt = PI/2.0;

          switch (domain.cell[i][j][k].surfacePtr->boundaryConditionType)
          {
          case Surface::ZERO_FLUX:
            {
            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              U[i][j][k] = UOld[i+1][j][k];
              break;
            case Surface::X_POS:
              U[i][j][k] = U[i-1][j][k];
              break;
            case Surface::Y_NEG:
              U[i][j][k] = UOld[i][j+1][k];
              break;
            case Surface::Y_POS:
              U[i][j][k] = U[i][j-1][k];
              break;
            case Surface::Z_NEG:
              U[i][j][k] = UOld[i][j][k+1];
              break;
            case Surface::Z_POS:
              U[i][j][k] = U[i][j][k-1];
              break;
            }
            }
            break;

          case Surface::CONSTANT_TEMPERATURE:

            U[i][j][k] = domain.cell[i][j][k].surfacePtr->temperature;
            break;

          case Surface::INTERIOR_TEMPERATURE:

            U[i][j][k] = bcs.indoorTemp;
            break;

          case Surface::EXTERIOR_TEMPERATURE:

            U[i][j][k] = bcs.outdoorTemp;
            break;

          case Surface::INTERIOR_FLUX:
            {
            double& Tair = bcs.indoorTemp;
            double& q = domain.cell[i][j][k].heatGain;

            double hc = getConvectionCoeff(TOld[i][j][k],
                    Tair,0.0,0.00208,false,tilt);  // TODO Make roughness a property of the interior surfaces
            double hr = getSimpleInteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,
                               TOld[i][j][k],Tair);

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              U[i][j][k] = (domain.getKXP(i,j,k)*UOld[i+1][j][k]/domain.getDXP(i) +
                   (hc + hr)*Tair + q)/(domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr));
              break;
            case Surface::X_POS:
              U[i][j][k] = (domain.getKXM(i,j,k)*U[i-1][j][k]/domain.getDXM(i) +
                   (hc + hr)*Tair + q)/(domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr));
              break;
            case Surface::Y_NEG:
              U[i][j][k] = (domain.getKYP(i,j,k)*UOld[i][j+1][k]/domain.getDYP(j) +
                   (hc + hr)*Tair + q)/(domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr));
              break;
            case Surface::Y_POS:
              U[i][j][k] = (domain.getKYM(i,j,k)*U[i][j-1][k]/domain.getDYM(j) +
                   (hc + hr)*Tair + q)/(domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr));
              break;
            case Surface::Z_NEG:
              U[i][j][k] = (domain.getKZP(i,j,k)*UOld[i][j][k+1]/domain.getDZP(k) +
                   (hc + hr)*Tair + q)/(domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr));
              break;
            case Surface::Z_POS:
              U[i][j][k] = (domain.getKZM(i,j,k)*U[i][j][k-1]/domain.getDZM(k) +
                   (hc + hr)*Tair + q)/(domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr));
              break;
            }
            }
            break;

          case Surface::EXTERIOR_FLUX:
            {
            double Tair = bcs.outdoorTemp;
            double v = bcs.localWindSpeed;
            double eSky = bcs.skyEmissivity;
            double F = getEffectiveExteriorViewFactor(eSky,tilt);
            double hc = getConvectionCoeff(TOld[i][j][k],Tair,v,foundation.surfaceRoughness,true,tilt);
            double hr = getExteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,TOld[i][j][k],Tair,eSky,tilt);
            double q = domain.cell[i][j][k].heatGain;

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              U[i][j][k] = (domain.getKXP(i,j,k)*UOld[i+1][j][k]/domain.getDXP(i) +
                    (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr));
              break;
            case Surface::X_POS:
              U[i][j][k] = (domain.getKXM(i,j,k)*U[i-1][j][k]/domain.getDXM(i) +
                    (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr));
              break;
            case Surface::Y_NEG:
              U[i][j][k] = (domain.getKYP(i,j,k)*UOld[i][j+1][k]/domain.getDYP(j) +
                    (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr));
              break;
            case Surface::Y_POS:
              U[i][j][k] = (domain.getKYM(i,j,k)*U[i][j-1][k]/domain.getDYM(j) +
                    (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr));
              break;
            case Surface::Z_NEG:
              U[i][j][k] = (domain.getKZP(i,j,k)*UOld[i][j][k+1]/domain.getDZP(k) +
                    (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr));
              break;
            case Surface::Z_POS:
              U[i][j][k] = (domain.getKZM(i,j,k)*U[i][j][k-1]/domain.getDZM(k) +
                    (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr));
              break;
            }
            }
            break;
          }
          }
          break;
        case Cell::INTERIOR_AIR:
          U[i][j][k] = bcs.indoorTemp;
          break;
        case Cell::EXTERIOR_AIR:
          U[i][j][k] = bcs.outdoorTemp;
          break;
        default:
          {
          double theta = timestep/
            (domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);

          double CXP = domain.cell[i][j][k].cxp*theta;
          double CXM = domain.cell[i][j][k].cxm*theta;
          double CZP = domain.cell[i][j][k].czp*theta;
          double CZM = domain.cell[i][j][k].czm*theta;
          double CYP = domain.cell[i][j][k].cyp*theta;
          double CYM = domain.cell[i][j][k].cym*theta;
          double Q = domain.cell[i][j][k].heatGain*theta;

          if (foundation.numberOfDimensions == 3)
            U[i][j][k] = (UOld[i][j][k]*(1.0 - CXP - CZP - CYP)
                - U[i-1][j][k]*CXM
                + UOld[i+1][j][k]*CXP
                - U[i][j][k-1]*CZM
                + UOld[i][j][k+1]*CZP
                - U[i][j-1][k]*CYM
                + UOld[i][j+1][k]*CYP
                + Q) /
                (1.0 - CXM - CZM - CYM);
          else if (foundation.numberOfDimensions == 2)
          {
            double CXPC = 0;
            double CXMC = 0;

            if (i != 0)
            {
              double r = domain.meshX.centers[i];
              CXPC = domain.cell[i][j][k].cxp_c*theta/r;
              CXMC = domain.cell[i][j][k].cxm_c*theta/r;
            }
            U[i][j][k] = (UOld[i][j][k]*(1.0 - CXPC - CXP - CZP)
                - U[i-1][j][k]*(CXMC + CXM)
                + UOld[i+1][j][k]*(CXPC + CXP)
                - U[i][j][k-1]*CZM
                + UOld[i][j][k+1]*CZP
                + Q) /
                (1.0 - CXMC - CXM - CZM);
          }
          else
          {
            U[i][j][k] = (UOld[i][j][k]*(1.0 - CZP)
                - U[i][j][k-1]*CZM
                + UOld[i][j][k+1]*CZP
                + Q) /
                (1.0 - CZM);
          }
          }
          break;
        }
      }
    }
  }
}

void Ground::calculateADEDownwardSweep()
{
  // Downward sweep (Solve V Matrix starting from I, K)
  for (size_t i = nX - 1; /* i >= 0 && */ i < nX; i--)
  {
    for (size_t j = nY - 1; /* j >= 0 && */ j < nY; j--)
    {
      for (size_t k = nZ - 1; /* k >= 0 && */ k < nZ; k--)
      {
        switch (domain.cell[i][j][k].cellType)
        {
        case Cell::BOUNDARY:
          {
          double tilt;
          if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_POS)
            tilt = 0;
          else if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_NEG)
            tilt = PI;
          else
            tilt = PI/2.0;

          switch (domain.cell[i][j][k].surfacePtr->boundaryConditionType)
          {
          case Surface::ZERO_FLUX:
            {
            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              V[i][j][k] = V[i+1][j][k];
              break;
            case Surface::X_POS:
              V[i][j][k] = VOld[i-1][j][k];
              break;
            case Surface::Y_NEG:
              V[i][j][k] = V[i][j+1][k];
              break;
            case Surface::Y_POS:
              V[i][j][k] = VOld[i][j-1][k];
              break;
            case Surface::Z_NEG:
              V[i][j][k] = V[i][j][k+1];
              break;
            case Surface::Z_POS:
              V[i][j][k] = VOld[i][j][k-1];
              break;
            }
            }
            break;

          case Surface::CONSTANT_TEMPERATURE:

            V[i][j][k] = domain.cell[i][j][k].surfacePtr->temperature;
            break;

          case Surface::INTERIOR_TEMPERATURE:

            V[i][j][k] = bcs.indoorTemp;
            break;

          case Surface::EXTERIOR_TEMPERATURE:

            V[i][j][k] = bcs.outdoorTemp;
            break;

          case Surface::INTERIOR_FLUX:
            {
            double& Tair = bcs.indoorTemp;
            double& q = domain.cell[i][j][k].heatGain;

            double hc = getConvectionCoeff(TOld[i][j][k],
                    Tair,0.0,0.00208,false,tilt);
            double hr = getSimpleInteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,
                               TOld[i][j][k],Tair);

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              V[i][j][k] = (domain.getKXP(i,j,k)*V[i+1][j][k]/domain.getDXP(i) +
                    (hc + hr)*Tair + q)/(domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr));
              break;
            case Surface::X_POS:
              V[i][j][k] = (domain.getKXM(i,j,k)*VOld[i-1][j][k]/domain.getDXM(i) +
                    (hc + hr)*Tair + q)/(domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr));
              break;
            case Surface::Y_NEG:
              V[i][j][k] = (domain.getKYP(i,j,k)*V[i][j+1][k]/domain.getDYP(j) +
                    (hc + hr)*Tair + q)/(domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr));
              break;
            case Surface::Y_POS:
              V[i][j][k] = (domain.getKYM(i,j,k)*VOld[i][j-1][k]/domain.getDYM(j) +
                    (hc + hr)*Tair + q)/(domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr));
              break;
            case Surface::Z_NEG:
              V[i][j][k] = (domain.getKZP(i,j,k)*V[i][j][k+1]/domain.getDZP(k) +
                    (hc + hr)*Tair + q)/(domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr));
              break;
            case Surface::Z_POS:
              V[i][j][k] = (domain.getKZM(i,j,k)*VOld[i][j][k-1]/domain.getDZM(k) +
                    (hc + hr)*Tair + q)/(domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr));
              break;
            }
            }
            break;

          case Surface::EXTERIOR_FLUX:
            {
            double& Tair = bcs.outdoorTemp;
            double& v = bcs.localWindSpeed;
            double& eSky = bcs.skyEmissivity;
            double F = getEffectiveExteriorViewFactor(eSky,tilt);
            double hc = getConvectionCoeff(TOld[i][j][k],Tair,v,foundation.surfaceRoughness,true,tilt);
            double hr = getExteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,TOld[i][j][k],Tair,eSky,tilt);
            double q = domain.cell[i][j][k].heatGain;

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              V[i][j][k] = (domain.getKXP(i,j,k)*V[i+1][j][k]/domain.getDXP(i) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr));
              break;
            case Surface::X_POS:
              V[i][j][k] = (domain.getKXM(i,j,k)*VOld[i-1][j][k]/domain.getDXM(i) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr));
              break;
            case Surface::Y_NEG:
              V[i][j][k] = (domain.getKYP(i,j,k)*V[i][j+1][k]/domain.getDYP(j) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr));
              break;
            case Surface::Y_POS:
              V[i][j][k] = (domain.getKYM(i,j,k)*VOld[i][j-1][k]/domain.getDYM(j) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr));
              break;
            case Surface::Z_NEG:
              V[i][j][k] = (domain.getKZP(i,j,k)*VOld[i][j][k+1]/domain.getDZP(k) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr));
              break;
            case Surface::Z_POS:
              V[i][j][k] = (domain.getKZM(i,j,k)*VOld[i][j][k-1]/domain.getDZM(k) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr));
              break;
            }
            }
            break;
          }
          }
          break;

        case Cell::INTERIOR_AIR:
          V[i][j][k] = bcs.indoorTemp;
          break;
        case Cell::EXTERIOR_AIR:
          V[i][j][k] = bcs.outdoorTemp;
          break;
        default:
          {
          double theta = timestep/
            (domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);

          double CXP = domain.cell[i][j][k].cxp*theta;
          double CXM = domain.cell[i][j][k].cxm*theta;
          double CZP = domain.cell[i][j][k].czp*theta;
          double CZM = domain.cell[i][j][k].czm*theta;
          double CYP = domain.cell[i][j][k].cyp*theta;
          double CYM = domain.cell[i][j][k].cym*theta;
          double Q = domain.cell[i][j][k].heatGain*theta;

          if (foundation.numberOfDimensions == 3)
            V[i][j][k] = (VOld[i][j][k]*(1.0 + CXM + CZM + CYM)
                - VOld[i-1][j][k]*CXM
                + V[i+1][j][k]*CXP
                - VOld[i][j][k-1]*CZM
                + V[i][j][k+1]*CZP
                - VOld[i][j-1][k]*CYM
                + V[i][j+1][k]*CYP
                + Q) /
                (1.0 + CXP + CZP + CYP);
          else if (foundation.numberOfDimensions == 2)
          {
            double CXPC = 0;
            double CXMC = 0;

            if (i != 0)
            {
              double r = domain.meshX.centers[i];
              CXPC = domain.cell[i][j][k].cxp_c*theta/r;
              CXMC = domain.cell[i][j][k].cxm_c*theta/r;
            }
            V[i][j][k] = (VOld[i][j][k]*(1.0 + CXMC + CXM + CZM)
                - VOld[i-1][j][k]*(CXMC + CXM)
                + V[i+1][j][k]*(CXPC + CXP)
                - VOld[i][j][k-1]*CZM
                + V[i][j][k+1]*CZP
                + Q) /
                (1.0 + CXPC + CXP + CZP);
          }
          else
          {
            V[i][j][k] = (VOld[i][j][k]*(1.0 + CZM)
                - VOld[i][j][k-1]*CZM
                + V[i][j][k+1]*CZP
                + Q) /
                (1.0 + CZP);
          }
          }
          break;
        }
      }
    }
  }
}

void Ground::calculateExplicit()
{
  for (size_t i = 0; i < nX; i++)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; k++)
      {
        switch (domain.cell[i][j][k].cellType)
        {
        case Cell::BOUNDARY:
          {
          double tilt;
          if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_POS)
            tilt = 0;
          else if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_NEG)
            tilt = PI;
          else
            tilt = PI/2.0;

          switch (domain.cell[i][j][k].surfacePtr->boundaryConditionType)
          {
          case Surface::ZERO_FLUX:
            {
            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              TNew[i][j][k] = TOld[i+1][j][k];
              break;
            case Surface::X_POS:
              TNew[i][j][k] = TOld[i-1][j][k];
              break;
            case Surface::Y_NEG:
              TNew[i][j][k] = TOld[i][j+1][k];
              break;
            case Surface::Y_POS:
              TNew[i][j][k] = TOld[i][j-1][k];
              break;
            case Surface::Z_NEG:
              TNew[i][j][k] = TOld[i][j][k+1];
              break;
            case Surface::Z_POS:
              TNew[i][j][k] = TOld[i][j][k-1];
              break;
            }
            }
            break;

          case Surface::CONSTANT_TEMPERATURE:

            TNew[i][j][k] = domain.cell[i][j][k].surfacePtr->temperature;
            break;

          case Surface::INTERIOR_TEMPERATURE:

            TNew[i][j][k] = bcs.indoorTemp;
            break;

          case Surface::EXTERIOR_TEMPERATURE:

            TNew[i][j][k] = bcs.outdoorTemp;
            break;

          case Surface::INTERIOR_FLUX:
            {
            double& Tair = bcs.indoorTemp;
            double& q = domain.cell[i][j][k].heatGain;

            double hc = getConvectionCoeff(TOld[i][j][k],
                    Tair,0.0,0.00208,false,tilt);
            double hr = getSimpleInteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,
                               TOld[i][j][k],Tair);

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              TNew[i][j][k] = (domain.getKXP(i,j,k)*TOld[i+1][j][k]/domain.getDXP(i) +
                    (hc + hr)*Tair + q)/(domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr));
              break;
            case Surface::X_POS:
              TNew[i][j][k] = (domain.getKXM(i,j,k)*TOld[i-1][j][k]/domain.getDXM(i) +
                    (hc + hr)*Tair + q)/(domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr));
              break;
            case Surface::Y_NEG:
              TNew[i][j][k] = (domain.getKYP(i,j,k)*TOld[i][j+1][k]/domain.getDYP(j) +
                    (hc + hr)*Tair + q)/(domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr));
              break;
            case Surface::Y_POS:
              TNew[i][j][k] = (domain.getKYM(i,j,k)*TOld[i][j-1][k]/domain.getDYM(j) +
                    (hc + hr)*Tair + q)/(domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr));
              break;
            case Surface::Z_NEG:
              TNew[i][j][k] = (domain.getKZP(i,j,k)*TOld[i][j][k+1]/domain.getDZP(k) +
                    (hc + hr)*Tair + q)/(domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr));
              break;
            case Surface::Z_POS:
              TNew[i][j][k] = (domain.getKZM(i,j,k)*TOld[i][j][k-1]/domain.getDZM(k) +
                    (hc + hr)*Tair + q)/(domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr));
              break;
            }
            }
            break;

          case Surface::EXTERIOR_FLUX:
            {
            double& Tair = bcs.outdoorTemp;
            double& v = bcs.localWindSpeed;
            double& eSky = bcs.skyEmissivity;
            double F = getEffectiveExteriorViewFactor(eSky,tilt);
            double hc = getConvectionCoeff(TOld[i][j][k],Tair,v,foundation.surfaceRoughness,true,tilt);
            double hr = getExteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,TOld[i][j][k],Tair,eSky,tilt);
            double q = domain.cell[i][j][k].heatGain;

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              TNew[i][j][k] = (domain.getKXP(i,j,k)*TOld[i+1][j][k]/domain.getDXP(i) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr));
              break;
            case Surface::X_POS:
              TNew[i][j][k] = (domain.getKXM(i,j,k)*TOld[i-1][j][k]/domain.getDXM(i) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr));
              break;
            case Surface::Y_NEG:
              TNew[i][j][k] = (domain.getKYP(i,j,k)*TOld[i][j+1][k]/domain.getDYP(j) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr));
              break;
            case Surface::Y_POS:
              TNew[i][j][k] = (domain.getKYM(i,j,k)*TOld[i][j-1][k]/domain.getDYM(j) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr));
              break;
            case Surface::Z_NEG:
              TNew[i][j][k] = (domain.getKZP(i,j,k)*TOld[i][j][k+1]/domain.getDZP(k) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr));
              break;
            case Surface::Z_POS:
              TNew[i][j][k] = (domain.getKZM(i,j,k)*TOld[i][j][k-1]/domain.getDZM(k) +
                  (hc + hr*pow(F,0.25))*Tair + q)/(domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr));
              break;
            }
            }
            break;
          }
          }
          break;
        case Cell::INTERIOR_AIR:
          TNew[i][j][k] = bcs.indoorTemp;
          break;

        case Cell::EXTERIOR_AIR:
          TNew[i][j][k] = bcs.outdoorTemp;
          break;
        default:
          {
          double theta = timestep/
            (domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);

          double CXP = domain.cell[i][j][k].cxp*theta;
          double CXM = domain.cell[i][j][k].cxm*theta;
          double CZP = domain.cell[i][j][k].czp*theta;
          double CZM = domain.cell[i][j][k].czm*theta;
          double CYP = domain.cell[i][j][k].cyp*theta;
          double CYM = domain.cell[i][j][k].cym*theta;
          double Q = domain.cell[i][j][k].heatGain*theta;

          if (foundation.numberOfDimensions == 3)
            TNew[i][j][k] = TOld[i][j][k]*(1.0 + CXM + CZM + CYM - CXP - CZP - CYP)
                - TOld[i-1][j][k]*CXM
                + TOld[i+1][j][k]*CXP
                - TOld[i][j][k-1]*CZM
                + TOld[i][j][k+1]*CZP
                - TOld[i][j-1][k]*CYM
                + TOld[i][j+1][k]*CYP
                + Q;
          else if (foundation.numberOfDimensions == 2)
          {
            double CXPC = 0;
            double CXMC = 0;

            if (i != 0)
            {
              double r = domain.meshX.centers[i];
              CXPC = domain.cell[i][j][k].cxp_c*theta/r;
              CXMC = domain.cell[i][j][k].cxm_c*theta/r;
            }

            TNew[i][j][k] = TOld[i][j][k]*(1.0 + CXMC + CXM + CZM - CXPC - CXP - CZP)
                - TOld[i-1][j][k]*(CXMC + CXM)
                + TOld[i+1][j][k]*(CXPC + CXP)
                - TOld[i][j][k-1]*CZM
                + TOld[i][j][k+1]*CZP
                + Q;
          }
          else
          {
            TNew[i][j][k] = TOld[i][j][k]*(1.0 + CZM - CZP)
                - TOld[i][j][k-1]*CZM
                + TOld[i][j][k+1]*CZP
                + Q;
          }
          }
          break;
        }
      }
    }
  }
  for (size_t i = 0; i < nX; ++i)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; ++k)
      {
        // Update old values for next timestep
        TOld[i][j][k] = TNew[i][j][k];
      }
    }
  }
}

void Ground::calculateMatrix(Foundation::NumericalScheme scheme)
{
  for (size_t i = 0; i < nX; i++)
  {
    for (size_t j = 0; j < nY; j++)
    {
      for (size_t k = 0; k < nZ; k++)
      {
        int index = i + nX*j + nX*nY*k;
        int index_ip = (i+1) + nX*j + nX*nY*k;
        int index_im = (i-1) + nX*j + nX*nY*k;
        int index_jp = i + nX*(j+1) + nX*nY*k;
        int index_jm = i + nX*(j-1) + nX*nY*k;
        int index_kp = i + nX*j + nX*nY*(k+1);
        int index_km = i + nX*j + nX*nY*(k-1);

        double A, Aip, Aim, Ajp, Ajm, Akp, Akm, bVal = 0.0;

        switch (domain.cell[i][j][k].cellType)
        {
        case Cell::BOUNDARY:
          {
          double tilt;
          if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_POS)
            tilt = 0;
          else if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_NEG)
            tilt = PI;
          else
            tilt = PI/2.0;

          switch (domain.cell[i][j][k].surfacePtr->boundaryConditionType)
          {
          case Surface::ZERO_FLUX:
            {
            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              A = 1.0;
              Aip = -1.0;
              bVal = 0.0;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setbValue(index,bVal);
              break;
            case Surface::X_POS:
              A = 1.0;
              Aim = -1.0;
              bVal = 0.0;

              setAmatValue(index,index,A);
              setAmatValue(index,index_im,Aim);
              setbValue(index,bVal);
              break;
            case Surface::Y_NEG:
              A = 1.0;
              Ajp = -1.0;
              bVal = 0.0;

              setAmatValue(index,index,A);
              setAmatValue(index,index_jp,Ajp);
              setbValue(index,bVal);
              break;
            case Surface::Y_POS:
              A = 1.0;
              Ajm = -1.0;
              bVal = 0.0;

              setAmatValue(index,index,A);
              setAmatValue(index,index_jm,Ajm);
              setbValue(index,bVal);
              break;
            case Surface::Z_NEG:
              A = 1.0;
              Akp = -1.0;
              bVal = 0.0;

              setAmatValue(index,index,A);
              setAmatValue(index,index_kp,Akp);
              setbValue(index,bVal);
              break;
            case Surface::Z_POS:
              A = 1.0;
              Akm = -1.0;
              bVal = 0.0;

              setAmatValue(index,index,A);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
              break;
            }
            }
            break;
          case Surface::CONSTANT_TEMPERATURE:
            A = 1.0;
            bVal = domain.cell[i][j][k].surfacePtr->temperature;

            setAmatValue(index,index,A);
            setbValue(index,bVal);
            break;
          case Surface::INTERIOR_TEMPERATURE:
            A = 1.0;
            bVal = bcs.indoorTemp;

            setAmatValue(index,index,A);
            setbValue(index,bVal);
            break;
          case Surface::EXTERIOR_TEMPERATURE:
            A = 1.0;
            bVal = bcs.outdoorTemp;

            setAmatValue(index,index,A);
            setbValue(index,bVal);
            break;
          case Surface::INTERIOR_FLUX:
            {
            double& Tair = bcs.indoorTemp;
            double& q = domain.cell[i][j][k].heatGain;

            double hc = getConvectionCoeff(TOld[i][j][k],
                    Tair,0.0,0.00208,false,tilt);
            double hr = getSimpleInteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,
                               TOld[i][j][k],Tair);

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              A = domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr);
              Aip = -domain.getKXP(i,j,k)/domain.getDXP(i);
              bVal = (hc + hr)*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setbValue(index,bVal);
              break;
            case Surface::X_POS:
              A = domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr);
              Aim = -domain.getKXM(i,j,k)/domain.getDXM(i);
              bVal = (hc + hr)*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_im,Aim);
              setbValue(index,bVal);
              break;
            case Surface::Y_NEG:
              A = domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr);
              Ajp = -domain.getKYP(i,j,k)/domain.getDYP(j);
              bVal = (hc + hr)*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_jp,Ajp);
              setbValue(index,bVal);
              break;
            case Surface::Y_POS:
              A = domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr);
              Ajm = -domain.getKYM(i,j,k)/domain.getDYM(j);
              bVal = (hc + hr)*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_jm,Ajm);
              setbValue(index,bVal);
              break;
            case Surface::Z_NEG:
              A = domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr);
              Akp = -domain.getKZP(i,j,k)/domain.getDZP(k);
              bVal = (hc + hr)*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_kp,Akp);
              setbValue(index,bVal);
              break;
            case Surface::Z_POS:
              A = domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr);
              Akm = -domain.getKZM(i,j,k)/domain.getDZM(k);
              bVal = (hc + hr)*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
              break;
            }
            }
            break;

          case Surface::EXTERIOR_FLUX:
            {
            double& Tair = bcs.outdoorTemp;
            double& v = bcs.localWindSpeed;
            double& eSky = bcs.skyEmissivity;
            double F = getEffectiveExteriorViewFactor(eSky,tilt);
            double hc = getConvectionCoeff(TOld[i][j][k],Tair,v,foundation.surfaceRoughness,true,tilt);
            double hr = getExteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,TOld[i][j][k],Tair,eSky,tilt);
            double q = domain.cell[i][j][k].heatGain;

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              A = domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr);
              Aip = -domain.getKXP(i,j,k)/domain.getDXP(i);
              bVal = (hc + hr*pow(F,0.25))*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setbValue(index,bVal);
              break;
            case Surface::X_POS:
              A = domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr);
              Aim = -domain.getKXM(i,j,k)/domain.getDXM(i);
              bVal = (hc + hr*pow(F,0.25))*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_im,Aim);
              setbValue(index,bVal);
              break;
            case Surface::Y_NEG:
              A = domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr);
              Ajp = -domain.getKYP(i,j,k)/domain.getDYP(j);
              bVal = (hc + hr*pow(F,0.25))*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_jp,Ajp);
              setbValue(index,bVal);
              break;
            case Surface::Y_POS:
              A = domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr);
              Ajm = -domain.getKYM(i,j,k)/domain.getDYM(j);
              bVal = (hc + hr*pow(F,0.25))*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_jm,Ajm);
              setbValue(index,bVal);
              break;
            case Surface::Z_NEG:
              A = domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr);
              Akp = -domain.getKZP(i,j,k)/domain.getDZP(k);
              bVal = (hc + hr*pow(F,0.25))*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_kp,Akp);
              setbValue(index,bVal);
              break;
            case Surface::Z_POS:
              A = domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr);
              Akm = -domain.getKZM(i,j,k)/domain.getDZM(k);
              bVal = (hc + hr*pow(F,0.25))*Tair + q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
              break;
            }
            }
            break;
          }
          }
          break;
        case Cell::INTERIOR_AIR:
          A = 1.0;
          bVal = bcs.indoorTemp;

          setAmatValue(index,index,A);
          setbValue(index,bVal);
          break;
        case Cell::EXTERIOR_AIR:
          A = 1.0;
          bVal = bcs.outdoorTemp;

          setAmatValue(index,index,A);
          setbValue(index,bVal);
          break;
        default:
          {
          if (scheme == Foundation::NS_STEADY_STATE)
          {
            double CXP = domain.cell[i][j][k].cxp;
            double CXM = domain.cell[i][j][k].cxm;
            double CZP = domain.cell[i][j][k].czp;
            double CZM = domain.cell[i][j][k].czm;
            double CYP = domain.cell[i][j][k].cyp;
            double CYM = domain.cell[i][j][k].cym;
            double Q = domain.cell[i][j][k].heatGain;

            if (foundation.numberOfDimensions == 3)
            {
              A = (CXM + CZM + CYM - CXP - CZP - CYP);
              Aim = -CXM;
              Aip = CXP;
              Akm = -CZM;
              Akp = CZP;
              Ajm = -CYM;
              Ajp = CYP;

              bVal = -Q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setAmatValue(index,index_im,Aim);
              setAmatValue(index,index_jp,Ajp);
              setAmatValue(index,index_jm,Ajm);
              setAmatValue(index,index_kp,Akp);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
            }
            else if (foundation.numberOfDimensions == 2)
            {
              double CXPC = 0;
              double CXMC = 0;

              if (i != 0)
              {
                double r = domain.meshX.centers[i];
                CXPC = domain.cell[i][j][k].cxp_c/r;
                CXMC = domain.cell[i][j][k].cxm_c/r;
              }
              A = (CXMC + CXM + CZM - CXPC - CXP - CZP);
              Aim = (-CXMC - CXM);
              Aip = (CXPC + CXP);
              Akm = -CZM;
              Akp = CZP;

              bVal = -Q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setAmatValue(index,index_im,Aim);
              setAmatValue(index,index_kp,Akp);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
            }
            else
            {
              A = (CZM - CZP);
              Akm = -CZM;
              Akp = CZP;

              bVal = -Q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_kp,Akp);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
            }
          }
          else
          {
            double theta = timestep/
              (domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);

            double f;
            if (scheme == Foundation::NS_IMPLICIT)
              f = 1.0;
            else
              f = 0.5;

            double CXP = domain.cell[i][j][k].cxp*theta;
            double CXM = domain.cell[i][j][k].cxm*theta;
            double CZP = domain.cell[i][j][k].czp*theta;
            double CZM = domain.cell[i][j][k].czm*theta;
            double CYP = domain.cell[i][j][k].cyp*theta;
            double CYM = domain.cell[i][j][k].cym*theta;
            double Q = domain.cell[i][j][k].heatGain*theta;

            if (foundation.numberOfDimensions == 3)
            {
              A = (1.0 + f*(CXP + CZP + CYP - CXM - CZM - CYM));
              Aim = f*CXM;
              Aip = f*(-CXP);
              Akm = f*CZM;
              Akp = f*(-CZP);
              Ajm = f*CYM;
              Ajp = f*(-CYP);

              bVal = TOld[i][j][k]*(1.0 + (1-f)*(CXM + CZM + CYM - CXP - CZP - CYP))
                 - TOld[i-1][j][k]*(1-f)*CXM
                 + TOld[i+1][j][k]*(1-f)*CXP
                 - TOld[i][j][k-1]*(1-f)*CZM
                 + TOld[i][j][k+1]*(1-f)*CZP
                 - TOld[i][j-1][k]*(1-f)*CYM
                 + TOld[i][j+1][k]*(1-f)*CYP
                 + Q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setAmatValue(index,index_im,Aim);
              setAmatValue(index,index_jp,Ajp);
              setAmatValue(index,index_jm,Ajm);
              setAmatValue(index,index_kp,Akp);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
            }
            else if (foundation.numberOfDimensions == 2)
            {
              double CXPC = 0;
              double CXMC = 0;

              if (i != 0)
              {
                double r = domain.meshX.centers[i];
                CXPC = domain.cell[i][j][k].cxp_c*theta/r;
                CXMC = domain.cell[i][j][k].cxm_c*theta/r;
              }
              A = (1.0 + f*(CXPC + CXP + CZP - CXMC - CXM - CZM));
              Aim = f*(CXMC + CXM);
              Aip = f*(-CXPC - CXP);
              Akm = f*CZM;
              Akp = f*(-CZP);

              bVal = TOld[i][j][k]*(1.0 + (1-f)*(CXMC + CXM + CZM - CXPC - CXP - CZP))
                 - TOld[i-1][j][k]*(1-f)*(CXMC + CXM)
                 + TOld[i+1][j][k]*(1-f)*(CXPC + CXP)
                 - TOld[i][j][k-1]*(1-f)*CZM
                 + TOld[i][j][k+1]*(1-f)*CZP
                 + Q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_ip,Aip);
              setAmatValue(index,index_im,Aim);
              setAmatValue(index,index_kp,Akp);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
            }
            else
            {
              A = (1.0 + f*(CZP - CZM));
              Akm = f*CZM;
              Akp = f*(-CZP);

              bVal = TOld[i][j][k]*(1.0 + (1-f)*(CZM - CZP))
                 - TOld[i][j][k-1]*(1-f)*CZM
                 + TOld[i][j][k+1]*(1-f)*CZP
                 + Q;

              setAmatValue(index,index,A);
              setAmatValue(index,index_kp,Akp);
              setAmatValue(index,index_km,Akm);
              setbValue(index,bVal);
            }
          }
          }
          break;
        }
      }
    }
  }

  solveLinearSystem();

  for (size_t i = 0; i < nX; ++i)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; ++k)
      {
        int index = i + nX*j + nX*nY*k;
        // Read solution into temperature matrix
        TNew[i][j][k] = getxValue(index);

        // Update old values for next timestep
        TOld[i][j][k] = TNew[i][j][k];
      }
    }
  }

  clearAmat();
}

void Ground::calculateADI(int dim)
{
  for (size_t i = 0; i < nX; i++)
  {
    for (size_t j = 0; j < nY; j++)
    {
      for (size_t k = 0; k < nZ; k++)
      {

        int index;
        if (dim == 1)
          index = i + nX*j + nX*nY*k;
        else if (dim == 2)
          index = j + nY*i + nY*nX*k;
        else //if (dim == 3)
          index = k + nZ*i + nZ*nX*j;

        double A, Ap, Am, bVal = 0.0;


        switch (domain.cell[i][j][k].cellType)
        {
        case Cell::BOUNDARY:
          {
          double tilt;
          if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_POS)
            tilt = 0;
          else if (domain.cell[i][j][k].surfacePtr->orientation == Surface::Z_NEG)
            tilt = PI;
          else
            tilt = PI/2.0;

          switch (domain.cell[i][j][k].surfacePtr->boundaryConditionType)
          {
          case Surface::ZERO_FLUX:
            {
            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              A = 1.0;

              if (dim == 1)
              {
                Ap = -1.0;
                bVal = 0;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i+1][j][k];
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::X_POS:
              A = 1.0;
              if (dim == 1)
              {
                Am = -1.0;
                bVal = 0;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i-1][j][k];
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            case Surface::Y_NEG:
              A = 1.0;
              if (dim == 2)
              {
                Ap = -1.0;
                bVal = 0;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i][j+1][k];
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::Y_POS:
              A = 1.0;
              if (dim == 2)
              {
                Am = -1.0;
                bVal = 0;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i][j-1][k];
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            case Surface::Z_NEG:
              A = 1.0;
              if (dim == 3)
              {
                Ap = -1.0;
                bVal = 0;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i][j][k+1];
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::Z_POS:
              A = 1.0;
              if (dim == 3)
              {
                Am = -1.0;
                bVal = 0;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i][j][k-1];
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            }
            }
            break;
          case Surface::CONSTANT_TEMPERATURE:
            A = 1.0;
            bVal = domain.cell[i][j][k].surfacePtr->temperature;

            setAmatValue(index,index,A);
            setbValue(index,bVal);
            break;
          case Surface::INTERIOR_TEMPERATURE:
            A = 1.0;
            bVal = bcs.indoorTemp;

            setAmatValue(index,index,A);
            setbValue(index,bVal);
            break;
          case Surface::EXTERIOR_TEMPERATURE:
            A = 1.0;
            bVal = bcs.outdoorTemp;

            setAmatValue(index,index,A);
            setbValue(index,bVal);
            break;
          case Surface::INTERIOR_FLUX:
            {
            double& Tair = bcs.indoorTemp;
            double& q = domain.cell[i][j][k].heatGain;

            double hc = getConvectionCoeff(TOld[i][j][k],
                    Tair,0.0,0.00208,false,tilt);
            double hr = getSimpleInteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,
                               TOld[i][j][k],Tair);

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              A = domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr);
              if (dim == 1)
              {
                Ap = -domain.getKXP(i,j,k)/domain.getDXP(i);
                bVal = (hc + hr)*Tair + q;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i+1][j][k]*domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr)*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::X_POS:
              A = domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr);
              if (dim == 1)
              {
                Am = -domain.getKXM(i,j,k)/domain.getDXM(i);
                bVal = (hc + hr)*Tair + q;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i-1][j][k]*domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr)*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            case Surface::Y_NEG:
              A = domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr);
              if (dim == 2)
              {
                Ap = -domain.getKYP(i,j,k)/domain.getDYP(j);
                bVal = (hc + hr)*Tair + q;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i][j+1][k]*domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr)*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::Y_POS:
              A = domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr);
              if (dim == 2)
              {
                Am = -domain.getKYM(i,j,k)/domain.getDYM(j);
                bVal = (hc + hr)*Tair + q;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i][j-1][k]*domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr)*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            case Surface::Z_NEG:
              A = domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr);
              if (dim == 3)
              {
                Ap = -domain.getKZP(i,j,k)/domain.getDZP(k);
                bVal = (hc + hr)*Tair + q;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i][j][k+1]*domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr)*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::Z_POS:
              A = domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr);
              if (dim == 3)
              {
                Am = -domain.getKZM(i,j,k)/domain.getDZM(k);
                bVal = (hc + hr)*Tair + q;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i][j][k-1]*domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr)*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            }
            }
            break;

          case Surface::EXTERIOR_FLUX:
            {
            double Tair = bcs.outdoorTemp;
            double v = bcs.localWindSpeed;
            double eSky = bcs.skyEmissivity;
            double F = getEffectiveExteriorViewFactor(eSky,tilt);
            double hc = getConvectionCoeff(TOld[i][j][k],Tair,v,foundation.surfaceRoughness,true,tilt);
            double hr = getExteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,TOld[i][j][k],Tair,eSky,tilt);
            double q = domain.cell[i][j][k].heatGain;

            switch (domain.cell[i][j][k].surfacePtr->orientation)
            {
            case Surface::X_NEG:
              A = domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr);
              if (dim == 1)
              {
                Ap = -domain.getKXP(i,j,k)/domain.getDXP(i);
                bVal = (hc + hr*pow(F,0.25))*Tair + q;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i+1][j][k]*domain.getKXP(i,j,k)/domain.getDXP(i) + (hc + hr*pow(F,0.25))*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::X_POS:
              A = domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr);
              if (dim == 1)
              {
                Am = -domain.getKXM(i,j,k)/domain.getDXM(i);
                bVal = (hc + hr*pow(F,0.25))*Tair + q;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i-1][j][k]*domain.getKXM(i,j,k)/domain.getDXM(i) + (hc + hr*pow(F,0.25))*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            case Surface::Y_NEG:
              A = domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr);
              if (dim == 2)
              {
                Ap = -domain.getKYP(i,j,k)/domain.getDYP(j);
                bVal = (hc + hr*pow(F,0.25))*Tair + q;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i][j+1][k]*domain.getKYP(i,j,k)/domain.getDYP(j) + (hc + hr*pow(F,0.25))*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::Y_POS:
              A = domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr);
              if (dim == 2)
              {
                Am = -domain.getKYM(i,j,k)/domain.getDYM(j);
                bVal = (hc + hr*pow(F,0.25))*Tair + q;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i][j-1][k]*domain.getKYM(i,j,k)/domain.getDYM(j) + (hc + hr*pow(F,0.25))*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            case Surface::Z_NEG:
              A = domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr);
              if (dim == 3)
              {
                Ap = -domain.getKZP(i,j,k)/domain.getDZP(k);
                bVal = (hc + hr*pow(F,0.25))*Tair + q;
              }
              else
              {
                Ap = 0.0;
                bVal = TOld[i][j][k+1]*domain.getKZP(i,j,k)/domain.getDZP(k) + (hc + hr*pow(F,0.25))*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index+1,Ap);
              setbValue(index,bVal);
              break;
            case Surface::Z_POS:
              A = domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr);
              if (dim == 3)
              {
                Am = -domain.getKZM(i,j,k)/domain.getDZM(k);
                bVal = (hc + hr*pow(F,0.25))*Tair + q;
              }
              else
              {
                Am = 0.0;
                bVal = TOld[i][j][k-1]*domain.getKZM(i,j,k)/domain.getDZM(k) + (hc + hr*pow(F,0.25))*Tair + q;
              }

              setAmatValue(index,index,A);
              setAmatValue(index,index-1,Am);
              setbValue(index,bVal);
              break;
            }
            }
            break;
          }
          }
          break;
        case Cell::INTERIOR_AIR:
          A = 1.0;
          bVal = bcs.indoorTemp;

          setAmatValue(index,index,A);
          setbValue(index,bVal);
          break;
        case Cell::EXTERIOR_AIR:
          A = 1.0;
          bVal = bcs.outdoorTemp;

          setAmatValue(index,index,A);
          setbValue(index,bVal);
          break;
        default:
          {
          double theta;
          if (foundation.numberOfDimensions == 3)
          {
            theta = timestep/
                (3*domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);
          }
          else if (foundation.numberOfDimensions == 2)
          {
            theta = timestep/
                (2*domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);
          }
          else
          {
            theta = timestep/
                (domain.cell[i][j][k].density*domain.cell[i][j][k].specificHeat);
          }

          double CXP = domain.cell[i][j][k].cxp*theta;
          double CXM = domain.cell[i][j][k].cxm*theta;
          double CZP = domain.cell[i][j][k].czp*theta;
          double CZM = domain.cell[i][j][k].czm*theta;
          double CYP = domain.cell[i][j][k].cyp*theta;
          double CYM = domain.cell[i][j][k].cym*theta;
          double Q = domain.cell[i][j][k].heatGain*theta;

          double f = foundation.fADI;

          if (foundation.numberOfDimensions == 3)
          {
            if (dim == 1) // x
            {
              A = 1.0 + (3 - 2*f)*(CXP - CXM);
              Am = (3 - 2*f)*CXM;
              Ap = (3 - 2*f)*(-CXP);

              bVal = TOld[i][j][k]*(1.0 + f*(CZM + CYM - CZP - CYP))
                   - TOld[i][j][k-1]*f*CZM
                   + TOld[i][j][k+1]*f*CZP
                   - TOld[i][j-1][k]*f*CYM
                   + TOld[i][j+1][k]*f*CYP
                   + Q;
            }
            else if (dim == 2) // y
            {
              A = (1.0 + (3 - 2*f)*(CYP - CYM));
              Am = (3 - 2*f)*CYM;
              Ap = (3 - 2*f)*(-CYP);

              bVal = TOld[i][j][k]*(1.0 + f*(CXM + CZM - CXP - CZP))
                   - TOld[i-1][j][k]*f*CXM
                   + TOld[i+1][j][k]*f*CXP
                   - TOld[i][j][k-1]*f*CZM
                   + TOld[i][j][k+1]*f*CZP
                   + Q;
            }
            else //if (dim == 3) // z
            {
              A = (1.0 + (3 - 2*f)*(CZP - CZM));
              Am = (3 - 2*f)*CZM;
              Ap = (3 - 2*f)*(-CZP);

              bVal = TOld[i][j][k]*(1.0 + f*(CXM + CYM - CXP - CYP))
                   - TOld[i-1][j][k]*f*CXM
                   + TOld[i+1][j][k]*f*CXP
                   - TOld[i][j-1][k]*f*CYM
                   + TOld[i][j+1][k]*f*CYP
                   + Q;
            }

          }
          else if (foundation.numberOfDimensions == 2)
          {
            double CXPC = 0;
            double CXMC = 0;
            if (i != 0)
            {
              double r = domain.meshX.centers[i];
              CXPC = domain.cell[i][j][k].cxp_c*theta/r;
              CXMC = domain.cell[i][j][k].cxm_c*theta/r;
            }
            if (dim == 1) // x
            {
              A = 1.0 + (2 - f)*(CXPC + CXP - CXMC - CXM);
              Am = (2 - f)*(CXMC + CXM);
              Ap = (2 - f)*(-CXPC - CXP);

              bVal = TOld[i][j][k]*(1.0 + f*(CZM - CZP))
                   - TOld[i][j][k-1]*f*CZM
                   + TOld[i][j][k+1]*f*CZP
                   + Q;
            }
            else //if (dim == 3) // z
            {
              A = 1.0 + (2 - f)*(CZP - CZM);
              Am = (2 - f)*CZM;
              Ap = (2 - f)*(-CZP);

              bVal = TOld[i][j][k]*(1.0 + f*(CXMC + CXM - CXPC - CXP))
                   - TOld[i-1][j][k]*f*(CXMC + CXM)
                   + TOld[i+1][j][k]*f*(CXPC + CXP)
                   + Q;
            }
          }
          else
          {
            A = 1.0 + CZP - CZM;
            Am = CZM;
            Ap = -CZP;

            bVal = TOld[i][j][k] + Q;
          }

          setAmatValue(index,index,A);
          setAmatValue(index,index-1,Am);
          setAmatValue(index,index+1,Ap);
          setbValue(index,bVal);

          }
          break;
        }
      }
    }
  }

  solveLinearSystem();

  for (size_t i = 0; i < nX; ++i)
  {
    for (size_t j = 0; j < nY; ++j)
    {
      for (size_t k = 0; k < nZ; ++k)
      {
        int index;
        if (dim == 1)
          index = i + nX*j + nX*nY*k;
        else if (dim == 2)
          index = j + nY*i + nY*nX*k;
        else //if (dim == 3)
          index = k + nZ*i + nZ*nX*j;

        // Read solution into temperature matrix
        TNew[i][j][k] = getxValue(index);
        // Update old values for next timestep
        TOld[i][j][k] = TNew[i][j][k];
      }
    }
  }

  clearAmat();
}

void Ground::calculate(BoundaryConditions& boundaryConidtions, double ts)
{
  bcs = boundaryConidtions;
  timestep = ts;
  // update boundary conditions
  setSolarBoundaryConditions();
  setInteriorRadiationBoundaryConditions();

  // Calculate Temperatures
  switch(foundation.numericalScheme)
  {
  case Foundation::NS_ADE:
    calculateADE();
    break;
  case Foundation::NS_EXPLICIT:
    calculateExplicit();
    break;
  case Foundation::NS_ADI:
    {
    if (foundation.numberOfDimensions > 1)
      calculateADI(1);
    if (foundation.numberOfDimensions == 3)
      calculateADI(2);
    calculateADI(3);
    }
    break;
  case Foundation::NS_IMPLICIT:
    calculateMatrix(Foundation::NS_IMPLICIT);
    break;
  case Foundation::NS_CRANK_NICOLSON:
    calculateMatrix(Foundation::NS_CRANK_NICOLSON);
    break;
  case Foundation::NS_STEADY_STATE:
    calculateMatrix(Foundation::NS_STEADY_STATE);
    break;
  }

}

void Ground::setAmatValue(const int i,const int j,const double val)
{
  if ((foundation.numericalScheme == Foundation::NS_ADI ||
    foundation.numberOfDimensions == 1) && TDMA)
  {
    if (j < i)
      a1[i] = val;
    else if (j == i)
      a2[i] = val;
    else
      a3[i] = val;
  }
  else
  {
    tripletList.emplace_back(i,j,val);
  }
}

void Ground::setbValue(const int i,const double val)
{
  if ((foundation.numericalScheme == Foundation::NS_ADI ||
    foundation.numberOfDimensions == 1) && TDMA)
  {
    b_[i] = val;
  }
  else
  {
    b(i) = val;
  }
}

void Ground::solveLinearSystem()
{
  if ((foundation.numericalScheme == Foundation::NS_ADI ||
    foundation.numberOfDimensions == 1) && TDMA)
  {
    solveTDM(a1,a2,a3,b_,x_);
  }
  else
  {
    int iters;
    double residual;

    bool success;

    Amat.setFromTriplets(tripletList.begin(), tripletList.end());
    pSolver->compute(Amat);
    x = pSolver->solveWithGuess(b,x);
    int status = pSolver->info();

//    Eigen::saveMarket(Amat, "Amat.mtx");
//    Eigen::saveMarketVector(b, "b.mtx");
    success = status == Eigen::Success;
    if (!success) {
      iters = pSolver->iterations();
      residual = pSolver->error();

      std::stringstream ss;
      ss << "Solution did not converge after " << iters << " iterations. The final residual was: (" << residual << ").";
      showMessage(MSG_ERR, ss.str());
    }
  }
}

void Ground::clearAmat()
{
  if ((foundation.numericalScheme == Foundation::NS_ADI ||
    foundation.numberOfDimensions == 1) && TDMA)
  {
    std::fill(a1.begin(), a1.end(), 0.0);
    std::fill(a2.begin(), a2.end(), 0.0);
    std::fill(a3.begin(), a3.end(), 0.0);
    std::fill(b_.begin(), b_.end(), 0.0);

  }
  else
  {
    tripletList.clear();
    tripletList.reserve(nX*nY*nZ*(1+2*foundation.numberOfDimensions));
  }
}

double Ground::getxValue(const int i)
{
  if ((foundation.numericalScheme == Foundation::NS_ADI ||
    foundation.numberOfDimensions == 1) && TDMA)
  {
    return x_[i];
  }
  else
  {
    return x(i);
  }
}

double Ground::getConvectionCoeff(double Tsurf,
                  double Tamb,
                  double Vair,
                    double roughness,
                  bool isExterior,
                  double tilt)
{
  if (foundation.convectionCalculationMethod == Foundation::CCM_AUTO)
    return getDOE2ConvectionCoeff(tilt,0.0,0.0,Tsurf,Tamb,Vair,roughness);
  else //if (foundation.convectionCalculationMethod == Foundation::CCM_CONSTANT_COEFFICIENT)
  {
    if (isExterior)
      return foundation.exteriorConvectiveCoefficient;
    else
      return foundation.interiorConvectiveCoefficient;
  }
}

double Ground::getSurfaceArea(Surface::SurfaceType surfaceType)
{
  double totalArea = 0;

  // Find surface(s)
  for (size_t s = 0; s < foundation.surfaces.size(); s++)
  {
    if (foundation.surfaces[s].type == surfaceType)
    {
      Surface surface;
      surface = foundation.surfaces[s];

      totalArea += surface.area;
    }
  }

  return totalArea;
}

void Ground::calculateSurfaceAverages(){
  for (auto output : groundOutput.outputMap) {
    Surface::SurfaceType surface = output.first;
    std::vector<GroundOutput::OutputType> outTypes = output.second;

    double constructionRValue = 0.0;
    double surfaceArea = foundation.surfaceAreas[surface];

    if (surface == Surface::ST_SLAB_CORE) {
      constructionRValue = foundation.slab.totalResistance();
    }
    else if (surface == Surface::ST_SLAB_PERIM) {
      constructionRValue = foundation.slab.totalResistance();
    }
    else if (surface == Surface::ST_WALL_INT) {
      constructionRValue = foundation.wall.totalResistance();
    }

    double totalHeatTransferRate = 0.0;
    //double TA = 0;
    double HA = 0.0;
    double totalArea = 0.0;

    double& Tair = bcs.indoorTemp;

    if (foundation.hasSurface[surface]) {
      // Find surface(s)
      for (size_t s = 0; s < foundation.surfaces.size(); s++)
      {
        if (foundation.surfaces[s].type == surface)
        {
          // Find tilt
          double tilt;
          if (foundation.surfaces[s].orientation == Surface::Z_POS)
            tilt = 0.0;
          else if (foundation.surfaces[s].orientation == Surface::Z_NEG)
            tilt = PI;
          else
            tilt = PI/2.0;

          #ifdef PRNTSURF
            std::ofstream output;
            output.open("surface.csv");
            output  << "x, T, h, q, dx\n";
          #endif

          for (std::size_t index = 0; index < foundation.surfaces[s].indices.size(); index++)
          {
            std::size_t i = std::get<0>(foundation.surfaces[s].indices[index]);
            std::size_t j = std::get<1>(foundation.surfaces[s].indices[index]);
            std::size_t k = std::get<2>(foundation.surfaces[s].indices[index]);

            double h = getConvectionCoeff(TNew[i][j][k],Tair,0.0,0.00208,false,tilt)
                 + getSimpleInteriorIRCoeff(domain.cell[i][j][k].surfacePtr->emissivity,
                     TNew[i][j][k],Tair);

            double& A = domain.cell[i][j][k].area;

            totalArea += A;
            totalHeatTransferRate += h*A*(Tair - TNew[i][j][k]);
            //TA += TNew[i][j][k]*A;
            HA += h*A;

            #ifdef PRNTSURF
              output <<
                domain.meshX.centers[i] << ", " <<
                TNew[i][j][k] << ", " <<
                h << ", " <<
                h*(Tair - TNew[i][j][k]) << ", " <<
                domain.meshX.deltas[i] << "\n";
            #endif

          }

          #ifdef PRNTSURF
            output.close();
          #endif

        }
      }
    }

    if (totalArea > 0.0) {
      double Tavg = Tair - totalHeatTransferRate/HA;
      double hAvg = HA/totalArea;

      groundOutput.outputValues[{surface,GroundOutput::OT_TEMP}] = Tavg;
      groundOutput.outputValues[{surface,GroundOutput::OT_FLUX}] = totalHeatTransferRate/totalArea;
      groundOutput.outputValues[{surface,GroundOutput::OT_RATE}] = totalHeatTransferRate/totalArea*surfaceArea;
      groundOutput.outputValues[{surface,GroundOutput::OT_CONV}] = hAvg;

      groundOutput.outputValues[{surface,GroundOutput::OT_EFF_TEMP}] = Tair - (totalHeatTransferRate/totalArea)*(constructionRValue+1/hAvg) - 273.15;
    }
    else {
      groundOutput.outputValues[{surface,GroundOutput::OT_TEMP}] = Tair;
      groundOutput.outputValues[{surface,GroundOutput::OT_FLUX}] = 0.0;
      groundOutput.outputValues[{surface,GroundOutput::OT_RATE}] = 0.0;
      groundOutput.outputValues[{surface,GroundOutput::OT_CONV}] = 0.0;

      groundOutput.outputValues[{surface,GroundOutput::OT_EFF_TEMP}] = Tair - 273.15;
    }
  }
}

double Ground::getSurfaceAverageValue(std::pair<Surface::SurfaceType, GroundOutput::OutputType> output)
{
  return groundOutput.outputValues[output];
}

std::vector<double> Ground::calculateHeatFlux(const size_t &i, const size_t &j, const size_t &k)
{
  std::vector<double> Qflux;
  double Qx = 0;
  double Qy = 0;
  double Qz = 0;

  double CXP = 0;
  double CXM = 0;
  double CYP = 0;
  double CYM = 0;
  double CZP = -domain.getKZP(i,j,k)*domain.getDZM(k)/(domain.getDZP(k)+domain.getDZM(k))/domain.getDZP(k);
  double CZM = -domain.getKZM(i,j,k)*domain.getDZP(k)/(domain.getDZP(k)+domain.getDZM(k))/domain.getDZM(k);

  if (foundation.numberOfDimensions > 1)
  {
    CXP = -domain.getKXP(i,j,k)*domain.getDXM(i)/(domain.getDXP(i)+domain.getDXM(i))/domain.getDXP(i);
    CXM = -domain.getKXM(i,j,k)*domain.getDXP(i)/(domain.getDXP(i)+domain.getDXM(i))/domain.getDXM(i);
  }


  if (foundation.numberOfDimensions == 3)
  {
    CYP = -domain.getKYP(i,j,k)*domain.getDYM(j)/(domain.getDYP(j)+domain.getDYM(j))/domain.getDYP(j);
    CYM = -domain.getKYM(i,j,k)*domain.getDYP(j)/(domain.getDYP(j)+domain.getDYM(j))/domain.getDYM(j);
  }

  double DTXP = 0;
  double DTXM = 0;
  double DTYP = 0;
  double DTYM = 0;
  double DTZP = 0;
  double DTZM = 0;

  if (i != nX - 1)
    DTXP = TNew[i+1][j][k]-TNew[i][j][k];

  if (i != 0)
    DTXM = TNew[i][j][k]-TNew[i-1][j][k];

  if (j != nY - 1)
    DTYP = TNew[i][j+1][k]-TNew[i][j][k];

  if (j != 0)
    DTYM = TNew[i][j][k]-TNew[i][j-1][k];

  if (k != nZ - 1)
    DTZP = TNew[i][j][k+1]-TNew[i][j][k];

  if (k != 0)
    DTZM = TNew[i][j][k]-TNew[i][j][k-1];

  switch (domain.cell[i][j][k].cellType)
  {
    case Cell::BOUNDARY:
      {
        switch (domain.cell[i][j][k].surfacePtr->orientation)
        {
          case Surface::X_NEG:
            {
              CXP = -domain.getKXP(i,j,k)/domain.getDXP(i);
              CXM = 0;
            }
          break;
          case Surface::X_POS:
            {
              CXP = 0;
              CXM = -domain.getKXM(i,j,k)/domain.getDXM(i);
            }
          break;
          case Surface::Y_NEG:
            {
              CYP = -domain.getKYP(i,j,k)/domain.getDYP(j);
              CYM = 0;
            }
          break;
          case Surface::Y_POS:
            {
              CYP = 0;
              CYM = -domain.getKYM(i,j,k)/domain.getDYM(j);
            }
          break;
          case Surface::Z_NEG:
            {
              CZP = -domain.getKZP(i,j,k)/domain.getDZP(k);
              CZM = 0;
            }
          break;
          case Surface::Z_POS:
            {
              CZP = 0;
              CZM = -domain.getKZM(i,j,k)/domain.getDZM(k);
            }
          break;
        }
        Qx = CXP*DTXP + CXM*DTXM;
        Qy = CYP*DTYP + CYM*DTYM;
        Qz = CZP*DTZP + CZM*DTZM;
      }
      break;
    case Cell::INTERIOR_AIR:
      break;
    case Cell::EXTERIOR_AIR:
      break;
    case Cell::ZERO_THICKNESS:
      {
        //int numZeroDims = domain.getNumZeroDims(i,j,k);

        std::vector<double> Qm;
        std::vector<double> Qp;

        if (isEqual(domain.meshX.deltas[i], 0.0))
        {
          Qm = calculateHeatFlux(i-1,j,k);
          Qp = calculateHeatFlux(i+1,j,k);
        }
        if (isEqual(domain.meshY.deltas[j], 0.0))
        {
          Qm = calculateHeatFlux(i,j-1,k);
          Qp = calculateHeatFlux(i,j+1,k);
        }
        if (isEqual(domain.meshZ.deltas[k], 0.0))
        {
          Qm = calculateHeatFlux(i,j,k-1);
          Qp = calculateHeatFlux(i,j,k+1);
        }

        Qx = (Qm[0] + Qp[0])*0.5;
        Qy = (Qm[1] + Qp[1])*0.5;
        Qz = (Qm[2] + Qp[2])*0.5;
      }
      break;
    default:
      {
        Qx = CXP*DTXP + CXM*DTXM;
        Qy = CYP*DTYP + CYM*DTYM;
        Qz = CZP*DTZP + CZM*DTZM;
      }
    break;
  }

  Qflux.push_back(Qx);
  Qflux.push_back(Qy);
  Qflux.push_back(Qz);

  return Qflux;
}

void Ground::calculateBoundaryLayer()
{
  Foundation fd = foundation;

  BoundaryConditions preBCs;
  preBCs.localWindSpeed = 0;
  preBCs.outdoorTemp = 273.15;
  preBCs.indoorTemp = 293.15;
  fd.coordinateSystem = Foundation::CS_CARTESIAN;
  fd.numberOfDimensions = 2;
  fd.reductionStrategy = Foundation::RS_AP;
  fd.numericalScheme = Foundation::NS_STEADY_STATE;
  fd.farFieldWidth = 100;

  Ground pre(fd);
  pre.buildDomain();
  pre.calculate(preBCs);

  std::vector<double> x2s;
  std::vector<double> fluxSums;

  double fluxSum = 0.0;

  double x1_0 = 0.0;

  bool firstIndex = true;

  size_t i_min = pre.domain.meshX.getNearestIndex(boost::geometry::area(foundation.polygon)/
      boost::geometry::perimeter(foundation.polygon));

  size_t k = pre.domain.meshZ.getNearestIndex(0.0);

  size_t j = pre.nY/2;

  for (size_t i = i_min; i < pre.nX; i++)
  {
    double Qz = pre.calculateHeatFlux(i,j,k)[2];
    double x1 = pre.domain.meshX.dividers[i];
    double x2 = pre.domain.meshX.dividers[i+1];

    if (Qz > 0.0)
    {
      fluxSum += std::max(Qz,0.0)*(x2-x1);

      if (firstIndex)
        x1_0 = x1;
      x2s.push_back(x2);
      fluxSums.push_back(fluxSum);

      firstIndex = false;
    }

  }

  //std::ofstream output;
  //output.open("Boundary.csv");

  //output << 0.0 << ", " << 0.0 << "\n";

  boundaryLayer.push_back(std::make_pair(0,0));

  for (std::size_t i = 0; i < fluxSums.size() - 1; i++) // last cell is a zero-thickness cell, so don't include it.
  {
    //output << x2s[i] - x1_0 << ", " << fluxSums[i]/fluxSum << "\n";
    boundaryLayer.push_back(std::make_pair(x2s[i] - x1_0,fluxSums[i]/fluxSum));
  }

}

double Ground::getBoundaryValue(double dist)
{
  double val = 0.0;
  if (dist > boundaryLayer[boundaryLayer.size()-1].first)
    val = 1.0;
  else
  {
    for (std::size_t i = 0; i < boundaryLayer.size()-1; i++)
    {
      if (dist >= boundaryLayer[i].first && dist < boundaryLayer[i+1].first)
      {
        double m = (boundaryLayer[i+1].first - boundaryLayer[i].first)/
            (boundaryLayer[i+1].second - boundaryLayer[i].second);
        val = (dist - boundaryLayer[i].first)/m + boundaryLayer[i].second;
        continue;
      }
    }
  }
  return val;
}

double Ground::getBoundaryDistance(double val)
{
  double dist = 0.0;
  if (val > 1.0 || val < 0.0)
  {
    showMessage(MSG_ERR, "Boundary value passed not between 0.0 and 1.0.");
  }
  else
  {
    for (std::size_t i = 0; i < boundaryLayer.size()-1; i++)
    {
      if (val >= boundaryLayer[i].second && val < boundaryLayer[i+1].second)
      {
        double m = (boundaryLayer[i+1].second - boundaryLayer[i].second)/
            (boundaryLayer[i+1].first - boundaryLayer[i].first);
        dist = (val - boundaryLayer[i].second)/m + boundaryLayer[i].first;
        continue;
      }
    }
  }
  return dist;
}

void Ground::setNewBoundaryGeometry()
{
  double area = boost::geometry::area(foundation.polygon);
  double perimeter = boost::geometry::perimeter(foundation.polygon);
  double interiorPerimeter = 0.0;

  std::size_t nV = foundation.polygon.outer().size();
  for (std::size_t v = 0; v < nV; v++)
  {
    std::size_t vPrev, vNext, vNext2;

    if (v == 0)
      vPrev = nV - 1;
    else
      vPrev = v - 1;

    if (v == nV -1)
      vNext = 0;
    else
      vNext = v + 1;

    if (v == nV - 2)
      vNext2 = 0;
    else if (v == nV -1)
      vNext2 = 1;
    else
      vNext2 = v + 2;

    Point a = foundation.polygon.outer()[vPrev];
    Point b = foundation.polygon.outer()[v];
    Point c = foundation.polygon.outer()[vNext];
    Point d = foundation.polygon.outer()[vNext2];

    // Correct U-turns
    if (foundation.isExposedPerimeter[vPrev] && foundation.isExposedPerimeter[v] && foundation.isExposedPerimeter[vNext])
    {
      if (isEqual(getAngle(a,b,c) + getAngle(b,c,d),PI))
      {
        double AB = getDistance(a,b);
        double BC = getDistance(b,c);
        double CD = getDistance(c,d);
        double edgeDistance = BC;
        double reductionDistance = std::min(AB,CD);
        double reductionValue = 1 - getBoundaryValue(edgeDistance);
        perimeter -= 2*reductionDistance*reductionValue;
      }
    }

    if (foundation.isExposedPerimeter[vPrev] && foundation.isExposedPerimeter[v])
    {
      double alpha = getAngle(a,b,c);
      double A = getDistance(a,b);
      double B = getDistance(b,c);


      if (sin(alpha) > 0)
      {
        double f = getBoundaryDistance(1-sin(alpha/2)/(1+cos(alpha/2)))/sin(alpha/2);

        // Chamfer
        double d = f/cos(alpha/2);
        if (A < d || B < d)
        {
          A = std::min(A,B);
          B = std::min(A,B);
        }
        else
        {
          A = d;
          B = d;
        }
        double C = sqrt(A*A + B*B - 2*A*B*cos(alpha));

        perimeter += C - (A + B);

      }
    }

    if (!foundation.isExposedPerimeter[v])
    {
      interiorPerimeter += getDistance(b,c);
    }

  }

  foundation.reductionStrategy = Foundation::RS_CUSTOM;
  foundation.twoParameters = false;
  foundation.reductionLength2 = area/(perimeter - interiorPerimeter);

}

void Ground::setSolarBoundaryConditions()
{
  if (foundation.numberOfDimensions == 1) {
    return;
  }
  for (std::size_t s = 0; s < foundation.surfaces.size() ; s++)
  {
    if (foundation.surfaces[s].type == Surface::ST_GRADE
        || foundation.surfaces[s].type == Surface::ST_WALL_EXT)
    {

      double& azi = bcs.solarAzimuth;
      double& alt = bcs.solarAltitude;
      double& qDN = bcs.directNormalFlux;
      double& qDH = bcs.diffuseHorizontalFlux;
      double qGH = cos(PI/2 - alt)*qDN + qDH;
      double pssf;
      double q;

      double incidence = 0.0;
      double aziYPos = foundation.orientation;
      double aziXPos = PI/2 + foundation.orientation;
      double aziYNeg = PI + foundation.orientation;
      double aziXNeg = 3*PI/2 + foundation.orientation;

      double tilt;
      if (foundation.surfaces[s].orientation == Surface::Z_POS)
      {
        tilt = 0.0;
        incidence = cos(PI/2 - alt);
      }
      else if (foundation.surfaces[s].orientation == Surface::Z_NEG)
      {
        tilt = PI;
        incidence = cos(PI/2 - alt - PI);
      }
      else
      {
        tilt = PI/2.0;

        if (foundation.numberOfDimensions == 2)
        {
          // incidence is the average incidence on the exterior of a vertical cylinder
          // 2*(int(cos(alt)*cos(x),x,0,PI/2))/(2*PI)
          // 2*(integral of incidence over a quarter of the cylinder) = lit portion
          // divide by the total radians in the circle (2*PI)
          // = 2*(cos(alt))/(2*PI)
          // = cos(alt)/PI
          incidence = cos(alt)/PI;
        }
        else
        {
          double aziSurf;
          if (foundation.surfaces[s].orientation == Surface::Y_POS)
          {
            aziSurf = aziYPos;
          }
          else if (foundation.surfaces[s].orientation == Surface::X_POS)
          {
            aziSurf = aziXPos;
          }
          else if (foundation.surfaces[s].orientation == Surface::Y_NEG)
          {
            aziSurf = aziYNeg;
          }
          else //if (foundation.surfaces[s].orientation == Surface::X_NEG)
          {
            aziSurf = aziXNeg;
          }

          if (foundation.numberOfDimensions == 3 && !foundation.useSymmetry)
          {
            // incidence = cos(alt)*cos(azi-aziSurf)*sin(tilt)+sin(alt)*cos(tilt)
            // simplifies for tilt = PI/2 to = cos(alt)*cos(azi-aziSurf)
            incidence = cos(alt)*cos(azi-aziSurf);
          }
          else // if (foundation.coordinateSystem == Foundation::CS_3D_SYMMETRY)
          {
            // if symmetric, use average incidence (one side will be facing the sun,
            // the other won't).
            if (foundation.surfaces[s].orientation == Surface::Y_POS ||
                foundation.surfaces[s].orientation == Surface::Y_NEG)
            {
              if (foundation.isXSymm)
              {
                double incidenceYPos = cos(alt)*cos(azi-aziYPos);
                if (incidenceYPos < 0)
                  incidenceYPos = 0;

                double incidenceYNeg = cos(alt)*cos(azi-aziYNeg);
                if (incidenceYNeg < 0)
                  incidenceYNeg = 0;

                incidence = (incidenceYPos + incidenceYNeg)/2.0;

              }
              else
              {
                incidence = cos(alt)*cos(azi-aziSurf);
              }
            }

            if (foundation.surfaces[s].orientation == Surface::X_POS ||
                foundation.surfaces[s].orientation == Surface::X_NEG)
            {
              if (foundation.isYSymm)
              {
                double incidenceXPos = cos(alt)*cos(azi-aziXPos);
                if (incidenceXPos < 0)
                  incidenceXPos = 0;

                double incidenceXNeg = cos(alt)*cos(azi-aziXNeg);
                if (incidenceXNeg < 0)
                  incidenceXNeg = 0;

                incidence = (incidenceXPos + incidenceXNeg)/2.0;

              }
              else
              {
                incidence = cos(alt)*cos(azi-aziSurf);
              }
            }
          }
        }
      }

      // if sun is below horizon, incidence is zero
      if (sin(alt) < 0)
        incidence = 0;
      if (incidence < 0)
        incidence = 0;

      double Fsky = (1.0 + cos(tilt))/2.0;
      double Fg = 1.0 - Fsky;
      double rho_g = 1.0 - foundation.soilAbsorptivity;

      for (std::size_t index = 0; index < foundation.surfaces[s].indices.size(); index++)
      {
        std::size_t i = std::get<0>(foundation.surfaces[s].indices[index]);
        std::size_t j = std::get<1>(foundation.surfaces[s].indices[index]);
        std::size_t k = std::get<2>(foundation.surfaces[s].indices[index]);

        double alpha = domain.cell[i][j][k].surfacePtr->absorptivity;

        if (qGH > 0.0)
        {
          pssf = incidence;
          q = alpha*(qDN*pssf + qDH*Fsky + qGH*Fg*rho_g);
        }
        else
        {
          q = 0;
        }
        domain.cell[i][j][k].heatGain = q;

      }
    }
  }
}

void Ground::setInteriorRadiationBoundaryConditions()
{
  for (std::size_t s = 0; s < foundation.surfaces.size() ; s++)
  {
    if (foundation.surfaces[s].type == Surface::ST_SLAB_CORE
        || foundation.surfaces[s].type == Surface::ST_SLAB_PERIM
        || foundation.surfaces[s].type == Surface::ST_WALL_INT)
    {
      for (std::size_t index = 0; index < foundation.surfaces[s].indices.size(); index++)
      {
        std::size_t i = std::get<0>(foundation.surfaces[s].indices[index]);
        std::size_t j = std::get<1>(foundation.surfaces[s].indices[index]);
        std::size_t k = std::get<2>(foundation.surfaces[s].indices[index]);

        if (foundation.surfaces[s].type == Surface::ST_WALL_INT) {
          domain.cell[i][j][k].heatGain = bcs.wallAbsRadiation;
        }
        else {
          domain.cell[i][j][k].heatGain = bcs.slabAbsRadiation;
        }
      }
    }
  }
}

double getArrayValue(std::vector<std::vector<std::vector<double>>> Mat, std::size_t i, std::size_t j, std::size_t k)
{
  return Mat[i][j][k];
}

}

#endif
