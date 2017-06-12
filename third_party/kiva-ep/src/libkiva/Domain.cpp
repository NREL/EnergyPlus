/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef Domain_CPP
#define Domain_CPP

#include "Domain.hpp"

namespace Kiva {

static const double PI = 4.0*atan(1.0);

Domain::Domain()
{

}

Domain::Domain(Foundation &foundation)
{
  setDomain(foundation);
}

// Having this separate from the constructor allows the correct resizing of
// multidimensional arrays on pre-existing initialized instances.
void Domain::setDomain(Foundation &foundation)
{

  {
    Mesher mX(foundation.xMeshData);
    meshX = mX;

    Mesher mY(foundation.yMeshData);
    meshY = mY;

    Mesher mZ(foundation.zMeshData);
    meshZ = mZ;
  }

  nX = meshX.centers.size();
  nY = meshY.centers.size();
  nZ = meshZ.centers.size();

  cell.resize(nX,std::vector<std::vector<Cell> >(nY,std::vector<Cell>(nZ)));

  for (std::size_t i = 0; i < nX; i++)
  {
    for (std::size_t j = 0; j < nY; j++)
    {
      for (std::size_t k = 0; k < nZ; k++)
      {

        // Set Cell Properties
        cell[i][j][k].density = foundation.soil.density;
        cell[i][j][k].specificHeat = foundation.soil.specificHeat;
        cell[i][j][k].conductivity = foundation.soil.conductivity;
        cell[i][j][k].heatGain = 0.0;

        // Default to normal cells
        cell[i][j][k].cellType = Cell::NORMAL;

        // Next set interior zero-width cells
        if (foundation.numberOfDimensions == 3)
        {
          if (isEqual(meshX.deltas[i], 0.0) ||
            isEqual(meshZ.deltas[k], 0.0) ||
            isEqual(meshY.deltas[j], 0.0))
          {
            cell[i][j][k].cellType = Cell::ZERO_THICKNESS;
          }
        }
        else if (foundation.numberOfDimensions == 2)
        {
          if (isEqual(meshX.deltas[i], 0.0) ||
            isEqual(meshZ.deltas[k], 0.0))
          {
            cell[i][j][k].cellType = Cell::ZERO_THICKNESS;
          }
        }
        else
        {
          if (isEqual(meshZ.deltas[k], 0.0))
          {
            cell[i][j][k].cellType = Cell::ZERO_THICKNESS;
          }
        }

        for (std::size_t b = 0; b < foundation.blocks.size(); b++)
        {
          if (boost::geometry::within(Point(meshX.centers[i],meshY.centers[j]), foundation.blocks[b].polygon) &&
            isGreaterThan(meshZ.centers[k], foundation.blocks[b].zMin) &&
            isLessThan(meshZ.centers[k], foundation.blocks[b].zMax))
          {
            cell[i][j][k].density = foundation.blocks[b].material.density;
            cell[i][j][k].specificHeat = foundation.blocks[b].material.specificHeat;
            cell[i][j][k].conductivity = foundation.blocks[b].material.conductivity;

            cell[i][j][k].blockPtr = &foundation.blocks[b];

            if (foundation.blocks[b].blockType == Block::INTERIOR_AIR)
            {
              cell[i][j][k].cellType = Cell::INTERIOR_AIR;
            }
            else if (foundation.blocks[b].blockType == Block::EXTERIOR_AIR)
            {
              cell[i][j][k].cellType = Cell::EXTERIOR_AIR;
            } else {
              cell[i][j][k].cellType = Cell::NORMAL;
            }
          }
        }

        for (std::size_t s = 0; s < foundation.surfaces.size(); s++)
        {
          if (pointOnPoly(Point(meshX.centers[i],meshY.centers[j]), foundation.surfaces[s].polygon))
          {
            if (isGreaterOrEqual(meshZ.centers[k], foundation.surfaces[s].zMin)
            &&  isLessOrEqual(meshZ.centers[k], foundation.surfaces[s].zMax))
            {
              cell[i][j][k].cellType = Cell::BOUNDARY;

              cell[i][j][k].surfacePtr = &foundation.surfaces[s];

              // Point/Line cells not on the boundary should be
              // zero-thickness cells
              int numZeroDims = getNumZeroDims(i,j,k);

              if (foundation.numberOfDimensions == 3)
              {
                if ((numZeroDims > 1) &&
                  i != 0 && i != nX - 1 &&
                  j != 0 && j != nY - 1 &&
                  k != 0 && k != nZ - 1)
                  cell[i][j][k].cellType = Cell::ZERO_THICKNESS;
              }
              else if (foundation.numberOfDimensions == 2)
              {
                if ((numZeroDims > 1) &&
                  i != 0 && i != nX - 1 &&
                  k != 0 && k != nZ - 1)
                  cell[i][j][k].cellType = Cell::ZERO_THICKNESS;
              }
              else
              {
                if ((numZeroDims > 1) &&
                  k != 0 && k != nZ - 1)
                  cell[i][j][k].cellType = Cell::ZERO_THICKNESS;
              }

              if (cell[i][j][k].cellType == Cell::BOUNDARY)
              {
                foundation.surfaces[s].indices.push_back(std::tuple<std::size_t,std::size_t,std::size_t> (i,j,k));
              }

            }
          }
        }

        // Set cell volume
        cell[i][j][k].volume = meshX.deltas[i]*meshY.deltas[j]*meshZ.deltas[k];

        // for boundary cells, set cell area
        if (cell[i][j][k].cellType == Cell::BOUNDARY)
        {
          if (foundation.numberOfDimensions == 2 &&
              foundation.coordinateSystem == Foundation::CS_CYLINDRICAL)
          {
            if (cell[i][j][k].surfacePtr->orientation == Surface::X_POS ||
              cell[i][j][k].surfacePtr->orientation == Surface::X_NEG)
            {
              cell[i][j][k].area = 2.0*PI*meshX.centers[i]*meshZ.deltas[k];
            }
            else // if (surface.orientation == Surface::Z_POS ||
               // surface.orientation == Surface::Z_NEG)
            {
              cell[i][j][k].area = PI*(meshX.dividers[i+1]*meshX.dividers[i+1] -
            		  meshX.dividers[i]*meshX.dividers[i] );
            }
          }
          else if (foundation.numberOfDimensions == 2 &&
                   foundation.coordinateSystem == Foundation::CS_CARTESIAN)
          {
            if (cell[i][j][k].surfacePtr->orientation == Surface::X_POS ||
              cell[i][j][k].surfacePtr->orientation == Surface::X_NEG)
            {
              cell[i][j][k].area = 2.0*meshZ.deltas[k]*foundation.linearAreaMultiplier;
            }
            else // if (surface.orientation == Surface::Z_POS ||
               // surface.orientation == Surface::Z_NEG)
            {
              cell[i][j][k].area = 2.0*meshX.deltas[i]*foundation.linearAreaMultiplier;
            }
          }
          else if (foundation.numberOfDimensions == 3)
          {
            if (cell[i][j][k].surfacePtr->orientation == Surface::X_POS ||
              cell[i][j][k].surfacePtr->orientation == Surface::X_NEG)
            {
              cell[i][j][k].area = meshY.deltas[j]*meshZ.deltas[k];
            }
            else if (cell[i][j][k].surfacePtr->orientation == Surface::Y_POS ||
                 cell[i][j][k].surfacePtr->orientation == Surface::Y_NEG)
            {
              cell[i][j][k].area = meshX.deltas[i]*meshZ.deltas[k];
            }
            else // if (surface.orientation == Surface::Z_POS ||
               // surface.orientation == Surface::Z_NEG)
            {
              cell[i][j][k].area = meshX.deltas[i]*meshY.deltas[j];
            }

            if (foundation.useSymmetry)
            {
              if (foundation.isXSymm)
                cell[i][j][k].area = 2*cell[i][j][k].area;

              if (foundation.isYSymm)
                cell[i][j][k].area = 2*cell[i][j][k].area;
            }
          }
          else
          {
            cell[i][j][k].area = 1.0;
          }
        }
      }
    }
  }

  // Set effective properties of zero-thickness cells
  // based on other cells
  for (std::size_t i = 0; i < nX; i++)
  {
    for (std::size_t j = 0; j < nY; j++)
    {
      for (std::size_t k = 0; k < nZ; k++)
      {
        int numZeroDims = getNumZeroDims(i,j,k);

        if (numZeroDims > 0
            && cell[i][j][k].cellType != Cell::INTERIOR_AIR
            && cell[i][j][k].cellType != Cell::EXTERIOR_AIR)
        {
          if (foundation.numberOfDimensions == 3)
          {
            if (i != 0 && i != nX - 1 &&
              j != 0 && j != nY - 1 &&
              k != 0 && k != nZ - 1)
              set3DZeroThicknessCellProperties(i,j,k);
          }
          else if (foundation.numberOfDimensions == 2)
          {
            if (i != 0 && i != nX - 1 && k != 0 && k != nZ - 1)
              set2DZeroThicknessCellProperties(i,j,k);
          }
          else
          {
            if (k != 0 && k != nZ - 1)
            {
              if (isEqual(meshZ.deltas[k], 0.0))
              {
                std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
                  {std::make_tuple(i,j,k-1),
                   std::make_tuple(i,j,k+1)};

                setZeroThicknessCellProperties(i, j, k, pointSet);
              }
            }
          }
        }
      }
    }
  }

  // Calculate matrix coefficients
  for (std::size_t i = 0; i < nX; i++)
  {
    for (std::size_t j = 0; j < nY; j++)
    {
      for (std::size_t k = 0; k < nZ; k++)
      {

        // PDE Coefficients

        if (foundation.numberOfDimensions > 1) {
          // Radial X terms
          if (foundation.coordinateSystem == Foundation::CS_CYLINDRICAL)
          {
            cell[i][j][k].cxp_c = (getDXM(i)*getKXP(i,j,k))/
                ((getDXM(i) + getDXP(i))*getDXP(i));
            cell[i][j][k].cxm_c = (getDXP(i)*getKXM(i,j,k))/
                ((getDXM(i) + getDXP(i))*getDXM(i));
          }
          else
          {
            cell[i][j][k].cxp_c = 0.0;
            cell[i][j][k].cxm_c = 0.0;
          }

          // Cartesian X terms
          cell[i][j][k].cxp = (2*getKXP(i,j,k))/
              ((getDXM(i) + getDXP(i))*getDXP(i));
          cell[i][j][k].cxm = -1*(2*getKXM(i,j,k))/
              ((getDXM(i) + getDXP(i))*getDXM(i));
        }

        // Cartesian Z terms
        cell[i][j][k].czp = (2*getKZP(i,j,k))/
            ((getDZM(k) + getDZP(k))*getDZP(k));
        cell[i][j][k].czm = -1*(2*getKZM(i,j,k))/
            ((getDZM(k) + getDZP(k))*getDZM(k));

        // Cartesian Y terms
        if (foundation.numberOfDimensions == 3)
        {
          cell[i][j][k].cyp = (2*getKYP(i,j,k))/
              ((getDYM(j) + getDYP(j))*getDYP(j));
          cell[i][j][k].cym = -1*(2*getKYM(i,j,k))/
              ((getDYM(j) + getDYP(j))*getDYM(j));
        }
        else
        {
          cell[i][j][k].cyp = 0.0;
          cell[i][j][k].cym = 0.0;
        }
      }
    }
  }

  for (std::size_t s = 0; s < foundation.surfaces.size(); s++)
  {
    foundation.surfaces[s].area = 0;
    for (std::size_t index = 0; index < foundation.surfaces[s].indices.size(); index++)
    {
      std::size_t i = std::get<0>(foundation.surfaces[s].indices[index]);
      std::size_t j = std::get<1>(foundation.surfaces[s].indices[index]);
      std::size_t k = std::get<2>(foundation.surfaces[s].indices[index]);

      foundation.surfaces[s].area += cell[i][j][k].area;
    }
  }
}

double Domain::getDXP(std::size_t i)
{
  if (i == nX - 1)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the previous cell
    return (meshX.deltas[i] + meshX.deltas[i - 1])/2.0;
  }
  else
  {
    return (meshX.deltas[i] + meshX.deltas[i + 1])/2.0;
  }
}

double Domain::getDXM(std::size_t i)
{
  if (i == 0)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the previous cell
    return (meshX.deltas[i] + meshX.deltas[i + 1])/2.0;
  }
  else
  {
    return (meshX.deltas[i] + meshX.deltas[i - 1])/2.0;
  }
}

double Domain::getDYP(std::size_t j)
{
  if (j == nY - 1)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the previous cell
    return (meshY.deltas[j] + meshY.deltas[j - 1])/2.0;
  }
  else
  {
    return (meshY.deltas[j] + meshY.deltas[j + 1])/2.0;
  }
}

double Domain::getDYM(std::size_t j)
{
  if (j == 0)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the previous cell
    return (meshY.deltas[j] + meshY.deltas[j + 1])/2.0;
  }
  else
  {
    return (meshY.deltas[j] + meshY.deltas[j - 1])/2.0;
  }
}

double Domain::getDZP(std::size_t k)
{
  if (k == nZ - 1)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the previous cell
    return (meshZ.deltas[k] + meshZ.deltas[k - 1])/2.0;
  }
  else
  {
    return (meshZ.deltas[k] + meshZ.deltas[k + 1])/2.0;
  }
}

double Domain::getDZM(std::size_t k)
{
  if (k == 0)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the previous cell
    return (meshZ.deltas[k] + meshZ.deltas[k + 1])/2.0;
  }
  else
  {
    return (meshZ.deltas[k] + meshZ.deltas[k - 1])/2.0;
  }
}

double Domain::getKXP(std::size_t i, std::size_t j, std::size_t k)
{
  if (i == nX - 1)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the current cell
    return cell[i][j][k].conductivity;
  }
  else
  {
    return 1/(meshX.deltas[i]/(2*getDXP(i)*cell[i][j][k].conductivity) +
        meshX.deltas[i + 1]/(2*getDXP(i)*cell[i+1][j][k].conductivity));
  }
}

double Domain::getKXM(std::size_t i, std::size_t j, std::size_t k)
{
  if (i == 0)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the current cell
    return cell[i][j][k].conductivity;
  }
  else
  {
    return 1/(meshX.deltas[i]/(2*getDXM(i)*cell[i][j][k].conductivity) +
        meshX.deltas[i - 1]/(2*getDXM(i)*cell[i-1][j][k].conductivity));
  }
}

double Domain::getKYP(std::size_t i, std::size_t j, std::size_t k)
{
  if (j == nY - 1)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the current cell
    return cell[i][j][k].conductivity;
  }
  else
  {
    return 1/(meshY.deltas[j]/(2*getDYP(j)*cell[i][j][k].conductivity) +
        meshY.deltas[j + 1]/(2*getDYP(j)*cell[i][j+1][k].conductivity));
  }
}

double Domain::getKYM(std::size_t i, std::size_t j, std::size_t k)
{
  if (j == 0)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the current cell
    return cell[i][j][k].conductivity;
  }
  else
  {
    return 1/(meshY.deltas[j]/(2*getDYM(j)*cell[i][j][k].conductivity) +
        meshY.deltas[j - 1]/(2*getDYM(j)*cell[i][j-1][k].conductivity));
  }
}

double Domain::getKZP(std::size_t i, std::size_t j, std::size_t k)
{
  if (k == nZ - 1)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the current cell
    return cell[i][j][k].conductivity;
  }
  else
  {
    return 1/(meshZ.deltas[k]/(2*getDZP(k)*cell[i][j][k].conductivity) +
        meshZ.deltas[k + 1]/(2*getDZP(k)*cell[i][j][k+1].conductivity));
  }
}

double Domain::getKZM(std::size_t i, std::size_t j, std::size_t k)
{
  if (k == 0)
  {
    // For boundary cells assume that the cell on the other side of the
    // boundary is the same as the current cell
    return cell[i][j][k].conductivity;
  }
  else
  {
    return 1/(meshZ.deltas[k]/(2*getDZM(k)*cell[i][j][k].conductivity) +
        meshZ.deltas[k - 1]/(2*getDZM(k)*cell[i][j][k-1].conductivity));
  }
}

void Domain::set2DZeroThicknessCellProperties(std::size_t i,std::size_t j,std::size_t k)
{
  if (isEqual(meshX.deltas[i], 0.0) &&
    isEqual(meshZ.deltas[k], 0.0))
  {

    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i-1,j,k+1),
       std::make_tuple(i+1,j,k+1),
       std::make_tuple(i-1,j,k-1),
       std::make_tuple(i+1,j,k-1)};

    setZeroThicknessCellProperties(i, j, k, pointSet);

  }
  else if (isEqual(meshX.deltas[i], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i-1,j,k),
       std::make_tuple(i+1,j,k)};

    setZeroThicknessCellProperties(i, j, k, pointSet);

  }
  else if (isEqual(meshZ.deltas[k], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i,j,k-1),
       std::make_tuple(i,j,k+1)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
}

void Domain::set3DZeroThicknessCellProperties(std::size_t i,std::size_t j,std::size_t k)
{
  if (isEqual(meshX.deltas[i], 0.0) &&
    isEqual(meshY.deltas[j], 0.0) &&
    isEqual(meshZ.deltas[k], 0.0))
  {
    // Use all 8 full volume cells
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i-1,j-1,k+1),
       std::make_tuple(i+1,j-1,k+1),
       std::make_tuple(i-1,j-1,k-1),
       std::make_tuple(i+1,j-1,k-1),
       std::make_tuple(i-1,j+1,k+1),
       std::make_tuple(i+1,j+1,k+1),
       std::make_tuple(i-1,j+1,k-1),
       std::make_tuple(i+1,j+1,k-1)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
  else if (isEqual(meshX.deltas[i], 0.0) &&
    isEqual(meshY.deltas[j], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i-1,j-1,k),
       std::make_tuple(i+1,j-1,k),
       std::make_tuple(i-1,j+1,k),
       std::make_tuple(i+1,j+1,k)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
  else if (isEqual(meshX.deltas[i], 0.0) &&
    isEqual(meshZ.deltas[k], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i-1,j,k+1),
       std::make_tuple(i+1,j,k+1),
       std::make_tuple(i-1,j,k-1),
       std::make_tuple(i+1,j,k-1)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
  else if (isEqual(meshY.deltas[j], 0.0) &&
    isEqual(meshZ.deltas[k], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i,j-1,k+1),
       std::make_tuple(i,j+1,k+1),
       std::make_tuple(i,j-1,k-1),
       std::make_tuple(i,j+1,k-1)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
  else if (isEqual(meshX.deltas[i], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i+1,j,k),
       std::make_tuple(i-1,j,k)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
  else if (isEqual(meshY.deltas[j], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i,j+1,k),
       std::make_tuple(i,j-1,k)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }
  else if (isEqual(meshZ.deltas[k], 0.0))
  {
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet =
      {std::make_tuple(i,j,k+1),
       std::make_tuple(i,j,k-1)};

    setZeroThicknessCellProperties(i, j, k, pointSet);
  }

}

void Domain::setZeroThicknessCellProperties(std::size_t i,
    std::size_t j, std::size_t k,
    std::vector<std::tuple<std::size_t,std::size_t,std::size_t> > pointSet)
{
  std::vector<double> volumes;
  std::vector<double> densities;
  std::vector<double> specificHeats;
  std::vector<double> conductivities;

  std::vector<double> masses;
  std::vector<double> capacities;
  std::vector<double> weightedConductivity;


  for (std::size_t p = 0; p < pointSet.size(); p++)
  {
    std::size_t iP = std::get<0>(pointSet[p]);
    std::size_t jP = std::get<1>(pointSet[p]);
    std::size_t kP = std::get<2>(pointSet[p]);

    // Do not add air cell properties into the weighted average
    if (cell[iP][jP][kP].cellType != Cell::INTERIOR_AIR &&
      cell[iP][jP][kP].cellType != Cell::EXTERIOR_AIR)
    {
    double vol = cell[iP][jP][kP].volume;
    double rho = cell[iP][jP][kP].density;
    double cp = cell[iP][jP][kP].specificHeat;
    double kth = cell[iP][jP][kP].conductivity;

    volumes.push_back(vol);
    masses.push_back(vol*rho);
    capacities.push_back(vol*rho*cp);
    weightedConductivity.push_back(vol*kth);
    }
  }

  // if the neighboring cells are all air cells set properties to air properties
  if (volumes.size() == 0)
  {
    volumes.push_back(1.0);
    masses.push_back(1.275);
    capacities.push_back(1.275*1007);
    weightedConductivity.push_back(0.02587);

  }

  double totalVolume = std::accumulate(volumes.begin(), volumes.end(), 0.0);

  cell[i][j][k].density = std::accumulate(masses.begin(), masses.end(), 0.0) /
      totalVolume;

  cell[i][j][k].specificHeat = std::accumulate(capacities.begin(), capacities.end(), 0.0) /
      (totalVolume*cell[i][j][k].density);

  cell[i][j][k].conductivity = std::accumulate(weightedConductivity.begin(), weightedConductivity.end(), 0.0) /
      totalVolume;
}

int Domain::getNumZeroDims(std::size_t i,std::size_t j,std::size_t k)
{
  int numZeroDims = 0;
  if (isEqual(meshX.deltas[i], 0.0))
    numZeroDims += 1;
  if (isEqual(meshY.deltas[j], 0.0))
    numZeroDims += 1;
  if (isEqual(meshZ.deltas[k], 0.0))
    numZeroDims += 1;

  return numZeroDims;
}
void Domain::printCellTypes()
{
  // TODO: Make the ability to output a specific slice in i, j, or k
  std::ofstream output;
  output.open("Cells.csv");

  for (std::size_t i = 0; i < nX; i++)
  {

    output << ", " << i;

  }

  output << "\n";

  for (std::size_t k = nZ - 1; /* k >= 0 && */ k < nZ; k--)
  {

    output << k;

    for (std::size_t i = 0; i < nX; i++)
    {

      output << ", " << cell[i][nY/2][k].cellType;

    }

    output << "\n";
  }
  output.close();

}

}

#endif
