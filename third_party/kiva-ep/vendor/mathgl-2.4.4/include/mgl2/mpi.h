/***************************************************************************
 * mpi.h is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef _MGL_MPI_H_
#define _MGL_MPI_H_

#include "mgl2/mgl_cf.h"

#ifdef __cplusplus
extern "C" {
#endif
/// Send graphical information to node id using MPI
void MGL_EXPORT mgl_mpi_send(HMGL gr, int id);
void MGL_EXPORT mgl_mpi_send_(uintptr_t *gr, int *id);
/// Receive graphical information from node id using MPI
void MGL_EXPORT mgl_mpi_recv(HMGL gr, int id);
void MGL_EXPORT mgl_mpi_recv_(uintptr_t *gr, int *id);
#ifdef __cplusplus
}

#include "mgl2/mgl.h"
//-----------------------------------------------------------------------------
/// Wrapper class for all graphics
class MGL_EXPORT mglGraphMPI:public mglGraph
{
	mglGraphMPI(const mglGraphMPI &t) {}	// copying is not allowed
	const mglGraphMPI &operator=(const mglGraphMPI &t)	{	return t;	}
public:
	inline mglGraphMPI(int kind=0, int width=600, int height=400):mglGraph(kind,width,height){}
	inline mglGraphMPI(HMGL graph):mglGraph(graph){}
	virtual ~mglGraphMPI(){}

	/// Send graphical information to node id using MPI
	inline void MPI_Send(int id)	{	mgl_mpi_send(gr,id);	}
	/// Receive graphical information from node id using MPI
	inline void MPI_Recv(int id)	{	mgl_mpi_recv(gr,id);	}

};
#endif
//-----------------------------------------------------------------------------
#endif
