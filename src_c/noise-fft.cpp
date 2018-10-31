/***************************************************************************
                          noise-fft.cpp  -  description
                             -------------------
    begin                : Sun Jun 24 19:41:22 MDT 2001
    copyright            : (C) 2001 by George Shapovalov
    email                : gerr@gasser.caltech.edu
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

/*
Reduces noise by subtracting fourier components of the baseline.
One has to select the region, which fft image will be subtracted from the full trace
(for selected ch#).
Outputs all data channels (multiplexed as required), does not change file header:
output has just datapoints for selected channel replaced with new values.

usage: noise-fft [options] in-file [out-file]

options:
-n nchanl   number of channel to "filter"
-q          be quiet: no output except data

if out-file is omitted (or is '-' or stdout), stdout is assumed.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
