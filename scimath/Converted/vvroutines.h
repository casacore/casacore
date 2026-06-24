#ifndef VVROUTINES_H
#define VVROUTINES_H

/**
 * @file vvroutines.h
 * @brief Van Vleck correction functions for 3-level and 9-level quantization
 *
 * These functions compute the true correlation coefficient from the observed
 * correlation for equi-spaced quantizer thresholds, as used in the VanVleck class.
 * Ported from the original AIPS++ / CASA Fortran code (vvroutines.f).
 */

// =======================================================================
// 3-level Van Vleck functions
// =======================================================================

/**
 * Van Vleck curve for 3x3 level quantization (general case)
 * @param mux   Mean input level for x (in units of rms)
 * @param muy   Mean input level for y (in units of rms)
 * @param v1x   First positive input threshold for x (in units of rms)
 * @param v1y   First positive input threshold for y (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr3(double mux, double muy, double v1x, double v1y, double rho);

/**
 * Van Vleck curve for 3x3 level quantization, auto-correlation case (x == y)
 * @param mux   Mean input level (in units of rms)
 * @param v1x   First positive input threshold (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr3auto(double mux, double v1x, double rho);

/**
 * Van Vleck curve for 3x3 level quantization, zero-mean case
 * @param v1x   First positive input threshold for x (in units of rms)
 * @param v1y   First positive input threshold for y (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr3zmean(double v1x, double v1y, double rho);

/**
 * Van Vleck curve for 3x3 level quantization, zero-mean auto-correlation case
 * @param v     First positive input threshold (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr3zauto(double v, double rho);

// =======================================================================
// 9-level Van Vleck functions
// =======================================================================

/**
 * Van Vleck curve for 9x9 level quantization (general case)
 * @param mux   Mean input level for x (in units of rms)
 * @param muy   Mean input level for y (in units of rms)
 * @param v1x   First positive input threshold for x (in units of rms)
 * @param v1y   First positive input threshold for y (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr9(double mux, double muy, double v1x, double v1y, double rho);

/**
 * Van Vleck curve for 9x9 level quantization, auto-correlation case (x == y)
 * @param mux   Mean input level (in units of rms)
 * @param v1x   First positive input threshold (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr9auto(double mux, double v1x, double rho);

/**
 * Van Vleck curve for 9x9 level quantization, zero-mean case
 * @param v1x   First positive input threshold for x (in units of rms)
 * @param v1y   First positive input threshold for y (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr9zmean(double v1x, double v1y, double rho);

/**
 * Van Vleck curve for 9x9 level quantization, zero-mean auto-correlation case
 * @param v     First positive input threshold (in units of rms)
 * @param rho   Observed correlation coefficient
 * @return      True correlation coefficient
 */
double vvr9zauto(double v, double rho);

#endif // VVROUTINES_H

