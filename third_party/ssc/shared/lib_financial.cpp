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

#include <cmath>
#include <limits>
#include "lib_financial.h"

using namespace libfin;

/* financial code here */
/* ported from http://code.google.com/p/irr-newtonraphson-calculator/ */

static bool is_valid_iter_bound(double estimatedReturnRate)
{
	return estimatedReturnRate != -1 && (estimatedReturnRate < std::numeric_limits<int>::max()) && (estimatedReturnRate > std::numeric_limits<int>::min());
}

static double irr_poly_sum(double estimatedReturnRate, const std::vector<double> &CashFlows, int Count)
{
    double sumOfPolynomial = 0;
    if (is_valid_iter_bound(estimatedReturnRate))
	{
		for (int j = 0; j < Count && j<(int)CashFlows.size(); j++)
        {
			double val = (pow((1 + estimatedReturnRate), j));
			if (val != 0.0)
				sumOfPolynomial += CashFlows[j]/val;
			else
				break;
        }
	}
    return sumOfPolynomial;
}

static double irr_derivative_sum(double estimatedReturnRate, const std::vector<double> &CashFlows, int Count)
{
    double sumOfDerivative = 0;
    if (is_valid_iter_bound(estimatedReturnRate))
		for (int i = 1; i < Count && i < (int)CashFlows.size(); i++)
        {
            sumOfDerivative += CashFlows[i]*(i)/pow((1 + estimatedReturnRate), i);
        }
    return sumOfDerivative*-1;
}

double libfin::irr(double tolerance, int maxIterations, const std::vector<double> &CashFlows, int Count)
{
/* Validation check - can write to log if move to FinModel or include SimModel
	if ((count < 2) || (CashFlows[0] > 0))
    {
		Messages.Add( "Cash flow for the first period  must be negative and there should");
    }
*/
	int numberOfIterations=0;
	double calculatedIRR=0;
	double initialGuess = 0.1; // 10% is default used in Excel IRR function

	if (CashFlows.size() < 3)
		return initialGuess;

    if ( (Count > 1) && (CashFlows[0] <= 0))
    {
		double deriv_sum = irr_derivative_sum(initialGuess,CashFlows,Count);
		if (deriv_sum != 0)
			calculatedIRR = initialGuess - irr_poly_sum(initialGuess,CashFlows,Count)/deriv_sum;
		else
			return initialGuess;

		numberOfIterations++;
		while (!(fabs(irr_poly_sum(calculatedIRR,CashFlows,Count)) <= tolerance) && (numberOfIterations < maxIterations))
		{
			deriv_sum = irr_derivative_sum(initialGuess,CashFlows,Count);
			if (deriv_sum != 0.0)
				calculatedIRR = calculatedIRR - irr_poly_sum(calculatedIRR,CashFlows,Count)/deriv_sum;
			else
				break;

			numberOfIterations++;
		}
	}
    return calculatedIRR;
}


/*ported directly from Delphi simple geometric sum*/
double libfin::npv(double Rate, const std::vector<double> &CashFlows, int Count) //, PaymentTime: TPaymentTime)
{
	/*
{ Caution: The sign of NPV is reversed from what would be expected for standard
   cash flows!}
*/
	if (Rate <= -1.0)
	{
		// TODO - throw exception?
		return -999;
	}

	if (Count > (int)CashFlows.size())
		Count = (int)CashFlows.size();

	double rr = 1/(1+Rate);
	double result = 0;
	for (int i=Count-1;i>0;i--)
	{
		result = rr * result + CashFlows[i];
	}
//	if PaymentTime = ptEndOfPeriod then result := rr * result;
	return result*rr; // assumes end of period payments!!
}

double libfin::payback(const std::vector<double> &CumulativePayback, const std::vector<double> &Payback, int Count)
{
/*
Return payback in years of inputs streams
Payback occures when cumulative stream is > 0
Find exact payback by subtracting cumulative / payback
*/
  double dPayback = 1e99; // report as > analysis period
  bool bolPayback = false;
  int iPayback = 0;
  int i = 1;
  while ((i<Count) && (!bolPayback))
  {
    if (CumulativePayback[i] > 0)
	{
      bolPayback = true;
      iPayback = i;
	}
	i++;
  }

  if (bolPayback)
  {
	  if (Payback[iPayback] !=0)
	  {
		  dPayback = iPayback - CumulativePayback[iPayback] /Payback[iPayback];
	  }
	  else
	  {
		  dPayback = iPayback;
	  }
  }
  return dPayback;
}


/* code source http://www.linkedin.com/answers/technology/software-development/TCH_SFT/445353-4527099?browseCategory=TCH_SFT
Returns the payment on the principal for a given period for an investment based on periodic, constant payments and a constant interest rate.

Syntax

PPMT(rate,per,nper,pv,fv,type)

For a more complete description of the arguments in PPMT, see PV.

Rate   is the interest rate per period.

Per   specifies the period and must be in the range 1 to nper.

Nper   is the total number of payment periods in an annuity.

Pv   is the present value � the total amount that a series of future payments is worth now.

Fv   is the future value, or a cash balance you want to attain after the last payment is made. If fv is omitted, it is assumed to be 0 (zero), that is, the future value of a loan is 0.

Type   is the number 0 or 1 and indicates when payments are due.

Set type equal to If payments are due
0 or omitted At the end of the period
1 At the beginning of the period

Remark
Make sure that you are consistent about the units you use for specifying rate and nper. If you make monthly payments on a four-year loan at 12 percent annual interest, use 12%/12 for rate and 4*12 for nper. If you make annual payments on the same loan, use 12% for rate and 4 for nper.


*/
double libfin::pow1pm1 (double x, double y)
{
	return (x <= -1) ? pow (1 + x, y) - 1 : exp(y * log(1.0 + x)) - 1;
}
double libfin::pow1p (double x, double y)
{
	return (fabs (x) > 0.5) ? pow (1 + x, y) : exp (y * log(1.0 + x));
}
double libfin::fvifa (double rate, double nper)
{
	return (rate == 0) ? nper : pow1pm1 (rate, nper) / rate;
}

double libfin::pvif (double rate, double nper)
{
	return pow1p (rate, nper);
}

double libfin::pmt (double rate, double nper, double pv, double fv, int type)
{
	return ((-pv * pvif (rate, nper) - fv ) / ((1.0 + rate * type) * fvifa (rate, nper)));
}

double libfin::ipmt (double rate, double per, double nper, double pv, double fv, int type)
{
	double p = pmt (rate, nper, pv, fv, 0);
	double ip = -(pv * pow1p (rate, per - 1) * rate + p * pow1pm1 (rate, per - 1));
	return (type == 0) ? ip : ip / (1 + rate);
}

double libfin::ppmt (double rate, double per, double nper, double pv, double fv, int type)
{
	// exception identified 1/26/2010 by Aron and J.MacKnick when term (nper = 0)
	if (nper == 0) return 0.0;
	double p = pmt (rate, nper, pv, fv, type);
	double ip = ipmt (rate, per, nper, pv, fv, type);
	return p - ip;
}

// from http://www.codeproject.com/Articles/58289/C-Round-Function.aspx
long libfin::round_irs(double number)
{
    return (number >= 0) ? (long)(number + 0.5) : (long)(number - 0.5);
}
