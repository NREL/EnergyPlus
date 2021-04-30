#include <gtest/gtest.h>
#include <splinter/datatable.h>
#include <splinter/bspline.h>
#include <splinter/bsplinebuilder.h>

//using std::cout;
//using std::endl;

using namespace SPLINTER;

double f1d(DenseVector x)
{
	assert(x.rows() == 1);
	return (4 - 2.1*x(0)*x(0)
		+ (1 / 3.)*x(0)*x(0)*x(0)*x(0))*x(0)*x(0);
}

double f(DenseVector x)
{
	assert(x.rows() == 2);
	return (4 - 2.1*x(0)*x(0)
		+ (1 / 3.)*x(0)*x(0)*x(0)*x(0))*x(0)*x(0)
		+ x(0)*x(1)
		+ (-4 + 4 * x(1)*x(1))*x(1)*x(1);
}

// test from https://github.com/bgrimstad/splinter/blob/master/docs/cpp_interface.md
TEST(splinterTests, testCamelback2d)
{
	// Create new DataTable to manage samples
	DataTable samples;

	// Sample the function
	DenseVector x(2);
	double y;
	for (int i = 0; i < 20; i++)
	{
		for (int j = 0; j < 20; j++)
		{
			// Sample function at x
			x(0) = i * 0.1;
			x(1) = j * 0.1;
			y = f(x);

			// Store sample
			samples.addSample(x, y);
		}
	}

	// Build B-splines that interpolate the samples
	BSpline bspline1 = BSpline::Builder(samples).degree(1).build();
	BSpline bspline3 = BSpline::Builder(samples).degree(3).build();

	// Build penalized B-spline (P-spline) that smooths the samples
	BSpline pspline = BSpline::Builder(samples)
		.degree(3)
		.smoothing(BSpline::Smoothing::PSPLINE)
		.alpha(0.03)
		.build();

	/* Evaluate the approximants at x = (1,1)
	 * Note that the error will be 0 at that point (except for the P-spline, which may introduce an error
	 * in favor of a smooth approximation) because it is a point we sampled at.
	 */
	x(0) = 0.35; x(1) = 1.68;
	double func_val = f(x);
	double lin_spline = bspline1.eval(x);
	double cubic_spline = bspline3.eval(x);
	double p_spline = pspline.eval(x);

	EXPECT_NEAR(lin_spline, func_val, 0.15) << "Linear Spline";
	EXPECT_NEAR(cubic_spline, func_val, 0.01) << "Cubic Spline";
	EXPECT_NEAR(p_spline, func_val, 0.1) << "P Spline";

	/*
	cout << "-----------------------------------------------------" << endl;
	cout << "Function at x:                 " << f(x) << endl;
	cout << "Linear B-spline at x:          " << bspline1.eval(x) << endl;
	cout << "Cubic B-spline at x:           " << bspline3.eval(x) << endl;
	cout << "P-spline at x:                 " << pspline.eval(x) << endl;
	cout << "-----------------------------------------------------" << endl;
	*/
}

TEST(splinterTests, testCamelback1d)
{
	// Create new DataTable to manage samples
	DataTable samples;

	// Sample the function
	DenseVector x(1);
	double y;
	for (int i = 0; i < 20; i++)
	{
			// Sample function at x
			x(0) = i * 0.1;
			y = f1d(x);

			// Store sample
			samples.addSample(x, y);
	}

	// Build B-splines that interpolate the samples
	BSpline bspline1 = BSpline::Builder(samples).degree(1).build();
	BSpline bspline3 = BSpline::Builder(samples).degree(3).build();

	// Build penalized B-spline (P-spline) that smooths the samples
	BSpline pspline = BSpline::Builder(samples)
		.degree(3)
		.smoothing(BSpline::Smoothing::PSPLINE)
		.alpha(0.03)
		.build();

	/* Evaluate the approximants at x = (1,1)
	 * Note that the error will be 0 at that point (except for the P-spline, which may introduce an error
	 * in favor of a smooth approximation) because it is a point we sampled at.
	 */
	x(0) = 0.35;
	double func_val = f1d(x);
	double lin_spline = bspline1.eval(x);
	double cubic_spline = bspline3.eval(x);
	double p_spline = pspline.eval(x);

	EXPECT_NEAR(lin_spline, func_val, 0.15) << "Linear Spline";
	EXPECT_NEAR(cubic_spline, func_val, 0.01) << "Cubic Spline";
	EXPECT_NEAR(p_spline, func_val, 0.1) << "P Spline";

	/*
	cout << "-----------------------------------------------------" << endl;
	cout << "Function at x:                 " << f1d(x) << endl;
	cout << "Linear B-spline at x:          " << bspline1.eval(x) << endl;
	cout << "Cubic B-spline at x:           " << bspline3.eval(x) << endl;
	cout << "P-spline at x:                 " << pspline.eval(x) << endl;
	cout << "-----------------------------------------------------" << endl;
	*/
}