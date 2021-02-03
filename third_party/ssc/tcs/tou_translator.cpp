#define _TCSTYPEINTERFACE_
#include "tcstype.h"

enum {	
	P_WEEKDAY_SCHEDULE,
	P_WEEKEND_SCHEDULE,

	O_TOU_VALUE,

	N_MAX };

tcsvarinfo tou_translator_variables[] = {
	// vartype            datatype              index                       name                        label                                          units           meta            group    default_value
	{ TCS_PARAM,          TCS_MATRIX,           P_WEEKDAY_SCHEDULE,         "weekday_schedule",         "12x24 matrix of values for weekdays",         "",             "",             "",          "" },
	{ TCS_PARAM,          TCS_MATRIX,           P_WEEKEND_SCHEDULE,         "weekend_schedule",         "12x24 matrix of values for weekend days",     "",             "",             "",          "" },
	
	{ TCS_OUTPUT,         TCS_NUMBER,           O_TOU_VALUE,                "tou_value",                "Value during time step",                      "",             "",             "",          "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class tou_translator : public tcstypeinterface
{
private:
	double m_hourly_tou[8760];

public:
	tou_translator( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
		for (int i=0; i<8760; i++)
			m_hourly_tou[i]=0;
	}

	virtual ~tou_translator()
	{
	}

	virtual int init()
	{
		int nrows, ncols;
		double *weekdays = value( P_WEEKDAY_SCHEDULE, &nrows, &ncols );
		if ( nrows != 12 || ncols != 24 ) {
			message(TCS_ERROR,  "The TOU translator did not get a 12x24 matrix for the weekday schedule." );
			return -1;
		}
		double *weekends = value( P_WEEKEND_SCHEDULE, &nrows, &ncols );		
		if ( nrows != 12 || ncols != 24 ) {
			message( TCS_ERROR, "The TOU translator did not get a 12x24 matrix for the weekend schedule." );
			return -1;
		}

		int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

		int wday = 5, i=0;
		for (int m=0;m<12;m++)
		{
			for (int d=0;d<nday[m];d++)
			{
				bool bWeekend = (wday <= 0);

				if (wday >= 0) wday--;
				else wday = 5;

				for (int h=0; h<24 && i<8760 && m*24+h<288; h++)
				{
					if (bWeekend)
						m_hourly_tou[i] = weekends[ m*24 + h ];
					else
						m_hourly_tou[i] = weekdays[ m*24 + h ];
					i++;					
				}
			}
		}




		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{
		//int ihour = (int)(floor(time/3600.0 + 1.e-6)-1);
		int ihour = (int)(ceil(time / 3600.0 - 1.e-6)-1);
		if(ihour>8760-1 || ihour<0) {
			return -1;	//ERROR
		}

		double tou = m_hourly_tou[ihour];
		value( O_TOU_VALUE, tou );

		return 0;
	}
};


TCS_IMPLEMENT_TYPE( tou_translator, "Time of Use translator", "Tom Ferguson", 1, tou_translator_variables, NULL, 0 )
