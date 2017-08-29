#ifndef NUSSELTNUMBER_H
#define NUSSELTNUMBER_H

namespace Tarcog {
	class CNusseltNumberStrategy {
	public:
		virtual double calculate( double const t_Tilt, double const t_Ra, double const t_Asp );
	protected:
		~CNusseltNumberStrategy() = default;
		double pos( double const t_Value );
	};

	class CNusseltNumber0To60 : public CNusseltNumberStrategy {
	public:
		virtual ~CNusseltNumber0To60() = default;
		double calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) override;
	};

	class CNusseltNumber60 : public CNusseltNumberStrategy {
	public:
		virtual ~CNusseltNumber60() = default;
		double calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) override;
	};

	class CNusseltNumber60To90 : public CNusseltNumberStrategy {
	public:
		virtual ~CNusseltNumber60To90() = default;
		double calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) override;
	};

	class CNusseltNumber90to180 : public CNusseltNumberStrategy {
	public:
		virtual ~CNusseltNumber90to180() = default;
		double calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) override;
	};

	class CNusseltNumber90 : public CNusseltNumberStrategy {
	public:
		virtual ~CNusseltNumber90() = default;
		double calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) override;
	};

	class CNusseltNumber {
	public:
		double calculate( double const t_Tilt, double const t_Ra, double const t_Asp );
	};
}


#endif
