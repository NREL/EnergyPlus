#ifndef OutputProcessorFixture_hh_INCLUDED
#define OutputProcessorFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "SQLiteFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

namespace EnergyPlus {

	class OutputProcessorFixture : public SQLiteFixture
	{

	protected:
		static void SetUpTestCase() { }

		static void TearDownTestCase() { }

		virtual void SetUp() {
			SQLiteFixture::SetUp();  // Sets up the base fixture first.
			this->eso_stream = std::unique_ptr<std::ostringstream>( new std::ostringstream );
			this->mtr_stream = std::unique_ptr<std::ostringstream>( new std::ostringstream );
			DataGlobals::eso_stream = this->eso_stream.get();
			DataGlobals::mtr_stream = this->mtr_stream.get();
		}

		virtual void TearDown() {
			this->eso_stream.reset();
			this->eso_stream = nullptr;
			this->mtr_stream.reset();
			this->mtr_stream = nullptr;
			SQLiteFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}

		void compareESOStream( std::string const & correctString, bool resetStream = true ) {
			EXPECT_EQ( correctString, this->eso_stream->str() );
			if ( resetStream ) this->eso_stream->str(std::string());
		}

		void compareMTRStream( std::string const & correctString, bool resetStream = true ) {
			EXPECT_EQ( correctString, this->mtr_stream->str() );
			if ( resetStream ) this->mtr_stream->str(std::string());
		}

	private:
		std::unique_ptr<std::ostringstream> eso_stream;
		std::unique_ptr<std::ostringstream> mtr_stream;

	};

}

#endif
