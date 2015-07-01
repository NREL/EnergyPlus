#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/DataStringGlobals.hh>

#include <memory>

using namespace EnergyPlus;
using namespace ObjexxFCL;

namespace EnergyPlus {

	class EnergyPlusFixture : public testing::Test
	{
		struct RedirectCout {
			RedirectCout( std::unique_ptr<std::ostringstream> const & m_buffer ) 
			: m_old_buffer( std::cout.rdbuf( m_buffer->rdbuf() ) )
			{ }

			~RedirectCout( ) {
				std::cout.rdbuf( m_old_buffer.release() );
			}

			private:
				std::unique_ptr<std::streambuf> m_old_buffer;
		};

		struct RedirectCerr {
			RedirectCerr( std::unique_ptr<std::ostringstream> const & m_buffer ) 
			: m_old_buffer( std::cerr.rdbuf( m_buffer->rdbuf() ) )
			{ }

			~RedirectCerr( ) {
				std::cerr.rdbuf( m_old_buffer.release() );
			}

			private:
				std::unique_ptr<std::streambuf> m_old_buffer;
		};

	protected:
		static void SetUpTestCase() { }

		static void TearDownTestCase() { }

		virtual void SetUp() {
			m_cout_buffer = std::unique_ptr<std::ostringstream>( new std::ostringstream );
			m_redirect_cout = std::unique_ptr<RedirectCout>( new RedirectCout( m_cout_buffer ) );

			m_cerr_buffer = std::unique_ptr<std::ostringstream>( new std::ostringstream );
			m_redirect_cerr = std::unique_ptr<RedirectCerr>( new RedirectCerr( m_cerr_buffer ) );
		}

		virtual void TearDown() {
			m_redirect_cout.reset();
			m_redirect_cout = nullptr;
			m_redirect_cerr.reset();
			m_redirect_cerr = nullptr;

			m_cout_buffer.reset();
			m_cout_buffer = nullptr;
			m_cerr_buffer.reset();
			m_cerr_buffer = nullptr;
		}

		std::string delimitedString( std::vector<std::string> const & strings, std::string const & delimiter = DataStringGlobals::NL ) {
			std::unique_ptr<std::ostringstream> compare_text(new std::ostringstream);
			for( auto const & str : strings ) {
				* compare_text << str << delimiter;
			}
			return compare_text->str();
		}

		// Must use ASSERT_NO_FATAL_FAILURE or EXPECT_NO_FATAL_FAILURE in calling test if using assert_eq = true, otherwise
		// ASSERT_EQ will not cause fatal failure in calling test.
		// Usage: 	ASSERT_NO_FATAL_FAILURE(Foo());
		// 			int i;
		// 			EXPECT_NO_FATAL_FAILURE({
		//   			i = Bar();
		// 			});
		// Or we can check if current test has fatal failure
		// Usage:	if ( HasFatalFailure() ) return;
		void compareCOUTStream( std::string const & correctString, bool resetStream = true, bool assert_eq = false ) {
			if ( assert_eq ) {
				ASSERT_EQ( correctString, this->m_cout_buffer->str() );
			} else {
				EXPECT_EQ( correctString, this->m_cout_buffer->str() );
			}
			if ( resetStream ) this->m_cout_buffer->str( std::string() );
		}

		void compareCERRStream( std::string const & correctString, bool resetStream = true, bool assert_eq = false ) {
			if ( assert_eq ) {
				ASSERT_EQ( correctString, this->m_cerr_buffer->str() );
			} else {
				EXPECT_EQ( correctString, this->m_cerr_buffer->str() );
			}
			if ( resetStream ) this->m_cerr_buffer->str( std::string() );
		}

		bool hasCoutOutput(){
			return this->m_cout_buffer->str().size() > 0;
		}

		bool hasCerrOutput(){
			return this->m_cerr_buffer->str().size() > 0;
		}

	private:
		std::unique_ptr<std::ostringstream> m_cout_buffer;
		std::unique_ptr<std::ostringstream> m_cerr_buffer;
		std::unique_ptr<RedirectCout> m_redirect_cout;
		std::unique_ptr<RedirectCerr> m_redirect_cerr;

	};

}

#endif
