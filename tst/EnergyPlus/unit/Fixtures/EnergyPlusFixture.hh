#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataStringGlobals.hh>

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

	private:
		std::unique_ptr<std::ostringstream> m_cout_buffer;
		std::unique_ptr<std::ostringstream> m_cerr_buffer;
		std::unique_ptr<RedirectCout> m_redirect_cout;
		std::unique_ptr<RedirectCerr> m_redirect_cerr;

	};

}

#endif
