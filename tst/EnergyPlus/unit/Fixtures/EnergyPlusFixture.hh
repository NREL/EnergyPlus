#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataStringGlobals.hh>

#include <regex>

namespace EnergyPlus {

	// void redirect_cout( std::unique_ptr<std::ostringstream> const & m_buffer, std::unique_ptr<std::streambuf> & m_old_buffer ) {
	// 	m_buffer = std::unique_ptr<std::ostringstream>( new std::ostringstream );
	// 	// Save cout's buffer here
	// 	m_old_buffer = std::unique_ptr<std::streambuf>( std::cout.rdbuf() );
	// 	// Redirect cout to our ostringstream buffer or any other ostream
	// 	std::cout.rdbuf( m_buffer->rdbuf() );
	// }

	// void redirect_cout( std::unique_ptr<std::ostringstream> const & m_buffer, std::unique_ptr<std::streambuf> & m_old_buffer ) {
	// 	m_buffer = std::unique_ptr<std::ostringstream>( new std::ostringstream );
	// 	// Save cout's buffer here
	// 	m_old_buffer = std::unique_ptr<std::streambuf>( std::cout.rdbuf() );
	// 	// Redirect cout to our ostringstream buffer or any other ostream
	// 	std::cout.rdbuf( m_buffer->rdbuf() );
	// }

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
			// Save cout's buffer here
			// m_old_cout_buffer = std::unique_ptr<std::streambuf>( std::cout.rdbuf() );
			// Redirect cout to our ostringstream buffer or any other ostream
			// std::cout.rdbuf( m_buffer->rdbuf() );
		}

		virtual void TearDown() {
			// When done redirect cout to its old self
			// std::cout.rdbuf( m_old_buffer.release() );
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


		// void death_function( std::function<void()> func, std::string const & cerr_regex = ".*", bool assert_death = false, bool assert_message = false ) {
		// 	if ( assert_death ) {
		// 		ASSERT_DEATH( func(), cerr_regex );
		// 	} else {
		// 		EXPECT_DEATH( func(), cerr_regex );
		// 	}

		// 	// std::regex test_regex( fatal_regex );
		// 	// if ( assert_message ) {
		// 	// 	ASSERT_TRUE( std::regex_search( m_buffer->str(), test_regex ) );
		// 	// } else {
		// 	// 	EXPECT_TRUE( std::regex_search( m_buffer->str(), test_regex ) );
		// 	// }
		// }

		// template <typename T>
		// void death_function( std::function<T()> func, std::string const & cerr_regex = ".*", bool assert_death = false, bool assert_message = false ) {
		// 	if ( assert_death ) {
		// 		ASSERT_DEATH( func(), cerr_regex );
		// 	} else {
		// 		EXPECT_DEATH( func(), cerr_regex );
		// 	}

		// 	// std::regex test_regex( fatal_regex );
		// 	// if ( assert_message ) {
		// 	// 	ASSERT_TRUE( std::regex_search( m_buffer->str(), test_regex ) );
		// 	// } else {
		// 	// 	EXPECT_TRUE( std::regex_search( m_buffer->str(), test_regex ) ) << "cout: " << m_buffer->str() << " regex: " << fatal_regex;
		// 	// }
		// }

	private:
		std::unique_ptr<std::ostringstream> m_cout_buffer;
		std::unique_ptr<std::ostringstream> m_cerr_buffer;
		// std::unique_ptr<std::streambuf> m_old_cout_buffer;
		std::unique_ptr<RedirectCout> m_redirect_cout;
		std::unique_ptr<RedirectCerr> m_redirect_cerr;

	};

}

#endif
