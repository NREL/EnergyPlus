// Format Support
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Format.hh>
#include <ObjexxFCL/byte.hh>
#include <ObjexxFCL/ubyte.hh>
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/fmt.hh>

// C++ Headers
#include <algorithm>
#include <cctype>

namespace ObjexxFCL {

typedef  std::size_t  Size;
typedef  std::string  Token;
typedef  std::vector< Token >  Tokens;
typedef  std::vector< Format * >  Formats;

	// Input
	void
	Format::in( std::istream & stream, byte & b )
	{ // Default implementation
		skip_chunk( stream );
		b = 0;
		io_err( stream );
	}

	// Input
	void
	Format::in( std::istream & stream, ubyte & b )
	{ // Default implementation
		skip_chunk( stream );
		b = 0;
		io_err( stream );
	}

	// Spacing and Reversion Linefeed String for a Non-Spacer
	std::string
	Format::spc( std::string const & ter )
	{
		if ( reverted() ) { // Reverted: Line feed before reverted output
			reverted() = spacer() = false;
			return ter;
		} else { // Not reverted: Add space and set spacing state
			spacer() = false;
			return "";
		}
	}

	// Spacing and Reversion Linefeed String for a Spacer
	std::string
	Format::spc_spacer( std::string const & ter )
	{
		if ( reverted() ) { // Reverted: Line feed before reverted output
			reverted() = false;
			spacer() = true;
			return ter;
		} else { // Not reverted: Add space and set spacing state
			bool const add_space( ! spacer() ); // Add space if previous item was non-spacer
			spacer() = true;
			return ( add_space ? " " : "" );
		}
	}

	// Skip Over a Number of Characters in a Stream
	void
	Format::skip( std::istream & stream, Size const w )
	{
		static std::string const STOPPERS( "\r\n" + std::string( 1, std::istream::traits_type::eof() ) );
		char c;
		Size i( 0 );
		while ( ( i < w ) && stream && not_any_of( stream.peek(), STOPPERS ) ) {
			stream.get( c );
			++i;
		}
		// Don't clear eof bit since specified width read
	}

	// Read List-Directed Entry from a Stream
	EntryFormatLD
	Format::read_ld( std::istream & stream, bool const numeric, char const mode )
	{
		static char const eof( std::istream::traits_type::eof() );
		static std::string const STOPPERS( "\r\n" + std::string( 1, eof ) );
		std::string s;
		char c( '\0' ), q( '\0' ), peek( stream.peek() );
		bool in_body( false );
		bool in_quote( false );
		bool has_repeat( false );
		Size repeat( 1ul );
		while ( stream && ( s.empty() || ( peek != eof ) ) && ( ( ! numeric ) || ( peek != ')' ) ) ) {
			stream.get( c );
			if ( stream ) {
				peek = stream.peek();
				if ( in_quote ) { // In quote-wrapped string
					if ( c == q ) {
						if ( ( mode == 'F' ) && ( peek == q ) ) { // Fortran-style escaped quote
							stream.get( c );
							peek = stream.peek();
							s += c; // Add the quote without the repeat
						} else { // End of quote
							break;
						}
					} else if ( ( c == '\\' ) && ( mode == 'C' ) && ( peek == q ) ) { // C-style escaped quote
						stream.get( c );
						peek = stream.peek();
						s += c; // Add the quote without escaping
					} else if ( c == '\n' ) {
						// Discard and keep going until quote is closed
					} else if ( c == '\r' ) {
						// Discard and keep going until quote is closed
					} else { // Add to entry
						s += c;
					}
				} else if ( is_space_ld( c ) || ( c == '\n' ) || ( c == '\r' ) ) { // Separator: Skip over if no entry characters read yet
					if ( in_body ) break; // End of entry
				} else if ( c == ',' ) { // End of entry
					clear_eof( stream );
					return EntryFormatLD( s, in_body, repeat );
				} else if ( c == '/' ) { // Terminate i/o operation
					slash_terminated() = true;
					clear_eof( stream );
					return EntryFormatLD( s, in_body, repeat );
				} else if ( ( c == '*' ) && ( ! has_repeat ) && ( is_ulong( s ) ) && ( s[ 0 ] != '-' ) ) { // Repeat count: Strict => Add && ( ulong_of( s ) > 0ul )
					repeat = ulong_of( s );
					if ( repeat == 0ul ) repeat = 1ul; // Intel Fortran behavior: Zero should really cause repeat spec to be treated as part of entry
					has_repeat = true;
					in_body = false;
					s.clear();
				} else if ( ( ! in_body ) && ( ( c == '"' ) || ( c == '\'' ) ) ) { // Start of quoted section
					in_quote = true;
					q = c; // Save quote delimiter but don't add it to entry
				} else { // Entry character
					if ( numeric ) {
						if ( c == 'D' ) {
							c = 'E';
						} else if ( c == 'd' ) {
							c = 'e';
						}
					}
					s += c;
					in_body = true;
				}
				if ( ( ! in_quote ) && ( ( peek == '\n' ) || ( peek == '\r' ) ) ) break; // Entry stops at newline unless in quote
				if ( has_repeat && ( peek == eof ) ) break; // Don't put stream into eof yet
			}
		}
		if ( stream && ( c != eof ) ) { // Remove trailing separators before next item
			while ( stream && not_any_of( peek, STOPPERS ) ) {
				peek = stream.peek();
				if ( is_space_ld( peek ) ) { // Separator
					stream.ignore(); // Skip over separator
				} else if ( peek == ',' ) { // Closing comma separator
					stream.ignore(); // Skip over separator
					break; // Stop: We only want to take one closing separator
				} else if ( peek == '\n' ) { // End of content
					break;
				} else if ( peek == '\r' ) { // End of content
					break;
				} else if ( peek == eof ) { // End of content
					break;
				} else { // Non-separator: Leave at front of stream
					break;
				}
			}
		}
		clear_eof( stream );
		return EntryFormatLD( s, ( in_body || in_quote ), repeat );
	}

	// Apply Blank Processing to String
	std::string
	Format::blank_process( std::string const & s ) const
	{
		std::string o( s );
		if ( blank_null() ) {
			o.erase( std::remove( o.begin(), o.end(), ' ' ), o.end() );
		} else {
			assert( blank_zero() );
			std::replace( o.begin(), o.end(), ' ', '0' );
		}
		return o;
	}

	// Read from a Stream
	std::string
	Format::read( std::istream & stream, Size const w )
	{
		static char const eof( std::istream::traits_type::eof() );
		static std::string const STOPPERS( "\r\n" + std::string( 1, eof ) );
		std::string s;
		if ( ! stream ) return s; // Leave stream state alone
		if ( stream.peek() == eof ) return s; // At eof: Want eof bit set
		char c;
		Size i( 0 );
		while ( ( ( w == NOSIZE ) || ( i < w ) ) && stream && not_any_of( stream.peek(), STOPPERS ) ) {
			stream.get( c );
			if ( stream ) s += c;
			++i;
		}
		if ( w == NOSIZE ) clear_eof( stream );
		return s;
	}

	// Read Floating Point from a Stream
	std::string
	Format::read_float( std::istream & stream, Size const w )
	{
		static std::string const STOPPERS( "\r\n" + std::string( 1, std::istream::traits_type::eof() ) );
		static std::string const WHITE( " \t\0", 3 );
		std::string s;
		char c;
		bool in_num( false ), in_E( false );
		Size i( 0 );
		while ( ( ( w == NOSIZE ) || ( i < w ) ) && stream && not_any_of( stream.peek(), STOPPERS ) ) {
			stream.get( c );
			if ( c == 'D' ) {
				c = 'E';
			} else if ( c == 'd' ) {
				c = 'e';
			} else if ( in_num && ( ! in_E ) && ( ( c == '+' ) || ( c == '-' ) ) ) {
				s += 'E'; // Add omitted E before exponent: Fortran can read strings like 1.25+3 as 1.25E+3
			}
			if ( not_any_of( c, WHITE ) ) {
				in_num = true;
			} else if ( ( c == 'E' ) || ( c == 'e' ) ) {
				in_E = true;
			}
			if ( stream ) s += c;
			++i;
		}
		return s;
	}

	// Copy Assignment
	FormatList &
	FormatList::operator =( FormatList const & f )
	{
		if ( this != &f ) {
			FormatCombo::operator =( f );
			for ( Format * format : formats_ ) delete format;
			formats_.clear();
			for ( Format * format : f.formats() ) formats_.push_back( format->clone() );
		}
		return *this;
	}

	// Next Format: Upward Call
	Format *
	FormatList::next_up()
	{
		++i(); // Increment repeat index
		if ( i() < formats_.size() ) { // Next Format
			Format * next_format( formats_[ i() ] );
			return ( next_format ? next_format->current() : nullptr ); // Current Format branch will be initial branch due to resets before next_up calls
		} else { // Parent's next Format if any
			i() = 0ul; // Reset repeat index
			if ( p() ) { // Have parent
				return p()->next_up();
			} else { // Top (no parent)
				return nullptr; // Done
			}
		}
	}

	// Copy Assignment
	FormatGroup &
	FormatGroup::operator =( FormatGroup const & f )
	{
		if ( this != &f ) {
			FormatCombo::operator =( f );
			if ( format_ ) delete format_;
			format_ = ( f.format() ? f.format()->clone() : nullptr );
		}
		return *this;
	}

	// Next Format: Upward Call
	Format *
	FormatGroup::next_up()
	{
		++i(); // Increment repeat index
		if ( format_ && can_repeat( i() ) ) { // This Format again
			return format_->current(); // Current Format branch will be initial branch due to resets before next_up calls
		} else { // Parent's next Format if any
			i() = 0ul; // Reset repeat index
			if ( p() ) { // Have parent
				return p()->next_up();
			} else { // Top (no parent)
				return nullptr; // Done
			}
		}
	}

	// Reversion Format Setup
	Format *
	FormatGroupTop::revert()
	{
		Format * f( format() );
		if ( f ) { // Format present
			if ( f->is_group() ) {
				return f->current();
			} else if ( f->is_list() ) {
				if ( ! fr_ ) { // Initialize reversion specs
					ir_ = 0ul;
					fr_ = current();
					FormatList * fl( dynamic_cast< FormatList * >( f ) );
					for ( Formats::size_type j = fl->size(); j > 0; --j ) {
						Formats::size_type const i( j - 1 );
						Format * fi( fl->formats()[ i ] );
						if ( fi && fi->is_group() ) { // Found last group
							ir_ = i; // Set item index for reversion group
							fr_ = fi->current(); // Set reversion Format
						}
					}
				}
				f->i() = ir_;
				return fr_;
			} else {
				return f->current();
			}
		} else { // No Format
			return nullptr;
		}
	}

	// Next Format
	Format *
	FormatLeaf::next()
	{
		++i(); // Increment repeat index
		if ( can_repeat( i() ) ) { // This Format again
			return this;
		} else { // Parent's next Format
			i() = 0ul; // Reset repeat index
			if ( p() ) {
				return p()->next_up();
			} else { // Top (no parent)
				return nullptr; // Done
			}
		}
	}

	// Input
	void
	FormatA::in( std::istream & stream, char & c )
	{
		std::string const b( read( stream, wid( 1ul ) ) ); // Buffer string
		std::string::size_type const lb( b.length() ); // Might be < w_ if hit end of record
		if ( lb >= 1 ) { // Take rightmost character read
			c = b.back();
		} else { // No characters read: Set to space
			c = ' ';
		}
	}

	// Input
	void
	FormatL::in( std::istream & stream, bool & b )
	{
		std::string const s( read( stream, wid( 2ul ) ) );
		if ( has_prefix( s, "T", false ) || has_prefix( s, ".T", false ) ) {
			b = true;
		} else if ( has_prefix( s, "F", false ) || has_prefix( s, ".F", false ) || is_blank( s ) ) { // Blank is Intel Fortran extension
			b = false;
		} else { // Bad input
			b = false;
		}
	}

	// Output
	void
	FormatI::out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( b, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( b, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatI::out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::I( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( b, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( b, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatB::out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::B( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( b, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( b, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatO::out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::O( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( b, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( b, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatZ::out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::Z( i, w(), m() );
	}

	// Output
	void
	FormatF::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::F( v, w(), d(), P() );
	}

	// Output
	void
	FormatF::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::F( v, w(), d(), P() );
	}

	// Output
	void
	FormatF::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::F( v, w(), d(), P() );
	}

	// Input
	void
	FormatG::in( std::istream & stream, bool & b )
	{
		std::string const s( read( stream, wid( TraitsG< bool >::w ) ) );
		if ( has_prefix( s, "T", false ) || has_prefix( s, ".T", false ) ) {
			b = true;
		} else if ( has_prefix( s, "F", false ) || has_prefix( s, ".F", false ) || is_blank( s ) ) { // Blank is Intel Fortran extension
			b = false;
		} else { // Bad input
			b = false;
		}
	}

	// Input
	void
	FormatG::in( std::istream & stream, char & c )
	{
		std::string const b( read( stream, wid( TraitsG< char >::w ) ) ); // Buffer string
		std::string::size_type const lb( b.length() ); // Might be < w() if hit end of record
		if ( lb >= 1 ) { // Take rightmost character read
			c = b.back();
		} else { // No characters read: Set to space
			c = ' ';
		}
	}

	// Output
	void
	FormatG::out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( float( b ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( float( b ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( float( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( float( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( double( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( double( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( ( long double )( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( ( long double )( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( ( long double )( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( ( long double )( i ), w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatG::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::G( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatE::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::E( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatE::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::E( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatE::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::E( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatEN::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::EN( v, w(), d(), e() );
	}

	// Output
	void
	FormatEN::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::EN( v, w(), d(), e() );
	}

	// Output
	void
	FormatEN::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::EN( v, w(), d(), e() );
	}

	// Output
	void
	FormatES::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::ES( v, w(), d(), e() );
	}

	// Output
	void
	FormatES::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::ES( v, w(), d(), e() );
	}

	// Output
	void
	FormatES::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::ES( v, w(), d(), e() );
	}

	// Output
	void
	FormatD::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::D( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatD::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::D( v, w(), d(), e(), P() );
	}

	// Output
	void
	FormatD::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::D( v, w(), d(), e(), P() );
	}

	// Input
	void
	FormatLD::read_bool( std::istream & stream, bool & b )
	{
		if ( entry_.r == 0ul ) entry_ = read_ld( stream ); // Get a new entry if no repeats left on current entry
		if ( entry_.has() ) { // Non-null entry
			std::string const & s( entry_.s );
			if ( has_prefix( s, "T", false ) || has_prefix( s, ".T", false ) ) {
				b = true;
			} else if ( has_prefix( s, "F", false ) || has_prefix( s, ".F", false ) || is_blank( s ) ) { // Blank is Intel Fortran extension
				b = false;
			} else { // Bad input
				b = false;
			}
		}
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, bool const b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( b );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( b );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( b );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( i );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, float const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( v );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( v );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, long double const v, std::string const & ter )
	{
		stream << spc( ter ) << fmt::LD( v );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, char const c, std::string const & ter )
	{
		stream << spc_spacer( ter ) << fmt::LD( c );
	}

	// Output
	void
	FormatLD::out( std::ostream & stream, std::string const & s, std::string const & ter )
	{
		stream << spc_spacer( ter ) << fmt::LD( s );
	}

// Add a token to the list
inline
void
add_token( std::string & token, Tokens & tokens )
{
	if ( ! strip_whitespace( token ).empty() ) {
		tokens.push_back( token );
		token.clear();
	}
}

// Add a character token to the list
inline
void
add_token( char const token, Tokens & tokens )
{
	tokens.push_back( std::string( 1, token ) );
}

// Tokenize a Single Formatting Token
inline
Tokens
tokenize1( Token const & s )
{
	static std::string const WHITE( " \t\0", 3 );
	Tokens tokens;
	Token token;
	char c;
	std::string cc;
	for ( Size i = 0, e = s.size(); i < e; ++i ) {
		c = uppercased( s[ i ] );
		cc = uppercased( s.substr( i, 2 ) );
		if ( ( cc == "TL" ) || ( cc == "TR" ) || ( cc == "EN" ) || ( cc == "ES" ) || ( cc == "BN" ) ||
		 ( cc == "BZ" ) || ( cc == "SP" ) || ( cc == "SS" ) || ( cc == "SU" ) ) {
			add_token( token, tokens );
			add_token( cc, tokens );
			++i; // Skip 2nd character
		} else if ( is_any_of( c, "XRTALBIOZPFGEDS./" ) ) { // Break out token
			add_token( token, tokens );
			add_token( c, tokens );
		} else if ( is_any_of( c, WHITE ) ) { // # Whitespace isn't significant outside of string literals
			// Discard
		} else { // Add to token
			token += c;
		}
	}
	add_token( token, tokens ); // Last token
	return tokens;
}

// Alphabetic Token
inline
Token
alpha_token( Tokens const & tokens )
{
	for ( Token const & token : tokens ) {
		if ( is_alpha( token ) ) return token;
	}
	return Token(); // None found
}

	// Create Format
	Format *
	FormatFactory::create(
	 std::string const & s,
	 Format * p
	)
	{
		typedef  Format::Size  Size;
		Tokens tokens;
		Token token;
		char const dq( '"' ), sq( '\'' ), bs( '\\' );
		bool const nested( p != nullptr );
		int in_paren( 0 );
		bool in_dq( false ); // In double-quoted literal?
		bool in_sq( false ); // In single-quoted literal?
		char c( '\0' );
		int n_bs( 0 ); // Backslash sequence length mod 2
		std::string s_;
		if ( not_blank( s ) ) {
			if ( nested || ( ( s.front() == '(' ) && ( s.back() == ')' ) ) ) { // Use it as is
				s_ = s;
			} else { // Wrap in parens: Convenience feature: Esp to allow passing * for list-directed
				s_ = '(' + s + ')';
			}
		} else { // Empty
			s_ = "()";
		}
		for ( Size i = 0, e = s_.size(); i < e; ++i ) {
			c = s_[ i ];
			if ( in_paren ) {
				if ( in_dq ) {
					if ( ( c == dq ) && ( n_bs != 1 ) ) in_dq = false; // End of string literal
				} else if ( in_sq ) {
					if ( ( c == sq ) && ( n_bs != 1 ) ) in_sq = false; // End of string literal
				} else {
					if ( c == '(' ) {
						++in_paren;
					} else if ( c == ')' ) {
						--in_paren;
					} else if ( c == dq ) {
						in_dq = true;
					} else if ( c == sq ) {
						in_sq = true;
					}
				}
				if ( ! in_paren ) { // End of paren group
					add_token( token, tokens );
					add_token( c, tokens );
					in_dq = false;
					in_sq = false;
				} else {
					token += c;
				}
			} else if ( in_dq ) {
				token += c;
				if ( c == dq ) { // Double quote
					if ( n_bs != 1 ) { // End of string literal
						add_token( token, tokens );
						in_dq = false;
					}
				}
			} else if ( in_sq ) {
				token += c;
				if ( c == sq ) { // Single quote
					if ( n_bs != 1 ) { // End of string literal
						add_token( token, tokens );
						in_sq = false;
					}
				}
			} else if ( c == '*' ) { // List-directed format
				add_token( token, tokens );
				add_token( c, tokens );
			} else if ( c == '(' ) {
				add_token( token, tokens );
				add_token( c, tokens );
				in_paren = true;
			} else if ( c == dq ) {
				add_token( token, tokens );
				token = c;
				in_dq = true;
			} else if ( c == sq ) {
				add_token( token, tokens );
				token = c;
				in_sq = true;
			} else if ( c == ',' ) { // Comma separator
				add_token( token, tokens ); // Add current token but not the comma
			} else if ( is_any_of( c, "/:$\\" ) ) { // One character operator
				if ( ( c == '/' ) && is_ulong( token ) ) { // Line feed with repeat count
					token += c;
					add_token( token, tokens );
				} else { // Operator is separate token
					add_token( token, tokens );
					add_token( c, tokens );
				}
			} else if ( ( is_any_of( c, "Pp" ) ) && ( is_ulong( token ) ) ) { // # nP prefix: Split from rest of token
				token += c;
				add_token( token, tokens );
			} else if ( is_any_of( c, " \t" ) ) { // # Whitespace isn't significant outside of string literals
				// Discard
			} else { // Add to token
				token += c;
			}
			n_bs = ( c == bs ? ( n_bs + 1 ) % 2 : 0 );
		}
		if ( in_dq ) token += dq; // Close the quote
		if ( in_sq ) token += sq; // Close the quote
		add_token( token, tokens ); // Last token if present
		if ( in_paren ) add_token( ')', tokens ); // Close the group

		// Generate formats
		Tokens::size_type const n( tokens.size() );
		if ( n == 0 ) return nullptr; // No tokens
		if ( ( n == 2 || n == 3 ) && ( tokens[ 0 ] == "(" ) && ( tokens.back() == ")" ) ) { // Paren Group
			FormatGroup * fg( nested ? static_cast< FormatGroup * >( new FormatGroupSub( p ) ) : static_cast< FormatGroup * >( new FormatGroupTop() ) );
			if ( n == 3 ) fg->format( FormatFactory::create( tokens[ 1 ], fg ) );
			return fg;
		} else if ( ( n == 3 || n == 4 ) && ( is_ulong( tokens[ 0 ] ) || ( tokens[ 0 ] == "*" ) ) && ( tokens[ 1 ] == "(" ) && ( tokens.back() == ")" ) ) { // Paren group with repeat count
			Size const r( is_ulong( tokens[ 0 ] ) ? ulong_of( tokens[ 0 ] ) : static_cast< Size >( -1 ) ); //Do Flag * as infinite repeat
			FormatGroup * fg( nested ? static_cast< FormatGroup * >( new FormatGroupSub( p, r ) ) : static_cast< FormatGroup * >( new FormatGroupTop( r ) ) );
			if ( n == 4 ) fg->format( FormatFactory::create( tokens[ 2 ], fg ) );
			return fg;
		} else if ( n > 1 ) { // Comma-separated list
			FormatList * fl( new FormatList( p ) );
			Formats formats;
			for ( Tokens::size_type i = 0, e = tokens.size(); i < e; ++i ) {
				Token const & token( tokens[ i ] );
				if ( ( is_ulong( token ) || ( token == "*" ) ) && ( i + 2 < e ) ) { // Paren group with repeat count?
					if ( tokens[ i + 1 ] == "(" ) {
						if ( tokens[ i + 2 ] == ")" ) { // Empty paren group
							Size const r( is_ulong( token ) ? ulong_of( token ) : static_cast< Size >( -1 ) ); //Do Flag * as infinite repeat
							formats.push_back( new FormatGroupSub( fl, r ) );
							i += 2;
						} else if ( ( i + 3 < e ) && ( tokens[ i + 3 ] == ")" ) ) { // Paren group
							Size const r( is_ulong( token ) ? ulong_of( token ) : static_cast< Size >( -1 ) ); //Do Flag * as infinite repeat
							FormatGroupSub * fg( new FormatGroupSub( fl, r ) );
							fg->format( FormatFactory::create( tokens[ i + 2 ], fg ) );
							formats.push_back( fg );
							i += 3;
						} else {
							formats.push_back( FormatFactory::create( token, fl ) );
						}
					}
				} else if ( ( token == "(" ) && ( i + 1 < e ) ) { // Paren group?
					if ( tokens[ i + 1 ] == ")" ) { // Empty paren group
						formats.push_back( new FormatGroupSub( fl ) );
						i += 1;
					} else if ( ( i + 2 < e ) && ( tokens[ i + 2 ] == ")" ) ) { // Paren group
						FormatGroupSub * fg( new FormatGroupSub( fl ) );
						fg->format( FormatFactory::create( tokens[ i + 1 ], fg ) );
						formats.push_back( fg );
						i += 2;
					} else {
						formats.push_back( FormatFactory::create( token, fl ) );
					}
				} else {
					formats.push_back( FormatFactory::create( token, fl ) );
				}
			}
			fl->formats( formats );
			return fl;
		} else if ( n == 1 ) { // Single token
			token = tokens[ 0 ];
			if ( token == "*" ) { // List-directed
				if ( ( ! p ) || ( ! p->p() ) ) { // Only allowed as sole item at top-level
					return new FormatLD( p );
				} else {
					return nullptr;
				}
			} else if ( token == "/" ) { // Line feed
				return new FormatLinefeed( p );
			} else if ( token == ":" ) { // Colon: Terminate processing when no more items
				return new FormatColon( p );
			} else if ( ( token == "$" ) || ( token == "\\" ) ) { // Non-advancing i/o
				return new FormatDollar( p );
			} else if ( ( token[ 0 ] == '"' ) && ( token.back() == '"' ) ) { // Literal string
				Token::size_type const l( token.length() );
				if ( l > 2 ) {
					return new FormatString( p, token.substr( 1, token.length() - 2 ) );
				} else {
					return new FormatString( p, std::string() );
				}
			} else if ( ( token[ 0 ] == '\'' ) && ( token.back() == '\'' ) ) { // Literal character
				Token::size_type const l( token.length() );
				if ( l > 2 ) {
					return new FormatChar( p, token.substr( 1, token.length() - 2 ) );
				} else {
					return new FormatChar( p, std::string() );
				}
			} else {
				Tokens const parts( tokenize1( token ) );
				Token const alpha( alpha_token( parts ) );
				Tokens::size_type const m( parts.size() );
				if ( ( m == 2 ) && ( is_ulong( parts[ 0 ] ) ) && ( parts[ 1 ] == "/" ) ) { // r/ Line feed with repeat count
					return new FormatLinefeed( p, ulong_of( parts[ 0 ] ) );
				} else if ( alpha == "BN" ) { // BN
					return new FormatBN( p );
				} else if ( alpha == "BZ" ) { // BZ
					return new FormatBZ( p );
				} else if ( alpha == "S" ) { // S
					return new FormatS( p );
				} else if ( alpha == "SP" ) { // SP
					return new FormatSP( p );
				} else if ( alpha == "SS" ) { // SS
					return new FormatSS( p );
				} else if ( alpha == "SU" ) { // SU
					return new FormatSU( p );
				} else if ( alpha == "X" ) { // [r]X
					return new FormatX( p, ( ( m == 2 ) && is_ulong( parts[ 0 ] ) ? ulong_of( parts[ 0 ] ) : 1ul ) );
				} else if ( alpha == "R" ) { // [r]R
					return new FormatR( p, ( ( m == 2 ) && is_ulong( parts[ 0 ] ) ? ulong_of( parts[ 0 ] ) : 1ul ) );
				} else if ( alpha == "T" ) { // Tn
					return new FormatT( p, ( ( m == 2 ) && is_ulong( parts[ 1 ] ) ? ulong_of( parts[ 1 ] ) : 1ul ) );
				} else if ( alpha == "TL" ) { // TLn
					return new FormatTL( p, ( ( m == 2 ) && is_ulong( parts[ 1 ] ) ? ulong_of( parts[ 1 ] ) : 1ul ) );
				} else if ( alpha == "TR" ) { // TRn
					return new FormatTR( p, ( ( m == 2 ) && is_ulong( parts[ 1 ] ) ? ulong_of( parts[ 1 ] ) : 1ul ) );
				} else if ( alpha == "A" ) { // [r]A[w]
					if ( m == 1 ) { // A
						return new FormatA( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rA
							return new FormatA( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Aw
							return new FormatA( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad A descriptor
							return new FormatA( p );
						}
					} else { // rAw
						return new FormatA( p, is_ulong( parts[ 0 ] ) ? ulong_of( parts[ 0 ] ) : 1ul, is_ulong( parts[ 2 ] ) ? ulong_of( parts[ 2 ] ) : 1ul );
					}
				} else if ( alpha == "L" ) { // [r]L[w]
					if ( m == 1 ) { // L
						return new FormatL( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rL
							return new FormatL( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Lw
							return new FormatL( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad L descriptor
							return new FormatL( p );
						}
					} else { // rLw
						return new FormatL( p, is_ulong( parts[ 0 ] ) ? ulong_of( parts[ 0 ] ) : 1ul, is_ulong( parts[ 2 ] ) ? ulong_of( parts[ 2 ] ) : 1ul );
					}
				} else if ( alpha == "B" ) { // [r]Bw[.m]
					if ( m == 1 ) { // B
						return new FormatB( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rB
							return new FormatB( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Bw
							return new FormatB( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad B descriptor
							return new FormatB( p );
						}
					} else if ( m == 3 ) { // rBw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rBw
							return new FormatB( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad B descriptor
							return new FormatB( p );
						}
					} else if ( m == 4 ) { // Bw.m
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Bw.m
							return new FormatB( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad B descriptor
							return new FormatB( p );
						}
					} else { // rBw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rBw.m
							return new FormatB( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad B descriptor
							return new FormatB( p );
						}
					}
				} else if ( alpha == "I" ) { // [r]Iw[.m]
					if ( m == 1 ) { // I
						return new FormatI( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rI
							return new FormatI( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Iw
							return new FormatI( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad I descriptor
							return new FormatI( p );
						}
					} else if ( m == 3 ) { // rIw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rIw
							return new FormatI( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad I descriptor
							return new FormatI( p );
						}
					} else if ( m == 4 ) { // Iw.m
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Iw.m
							return new FormatI( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad I descriptor
							return new FormatI( p );
						}
					} else { // rIw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rIw.m
							return new FormatI( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad I descriptor
							return new FormatI( p );
						}
					}
				} else if ( alpha == "O" ) { // [r]Ow[.m]
					if ( m == 1 ) { // O
						return new FormatO( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rO
							return new FormatO( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Ow
							return new FormatO( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad O descriptor
							return new FormatO( p );
						}
					} else if ( m == 3 ) { // rOw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rOw
							return new FormatO( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad O descriptor
							return new FormatO( p );
						}
					} else if ( m == 4 ) { // Ow.m
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Ow.m
							return new FormatO( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad O descriptor
							return new FormatO( p );
						}
					} else { // rOw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rOw.m
							return new FormatO( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad O descriptor
							return new FormatO( p );
						}
					}
				} else if ( alpha == "Z" ) { // [r]Zw[.m]
					if ( m == 1 ) { // Z
						return new FormatZ( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rZ
							return new FormatZ( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Zw
							return new FormatZ( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad Z descriptor
							return new FormatZ( p );
						}
					} else if ( m == 3 ) { // rZw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rZw
							return new FormatZ( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad Z descriptor
							return new FormatZ( p );
						}
					} else if ( m == 4 ) { // Zw.m
						if ( is_ulong( parts[ 1 ] ) && is_ulong( parts[ 3 ] ) && ( parts[ 2 ] == "." ) ) { // Zw.m
							return new FormatZ( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad Z descriptor
							return new FormatZ( p );
						}
					} else { // rZw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rZw.m
							return new FormatZ( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad Z descriptor
							return new FormatZ( p );
						}
					}
				} else if ( alpha == "P" ) { // [k]P
					return new FormatP( p, ( ( m == 2 ) && is_int( parts[ 0 ] ) ? int_of( parts[ 0 ] ) : 1 ) );
				} else if ( alpha == "F" ) { // [r]F[w.d]
					if ( m == 1 ) { // F
						return new FormatF( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rF
							return new FormatF( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Fw
							return new FormatF( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad F descriptor
							return new FormatF( p );
						}
					} else if ( m == 3 ) { // rFw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rFw
							return new FormatF( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad F descriptor
							return new FormatF( p );
						}
					} else if ( m == 4 ) { // Fw.d
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Fw.d
							return new FormatF( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad F descriptor
							return new FormatF( p );
						}
					} else { // rFw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rFw.d
							return new FormatF( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad F descriptor
							return new FormatF( p );
						}
					}
				} else if ( alpha == "G" ) { // [r]G[w.d[Ee]]
					if ( m == 1 ) { // G
						return new FormatG( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rG
							return new FormatG( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Gw
							return new FormatG( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad G descriptor
							return new FormatG( p );
						}
					} else if ( m == 3 ) { // rGw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rGw
							return new FormatG( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad G descriptor
							return new FormatG( p );
						}
					} else if ( m == 4 ) { // Gw.d
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Gw.d
							return new FormatG( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad G descriptor
							return new FormatG( p );
						}
					} else if ( m == 5 ) { // rGw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rGw.d
							return new FormatG( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad G descriptor
							return new FormatG( p );
						}
					} else if ( m == 6 ) { // Gw.dEe or Gw.d.e
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) && ( ( parts[ 4 ] == "E" ) || ( parts[ 4 ] == "." ) ) && is_ulong( parts[ 5 ] ) ) { // Gw.dEe or Gw.d.e
							return new FormatG( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ), ulong_of( parts[ 5 ] ) );
						} else { // Bad G descriptor
							return new FormatG( p );
						}

					} else { // rGw.dEe or rGw.d.e
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) && ( ( parts[ 5 ] == "E" ) || ( parts[ 5 ] == "." ) ) && is_ulong( parts[ 6 ] ) ) { // rGw.dEe or rGw.d.e
							return new FormatG( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ), ulong_of( parts[ 6 ] ) );
						} else { // Bad G descriptor
							return new FormatG( p );
						}
					}
				} else if ( alpha == "E" ) { // [r]E[w.d[Ee]]
					if ( m == 1 ) { // E
						return new FormatE( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rE
							return new FormatE( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Ew
							return new FormatE( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad E descriptor
							return new FormatE( p );
						}
					} else if ( m == 3 ) { // rEw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rEw
							return new FormatE( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad E descriptor
							return new FormatE( p );
						}
					} else if ( m == 4 ) { // Ew.d
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Ew.d
							return new FormatE( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad E descriptor
							return new FormatE( p );
						}
					} else if ( m == 5 ) { // rEw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rEw.d
							return new FormatE( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad E descriptor
							return new FormatE( p );
						}
					} else if ( m == 6 ) { // Ew.dEe or Ew.d.e
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) && ( ( parts[ 4 ] == "E" ) || ( parts[ 4 ] == "." ) ) && is_ulong( parts[ 5 ] ) ) { // Ew.dEe or Ew.d.e
							return new FormatE( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ), ulong_of( parts[ 5 ] ) );
						} else { // Bad E descriptor
							return new FormatE( p );
						}

					} else { // rEw.dEe or rEw.d.e
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) && ( ( parts[ 5 ] == "E" ) || ( parts[ 5 ] == "." ) ) && is_ulong( parts[ 6 ] ) ) { // rEw.dEe or rEw.d.e
							return new FormatE( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ), ulong_of( parts[ 6 ] ) );
						} else { // Bad E descriptor
							return new FormatE( p );
						}
					}
				} else if ( alpha == "EN" ) { // [r]EN[w.d[Ee]]
					if ( m == 1 ) { // EN
						return new FormatEN( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rEN
							return new FormatEN( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // ENw
							return new FormatEN( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad EN descriptor
							return new FormatEN( p );
						}
					} else if ( m == 3 ) { // rENw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rENw
							return new FormatEN( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad EN descriptor
							return new FormatEN( p );
						}
					} else if ( m == 4 ) { // ENw.d
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // ENw.d
							return new FormatEN( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad EN descriptor
							return new FormatEN( p );
						}
					} else if ( m == 5 ) { // rENw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rENw.d
							return new FormatEN( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad EN descriptor
							return new FormatEN( p );
						}
					} else if ( m == 6 ) { // ENw.dEe or ENw.d.e
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) && ( ( parts[ 4 ] == "E" ) || ( parts[ 4 ] == "." ) ) && is_ulong( parts[ 5 ] ) ) { // ENw.dEe or ENw.d.e
							return new FormatEN( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ), ulong_of( parts[ 5 ] ) );
						} else { // Bad EN descriptor
							return new FormatEN( p );
						}

					} else { // rENw.dEe or rENw.d.e
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) && ( ( parts[ 5 ] == "E" ) || ( parts[ 5 ] == "." ) ) && is_ulong( parts[ 6 ] ) ) { // rENw.dEe or rENw.d.e
							return new FormatEN( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ), ulong_of( parts[ 6 ] ) );
						} else { // Bad EN descriptor
							return new FormatEN( p );
						}
					}
				} else if ( alpha == "ES" ) { // [r]ES[w.d[Ee]]
					if ( m == 1 ) { // ES
						return new FormatES( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rES
							return new FormatES( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // ESw
							return new FormatES( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad ES descriptor
							return new FormatES( p );
						}
					} else if ( m == 3 ) { // rESw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rESw
							return new FormatES( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad ES descriptor
							return new FormatES( p );
						}
					} else if ( m == 4 ) { // ESw.d
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // ESw.d
							return new FormatES( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad ES descriptor
							return new FormatES( p );
						}
					} else if ( m == 5 ) { // rESw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rESw.d
							return new FormatES( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad ES descriptor
							return new FormatES( p );
						}
					} else if ( m == 6 ) { // ESw.dEe or ESw.d.e
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) && ( ( parts[ 4 ] == "E" ) || ( parts[ 4 ] == "." ) ) && is_ulong( parts[ 5 ] ) ) { // ESw.dEe or ESw.d.e
							return new FormatES( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ), ulong_of( parts[ 5 ] ) );
						} else { // Bad ES descriptor
							return new FormatES( p );
						}

					} else { // rESw.dEe or rESw.d.e
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) && ( ( parts[ 5 ] == "E" ) || ( parts[ 5 ] == "." ) ) && is_ulong( parts[ 6 ] ) ) { // rESw.dEe or rESw.d.e
							return new FormatES( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ), ulong_of( parts[ 6 ] ) );
						} else { // Bad ES descriptor
							return new FormatES( p );
						}
					}
				} else if ( alpha == "D" ) { // [r]D[w.d[Ee]]
					if ( m == 1 ) { // D
						return new FormatD( p );
					} else if ( m == 2 ) {
						if ( is_ulong( parts[ 0 ] ) ) { // rD
							return new FormatD( p, ulong_of( parts[ 0 ] ) );
						} else if ( is_ulong( parts[ 1 ] ) ) { // Dw
							return new FormatD( p, 1ul, ulong_of( parts[ 1 ] ) );
						} else { // Bad D descriptor
							return new FormatD( p );
						}
					} else if ( m == 3 ) { // rDw
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) ) { // rDw
							return new FormatD( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ) );
						} else { // Bad D descriptor
							return new FormatD( p );
						}
					} else if ( m == 4 ) { // Dw.d
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) ) { // Dw.d
							return new FormatD( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ) );
						} else { // Bad D descriptor
							return new FormatD( p );
						}
					} else if ( m == 5 ) { // rDw.d
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) ) { // rDw.d
							return new FormatD( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ) );
						} else { // Bad D descriptor
							return new FormatD( p );
						}
					} else if ( m == 6 ) { // Dw.dEe or Dw.d.e
						if ( is_ulong( parts[ 1 ] ) && ( parts[ 2 ] == "." ) && is_ulong( parts[ 3 ] ) && ( ( parts[ 4 ] == "E" ) || ( parts[ 4 ] == "." ) ) && is_ulong( parts[ 5 ] ) ) { // Dw.dEe or Dw.d.e
							return new FormatD( p, 1ul, ulong_of( parts[ 1 ] ), ulong_of( parts[ 3 ] ), ulong_of( parts[ 5 ] ) );
						} else { // Bad D descriptor
							return new FormatD( p );
						}

					} else { // rDw.dEe or rDw.d.e
						if ( is_ulong( parts[ 0 ] ) && is_ulong( parts[ 2 ] ) && ( parts[ 3 ] == "." ) && is_ulong( parts[ 4 ] ) && ( ( parts[ 5 ] == "E" ) || ( parts[ 5 ] == "." ) ) && is_ulong( parts[ 6 ] ) ) { // rDw.dEe or rDw.d.e
							return new FormatD( p, ulong_of( parts[ 0 ] ), ulong_of( parts[ 2 ] ), ulong_of( parts[ 4 ] ), ulong_of( parts[ 6 ] ) );
						} else { // Bad D descriptor
							return new FormatD( p );
						}
					}
				} else { // Unsupported
					return nullptr;
				}
			}
		} else { // Won't get here
			return nullptr;
		}
	}

	// Static Data Member Definitions
	Format::Size const Format::NOSIZE = static_cast< Size >( -1 );
	std::string const Format::LF = std::string( "\n" );

} // ObjexxFCL
