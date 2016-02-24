#ifndef ObjexxFCL_Format_hh_INCLUDED
#define ObjexxFCL_Format_hh_INCLUDED

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
#include <ObjexxFCL/noexcept.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/TraitsA.hh>
#include <ObjexxFCL/TraitsB.hh>
#include <ObjexxFCL/TraitsF.hh>
#include <ObjexxFCL/TraitsG.hh>
#include <ObjexxFCL/TraitsI.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <istream>
#include <limits>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// List-Directed Input Entry
struct EntryFormatLD
{

	typedef  std::size_t  Size;

	// Default Constructor
	EntryFormatLD() :
	 h( false ),
	 r( 0ul )
	{}

	// Constructor
	explicit
	EntryFormatLD( std::string const & str, bool const has = true, Size const rep = 1ul ) :
	 s( str ),
	 h( has ),
	 r( has ? rep : 0ul )
	{}

	// Has a Non-Null Entry? (And decrement counter)
	bool
	has()
	{
		if ( r > 0ul ) {
			--r;
			return h;
		} else { // Counter was at zero
			return false;
		}
	}

	// Reset
	void
	reset()
	{
		s.clear();
		h = false;
		r = 0ul;
	}

	std::string s; // Entry string
	bool h; // Has non-null entry?
	Size r; // Repeat counter

}; // EntryFormatLD

// Format Base Class
class Format
{

private: // Friends

	friend class FormatGroupTop;

public: // Types

	typedef  std::size_t  Size;
	typedef  std::string  Token;
	typedef  std::vector< Token >  Tokens;
	typedef  std::vector< Format * >  Formats;

protected: // Creation

	// Repeat Constructor
	explicit
	Format( Format * p, Size const r = 1ul ) :
	 p_( p ),
	 r_( r ),
	 u_( false ),
	 i_( 0ul )
	{}

	// Star Constructor
	Format( Format * p, char const star ) :
	 p_( p ),
	 r_( -1 ),
	 u_( true ),
	 i_( 0ul )
	{
		assert( star == '*' );
#ifdef NDEBUG
		static_cast< void const >( star ); // Suppress unused warning
#endif
	}

	// Copy Constructor
	Format( Format const & f, Format * p = nullptr ) :
	 p_( p ? p : f.p_ ),
	 r_( f.r_ ),
	 u_( f.u_ ),
	 i_( 0ul )
	{}

	// Move Constructor
	Format( Format && f ) NOEXCEPT :
	 p_( f.p_ ),
	 r_( f.r_ ),
	 u_( f.u_ ),
	 i_( 0ul )
	{}

public: // Creation

	// Clone
	virtual
	Format *
	clone( Format * p = nullptr ) const = 0;

	// Destructor
	virtual
	~Format()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( Format const & f )
	{
		if ( this != &f ) {
			p_ = f.p_;
			r_ = f.r_;
			u_ = f.u_;
			i_ = 0ul;
		}
	}

public: // Properties

	// Parent Format
	Format const *
	p() const
	{
		return p_;
	}

	// Parent Format
	Format *
	p()
	{
		return p_;
	}

	// Repeat Count
	Size
	r() const
	{
		return r_;
	}

	// Unlimited Repeat?
	bool
	u() const
	{
		return u_;
	}

	// Uses an Argument?
	virtual
	bool
	uses_arg() const
	{ // Default implementation
		return false;
	}

	// Uses an Argument?
	bool
	no_arg() const
	{
		return ! uses_arg();
	}

	// Format Group?
	virtual
	bool
	is_group() const
	{ // Default implementation
		return false;
	}

	// Format List?
	virtual
	bool
	is_list() const
	{ // Default implementation
		return false;
	}

	// FormatLD?
	virtual
	bool
	is_FormatLD() const
	{ // Default implementation
		return false;
	}

	// List-Directed?
	virtual
	bool
	is_list_directed() const
	{ // Default implementation
		return false;
	}

	// Can Repeat with Given Index?
	bool
	can_repeat( Size const i ) const
	{
		return ( u_ || ( i < r_ ) );
	}

	// P Scaling State
	virtual
	int
	P() const
	{
		assert( p_ );
		return p_->P();
	}

	// P Scaling State
	virtual
	int &
	P()
	{
		assert( p_ );
		return p_->P();
	}

	// Blank Null?
	bool
	blank_null() const
	{
		return ! blank_zero();
	}

	// Blank Zero?
	virtual
	bool
	blank_zero() const
	{
		assert( p_ );
		return p_->blank_zero();
	}

	// Blank Zero?
	virtual
	bool &
	blank_zero()
	{
		assert( p_ );
		return p_->blank_zero();
	}

	// Colon Terminated?
	virtual
	bool
	colon_terminated() const
	{
		assert( p_ );
		return p_->colon_terminated();
	}

	// Colon Terminated?
	virtual
	bool &
	colon_terminated()
	{
		assert( p_ );
		return p_->colon_terminated();
	}

	// Not Colon Terminated?
	bool
	not_colon_terminated() const
	{
		return ! colon_terminated();
	}

	// Slash Terminated?
	virtual
	bool
	slash_terminated() const
	{
		assert( p_ );
		return p_->slash_terminated();
	}

	// Slash Terminated?
	virtual
	bool &
	slash_terminated()
	{
		assert( p_ );
		return p_->slash_terminated();
	}

	// Not Slash Terminated?
	bool
	not_slash_terminated() const
	{
		return ! slash_terminated();
	}

	// Terminated?
	bool
	terminated() const
	{
		return ( colon_terminated() || slash_terminated() );
	}

	// Not Terminated?
	bool
	not_terminated() const
	{
		return ( ( ! colon_terminated() ) && ( ! slash_terminated() ) );
	}

	// Non-Advancing?
	virtual
	bool
	non_advancing() const
	{
		assert( p_ );
		return p_->non_advancing();
	}

	// Non-Advancing?
	virtual
	bool &
	non_advancing()
	{
		assert( p_ );
		return p_->non_advancing();
	}

	// Reverted?
	virtual
	bool
	reverted() const
	{
		assert( p_ );
		return p_->reverted();
	}

	// Reverted?
	virtual
	bool &
	reverted()
	{
		assert( p_ );
		return p_->reverted();
	}

	// Revert Count
	virtual
	Size
	reverts() const
	{
		assert( p_ );
		return p_->reverts();
	}

	// Spacer?
	virtual
	bool
	spacer() const
	{
		assert( p_ );
		return p_->spacer();
	}

	// Spacer?
	virtual
	bool &
	spacer()
	{
		assert( p_ );
		return p_->spacer();
	}

	// Current Format
	virtual
	Format *
	current() = 0;

	// Next Format
	virtual
	Format *
	next() = 0;

	// Next Format: Upward Call
	virtual
	Format *
	next_up() = 0;

public: // Methods

	// Reset
	virtual
	Format &
	reset()
	{
		i_ = 0ul;
		return *this;
	}

public: // Input Methods

	// Input without Argument
	std::istream &
	input( std::istream & stream, std::streampos const poa, std::streampos & por )
	{
		in( stream, poa, por );
		return stream;
	}

	// Input without Argument
	std::istream &
	input( std::istream & stream, std::streampos & por )
	{
		in( stream, por );
		return stream;
	}

	// Input with Argument
	template< typename T >
	std::istream &
	input( std::istream & stream, std::streampos const poa, std::streampos & por, T & t )
	{
		stream.seekg( poa + por, std::ios::beg ); // Position the stream at the virtual position: Could check that it is <= end but read could still exceed stream bounds
		in( stream, t );
		por = stream.tellg() - poa; // After reading a value the current stream position is also the virtual position
		return stream;
	}

	// Input with Argument
	template< typename T >
	std::istream &
	input( std::istream & stream, std::streampos & por, T & t )
	{
		stream.seekg( por, std::ios::beg ); // Position the stream at the virtual position: Could check that it is <= end but read could still exceed stream bounds
		in( stream, t );
		por = stream.tellg(); // After reading a value the current stream position is also the virtual position
		return stream;
	}

	// Input Pad/Position
	void
	input_pos( std::istream & stream, std::streampos const pos ) // pos == absolute position
	{
		stream.seekg( pos, std::ios::beg ); // Position the stream at the virtual position
	}

	// Input
	virtual
	void
	in( std::istream &, std::streampos const, std::streampos & )
	{} // Default implementation

	// Input
	virtual
	void
	in( std::istream &, std::streampos & )
	{} // Default implementation

	// Input
	virtual
	void
	in( std::istream & stream, bool & b )
	{ // Default implementation
		skip_chunk( stream );
		b = false;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, byte & b );

	// Input
	virtual
	void
	in( std::istream & stream, ubyte & b );

	// Input
	virtual
	void
	in( std::istream & stream, short int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, unsigned short int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, unsigned int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, long int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, unsigned long int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, long long int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, unsigned long long int & i )
	{ // Default implementation
		skip_chunk( stream );
		i = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, float & v )
	{ // Default implementation
		skip_chunk( stream );
		v = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, double & v )
	{ // Default implementation
		skip_chunk( stream );
		v = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, long double & v )
	{ // Default implementation
		skip_chunk( stream );
		v = 0;
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, char & c )
	{ // Default implementation
		skip_chunk( stream );
		c = ' ';
		io_err( stream );
	}

	// Input
	virtual
	void
	in( std::istream & stream, std::string & s )
	{ // Default implementation
		skip_chunk( stream );
		s.clear();
		io_err( stream );
	}

public: // Output Methods

	// Output without Argument
	std::ostream &
	output_no_arg( std::ostream & stream, std::streampos & pos, std::string const & ter = LF )
	{
		out_pos( stream, pos, ter );
		return stream;
	}

	// Output a Value
	template< typename T >
	std::ostream &
	output_val( std::ostream & stream, std::streampos & pos, T const & t, std::string const & ter = LF )
	{
		output_pad( stream, pos );
		out( stream, t, ter );
		pos = stream.tellp(); // After writing a value the current stream position is also the virtual position
		return stream;
	}

	// Output Pad/Position
	void
	output_pos( std::ostream & stream, std::streampos const pos )
	{
		output_pad( stream, pos );
	}

	// Spacing and Reversion Linefeed String for a Non-Spacer
	std::string
	spc( std::string const & ter );

	// Spacing and Reversion Linefeed String for a Spacer
	std::string
	spc_spacer( std::string const & ter );

	// Output
	virtual
	void
	out_pos( std::ostream &, std::streampos &, std::string const & )
	{} // Default implementation

	// Output
	virtual
	void
	out( std::ostream & stream, bool const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, byte const &, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, ubyte const &, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, short int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, unsigned short int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, unsigned int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, long int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, unsigned long int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, long long int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, unsigned long long int const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, float const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, double const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, long double const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, char const, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

	// Output
	void
	out( std::ostream & stream, char const * s, std::string const & ter ) // Non-virtual forwarding wrapper
	{
		return out( stream, std::string( s ), ter );
	}

	// Output
	virtual
	void
	out( std::ostream & stream, std::string const &, std::string const & )
	{ // Default implementation
		io_err( stream );
	}

public: // Static Methods

	// Skip Rest of Line and Line Terminator (Manipulator)
	static
	std::istream &
	skip( std::istream & stream )
	{
		return stream.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
	}

	// Skip Over a Number of Characters in a Stream
	static
	void
	skip( std::istream & stream, Size const w = 1ul );

protected: // Properties

	// Active index
	Size
	i() const
	{
		return i_;
	}

	// Active index
	Size &
	i()
	{
		return i_;
	}

protected: // Methods

	// Read List-Directed Entry from a Stream
	EntryFormatLD
	read_ld( std::istream & stream, bool const numeric = false, char const mode = 'C' ); // Use mode = 'F' for reading files that can't be changed to C-style quote escapes

	// Skip Chunk from Stream and Discard
	void
	skip_chunk( std::istream & stream, Size const w = NOSIZE ) const
	{
		skip( stream, wid( w ) );
	}

	// Width to Read
	virtual
	Size
	wid( Size const w = NOSIZE ) const
	{ // Default implementation
		return ( w == NOSIZE ? Size( 0 ) : w );
	}

	// Apply Blank Processing to String
	std::string
	blank_process( std::string const & s ) const;

protected: // Static Methods

	// Read from a Stream
	static
	std::string
	read( std::istream & stream, Size const w = NOSIZE );

	// Read Floating Point from a Stream
	static
	std::string
	read_float( std::istream & stream, Size const w = NOSIZE );

	// string is Blank?
	static
	bool
	is_blank( std::string const & s )
	{
		return ( s.empty() ? true : s.find_first_not_of( ' ' ) == std::string::npos );
	}

	// Character is a List-Directed Whitespace Separator?
	static
	bool
	is_space_ld( char const c )
	{
#ifdef OBJEXXFCL_INTEL_LIST_DIRECTED_IO
		return ( ( c == ' ' ) || ( c == '\t' ) ); // Intel Fortran treats tabs as separators
#else
		return ( c == ' ' );
#endif
	}

	// Number of a String
	template< typename T >
	static
	T
	number_of( std::string const & s )
	{
		if ( is_type< T >( s ) ) {
			return type_of< T >( s );
		} else { // Bad input
			return 0;
		}
	}

	// Float String has Exponent?
	static
	bool
	has_exponent( std::string const & s )
	{
		return has_any_of( s, "Ee" ); // Assumes D/d already converted to E/e
	}

	// Pad Output Stream to Output Position
	static
	void
	output_pad( std::ostream & stream, std::streampos const pos )
	{
		stream.seekp( 0, std::ios::end );
		std::streampos const end( stream.tellp() );
		if ( pos > end ) {
			stream << std::string( pos - end, ' ' ); // Space fill before output
		} else if ( pos < end ) {
			stream.seekp( pos, std::ios::beg ); // Position the stream at the virtual position: Could check that it is <= end but read could still exceed stream bounds
		}
	}

	// Clear EOF State if No Error
	static
	void
	clear_eof( std::istream & stream )
	{
		if ( stream && stream.eof() ) stream.clear( stream.rdstate() & ~std::ios::eofbit );
	}

	// Set Stream Fail Bit for I/O Error
	static
	void
#ifndef OBJEXXFCL_IO_RELAX
	io_err( std::ios & stream )
	{
		stream.setstate( std::ios::failbit );
	}
#else
	io_err( std::ios & ) // Avoid unused argument warnings
	{}
#endif

private: // Data

	Format * p_; // Parent format (non-owning)
	Size r_; // Repeat count
	bool u_; // Unlimited repeat?
	Size i_; // Active repeat or member index

protected: // Static Data

	static Size const NOSIZE;
	static std::string const LF;

}; // Format

// Format Combining Child Formats Base Class
class FormatCombo : public Format
{

protected: // Creation

	// Constructor
	explicit
	FormatCombo( Format * p, Size const r = 1ul ) :
	 Format( p, r )
	{}

	// Constructor
	FormatCombo( Format * p, char const star ) :
	 Format( p, star )
	{}

	// Copy Constructor
	FormatCombo( FormatCombo const & f, Format * p = nullptr ) :
	 Format( f, p )
	{}

	// Move Constructor
	FormatCombo( FormatCombo && f ) NOEXCEPT :
	 Format( std::move( f ) )
	{}

public: // Creation

	// Destructor
	virtual
	~FormatCombo()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( FormatCombo const & f )
	{
		if ( this != &f ) {
			Format::operator =( f );
		}
	}

}; // FormatCombo

// Format Expression for List of Formats
class FormatList : public FormatCombo
{

public: // Creation

	// Constructor
	explicit
	FormatList( Format * p, Formats const & formats = Formats() ) :
	 FormatCombo( p ),
	 formats_( formats )
	{}

	// Copy Constructor
	FormatList( FormatList const & f, Format * p = nullptr ) :
	 FormatCombo( f, p )
	{
		for ( Format * format : f.formats() ) formats_.push_back( format->clone( this ) );
	}

	// Move Constructor
	FormatList( FormatList && f ) NOEXCEPT :
	 FormatCombo( std::move( f ) ),
	 formats_( std::move( f.formats_ ) )
	{
		f.formats_.clear();
	}

	// Clone
	FormatList *
	clone( Format * p = nullptr ) const
	{
		return new FormatList( *this, p );
	}

	// Destructor
	virtual
	~FormatList()
	{
		for ( Format * format : formats_ ) delete format;
	}

public: // Assignment

	// Copy Assignment
	FormatList &
	operator =( FormatList const & f );

public: // Properties

	// Format List?
	bool
	is_list() const
	{
		return true;
	}

	// Current Format
	Format *
	current()
	{
		return ( ! formats_.empty() ? ( formats_[ i() ] ? formats_[ i() ]->current() : nullptr ) : nullptr );
	}

	// Next Format
	Format *
	next()
	{
		return ( ! formats_.empty() ? ( formats_[ i() ] ? formats_[ i() ]->next() : nullptr ) : nullptr );
	}

	// Next Format: Upward Call
	Format *
	next_up();

	// Size
	Size
	size() const
	{
		return formats_.size();
	}

	// Formats
	Formats const &
	formats() const
	{
		return formats_;
	}

	// Formats
	Formats &
	formats()
	{
		return formats_;
	}

	// Set the Formats
	void
	formats( Formats & f )
	{
		for ( Format * format : formats_ ) delete format;
		formats_ = f;
	}

public: // Methods

	// Reset
	FormatList &
	reset()
	{
		FormatCombo::reset();
		for ( Format * format : formats_ ) format->reset();
		return *this;
	}

private: // Data

	Formats formats_;

}; // FormatList

// Format Group Base Class
class FormatGroup : public FormatCombo
{

protected: // Creation

	// Constructor
	explicit
	FormatGroup( Format * p, Format * format = nullptr ) :
	 FormatCombo( p ),
	 format_( format )
	{}

	// Size Constructor
	FormatGroup( Format * p, Size const r, Format * format = nullptr ) :
	 FormatCombo( p, r ),
	 format_( format )
	{}

	// Star Constructor
	FormatGroup( Format * p, char const star, Format * format = nullptr ) :
	 FormatCombo( p, star ),
	 format_( format )
	{}

	// Copy Constructor
	FormatGroup( FormatGroup const & f, Format * p = nullptr ) :
	 FormatCombo( f, p ),
	 format_( f.format() ? f.format()->clone( this ) : nullptr )
	{}

	// Move Constructor
	FormatGroup( FormatGroup && f ) NOEXCEPT :
	 FormatCombo( std::move( f ) ),
	 format_( f.format_ )
	{
		f.format_ = nullptr;
	}

public: // Creation

	// Destructor
	virtual
	~FormatGroup()
	{
		if ( format_ ) delete format_;
	}

protected: // Assignment

	// Copy Assignment
	FormatGroup &
	operator =( FormatGroup const & f );

public: // Properties

	// Format Group?
	bool
	is_group() const
	{
		return true;
	}

	// Current Format
	Format *
	current()
	{
		return ( format_ ? format_->current() : nullptr );
	}

	// Next Format
	Format *
	next()
	{
		return ( format_ ? format_->next() : nullptr );
	}

	// Next Format: Upward Call
	Format *
	next_up();

	// Format
	Format const *
	format() const
	{
		return format_;
	}

	// Format
	Format *
	format()
	{
		return format_;
	}

	// Set the Format
	virtual
	void
	format( Format * f )
	{
		if ( format_ ) delete format_;
		format_ = f;
	}

public: // Methods

	// Reset
	FormatGroup &
	reset()
	{
		FormatCombo::reset();
		if ( format_ ) format_->reset();
		return *this;
	}

private: // Data

	Format * format_;

}; // FormatGroup

// Format Top Group
class FormatGroupTop : public FormatGroup
{

public: // Types

	using FormatGroup::format;

public: // Creation

	// Constructor
	explicit
	FormatGroupTop( Format * format = nullptr ) :
	 FormatGroup( nullptr, format ),
	 P_( 0 ),
	 blank_zero_( false ),
	 colon_terminated_( false ),
	 slash_terminated_( false ),
	 non_advancing_( false ),
	 reverted_( false ),
	 reverts_( 0ul ),
	 ir_( 0ul ),
	 fr_( nullptr ),
	 spacer_( false )
	{}

	// Size Constructor
	explicit
	FormatGroupTop( Size const r, Format * format = nullptr ) :
	 FormatGroup( nullptr, r, format ),
	 P_( 0 ),
	 blank_zero_( false ),
	 colon_terminated_( false ),
	 slash_terminated_( false ),
	 non_advancing_( false ),
	 reverted_( false ),
	 reverts_( 0ul ),
	 ir_( 0ul ),
	 fr_( nullptr ),
	 spacer_( false )
	{}

	// Star Constructor
	explicit
	FormatGroupTop( char const star, Format * format = nullptr ) :
	 FormatGroup( nullptr, star, format ),
	 P_( 0 ),
	 blank_zero_( false ),
	 colon_terminated_( false ),
	 slash_terminated_( false ),
	 non_advancing_( false ),
	 reverted_( false ),
	 reverts_( 0ul ),
	 ir_( 0ul ),
	 fr_( nullptr ),
	 spacer_( false )
	{}

	// Copy Constructor
	FormatGroupTop( FormatGroupTop const & f, Format * p = nullptr ) :
	 FormatGroup( f, p ),
	 P_( 0 ),
	 blank_zero_( false ),
	 colon_terminated_( false ),
	 slash_terminated_( false ),
	 non_advancing_( false ),
	 reverted_( false ),
	 reverts_( 0ul ),
	 ir_( 0ul ),
	 fr_( nullptr ),
	 spacer_( false )
	{}

	// Move Constructor
	FormatGroupTop( FormatGroupTop && f ) NOEXCEPT :
	 FormatGroup( std::move( f ) ),
	 P_( 0 ),
	 blank_zero_( false ),
	 colon_terminated_( false ),
	 slash_terminated_( false ),
	 non_advancing_( false ),
	 reverted_( false ),
	 reverts_( 0ul ),
	 ir_( 0ul ),
	 fr_( f.fr_ ),
	 spacer_( false )
	{
		f.fr_ = nullptr;
	}

	// Clone
	FormatGroupTop *
	clone( Format * p = nullptr ) const
	{
		return new FormatGroupTop( *this, p );
	}

	// Destructor
	virtual
	~FormatGroupTop()
	{}

public: // Assignment

	// Copy Assignment
	FormatGroupTop &
	operator =( FormatGroupTop const & f )
	{
		if ( this != &f ) {
			FormatGroup::operator =( f );
			P_ = 0;
			blank_zero_ = false;
			colon_terminated_ = false;
			slash_terminated_ = false;
			non_advancing_ = false;
			reverted_ = false;
			reverts_ = 0ul;
			ir_ = 0ul;
			fr_ = nullptr;
			spacer_ = false;
		}
		return *this;
	}

public: // Properties

	// List-Directed?
	bool
	is_list_directed() const
	{
		return format()->is_FormatLD(); // Format containing only * is considered list-directed
	}

	// P Scaling State
	int
	P() const
	{
		return P_;
	}

	// P Scaling State
	int &
	P()
	{
		return P_;
	}

	// Blank Zero?
	bool
	blank_zero() const
	{
		return blank_zero_;
	}

	// Blank Zero?
	bool &
	blank_zero()
	{
		return blank_zero_;
	}

	// Colon Terminated?
	bool
	colon_terminated() const
	{
		return colon_terminated_;
	}

	// Colon Terminated?
	bool &
	colon_terminated()
	{
		return colon_terminated_;
	}

	// Slash Terminated?
	bool
	slash_terminated() const
	{
		return slash_terminated_;
	}

	// Slash Terminated?
	bool &
	slash_terminated()
	{
		return slash_terminated_;
	}

	// Non-Advancing?
	bool
	non_advancing() const
	{
		return non_advancing_;
	}

	// Non-Advancing?
	bool &
	non_advancing()
	{
		return non_advancing_;
	}

	// Reverted?
	bool
	reverted() const
	{
		return reverted_;
	}

	// Reverted?
	bool &
	reverted()
	{
		return reverted_;
	}

	// Revert Count
	Size
	reverts() const
	{
		return reverts_;
	}

	// Spacer?
	bool
	spacer() const
	{
		return spacer_;
	}

	// Spacer?
	bool &
	spacer()
	{
		return spacer_;
	}

	// Next Format: Upward Call
	Format *
	next_up()
	{
		reverted_ = true;
		++reverts_;
		return revert();
	}

public: // Methods

	// Reset
	FormatGroupTop &
	reset()
	{
		FormatGroup::reset();
		P_ = 0;
		blank_zero_ = false;
		colon_terminated_ = false;
		slash_terminated_ = false;
		non_advancing_ = false;
		reverted_ = false;
		reverts_ = 0ul;
		ir_ = 0ul;
		fr_ = nullptr;
		spacer_ = false;
		return *this;
	}

private: // Methods

	// Reversion Format Setup
	Format *
	revert();

private: // Data

	int P_; // P scaling
	bool blank_zero_; // Treat blanks in numeric inputs as zero?
	bool colon_terminated_; // Colon terminated? (Stops all i/o after last item)
	bool slash_terminated_; // Slash terminated? (Stops list-directed read)
	bool non_advancing_; // Non-advancing output?
	bool reverted_; // Reverted since any output performed?
	Size reverts_; // Number of reversions
	Size ir_; // Reversion Format list index
	Format * fr_; // Reversion Format
	bool spacer_; // Spacer item output last?

}; // FormatGroupTop

// Format Sub-Group
class FormatGroupSub : public FormatGroup
{

public: // Creation

	// Constructor
	explicit
	FormatGroupSub( Format * p, Format * format = nullptr ) :
	 FormatGroup( p, format )
	{}

	// Constructor
	FormatGroupSub( Format * p, Size const r, Format * format = nullptr ) :
	 FormatGroup( p, r, format )
	{}

	// Constructor
	FormatGroupSub( Format * p, char const star, Format * format = nullptr ) :
	 FormatGroup( p, star, format )
	{}

	// Copy Constructor
	explicit
	FormatGroupSub( FormatGroupSub const & f, Format * p = nullptr ) :
	 FormatGroup( f, p )
	{}

	// Clone
	FormatGroupSub *
	clone( Format * p = nullptr ) const
	{
		return new FormatGroupSub( *this, p );
	}

	// Destructor
	virtual
	~FormatGroupSub()
	{}

}; // FormatGroupSub

// Format Leaf Base Class
class FormatLeaf : public Format
{

protected: // Creation

	// Constructor
	explicit
	FormatLeaf( Format * p, Size const r = 1ul ) :
	 Format( p, r )
	{}

	// Copy Constructor
	FormatLeaf( FormatLeaf const & f, Format * p = nullptr ) :
	 Format( f, p )
	{}

	// Move Constructor
	FormatLeaf( FormatLeaf && f ) NOEXCEPT :
	 Format( std::move( f ) )
	{}

public: // Creation

	// Destructor
	virtual
	~FormatLeaf()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( FormatLeaf const & f )
	{
		if ( this != &f ) {
			Format::operator =( f );
		}
	}

public: // Properties

	// Current Format
	Format *
	current()
	{
		return this;
	}

	// Next Format
	Format *
	next();

	// Next Format: Upward Call
	Format *
	next_up()
	{
		assert( false ); // next_up() should never be called on a leaf Format
		return nullptr; // Done
	}

}; // FormatLeaf

// Literal String Format
class FormatString : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatString( Format * p, std::string const & s ) :
	 FormatLeaf( p ),
	 s_( replaced( replaced( s, "\\\\", "\\" ), "\\\"", "\"" ) )
	{}

	// Copy Constructor
	explicit
	FormatString( FormatString const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 s_( f.s_ )
	{}

	// Clone
	FormatString *
	clone( Format * p = nullptr ) const
	{
		return new FormatString( *this, p );
	}

	// Destructor
	virtual
	~FormatString()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & por )
	{
		por += s_.length(); // Some Fortran versions/compilers allow literal strings on input to skip the string's length
	}

	// Input
	void
	in( std::istream &, std::streampos & por )
	{
		por += s_.length(); // Some Fortran versions/compilers allow literal strings on input to skip the string's length
	}

	// Output
	void
	out_pos( std::ostream & stream, std::streampos & pos, std::string const & ter )
	{
		output_pad( stream, pos );
		stream << spc( ter ) << s_;
		pos = stream.tellp();
	}

private: // Data

	std::string const s_;

}; // FormatString

// Literal Character Format
class FormatChar : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatChar( Format * p, std::string const & s ) :
	 FormatLeaf( p ),
	 s_( replaced( replaced( s, "\\\\", "\\" ), "\\'", "'" ) )
	{}

	// Copy Constructor
	explicit
	FormatChar( FormatChar const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 s_( f.s_ )
	{}

	// Clone
	FormatChar *
	clone( Format * p = nullptr ) const
	{
		return new FormatChar( *this, p );
	}

	// Destructor
	virtual
	~FormatChar()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & por )
	{
		por += s_.length(); // Some Fortran versions/compilers allow literal strings on input to skip the string's length
	}

	// Input
	void
	in( std::istream &, std::streampos & por )
	{
		por += s_.length(); // Some Fortran versions/compilers allow literal strings on input to skip the string's length
	}

	// Output
	void
	out_pos( std::ostream & stream, std::streampos & pos, std::string const & ter )
	{
		output_pad( stream, pos );
		stream << spc( ter ) << s_;
		pos = stream.tellp();
	}

private: // Data

	std::string const s_;

}; // FormatChar

// Blank=NULL Mode Format
class FormatBN : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;

public: // Creation

	// Constructor
	explicit
	FormatBN( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatBN( FormatBN const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatBN *
	clone( Format * p = nullptr ) const
	{
		return new FormatBN( *this, p );
	}

	// Destructor
	virtual
	~FormatBN()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & )
	{
		blank_zero() = false;
	}

	// Input
	void
	in( std::istream &, std::streampos & )
	{
		blank_zero() = false;
	}

}; // FormatBN

// Blank=Zero Mode Format
class FormatBZ : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;

public: // Creation

	// Constructor
	explicit
	FormatBZ( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatBZ( FormatBZ const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatBZ *
	clone( Format * p = nullptr ) const
	{
		return new FormatBZ( *this, p );
	}

	// Destructor
	virtual
	~FormatBZ()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & )
	{
		blank_zero() = true;
	}

	// Input
	void
	in( std::istream &, std::streampos & )
	{
		blank_zero() = true;
	}

}; // FormatBZ

class FormatS : public FormatLeaf
{

public: // Creation

	// Constructor
	explicit
	FormatS( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatS( FormatS const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatS *
	clone( Format * p = nullptr ) const
	{
		return new FormatS( *this, p );
	}

	// Destructor
	virtual
	~FormatS()
	{}

}; // FormatS

class FormatSP : public FormatLeaf
{

public: // Creation

	// Constructor
	explicit
	FormatSP( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatSP( FormatSP const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatSP *
	clone( Format * p = nullptr ) const
	{
		return new FormatSP( *this, p );
	}

	// Destructor
	virtual
	~FormatSP()
	{}

}; // FormatSP

class FormatSS : public FormatLeaf
{

public: // Creation

	// Constructor
	explicit
	FormatSS( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatSS( FormatSS const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatSS *
	clone( Format * p = nullptr ) const
	{
		return new FormatSS( *this, p );
	}

	// Destructor
	virtual
	~FormatSS()
	{}

}; // FormatSS

class FormatSU : public FormatLeaf
{

public: // Creation

	// Constructor
	explicit
	FormatSU( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatSU( FormatSU const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatSU *
	clone( Format * p = nullptr ) const
	{
		return new FormatSU( *this, p );
	}

	// Destructor
	virtual
	~FormatSU()
	{}

}; // FormatSU

// Blank Spaces Format
class FormatX : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatX( Format * p, Size const n = 1ul ) :
	 FormatLeaf( p ),
	 n_( n )
	{}

	// Copy Constructor
	explicit
	FormatX( FormatX const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 n_( f.n_ )
	{}

	// Clone
	FormatX *
	clone( Format * p = nullptr ) const
	{
		return new FormatX( *this, p );
	}

	// Destructor
	virtual
	~FormatX()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & por )
	{
		por += n_;
	}

	// Input
	void
	in( std::istream &, std::streampos & por )
	{
		por += n_;
	}

	// Output
	void
	out_pos( std::ostream &, std::streampos & por, std::string const & )
	{
		por += n_;
	}

private: // Data

	Size n_; // Number of spaces

}; // FormatX

// Radix Format
class FormatR : public FormatLeaf
{

public: // Creation

	// Constructor
	explicit
	FormatR( Format * p, Size const radix ) :
	 FormatLeaf( p ),
	 radix_( radix )
	{}

	// Copy Constructor
	explicit
	FormatR( FormatR const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 radix_( f.radix_ )
	{}

	// Clone
	FormatR *
	clone( Format * p = nullptr ) const
	{
		return new FormatR( *this, p );
	}

	// Destructor
	virtual
	~FormatR()
	{}

private: // Data

	Size radix_;

}; // FormatR

// Line Feed Format
class FormatLinefeed : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatLinefeed( Format * p, Size const r = 1ul ) :
	 FormatLeaf( p, r )
	{}

	// Copy Constructor
	explicit
	FormatLinefeed( FormatLinefeed const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatLinefeed *
	clone( Format * p = nullptr ) const
	{
		return new FormatLinefeed( *this, p );
	}

	// Destructor
	virtual
	~FormatLinefeed()
	{}

public: // Methods

	// Input
	void
	in( std::istream & stream, std::streampos const poa, std::streampos & por )
	{
		stream.seekg( poa + por, std::ios::beg );
		stream >> skip;
		por = stream.tellg() - poa; // After skip the current stream position is also the virtual position
	}

	// Input
	void
	in( std::istream & stream, std::streampos & por )
	{
		stream.seekg( por, std::ios::beg );
		stream >> skip;
		por = stream.tellg(); // After skip the current stream position is also the virtual position
	}

	// Output
	void
	out_pos( std::ostream & stream, std::streampos & pos, std::string const & ter )
	{
		output_pad( stream, pos );
		stream << spc( ter ) << ter;
		pos = stream.tellp();
	}

}; // FormatLinefeed

// Terminator Format
class FormatColon : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatColon( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatColon( FormatColon const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatColon *
	clone( Format * p = nullptr ) const
	{
		return new FormatColon( *this, p );
	}

	// Destructor
	virtual
	~FormatColon()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & )
	{
		colon_terminated() = true;
	}

	// Input
	void
	in( std::istream &, std::streampos & )
	{
		colon_terminated() = true;
	}

	// Output
	void
	out_pos( std::ostream &, std::streampos &, std::string const & )
	{
		colon_terminated() = true;
	}

}; // FormatColon

// Line Feed Suppression Format
class FormatDollar : public FormatLeaf
{

public: // Types

	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatDollar( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatDollar( FormatDollar const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatDollar *
	clone( Format * p = nullptr ) const
	{
		return new FormatDollar( *this, p );
	}

	// Destructor
	virtual
	~FormatDollar()
	{}

public: // Methods

	// Output
	void
	out_pos( std::ostream &, std::streampos &, std::string const & )
	{
		non_advancing() = true;
	}

}; // FormatDollar

// Format: Format Expression for Tab Positioning
class FormatT : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	FormatT( Format * p, Size const n ) :
	 FormatLeaf( p ),
	 n_( n > 0 ? n : 1ul ) // Assure that n_ > 0
	{}

	// Copy Constructor
	explicit
	FormatT( FormatT const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 n_( f.n_ )
	{}

	// Clone
	FormatT *
	clone( Format * p = nullptr ) const
	{
		return new FormatT( *this, p );
	}

	// Destructor
	virtual
	~FormatT()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & por )
	{
		por = n_ - 1; // Stream positions are zero-based but Fortran tab positions are 1-based
	}

	// Input
	void
	in( std::istream &, std::streampos & por )
	{
		por = n_ - 1; // Stream positions are zero-based but Fortran tab positions are 1-based
	}

	// Output
	void
	out_pos( std::ostream &, std::streampos & pos, std::string const & )
	{
		pos = n_ - 1; // Stream positions are zero-based but Fortran tab positions are 1-based
	}

private: // Data

	Size n_;

}; // FormatT

// Tab-Left Positioning Format
class FormatTL : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	FormatTL( Format * p, Size const n ) :
	 FormatLeaf( p ),
	 n_( n )
	{}

	// Copy Constructor
	explicit
	FormatTL( FormatTL const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 n_( f.n_ )
	{}

	// Clone
	FormatTL *
	clone( Format * p = nullptr ) const
	{
		return new FormatTL( *this, p );
	}

	// Destructor
	virtual
	~FormatTL()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & por )
	{
		por -= n_;
	}

	// Input
	void
	in( std::istream &, std::streampos & por )
	{
		por -= n_;
	}

	// Output
	void
	out_pos( std::ostream &, std::streampos & pos, std::string const & )
	{
		pos -= n_;
	}

private: // Data

	Size n_;

}; // FormatTL

// Tab-Right Positioning Format
class FormatTR : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	FormatTR( Format * p, Size const n ) :
	 FormatLeaf( p ),
	 n_( n )
	{}

	// Copy Constructor
	explicit
	FormatTR( FormatTR const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 n_( f.n_ )
	{}

	// Clone
	FormatTR *
	clone( Format * p = nullptr ) const
	{
		return new FormatTR( *this, p );
	}

	// Destructor
	virtual
	~FormatTR()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & por )
	{
		por += n_;
	}

	// Input
	void
	in( std::istream &, std::streampos & por )
	{
		por += n_;
	}

	// Output
	void
	out_pos( std::ostream &, std::streampos & pos, std::string const & )
	{
		pos += n_;
	}

private: // Data

	Size n_;

}; // FormatTR

// String Format
class FormatA : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatA( Format * p, Size const r = 1ul, Size const w = NOSIZE ) :
	 FormatLeaf( p, r ),
	 w_( w )
	{}

	// Copy Constructor
	explicit
	FormatA( FormatA const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 w_( f.w_ )
	{}

	// Clone
	FormatA *
	clone( Format * p = nullptr ) const
	{
		return new FormatA( *this, p );
	}

	// Destructor
	virtual
	~FormatA()
	{}

public: // Properties

	// Uses an Argument?
	bool
	uses_arg() const
	{
		return true;
	}

	// Has a Width?
	bool
	has_w() const
	{
		return ( w_ != NOSIZE );
	}

public: // Input Methods

	// Input
	void
	in( std::istream & stream, bool & b )
	{
		read_val_reinterpret( stream, b );
		b = false;
	}

	// Input
	void
	in( std::istream & stream, byte & b )
	{
		read_val_reinterpret( stream, b );
	}

	// Input
	void
	in( std::istream & stream, ubyte & b )
	{
		read_val_reinterpret( stream, b );
	}

	// Input
	void
	in( std::istream & stream, short int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned short int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, float & v )
	{
		read_val_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, double & v )
	{
		read_val_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, long double & v )
	{
		read_val_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, char & c );

	// Input
	void
	in( std::istream & stream, std::string & s )
	{
		s = read( stream, w_ ); // Reads the whole record if w_ unspecified since std::string is variable length
	}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, bool const b, std::string const & ter )
	{
		write_val_reinterpret( stream, b, ter ); // Bit rep of LOGICAL(4) .TRUE. value varies across compilers: Intel Fortran it is FFFFFFFF and on GFortran it is 01000000
	}

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter )
	{
		write_val_reinterpret( stream, b, ter );
	}

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter )
	{
		write_val_reinterpret( stream, b, ter );
	}

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, int const i , std::string const & ter)
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter )
	{
		write_val_reinterpret( stream, i, ter );
	}

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter )
	{
		write_val_reinterpret( stream, v, ter );
	}

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter )
	{
		write_val_reinterpret( stream, v, ter);
	}

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter )
	{
		write_val_reinterpret( stream, v, ter );
	}

	// Output
	void
	out( std::ostream & stream, char const c, std::string const & ter )
	{
		stream << spc( ter ) << std::string( ( has_w() && ( w_ > 1ul ) ? w_ - 1ul : 0ul ), ' ' ) << c;
	}

	// Output
	void
	out( std::ostream & stream, std::string const & s, std::string const & ter )
	{
		std::string::size_type const l( s.length() );
		stream << spc( ter ) << std::string( ( has_w() && ( w_ > l ) ? w_ - l : 0ul ), ' ' ) << s;
	}

private: // Methods

	// Read Value from Stream and Reinterpret as Type T
	template< typename T >
	void
	read_val_reinterpret( std::istream & stream, T & t ) const
	{
		Size const w( TraitsA< T >::w );
		std::string s( read( stream, wid( w ) ) );
		std::string::size_type const ls( s.length() );
		if ( ( w > 0ul ) && ( ls < w ) ) s += std::string( w - ls, ' ' ); // Right-pad to width needed by T
		if ( ! s.empty() ) {
			void const * vp( s.substr( s.length() - w ).c_str() );
			t = *reinterpret_cast< T const * >( vp );
		} else { // Nothing read
			t = T( 0 );
			io_err( stream );
		}
	}

	// Write Type T Value Reinterpreted as String to Stream
	template< typename T >
	void
	write_val_reinterpret( std::ostream & stream, T const & t, std::string const & ter )
	{
		Size const w( TraitsA< T >::w );
		Size const ww( wid( w ) );
		Size const wt( std::min( w, ww ) );
		std::string s( ww, ' ' );
		void const * vp( &t );
		s.replace( 0, wt, reinterpret_cast< c_cstring >( vp ), wt );
		stream << spc( ter ) << s;
	}

	// Write bool Value Reinterpreted as String to Stream: Override to Treat bool as 4 Byte Equivalent of LOGICAL(4)
	void
	write_val_reinterpret( std::ostream & stream, bool const b, std::string const & ter )
	{
		Size const w( TraitsA< bool >::w );
		Size const ww( wid( w ) );
		Size const wt( std::min( w, ww ) );
		std::string s( ww, ' ' );
		Size const l( sizeof( b ) );
		void const * vp( &b );
		if ( l >= wt ) { // Use all bits of b
			s.replace( 0, wt, reinterpret_cast< c_cstring >( vp ), wt );
		} else { // Tail-fill with nul: Used for bool that is treated as 4 bytes to correspond with Fortran LOGICAL(4)
			Size const n( wt - l );
			s.replace( 0, l, reinterpret_cast< c_cstring >( vp ), l );
			s.replace( l, n, n, '\0' );
		}
		stream << spc( ter ) << s;
	}

	// Width for I/O
	Size
	wid( Size const w ) const
	{
		return ( w_ != NOSIZE ? w_ : w );
	}

private: // Data

	Size w_;

}; // FormatA

// Logical Format
class FormatL : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatL( Format * p, Size const r = 1ul, Size const w = NOSIZE ) :
	 FormatLeaf( p, r ),
	 w_( w )
	{}

	// Copy Constructor
	explicit
	FormatL( FormatL const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 w_( f.w_ )
	{}

	// Clone
	FormatL *
	clone( Format * p = nullptr ) const
	{
		return new FormatL( *this, p );
	}

	// Destructor
	virtual
	~FormatL()
	{}

public: // Properties

	// Uses an Argument?
	bool
	uses_arg() const
	{
		return true;
	}

	// Has a Width?
	bool
	has_w() const
	{
		return ( w_ != NOSIZE );
	}

public: // Methods

	// Input
	void
	in( std::istream & stream, bool & b );

	// Output
	void
	out( std::ostream & stream, bool const b, std::string const & ter )
	{
		stream << spc( ter ) << std::string( has_w() && ( w_ > 0ul ) ? w_ - 1ul : 0ul, ' ' ) << ( b ? 'T' : 'F' );
	}

private: // Methods

	// Width for I/O
	Size
	wid( Size const w ) const
	{
		return ( w_ != NOSIZE ? w_ : w );
	}

private: // Data

	Size w_;

}; // FormatL

// Integer Format Base Class
class FormatInteger : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;

protected: // Creation

	// Constructor
	explicit
	FormatInteger( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const m = 0ul ) :
	 FormatLeaf( p, r ),
	 w_( w ),
	 m_( m )
	{}

	// Copy Constructor
	FormatInteger( FormatInteger const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 w_( f.w_ ),
	 m_( f.m_ )
	{}

	// Move Constructor
	FormatInteger( FormatInteger && f ) NOEXCEPT :
	 FormatLeaf( std::move( f ) ),
	 w_( f.w_ ),
	 m_( f.m_ )
	{}

public: // Creation

	// Destructor
	virtual
	~FormatInteger()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( FormatInteger const & f )
	{
		if ( this != &f ) {
			FormatLeaf::operator =( f );
			w_ = f.w_;
			m_ = f.m_;
		}
	}

public: // Properties

	// Uses an Argument?
	bool
	uses_arg() const
	{
		return true;
	}

	// Has a Width?
	bool
	has_w() const
	{
		return ( w_ != NOSIZE );
	}

	// Field Width
	Size
	w() const
	{
		return w_;
	}

	// Minimum Width
	Size
	m() const
	{
		return m_;
	}

public: // Input Methods

	// Input
	void
	in( std::istream & stream, bool & b )
	{
		int i;
		read_int( stream, i );
		b = ( std::abs( i ) % 2 == 1 ); // Intel Fortran behavior
	}

	// Input
	void
	in( std::istream & stream, byte & b )
	{
		read_int( stream, b );
	}

	// Input
	void
	in( std::istream & stream, ubyte & b )
	{
		read_int( stream, b );
	}

	// Input
	void
	in( std::istream & stream, short int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned short int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, float & v )
	{
		read_int_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, double & v )
	{
		read_int_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, long double & v )
	{
		read_int_reinterpret( stream, v );
	}

protected: // Methods

	// Read Integer from Stream
	template< typename T >
	void
	read_int( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read( stream, wid( TraitsI< T >::w ) ) ) );
		t = static_cast< T >( read_int_base( stream, s ) );
	}

	// Read Integer from Stream and Reinterpret as Type T
	template< typename T >
	void
	read_int_reinterpret( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read( stream, wid( TraitsI< T >::w ) ) ) );
		read_int_reinterpret( stream, s, t );
	}

	// Read Integer from String and Reinterpret as Type T
	template< typename T >
	void
	read_int_reinterpret( std::istream & stream, std::string const & s, T & t ) const
	{
		if ( is_int_base( s ) ) {
			long int const l( read_int_base( stream, s ) );
			bool ok( true );
			if ( sizeof( T ) < sizeof( int ) ) {
				ok = ( ( ( long int )( std::numeric_limits< int >::min() ) <= l ) && ( l <= ( long int )( std::numeric_limits< int >::max() ) ) );
				int const v( l );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp ); // Probably not what user wants: May want to throw an error here
			} else if ( sizeof( T ) == sizeof( int ) ) {
				ok = ( ( ( long int )( std::numeric_limits< int >::min() ) <= l ) && ( l <= ( long int )( std::numeric_limits< int >::max() ) ) );
				int const v( l );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp );
			} else if ( sizeof( T ) == sizeof( long int ) ) {
				void const * vp( &l );
				t = *reinterpret_cast< T const * >( vp );
			} else if ( sizeof( T ) == sizeof( long long int ) ) {
				long long int const v( l );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp );
			} else {
				long long int const v( l );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp ); // Probably not what user wants: May want to throw an error here
			}
			if ( ! ok ) io_err( stream );
		} else { // Bad input
			t = T( 0 );
			io_err( stream );
		}
	}

	// Width for I/O
	Size
	wid( Size const w ) const
	{
		return ( w_ != NOSIZE ? w_ : w );
	}

	// Can Convert String in Integer Base Format to Integer?
	virtual
	bool
	is_int_base( std::string const & s ) const = 0;

	// Convert String in Integer Base Format to Integer
	virtual
	long int
	read_int_base( std::istream & stream, std::string const & s ) const = 0;

private: // Data

	Size w_;
	Size m_;

}; // FormatInteger

// Decimal Integer Format
class FormatI : public FormatInteger
{

public: // Types

	using FormatInteger::out;

public: // Creation

	// Constructor
	explicit
	FormatI( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const m = 0ul ) :
	 FormatInteger( p, r, w, m )
	{}

	// Copy Constructor
	explicit
	FormatI( FormatI const & f, Format * p = nullptr ) :
	 FormatInteger( f, p )
	{}

	// Clone
	FormatI *
	clone( Format * p = nullptr ) const
	{
		return new FormatI( *this, p );
	}

	// Destructor
	virtual
	~FormatI()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter );

protected: // Methods

	// Can Convert String in Integer Base Format to Integer?
	bool
	is_int_base( std::string const & s ) const
	{
		return is_decimal( s );
	}

	// Convert String in Integer Base Format to Integer
	long int
	read_int_base( std::istream & stream, std::string const & s ) const
	{
		if ( is_decimal( s ) ) {
			return decimal_of( s );
		} else { // Bad input
			io_err( stream );
			return 0l;
		}
	}

}; // FormatI

// Binary Integer Format
class FormatB : public FormatInteger
{

public: // Types

	using FormatInteger::in;
	using FormatInteger::out;

public: // Creation

	// Constructor
	explicit
	FormatB( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const m = 0ul ) :
	 FormatInteger( p, r, w, m )
	{}

	// Copy Constructor
	explicit
	FormatB( FormatB const & f, Format * p = nullptr ) :
	 FormatInteger( f, p )
	{}

	// Clone
	FormatB *
	clone( Format * p = nullptr ) const
	{
		return new FormatB( *this, p );
	}

	// Destructor
	virtual
	~FormatB()
	{}

public: // Input Methods

	// Input
	void
	in( std::istream & stream, bool & b )
	{
		int i;
		read_binary( stream, i );
		b = ( std::abs( i ) % 2 == 1 ); // Intel Fortran behavior
	}

	// Input
	void
	in( std::istream & stream, byte & b )
	{
		read_binary( stream, b );
	}

	// Input
	void
	in( std::istream & stream, ubyte & b )
	{
		read_binary( stream, b );
	}

	// Input
	void
	in( std::istream & stream, short int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned short int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long long int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long long int & i )
	{
		read_binary( stream, i );
	}

	// Input
	void
	in( std::istream & stream, float & v )
	{
		read_binary_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, double & v )
	{
		read_binary_reinterpret( stream, v );
	}

	// Input
	void
	in( std::istream & stream, long double & v )
	{
		read_binary_reinterpret( stream, v );
	}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter );

protected: // Methods

	// Read Integer Value from Stream
	template< typename T >
	void
	read_binary( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read( stream, wid( TraitsB< T >::w ) ) ) );
		t = static_cast< T >( read_int_base( stream, s ) );
	}

	// Read Binary from Stream and Reinterpret as Type T
	template< typename T >
	void
	read_binary_reinterpret( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read( stream, wid( TraitsB< T >::w ) ) ) );
		read_int_reinterpret( stream, s, t );
	}

	// Can Convert String in Integer Base Format to Integer?
	bool
	is_int_base( std::string const & s ) const
	{
		return is_binary( s, false );
	}

	// Convert String in Integer Base Format to Integer
	long int
	read_int_base( std::istream & stream, std::string const & s ) const
	{
		if ( is_binary( s, false ) ) {
			return binary_of( s );
		} else { // Bad input
			io_err( stream );
			return 0l;
		}
	}

}; // FormatB

// Octal Integer Format
class FormatO : public FormatInteger
{

public: // Types

	using FormatInteger::out;

public: // Creation

	// Constructor
	explicit
	FormatO( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const m = 0ul ) :
	 FormatInteger( p, r, w, m )
	{}

	// Copy Constructor
	explicit
	FormatO( FormatO const & f, Format * p = nullptr ) :
	 FormatInteger( f, p )
	{}

	// Clone
	FormatO *
	clone( Format * p = nullptr ) const
	{
		return new FormatO( *this, p );
	}

	// Destructor
	virtual
	~FormatO()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter );

protected: // Methods

	// Can Convert String in Integer Base Format to Integer?
	bool
	is_int_base( std::string const & s ) const
	{
		return is_octal( s, false );
	}

	// Convert String in Integer Base Format to Integer
	long int
	read_int_base( std::istream & stream, std::string const & s ) const
	{
		if ( is_octal( s, false ) ) {
			return octal_of( s );
		} else { // Bad input
			io_err( stream );
			return 0l;
		}
	}

}; // FormatO

// Hexidecimal Integer Format
class FormatZ : public FormatInteger
{

public: // Types

	using FormatInteger::out;

public: // Creation

	// Constructor
	explicit
	FormatZ( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const m = 0ul ) :
	 FormatInteger( p, r, w, m )
	{}

	// Copy Constructor
	explicit
	FormatZ( FormatZ const & f, Format * p = nullptr ) :
	 FormatInteger( f, p )
	{}

	// Clone
	FormatZ *
	clone( Format * p = nullptr ) const
	{
		return new FormatZ( *this, p );
	}

	// Destructor
	virtual
	~FormatZ()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter );

protected: // Methods

	// Can Convert String in Integer Base Format to Integer?
	bool
	is_int_base( std::string const & s ) const
	{
		return is_hexidecimal( s, false );
	}

	// Convert String in Integer Base Format to Integer
	long int
	read_int_base( std::istream & stream, std::string const & s ) const
	{
		if ( is_hexidecimal( s, false ) ) {
			return hexidecimal_of( s );
		} else { // Bad input
			io_err( stream );
			return 0l;
		}
	}

}; // FormatZ

// Scaling Format
class FormatP : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatP( Format * p, int k = 1 ) :
	 FormatLeaf( p ),
	 k_( k )
	{}

	// Copy Constructor
	explicit
	FormatP( FormatP const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 k_( f.k_ )
	{}

	// Clone
	FormatP *
	clone( Format * p = nullptr ) const
	{
		return new FormatP( *this, p );
	}

	// Destructor
	virtual
	~FormatP()
	{}

public: // Methods

	// Input
	void
	in( std::istream &, std::streampos const, std::streampos & )
	{
		P() = k_; // Set P scaling
	}

	// Input
	void
	in( std::istream &, std::streampos & )
	{
		P() = k_; // Set P scaling
	}

	// Output
	void
	out_pos( std::ostream &, std::streampos &, std::string const & )
	{
		P() = k_; // Set P scaling
	}

private: // Data

	int const k_;

}; // FormatP

// Floating Format
class FormatFloat : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;

protected: // Creation

	// Constructor
	explicit
	FormatFloat( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul ) :
	 FormatLeaf( p, r ),
	 w_( w ),
	 d_( d )
	{}

	// Copy Constructor
	FormatFloat( FormatFloat const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p ),
	 w_( f.w_ ),
	 d_( f.d_ )
	{}

	// Move Constructor
	FormatFloat( FormatFloat && f ) NOEXCEPT :
	 FormatLeaf( std::move( f ) ),
	 w_( f.w_ ),
	 d_( f.d_ )
	{}

public: // Creation

	// Destructor
	virtual
	~FormatFloat()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( FormatFloat const & f )
	{
		if ( this != &f ) {
			FormatLeaf::operator =( f );
			w_ = f.w_;
			d_ = f.d_;
		}
	}

public: // Properties

	// Uses an Argument?
	bool
	uses_arg() const
	{
		return true;
	}

	// Has a Width?
	bool
	has_w() const
	{
		return ( w_ != NOSIZE );
	}

	// Field Width
	Size
	w() const
	{
		return w_;
	}

	// Field Digits
	Size
	d() const
	{
		return d_;
	}

public: // Input Methods

	// Input
	void
	in( std::istream & stream, bool & b )
	{
		std::string const s( blank_process( read_float( stream, wid( TraitsF< bool >::w ) ) ) );
		if ( s.length() > 0 ) {
			bool ok( is_type< float >( s ) );
			float const v( val_of< float >( s ) );
			void const * vp( &v );
			b = *reinterpret_cast< bool const * >( vp );
			if ( ! ok ) io_err( stream );
		} else { // Nothing read
			b = false;
			io_err( stream );
		}
	}

	// Input
	void
	in( std::istream & stream, int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long long int & i )
	{
		read_val_reinterpret( stream, i );
	}

	// Input
	void
	in( std::istream & stream, float & v )
	{
		read_val( stream, v );
	}

	// Input
	void
	in( std::istream & stream, double & v )
	{
		read_val( stream, v );
	}

	// Input
	void
	in( std::istream & stream, long double & v )
	{
		read_val( stream, v );
	}

protected: // Methods

	// Read Value from Stream
	template< typename T >
	void
	read_val( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read_float( stream, wid( TraitsF< T >::w ) ) ) );
		if ( is_type< T >( s ) ) {
			t = type_of< T >( s );
			if ( ( d_ > 0ul ) && ( t != T( 0 ) ) && ( ! has( s, '.' ) ) ) t /= static_cast< T >( std::pow( T( 10 ), d_ ) ); // Apply implied decimal point
			if ( ( P() != 0 ) && ( t != T( 0 ) ) && ( ! has_exponent( s ) ) ) t /= static_cast< T >( std::pow( T( 10 ), P() ) ); // Apply scaling
		} else { // Bad input
			t = 0;
			io_err( stream );
		}
	}

	// Read Value from Stream and Reinterpret as Type T
	template< typename T >
	void
	read_val_reinterpret( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read_float( stream, wid( TraitsF< T >::w ) ) ) );
		if ( s.length() > 0 ) {
			bool ok( true );
			if ( sizeof( T ) < sizeof( float ) ) {
				ok = is_type< float >( s );
				float const v( val_of< float >( s ) );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp ); // Probably not what user wants: May want to throw an error here
			} else if ( sizeof( T ) == sizeof( float ) ) {
				ok = is_type< float >( s );
				float const v( val_of< float >( s ) );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp );
			} else if ( sizeof( T ) == sizeof( double ) ) {
				ok = is_type< double >( s );
				double const v( val_of< double >( s ) );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp );
			} else if ( sizeof( T ) == sizeof( long double ) ) {
				ok = is_type< long double >( s );
				long double const v( val_of< long double >( s ) );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp );
			} else {
				ok = is_type< long double >( s );
				long double const v( val_of< long double >( s ) );
				void const * vp( &v );
				t = *reinterpret_cast< T const * >( vp ); // Probably not what user wants: May want to throw an error here
			}
			if ( ! ok ) io_err( stream );
		} else { // Nothing read
			t = T( 0 );
			io_err( stream );
		}
	}

	// Width for I/O
	Size
	wid( Size const w ) const
	{
		return ( w_ != NOSIZE ? w_ : w );
	}

	// Value of Type F from a String
	template< typename F >
	F
	val_of( std::string const & s ) const
	{
		F v( number_of< F >( s ) );
		if ( ( d_ > 0ul ) && ( v != F( 0 ) ) && ( ! has( s, '.' ) ) ) v /= static_cast< F >( std::pow( F( 10 ), d_ ) ); // Apply implied decimal point
		if ( ( P() != 0 ) && ( v != F( 0 ) ) && ( ! has_exponent( s ) ) ) v /= static_cast< F >( std::pow( F( 10 ), P() ) ); // Apply scaling
		return v;
	}

private: // Data

	Size w_;
	Size d_;

}; // FormatFloat

// Fixed Point Format
class FormatF : public FormatFloat
{

public: // Types

	using FormatFloat::out;

public: // Creation

	// Constructor
	explicit
	FormatF( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul ) :
	 FormatFloat( p, r, w, d )
	{}

	// Copy Constructor
	explicit
	FormatF( FormatF const & f, Format * p = nullptr ) :
	 FormatFloat( f, p )
	{}

	// Clone
	FormatF *
	clone( Format * p = nullptr ) const
	{
		return new FormatF( *this, p );
	}

	// Destructor
	virtual
	~FormatF()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

}; // FormatF

// Floating with Exponent Format Base Class
class FormatGED : public FormatFloat
{

protected: // Creation

	// Constructor
	explicit
	FormatGED( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul, Size const e = 2ul ) :
	 FormatFloat( p, r, w, d ),
	 e_( e )
	{}

	// Copy Constructor
	FormatGED( FormatGED const & f, Format * p = nullptr ) :
	 FormatFloat( f, p ),
	 e_( f.e_ )
	{}

	// Move Constructor
	FormatGED( FormatGED && f ) NOEXCEPT :
	 FormatFloat( std::move( f ) ),
	 e_( f.e_ )
	{}

public: // Creation

	// Destructor
	virtual
	~FormatGED()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( FormatGED const & f )
	{
		if ( this != &f ) {
			FormatFloat::operator =( f );
			e_ = f.e_;
		}
	}

public: // Properties

	// Field Exponent Digits
	Size
	e() const
	{
		return e_;
	}

private: // Data

	Size e_;

}; // FormatGED

// General Floating with Exponent Format
class FormatG : public FormatGED
{

public: // Types

	using FormatGED::in;
	using FormatGED::out;

public: // Creation

	// Constructor
	explicit
	FormatG( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul, Size const e = 2ul ) :
	 FormatGED( p, r, w, d, e )
	{}

	// Copy Constructor
	explicit
	FormatG( FormatG const & f, Format * p = nullptr ) :
	 FormatGED( f, p )
	{}

	// Clone
	FormatG *
	clone( Format * p = nullptr ) const
	{
		return new FormatG( *this, p );
	}

	// Destructor
	virtual
	~FormatG()
	{}

public: // Input Methods

	// Input
	void
	in( std::istream & stream, bool & b );

	// Input
	void
	in( std::istream & stream, byte & b )
	{
		read_int( stream, b );
	}

	// Input
	void
	in( std::istream & stream, ubyte & b )
	{
		read_int( stream, b );
	}

	// Input
	void
	in( std::istream & stream, short int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned short int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long long int & i )
	{
		read_int( stream, i );
	}

	// Input
	void
	in( std::istream & stream, char & c );

	// Input
	void
	in( std::istream & stream, std::string & s )
	{
		s = read( stream, w() ); // Reads the whole record if w() unspecified since std::string is variable length
	}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

protected: // Methods

	// Read Integer from Stream
	template< typename T >
	void
	read_int( std::istream & stream, T & t ) const
	{
		std::string const s( blank_process( read( stream, wid( TraitsG< T >::w ) ) ) );
		if ( is_type< T >( s ) ) {
			t = type_of< T >( s );
		} else { // Bad input
			t = 0;
			io_err( stream );
		}
	}

}; // FormatG

// Floating with Exponent Format
class FormatE : public FormatGED
{

public: // Types

	using FormatGED::out;

public: // Creation

	// Constructor
	explicit
	FormatE( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul, Size const e = 2ul ) :
	 FormatGED( p, r, w, d, e )
	{}

	// Copy Constructor
	explicit
	FormatE( FormatE const & f, Format * p = nullptr ) :
	 FormatGED( f, p )
	{}

	// Clone
	FormatE *
	clone( Format * p = nullptr ) const
	{
		return new FormatE( *this, p );
	}

	// Destructor
	virtual
	~FormatE()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

}; // FormatE

// Engineering Floating Format
class FormatEN : public FormatGED
{

public: // Types

	using FormatGED::out;

public: // Creation

	// Constructor
	explicit
	FormatEN( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul, Size const e = 2ul ) :
	 FormatGED( p, r, w, d, e )
	{}

	// Copy Constructor
	explicit
	FormatEN( FormatEN const & f, Format * p = nullptr ) :
	 FormatGED( f, p )
	{}

	// Clone
	FormatEN *
	clone( Format * p = nullptr ) const
	{
		return new FormatEN( *this, p );
	}

	// Destructor
	virtual
	~FormatEN()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

}; // FormatEN

// Scientific Floating Format
class FormatES : public FormatGED
{

public: // Types

	using FormatGED::out;

public: // Creation

	// Constructor
	explicit
	FormatES( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul, Size const e = 2ul ) :
	 FormatGED( p, r, w, d, e )
	{}

	// Copy Constructor
	explicit
	FormatES( FormatES const & f, Format * p = nullptr ) :
	 FormatGED( f, p )
	{}

	// Clone
	FormatES *
	clone( Format * p = nullptr ) const
	{
		return new FormatES( *this, p );
	}

	// Destructor
	virtual
	~FormatES()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

}; // FormatES

// Double Precision Floating Format
class FormatD : public FormatGED
{

public: // Types

	using FormatGED::out;

public: // Creation

	// Constructor
	explicit
	FormatD( Format * p, Size const r = 1ul, Size const w = NOSIZE, Size const d = 0ul, Size const e = 2ul ) :
	 FormatGED( p, r, w, d, e )
	{}

	// Copy Constructor
	explicit
	FormatD( FormatD const & f, Format * p = nullptr ) :
	 FormatGED( f, p )
	{}

	// Clone
	FormatD *
	clone( Format * p = nullptr ) const
	{
		return new FormatD( *this, p );
	}

	// Destructor
	virtual
	~FormatD()
	{}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

}; // FormatD

// List-Directed Format
class FormatLD : public FormatLeaf
{

public: // Types

	using FormatLeaf::in;
	using FormatLeaf::out;

public: // Creation

	// Constructor
	explicit
	FormatLD( Format * p ) :
	 FormatLeaf( p )
	{}

	// Copy Constructor
	explicit
	FormatLD( FormatLD const & f, Format * p = nullptr ) :
	 FormatLeaf( f, p )
	{}

	// Clone
	FormatLD *
	clone( Format * p = nullptr ) const
	{
		return new FormatLD( *this, p );
	}

	// Destructor
	virtual
	~FormatLD()
	{}

public: // Assignment

	// Copy Assignment
	void
	operator =( FormatLD const & f )
	{
		if ( this != &f ) {
			FormatLeaf::operator =( f );
			entry_.reset(); // Don't assign entry
		}
	}

public: // Properties

	// Uses an Argument?
	bool
	uses_arg() const
	{
		return true;
	}

	// FormatLD?
	bool
	is_FormatLD() const
	{
		return true;
	}

	// List-Directed?
	bool
	is_list_directed() const
	{
		return true;
	}

	// Next Format
	Format *
	next()
	{
		++i(); // Increment repeat index
		if ( can_repeat( i() ) ) { // This Format again
			return this;
		} else { // Parent's next Format
			i() = 0ul; // Reset repeat index
			if ( p() ) {
				if ( p()->p() ) { // Nested
					return p()->next_up();
				} else { // Parent is top-level: Repeat without reversion
					return this; // Top-level * can repeat indefinitely
				}
			} else { // Top (no parent)
				return this; // Top-level * can repeat indefinitely
			}
		}
	}

public: // Methods

	// Reset
	FormatLD &
	reset()
	{
		FormatLeaf::reset();
		entry_.reset();
		return *this;
	}

public: // Input Methods

	// Input
	void
	in( std::istream & stream, bool & b )
	{
		read_bool( stream, b );
	}

	// Input
	void
	in( std::istream & stream, byte & b )
	{
		read_num( stream, b );
	}

	// Input
	void
	in( std::istream & stream, ubyte & b )
	{
		read_num( stream, b );
	}

	// Input
	void
	in( std::istream & stream, short int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned short int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, long long int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, unsigned long long int & i )
	{
		read_num( stream, i );
	}

	// Input
	void
	in( std::istream & stream, float & v )
	{
		read_num( stream, v );
	}

	// Input
	void
	in( std::istream & stream, double & v )
	{
		read_num( stream, v );
	}

	// Input
	void
	in( std::istream & stream, long double & v )
	{
		read_num( stream, v );
	}

	// Input
	void
	in( std::istream & stream, char & c )
	{
		read_char( stream, c );
	}

	// Input
	void
	in( std::istream & stream, std::string & s )
	{
		read_string( stream, s );
	}

public: // Output Methods

	// Output
	void
	out( std::ostream & stream, bool const b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, byte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, ubyte const & b, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned short int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, unsigned long long int const i, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, float const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, long double const v, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, char const c, std::string const & ter );

	// Output
	void
	out( std::ostream & stream, std::string const & s, std::string const & ter );

protected: // Methods

	// Read Boolean from Stream
	void
	read_bool( std::istream & stream, bool & b );

	// Read Number from Stream
	template< typename T >
	void
	read_num( std::istream & stream, T & t )
	{
		if ( entry_.r == 0ul ) entry_ = read_ld( stream, true ); // Get a new entry if no repeats left on current entry
		if ( entry_.has() ) { // Non-null entry
			std::string const & s( entry_.s );
			if ( is_type< T >( s ) ) {
				t = type_of< T >( s );
			} else { // Bad input
				t = T();
				io_err( stream );
			}
		}
	}

	// Read Character from Stream
	template< typename T >
	void
	read_char( std::istream & stream, T & t )
	{
		if ( entry_.r == 0ul ) entry_ = read_ld( stream ); // Get a new entry if no repeats left on current entry
		if ( entry_.has() ) { // Non-null entry
			std::string const & s( entry_.s );
			t = ( s.length() > 0 ? s[ 0 ] : ' ' );
		}
	}

	// Read String from Stream
	template< typename T >
	void
	read_string( std::istream & stream, T & t )
	{
		if ( entry_.r == 0ul ) entry_ = read_ld( stream ); // Get a new entry if no repeats left on current entry
		if ( entry_.has() ) { // Non-null entry
			std::string const & s( entry_.s );
			t = T( s );
		}
	}

private: // Data

	EntryFormatLD entry_; // Active input entry

}; // FormatLD

// Format Expression Factory
class FormatFactory
{

public: // Types

	typedef  std::size_t  Size;
	typedef  std::string  Token;
	typedef  std::vector< Token >  Tokens;
	typedef  std::vector< Format * >  Formats;

private: // Creation

	// Default Constructor
	FormatFactory()
	{}

public: // Creation

	// Create Format
	static
	Format *
	create(
	 std::string const & s,
	 Format * p = nullptr
	);

}; // FormatFactory

} // ObjexxFCL

#endif // ObjexxFCL_Format_hh_INCLUDED
