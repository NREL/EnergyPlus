#ifndef WindowModel_H
#define WindowModel_H

#include <memory>
#include <iostream>
#include <map>

namespace EnergyPlus {

  template< typename T >
  class EnumParser {
    std::map< std::string, T > m_Map;
  public:
    EnumParser() {};

    T StringToEnum( const std::string &value ) {
      map< std::string, T >::const_iterator iValue = m_Map.find( value );
      if( iValue == m_Map.end() )
        throw std::runtime_error( "Incorrect enumerator assigned." );
      return iValue->second;
    }
  };

  namespace WindowManager {

    class CWindowModel;

    enum WindowsModel { BuiltIn, External };

    // Class that reads IDD object and decides if interior or exterior window models
    // will be used.
    class CWindowModel {
    public:
      CWindowModel();

      static std::shared_ptr< CWindowModel > WindowModelFactory( std::string objectName );

      WindowsModel getWindowsModel() const;

    private:
      WindowsModel m_Model;
    };
  }

}

#endif