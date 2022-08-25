#ifndef RS_INSTANCE_FACTORY_H_
#define RS_INSTANCE_FACTORY_H_

#include <string>
#include <memory>
#include "rs_instance_base.h" // definition req'd for unique_ptr

/// @class RSInstanceFactory rs_instance_factory.h
/// @brief This class is an abstract interface to support RS factory sub-classes

namespace tk205  {

   inline void read_binary_file(const char* filename, std::vector<char> &bytes)
   {
      std::ifstream is (filename, std::ifstream::binary);
      if (is) 
      {
            // get length of file:
            is.seekg(0, is.end);
            size_t length = static_cast<size_t>(is.tellg());
            is.seekg(0, is.beg);

            bytes.resize(length);
            // read data as a block:
            is.read(bytes.data(), length);

            is.close();
      }
   }

   inline nlohmann::json load_json(const char* input_file)
   {
      std::string filename(input_file);
      std::string::size_type idx = filename.rfind('.');

      using namespace nlohmann;
      
      json j;

      if(idx != std::string::npos)
      {
            std::string extension = filename.substr(idx+1);

            if (extension == "cbor")
            {
               std::vector<char> bytearray;
               read_binary_file(input_file, bytearray);
               j = json::from_cbor(bytearray);
            }
            else if (extension == "json")
            {
               std::string schema(input_file);
               std::ifstream in(schema);
               in >> j;
            }
      }
      return j;
   }

   class RSInstanceFactory
   {
   public: // Interface

      RSInstanceFactory() = default;
      virtual ~RSInstanceFactory() = default;

      static bool register_factory(std::string const &RS_ID,
                                   std::shared_ptr<RSInstanceFactory> factory);

      // Universal factory interface create(). Factory::create() will, through delegation,
      // actually return the requested object.
      static std::shared_ptr<RSInstanceBase> create(std::string const &RS_ID,
                                                    const char* RS_instance_file);

      // Derived factories override create_instance() for actual resource creation
      virtual std::shared_ptr<RSInstanceBase> create_instance(const char* RS_instance_file) const = 0;

      // Rule of five
      RSInstanceFactory(const RSInstanceFactory& other) = delete;
      RSInstanceFactory& operator=(const RSInstanceFactory& other) = delete;
      RSInstanceFactory(RSInstanceFactory&&) = delete;
      RSInstanceFactory& operator=(RSInstanceFactory&&) = delete;
   };
}

#endif 