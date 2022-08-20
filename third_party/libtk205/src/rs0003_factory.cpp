#include "rs0003_factory.h"
#include "rs0003.h"
#include <memory>

/// @note  This class has been generated from a template. Local changes will not be saved!

using namespace tk205;

std::shared_ptr<RSInstanceBase> RS0003Factory::create_instance(const char* RS_instance_file) const
{
    auto p_rs = std::make_shared<rs0003_ns::RS0003>();
    auto j = tk205::load_json(RS_instance_file);
    if (j["metadata"]["schema"] == "RS0003")
    {
        p_rs->initialize(j);
    }
    else
    {
       p_rs = nullptr;
    }
    return p_rs;
}
