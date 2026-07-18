#pragma once

#include "base/linked_list.h"
#include "ir/constant.h"

namespace sysy {

class GlobalVariable : public Constant,
                       public base::LinkNode<GlobalVariable> {};

}  // namespace sysy
