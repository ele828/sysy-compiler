#pragma once

#include "base/linked_list.h"
#include "ir/constant.h"

namespace sysy {

class Function : public Constant, public base::LinkNode<Function> {};

}  // namespace sysy
