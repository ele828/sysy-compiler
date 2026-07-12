#pragma once

#include "ir/constant.h"

namespace sysy {

class ConstantData : public Constant {};

class ConstantInt : public ConstantData {};

class ConstantFP : public ConstantData {};

}  // namespace sysy
