#pragma once

#include <string_view>

#include "core/global_context.h"
#include "ir/basic_block.h"
#include "ir/instruction.h"
#include "ir/value.h"

namespace sysy {

class IRBuilder {
 public:
  explicit IRBuilder(GlobalContext& context);

  LoadInst* CreateLoad(Type* type, Value* ptr, std::string_view name) {
    return Insert(new LoadInst(type, ptr), name);
  }

  StoreInst* CreateStore(Value* value, Value* ptr) {
    return Insert(new StoreInst(value, ptr));
  }

  BinaryInstruction* CreateAdd(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kAdd, lhs, rhs), name);
  }

  BinaryInstruction* CreateFAdd(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kFAdd, lhs, rhs), name);
  }

  BinaryInstruction* CreateSub(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kSub, lhs, rhs), name);
  }

  BinaryInstruction* CreateFSub(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kFSub, lhs, rhs), name);
  }

  BinaryInstruction* CreateMul(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kMul, lhs, rhs), name);
  }

  BinaryInstruction* CreateFMul(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kFMul, lhs, rhs), name);
  }

  BinaryInstruction* CreateDiv(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kDiv, lhs, rhs), name);
  }

  BinaryInstruction* CreateFDiv(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kFDiv, lhs, rhs), name);
  }

  BinaryInstruction* CreateRem(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kRem, lhs, rhs), name);
  }

  BinaryInstruction* CreateFRem(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new BinaryInstruction(Instruction::kFRem, lhs, rhs), name);
  }

  ICmpInst* CreateICmpEq(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new ICmpInst(ICmpInst::kICmpEq, lhs, rhs), name);
  }

  ICmpInst* CreateICmpNe(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new ICmpInst(ICmpInst::kICmpNe, lhs, rhs), name);
  }

  ICmpInst* CreateICmpSGt(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new ICmpInst(ICmpInst::kICmpSGt, lhs, rhs), name);
  }

  ICmpInst* CreateICmpSGe(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new ICmpInst(ICmpInst::kICmpSGe, lhs, rhs), name);
  }

  ICmpInst* CreateICmpSLt(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new ICmpInst(ICmpInst::kICmpSLt, lhs, rhs), name);
  }

  ICmpInst* CreateICmpSLe(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new ICmpInst(ICmpInst::kICmpSLe, lhs, rhs), name);
  }

  FCmpInst* CreateFCmpOEq(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new FCmpInst(FCmpInst::kFCmpOEq, lhs, rhs), name);
  }

  FCmpInst* CreateFCmpOGt(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new FCmpInst(FCmpInst::kFCmpOGt, lhs, rhs), name);
  }

  FCmpInst* CreateFCmpOGe(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new FCmpInst(FCmpInst::kFCmpOGe, lhs, rhs), name);
  }

  FCmpInst* CreateFCmpOLt(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new FCmpInst(FCmpInst::kFCmpOLt, lhs, rhs), name);
  }

  FCmpInst* CreateFCmpOLe(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new FCmpInst(FCmpInst::kFCmpOLe, lhs, rhs), name);
  }

  FCmpInst* CreateFCmpUNe(Value* lhs, Value* rhs, std::string_view name) {
    return Insert(new FCmpInst(FCmpInst::kFCmpUNe, lhs, rhs), name);
  }

  SIToFPInst* CreateSIToFP(Value* value, Type* dest_type,
                           std::string_view name) {
    return Insert(new SIToFPInst(dest_type, value), name);
  }

  FPToSIInst* CreateFPToSIInst(Value* value, Type* dest_type,
                               std::string_view name) {
    return Insert(new FPToSIInst(dest_type, value), name);
  }

  ZExtInst* CreateZExtInst(Value* value, Type* dest_type,
                           std::string_view name) {
    return Insert(new ZExtInst(dest_type, value), name);
  }

  ReturnInst* CreateRetVoid() {
    return Insert(ReturnInst::Create(context_, nullptr));
  }

  ReturnInst* CreateRet(Value* retval) {
    return Insert(ReturnInst::Create(context_, retval));
  }

  void SetInsertPoint(BasicBlock* basic_block) { basic_block_ = basic_block; }

 private:
  template <typename InstType>
  InstType* Insert(InstType* inst, std::string_view name = "") {
    inst->InsertInto(basic_block_);
    inst->SetName(name);
    return inst;
  }

  GlobalContext& context_;
  BasicBlock* basic_block_;
};

}  // namespace sysy
