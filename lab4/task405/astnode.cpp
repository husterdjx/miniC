#include "astnode.h"

extern int spaces;
extern std::unique_ptr<LLVMContext> theContext;
extern std::unique_ptr<Module> theModule;
extern std::unique_ptr<IRBuilder<>> builder;
extern std::map<std::string, AllocaInst *> namedValues;//局部变量
extern std::unique_ptr<legacy::FunctionPassManager> theFPM;
extern int grammererror;
extern std::map<std::string, AllocaInst *> curNamedValues;//全局变量或者外面的变量

extern BasicBlock *continueBasicBlock;
void printspaces() {
  for (int i = 0; i < spaces; ++i)
    std::cout << " ";
}
void printGrammerInfo(std::string nodeName, int line) {
  printspaces();
  std::cout << nodeName << " (" << line << ")" << std::endl;
}

void printSemanticError(int type, int line, std::string info = "") {
  grammererror = 1;
  std::cout << "Error type " << type << " at Line " << line << "."
            << std::endl;
}

int parseNIdentifier(NIdentifier &nIdentifier) {
  printspaces();
  std::cout << "ID: " << nIdentifier.name << std::endl;
  return 0;
}

Value *LogErrorV(const char *Str) {
  // std::cout << Str << std::endl;
  return nullptr;
}

void InitializeModuleAndPassManager() {
  // Open a new module.
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("test", *theContext);

  // theModule->setDataLayout(dL);

  // Create a new builder for the module.
  builder = std::make_unique<IRBuilder<>>(*theContext);

  // Create a new pass manager attached to it.
  theFPM = std::make_unique<legacy::FunctionPassManager>(theModule.get());

  // Promote allocas to registers.
  //theFPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  //theFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  //theFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  //theFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  //theFPM->add(createCFGSimplificationPass());

  theFPM->doInitialization();
}

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = theModule->getFunction(Name))
    return F;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
/// of the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          StringRef VarName, Type *varType) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(varType, nullptr, VarName);
}

int NInteger::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "INT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NFloat::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "FLOAT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NChar::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "CHAR"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NIdentifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "ID"
            << ": " << name << std::endl;
  spaces -= 2;
  return 0;
}
int NDotOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  exp.parse();
  printspaces();
  std::cout << "DOT" << std::endl;
  parseNIdentifier(id);
  // id.parse();
  spaces -= 2;
  return 0;
}
int NListOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << "LB" << std::endl;
  rhs.parse();
  printspaces();
  std::cout << "RB" << std::endl;
  spaces -= 2;
  return 0;
}
int NArgs::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  exp.parse();
  if (nArgs) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nArgs->parse();
  }
  spaces -= 2;
  return 0;
}
int NMethodCall::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(id);
  // id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (nargs) {
    nargs->parse();
  }
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NParenOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LP" << std::endl;
  printspaces();
  exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NSingleOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << name << std::endl;
  hs.parse();
  spaces -= 2;
  return 0;
}
int NBinaryOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  if (name.substr(0, 5) == "RELOP")
    std::cout << "RELOP" << std::endl;
  else
    std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NAssignment::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "TYPE: " << type << std::endl;
  spaces -= 2;
  return 0;
}
int NVarDec::parse() {
  printGrammerInfo(getNodeName(), line);

  if (v.size()) {
    spaces += 2;
    for (int i = 0; i < v.size(); ++i) {
      printGrammerInfo(getNodeName(), line);

      spaces += 2;
    }
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
    for (int i = 0; i < v.size(); ++i) {
      printspaces();
      std::cout << "LB" << std::endl;
      printspaces();
      std::cout << "INT: " << v[i] << std::endl;
      printspaces();
      std::cout << "RB" << std::endl;
      spaces -= 2;
    }
  } else {
    spaces += 2;
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
  }
  return 0;
}
int NParamDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  varDec.parse();
  spaces -= 2;
  return 0;
}
int NVarList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nParamDec.parse();
  if (nVarList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nVarList->parse();
  }
  spaces -= 2;
  return 0;
}
int NFunDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(Id);
  // Id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (arguments)
    arguments->parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  vardec.parse();
  if (exp) {
    printspaces();
    std::cout << "ASSIGNOP" << std::endl;
    exp->parse();
  }
  spaces -= 2;
  return 0;
}
int NDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  dec.parse();
  if (nDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  if (nDecList)
    nDecList->parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nDef.parse();
  if (nDefList) {
    nDefList->parse();
  }
  spaces -= 2;
  return 0;
}
int NStructSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printGrammerInfo("StructSpecifier", line);

  spaces += 2;
  printspaces();
  std::cout << "STRUCT" << std::endl;
  if (deflist) {
    if (tag) {
      printGrammerInfo("OptTag", line);
      spaces += 2;
      parseNIdentifier(*tag);
      spaces -= 2;
      printspaces();
      std::cout << "LC" << std::endl;
      deflist->parse();
      printspaces();
      std::cout << "RC" << std::endl;
    } else {
      deflist->parse();
    }
  } else if (tag) {
    printGrammerInfo("Tag", line);

    spaces += 2;
    parseNIdentifier(*tag);
    spaces -= 2;
  }
  spaces -= 2;
  spaces -= 2;
  return 0;
}
int NStmtList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nStmt.parse();
  if (nStmtList)
    nStmtList->parse();
  spaces -= 2;
  return 0;
}

int NCompSt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LC" << std::endl;
  if (ndeflist)
    ndeflist->parse();
  if (nstmtlist)
    nstmtlist->parse();
  printspaces();
  std::cout << "RC" << std::endl;
  spaces -= 2;
  return 0;
}
int NExpStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NCompStStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  compst.parse();
  spaces -= 2;
  return 0;
}
int NRetutnStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "RETURN" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NIfStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NIfElseStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  printspaces();
  std::cout << "ELSE" << std::endl;
  this->stmt_else.parse();
  spaces -= 2;
  return 0;
}
int NWhileStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "WHILE" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NBreakStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "BREAK" << std::endl;
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NExtDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nVarDec.parse();
  if (nExtDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nExtDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NExtDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  specifier.parse();
  if (fundec) {
    fundec->parse();
    if (compst) {
      compst->parse();
    }
  } else {
    if (nextdeclist) {
      nextdeclist->parse();
    }
    printspaces();
    std::cout << "SEMI" << std::endl;
  }

  spaces -= 2;
  return 0;
}
int NExtDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nExtDef.parse();
  if (nExtDefList)
    nExtDefList->parse();
  spaces -= 2;
  return 0;
}
int NProgram::parse() {
  printGrammerInfo("Program", line);
  spaces += 2;
  if (nextdeflist)
    nextdeflist->parse();
  spaces -= 2;
  return 0;
}

// codegen()
Value *Node::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NExpression::codegen() {
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NInteger::codegen() {
  return ConstantInt::get(*theContext, APInt(32, value, true));
}    
Value *NFloat::codegen() { 
  // begin
  return ConstantFP::get(*theContext, APFloat(value));
  // end
}
Value *NChar::codegen() { 
  // begin

  return ConstantInt::get(*theContext, APInt(8, value, true));;
  // end
}
//变量取值
Value *NIdentifier::codegen() { 
  // begin
  Value *retValue=nullptr;
  if (namedValues[name]){//在块内有定义
    retValue = builder->CreateLoad(namedValues[name]->getAllocatedType(), namedValues[name], name);
  }else if(curNamedValues[name]){//在块外面有定义
    retValue = builder->CreateLoad(curNamedValues[name]->getAllocatedType(), curNamedValues[name], name);
  }else
    printSemanticError(1, line, "变量在调用时未经定义" + name);
  return retValue;
  // end
}
Value *NArgs::codegen() {  return exp.codegen(); }//只解析第一个参数
Value *NMethodCall::codegen() { 
  // begin
  Value *retValue=nullptr;
  Function *calleeF = theModule->getFunction(id.name);
  //检查函数是否定义
  if (!calleeF)
  {
    printSemanticError(2, line, "函数在调用时未经定义" + id.name);
    return retValue;
  }
  //遍历输入的参数，检查参数数量和类型是否对得上
  std::vector<Type *> argsTypes;
  std::vector<Value *>argsValue;
  for (auto *item = nargs; item; item = item->nArgs) {
    std::string expname= item->exp.name;
    Value *argVal = item->codegen();
    argsTypes.push_back(argVal->getType());
    argsValue.push_back(argVal);
  }
  //getType()参考llvm::Argument类的定义：https://llvm.org/doxygen/classllvm_1_1Argument.html
  //遍历函数的参数以及类型
  std::vector<Type *> argsTypesF;
  for (auto &arg : calleeF->args()) {
    argsTypesF.push_back(arg.getType());
  }
  if(argsTypesF != argsTypes) //不仅可以判断大小还能判断类型，c++重载太强了
  {
    printSemanticError(8, line, "函数调用时实参与形参的数目或类型不匹配");
    return retValue;
  }
  //调用函数
  if (nargs)
    retValue = builder->CreateCall(calleeF, argsValue, "calltmp");
  else
    retValue = builder->CreateCall(calleeF);
  return retValue;
  // end
}
Value *NParenOperator::codegen() { return exp.codegen(); }
Value *NSingleOperator::codegen() {
  // begin

  return nullptr;
  // end
}
Value *NBinaryOperator::codegen() {
  // begin
  Value *left, *right, *retValue;
  left=right=retValue=nullptr;
  left = lhs.codegen();
  right = rhs.codegen();
  if(!left||!right) return retValue;
  if (name == "PLUS") retValue = builder->CreateAdd(left, right, "add");
  else if(name=="MINUS") retValue = builder->CreateSub(left, right, "minus");
  else if(name=="STAR") retValue = builder->CreateMul(left, right, "mul");
  else if(name=="DIV") retValue = builder->CreateSDiv(left, right, "div");
  else if(name.substr(0, 5) == "RELOP") {
    if(name.substr(5, 6) == "==") retValue = builder->CreateICmpEQ(left, right, "cond");
    else if(name.substr(5, 6) == "<=") retValue = builder->CreateICmpSLE(left, right, "cond");
    else if(name.substr(5, 6) == ">=") retValue = builder->CreateICmpSGE(left, right, "cond");
    else if(name.substr(5, 6) == "!=") retValue = builder->CreateICmpNE(left, right, "cond");
  }
  else if(name=="OR") {
    AllocaInst *temp =
      builder->CreateAlloca(left->getType(), nullptr, "shandian");
    Function *theFun = builder->GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(*theContext, "then", theFun);
    BasicBlock *ElseBB = BasicBlock::Create(*theContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*theContext, "ifcont");

    builder->CreateCondBr(left, ThenBB, ElseBB);
    
    // Emit then value.
    builder->SetInsertPoint(ThenBB);
    builder->CreateStore(left,temp);
    builder->CreateBr(MergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    ThenBB = builder->GetInsertBlock();

    // Emit else block.
    theFun->getBasicBlockList().push_back(ElseBB);
    builder->SetInsertPoint(ElseBB);
    builder->CreateStore(right,temp);
    builder->CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = builder->GetInsertBlock();

    // Emit merge block.
    theFun->getBasicBlockList().push_back(MergeBB);
    builder->SetInsertPoint(MergeBB);

    return builder->CreateLoad(temp->getAllocatedType(), temp, "shandian");;
  }
  return retValue;
  // end
}
Value *NAssignment::codegen() {
  // Assignment requires the LHS to be an identifier.
  // begin
  Value *left, *right, *retValue;
  left=right=retValue=nullptr;
  if (name == "ASSIGNOP"){
    if(lhs.codegen()&&lhs.name.empty())//如果有值，但是却没有左值，就报错
    {
      printSemanticError(6, line, "赋值号左边出现一个只有右值的表达式" + lhs.name);
      return nullptr;
    }
    AllocaInst *lhsAlloca=nullptr;
    if(namedValues[lhs.name])
      lhsAlloca=namedValues[lhs.name];
    else if (curNamedValues[lhs.name])
      lhsAlloca=curNamedValues[lhs.name];
    else{
      printSemanticError(1, line, "变量在调用时未经定义" + lhs.name);
      return nullptr;
    }
    right = rhs.codegen();
    if(right) {
      if(lhsAlloca->getAllocatedType()!=right->getType()) {//判断赋值号两边的表达式类型是否匹配
        printSemanticError(5, line, "赋值号两边的表达式类型不匹配" + lhs.name);
        return nullptr;
      }
      builder->CreateStore(right, lhsAlloca);
    }
  }
  return nullptr;
  // end
}
Value *NSpecifier::codegen() {
  // begin

  return nullptr;
  // end
}
Type *NSpecifier::getType() {
  if (type == "int")
    return Type::getInt32Ty(*theContext);
  if (type == "float")
    return Type::getFloatTy(*theContext);
  if (type == "char")
    return Type::getInt8Ty(*theContext);
  assert(false);
  return Type::getInt32Ty(*theContext);
}
//变量声明
AllocaInst *NVarDec::codegen(Type *type) { 
  // begin
  if (namedValues[Id.name])
  {
    printSemanticError(3, line, "变量出现重复定义" + Id.name);
    return nullptr;
  }
  //声明变量
  AllocaInst *alloca_id =
      builder->CreateAlloca(type, nullptr, Id.name);
  namedValues[Id.name]=alloca_id;
  return alloca_id;
  // end
}
Value *NParamDec::codegen() {
  // begin

  return nullptr;
  // end
}

std::pair<std::string, Type *> NParamDec::getType() {
  assert(varDec.v.size() == 0);
  std::pair<std::string, Type *> tmp(varDec.Id.name, nSpecifier.getType());
  return tmp;
}
Value *NVarList::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
//声明函数
Function *NFunDec::funcodegen(Type *retType) {
  // check if it exists the same name of fun
  if (theModule->getFunction(Id.name)) {
    printSemanticError(4, line, "Redefined " + Id.name);
    return nullptr;
  }

  std::vector<Type *> argsTypes;
  std::vector<std::string> argNames;
  for (NVarList *item = arguments; item; item = item->nVarList) {
    auto tmp = item->nParamDec.getType();
    argNames.push_back(tmp.first);
    argsTypes.push_back(tmp.second);
  }

  FunctionType *ft = FunctionType::get(retType, argsTypes, false);
  Function *f =
      Function::Create(ft, Function::ExternalLinkage, Id.name, theModule.get());
  unsigned idx = 0;
  for (auto &arg : f->args()) {
    arg.setName(argNames[idx++]);
  }
  return f;
}

Value *NDec::codegen(Type *type){
  Value *retValue=nullptr;
  AllocaInst * varAlloca = vardec.codegen(type);
  if (varAlloca&&exp) {
    retValue = exp->codegen();
    if(retValue) builder->CreateStore(retValue, varAlloca);
  }
  return retValue;
}
Value *NDecList::codegen(Type *type){
  Value *retValue=nullptr;
  retValue = dec.codegen(type);
  if (nDecList)
  {
    retValue = nDecList->codegen(type);
  }
  return retValue;
}

Value *NDef::codegen() { 
  // begin
  Type *type = nSpecifier.getType();
  return nDecList->codegen(type);
  // end
}
Value *NDefList::codegen() { 
  // begin
  Value *retVal = nullptr;
  retVal = nDef.codegen();
  if(nDefList)retVal = nDefList->codegen();
  return retVal;
  // end
}
Value *NStmtList::codegen() {
  auto *retVal = nStmt.codegen();
  if (nStmtList)
    retVal = nStmtList->codegen();
  return retVal;
}
Value *NCompSt::codegen() {
  std::map<std::string, AllocaInst *> oldCur=curNamedValues;
  std::map<std::string, AllocaInst *> oldName=namedValues;
  for(auto &tmp: namedValues)
    if(tmp.second) curNamedValues[tmp.first]=tmp.second;
  namedValues.clear();
  Value *retVal = nullptr;
  if (ndeflist) retVal = ndeflist->codegen();
  if (nstmtlist) retVal = nstmtlist->codegen();
  namedValues=oldName;
  curNamedValues=oldCur;
  return retVal;
}
Value *NExpStmt::codegen() { return exp.codegen(); }
Value *NCompStStmt::codegen() {
  // begin
  return compst.codegen();
  // end
}
Value *NRetutnStmt::codegen() { 
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *bb = BasicBlock::Create(*theContext, "ret", theFun);
  builder->CreateBr(bb);
  builder->SetInsertPoint(bb);
  auto *retVal = exp.codegen();
  // check the return type and fundec type
  // begin
  if(retVal->getType() != theFun->getReturnType()){
    printSemanticError(7, line, "return 语句的返回类型与函数定义的返回类型不匹配");
    return nullptr;
  }
  // end
    builder->CreateRet(retVal);
  return retVal;
}
Value *NIfStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  Value *CondV = exp.codegen();
  if(!CondV)return nullptr;

  BasicBlock *ThenBB = BasicBlock::Create(*theContext, "then", theFun);
  BasicBlock *MergeBB = BasicBlock::Create(*theContext, "ifcont");

  builder->CreateCondBr(CondV, ThenBB, MergeBB);

  // Emit then value.
  builder->SetInsertPoint(ThenBB);
  stmt.codegen();
  builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder->GetInsertBlock();

  // Emit merge block.
  theFun->getBasicBlockList().push_back(MergeBB);
  builder->SetInsertPoint(MergeBB);
  return nullptr;
  // end
}
Value *NIfElseStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  Value *CondV = exp.codegen();
  if(!CondV)return nullptr;

  BasicBlock *ThenBB = BasicBlock::Create(*theContext, "then", theFun);
  BasicBlock *ElseBB = BasicBlock::Create(*theContext, "else");
  BasicBlock *MergeBB = BasicBlock::Create(*theContext, "ifcont");

  builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  builder->SetInsertPoint(ThenBB);
  stmt.codegen();
  builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder->GetInsertBlock();

  // Emit else block.
  theFun->getBasicBlockList().push_back(ElseBB);
  builder->SetInsertPoint(ElseBB);
  stmt_else.codegen();
  builder->CreateBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = builder->GetInsertBlock();

  // Emit merge block.
  theFun->getBasicBlockList().push_back(MergeBB);
  builder->SetInsertPoint(MergeBB);

  return nullptr;
  // end
}
Value *NWhileStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  Value *CondV;
  BasicBlock *LoopBB = BasicBlock::Create(*theContext, "loop", theFun);
  BasicBlock *ThenBB = BasicBlock::Create(*theContext, "then");
  BasicBlock *MergeBB = BasicBlock::Create(*theContext, "ifcont");
  builder->CreateBr(LoopBB);
  builder->SetInsertPoint(LoopBB);
  CondV = exp.codegen();
  if(!CondV)return nullptr;
  builder->CreateCondBr(CondV, ThenBB, MergeBB);

  theFun->getBasicBlockList().push_back(ThenBB);
  builder->SetInsertPoint(ThenBB);
  stmt.codegen();
  builder->CreateBr(LoopBB);
  ThenBB = builder->GetInsertBlock();

  theFun->getBasicBlockList().push_back(MergeBB);
  builder->SetInsertPoint(MergeBB);

  return nullptr;
  // end
}
Value *NBreakStmt::codegen() {
  // begin

  return nullptr;
  // end
}
Value *NExtDefVarDec::codegen() {
  // begin

  return nullptr;
  // end
}
Value *NExtDefFunDec::codegen() {
  std::map<std::string, AllocaInst *> oldCur=curNamedValues;
  std::map<std::string, AllocaInst *> oldName=namedValues;
  for(auto &tmp: namedValues)
    if(tmp.second) curNamedValues[tmp.first]=tmp.second;
  namedValues.clear();
  Type *retType = specifier.getType();

  Function *f = fundec->funcodegen(retType);
  if (!f) {
    return nullptr;
  }
  assert(compst != nullptr); // Assert compst is not null.
  BasicBlock *bb = BasicBlock::Create(*theContext, "entry", f);
  builder->SetInsertPoint(bb);
  namedValues.clear();
  for (auto &arg : f->args()) {
    // Create an alloca for this variable.
    AllocaInst *alloca =
        CreateEntryBlockAlloca(f, arg.getName(), arg.getType());

    if (namedValues[std::string(arg.getName())]) {
      printSemanticError(3, line, "Redefined " + arg.getName().str());
      return LogErrorV("Unknown function referenced");
    }
    // Store the initial value into the alloca.
    builder->CreateStore(&arg, alloca);
    // Add arguments to variable symbol table.
    namedValues[std::string(arg.getName())] = alloca;
    curNamedValues[std::string(arg.getName())] = alloca;
  }
  if (Value *retVal = compst->codegen()) {
    // Finish off the function.
    namedValues=oldName;
    curNamedValues=oldCur;
    // Validate the generated code, checking for consistency.
    verifyFunction(*f);

    // Run the optimizer on the function.
    // theFPM->run(*f);
    return f;
  }
  // Error reading body, remove function.
  f->eraseFromParent();

  return nullptr;
}
Value *NExtDefList::codegen() {
  auto *lastCode = nExtDef.codegen();
  // lastCode->print(errs());
  // assert(nExtDefList == nullptr);
  if (nExtDefList)
    lastCode = nExtDefList->codegen();
  return lastCode;
}
Value *NProgram::codegen() {

  //默认输出函数putchar
  std::vector<Type *> putArgs;
  putArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *putType =
      FunctionType::get(builder->getInt32Ty(), putArgs, false);
  Function *putFunc = Function::Create(putType, Function::ExternalLinkage,
                                       "putchar", theModule.get());

  //默认输入函数getchar
  std::vector<Type *> getArgs;
  // getArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *getType =
      FunctionType::get(builder->getInt32Ty(), getArgs, false);
  Function *getFunc = Function::Create(getType, Function::ExternalLinkage,
                                       "getchar", theModule.get());

  Value *lastCode = nextdeflist->codegen();
  if (grammererror)
    return nullptr;
  return lastCode;
}