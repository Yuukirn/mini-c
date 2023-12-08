/*********以下程序只完成了部分静态语义检查，需自行补充完整*******************/
#include "parser.tab.hpp"
#include <utility>

#include "def.h"
SymbolStackDef AST::SymbolStack = SymbolStackDef(); // 初始化静态成员符号表

// TODO: 数组字节
map<int, int> TypeWidth = {
    {T_CHAR, 1}, {T_INT, 4}, {T_FLOAT, 8}}; // 各类型所占字节数
map<char, string> KindName = {
    {'V', "变量"}, {'F', "函数"}, {'P', "形参"}}; // 各类型所占字节数

vector<Error> Errors::Errs = {};
void Errors::ErrorAdd(int Line, int Column, string ErrMsg) {
  Error e = {Line, Column, std::move(ErrMsg)};
  Errs.push_back(e);
}
void Errors::ErrorsDisplay() {
  for (const auto &a : Errs)
    cout << "第" << a.Line << "行、第" << a.Column << "列处错误: " << a.ErrMsg
         << endl;
}

string NewAlias() {
  static int num = 0;
  return "V_" + to_string(++num);
}

void DisplaySymbolTable(SymbolStackDef *SYM) {
  for (int i = 0; i < SYM->Symbols.size(); i++) {
    cout << "------------------------------------------------------------------"
            "----"
         << endl;
    cout << " 层号: " << i << endl;
    cout << " 符 号 名           别名    类型    种 类   其它信息" << endl;
    cout << "------------------------------------------------------------------"
            "----"
         << endl;
    if (SYM->Symbols.at(i)->Symbols.empty())
      cout << "  空 表" << endl;
    else
      for (auto SymPtr : SYM->Symbols.at(i)->Symbols) {
        // 取第i层第j个符号对象的指针
        cout.width(20);
        cout << SymPtr->Name;
        cout.width(8);
        if (SymPtr->Kind == 'V' ||
            SymPtr->Kind == 'P') // 符号是变量,形参,显示别名
          cout << ((VarSymbol *)SymPtr)->Alias;
        else
          cout << " ";
        cout.width(8);
        cout << SymbolMap[SymPtr->Type];
        cout.width(8);
        cout << KindName[SymPtr->Kind];
        if (SymPtr->Kind == 'V' || SymPtr->Kind == 'P') // 符号是变量,形参
          cout << "偏移量: " << ((VarSymbol *)SymPtr)->Offset;
        else if (SymPtr->Kind == 'F') // 符号是函数
        {
          cout << "形参数: " << ((FuncSymbol *)SymPtr)->ParamNum;
          cout << "  变量空间: " << ((FuncSymbol *)SymPtr)->ARSize;
        } else if (SymPtr->Kind == 'A') {
          // 符号是数组，需要显示各维大小
          // TODO:
        }
        cout << endl;
      }
    cout << "------------------------------------------------------------------"
            "----"
         << endl;
  }
}

bool IsLeftValue(ExpAST *PExp) {
  if (typeid(*PExp) == typeid(VarAST)) {
    return true;
  }
  if (typeid(*PExp) == typeid(ArrayIndexAST)) {
    // 数组下标表达式
    return true;
  }
  return false;
}

Symbol *SymbolStackDef::LocateNameCurrent(const string &Name) {
  // 在当前(最内层)作用域中查找该符号是否有定义
  SymbolsInAScope *curScope = Symbols.back();
  for (auto &Symbol : curScope->Symbols)
    if (Symbol->Name == Name)
      return Symbol;
  return nullptr;
}

Symbol *SymbolStackDef::LocateNameGlobal(const string &Name) {
  // 由内向外，整个符号表中查找该符号是否有定义
  for (int i = static_cast<int>(Symbols.size() - 1); i >= 0; i--) {
    for (auto &Symbol : Symbols.at(i)->Symbols)
      if (Symbol->Name == Name)
        return Symbol;
  }
  return nullptr;
}

void ProgAST::Semantics0() {
  int Offset = 0;
  Semantics(Offset);
  Errors::ErrorsDisplay();
}

void ProgAST::Semantics(int &Offset) {
  // 全局作用域
  auto *Global =
      new SymbolsInAScope(); // 全局变量的作用域符号表，记录外部变量、函数名
  SymbolStack.Symbols.push_back(Global);
  GlobalSymbolTable = Global; // 程序对象挂一个全局符号表

  // 预先设置缺省函数read和write
  // 内置函数 read、write
  auto FuncDefPtr = new FuncSymbol();
  FuncDefPtr->Name = string("read");
  FuncDefPtr->Type = T_INT;
  FuncDefPtr->Kind = 'F';
  FuncDefPtr->ParamNum = 0;
  FuncDefPtr->ARSize = 12;
  SymbolStack.Symbols.back()->Symbols.push_back(FuncDefPtr);
  FuncDefPtr = new FuncSymbol();
  FuncDefPtr->Name = string("write");
  FuncDefPtr->Type = T_VOID;
  FuncDefPtr->Kind = 'F';
  FuncDefPtr->ParamNum = 1;
  FuncDefPtr->ARSize = 4;

  // write 的参数
  auto VarDefPtr = new VarSymbol();
  VarDefPtr->Name = VarDefPtr->Alias = string("x");
  VarDefPtr->Type = T_INT;
  VarDefPtr->Kind = 'P';
  VarDefPtr->Offset = 4;
  SymbolStack.Symbols.back()->Symbols.push_back(VarDefPtr);

  FuncDefPtr->ParamPtr = new SymbolsInAScope();
  FuncDefPtr->ParamPtr->Symbols.emplace_back(VarDefPtr);
  SymbolStack.Symbols.back()->Symbols.push_back(FuncDefPtr);

  // 对所有节点做语义分析
  for (auto a : ExtDefs) {
    a->Semantics(Offset);
  }

  // 打印符号表
  DisplaySymbolTable(&SymbolStack);
}

void ExtVarDefAST::Semantics(int &Offset) // 外部定义对象的语义
{
  for (auto a : ExtVars) {
    // a: VarDecAST
    a->Semantics(Offset, Type);
  }
}

void VarDecAST::Semantics(int &Offset, TypeAST *Type) {
  if (!SymbolStack.LocateNameCurrent(Name)) {
    // 当前作用域未定义，将变量加入符号表
    VarDefPtr = new VarSymbol();
    VarDefPtr->Name = Name;
    VarDefPtr->Alias = NewAlias();

    if (Dims.empty()) {
      // var
      VarDefPtr->Kind = 'V';
    } else {
      // array
      VarDefPtr->Kind = 'A';
    }
    if (typeid(*Type) == typeid(BasicTypeAST)) {
      VarDefPtr->Type = (dynamic_cast<BasicTypeAST *>(Type))->Type;
    }

    // 设置并增加 offset
    VarDefPtr->Offset = Offset;
    Offset += TypeWidth[VarDefPtr->Type];

    if (Exp) {
      // 有初值表达式时的处理
      Exp->Semantics(Offset);
    }
    SymbolStack.Symbols.back()->Symbols.push_back(VarDefPtr);
  } else {
    Errors::ErrorAdd(Line, Column, "变量 " + Name + " 重复定义");
  }
}

void DefAST::Semantics(int &Offset) { // 依次提取变量符号进行语义分析
  for (auto a : LocVars) {
    a->Semantics(Offset, Type);
  }
}

void BasicTypeAST::Semantics(int &Offset) {}

void FuncDefAST::Semantics(int &Offset) {
  if (!SymbolStack.LocateNameCurrent(Name)) {
    // 当前作用域未定义，将变量加入符号表
    int offset =
        12; // 局部变量偏移量初始化,预留12个字节存放返回地址等信息，可根据实际情况修改
    MaxVarSize = 12; // 计算函数变量需要的最大容量
    FuncDefPtr = new FuncSymbol();
    FuncDefPtr->Name = Name;
    FuncDefPtr->Kind = 'F';
    if (typeid(*Type) == typeid(BasicTypeAST)) {
      // 处理符号项的返回类型，目前仅基本类型T_CHAR,T_INT,T_FLOAT
      FuncDefPtr->Type = (dynamic_cast<BasicTypeAST *>(Type))->Type;
    }
    FuncDefPtr->ParamNum = static_cast<int>(Params.size());

    auto Local = new SymbolsInAScope(); // 生成函数体作用域变量表
    FuncDefPtr->ParamPtr = Local;       // 函数符号表项，指向形参
    SymbolStack.Symbols.back()->Symbols.push_back(
        FuncDefPtr); // 填写函数符号到符号表

    SymbolStack.Symbols.push_back(Local); // 函数体符号表（含形参）进栈
    Body->LocalSymbolTable = Local;
    for (auto a : Params) {
      a->Semantics(offset); // 未考虑参数用寄存器，只是简单在AR中分配单元
    }

    // 检查是否有 return 语句
    bool hasReturn = false;
    for (auto stm : Body->Stms) {
      if (typeid(*stm) == typeid(ReturnStmAST)) {
        hasReturn = true;
        break;
      }
    }
    if (!hasReturn && FuncDefPtr->Type != T_VOID) {
      Errors::ErrorAdd(Line, Column, "函数 " + Name + " 没有返回语句");
      return;
    }

    Body->Semantics(offset); // 对函数中的变量，在AR中接在参数后分配单元
    FuncDefPtr->ARSize =
        MaxVarSize; // 函数变量需要空间大小（未考虑临时变量），后续再加临时变量单元得到AR大小
  } else {
    Errors::ErrorAdd(Line, Column, "函数 " + Name + " 重复定义");
  }
}

void ParamAST::Semantics(int &Offset) {
  if (!SymbolStack.LocateNameCurrent(ParamName->Name)) {
    // 当前作用域未重复定义，将形参名加入符号表
    auto *SymPtr = new VarSymbol();
    SymPtr->Name = ParamName->Name;
    SymPtr->Kind = 'P';
    SymPtr->Alias = NewAlias();
    if (typeid(*Type) == typeid(BasicTypeAST)) {
      SymPtr->Type = (dynamic_cast<BasicTypeAST *>(Type))->Type;
    }
    SymPtr->Offset = Offset;
    Offset += TypeWidth[SymPtr->Type];
    SymbolStack.Symbols.back()->Symbols.push_back(SymPtr);
  } else {
    Errors::ErrorAdd(Line, Column, "形参名 " + ParamName->Name + " 重复定义");
  }
}

/**************语句显示******************************/
void CompStmAST::Semantics(int &Offset) {
  if (!LocalSymbolTable) {
    // 如果不是函数体的复合语句，需自行生成局部符号表
    auto Local =
        new SymbolsInAScope(); // 全局变量的作用域符号表，记录外部变量、函数名
    SymbolStack.Symbols.push_back(Local);
    LocalSymbolTable = Local; // 程序对象挂一个符号表
  }
  for (auto a : Decls) {
    a->Semantics(Offset);
  }
  if (Offset > MaxVarSize) {
    MaxVarSize = Offset;
  }
  for (auto a : Stms) {
    int Offset_S =
        Offset; // 前后并列语句可以使用同一片单元，所以取最大值，这里保存起始偏移量
    a->Semantics(Offset);
    if (Offset > MaxVarSize) {
      MaxVarSize = Offset;
    }
    Offset = Offset_S;
  }
  cout << "\n\n********************当前复合语句符号表状态**********************"
          "****"
       << endl;
  DisplaySymbolTable(&SymbolStack);
  cout << endl << endl;
  SymbolStack.Symbols.pop_back(); // 复合语句的符号表退栈
}

void ExprStmAST::Semantics(int &Offset) { Exp->Semantics(Offset); }

void IfStmAST::Semantics(int &Offset) {
  Cond->Semantics(Offset);
  ThenStm->Semantics(Offset);
}

void IfElseStmAST::Semantics(int &Offset) {
  Cond->Semantics(Offset);
  ThenStm->Semantics(Offset);
  ElseStm->Semantics(Offset);
}

void WhileStmAST::Semantics(int &Offset) {
  Cond->Semantics(Offset);
  Body->Semantics(Offset);
}

void ForStmAST::Semantics(int &Offset) {
  Init->Semantics(Offset);
  Cond->Semantics(Offset);
  Update->Semantics(Offset);
  Body->Semantics(Offset);
}

void ReturnStmAST::Semantics(int &Offset) {
  if (Exp)
    Exp->Semantics(Offset);
}

void BreakStmAST::Semantics(int &Offset) {}
void ContinueStmAST::Semantics(int &Offset) {}

/**************表达式显示******************************/
void VarAST::Semantics(int &Offset) {
  auto symbol = SymbolStack.LocateNameGlobal(Name);
  if (!symbol) {
    // 未定义的符号
    Errors::ErrorAdd(Line, Column, "引用未定义的符号 " + Name);
    return;
  }
  if (symbol->Kind == 'F') {
    // 如果是函数名，报错，
    Errors::ErrorAdd(Line, Column, "对函数名采用非函数调用形式访问 " + Name);
    return;
  }
  VarRef = reinterpret_cast<VarSymbol *>(symbol);
  if (VarRef) {
    // 简单变量则提取变量类型属性
    Type = static_cast<BasicTypes>(VarRef->Type);
  } else {
    Errors::ErrorAdd(Line, Column, "引用未定义的符号 " + Name);
  }
}

void ConstAST::Semantics(int &Offset) {
  // 提取类型属性
  // const 类型在词法解析时便确定
}

void AssignAST::Semantics(int &Offset) {
  LeftValExp->Semantics(Offset);
  if (!IsLeftValue(LeftValExp)) {
    Errors::ErrorAdd(Line, Column, "非左值表达式");
  }
  RightValExp->Semantics(Offset);
}

void BinaryExprAST::Semantics(int &Offset) {
  LeftExp->Semantics(Offset);
  RightExp->Semantics(Offset);
  if (LeftExp->Type == T_VOID || RightExp->Type == T_VOID) {
    Errors::ErrorAdd(Line, Column, "void类型不能参与运算");
    return;
  }
  // 判断左右值类型是否一致
  if (LeftExp->Type != RightExp->Type) {
    Errors::ErrorAdd(Line, Column,
                     "类型不匹配 " + TypeName(LeftExp->Type) + " " +
                         TypeName(RightExp->Type));
    return;
  }
  // 确定运算结果
  Type = LeftExp->Type;
}

void UnaryExprAST::Semantics(int &Offset) {
  if (Op == DMINUS || Op == DPLUS) {
    // 单目运算符++、--
    if (!IsLeftValue(Exp)) {
      Errors::ErrorAdd(Line, Column, "非左值表达式");
      return;
    }
  }
  Exp->Semantics(Offset);
}

void FuncCallAST::Semantics(int &Offset) {
  auto symbol = SymbolStack.LocateNameGlobal(Name);
  if (!symbol) {
    // 未定义的符号
    Errors::ErrorAdd(Line, Column, "引用未定义的函数 " + Name);
    return;
  }
  if (symbol->Kind != 'F') {
    Errors::ErrorAdd(Line, Column, "对非函数名采用函数调用形式 " + Name);
    return;
  }
  FuncRef = reinterpret_cast<FuncSymbol *>(symbol);
  if (FuncRef) {
    Type = static_cast<BasicTypes>(FuncRef->Type);

    // 检查实参表达式个数和形参数是否一致
    if (FuncRef->ParamNum != static_cast<int>(Params.size())) {
      Errors::ErrorAdd(Line, Column, "函数 " + Name + " 参数个数不匹配");
      return;
    }

    if (FuncRef->ParamNum == 0) {
      return;
    }

    auto paramSymbols = FuncRef->ParamPtr->Symbols;
    for (int i = 0; i < static_cast<int>(Params.size()); i++) {
      // 检查类型是否一致
      if (Params[i]->Type != paramSymbols[i]->Type) {
        Errors::ErrorAdd(Line, Column, "函数 " + Name + " 参数类型不匹配");
        return;
      }
      Params[i]->Semantics(Offset);
    }
  } else {
    Errors::ErrorAdd(Line, Column, "引用未定义的函数 " + Name);
  }
}

void ArrayIndexAST::Semantics(int &Offset) {
  auto preAST = dynamic_cast<VarAST *>(Pre);
  if (preAST) {
    auto symbol = SymbolStack.LocateNameGlobal(preAST->Name);
    if (!symbol) {
      // 未定义的符号
      Errors::ErrorAdd(Line, Column, "引用未定义的符号 " + preAST->Name);
      return;
    }
    if (symbol->Kind != 'A') {
      Errors::ErrorAdd(Line, Column,
                       "对非数组名采用数组下标形式 " + preAST->Name);
      return;
    }
  }

  Pre->Semantics(Offset);
  Index->Semantics(Offset);
  if (Index->Type != T_INT) {
    Errors::ErrorAdd(Line, Column, "数组下标非整型");
    return;
  }
}