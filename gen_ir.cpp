/*********在静态语义正确的情况下生成中间代码，否则程序可能崩溃*****************/
#include "def.h"
#include <iostream>
void error(int Line, int Colum, string errMsg);
#define YYSTYPE                                                                \
  int // 此行是为了包含parser.tab.hpp不引起错误而加,可以在后面使用相关常量
#include "parser.tab.hpp"

string NewTemp() {
  static int num = 0;
  return "Temp_" + to_string(++num);
}

string NewLabel() {
  static int num = 0;
  return "Label_" + to_string(++num);
}

map<int, string> Instruction = {{LABEL, "LABEL "},
                                {FUNCTION, "FUNCTION  "},
                                {ASSIGN, ":="},
                                {PLUS, "+"},
                                {UPLUS, "+"},
                                {DPLUS, "++"},
                                {MINUS, "-"},
                                {UMINUS, "-"},
                                {DMINUS, "--"},
                                {STAR, "*"},
                                {DIV, "/"},
                                {GOTO, "  GOTO  "},
                                {GT, ">"},
                                {GE, ">="},
                                {LT, "<"},
                                {LE, "<="},
                                {EQ, "=="},
                                {NE, "!="},
                                {JGT, ">"},
                                {JGE, ">="},
                                {JLT, "<"},
                                {JLE, "<="},
                                {JEQ, "=="},
                                {JNE, "!="},
                                {RETURN, "  RETURN  "},
                                {ARG, "  ARG  "},
                                {PARAM, "  PARAM  "}};

void DisplayIR(const list<IRCode> &IRCodes) {
  for (const auto &a : IRCodes) {
    string OpnStr1, OpnStr2 = a.Opn2.Name, ResultStr = a.Result.Name;
    if (a.Opn1.Name == string("_CONST"))
      switch (a.Opn1.Type) {
      case T_CHAR:
        OpnStr1 = string("#") + to_string(a.Opn1.constCHAR);
        break;
      case T_INT:
        OpnStr1 = string("#") + to_string(a.Opn1.constINT);
        break;
      case T_FLOAT:
        OpnStr1 = string("#") + to_string(a.Opn1.constFLOAT);
        break;
      }
    else {
      OpnStr1 = a.Opn1.Name;
    }

    switch (a.Op) {
    case ASSIGN:
      cout << "  " << ResultStr << " := " << OpnStr1 << endl;
      break;
    case UPLUS:
    case UMINUS:
      cout << "  " << ResultStr << " := " << Instruction[a.Op] << " " << OpnStr1
           << endl;
      break;
    case PLUS:
    case MINUS:
    case STAR:
    case DIV:
    case LE:
    case LT:
    case GE:
    case GT:
    case EQ:
    case NE:
      cout << "  " << ResultStr << ":= " << OpnStr1 << Instruction[a.Op]
           << OpnStr2 << endl;
      break;
    case JLE:
    case JLT:
    case JGE:
    case JGT:
    case JEQ:
    case JNE:
      cout << "  "
           << "IF " << OpnStr1 << Instruction[a.Op] << OpnStr2 << " GOTO "
           << ResultStr << endl;
      break;
    case GOTO:
    case PARAM:
    case ARG:
    case RETURN:
      cout << Instruction[a.Op] << ResultStr << endl;
      break;
    case FUNCTION:
    case LABEL:
      cout << Instruction[a.Op] << ResultStr << ":" << endl;
      break;
    case CALL:
      cout << "  " << ResultStr << " := "
           << "CALL " << OpnStr1 << endl;
      break;
    case CALL0:
      cout << "  CALL " << OpnStr1 << endl;
      break;
    case END:
      cout << "  End Of Program" << endl;
      break;
    case ARR:
      cout << "  " << ResultStr << " := " << OpnStr1 << "[" << OpnStr2 << "]"
           << endl;
      break;
    }
  }
}

void GenObject(const list<IRCode> &IRCodes) {}

void DisplaySymbolTable(SymbolStackDef *SYM);

void ProgAST::GenIR() {
  for (auto a : ExtDefs) {
    // 将各外部项代码依次连接
    a->GenIR();
    auto it = IRCodes.end();
    IRCodes.splice(it, a->IRCodes);
  }
  DisplayIR(IRCodes);
  GenObject(IRCodes);
  DisplaySymbolTable(&SymbolStack);
}

// 外部变量定义
void ExtVarDefAST::GenIR() { /*由于未考虑全局变量的初始化，所以无中间代码*/
}

void VarDecAST::GenIR() { // 有初始化表达式，需要生成中间代码
  if (!Exp) {
    return;
  }
  Opn Result(VarDefPtr->Alias, VarDefPtr->Type, VarDefPtr->Offset);
  int TempVarOffset = 0;
  Opn Opn1 = Exp->GenIR(TempVarOffset);
  auto it = IRCodes.end();
  IRCodes.splice(it, Exp->IRCodes);
  IRCodes.emplace_back(ASSIGN, Opn1, Opn(), Result);
}

void DefAST::GenIR() {
  if (typeid(*Type) == typeid(StructTypeAST)) {
    return;
  }

  list<IRCode>::iterator it;
  for (auto a : LocVars) {
    a->GenIR();
    it = IRCodes.end();
    IRCodes.splice(it, a->IRCodes);
  }
}

//void StructDefAST::GenIR() {}

void ExtStructDefAST::GenIR() {}

void BasicTypeAST::GenIR() {}

void StructTypeAST::GenIR() {}

void FuncDefAST::GenIR() {
  for (auto a : Params) {
    IRCodes.emplace_back(PARAM, Opn(), Opn(),
                         Opn(a->ParamName->Name, 0, 0)); //(PARAM,,,形参名)
  }

  MaxVarSize = FuncDefPtr->ARSize;
  Body->GenIR();
  auto it = IRCodes.end();
  IRCodes.splice(it, Body->IRCodes);      // 连接函数体语句中间代码
  FuncDefPtr->ARSize += MaxTempVarOffset; // 函数AR(栈帧)的大小
  IRCode IRFunc = IRCode(FUNCTION, Opn(), Opn(), Opn(Name, 0, 0));
  if (Name == string("main")) {
    IRFunc.Result.Offset = FuncDefPtr->ARSize;      // 主函数的栈帧大小
    IRCodes.emplace_back(END, Opn(), Opn(), Opn()); // 添加程序结束标记
  }
  IRCodes.push_front(IRFunc); // 函数开始(FUNCTION,,,Name)
}

void ParamAST::GenIR() {}

void CompStmAST::GenIR() {
  // T.code = T1.code || T2.code
  list<IRCode>::iterator it;
  for (auto a : Decls) {
    a->GenIR();
    it = IRCodes.end();
    IRCodes.splice(it, a->IRCodes);
  }
  for (auto a : Stms) {
    a->GenIR();
    it = IRCodes.end();
    IRCodes.splice(it, a->IRCodes);
  }
}

void ExprStmAST::GenIR() {
  int TempVarOffset = 0;
  //    if (typeid(*Exp)==typeid(FuncCallAST) && ((FuncCallAST
  //    *)Exp)->FuncRef->Type==T_VOID)
  //        cout<<"无参函数的语句"<<((FuncCallAST *)Exp)->FuncRef->Name<<endl;
  Exp->GenIR(TempVarOffset);
  if (TempVarOffset > MaxTempVarOffset) {
    MaxTempVarOffset = TempVarOffset;
  }
  auto it = IRCodes.end();
  IRCodes.splice(it, Exp->IRCodes);
}

void IfStmAST::GenIR() {
  // T.code = T1.code || T1.Etrue || T2.code
  string LabelThen = NewLabel();
  string LabelEnd = NewLabel();

  /*计算条件表达式，后面这条件判断处理可以将修改成短路语句的形式，即将标号LabelThen
    和LabelEnd带入，但表达式一旦能得到真假结果，后续不需要计算，直接转移到目标位置。
    而不是下面把整个条件表达式计算完成后，再根据结果确定转移位置*/
  int TempVarOffset = 0;
  Cond->GenIR(TempVarOffset, LabelThen, LabelEnd); // 计算条件表达式
  ThenStm->GenIR();

  auto it = IRCodes.end();
  // T1.code
  IRCodes.splice(it, Cond->IRCodes);
  // T1.Etrue
  IRCodes.emplace_back(LABEL, Opn(), Opn(),
                       Opn(LabelThen, 0, 0)); // if子句前的标号

  it = IRCodes.end();
  // T2.code
  IRCodes.splice(it, ThenStm->IRCodes);
  // end
  IRCodes.emplace_back(LABEL, Opn(), Opn(),
                       Opn(LabelEnd, 0, 0)); // if语句出口标号
}

void IfElseStmAST::GenIR() {
  // T.code = T1.code || T1.Etrue || T2.code || goto T.Enext || T1.Efalse ||
  // T3.code
  string LabelThen = NewLabel();
  string LabelElse = NewLabel();
  string LabelEnd = NewLabel();

  int TempVarOffset = 0;
  Cond->GenIR(TempVarOffset, LabelThen, LabelElse);
  ThenStm->GenIR();
  ElseStm->GenIR();

  auto it = IRCodes.end();
  // T1.code
  IRCodes.splice(it, Cond->IRCodes);
  // T1.Etrue
  IRCodes.emplace_back(LABEL, Opn(), Opn(),
                       Opn(LabelThen, 0, 0)); // if子句前的标号

  it = IRCodes.end();
  // T2.code
  IRCodes.splice(it, ThenStm->IRCodes);
  // goto t.Enext
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelEnd, 0, 0));
  // T1.Efalse
  IRCodes.emplace_back(LABEL, Opn(), Opn(),
                       Opn(LabelElse, 0, 0)); // else子句前的标号

  it = IRCodes.end();
  // T3.code
  IRCodes.splice(it, ElseStm->IRCodes);
  // end
  IRCodes.emplace_back(LABEL, Opn(), Opn(),
                       Opn(LabelEnd, 0, 0)); // if语句出口标号
}

void WhileStmAST::GenIR() {
  // T.code = T2.Snext || T1.code || T1.Etrue || T2.code || goto T2.Snext
  string LoopCond = NewLabel();
  string LoopEntry = NewLabel();
  string LoopEnd = NewLabel();

  int TempVarOffset = 0;
  Cond->GenIR(TempVarOffset, LoopEntry, LoopEnd); // 计算条件表达式
  Body->GenIR();

  // T2.Snext
  IRCodes.emplace_back(LABEL, Opn(), Opn(), Opn(LoopCond, 0, 0));

  auto it = IRCodes.end();
  // T1.code
  IRCodes.splice(it, Cond->IRCodes);
  IRCodes.emplace_back(LABEL, Opn(), Opn(),
                       Opn(LoopEntry, 0, 0)); // 循环入口标号

  it = IRCodes.end();
  // T2.code
  IRCodes.splice(it, Body->IRCodes);
  // goto T2.Snext
  IRCodes.emplace_back(
      GOTO, Opn(), Opn(),
      Opn(LoopCond, 0, 0)); // 结束本次循环，转去重新计算循环条件
  // end
  IRCodes.emplace_back(LABEL, Opn(), Opn(), Opn(LoopEnd, 0, 0)); // 循环结束标号
}

void ForStmAST::GenIR() {
  // 循环开始、条件检查和循环结束的标签
  string LoopStart = NewLabel();
  string LoopCond = NewLabel();
  string LoopEnd = NewLabel();

  int TempVarOffset = 0;

  if (Init) {
    Init->GenIR(TempVarOffset);
    auto it = IRCodes.end();
    IRCodes.splice(it, Init->IRCodes);
  }

  // 循环开始的标签(用于更新跳转)
  IRCodes.emplace_back(LABEL, Opn(), Opn(), Opn(LoopStart, 0, 0));

  if (Cond) {
    Cond->GenIR(TempVarOffset, LoopCond, LoopEnd);
    auto it = IRCodes.end();
    IRCodes.splice(it, Cond->IRCodes);
  } else {
    // 如果没有提供任何条件，则假定是一个无限循环
    IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LoopCond, 0, 0));
  }

  IRCodes.emplace_back(LABEL, Opn(), Opn(), Opn(LoopCond, 0, 0));

  if (Body) {
    Body->GenIR();
    auto it = IRCodes.end();
    IRCodes.splice(it, Body->IRCodes);
  }

  if (Update) {
    Update->GenIR(TempVarOffset);
    auto it = IRCodes.end();
    IRCodes.splice(it, Update->IRCodes);
  }

  // 跳转到下一次迭代的循环开始
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LoopStart, 0, 0));

  IRCodes.emplace_back(LABEL, Opn(), Opn(), Opn(LoopEnd, 0, 0));
}

void ReturnStmAST::GenIR() {
  if (!Exp) {
    return;
  }
  int TempVarOffset = 0;
  Opn Result = Exp->GenIR(TempVarOffset);

  auto it = IRCodes.end();
  IRCodes.splice(it, Exp->IRCodes);

  IRCodes.emplace_back(RETURN, Opn(), Opn(), Result);
}

void BreakStmAST::GenIR() {}

void ContinueStmAST::GenIR() {}

/**************表达式的中间代码生成************************/
Opn VarAST::GenIR(int &TempVarOffset) {
  // 通过语义检查后，VarRef指向对应表项，否则为空，程序会崩溃
  Opn VarOpn(VarRef->Alias, VarRef->Type, VarRef->Offset);
  return VarOpn;
}

void VarAST::GenIR(int &TempVarOffset, string LabelTrue, string LabelFalse) {
  // 根据变量的值确定转移方向
  Opn Result = GenIR(TempVarOffset);
  Opn Zero("_CONST", T_INT, 0);
  Zero.constINT = 0;
  IRCodes.emplace_back(JNE, Result, Zero, Opn(LabelTrue, 0, 0));
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
}

Opn ConstAST::GenIR(int &TempVarOffset) {
  // 将常量赋值给生成的临时变量
  Opn Result(NewTemp(), Type,
             TempVarOffset + MaxVarSize); // 生成临时变量保存常量值
  TempVarOffset += TypeWidth[Type];       // 修改临时变量的偏移量
  if (TempVarOffset > MaxTempVarOffset) {
    MaxTempVarOffset = TempVarOffset;
  }

  Opn Opn1("_CONST", Type, 0); // 别名或临时变量名为_CONST时，表示常量
  Opn1.constFLOAT = ConstVal.constFLOAT; // 按最大长度的成员进行整体复制

  IRCodes.emplace_back(ASSIGN, Opn1, Opn(), Result);
  return Result;
}

void ConstAST::GenIR(int &TempVarOffset, string LabelTrue, string LabelFalse) {
  Opn Result = GenIR(TempVarOffset);
  Opn Zero("_CONST", T_INT, 0);
  Zero.constINT = 0;
  IRCodes.emplace_back(JNE, Result, Zero, Opn(LabelTrue, 0, 0));
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
}

Opn AssignAST::GenIR(int &TempVarOffset) {
  Opn Result = LeftValExp->GenIR(TempVarOffset);
  Opn Opn1 = RightValExp->GenIR(TempVarOffset);

  auto it = IRCodes.end();
  IRCodes.splice(it, LeftValExp->IRCodes);

  it = IRCodes.end();
  IRCodes.splice(it, RightValExp->IRCodes);

  // x := y -> ASSIGN y nil x
  IRCodes.emplace_back(ASSIGN, Opn1, Opn(), Result);
  return Result;
}

void AssignAST::GenIR(int &TempVarOffset, string LabelTrue, string LabelFalse) {
  // 根据左值表达式的值确定转移方向
  Opn Result = LeftValExp->GenIR(TempVarOffset);
  Opn Zero("_CONST", T_INT, 0);
  Zero.constINT = 0;
  IRCodes.emplace_back(JNE, Result, Zero, Opn(LabelTrue, 0, 0));
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
}

Opn BinaryExprAST::GenIR(int &TempVarOffset) {
  Opn Opn1 = LeftExp->GenIR(TempVarOffset);
  Opn Opn2 = RightExp->GenIR(TempVarOffset);
  Opn Result(
      NewTemp(), Opn1.Type,
      TempVarOffset +
          MaxVarSize); // 生成临时变量保存运算结果，结果类型应该根据运算结果来定
  TempVarOffset += TypeWidth
      [Opn1.Type]; // 这里只是简单处理成和左右操作数类型相同，修改临时变量的偏移量
  if (TempVarOffset > MaxTempVarOffset) {
    MaxTempVarOffset = TempVarOffset;
  }

  // A1.code
  auto it = IRCodes.end();
  IRCodes.splice(it, LeftExp->IRCodes);

  // A2.code
  it = IRCodes.end();
  IRCodes.splice(it, RightExp->IRCodes);

  // result(newTemp) := Opn1 Op Opn2
  IRCodes.emplace_back(Op, Opn1, Opn2, Result);

  return Result;
}

void BinaryExprAST::GenIR(int &TempVarOffset, string LabelTrue,
                          string LabelFalse) {
  list<IRCode>::iterator it;
  switch (Op) {
  case AND:
  case OR: {
    string Label = NewLabel();
    if (Op == AND) {
      LeftExp->GenIR(TempVarOffset, Label, LabelFalse);
    } else {
      LeftExp->GenIR(TempVarOffset, LabelTrue, Label);
    }
    RightExp->GenIR(TempVarOffset, LabelTrue, LabelFalse);
    it = IRCodes.end();
    IRCodes.splice(it, LeftExp->IRCodes);
    IRCodes.emplace_back(LABEL, Opn(), Opn(), Opn(Label, 0, 0));

    it = IRCodes.end();
    IRCodes.splice(it, RightExp->IRCodes);
  } break;
  case PLUS:
  case MINUS:
  case STAR:
  case DIV: {
    Opn Result = GenIR(TempVarOffset);
    Opn Zero("_CONST", T_INT, 0);
    Zero.constINT = 0;
    IRCodes.emplace_back(JNE, Result, Zero, Opn(LabelTrue, 0, 0));
    IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
  } break;
  default: // 处理关系运算符
    Opn Opn1 = LeftExp->GenIR(TempVarOffset);
    Opn Opn2 = RightExp->GenIR(TempVarOffset);

    it = IRCodes.end();
    IRCodes.splice(it, LeftExp->IRCodes);

    it = IRCodes.end();
    IRCodes.splice(it, RightExp->IRCodes);

    IRCode IR(JGT, Opn1, Opn2, Opn(LabelTrue, 0, 0));
    if (Op == GE)
      IR.Op = JGE;
    else if (Op == LT)
      IR.Op = JLT;
    else if (Op == LE)
      IR.Op = JLE;
    else if (Op == EQ)
      IR.Op = JEQ;
    else if (Op == NE)
      IR.Op = JNE;
    IRCodes.push_back(IR);
    IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
  }
}

Opn UnaryExprAST::GenIR(int &TempVarOffset) {
  Opn Opn1 = Exp->GenIR(TempVarOffset);
  Opn Result(NewTemp(), Exp->Type, TempVarOffset + MaxVarSize);
  TempVarOffset += TypeWidth[Exp->Type];
  if (TempVarOffset > MaxTempVarOffset) {
    MaxTempVarOffset = TempVarOffset;
  }

  auto it = IRCodes.end();
  IRCodes.splice(it, Exp->IRCodes);

  IRCodes.emplace_back(Op, Opn1, Opn(), Result);
  return Result;
}

void UnaryExprAST::GenIR(int &TempVarOffset, string LabelTrue,
                         string LabelFalse) {
  Exp->GenIR(TempVarOffset, LabelTrue, LabelFalse);
  auto it = IRCodes.end();
  IRCodes.splice(it, Exp->IRCodes);
}

Opn FuncCallAST::GenIR(int &TempVarOffset) {
  list<IRCode> args;
  list<IRCode>::iterator it;
  Opn Opn1, Result;
  SymbolsInAScope *ParamPtr = FuncRef->ParamPtr;
  int i = 0;
  for (auto a : Params) {
    if (Name != string("write")) {
      // 用Opn1的Offset保存形参的偏移量,方便目标代码参数传递,将实参值保存在AR中
      // write函数特殊处理，参数传递用的寄存器
      auto Sym = reinterpret_cast<VarSymbol *>((ParamPtr->Symbols).at(i++));
      Opn1.Offset = Sym->Offset;
    }
    Result = a->GenIR(TempVarOffset); // 计算实参表达式的值
    it = IRCodes.end();
    IRCodes.splice(it, a->IRCodes);
    args.emplace_back(ARG, Opn1, Opn(), Result);
  }
  it = IRCodes.end();
  IRCodes.splice(it, args);

  Opn1.Name = Name;
  Opn1.Type = FuncRef->Type;
  Opn1.SymPtr = FuncRef;
  if (FuncRef->Type != T_VOID) {
    Result = Opn(NewTemp(), FuncRef->Type,
                 TempVarOffset + MaxVarSize); // 临时变量保存返回结果
    TempVarOffset += TypeWidth[FuncRef->Type];
    if (TempVarOffset > MaxTempVarOffset) {
      MaxTempVarOffset = TempVarOffset;
    }
    IRCodes.emplace_back(CALL, Opn1, Opn(), Result);
  } else {
    IRCodes.emplace_back(CALL0, Opn1, Opn(), Opn()); // 返回值为void
  }
  return Result;
}

void FuncCallAST::GenIR(int &TempVarOffset, string LabelTrue,
                        string LabelFalse) {
  // 根据函数返回值确定转移方向
  Opn Result(NewTemp(), FuncRef->Type, TempVarOffset + MaxVarSize);
  Opn Zero("_CONST", T_INT, 0);
  Zero.constINT = 0;
  IRCodes.emplace_back(JNE, Result, Zero, Opn(LabelTrue, 0, 0));
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
}

Opn ArrayIndexAST::GenIR(int &TempVarOffset) {
  // 生成 IR 只考虑一维数组
  Opn Opn1 = Pre->GenIR(TempVarOffset);
  Opn Opn2 = Index->GenIR(TempVarOffset);

  if (!VarRef) {
    // NOTE: 为 nullptr 说明是多维数组，这里不考虑多维数组的 IR
    return Opn{};
  }

  Opn Result(NewTemp(), VarRef->Type, TempVarOffset + MaxVarSize);
  TempVarOffset += TypeWidth[VarRef->Type];
  if (TempVarOffset > MaxTempVarOffset) {
    MaxTempVarOffset = TempVarOffset;
  }

  auto it = IRCodes.end();
  IRCodes.splice(it, Pre->IRCodes);

  it = IRCodes.end();
  IRCodes.splice(it, Index->IRCodes);

  IRCodes.emplace_back(ARR, Opn1, Opn2, Result);
  return Result;
}

void ArrayIndexAST::GenIR(int &TempVarOffset, std::string LabelTrue,
                          std::string LabelFalse) {
  Opn Result(NewTemp(), VarRef->Type, TempVarOffset + MaxVarSize);
  Opn Zero("_CONST", T_INT, 0);
  Zero.constINT = 0;
  IRCodes.emplace_back(JNE, Result, Zero, Opn(LabelTrue, 0, 0));
  IRCodes.emplace_back(GOTO, Opn(), Opn(), Opn(LabelFalse, 0, 0));
}

Opn StructValueAST::GenIR(int &TempVarOffset) {
  return Opn{};
}

void StructValueAST::GenIR(int &TempVarOffset, std::string LabelTrue,
                           std::string LabelFalse) {}