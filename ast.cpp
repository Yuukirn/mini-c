#include "def.h"

#define YYSTYPE                                                                \
  int // 此行是为了包含parser.tab.hpp不引起错误而加,可以在后面使用相关常量
#include "parser.tab.hpp"
int AST::MaxVarSize = 0;
int AST::MaxTempVarOffset = 0;
// SymbolStackDef AST SymbolStack1={{}};

void space(int indent) {
  for (int i = 0; i < indent; i++)
    cout << " ";
}
map<int, string> SymbolMap = {
    {T_CHAR, "char"},     {T_INT, "int"}, {T_FLOAT, "float"}, {T_VOID, "void"},
    {T_STRUCT, "struct"}, {GT, ">"},      {GE, ">="},         {LT, "<"},
    {LE, "<="},           {EQ, "=="},     {NE, "!="},         {PLUS, "+"},
    {MINUS, "-"},         {UPLUS, "*"},   {UMINUS, "/"},      {ASSIGN, "="},
    {AND, "&&"},          {OR, "||"},     {NOT, "!"},         {DMINUS, "--"},
    {DPLUS, "++"}};

void ProgAST::DisplayAST(
    int indent) { // 依次显示向量ExtDefs中的各个外部定义(外部变量定义和函数定义)
  for (auto a : ExtDefs) {
    a->DisplayAST(0);
  }
}

void ExtVarDefAST::DisplayAST(int indent) { // 显示外部变量定义
  cout << "外部变量定义:" << endl;
  cout << "    类 型 名: ";
  Type->DisplayAST(indent);
  cout << endl << "    变量列表: " << endl;
  for (auto a : ExtVars) {
    a->DisplayAST(15);
  }
}

void BasicTypeAST::DisplayAST(int indent) { // 显示基本类型名符号串
  cout.width(6);
  cout.setf(ios::left);
  cout << SymbolMap[Type];
}

void StructTypeAST::DisplayAST(int indent) {
  cout << "结构体名: " << endl;

  space(indent + 4);
  cout << Name << endl;

  if (IsDef) {
    space(indent);
    cout << "字段: " << endl;

    for (auto a : Fields) {
      a->DisplayAST(indent + 4);
    }
  }
}

void VarDecAST::DisplayAST(int indent) { // 显示外部变量定义中的单个变量
  space(indent);
  cout << Name;       // 显示变量名
  for (auto a : Dims) {
    // 如果是数组，依次显示各维大小
    cout << "[" << a << "]";
  }
  if (Exp) {
    // 有初始化表达式
    cout << "= ";
    if (typeid(*Exp) == typeid(BinaryExprAST)) {
      cout << SymbolMap[(dynamic_cast<BinaryExprAST *>(Exp))->Op] << endl;
      (dynamic_cast<BinaryExprAST *>(Exp))
          ->LeftExp->DisplayAST(indent + static_cast<int>(Name.length()) + 5);
      cout << endl;
      (dynamic_cast<BinaryExprAST *>(Exp))
          ->RightExp->DisplayAST(indent + static_cast<int>(Name.length()) + 5);
    } else
      Exp->DisplayAST(0);
  }
  cout << endl;
}

void FuncDefAST::DisplayAST(int indent) { // 显示函数定义
  cout << "函数定义:" << endl;
  cout << "    返回类型："; // 显示函数返回值类型
  Type->DisplayAST(indent);
  cout << endl << "    函 数 名：" << Name << endl; // 显示函数名
  cout << "    形 参 表：";                         // 显示形参
  if (Params.empty()) {
    cout << "无" << endl;
  } else {
    cout << endl;
    for (auto a : Params)
      a->DisplayAST(14);
  }
  cout << "    函 数 体：  " << endl; // 显示函数返回值类型
  Body->DisplayAST(0);
  cout << endl;
}

void ParamAST::DisplayAST(int indent) { // 显示形式参数
  space(indent);
  Type->DisplayAST(indent);
  ParamName->DisplayAST(indent);
}

/***********各种语句结点***************/
void CompStmAST::DisplayAST(int indent) { // 显示复合语句
  space(indent);
  if (indent) {
    cout << "复合语句：" << endl;
  } else {
    indent = 8; // 显示函数体
  }

  if (!Decls.empty()) {
    space(indent + 2);
    cout << "说明部分:" << endl;
    for (auto a : Decls)
      a->DisplayAST(indent + 4);
  }

  if (!Stms.empty()) {
    space(indent + 2);
    cout << "语句部分:" << endl;
    for (auto a : Stms)
      a->DisplayAST(indent + 4);
  }
}

void ExprStmAST::DisplayAST(int indent) { // 显示表达式语句
  if (typeid(*Exp) == typeid(FuncCallAST)) {
    Exp->DisplayAST(indent); // 对形式为函数调用后接分号的函数调用语句
  } else {
    space(indent);
    cout << "表达式语句：" << endl;
    Exp->DisplayAST(indent + 4);
    cout << endl;
  }
}

void IfStmAST::DisplayAST(int indent) { // 显示条件语句1
  space(indent);
  cout << "if语句：" << endl;
  space(indent + 2);
  cout << "条件：" << endl;
  Cond->DisplayAST(indent + 8);
  space(indent + 2);
  cout << "if子句：" << endl;
  ThenStm->DisplayAST(indent + 8);
}

void IfElseStmAST::DisplayAST(int indent) { // 显示条件语句2
  space(indent);
  cout << "if语句：" << endl;
  space(indent + 2);
  cout << "条件：" << endl;
  Cond->DisplayAST(indent + 8);
  cout << endl;
  space(indent + 2);
  cout << "if子句：" << endl;
  ThenStm->DisplayAST(indent + 8);
  space(indent + 2);
  cout << "else子句：" << endl;
  ElseStm->DisplayAST(indent + 8);
}

void WhileStmAST::DisplayAST(int indent) { // 显示while循环语句
  space(indent);
  cout << "while语句：" << endl;
  space(indent + 2);
  cout << "循环条件：" << endl;
  Cond->DisplayAST(indent + 8);
  space(indent + 2);
  cout << "循环体：" << endl;
  Body->DisplayAST(indent + 8);
}

void ForStmAST::DisplayAST(int indent) { // 显示for循环语句
  space(indent);
  cout << "for语句：" << endl;
  space(indent + 2);
  cout << "初始化：" << endl;
  Init->DisplayAST(indent + 8);

  cout << endl;
  space(indent + 2);
  cout << "循环条件：" << endl;
  Cond->DisplayAST(indent + 8);

  cout << endl;
  space(indent + 2);
  cout << "步长：" << endl;
  Update->DisplayAST(indent + 8);

  space(indent + 2);
  cout << "循环体：" << endl;
  Body->DisplayAST(indent + 8);
}

void ReturnStmAST::DisplayAST(int indent) { // 显示返回语句
  space(indent);
  if (Exp) {
    cout << "返回表达式：" << endl;
    Exp->DisplayAST(indent + 4);
  } else
    cout << "返回语句" << endl;
  cout << endl;
}

void BreakStmAST::DisplayAST(int indent) {
  space(indent);
  cout << "break语句" << endl << endl;
}

void ContinueStmAST::DisplayAST(int indent) {
  space(indent);
  cout << "continue语句" << endl << endl;
}

void DefAST::DisplayAST(int indent) { // 显示局部变量
  space(indent);
  cout << "类型：";
  Type->DisplayAST(indent);
  cout << endl;
  space(indent);
  cout << "变量列表: " << endl;
  for (auto a : LocVars) // 如果是数组，依次显示各维大小
    a->DisplayAST(indent + 10);
  cout << endl;
}

//void StructDefAST::DisplayAST(int indent) {
//  cout << "StructDefAST: not implement" << endl;
//}

void ExtStructDefAST::DisplayAST(int indent) {
  space(indent);
  cout << "结构体定义: " << endl;

  space(indent + 4);
  cout << "类型: ";
  TypeAST->DisplayAST(indent + 8);

  if (!Vars.empty()) {
    space(indent + 4);
    cout << "变量列表: " << endl;
    for (auto a : Vars) {
      a->DisplayAST(indent + 8);
    }
  }
}

/***********表达式结点***************/
void AssignAST::DisplayAST(int indent) { // 显示赋值表达式
  space(indent);
  cout << "赋值运算符：" << SymbolMap[Op] << endl;
  space(indent + 2);
  cout << "左值表达式：" << endl;
  LeftValExp->DisplayAST(indent + 16);
  space(indent + 2);
  cout << "右值表达式：" << endl;
  RightValExp->DisplayAST(indent + 16);
}

void BinaryExprAST::DisplayAST(int indent) { // 显示二元运算表达式
  space(indent);
  cout << SymbolMap[Op] << endl;
  LeftExp->DisplayAST(indent + 4);
  cout << endl;
  RightExp->DisplayAST(indent + 4);
  cout << endl;
}

void ConstAST::DisplayAST(int indent) { // 显示常量
  space(indent);
  switch (Type) // 显示常量值
  {
  case T_CHAR:
    cout << ConstVal.constCHAR;
    break;
  case T_INT:
    cout << ConstVal.constINT;
    break;
  case T_FLOAT:
    cout << ConstVal.constFLOAT;
    break;
  case T_VOID:
    break;
  }
}
void VarAST::DisplayAST(int indent) { // 显示变量
  space(indent);
  cout << Name << endl; // 显示外部变量名
}

void FuncCallAST::DisplayAST(int indent) { // 显示函数调用
  space(indent);
  cout << "函数调用: ";
  //    space(indent+4);
  cout << "函数名：" << Name;
  if (Params.empty()) {
    cout << " <无实参表达式>" << endl;
    return;
  }
  cout << endl;
  space(indent + 10);
  cout << Params.size() << "个实参表达式:\n" << endl;
  for (auto a : Params) {
    a->DisplayAST(indent + 14);
  }
}

void UnaryExprAST::DisplayAST(int indent) { // 显示单目运算
  space(indent);
  cout << "单目：" << SymbolMap[Op] << endl;
  Exp->DisplayAST(indent + 8);
}

void ArrayIndexAST::DisplayAST(int indent) {
  space(indent);
  cout << "数组下标取值：" << endl;
  Pre->DisplayAST(indent + 4);
  Index->DisplayAST(indent + 4);
}

void StructValueAST::DisplayAST(int indent) {
  space(indent);
  cout << "结构体: " << endl;

  space(indent + 2);
  cout << Name << endl;

  space(indent);
  cout << "字段: " << endl;

  space(indent + 2);
  cout << Field << endl;
}