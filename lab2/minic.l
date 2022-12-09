%{
#include "string.h"
#include "def.h"

extern int yycolumn;

#define YYSTYPE yylval
extern "C"
{	
	int yywrap(void);
	int yylex(void);
}

void flexout(std::string name,std::string value){
  std::cout<<name<<"\t:"<<value<<std::endl;
}
//flex为bison提供行号信息
%}
%option yylineno 
int (0x)?[0-9A-F]+
Illegal_octal_number 09
Illegal_hexadecimal_number 0x3G
float ([0-9]*\.[0-9]+)|([0-9]+\.)|(1.05e-4)

%%
{Illegal_octal_number} {std::cout<<"Error type A at Line "<<yylineno<<": Illegal octal number " << "'"<<yytext <<"'"<< std::endl;}
{Illegal_hexadecimal_number} {std::cout<<"Error type A at Line "<<yylineno<<": Illegal hexadecimal number " << "'"<<yytext <<"'"<< std::endl;}
{float} {flexout("FLOAT",yytext);}
{int} {flexout("INT",yytext);}
"int"        {flexout("TYPE","int");}
"float"      {flexout("TYPE","float");}
"char"      {flexout("TYPE","char");}
"return"    {flexout("RETURN","return");}
"if"    {flexout("IF","if");}
"else"    {flexout("ELSE","else");}
"while"    {flexout("WHILE","while");}
"for"    {flexout("FOR","for");}
"struct" {flexout("STRUCT", "struct");}
"." {flexout("DOT", ".");}
[A-Za-z][A-Za-z0-9]* {flexout("ID", yytext);}
";"         {flexout("SEMI",";");}
","         {flexout("COMMA",",");}
"=="      {flexout("RELOP", "==");}
"="         {flexout("ASSIGNOP","=");}
"+"         {flexout("PLUS","+");}
"-"         {flexout("MINUS","-");}
"+="         {flexout("PLUSASS","+=");}
"-="         {flexout("MINUSASS","-=");}
"++"         {flexout("PLUSPLUS","++");}
"--"         {flexout("MINUSMINUS","--");}
"*"         {flexout("STAR","*");}
"/"         {flexout("DIV","/");}
"&&"         {flexout("AND","&&");}
"||"         {flexout("OR","||");}
"!"         {flexout("NOT","!");}
"("         {flexout("LP","(");}
")"         {flexout("RP",")");}
"{"         {flexout("LC","{");}
"}"         {flexout("RC","}");}
"["         {flexout("LB","[");}
"]"         {flexout("RB","]");}

[\n]		{yycolumn=1;}
[ \r\t]		{/*printf("过滤空格等字符\n");*/}
.  {std::cout << "Error type A at Line "<<yylineno<<": Mysterious characters " <<"'"<<yytext<<"'"<<std::endl;}

%%

int yywrap()
{
    return 1;
}