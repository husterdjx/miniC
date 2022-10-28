%%
a*b             {printf("1%s\n", yytext);}
ca              {printf("2%s\n", yytext);}
a*ca*            {printf("3%s\n", yytext);}
%%
int main( argc, argv )
int argc;
char **argv;
{
++argv, --argc; /* skip over program name */
if ( argc > 0 )
        yyin = fopen( argv[0], "r" );
else
        yyin = stdin;
        yylex();
}     