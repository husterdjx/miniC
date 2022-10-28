        int num_lines=0,num_chars=0,num_n=0;
digit [0-9]
letter [a-zA-z]
str [a-zA-Z0-9]
%%
. {++num_chars;}
\n {++num_n;}
%%
int main(int argc, char** argv)
{
    if (argc > 1) {
        if (!(yyin = fopen(argv[1], "r"))) {
            perror(argv[1]);
            return 1;
        }
    }
    yylex();
    printf("Lines=%d,Chars=%d\n",num_n,num_chars);
}