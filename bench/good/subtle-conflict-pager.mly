%token A B C D L R
%type <unit> s
%start s

/* Cf. mon journal au 19 septembre 2005. Si on r�soud le conflit situ�
   dans l'�tat LA en donnant priorit� � la r�duction, alors la cha�ne
   RABC, qui aurait d� �tre accept�e sans ambigu�t�, est rejet�e. */

%nonassoc B
%nonassoc A /* gives higher precedence to production b -> A over token B */

%%

s:
  L l { () }
| R r { () }

l:
  a C { () }
| b B { () }

r:
  a C { () }
| b D { () }

a:
  A B { () }

b:
  A   { () }

