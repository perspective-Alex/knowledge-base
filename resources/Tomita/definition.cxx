#encoding "utf8"

Term -> Word<kwtype="ключевые_слова">;
MulTerm -> Term<gram="S"> | Term<c-agr[1]> MulTerm<rt,c-agr[1]> ;
Dash -> '-' | '—' ;
Definition -> AnyWord+;
DefConnection -> Word<kwtype="глаголы_связки1"> | Dash 'это';

TermAndDefinition ->
    MulTerm interp (TDefinition.Term :: norm="nom")
      DefConnection
    Definition interp (TDefinition.Defin :: not_norm);
TermAndDefinition ->
    Definition interp (TDefinition.Defin :: not_norm)
      DefConnection
    MulTerm interp (TDefinition.Term :: norm="nom");
