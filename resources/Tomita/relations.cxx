#encoding "utf8"

Term -> Word<kwtype="ключевые_слова">;
MulTerm -> Term<gram="S"> | Term<c-agr[1]> MulTerm<rt,c-agr[1]>;
RelConnectionPartToWhole -> Word<kwtype="глаголы_связки2">;
RelConnectionWholeToPart -> Word<kwtype="глаголы_связки3">;

Part -> MulTerm ;
Whole -> MulTerm ;

Relation -> Part interp (TRelation.Part :: norm="nom")
            RelConnectionPartToWhole
            Whole interp (TRelation.Whole :: norm="nom") ;
Relation -> Whole interp (TRelation.Whole :: norm="nom")
            RelConnectionWholeToPart
            Part interp (TRelation.Part :: norm="nom");
