------------------------------------------------------------------------------
--                                Sodoku                                    --
--                                                                          --
--                         Copyright (C) 2005-2006                          --
--                                                                          --
--  Authors: Patrice Freydiere                                              --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_Io;
use Ada.Text_Io;


package body Combinatoire is



   procedure Image(C : Combinaison) is
   begin
      for I in C'Range loop
         if C(I) then
            Put("1");
         else
            Put("0");
         end if;
      end loop;
   end;


   function Calc(Length : in Natural) return Combinaison_Precalc_Access is
     CP : Combinaison_Precalc_Access := new combinaison_precalc(Length);
   begin
      for I in CP.C'Range loop
         CP.c(I) := null;
      end loop;

      for I in 0..2**Length - 1 loop
         declare
            N : Natural;
            Count : Natural := 0;
            E : Enum_Combinaison_Access :=
              new Enum_Combinaison'(Length,(others =>False), null) ;
         begin
            N := I;
            for J in 1..Length loop
               E.C(J) := N mod 2 > 0;
               if E.C(J) then
                  Count := Count + 1;
               end if;
               N := N / 2;
            end loop;

            -- count contient le nombre bits ..
            -- E contient la combinaison

            -- on ajoute la combinaison en tête
            E.Next := Cp.C(Count);
            Cp.C(Count) := E;

         end;

      end loop;

      return Cp;

   end;


   function Calc_Dictionnaire return Dictionnaire_Combinaison is
      D : Dictionnaire_Combinaison;
   begin
      for I in Dictionnaire_Combinaison'Range loop
         declare
            C : Combinaison_Precalc_access := Calc(I);
         begin
            D(I) := C;
         end;
      end loop;
      return D;
   end;



   procedure Image(C : in Combinaison_Precalc) is
      E:Enum_Combinaison_Access := null;
   begin
      for I in C.c'Range loop
         E := C.C(I);
         Put_Line("---- combinaison avec " & Natural'Image(I) & " elements");
         while E /= null loop
            Image(E.C);
            New_Line;
            E := E.Next;
         end loop;
      end loop;
   end;


   -- creation d'un iterateur ...

   function Create (Tailletotale : Natural;
                    Nbe          : Positive) return Iterator is
      IT : Iterator;



      procedure Calc_Possibilites(Current_Possibilite : Nbelements;
                                  Reste : Natural;
                                  Index : Natural) is
      begin

         -- Put_line("index "& Natural'Image(Index) &" reste -> " & Natural'Image(Reste));
         if Reste < 0 then
            return;
         end if;


         if Reste = 0 then
            -- Ajout de la combinaison à la liste chainee
            declare
               N : Nbelements_Access := new Nbelements(Current_Possibilite'Range);
               P : Enum_Possibilite_Access := new Enum_Possibilite'(N, It.Possibilite);
            begin
               N.all := Current_Possibilite;
               It.Possibilite := P;
            end;

            --   for I in Current_Possibilite'Range loop
            --                 Put(Natural'Image(Current_Possibilite(I)));
            --              end loop;
            --              New_Line;


            -- pas besoin d'aller plus loin puisque le reste est nul
            return;
         end if;

         if Index > Current_Possibilite'Last then
            return;
         end if;

         -- invariant, reste > 0

         for I in 0..Natural'Min(Reste, It.Nbe(Index)) loop

            declare
               Cp : Nbelements := Current_Possibilite;
            begin
               Cp(Index) := I;
               Calc_Possibilites(Cp, Reste - I, Index + 1);
            end;

         end loop;

      end;




   begin

      It.Tailletotale := Tailletotale;

      -- calcule de la taille des schemas ..
      if Tailletotale mod MaxPrecalc = 0 then
         -- pas de reste

         declare
            N : Natural := Tailletotale / Maxprecalc;
            Nbe : Nbelements_Access := new Nbelements(1..n);
         begin
            for I in Nbe'Range loop
               Nbe(I) := Maxprecalc;
            end loop;

            It.Nbe := Nbe;
         end;

      else
         -- il y a un reste
         declare
            N : Natural := Tailletotale / Maxprecalc;
            Nbe : Nbelements_Access := new Nbelements(1..N + 1);
         begin
            for I in Nbe'Range loop
               Nbe(I) := Maxprecalc;
            end loop;
            Nbe(Nbe'Last) := Tailletotale mod Maxprecalc;
            It.Nbe := Nbe;
         end;
      end if;

      -- invariant , le nombre d'éléments est défini pour
      -- l'iterateur dans le tableau nbe


      -- on calcule toutes les possibilités ...
      declare
         Start : Nbelements := (1..It.Nbe'Last => 0);
      begin
         Calc_Possibilites( Start, nbe, 1);
      end;


      -- ok, on a calculé les différentes possibilitées,
      -- on initialise maintenant l'état de l'iterateur ...

      It.Current_Possibilite := new Enum_Possibilite_Access;
      It.Current_Possibilite.all := It.Possibilite;
      declare
         Ea : Enum_Array_Access := new Enum_Array(It.Nbe'Range);
      begin

         for I in 1..It.Nbe'Last loop
            Ea(I) := Dictionnaire(It.Nbe(I)).C(It.Current_Possibilite.all.T(I));
         end loop;

         It.Current_Possibilite_Pos := Ea;

      end;

      return It;
   end;


   function Next(It : in Iterator) return Combinaison is
      C : Combinaison(1..It.Tailletotale);
      N : Positive := 1;
   begin

      if It.Current_Possibilite.all = null then
         -- fin du parcours
         raise END_OF_ITERATOR;
      end if;

      -- retourne la combinaison actuelle et passe à la suivante ...
      for I in It.Nbe'Range loop
         C(N..N+It.Nbe(I) - 1) := It.Current_Possibilite_Pos(I).C;
         N := N + It.Nbe(I);
      end loop;

      -- on passe à la suivante ...
      for I in reverse It.Current_Possibilite_Pos'Range loop

         It.Current_Possibilite_Pos(I) := It.Current_Possibilite_Pos(I).Next;

         -- on remet les compteurs suivants à zero pour cette possibilite
         for J in I+1..It.Current_Possibilite_Pos'Last loop
            It.Current_Possibilite_Pos(J) :=
              Dictionnaire(It.Nbe(j)).C(It.Current_Possibilite.all.T(j));
         end loop;

         if It.Current_Possibilite_Pos(I) /= null then
            return C;
         end if;

         -- la position actuelle est nulle,

      end loop;

      -- si on sort ici, c'est qu'il faut passer à la possibilite suivante ...
      It.Current_Possibilite.all := It.Current_Possibilite.all.Next;
      if It.Current_Possibilite.all /= null then
         for I in It.Current_Possibilite_Pos'range loop
            It.Current_Possibilite_Pos(I) :=  Dictionnaire(It.Nbe(I)).C(It.Current_Possibilite.all.T(I));
         end loop;
      end if;

      return C;
   end;


   procedure Image(It: Iterator) is
   begin
      Put_line("Tailles des schemas ");
      for I in It.Nbe'Range loop
         Put(Natural'Image(It.Nbe(I)));
      end loop;

   end;


begin
   Dictionnaire := Calc_Dictionnaire;


end combinatoire;
