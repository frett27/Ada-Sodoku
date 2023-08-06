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

with Ada.Text_Io; use Ada.Text_Io;

package body Combinatoire is

   procedure Image (C : Combinaison) is
   begin
      for I in C'Range loop
         if C (I) then
            Put ("1");
         else
            Put ("0");
         end if;
      end loop;
   end Image;

   function Calc (Length : in Natural) return Combinaison_Precalc_Access is
      CP : Combinaison_Precalc_Access := new Combinaison_Precalc (Length);
   begin
      for I in CP.C'Range loop
         CP.C (I) := null;
      end loop;

      for I in 0 .. 2**Length - 1 loop
         declare
            N     : Natural;
            Count : Natural                 := 0;
            E     : Enum_Combinaison_Access :=
              new Enum_Combinaison'(Length, (others => False), null);
         begin
            N := I;
            for J in 1 .. Length loop
               E.C (J) := N mod 2 > 0;
               if E.C (J) then
                  Count := Count + 1;
               end if;
               N := N / 2;
            end loop;

            -- count contient le nombre bits ..
            -- E contient la combinaison

            -- on ajoute la combinaison en t�te
            E.Next       := CP.C (Count);
            CP.C (Count) := E;

         end;

      end loop;

      return CP;

   end Calc;

   function Calc_Dictionnaire return Dictionnaire_Combinaison is
      D : Dictionnaire_Combinaison;
   begin
      for I in Dictionnaire_Combinaison'Range loop
         declare
            C : Combinaison_Precalc_Access := Calc (I);
         begin
            D (I) := C;
         end;
      end loop;
      return D;
   end Calc_Dictionnaire;

   procedure Image (C : in Combinaison_Precalc) is
      E : Enum_Combinaison_Access := null;
   begin
      for I in C.C'Range loop
         E := C.C (I);
         Put_Line ("---- combinaison avec " & Natural'Image (I) & " elements");
         while E /= null loop
            Image (E.C);
            New_Line;
            E := E.Next;
         end loop;
      end loop;
   end Image;

   -- creation d'un iterateur ...

   function Create (Tailletotale : Natural; Nbe : Positive) return Iterator is
      IT : Iterator;

      procedure Calc_Possibilites
        (Current_Possibilite : Nbelements; Reste : Natural; Index : Natural)
      is
      begin

         -- Put_line("index "& Natural'Image(Index) &" reste -> " & Natural'Image(Reste));
         if Reste < 0 then
            return;
         end if;

         if Reste = 0 then
            -- Ajout de la combinaison � la liste chainee
            declare
               N : Nbelements_Access       :=
                 new Nbelements (Current_Possibilite'Range);
               P : Enum_Possibilite_Access :=
                 new Enum_Possibilite'(N, IT.Possibilite);
            begin
               N.all          := Current_Possibilite;
               IT.Possibilite := P;
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

         for I in 0 .. Natural'Min (Reste, IT.Nbe (Index)) loop

            declare
               Cp : Nbelements := Current_Possibilite;
            begin
               Cp (Index) := I;
               Calc_Possibilites (Cp, Reste - I, Index + 1);
            end;

         end loop;

      end Calc_Possibilites;

   begin

      IT.Tailletotale := Tailletotale;

      -- calcule de la taille des schemas ..
      if Tailletotale mod MaxPrecalc = 0 then
         -- pas de reste

         declare
            N   : Natural           := Tailletotale / MaxPrecalc;
            Nbe : Nbelements_Access := new Nbelements (1 .. N);
         begin
            for I in Nbe'Range loop
               Nbe (I) := MaxPrecalc;
            end loop;

            IT.Nbe := Nbe;
         end;

      else
         -- il y a un reste
         declare
            N   : Natural           := Tailletotale / MaxPrecalc;
            Nbe : Nbelements_Access := new Nbelements (1 .. N + 1);
         begin
            for I in Nbe'Range loop
               Nbe (I) := MaxPrecalc;
            end loop;
            Nbe (Nbe'Last) := Tailletotale mod MaxPrecalc;
            IT.Nbe         := Nbe;
         end;
      end if;

      -- invariant , le nombre d'�l�ments est d�fini pour
      -- l'iterateur dans le tableau nbe

      -- on calcule toutes les possibilit�s ...
      declare
         Start : Nbelements := (1 .. IT.Nbe'Last => 0);
      begin
         Calc_Possibilites (Start, Nbe, 1);
      end;

      -- ok, on a calculé les différentes possibilitées,
      -- on initialise maintenant l'état de l'iterateur ...

      IT.Current_Possibilite     := new Enum_Possibilite_Access;
      IT.Current_Possibilite.all := IT.Possibilite;
      declare
         Ea : Enum_Array_Access := new Enum_Array (IT.Nbe'Range);
      begin

         for I in 1 .. IT.Nbe'Last loop
            Ea (I) :=
              Dictionnaire (IT.Nbe (I)).C (IT.Current_Possibilite.all.T (I));
         end loop;

         IT.Current_Possibilite_Pos := Ea;

      end;

      return IT;
   end Create;

   function Next (It : in Iterator) return Combinaison is
      C : Combinaison (1 .. It.Tailletotale);
      N : Positive := 1;
   begin

      if It.Current_Possibilite.all = null then
         -- fin du parcours
         raise END_OF_ITERATOR;
      end if;

      -- retourne la combinaison actuelle et passe � la suivante ...
      for I in It.Nbe'Range loop
         C (N .. N + It.Nbe (I) - 1) := It.Current_Possibilite_Pos (I).C;
         N                           := N + It.Nbe (I);
      end loop;

      -- on passe � la suivante ...
      for I in reverse It.Current_Possibilite_Pos'Range loop

         It.Current_Possibilite_Pos (I) := It.Current_Possibilite_Pos (I).Next;

         -- on remet les compteurs suivants � zero pour cette possibilite
         for J in I + 1 .. It.Current_Possibilite_Pos'Last loop
            It.Current_Possibilite_Pos (J) :=
              Dictionnaire (It.Nbe (J)).C (It.Current_Possibilite.all.T (J));
         end loop;

         if It.Current_Possibilite_Pos (I) /= null then
            return C;
         end if;

         -- la position actuelle est nulle,

      end loop;

      -- si on sort ici, c'est qu'il faut passer � la possibilite suivante ...
      It.Current_Possibilite.all := It.Current_Possibilite.all.Next;
      if It.Current_Possibilite.all /= null then
         for I in It.Current_Possibilite_Pos'Range loop
            It.Current_Possibilite_Pos (I) :=
              Dictionnaire (It.Nbe (I)).C (It.Current_Possibilite.all.T (I));
         end loop;
      end if;

      return C;
   end Next;

   procedure Image (It : Iterator) is
   begin
      Put_line ("Tailles des schemas ");
      for I in It.Nbe'Range loop
         Put (Natural'Image (It.Nbe (I)));
      end loop;

   end Image;

begin
   Dictionnaire := Calc_Dictionnaire;

end Combinatoire;
