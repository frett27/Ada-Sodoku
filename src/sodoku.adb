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

package body sodoku is

   procedure Image (G : in Grille) is
   begin
      for I in Line'Range loop
         for J in Line'Range loop

            for K in Column'Range loop
               for L in Column'Range loop
                  Put (Number'Image (G (K, I) (L, J)));
               end loop;

            end loop;
            New_Line;
         end loop;
      end loop;
   end Image;

   function Empty return Grille is
      G : Grille;
   begin
      for I in Line'Range loop
         for J in Line'Range loop
            for K in Column'Range loop
               for L in Column'Range loop
                  G (K, I) (L, J) := 0;
               end loop;
            end loop;
         end loop;
      end loop;
      return G;
   end Empty;

   procedure Put (G : in out Grille; R : Ref; N : Number) is
   begin
      G (R.GC, R.GL) (R.MC, R.ML) := N;
   end Put;

   function Get (G : in Grille; R : Ref) return Number is
   begin
      return G (R.GC, R.GL) (R.MC, R.ML);
   end Get;

   --
   -- Manipulation des �l�ments de recherche ...
   --

   function Empty return Search is
      S : Search;
   begin
      for I in Line'Range loop
         for J in Line'Range loop
            for K in Column'Range loop
               for L in Column'Range loop
                  S.P (K, I) (L, J) := (others => True);
               end loop;
            end loop;
         end loop;
      end loop;
      S.G := Empty;
      return S;
   end Empty;

   function To_Grille (S : in Search) return Grille is
   begin
      return S.G;
   end To_Grille;

   function To_Search (G : in Grille) return Search is
      S : Search := Empty;
   begin
      for I in NCase'Range loop
         declare
            R : Ref    := ToRef (I);
            N : Number := Get (G, R);
         begin
            if N /= 0 then
               Put (S, R, N);
            end if;
         end;
      end loop;
      return S;
   end To_Search;

   procedure Image (S : Search) is
   begin
      Image (S.G);
   end Image;

   procedure Remove (S : in out Search; R : Ref) is
      G : Grille := To_Grille (S);
   begin
      Put (G, R, 0);
      S := To_Search (G);
   end Remove;

   procedure Put (S : in out Search; R : Ref; N : Number) is
   begin

      -- on v�rifie que le nombre est possible pour la case ..
      if not S.P (R.GC, R.GL) (R.MC, R.ML) (N) then
         raise Invalid_Number;
      end if;

      -- on enl�ve les possibilites du nombre sur la colonne ...
      for I in Line'Range loop
         for K in Line'Range loop
            S.P (R.GC, K) (R.MC, I) (N) := False;
         end loop;
      end loop;

      -- on enl�ve les possibilites du nombre sur la ligne ...
      for I in Column'Range loop
         for K in Column'Range loop
            S.P (I, R.GL) (K, R.ML) (N) := False;
         end loop;
      end loop;

      -- on enleve les possibilit�s du nombre sur la matrice
      for I in Column'Range loop
         for K in Line'Range loop
            S.P (R.GC, R.GL) (I, K) (N) := False;
         end loop;
      end loop;

      -- on met le nombre dans la matrice
      Put (S.G, R, N);

      S.P (R.GC, R.GL) (R.MC, R.ML) := (others => False);
   end Put;

   function Get (S : in Search; R : Ref) return Number is
   begin
      return Get (S.G, R);
   end Get;

   --
   -- Manipulation des r�f�rences
   --

   function Topleft return Ref is
   begin
      return Ref'(GL => 1, GC => 1, ML => 1, MC => 1);
   end Topleft;

   function Next (R : Ref) return Ref is
      Retvalue : Ref := R;
   begin
      if (R.MC = Column'Last) then

         -- on est en fin de colonne
         Retvalue.MC := 1;

         if (R.ML = Line'Last) then
            -- on est en fin de ligne ...

            Retvalue.ML := 1;

            -- on incremente la colonne de la grille ...
            if (R.GC = Column'Last) then

               Retvalue.GC := 1;

               if (R.GL = Line'Last) then
                  Retvalue.GL := 1;
               else
                  Retvalue.GL := Line'Succ (R.GL);
               end if;
            else
               Retvalue.GC := Column'Succ (R.GC);
            end if;
         else
            Retvalue.ML := Line'Succ (R.ML);
         end if;
      else
         Retvalue.MC := Column'Succ (R.MC);
      end if;

      return Retvalue;
   end Next;

   procedure Image (R : Ref) is
   begin
      for I in Line'Range loop
         for J in Line'Range loop
            for K in Column'Range loop
               for L in Column'Range loop

                  if
                    (K = R.GC and then L = R.MC and then J = R.ML
                     and then I = R.GL)
                  then
                     Put ("X");
                  else
                     Put (".");
                  end if;
               end loop;
            end loop;
            New_Line;
         end loop;
      end loop;
   end Image;

   function List_Possibilite (S : in Search; R : Ref) return Possibilite is
   begin
      return S.P (R.GC, R.GL) (R.MC, R.ML);
   end List_Possibilite;

   function Count_Possibilite (S : in Search; R : Ref) return Natural is
      P        : Possibilite := List_Possibilite (S, R);
      Retvalue : Natural     := 0;
   begin
      for I in P'Range loop
         if P (I) then
            Retvalue := Natural'Succ (Retvalue);
         end if;
      end loop;

      return Retvalue;
   end Count_Possibilite;

   function ToNCase (R : Ref) return NCase is
      Colonne : Natural :=
        (Natural (R.GC) - 1) * Natural (Column'Last) + Natural (R.MC) - 1;
      Ligne   : Natural :=
        (Natural (R.GL) - 1) * Natural (Line'Last) + Natural (R.ML) - 1;
   begin
      return NCase (Ligne * (Natural (Column'Last)**2) + Colonne);
   end ToNCase;

   function ToRef (N : NCase) return Ref is
      R       : Ref;
      Ligne   : Natural := Natural (N) / (Natural (Column'Last**2));
      Colonne : Natural := Natural (N) mod (Natural (Column'Last**2));
   begin
      R.GL := Line ((Ligne / Natural (Line'Last)) + 1);
      R.ML := Line ((Ligne mod Natural (Line'Last)) + 1);

      R.GC := Column (Colonne / Natural (Column'Last) + 1);
      R.MC := Column (Colonne mod Natural (Column'Last) + 1);

      return R;
   end ToRef;

   procedure Save (G : in Grille; FileName : String) is
      F : File_Type;

      package NIO is new Integer_Io (Num => Number);

   begin
      Create (File => F, Name => Filename, Mode => Out_file);
      for I in NCase'Range loop
         if I mod 9 = 0 then
            New_Line (F);
         end if;
         Nio.Put (F, Get (G, Toref (I)));
      end loop;

      Close (F);

   end Save;

   function Load (FileName : String) return Grille is

      F : File_Type;
      G : Grille := Empty;
      N : Number;
      package NIO is new Integer_Io (Num => Number);

   begin
      Open (File => F, Name => Filename, Mode => In_File);

      for I in NCase'Range loop
         Nio.Get (F, N);
         Put (G, ToRef (I), N);
      end loop;

      Close (F);

      return G;
   end Load;

   procedure Find
     (S      : in     Search; Found : out Boolean; Result : out Search;
      Unique :    out Boolean)
   is

      procedure Find_Zero_Minimum_Valence
        (Found : out Boolean; Result : out NCase; Valence : out Natural)
      is
         N : Natural := Natural'Last;
      begin
         Found := False;

         for I in NCase'Range loop
            declare
               R : Ref := ToRef (I);
            begin
               if Get (S, R) = 0 then
                  Found := True;
                  if Count_Possibilite (S, R) < N then
                     Result := I;
                     N      := Count_Possibilite (S, R);
                  end if;
               end if;
            end;
         end loop;

         Valence := N;

      end Find_Zero_Minimum_Valence;

      B : Boolean := False;
      C : NCase;
      V : Natural;
   begin
      Unique := True;
      Find_Zero_Minimum_Valence (B, C, V);
      if B then

         if V > 1 then
            Unique := False;
         end if;

         -- on a trouv� un zero ...
         -- C contient la case � rechercher ...

         declare
            R : Ref         := ToRef (C);
            P : Possibilite := List_Possibilite (S, R);
         begin
            -- on essaye toutes les possibilites ...
            for I in P'Range loop
               if P (I) then

                  declare
                     NS      : Search := S;
                     NR      : Boolean;
                     NU      : Boolean;
                     NResult : Search;
                  begin
                     Put (NS, R, I);
                     Find (NS, NR, NResult, NU);
                     if NR then
                        Found  := True;
                        Result := NResult;
                        Unique := Unique and NU;
                        return;
                     end if;
                  end;
               end if;
            end loop;

            -- si on sort de la boucle, il n'y a plus
            -- de possibilites ...

            Found := False;

         end;
      else
         -- c'est la fin ...
         Found  := True;
         Result := S;
      end if;

   end Find;

end sodoku;
