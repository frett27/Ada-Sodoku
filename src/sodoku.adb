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

with Ada.Text_Io;use Ada.Text_Io;

package body sodoku is

   procedure Image(G : in Grille) is
   begin
      for I in line'Range loop
         for J in line'Range loop

            for K in column'Range loop
               for L in column'Range loop
                  Put(Number'Image(G(K, I)(L, J)));
               end loop;

            end loop;
            New_Line;
         end loop;
      end loop;
   end;

   function Empty return Grille is
      G : Grille;
   begin
      for I in line'Range loop
         for J in line'Range loop
            for K in column'Range loop
               for L in column'Range loop
                  G(K, I)(L, J):=0;
               end loop;
            end loop;
         end loop;
      end loop;
      return G;
   end;

   procedure Put(G : in out Grille; R : Ref ; N : Number) is
   begin
      G(R.Gc, R.Gl)(R.Mc, R.Ml) := N;
   end;

   function Get(G : in Grille; R : Ref) return Number is
   begin
      return G(R.Gc, R.Gl)(R.Mc, R.Ml);
   end;

   --
   -- Manipulation des éléments de recherche ...
   --

   function Empty return Search is
   S : Search;
   begin
      for I in line'Range loop
         for J in line'Range loop
            for K in column'Range loop
               for L in column'Range loop
                  S.P(K,I)(L,J) := (others => true);
               end loop;
            end loop;
         end loop;
      end loop;
      S.G := Empty;
      return s;
   end;

   function To_Grille(S : in Search) return Grille is
   begin
      return S.G;
   end;

   function To_Search(G : in Grille) return Search is
      S:Search := Empty;
   begin
      for I in Ncase'Range loop
         declare
            R : Ref := Toref(I);
            N : Number := Get(G,R);
         begin
            if n /= 0 then
               Put(S,R,N);
            end if;
         end;
      end loop;
      return S;
   end;


   procedure Image( S : Search ) is
   begin
      Image(S.G);
   end;

   procedure Remove(S : in out Search; R : Ref) is
     G : Grille := To_Grille(S);
   begin
      Put(G, R, 0);
      S := To_Search(G);
   end;


   procedure Put(S : in out Search; R : Ref ; N : Number) is
   begin

      -- on vérifie que le nombre est possible pour la case ..
      if not S.p(R.Gc, R.Gl)(R.Mc, R.Ml)(N) then
         raise INVALID_NUMBER;
      end if;

      -- on enlève les possibilites du nombre sur la colonne ...
      for I in line'Range loop
         for K in line'Range loop
            S.P(R.Gc, K)(R.Mc, I)(N) := False;
         end loop;
      end loop;

       -- on enlève les possibilites du nombre sur la ligne ...
      for I in column'Range loop
         for K in column'Range loop
            S.P(i, R.gl)(k, R.ml)(N) := False;
         end loop;
      end loop;

      -- on enleve les possibilités du nombre sur la matrice
      for I in column'Range loop
         for K in line'Range loop
            S.P(R.gc, R.gl)(i, k)(N) := False;
         end loop;
      end loop;

      -- on met le nombre dans la matrice
      Put(S.G, R, N);

      S.P(R.gc, R.gl)(R.mc, R.ml) := (others=>False);
   end;

   function Get(S : in Search; R : Ref) return Number is
   begin
      return Get(S.G, R);
   end;


   --
   -- Manipulation des références
   --

   function Topleft return Ref is
   begin
      return Ref'(Gl=>1,Gc=>1,Ml=>1,Mc=>1);
   end;

   function Next(R : Ref) return Ref  is
      Retvalue : Ref := R;
   begin
      if (R.Mc = Column'Last) then

         -- on est en fin de colonne
         Retvalue.Mc := 1;

         if (R.Ml = Line'Last) then
            -- on est en fin de ligne ...

            Retvalue.Ml := 1;

            -- on incremente la colonne de la grille ...
            if (R.Gc = Column'Last) then

               Retvalue.Gc  := 1;

               if (R.Gl = Line'Last) then
                  Retvalue.Gl := 1;
               else
                  Retvalue.Gl := Line'Succ(R.Gl);
               end if;
            else
               Retvalue.Gc := Column'Succ(R.Gc);
            end if;
         else
            Retvalue.Ml := Line'Succ(R.Ml);
         end if;
      else
         Retvalue.Mc := column'Succ(R.Mc);
      end if;

      return Retvalue;
   end;

   procedure Image(R:Ref) is
   begin
      for I in line'Range loop
         for J in line'Range loop
            for K in column'Range loop
               for L in column'Range loop

                  if (K = R.Gc and then
                      L = R.Mc and then
                      J = R.Ml and then
                      I = R.Gl ) then
                     Put("X");
                  else
                     Put(".");
                  end if;
               end loop;
            end loop;
            New_Line;
         end loop;
      end loop;
   end;

   function List_Possibilite(S : in Search ; R : Ref ) return Possibilite is
   begin
      return S.P(R.Gc, R.Gl)(R.Mc, R.Ml);
   end;

   function Count_Possibilite(S : in Search ; R : Ref) return Natural is
      P : Possibilite := List_Possibilite(S, R);
      Retvalue : Natural := 0;
   begin
      for I in P'Range loop
         if P(I) then
            Retvalue := Natural'Succ(Retvalue);
         end if;
      end loop;

      return Retvalue;
   end;

   function ToNCase(R:Ref) return NCase is
      Colonne : Natural := (Natural(R.Gc) - 1) * Natural(Column'Last) + Natural(R.Mc) - 1;
      Ligne : Natural := (Natural(R.Gl) - 1) * Natural(Line'Last) + Natural(R.Ml) - 1;
   begin
      return NCase(Ligne * (Natural(Column'Last) ** 2) + Colonne );
   end;

   function ToRef(N : NCase) return Ref is
      R : Ref ;
      Ligne : Natural := Natural(N) / (Natural(Column'Last ** 2));
      Colonne : Natural := Natural(N) mod (Natural(Column'Last ** 2));
   begin
      R.Gl := Line((Ligne / Natural(Line'Last)) + 1);
      R.Ml := Line((Ligne mod Natural(Line'Last)) + 1);

      R.Gc := Column(Colonne / Natural(Column'Last) + 1);
      R.Mc := Column(Colonne mod Natural(Column'Last) + 1);

      return R;
   end;


  procedure Save(G : in Grille;
                  FileName : String) is
      F : File_Type;

      package NIO is new Integer_Io(Num=>Number);

   begin
      Create(File => F,
             Name=> Filename,
             Mode => Out_file);
      for I in Ncase'Range loop
         if I mod 9 = 0 then
            New_Line(F);
         end if;
         Nio.Put(F, Get(G,Toref(I)));
      end loop;

      Close(F);

   end;

   function Load(FileName:String) return Grille is

      F : File_Type;
      G : Grille := Empty;
      N : Number ;
      package NIO is new Integer_Io(Num=>Number);

   begin
      Open(File=>F,
           Name => Filename,
           Mode => In_File);

      for I in Ncase'Range loop
         Nio.Get(F,N);
         Put(G,Toref(I),N);
      end loop;

      Close(F);

      return G;
   end;



   procedure Find(S:in Search;
                  Found:out Boolean;
                  Result : out Search;
                  Unique : out Boolean ) is


      procedure Find_Zero_Minimum_Valence(Found : out Boolean;
                                          Result : out NCase;
                                          Valence : out Natural) is
         N : Natural := Natural'last;
      begin
         Found := False;

         for I in Ncase'Range loop
            declare
               R: Ref := Toref(I);
            begin
               if Get(S, R) = 0 then
                  Found := True;
                  if Count_Possibilite(S, R) < N then
                     Result := i;
                     N := Count_Possibilite(S,R);
                  end if;
               end if;
            end;
         end loop;

         Valence := N;

      end;

      B : Boolean := False;
      C : NCase;
      V : Natural;
   begin
      Unique := True;
      Find_Zero_Minimum_Valence (B , C, V);
      if B then

         if V > 1 then
            Unique := False;
         end if;

         -- on a trouvé un zero ...
         -- C contient la case à rechercher ...

         declare
            R : Ref := Toref(C);
            P : Possibilite := List_Possibilite(S, R);
         begin
            -- on essaye toutes les possibilites ...
            for I in P'Range loop
               if P(I) then

                  declare
                     NS:Search := S;
                     NR : Boolean ;
                     NU : Boolean;
                     NResult : Search  ;
                  begin
                     Put(Ns,R, I);
                     Find(Ns,Nr, Nresult, Nu);
                     if Nr then
                        Found := True;
                        Result := nresult;
                        Unique := Unique and Nu;
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
         Found := True;
         Result := S;
      end if;

   end;

end sodoku;
