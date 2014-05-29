------------------------------------------------------------------------------
--                               Combinatoire                               --
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

with Combinatoire;use Combinatoire;
with Ada.Text_Io;use Ada.Text_Io;

procedure Test_Combinatoire is


   Assertion_Failed : exception;


   function Compte_Combinaisons(Tailletotale : Natural;
                                Nbe : Positive)
                               return Natural is
      It : Iterator := Create(Tailletotale => Tailletotale,
                              Nbe => Nbe);
      Compteur : Natural := 0;
      C : Combinaison(1..Tailletotale);
   begin

      begin
         loop
            C  := Next(It);
            Compteur := Compteur + 1;
            New_Line;
            Image(C);
         end loop;
      exception
         when End_Of_Iterator =>
            return Compteur;
         when others =>
            raise;
      end;
   end;

   procedure Check_Combinaisons(Tailletotale : Natural;
                                Nbe : Positive;
                                Nbcombinaisons : Positive) is

   begin
      Put_Line("Test du packetage combinatoire ... ");
      Put_Line("   " & Natural'Image(Tailletotale) & ", " & Positive'Image(Nbe));
      declare
         C : Natural := Compte_Combinaisons(tailletotale,nbe);
      begin
         New_Line;
         Put_Line("nombre de combinaison => " & Natural'Image(C));
         if C /= Nbcombinaisons then
            raise Assertion_Failed with "in calculing " & Natural'Image(Tailletotale)
              & " - " & Positive'Image(Nbe) & " -- nb attended " & Positive'Image(Nbcombinaisons);
         end if;

      end;
   end;

begin
   Check_Combinaisons(1, 1, 1);
   Check_Combinaisons(16, 1, 16);
   Check_Combinaisons(32, 1, 32);
   Check_Combinaisons(32, 2, 496); -- C2 32
   Check_Combinaisons(64, 2, 2016); -- C2 64
end;
