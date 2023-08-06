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

package Combinatoire is

   -- tableau de combinaison
   type Combinaison is array (Positive range <>) of Boolean;
   pragma Pack (Combinaison);

   -- affichage d'une combinaison
   procedure Image (C : Combinaison);

   -- iterateur permettant de parcourir les combinaisons ayant
   -- TailleTotale de "bit" et ayant nbe "bit" de positionn�s � 1
   type Iterator is private;

   -- Cree l'iterateur

   ---
   -- Create
   --    creation d'un iterateur
   function Create (Tailletotale : Natural; Nbe : Positive) return Iterator;

   -- combinaison suivante ... :-)
   function Next (It : in Iterator) return Combinaison;

   END_OF_ITERATOR : exception;

private

   MaxPrecalc : constant Natural := 16;

   type Enum_Combinaison;
   type Enum_Combinaison_Access is access Enum_Combinaison;

   type Enum_Combinaison (Length : Natural) is record
      C    : Combinaison (1 .. Length);
      Next : Enum_Combinaison_Access;
   end record;

   type Enum_Array is array (Natural range <>) of Enum_Combinaison_Access;
   type Enum_Array_Access is access Enum_Array;

   -- par compte de bit � 1, on liste les combinaisons
   type Combinaison_Precalc (Length : Natural) is record
      C : Enum_Array (0 .. Length);
   end record;

   type Combinaison_Precalc_Access is access all Combinaison_Precalc;

   type Dictionnaire_Combinaison is
     array (Natural range 0 .. MaxPrecalc) of Combinaison_Precalc_Access;

   -- tableau contenant les tailles
   type Nbelements is array (Positive range <>) of Natural;
   type Nbelements_Access is access all Nbelements;

   type Enum_Possibilite;
   type Enum_Possibilite_Access is access all Enum_Possibilite;
   type Enum_Possibilite_Access_Access is access all Enum_Possibilite_Access;

   type Enum_Possibilite is record
      T    : Nbelements_Access;
      Next : Enum_Possibilite_Access;
   end record;

   function Calc_Dictionnaire return Dictionnaire_Combinaison;

   -- dictionnaire des combinaison pr�calcul�es
   Dictionnaire : Dictionnaire_Combinaison;

   type Iterator is record
      Tailletotale : Natural;
      Nbe : Nbelements_Access; -- tableau contenant le nombre total d'�l�ments pour chaque schema
      Possibilite  : Enum_Possibilite_Access; -- nombre de possibilites
      -- (arrangement pour chaque schemas)

      Current_Possibilite : Enum_Possibilite_Access_Access; -- possibilite courante en cours d'exploitation ..
      Current_Possibilite_Pos : Enum_Array_Access; -- pointeurs sur les combinaisons pr�calcul�es

   end record;

end Combinatoire;
