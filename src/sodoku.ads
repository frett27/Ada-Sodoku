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


package sodoku is

   type Column is range 1..3;
   for Column'Size use 2;

   type Line is range 1..3;
   for Line'Size use 2;

   -- nombre contenu dans une grille de sodoku
   type Number is new Integer
     range 0.. Integer(Column'Last) * Integer(Line'Last);
   -- 0 signifie, pas définit
   for Number'Size use 4;


   -- type décrivant une grille de sodoku
   type Grille is private;


   -- type définissant une reference à une case dans
   -- la grille
   type Ref is private;


   procedure Image(G : in Grille);
   function Empty return Grille;

   -- fonctions de manipulation d'une grille de sodoku
   procedure Put(G : in out Grille; R : Ref ; N : Number);
   function Get(G : in Grille; R : Ref) return Number;

   -- type decrivant une recherche de grille, dans laquelle
   -- il y a des zero, signifiant que la grille n'est pas
   -- complète. associé à chaque élément incomplet, une liste
   -- de possibilités est associée
   type Search is private;

   -- cette fonction convertie une recherche en grille (
   -- extraction de la grille)
   function To_Grille (S : in Search) return Grille;

   procedure Image (S : in Search);

   -- creation d'une grille de recherche vide
   function Empty return Search;

   -- définit la valeur d'un élément de la grille de recherche,
   -- en reduisant les possibilités associées aux autres éléments
   -- liés (colonne, ligne et sous-grille)
   procedure Put (S : in out Search;
                  R :        Ref;
                  N :        Number);

   -- récupère la valeur associée à une grille de recherche
   function Get (S : in Search;
                 R :    Ref) return Number;

   -- supprime la valeur à l'endroit donné
   procedure Remove (S : in out Search;
                     R :        Ref);


   -- converti une grille de sodoku en grille de recherche
   function To_Search(G : in Grille) return Search;


   Invalid_Number : exception;


   -- liste des possibilités pour une case dans la grille
   type Possibilite is array (1 .. Number'Last) of Boolean;
   pragma Pack(Possibilite);

   --
   -- Liste les possibilités pour une case de la grille ...
   --
   function List_Possibilite (S : in Search;
                              R :    Ref) return Possibilite;

   -- Compte le nombre de possibilités pour une case ..
   function Count_Possibilite (S : in Search;
                               R :    Ref) return Natural;



   -- manipulation des référence

   function TopLeft return Ref;
   procedure Image (R : Ref);
   function Next(R : Ref) return Ref;

   -- numéros de case
   type NCase is
     range 0..Natural(Column'Last **2) * Natural(Line'Last **2) - 1;

     -- converti une référence de case en numéros de case
     function ToNCase(R:Ref) return NCase;

     -- converti un numero de case en reference
     function ToRef(N : NCase) return Ref;


     -- fonction d'IO des grilles ...
     procedure Save (G        : in Grille;
                     FileName :    String);

     function Load(FileName:String) return Grille;


     -- fonction de recherche
     procedure Find(S:in Search;
                    Found:out Boolean;
                    Result : out Search;
                    Unique : out Boolean );





private


   type Matrice is array(column,line) of Number;
   pragma Pack(Matrice);

   type Grille is array (column,line) of Matrice;
   pragma Pack(Grille);

   type Matrice_Possibilite is
     array (column, line) of Possibilite;
   pragma Pack(Matrice_Possibilite);

   type Grille_Possibilite is
     array (column, line) of Matrice_Possibilite;
   pragma Pack(Grille_Possibilite);

   type Search is record
      G : Grille;
      P : Grille_Possibilite;
   end record;


   -- référence à une case du sodoku ...
   type Ref is record
      MC : Column; -- colonne de la matrice
      ML : Line; -- ligne de la matrice
      GC : Column;
      GL : Line;
   end record;

end sodoku;
