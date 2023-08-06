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

with sodoku;      use sodoku;
with Ada.Text_Io; use Ada.Text_Io;
with GNAT.Command_Line;

procedure Sdksolver is

   -- cas du chargement d'une grille dans un fichier
   Fichier_Grille_Lecture : access String := null;
   Fichier_Grille_Sortie  : access String := null;

   Syntax_Error : exception;

   procedure Parse_Command_Line is
   begin
      loop
         case GNAT.Command_Line.Getopt ("f= o=") is
            when ASCII.NUL =>
               exit;

            when 'f' =>
               Fichier_Grille_Lecture :=
                 new String'(GNAT.Command_Line.Parameter);

            when 'o' =>
               Fichier_Grille_Sortie :=
                 new String'(GNAT.Command_Line.Parameter);

            when others =>
               raise Syntax_Error;

         end case;

      end loop;

   end Parse_Command_Line;

   procedure Print_Usage is
   begin
      New_Line;
      Put_Line ("Usage sdksolver -f inputfilename [-o outputfilename]");
   end Print_Usage;

begin

   Put_Line ("SdkSolver - Version 1.0");
   begin
      Parse_Command_Line;
   exception
      when Syntax_Error =>
         Print_Usage;
         return;
   end;

   if Fichier_Grille_Lecture = null then
      Put_Line ("-f option must be specified");
      return;
   end if;

   -- lecture de la grille ...

   declare
      G             : Grille := Load (Fichier_Grille_Lecture.all);
      S             : Search := To_Search (G);
      Sout          : Search;
      Found, Unique : Boolean;

   begin
      Find (S => S, Found => Found, Result => Sout, Unique => Unique);

      if Found then
         Put_Line ("Solution found");
      end if;

      if Unique then
         Put_Line ("unique solution found");
      end if;

      if Fichier_Grille_Sortie /= null then
         -- on demande l'�criture du resultat
         Image (Sout);
         Put_Line ("writing result in " & Fichier_Grille_Sortie.all);
         Save (To_Grille (Sout), Fichier_Grille_Sortie.all);
      else
         -- le resultat est donn� sur la ligne de commande
         Image (Sout);
      end if;

   end;

end Sdksolver;
