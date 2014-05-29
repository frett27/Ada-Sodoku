with Sodoku;
use Sodoku;

with Combinatoire;
use Combinatoire;

with GNAT.Traceback.Symbolic;

with Ada.Text_Io;
use Ada.Text_Io;

with Ada.Calendar;
with Ada.Real_Time;

procedure Recherche_Solution is

   N : Natural := 0;

   procedure Find_All(S : in Search;
                      R : in Ref ;
                      Found : out Boolean ;
                      Result : out Search) is

      P : Possibilite := List_Possibilite(S, R);
      B : Boolean := False;
   begin
      for I in P'Range loop
         if (P(I) = True) then
            B := True;
            -- on explore cette possibilite
            declare
               NS : Search := S;
               NR : Ref := Next(R);
            begin
               Put(NS, R, I);
               if (Nr = Topleft) then
                  -- on a parcouru toute la grille, c'est une solution ..
                  N := N + 1;
                  if (N mod 10000) = 0 then
                     Put_line("---------------------- SOLUTION N° " & Natural'Image(N) & "------------------ ");
                     Image(NS);
                  end if;
                  Found := True;
                  Result := Ns;
                  return;
               end if;

               Find_All(NS, Nr, Found, Result);
            end;
         end if;
      end loop;

      if B = False then
         -- pas de possibilité pour cet element ...
         return;
      end if;

   end;




   -- cette fonction cree une grille de sodoku
   -- contenant des numero où la position
   -- Dans le tableau de combinaison est a true
   function Create_Grille_Sodoku(G : Grille;
                                 C : Combinaison) return search is
      Ng : Search := Empty;
   begin

      for I in Ncase'Range loop
         declare
            R : Ref := Toref(I);
         begin
            if C(Natural(I) + 1) then
               Put(Ng,R,Get(G,R));
            end if;
         end;
      end loop;
      return Ng;
   end;


   procedure Recherche_Grille_Sodoku(S : Search ;
                                     Nbe : Positive) is

      use Ada.Real_Time;

      NS : Search;
      IT : Iterator;
      N : Long_Integer := 1;
      Found : Boolean;
      Unique : Boolean;
      Result : Search;
      Start : Time := Clock;
   begin
      -- iterateur utilisé pour savoir quelle grille
      -- contient des cases vides ...
      It := Create(81,Nbe);
      while True loop
         declare
            C : Combinaison := Next(It);
         begin
            N := Long_Integer'Succ(N);
            if N mod 1000 = 0 then
               Put(".");
            end if;

            Ns := Create_Grille_Sodoku(To_Grille(S), C);
            Find (Ns, Found, Result, Unique);
            if Found and then Unique then
               Put_Line("Grille de sodoku trouvée :");
               Image(Ns);
            end if;

--          if N mod 10000 = 0 then
--             Put_Line("Grille actuelle ");
--             Image(NS);
--             Put_Line("temps de resolution de grille "
--                      & Duration'Image( To_Duration(Clock - Start) / 10000 ));
--             Start := Clock;
--          end if;

         end;
      end loop;
   exception
      when END_OF_ITERATOR =>
         return;
   end;

   R : Ref := Topleft;
   S: Search := Empty;
   Found : Boolean;
   Result : Search;
   Unique : Boolean := true;

begin


   S := To_Search(Load("grille1.sdk"));

   Find(S, Found, Result, Unique);
   Image(Result);

   S := Result;

   if Found and then Unique then
      Put_Line("Cette solution est unique");
   end if;

   -- recherche d'une grille associée ...
   Recherche_Grille_Sodoku(S, 30);

exception
   when E : others =>
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end;
