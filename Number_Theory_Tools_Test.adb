-------------------------------------------------------------------------------
--                                                                           --
--                           Number Theory Tools                             --
--                                                                           --
--                         Number_Theory_Tools.adb                           --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 1997 Ulrik Hørlyk Hjort                   --
--                                                                           --
--  Number Theory Tools is free software;  you can  redistribute it          --
--  and/or modify it under terms of the  GNU General Public License          --
--  as published  by the Free Software  Foundation;  either version 2,       --
--  or (at your option) any later version.                                   --
--  Number Theory Tools is distributed in the hope that it will be           --
--  useful, but WITHOUT ANY WARRANTY;  without even the  implied warranty    --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  --
--  See the GNU General Public License for  more details.                    --
--  You should have  received  a copy of the GNU General                     --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------
with Number_Theory_Tools; use Number_Theory_Tools;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

procedure Number_Theory_Tools_Test is
   P : Large_Positive := 0;
   Prime_Factors : Large_Positive_Array_T(1..20) := (others => 1);
begin
   Put_Line("Test: Get a prime > 3000:");
   P:= Get_Prime(3000);
   Put_Line("Prime = " & Integer'Image(Integer(P)));

   New_Line;
   Put_Line("Test: Find greatest common divisor for 2400 and 1000 :");
   P := Gcd(2400,1000);
   Put_Line("Greatest common divisor for 2400 and 1000 = " & Integer'Image(Integer(P)));

   New_Line;
   Put_Line("Test: Get a coprime to 5600 :");
   P := Get_CoPrime(5600);
   Put_Line("Coprime to 5600 = " & Integer'Image(Integer(P)));

   New_Line;
   Put_Line("Test: Factorize 8640 into prime factors :");
   Factorize(8640, Prime_Factors);
   Put("Prime factors for 8640: ");
   for I in Prime_Factors'First .. Prime_Factors'Last loop
    P:= Prime_Factors(I);
      Put(Unsigned_32'Image(Unsigned_32(Prime_Factors(I))) & " ");
      exit when Prime_Factors(I) = 1;
   end loop;
   New_Line;
   Put_Line("Inverse: " & Large_Positive'Image(Get_Inverse(8,7)));
   Put_Line("Inverse: " & Large_Positive'Image(Get_Inverse(58,7)));
   Put_Line("Inverse: " & Large_Positive'Image(Get_Inverse(18,3)));
   Put_Line("Inverse: " & Large_Positive'Image(Get_Inverse(20,8)));
   Put_Line("Inverse: " & Large_Positive'Image(Get_Inverse(4,2)));
 end Number_Theory_Tools_Test;
